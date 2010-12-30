-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Text.IDN.Punycode
	( encode
	, decode
	) where

import Control.Exception (ErrorCall(..), throwIO)
import Control.Monad (unless)
import Data.List (unfoldr)
import qualified Data.ByteString as B
import qualified Data.Text as T

import Foreign
import Foreign.C

import Data.Text.IDN.Internal (toUCS4, fromUCS4)

#include <punycode.h>

{# enum Punycode_status {} with prefix = "PUNYCODE_" #}

type SizeT = {# type size_t #}

-- | Encode
encode :: T.Text -- * Input
       -> Maybe (Integer -> Bool) -- * Case flag predicate
       -> B.ByteString
encode input maybeIsCase = unsafePerformIO io where
	inSize = T.length input
	
	flags = flip fmap maybeIsCase $ \isCase -> let
		step idx = Just (fromBool (isCase idx), idx + 1)
		in unfoldr step 0
	
	io = maybeWith (withArray . take inSize) flags impl
	
	impl caseBuf = withArray (toUCS4 input) (loop caseBuf inSize . castPtr)
	
	loop caseBuf outMax inBuf = do
		res <- tryEnc caseBuf outMax inBuf
		case res of
			Nothing -> loop caseBuf (outMax + 50) inBuf
			Just (Right bytes) -> return bytes
			Just (Left rc) -> cToError rc
	
	tryEnc caseBuf outMax inBuf =
		allocaBytes outMax $ \outBuf ->
		alloca $ \outSizeBuf -> do
			poke outSizeBuf (fromIntegral outMax)
			c_rc <- {# call punycode_encode #}
				(fromIntegral inSize)
				inBuf
				caseBuf
				outSizeBuf
				outBuf
			
			let rc = fromIntegral c_rc
			if rc == fromEnum OVERFLOW
				then return Nothing
				else if rc == fromEnum SUCCESS
					then do
						outSize <- peek outSizeBuf
						bytes <- peekOut outBuf outSize
						return (Just (Right bytes))
					else return (Just (Left c_rc))
	
	peekOut outBuf outSize = B.packCStringLen cstr where
		cstr = (outBuf, fromIntegral outSize)

-- | Decode
decode :: B.ByteString -- * Input
       -> Maybe (T.Text, (Integer -> Bool))
decode input = unsafePerformIO $
	let outMax = B.length input in
	B.useAsCStringLen input $ \(inBuf, inSize) ->
	alloca $ \outSizeBuf ->
	allocaArray outMax $ \outBuf -> do
	
	flagForeign <- mallocForeignPtrArray outMax
	poke outSizeBuf (fromIntegral outMax)
	
	c_rc <- withForeignPtr flagForeign $ \flagBuf ->
		{# call punycode_decode #}
			(fromIntegral inSize)
			inBuf
			outSizeBuf
			outBuf
			flagBuf
	
	let rc = fromIntegral c_rc
	if rc == fromEnum BAD_INPUT
		then return Nothing
		else do
			unless (rc == fromEnum SUCCESS) (cToError c_rc)
			
			outSize <- peek outSizeBuf
			ucs4 <- peekArray (fromIntegral outSize) (castPtr outBuf)
			let text = fromUCS4 ucs4
			return (Just (text, checkCaseFlag flagForeign outSize))

checkCaseFlag :: ForeignPtr CUChar -> SizeT -> Integer -> Bool
checkCaseFlag ptr csize = checkIdx where
	intsize = toInteger csize
	checkIdx idx | idx < 0        = False
	checkIdx idx | idx >= intsize = False
	checkIdx idx =
		unsafePerformIO $
		withForeignPtr ptr $ \buf -> do
			cuchar <- peekElemOff buf (fromInteger idx)
			return (toBool cuchar)

cToError :: CInt -> IO a
cToError rc = do
	str <- peekCString =<< {# call punycode_strerror #} rc
	throwIO (ErrorCall str)
