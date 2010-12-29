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
import Data.Char (chr, ord)
import Data.List (unfoldr)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Foreign as F
import qualified Foreign.C as F

-- | Encode
encode :: T.Text -- * Input
       -> Maybe (Integer -> Bool) -- * Case flag predicate
       -> B.ByteString
encode input maybeIsCase = F.unsafePerformIO io where
	inSize = T.length input
	
	flags = flip fmap maybeIsCase $ \isCase -> let
		step idx = Just (F.fromBool (isCase idx), idx + 1)
		in unfoldr step 0
	
	io = F.maybeWith (F.withArray . take inSize) flags impl
	
	codepoints :: [F.Word32]
	codepoints = map (fromIntegral . ord) (T.unpack input)
	
	impl caseBuf = F.withArray codepoints (loop caseBuf inSize)
	
	loop caseBuf outMax inBuf = do
		res <- tryEnc caseBuf outMax inBuf
		case res of
			Nothing -> loop caseBuf (outMax + 50) inBuf
			Just (Right bytes) -> return bytes
			Just (Left rc) -> cToError rc
	
	tryEnc caseBuf outMax inBuf =
		F.allocaBytes outMax $ \outBuf ->
		F.alloca $ \outSizeBuf -> do
			F.poke outSizeBuf (fromIntegral outMax)
			rc <- c_encode
				(fromIntegral inSize)
				inBuf
				caseBuf
				outSizeBuf
				outBuf
			
			case rc of
				2 -> return Nothing
				
				0 -> do
					outSize <- F.peek outSizeBuf
					bytes <- peekOut outBuf outSize
					return (Just (Right bytes))
				
				_ -> return (Just (Left rc))
	
	peekOut outBuf outSize = B.packCStringLen cstr where
		cstr = (outBuf, fromIntegral outSize)

-- | Decode
decode :: B.ByteString -- * Input
       -> Maybe (T.Text, (Integer -> Bool))
decode input = F.unsafePerformIO $
	let outMax = B.length input in
	B.useAsCStringLen input $ \(inBuf, inSize) ->
	F.alloca $ \outSizeBuf ->
	F.allocaArray outMax $ \outBuf -> do
	
	flagForeign <- F.mallocForeignPtrArray outMax
	F.poke outSizeBuf (fromIntegral outMax)
	
	rc <- F.withForeignPtr flagForeign $ \flagBuf -> c_decode
		(fromIntegral inSize)
		inBuf
		outSizeBuf
		outBuf
		flagBuf
	
	if rc == 1
		then return Nothing
		else do
			unless (rc == 0) (cToError rc)
			
			outSize <- F.peek outSizeBuf
			codepoints <- F.peekArray (fromIntegral outSize) outBuf
			
			let text = T.pack (map (chr . fromIntegral) codepoints)
			return (Just (text, checkCaseFlag flagForeign outSize))

checkCaseFlag :: F.ForeignPtr (F.CUChar) -> F.CSize -> Integer -> Bool
checkCaseFlag ptr csize = checkIdx where
	intsize = toInteger csize
	checkIdx idx | idx >= intsize = False
	checkIdx idx =
		F.unsafePerformIO $
		F.withForeignPtr ptr $ \buf -> do
			cuchar <- F.peekElemOff buf (fromInteger idx)
			return (F.toBool cuchar)

cToError :: F.CInt -> IO a
cToError rc = do
	str <- F.peekCString =<< c_strerror rc
	throwIO (ErrorCall str)

foreign import ccall "punycode_encode"
	c_encode :: F.CSize
	         -> F.Ptr F.Word32
	         -> F.Ptr F.CUChar
	         -> F.Ptr F.CSize
	         -> F.Ptr F.CChar
	         -> IO F.CInt

foreign import ccall "punycode_decode"
	c_decode :: F.CSize
	         -> F.Ptr F.CChar
	         -> F.Ptr F.CSize
	         -> F.Ptr F.Word32
	         -> F.Ptr F.CUChar
	         -> IO F.CInt

foreign import ccall "punycode_strerror"
	c_strerror :: F.CInt -> IO F.CString
