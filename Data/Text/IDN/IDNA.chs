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
module Data.Text.IDN.IDNA
	( Flags (..)
	, Error
	, defaultFlags
	, toASCII
	, toUnicode
	) where

import qualified Data.ByteString as B
import qualified Data.Text as T

import Foreign
import Foreign.C

import Data.Text.IDN.Internal

#include <idna.h>
#include <idn-free.h>

{# enum Idna_rc {} with prefix = "IDNA_" #}

{# enum Idna_flags {} with prefix = "IDNA_" #}

data Flags = Flags
	{ useStd3Rules :: Bool
	, allowUnassigned :: Bool
	}
	deriving (Show, Eq)

defaultFlags :: Flags
defaultFlags = Flags False False

toASCII :: Flags -> T.Text -> Either Error B.ByteString
toASCII flags input =
	unsafePerformIO $
	withArray0 0 (toUCS4 input) $ \buf ->
	let c_flags = encodeFlags flags in
	alloca $ \outBufPtr -> do
		c_rc <- {# call idna_to_ascii_4z #}
			(castPtr buf) outBufPtr c_flags
		
		let rc = fromIntegral c_rc
		if rc /= fromEnum SUCCESS
			then return (Left (cToError c_rc))
			else do
				outBuf <- peek outBufPtr
				bytes <- B.packCString outBuf
				{# call idn_free #} (castPtr outBuf)
				return (Right bytes)

toUnicode :: Flags -> B.ByteString -> Either Error T.Text
toUnicode flags input =
	unsafePerformIO $
	B.useAsCString input $ \buf ->
	let c_flags = encodeFlags flags in
	alloca $ \outBufPtr -> do
		c_rc <- {# call idna_to_unicode_8z4z #}
			(castPtr buf) outBufPtr c_flags
		
		let rc = fromIntegral c_rc
		if rc /= fromEnum SUCCESS
			then return (Left (cToError c_rc))
			else do
				outBuf <- peek outBufPtr
				ucs4 <- peekArray0 0 (castPtr outBuf)
				{# call idn_free #} (castPtr outBuf)
				return (Right (fromUCS4 ucs4))

encodeFlags :: Flags -> CInt
encodeFlags flags = foldr (.|.) 0 bits where
	bitAt f e = if f flags then 0 else fromIntegral (fromEnum e)
	bits = [ bitAt useStd3Rules USE_STD3_ASCII_RULES
	       , bitAt allowUnassigned ALLOW_UNASSIGNED
	       ]

cToError :: CInt -> Error
cToError rc = IDNAError (T.pack str) where
	c_strerror = {# call idna_strerror #}
	str = unsafePerformIO (c_strerror rc >>= peekCString)
