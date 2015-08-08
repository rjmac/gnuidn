{-# LANGUAGE ForeignFunctionInterface #-}

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

module Data.Text.IDN.IDNA
	( Flags (..)
	, Error
	, defaultFlags
	, toASCII
	, toUnicode
	) where

import Control.Exception (ErrorCall(..), throwIO)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified System.IO.Unsafe as Unsafe

import Foreign
import Foreign.C

import Data.Text.IDN.Internal

#include <idna.h>
#include <idn-free.h>

{# enum Idna_rc {} with prefix = "IDNA_" #}

{# enum Idna_flags {} with prefix = "IDNA_" #}

data Flags = Flags
	{ verifySTD3 :: Bool
	-- ^ Check output to make sure it is a STD3-conforming host name
	
	, allowUnassigned :: Bool
	-- ^ Allow unassigned Unicode code points
	}
	deriving (Show, Eq)

-- | @defaultFlags = Flags True False@
defaultFlags :: Flags
defaultFlags = Flags True False

-- | Convert a Unicode domain name to an ASCII 'B.ByteString'. The domain
-- name may contain several labels, separated by periods.
--
-- @toASCII@ never alters a sequence of code points that are all in the
-- ASCII range to begin with (although it could fail). Applying @toASCII@
-- multiple times gives the same result as applying it once.
toASCII :: Flags -> T.Text -> Either Error B.ByteString
toASCII flags input =
	Unsafe.unsafePerformIO $
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

-- | Convert a possibly ACE-encoded domain name to Unicode. The domain
-- name may contain several labels, separated by dots.
--
-- Aside from memory allocation failure, @toUnicode@ always succeeds.
-- If the input cannot be decoded, it is returned unchanged.
toUnicode :: Flags -> B.ByteString -> T.Text
toUnicode flags input =
	Unsafe.unsafePerformIO $
	B.useAsCString input $ \buf ->
	let c_flags = encodeFlags flags in
	alloca $ \outBufPtr -> do
		c_rc <- {# call idna_to_unicode_8z4z #}
			(castPtr buf) outBufPtr c_flags
		
		let rc = fromIntegral c_rc
		if rc == fromEnum MALLOC_ERROR
			then throwError c_rc
			else do
				outBuf <- peek outBufPtr
				ucs4 <- peekArray0 0 (castPtr outBuf)
				{# call idn_free #} (castPtr outBuf)
				return (fromUCS4 ucs4)

encodeFlags :: Flags -> CInt
encodeFlags flags = foldr (.|.) 0 bits where
	bitAt f e = if f flags then fromIntegral (fromEnum e) else 0
	bits = [ bitAt verifySTD3 USE_STD3_ASCII_RULES
	       , bitAt allowUnassigned ALLOW_UNASSIGNED
	       ]

cToError :: CInt -> Error
cToError rc = IDNAError (T.pack str) where
	c_strerror = {# call idna_strerror #}
	str = Unsafe.unsafePerformIO (c_strerror rc >>= peekCString)

throwError :: CInt -> IO a
throwError rc = do
	str <- peekCString =<< {# call idna_strerror #} rc
	throwIO (ErrorCall str)
