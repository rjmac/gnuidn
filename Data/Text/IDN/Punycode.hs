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
	) where

import Control.Exception (ErrorCall(..), throwIO)
import Data.Char (ord)
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

foreign import ccall "punycode_strerror"
	c_strerror :: F.CInt -> IO F.CString
