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
module Data.Text.IDN.StringPrep
	(
	-- * Stringprep
	  Flags (..)
	, Error
	, Profile
	, defaultFlags
	, stringprep
	
	-- * Profiles
	, iscsi
	, kerberos5
	, nameprep
	, sasl
	, saslAnonymous
	, trace
	, xmppNode
	, xmppResource
	) where

import Data.Bits ((.|.))
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Foreign as F
import qualified Foreign.C as F

data Error = StringPrepError T.Text
	deriving (Show, Eq)

data Flags = Flags
	{ enableNFKC :: Bool
	, enableBidi :: Bool
	, allowUnassigned :: Bool
	}
	deriving (Show, Eq)

newtype Profile = Profile (F.Ptr Profile)

defaultFlags :: Flags
defaultFlags = Flags True True False

stringprep :: Profile -> Flags -> T.Text -> Either Error T.Text
stringprep profile flags input = F.unsafePerformIO io where
	io = B.useAsCString utf8 (loop inSize)
	
	utf8 = TE.encodeUtf8 input
	cflags = encodeFlags flags
	inSize = B.length utf8 + 1 -- + 1 for NUL
	
	loop outSize inBuf = do
		res <- tryPrep outSize inBuf
		case res of
			Nothing -> loop (outSize + 50) inBuf
			Just (Right bytes) -> return (Right (TE.decodeUtf8 bytes))
			Just (Left rc) -> return (Left (cToError rc))
	
	tryPrep outSize inBuf = F.allocaBytes outSize $ \outBuf -> do
		F.copyArray outBuf inBuf inSize
		let csize = fromIntegral outSize
		rc <- c_stringprep outBuf csize cflags profile
		case rc of
			-- TOO_SMALL_BUFFER
			100 -> return Nothing
			
			-- success
			0 -> fmap (Just . Right) (B.packCString outBuf)
			
			-- failure
			_ -> return (Just (Left rc))

encodeFlags :: Flags -> F.CInt
encodeFlags flags = foldr (.|.) 0 bits where
	bit f x y = if f flags then x else y
	bits = [ bit enableNFKC 0 1
	       , bit enableBidi 0 2
	       , bit allowUnassigned 0 4
	       ]

cToError :: F.CInt -> Error
cToError rc = StringPrepError (T.pack str) where
	str = F.unsafePerformIO (c_strerror rc >>= F.peekCString)

foreign import ccall "stringprep"
	c_stringprep :: F.CString -> F.CSize -> F.CInt -> Profile -> IO F.CInt

foreign import ccall "stringprep_strerror"
	c_strerror :: F.CInt -> IO F.CString

-- | iSCSI (RFC 3722)
foreign import ccall "&stringprep_iscsi"
	iscsi :: Profile

-- | Kerberos 5
foreign import ccall "&stringprep_kerberos5"
	kerberos5 :: Profile

-- | Nameprep (RFC 3491)
foreign import ccall "&stringprep_nameprep"
	nameprep :: Profile

-- | SASLprep (RFC 4013)
foreign import ccall "&stringprep_saslprep"
	sasl :: Profile

-- | Draft SASL ANONYMOUS
foreign import ccall "&stringprep_plain"
	saslAnonymous :: Profile

foreign import ccall "&stringprep_trace"
	trace :: Profile

-- | XMPP node (RFC 3920)
foreign import ccall "&stringprep_xmpp_nodeprep"
	xmppNode :: Profile

-- | XMPP resource (RFC 3920)
foreign import ccall "&stringprep_xmpp_resourceprep"
	xmppResource :: Profile
