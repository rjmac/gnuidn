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

module Data.Text.IDN.StringPrep
	(
	-- * Stringprep
	  Flags (..)
	, Error
	, defaultFlags
	, stringprep
	
	-- * Profiles
	, Profile
	, iscsi
	, kerberos5
	, nameprep
	, sasl
	, saslAnonymous
	, trace
	, xmppNode
	, xmppResource
	) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified System.IO.Unsafe as Unsafe

import Foreign
import Foreign.C

import Data.Text.IDN.Internal

#include <stringprep.h>

{# pointer *Stringprep_profile as Profile newtype #}

{# enum Stringprep_rc {} with prefix = "STRINGPREP_" #}

{# enum Stringprep_profile_flags {} with prefix = "STRINGPREP_" #}

data Flags = Flags
	{ enableNFKC :: Bool
	-- ^ Enable the NFKC normalization, as well as selecting the NFKC
	-- case folding tables. Usually the profile specifies BIDI and NFKC
	-- settings, and applications should not override it unless in
	-- special situations.
	
	, enableBidi :: Bool
	-- ^ Enable the BIDI step. Usually the profile specifies BIDI and
	-- NFKC settings, and applications should not override it unless in
	-- special situations.
	
	, allowUnassigned :: Bool
	-- ^ If false, 'stringprep' will return an error if the input
	-- contains characters not assigned to the profile.
	}
	deriving (Show, Eq)

-- | @defaultFlags = Flags True True False@
defaultFlags :: Flags
defaultFlags = Flags True True False

stringprep :: Profile -> Flags -> T.Text -> Either Error T.Text
stringprep profile flags input = Unsafe.unsafePerformIO io where
	io = B.useAsCString utf8 (loop inSize)
	
	utf8 = TE.encodeUtf8 input
	c_flags = encodeFlags flags
	inSize = B.length utf8 + 1 -- + 1 for NUL
	
	loop outSize inBuf = do
		res <- tryPrep outSize inBuf
		case res of
			Nothing -> loop (outSize + 50) inBuf
			Just (Right bytes) -> return (Right (TE.decodeUtf8 bytes))
			Just (Left rc) -> return (Left (cToError rc))
	
	tryPrep outSize inBuf = allocaBytes outSize $ \outBuf -> do
		copyArray outBuf inBuf inSize
		let csize = fromIntegral outSize
		c_rc <- {# call stringprep as c_stringprep #}
			outBuf csize c_flags profile
		
		let rc = fromIntegral c_rc
		if rc == fromEnum TOO_SMALL_BUFFER
			then return Nothing
			else if rc == fromEnum OK
				then fmap (Just . Right) (B.packCString outBuf)
				else return (Just (Left c_rc))

encodeFlags :: Flags -> CInt
encodeFlags flags = foldr (.|.) 0 bits where
	bitAt f e = if f flags then 0 else fromIntegral (fromEnum e)
	bits = [ bitAt enableNFKC NO_NFKC
	       , bitAt enableBidi NO_BIDI
	       , bitAt allowUnassigned NO_UNASSIGNED
	       ]

cToError :: CInt -> Error
cToError rc = StringPrepError (T.pack str) where
	c_strerror = {# call stringprep_strerror #}
	str = Unsafe.unsafePerformIO (c_strerror rc >>= peekCString)

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
