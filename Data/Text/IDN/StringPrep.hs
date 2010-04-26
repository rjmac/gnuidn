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
	, Error (..)
	, defaultFlags
	, stringprep
	
	-- * Profiles
	, Profile
	, profileNameprep
	, profileSaslPrep
	, profilePlain
	, profileTrace
	, profileKerberos5
	, profileNodeprep
	, profileResourceprep
	, profileISCSI
	) where
import Data.Bits ((.|.))
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Foreign as F
import qualified Foreign.C as F
import System.IO.Unsafe (unsafePerformIO)

data Error
	= ErrorContainsUnassigned
	| ErrorContainsProhibited
	| ErrorBidiBothLAndRAL
	| ErrorBidiLeadTrailNotRAL
	| ErrorBidiContainsProhibited
	| ErrorInconsistentProfile
	| ErrorInvalidFlag
	| ErrorNormalisationFailed
	| ErrorUnknown Integer
	deriving (Show, Eq)

data Flags = Flags
	{ enableNFKC :: Bool
	, enableBidi :: Bool
	, allowUnassigned :: Bool
	}

newtype Profile = Profile (F.Ptr Profile)

defaultFlags :: Flags
defaultFlags = Flags True True False

stringprep :: Profile -> Flags -> T.Text -> Either Error T.Text
stringprep profile flags input = unsafePerformIO io where
	utf8 = TE.encodeUtf8 input
	cflags = encodeFlags flags
	len = B.length utf8
	io = B.useAsCString utf8 (loop len)
	loop bufsize pIn = F.allocaBytes bufsize $ \buf -> do
		F.copyArray buf pIn len
		let csize = fromIntegral bufsize
		rc <- c_stringprep buf csize cflags profile
		case rc of
			-- TOO_SMALL_BUFFER
			100 -> loop (bufsize + 50) pIn
			
			0 -> do
				bytes <- B.packCString buf
				return $ Right $ TE.decodeUtf8 bytes
			_ -> return $ Left $ cToError rc

encodeFlags :: Flags -> F.CInt
encodeFlags flags = foldr (.|.) 0 bits where
	bit f x y = if f flags then x else y
	bits = [ bit enableNFKC 0 1
	       , bit enableBidi 0 2
	       , bit allowUnassigned 0 4
	       ]

cToError :: F.CInt -> Error
cToError x = case x of
	1 -> ErrorContainsUnassigned
	2 -> ErrorContainsProhibited
	3 -> ErrorBidiBothLAndRAL
	4 -> ErrorBidiLeadTrailNotRAL
	5 -> ErrorBidiContainsProhibited
	101 -> ErrorInconsistentProfile
	102 -> ErrorInvalidFlag
	200 -> ErrorNormalisationFailed
	_ -> ErrorUnknown $ toInteger x

foreign import ccall unsafe "stringprep"
	c_stringprep :: F.CString -> F.CSize -> F.CInt -> Profile -> IO F.CInt

foreign import ccall "&stringprep_nameprep"
	profileNameprep :: Profile

foreign import ccall "&stringprep_saslprep"
	profileSaslPrep :: Profile

foreign import ccall "&stringprep_plain"
	profilePlain :: Profile

foreign import ccall "&stringprep_trace"
	profileTrace :: Profile

foreign import ccall "&stringprep_kerberos5"
	profileKerberos5 :: Profile

foreign import ccall "&stringprep_xmpp_nodeprep"
	profileNodeprep :: Profile

foreign import ccall "&stringprep_xmpp_resourceprep"
	profileResourceprep :: Profile

foreign import ccall "&stringprep_iscsi"
	profileISCSI :: Profile
