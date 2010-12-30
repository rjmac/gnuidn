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

module Data.Text.IDN.Internal
	( Error (..)
	, toUCS4
	, fromUCS4
	) where

import qualified Data.Text as T
import Data.Char (chr, ord)

import qualified Foreign as F

data Error = IDNAError T.Text
           | StringPrepError T.Text
	deriving (Show, Eq)

toUCS4 :: T.Text -> [F.Word32]
toUCS4 = map (fromIntegral . ord) . T.unpack

fromUCS4 :: [F.Word32] -> T.Text
fromUCS4 = T.pack . map (chr . fromIntegral)
