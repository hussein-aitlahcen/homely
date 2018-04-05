-- Types.hs ---

-- Copyright (C) 2018 Hussein Ait-Lahcen

-- Author: Hussein Ait-Lahcen <hussein.aitlahcen@gmail.com>

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

-- Sorry guys
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  (
    WithFilePath,
    AnchoredDirTree (..),
    DirTree (..)
  )
  where

import           Data.Aeson            (FromJSON (..), ToJSON (..), object,
                                        withObject, (.:), (.=))
import           System.Directory.Tree (AnchoredDirTree (..), DirTree (..))

type WithFilePath f = f FilePath

fileType :: String
fileType = "file"

dirType :: String
dirType = "directory"

instance FromJSON (WithFilePath AnchoredDirTree) where
  parseJSON = withObject "root" $ \o ->
    (:/) <$> o .: "root" <*> o .: "tree"

instance ToJSON (WithFilePath AnchoredDirTree) where
  toJSON ((:/) a t) = object ["root" .= a, "tree" .= t]

instance FromJSON (WithFilePath DirTree) where
  parseJSON = withObject "tree" $ \o -> do
    kind <- o .: "kind"
    case kind of
      "directory" -> Dir  <$> o .: "name" <*> o .: "contents"
      "file"      -> File <$> o .: "name" <*> o .: "absolute"
      _           -> fail ("invalid tree object: " ++ kind)

instance ToJSON (WithFilePath DirTree) where
  toJSON (Dir p c)    = object ["kind" .= dirType,  "name" .= p, "contents" .= c]
  toJSON (File p c)   = object ["kind" .= fileType, "name" .= p, "absolute" .= c]
  toJSON (Failed _ _) = error "errors should not be serialized"
