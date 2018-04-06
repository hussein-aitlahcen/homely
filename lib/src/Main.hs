-- Main.hs ---

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

{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad         ((>=>))
import           Data.Aeson            (eitherDecode, encode)
import qualified Data.ByteString.Lazy  as B
import           Data.Semigroup        ((<>))
import           System.Directory.Tree (readDirectoryWith, writeDirectoryWith)
import           System.Environment    (getArgs)
import           System.Posix.Files    (createSymbolicLink)
import           Types

data Command = Bundle | Deliver | Unknown

getCommand :: String -> Command
getCommand "bundle"  = Bundle
getCommand "deliver" = Deliver
getCommand _         = Unknown

main :: IO ()
main = let executeCommand = \case
             [command, i, o] -> process (getCommand command) i o
             _               -> putStrLn "Invalid arguments."
       in executeCommand =<< getArgs
  where
    process Bundle  = bundle
    process Deliver = deliver
    process Unknown = const . const $ putStrLn "Unknown command."

bundle :: FilePath -> FilePath -> IO ()
bundle i o = B.writeFile o . encode =<< readDirectoryWith pure i

deliver :: FilePath -> FilePath -> IO ()
deliver i o =
  let link = writeDirectoryWith (flip createSymbolicLink) . replaceRoot o
      write = \case
        Right _      -> putStrLn   "Sucessfully linked files."
        Left message -> putStrLn $ "Delivery failed: " <> message
  in B.readFile i >>= (mapM link . decodeDirectory >=> write)
  where
    decodeDirectory :: B.ByteString -> Either String (WithFilePath AnchoredDirTree)
    decodeDirectory = eitherDecode

    replaceRoot :: String -> WithFilePath AnchoredDirTree -> WithFilePath AnchoredDirTree
    replaceRoot newRoot (_ :/ tree) = newRoot :/ tree
