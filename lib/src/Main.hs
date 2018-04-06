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

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           Control.Monad.Except  (MonadError, MonadIO, liftIO, runExceptT,
                                        throwError)
import           Data.Aeson            (eitherDecode, encode)
import qualified Data.ByteString.Lazy  as B
import           Data.Semigroup        ((<>))
import           System.Directory.Tree (anyFailed, flattenDir,
                                        readDirectoryWith, writeDirectoryWith)
import           System.Environment    (getArgs)
import           System.Posix.Files    (createSymbolicLink)
import           Types

main :: IO ()
main =
  let execute = \case
        [command, i, o] -> process command i o
        _               -> throwError "Invalid arguments."
      finalize = \case
        Right _       -> putStrLn "Sucess"
        Left  message -> error $  "Failure: " <> message
  in
    getArgs
    >>= runExceptT . execute
    >>= finalize
  where
    process "bundle"  i o = bundle  i o
    process "deliver" i o = deliver i o
    process x         _ _ = throwError $ "Unknown command: " <> x

-- Don't ask my why I append "/./" =)
bundle :: (MonadIO m, MonadError String m) => FilePath -> FilePath -> m ()
bundle i o = liftIO  (B.writeFile o . encode =<< readDirectoryWith pure (i <> "/./"))

deliver :: (MonadIO m, MonadError String m) => FilePath -> FilePath -> m ()
deliver i o =
  let link = liftIO . writeDirectoryWith (flip createSymbolicLink) . replaceRoot o
      checkSuccess written
        | (anyFailed . dirTree) written = throwError $ "Delivery failure: " <> extractErrors (flattenDir $ dirTree written)
        | otherwise                     = pure ()
  in
    (liftIO . B.readFile) i
    >>= mapM link . decodeDirectory
    >>= either throwError checkSuccess
  where
    extractErrors :: [] (DirTree ()) -> String
    extractErrors [] = ""
    extractErrors (Failed p e:xs) = "path=" <> p <> ", error=" <> show e <> "\n" <> extractErrors xs
    extractErrors (_:xs) = extractErrors xs

    decodeDirectory :: (MonadError String m) => B.ByteString -> m (WithFilePath AnchoredDirTree)
    decodeDirectory = either throwError pure . eitherDecode

    replaceRoot :: String -> WithFilePath AnchoredDirTree -> WithFilePath AnchoredDirTree
    replaceRoot newRoot (_ :/ tree) = newRoot :/ tree

