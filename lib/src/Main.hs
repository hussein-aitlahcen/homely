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

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

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

type AppContext m = (MonadIO m, MonadError String m)

main :: IO ()
main = getArgs
       >>= runExceptT . execute
       >>= finalize
  where
    finalize :: Either String a -> IO ()
    finalize = \case
        Right _       -> putStrLn "Sucess"
        Left  message -> error $  "Failure: " <> message

    execute ::  AppContext m => [String] -> m ()
    execute = \case
      [command, i, o] -> process command i o
      _               -> throwError "Invalid arguments."

    process ::  AppContext m => String -> FilePath -> FilePath -> m ()
    process "bundle"  i o = bundle  i o
    process "deliver" i o = deliver i o
    process x         _ _ = throwError $ "Unknown command: " <> x

-- Don't ask my why I append "/./" =)
bundle ::  AppContext m => FilePath -> FilePath -> m ()
bundle i o = liftIO  (B.writeFile o . encode =<< readDirectoryWith pure (i <> "/./"))

deliver ::  AppContext m => FilePath -> FilePath -> m ()
deliver i o = (liftIO . B.readFile) i
              >>= mapM (linkToDirectory o) . decodeDirectory
              >>= either throwError checkSuccess
  where
    linkToDirectory ::  AppContext m => String -> WithFilePath AnchoredDirTree -> m (AnchoredDirTree ())
    linkToDirectory d = liftIO . writeDirectoryWith (flip createSymbolicLink) . replaceRoot d

    checkSuccess ::  AppContext m => AnchoredDirTree () -> m ()
    checkSuccess written
      | (anyFailed . dirTree) written = throwError $ "Delivery failure: " <> extractErrors (flattenDir $ dirTree written)
      | otherwise                     = pure ()

    extractErrors :: [] (DirTree ()) -> String
    extractErrors [] = ""
    extractErrors (Failed p e:xs) = "path=" <> p <> ", error=" <> show e <> "\n" <> extractErrors xs
    extractErrors (_:xs) = extractErrors xs

    decodeDirectory :: (MonadError String m) => B.ByteString -> m (WithFilePath AnchoredDirTree)
    decodeDirectory = either throwError pure . eitherDecode

    replaceRoot :: String -> WithFilePath AnchoredDirTree -> WithFilePath AnchoredDirTree
    replaceRoot newRoot (_ :/ tree) = newRoot :/ tree

