{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (catch, IOException)
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.MD5
import Data.Maybe
import Data.Map.Lazy
import Data.Typeable
import Debug.Trace
import GHC.IO.Exception
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.Process

main :: IO ()
main = mapM_ redo =<< getArgs

traceShow' arg = traceShow arg arg

redo :: String -> IO ()
redo target = do
    upToDate' <- upToDate target
    unless upToDate' $ maybe printMissing redo' =<< redoPath target
  where
    redo' :: FilePath -> IO ()
    redo' path = do
      oldEnv <- getEnvironment
      let newEnv = toList $ adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv
      (_, _, _, ph) <- createProcess $ (shell $ cmd path) {env = Just newEnv}
      exit <- waitForProcess ph
      case exit of
        ExitSuccess -> do renameFile tmp target
        ExitFailure code -> do
          hPutStrLn stderr $ "Redo script exited with non-zero exit code: " ++ show code
          removeFile tmp
    tmp = target ++ "---redoing"
    printMissing = error $ "No .do file found for target '" ++ target ++ "'"
    cmd path =
        unwords ["sh", path, "0", takeBaseName target, tmp, " > ", tmp]

redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = listToMaybe <$> filterM doesFileExist candidates
  where
    candidates = (target ++ ".do") : [replaceBaseName target "default" ++ ".do" | hasExtension target]

upToDate :: String -> IO Bool
upToDate target = catch
    (do deps <- getDirectoryContents depDir
        all id `liftM` mapM depUpToDate deps)
    (\(e :: IOException) -> return False)
    where
      depDir = ".redo" </> target
      depUpToDate :: FilePath -> IO Bool
      depUpToDate dep = catch
        (do oldMD5 <- withFile (depDir </> dep) ReadMode hGetLine
            newMD5 <-  md5 `liftM` BL.readFile dep
            return $ oldMD5 == show newMD5)
        (\e -> return (ioeGetErrorType e == InappropriateType))

