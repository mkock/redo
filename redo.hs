{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (catch, IOException, catchJust)
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

metaDir = ".redo"

main :: IO ()
main = do
    mapM_ redo =<< getArgs
    progName <- getProgName
    redoTarget' <- lookupEnv "REDO_TARGET"
    case (progName, redoTarget') of
        ("redo-ifchange", Just redoTarget) -> mapM_ (writeMD5 redoTarget) =<< getArgs
        ("redo-ifchange", Nothing) -> error "Missing REDO_TARGET environment variable"
        _ -> return ()
    where writeMD5 redoTarget dep = writeFile (metaDir </> redoTarget </> dep) =<< md5' dep

traceShow' arg = traceShow arg arg

redo :: String -> IO ()
redo target = do
    upToDate' <- upToDate target
    unless upToDate' $ maybe missingDo redo' =<< doPath target
  where
    redo' :: FilePath -> IO ()
    redo' path = do
        catchJust (guard . isDoesNotExistError)
                  (removeDirectoryRecursive $ metaDepsDir)
                  (\_ -> return ())
        createDirectoryIfMissing True $ metaDepsDir
        writeFile (metaDepsDir </> path) =<< md5' path
        oldEnv <- getEnvironment
        let newEnv = toList $ adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv
        (_, _, _, ph) <- createProcess $ (shell $ cmd path) {env = Just newEnv}
        exit <- waitForProcess ph
        case exit of
            ExitSuccess -> renameFile tmp target
            ExitFailure code -> do
            hPutStrLn stderr $ "Redo script exited with non-zero exit code: " ++ show code
            removeFile tmp
    tmp = target ++ "---redoing"
    metaDepsDir = metaDir </> target
    missingDo = do
        exists <- doesFileExist target
        unless exists $ error $ "No .do file found for target '" ++ target ++ "'"
    cmd path =
        unwords ["sh -x", path, "0", takeBaseName target, tmp, " > ", tmp]

doPath :: FilePath -> IO (Maybe FilePath)
doPath target = listToMaybe <$> filterM doesFileExist candidates
  where
    candidates = (target ++ ".do") : [replaceBaseName target "default" ++ ".do" | hasExtension target]

upToDate :: FilePath -> IO Bool
upToDate target = catch
    (do
        exists <- doesFileExist target
        if exists
        then do 
            deps <- getDirectoryContents (metaDir </> target)
            (traceShow' . and) `liftM` mapM depUpToDate deps
        else return False)
    (\(_ :: IOException) -> return False)
    where
      depUpToDate :: FilePath -> IO Bool
      depUpToDate dep = catch
        (do oldMD5 <- withFile (metaDir </> target </> dep) ReadMode hGetLine
            newMD5 <-  md5' dep
            doScript <- doPath dep
            case doScript of
                Nothing -> return $ oldMD5 == newMD5
                Just _ -> do 
                    upToDate' <- upToDate dep
                    return $ (oldMD5 == newMD5) && upToDate'
            return $ oldMD5 == newMD5)
        (\e -> return (ioeGetErrorType e == InappropriateType))

md5' :: FilePath -> IO String
md5' path = (show . md5) `liftM` BL.readFile path

