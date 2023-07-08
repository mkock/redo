{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (IOException, catch, catchJust)
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.MD5 as MD5
import Data.Map.Lazy
import Data.Maybe
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
  topDir <- getCurrentDirectory
  getArgs
    >>= mapM_
      ( \arg -> do
          let (dir, file) = splitFileName arg
          setCurrentDirectory dir
          redo file dir
          setCurrentDirectory topDir
      )
  progName <- getProgName
  redoTarget' <- lookupEnv "REDO_TARGET"
  case (progName, redoTarget') of
    ("redo-ifchange", Just redoTarget) -> mapM_ (writeMD5 redoTarget) =<< getArgs
    ("redo-ifchange", Nothing) -> error "Missing REDO_TARGET environment variable"
    _invalid -> return ()

traceShow' arg = traceShow arg arg

redo :: String -> FilePath -> IO ()
redo target dir = do
  upToDate' <- upToDate target
  unless upToDate' $ maybe missingDo redo' =<< doPath target
  where
    redo' :: FilePath -> IO ()
    redo' path = do
      hPutStrLn stderr $ "redo " ++ (show dir ++ target)
      catchJust
        (guard . isDoesNotExistError)
        (removeDirectoryRecursive metaDepsDir)
        (\_ -> return ())
      createDirectoryIfMissing True metaDepsDir
      writeMD5 target path
      oldEnv <- getEnvironment
      let newEnv = toList $ adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv
      (_, _, _, ph) <- createProcess $ (shell $ cmd path) {env = Just newEnv}
      exit <- waitForProcess ph
      case exit of
        ExitSuccess -> do
          size <- fileSize tmp
          if size > 0
            then renameFile tmp target
            else removeFile tmp
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
upToDate target =
  catch
    ( do
        exists <- doesFileExist target
        if exists
          then do
            md5s <- getDirectoryContents (metaDir </> target)
            and `fmap` mapM depUpToDate md5s
          else return False
    )
    (\(_ :: IOException) -> return False)
  where
    depUpToDate :: String -> IO Bool
    depUpToDate oldMD5 =
      catch
        ( do
            dep <- withFile (metaDir </> target </> oldMD5) ReadMode hGetLine
            newMD5 <- fileMD5 dep
            doScript <- doPath dep
            case doScript of
              Nothing -> return (oldMD5 == newMD5)
              Just _ -> do
                upToDate' <- upToDate dep
                return $ (oldMD5 == newMD5) && upToDate'
        )
        (\e -> return (ioeGetErrorType e == InappropriateType))

fileMD5 :: FilePath -> IO String
fileMD5 path = (show . MD5.md5) `liftM` BL.readFile path

writeMD5 :: String -> FilePath -> IO ()
writeMD5 redoTarget dep = do
  md5 <- fileMD5 dep
  writeFile (metaDir </> redoTarget </> md5) dep

fileSize :: FilePath -> IO Integer
fileSize path = withFile path ReadMode hFileSize
