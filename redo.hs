import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process

main = do
  args <- getArgs
  mapM redo args

redo :: String -> IO ()
redo target = do
  let tmp = target ++ "---redoing"
  (_, _, _, ph) <- createProcess $ shell $ "sh " ++ target ++ ".do - - " ++ tmp ++ " > " ++ tmp
  exit <- waitForProcess ph
  case exit of
    ExitSuccess -> do renameFile tmp target
    ExitFailure code -> do
      hPutStrLn stderr $ "Redo script exited with non-zero exit code: " ++ show code
      removeFile tmp
