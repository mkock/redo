import System.Environment
import System.Process

main = do
  args <- getArgs
  mapM redo args

redo :: String -> IO ()
redo target = do
  (_, _, _, ph) <- createProcess $ shell $ "fish " ++ target ++ ".do"
  _ <- waitForProcess ph
  return ()
