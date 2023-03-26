import System.Process

main = do
    _ <- createProcess $ shell "fish redo.do"
    return ()

