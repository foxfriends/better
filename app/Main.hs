module Main where
    import System.IO
    import Args
    import Commands.Concept
    import Commands.Problem
    import Commands.Project
    import Commands.Tutorial

    main :: IO ()
    main = do
        args <- parseArgs
        result <- case args of
            Tutorial args -> tutorial args
            Problem args -> problem args
            Concept args -> concept args
            Project args -> project args
        case result of
            Left problem -> hPutStrLn stderr problem
            Right () -> return ()
