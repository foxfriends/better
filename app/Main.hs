module Main where
    import Args
    import Commands.Concept
    import Commands.Problem
    import Commands.Project
    import Commands.Tutorial

    main :: IO ()
    main = do
        args <- parseArgs
        case args of
            Tutorial args -> tutorial args
            Problem args -> problem args
            Concept args -> concept args
            Project args -> project args
