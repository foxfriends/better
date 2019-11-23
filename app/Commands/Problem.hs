module Commands.Problem (problem) where
    import Args (ProblemArgs)

    problem :: ProblemArgs -> IO ()
    problem _ = putStrLn "Problem"
