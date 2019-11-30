module Commands.Problem (problem) where
    import Args (ProblemArgs)

    problem :: ProblemArgs -> IO (Either String ())
    problem _ = do
        putStrLn "Problem"
        return $ Right ()
