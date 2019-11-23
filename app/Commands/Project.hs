module Commands.Project (project) where
    import Args (ProjectArgs)

    project :: ProjectArgs -> IO ()
    project _ = putStrLn "Project"
