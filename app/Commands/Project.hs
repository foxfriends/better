module Commands.Project (project) where
    import Args (ProjectArgs)

    project :: ProjectArgs -> IO (Either String ())
    project _ = do
        putStrLn "Project"
        return $ Right ()
