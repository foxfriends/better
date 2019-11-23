module Commands.Tutorial (tutorial) where
    import Args (TutorialArgs)

    tutorial :: TutorialArgs -> IO ()
    tutorial _ = putStrLn "Tutorial"
