module Commands.Tutorial (tutorial) where
    import Repo (tutorialsRepo)
    import qualified Lib.Repo as Repo
    import Args (TutorialArgs (..))

    tutorial :: TutorialArgs -> IO ()
    tutorial (TutorialArgs "something" _) = putStrLn "Topic suggestion not yet implemented"
    tutorial (TutorialArgs _ True) = putStrLn "Search not yet implemented"
    tutorial (TutorialArgs name _) = do
        Just repo <- tutorialsRepo
        tutorial <- Repo.lookup name repo
        print tutorial
