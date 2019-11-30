module Commands.Tutorial (tutorial) where
    import Control.Monad
    import Data.Maybe

    import qualified Lib.Repo as Repo
    import qualified Lib.Tutorial as Tutorial

    import Repo (tutorialsRepo)
    import Args (TutorialArgs (..))

    tutorial :: TutorialArgs -> IO (Either String ())
    tutorial (TutorialArgs "something" _) = return $ Left "Topic suggestion not yet implemented"
    tutorial (TutorialArgs _ True) = return $ Left "Search not yet implemented"
    tutorial (TutorialArgs name _) = do
        repo <- tutorialsRepo
        tutorialPath <- ioEitherMap (fmap (maybe notFoundError Right) . Repo.lookup name) repo
        tutorialSpec <- ioEitherMap Tutorial.spec tutorialPath
        print tutorialSpec
        return $ Right ()
        where
            notFoundError = Left $ "No Tutorial named " ++ name ++ " was found. Maybe you should try writing it!"
            ioEitherMap = either (return . Left)
