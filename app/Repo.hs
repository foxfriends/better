module Repo (tutorialsRepo) where
    import System.FilePath ((</>))
    import Dirs (getRepoDir)
    import Lib.Repo (Repo)
    import qualified Lib.Repo as Repo
    import Lib.Tutorial (Tutorial)

    tutorialsRepo :: IO (Either String (Repo Tutorial))
    tutorialsRepo = getRepoDir >>= Repo.load . (</> "tutorial")
