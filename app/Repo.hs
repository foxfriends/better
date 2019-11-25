module Repo (tutorialsRepo) where
    import System.FilePath ((</>))
    import Dirs (getRepoDir)
    import Lib.Repo as Repo

    tutorialsRepo :: IO (Maybe Repo.Repo)
    tutorialsRepo = getRepoDir >>= Repo.load . (</> "tutorial")
