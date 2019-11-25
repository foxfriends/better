module Dirs (getRepoDir, getConfDir, getWorkDir) where
    import System.FilePath ((</>))
    import System.Directory (getHomeDirectory, getCurrentDirectory)

    getRepoDir :: IO FilePath
    -- final implementation maybe like this
    -- getRepoDir = (++ ".local/share/better/") <$> getHomeDirectory
    -- for now, we just use this one:
    getRepoDir = (</> "data") <$> getCurrentDirectory

    getConfDir :: IO FilePath
    getConfDir = (</> ".config/better/") <$> getHomeDirectory

    getWorkDir :: IO FilePath
    getWorkDir = (</> "/Documents/better") <$> getHomeDirectory
