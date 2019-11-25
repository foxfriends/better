module Lib.Repo (Repo, load, lookup) where
    import Prelude hiding (lookup)
    import Control.Monad (filterM)
    import Data.Map (Map)
    import qualified Data.Map as Map
    import System.FilePath ((</>))
    import System.Directory (listDirectory, doesDirectoryExist)
    type Repo = Map String FilePath

    load :: FilePath -> IO (Maybe Repo)
    load path = do
        contents <- listDirectory path
        let names = zip contents (fmap (path </>) contents) in do
            directoryNames <- filterM (doesDirectoryExist . snd) names
            return $ Just $ Map.fromList directoryNames

    lookup :: String -> Repo -> IO (Maybe FilePath)
    lookup name repo = return $ Map.lookup name repo
