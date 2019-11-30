module Lib.Repo (Repo, Content (fromFilePath), load, lookup) where
    import Prelude hiding (lookup)
    import Control.Monad (filterM)
    import Data.Map (Map)
    import qualified Data.Map as Map
    import System.FilePath ((</>))
    import System.Directory (listDirectory, doesDirectoryExist)

    class Content c where
        fromFilePath :: FilePath -> c

    newtype Repo c = Repo (Map String c)

    load :: (Content c) => FilePath -> IO (Either String (Repo c))
    load path = do
        contents <- listDirectory path
        let names = zip contents (fmap (path </>) contents) in do
            directoryNames <- filterM (doesDirectoryExist . snd) names
            return $ Right $ Repo $ Map.fromList (fmap (\(a, b) -> (a, fromFilePath b)) directoryNames)

    lookup :: String -> Repo c -> IO (Maybe c)
    lookup name (Repo repo) = return $ Map.lookup name repo
