module Lib.Tutorial (Tutorial, Spec, Module, spec) where
    import Data.Text.IO as TIO
    import Data.Bifunctor
    import System.FilePath ((</>))

    import qualified Toml

    import Lib.Repo (Content (..))
    import Lib.Tutorial.Spec (Spec, specCodec)
    import Lib.Tutorial.Module (Module)

    newtype Tutorial = Tutorial FilePath deriving (Show)
    instance Content Tutorial where
        fromFilePath = Tutorial

    spec :: Tutorial -> IO (Either String Spec)
    spec (Tutorial path) = do
        src <- TIO.readFile (path </> "spec.toml")
        return . first show $ Toml.decode specCodec src
