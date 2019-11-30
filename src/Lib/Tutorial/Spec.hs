{-# LANGUAGE DeriveGeneric #-}
module Lib.Tutorial.Spec (Spec(..), specCodec) where
    import GHC.Generics
    import Data.Text
    import Lib.Tutorial.Module
    import Toml

    data Spec = Spec
        { name :: Text
        , description :: Text
        , level :: Int
        , modules :: [Module]
        } deriving (Show, Generic)

    specCodec :: TomlCodec Spec
    specCodec = genericCodec

    instance HasCodec Spec where
        hasCodec = Toml.table specCodec

    instance HasItemCodec Spec where
        hasItemCodec = Right specCodec
