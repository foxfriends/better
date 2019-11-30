{-# LANGUAGE DeriveGeneric #-}
module Lib.Tutorial.Module (Module (..), moduleCodec) where
    import GHC.Generics
    import Data.Text
    import Toml

    data Module = Module
        { src :: Text
        } deriving (Show, Generic)

    moduleCodec :: TomlCodec Module
    moduleCodec = genericCodec

    instance HasCodec Module where
        hasCodec = Toml.table moduleCodec

    instance HasItemCodec Module where
        hasItemCodec = Right moduleCodec
