module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Lib

data ProblemArgs = ProblemArgs
    { problemName :: Maybe String
    , problemLang :: String
    , problemLevel :: Int
    } deriving (Show)

problemArgs :: Parser ProblemArgs
problemArgs = ProblemArgs
    <$> optional (strOption
        (  long "name"
        <> short 'n'
        <> metavar "<name>"
        <> help "The name of a problem to try"))
    <*> strOption
        (  long "language"
        <> short 'l'
        <> metavar "<lang>"
        <> help "The programming language you will write the solution in")
    <*> option auto
        (  long "level"
        <> short 'v'
        <> metavar "<level>"
        <> value 3
        <> help "The difficulty level, 1 being the the easiest")

data ConceptArgs = ConceptArgs
    { conceptName :: String
    , conceptLang :: String
    } deriving (Show)

conceptArgs :: Parser ConceptArgs
conceptArgs = ConceptArgs
    <$> argument auto
        (  metavar "<problem-name>"
        <> help "The name of a problem to try")
    <*> strOption
        (  long "language"
        <> short 'l'
        <> metavar "<lang>"
        <> help "The programming language you are learning the concept for")

data Command
    = Tutorial
    | Problem ProblemArgs
    | Concept ConceptArgs
    deriving (Show)

commandParser :: Parser Command
commandParser = hsubparser
    ( command "tutorial" (info (pure Tutorial) (progDesc "A good place to start if you are new to the command line"))
   <> command "problem" (info (Problem <$> problemArgs) (progDesc "Start a coding problem to test your skills"))
   <> command "concept" (info (Concept <$> conceptArgs) (progDesc "Learn a new concept"))
    )

parseArgs :: IO Command
parseArgs = execParser $ info (helper <*> commandParser)
    (fullDesc
    <> progDesc "Help yourself to become a better programmer"
    <> header "Header text")

main :: IO ()
main = parseArgs >>= putStrLn . show
