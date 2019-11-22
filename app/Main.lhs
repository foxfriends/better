> module Main where
>
> import Options.Applicative
> import Data.Semigroup ((<>))
> import Lib

The `problem` subcommand is intended to present a standalone programming problem to the user.
They should write up a solution to this problem, and then submit it with the `submit` command.

At the end of a problem, the user should be left with the problem specification and their solution
to the problem in the corresponding subdirectory of the problems root directory. Attempting the
same problem in another language should add another solution, so the user may look back on and
improve their solution to the problem in the future.

> data ProblemArgs = ProblemArgs
>     { problemName :: Maybe String
>     , problemLang :: String
>     , problemLevel :: Int
>     } deriving (Show)
> 
> problemArgs :: Parser ProblemArgs
> problemArgs = ProblemArgs
>     <$> optional (strOption
>         (  long "name"
>         <> short 'n'
>         <> metavar "<name>"
>         <> help "The name of a problem to try"))
>     <*> strOption
>         (  long "language"
>         <> short 'l'
>         <> metavar "<lang>"
>         <> help "The programming language you will write the solution in")
>     <*> option auto
>         (  long "level"
>         <> short 'v'
>         <> metavar "<level>"
>         <> value 3
>         <> help "The difficulty level, 1 being the the easiest")

The `concept` command is intended to teach the user a new concept. The concept can be presented
incrementally, providing a [lesson -> guided exercise -> additional exercise -> evaluation -> ...]
flow, building up a solid understanding of the concept. Multiple concepts may be started at one time,
and their progress can be tracked independently.

At the end of a concept, the user should be left with the entire lesson and their solutions to the
exercise in a well organized directory in the concepts root directory. They should be able to review
the material and the exercises at any time in the future.

> data ConceptArgs = ConceptArgs
>     { conceptName :: String
>     , conceptLang :: String
>     } deriving (Show)
> 
> conceptArgs :: Parser ConceptArgs
> conceptArgs = ConceptArgs
>     <$> argument auto
>         (  metavar "<problem-name>"
>         <> help "The name of a problem to try")
>     <*> strOption
>         (  long "language"
>         <> short 'l'
>         <> metavar "<lang>"
>         <> help "The programming language you are learning the concept for")

We combine the `problem` and `concept` args parsers here, adding too the `tutorial` command which will
give a user who is brand new to development a full tutorial on how to use the command line, and in particular
how to use the `better` application to explore the world of programming further.

> data Command
>     = Tutorial
>     | Problem ProblemArgs
>     | Concept ConceptArgs
>     deriving (Show)
> 
> commandParser :: Parser Command
> commandParser = hsubparser
>     ( command "tutorial" (info (pure Tutorial) (progDesc "A good place to start if you are new to the command line"))
>    <> command "problem" (info (Problem <$> problemArgs) (progDesc "Start a coding problem to test your skills"))
>    <> command "concept" (info (Concept <$> conceptArgs) (progDesc "Learn a new concept"))
>     )
> 
> parseArgs :: IO Command
> parseArgs = execParser $ info (helper <*> commandParser)
>     (fullDesc
>     <> progDesc "Help yourself to become a better programmer"
>     <> header "Header text")
> 
> main :: IO ()
> main = parseArgs >>= print
