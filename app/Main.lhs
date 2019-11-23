> module Main where
>
> import Options.Applicative
> import Data.Semigroup ((<>))
> import Data.Maybe (maybe)
> import System.Console.Terminal.Size
> import Lib

The `tutorial` subcommand provides the user with a short and simple tutorial style lesson. A Tutorial
tends to cover a broader range of content, but in less detail than a Concept should. Tutorials also
tend to be less interactive and more linear, to give a quick introduction for a user who plans to 
continue on their own.

A Tutorial does not leave any data on the user's device, instead just running the Tutorial again
would be sufficient to review the entire experience.

> data TutorialArgs = TutorialArgs
>     { tutorialName :: Maybe String
>     , tutorialSearch :: Bool
>     } deriving (Show)
>     
> tutorialArgs :: Parser TutorialArgs
> tutorialArgs = TutorialArgs
>     <$> optional (strOption
>         (  long "name"
>         <> short 'n'
>         <> metavar "<name>"
>         <> help "The name of the tutorial"))
>     <*> switch 
>         (  long "search"
>         <> short 's'
>         <> help "Just search for matching tutorials, don't actually start one")

The `problem` subcommand is intended to present a standalone programming problem to the user.
They should write up a solution to this problem, and then submit it with the `submit` command.

At the end of a Problem, the user should be left with the problem specification and their solution
to the problem in the corresponding subdirectory of the problems root directory. Attempting the
same Problem in another language should add another solution to their existing directory, so the 
user may look back on and improve their solution to the problem in the future.

> data ProblemArgs = ProblemArgs
>     { problemName :: Maybe String
>     , problemConcepts :: [String]
>     , problemLang :: Maybe String
>     , problemLevel :: Int
>     , problemSearch :: Bool
>     } deriving (Show)
>
> problemArgs :: Parser ProblemArgs
> problemArgs = ProblemArgs
>     <$> optional (argument str
>         (  metavar "<name>"
>         <> help "The name of a specific problem to attempt"))
>     <*> many (strOption
>         (  long "concept"
>         <> short 'c'
>         <> metavar "<concept>"
>         <> help "A concept that the problem should cover"))
>     <*> optional (strOption
>         (  long "language"
>         <> short 'l'
>         <> metavar "<lang>"
>         <> help "The programming language you will write the solution in"))
>     <*> option auto
>         (  long "level"
>         <> short 'v'
>         <> metavar "<level>"
>         <> value 3
>         <> help "The difficulty level, 1 being the the easiest")
>     <*> switch 
>         (  long "search"
>         <> short 's'
>         <> help "Just search for matching problems, don't actually assign one")

The `concept` command is intended to teach the user a new concept. The concept can be presented
incrementally, providing a [lesson -> guided exercise -> additional exercise -> evaluation -> ...]
flow, building up a solid understanding of the concept. Multiple concepts may be started at one time,
and their progress can be tracked independently.

At the end of a concept, the user should be left with the entire lesson and their solutions to the
exercise in a well organized directory in the concepts root directory. They should be able to review
the material and the exercises at any time in the future.

> data ConceptArgs = ConceptArgs
>     { conceptName :: Maybe String
>     , conceptLang :: Maybe String
>     , conceptSearch :: Bool
>     } deriving (Show)
>
> conceptArgs :: Parser ConceptArgs
> conceptArgs = ConceptArgs
>     <$> optional (argument str
>         (  metavar "<name>"
>         <> help "The name of a specific concept to learn"))
>     <*> optional (strOption
>         (  long "language"
>         <> short 'l'
>         <> metavar "<lang>"
>         <> help "The programming language you are learning the concept for"))
>     <*> switch
>         (  long "search"
>         <> short 's'
>         <> help "Just search for matching concepts, don't actually provide one")

The `project` command gives a simple project for the user. A Project tends to be more involved than
a Problem. Rather than requiring the implementation or application of just one concept, the Project
combines a number of concepts and an architectural component, usually requiring at least a few files
to complete.

At the end of a Project, the user should be left with the project specification and a working application
that matches said specification. They should be able to return to the Project and improve their solution,
or even just use the working application for its purpose, as it should probably be somewhat practical.

> data ProjectArgs = ProjectArgs
>     { projectName :: Maybe String
>     , projectConcepts :: [String]
>     , projectSearch :: Bool
>     } deriving (Show)
> 
> projectArgs :: Parser ProjectArgs
> projectArgs = ProjectArgs
>     <$> optional (argument str
>         (  metavar "<name>"
>         <> help "The name of a specific project to build"))
>     <*> many (strOption
>         (  long "concept"
>         <> short 'c'
>         <> metavar "<concept>"
>         <> help "A concept that the project should cover"))
>     <*> switch
>         (  long "search"
>         <> short 's'
>         <> help "Just search for matching projects, don't actually start one")

We combine all the subcommands here, into the command type, and then handle each appropriately
in the `main` function.

> data Command
>     = Tutorial TutorialArgs
>     | Problem ProblemArgs
>     | Concept ConceptArgs
>     | Project ProjectArgs
>     deriving (Show)
>
> commandParser :: Parser Command
> commandParser = hsubparser
>     ( command "tutorial" (info (Tutorial <$> tutorialArgs) (progDesc "Go through a tutorial to get a brief overview on a topic"))
>    <> command "problem" (info (Problem <$> problemArgs) (progDesc "Start a coding problem to test your skills"))
>    <> command "concept" (info (Concept <$> conceptArgs) (progDesc "Learn a new concept"))
>    <> command "project" (info (Project <$> projectArgs) (progDesc "Start a project to practice combining a few concepts"))
>     )
>
> parseArgs :: IO Command
> parseArgs = do
>     terminalWidth <- maybe 80 width <$> size
>     let preferences = prefs (showHelpOnEmpty <> showHelpOnError <> columns (min terminalWidth 100)) in
>       customExecParser preferences $ info (helper <*> commandParser)
>         (  fullDesc
>         <> header "Help yourself to become a better programmer")
>
> main :: IO ()
> main = do
>     args <- parseArgs
>     case args of
>         Tutorial args -> tutorial args
>         Problem args -> problem args
>         Concept args -> concept args
>         Project args -> project args
>
> tutorial :: TutorialArgs -> IO ()
> tutorial _ = putStrLn "Tutorial"
>
> problem :: ProblemArgs -> IO ()
> problem _ = putStrLn "Problem"
>
> concept :: ConceptArgs -> IO ()
> concept _ = putStrLn "Concept"
> 
> project :: ProjectArgs -> IO ()
> project _ = putStrLn "Project"
