> module Args
>     ( Command (..)
>     , TutorialArgs (..)
>     , ProblemArgs (..)
>     , ConceptArgs (..)
>     , ProjectArgs (..)
>     , parseArgs ) where
>
> import Options.Applicative
> import Data.Semigroup ((<>))
> import Data.Maybe (maybe)
> import System.Console.Terminal.Size

The `tutorial` subcommand provides the user with a short and simple tutorial style lesson. A Tutorial
tends to cover a broader range of content, but in less detail than a Concept should. Tutorials also
tend to be less interactive and more linear, to give a quick introduction for a user who plans to 
continue on their own.

A Tutorial does not leave any data on the user's device, instead just running the Tutorial again
would be sufficient to review the entire experience.

> data TutorialArgs = TutorialArgs
>     { tutorialName :: String
>     , tutorialSearch :: Bool
>     } deriving (Show)
>     
> tutorialArgs :: Parser TutorialArgs
> tutorialArgs = TutorialArgs
>     <$> argument str
>         (  metavar "<something>"
>         <> value "something"
>         <> help "What you want to learn. \"learn something\" and we'll pick for you.")
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
>     { problemName :: String
>     , problemConcepts :: [String]
>     , problemLang :: Maybe String
>     , problemLevel :: Int
>     , problemSearch :: Bool
>     } deriving (Show)
>
> problemArgs :: Parser ProblemArgs
> problemArgs = ProblemArgs
>     <$> argument str
>         (  metavar "<something>"
>         <> value "something"
>         <> help "What you want to practice. \"practice something\" and we'll pick for you.")
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
>     { conceptName :: String
>     , conceptLang :: Maybe String
>     , conceptSearch :: Bool
>     } deriving (Show)
>
> conceptArgs :: Parser ConceptArgs
> conceptArgs = ConceptArgs
>     <$> argument str
>         (  metavar "<something>"
>         <> value "something"
>         <> help "What you want to learn. \"at something\" and we'll pick for you.")
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
>     { projectName :: String
>     , projectConcepts :: [String]
>     , projectSearch :: Bool
>     } deriving (Show)
> 
> projectArgs :: Parser ProjectArgs
> projectArgs = ProjectArgs
>     <$> argument str
>         (  metavar "<something>"
>         <> value "something"
>         <> help "What you want to make. \"make something\" and we'll pick for you.")
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
>     ( command "learn" (info (Tutorial <$> tutorialArgs) (progDesc "You had better learn something quickly"))
>    <> command "practice" (info (Problem <$> problemArgs) (progDesc "You had better practice up on some skills"))
>    <> command "at" (info (Concept <$> conceptArgs) (progDesc "You should get better at something in particular"))
>    <> command "make" (info (Project <$> projectArgs) (progDesc "You had better make something useful"))
>     )
>
> parseArgs :: IO Command
> parseArgs = do
>     terminalWidth <- maybe 80 width <$> size
>     let preferences = prefs (showHelpOnEmpty <> showHelpOnError <> columns (min terminalWidth 100)) in
>       customExecParser preferences $ info (helper <*> commandParser)
>         (  fullDesc
>         <> header "Help yourself to become a better programmer")
