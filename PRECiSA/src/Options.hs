module Options where

import Options.Applicative
import Data.Monoid ((<>))

newtype Options = Options
   { optCommand :: Command
   } deriving Show

data Command = Analyze  AnalyzeOptions 
             | Generate GenerateOptions
  deriving Show

data AnalyzeOptions = AnalyzeOptions
   { optProgramFile          :: FilePath
   , optInputRangeFile       :: FilePath
   , optPathFile             :: FilePath
   , optMaxDepth             :: Int
   , optPrecision            :: Int
   , optMaxNumLemma          :: Int
   , optAssumeStability      :: Bool
   , optNoCollapsedUnstables :: Bool
   , optSMTOptimization      :: Bool
   } deriving Show

data GenerateOptions = GenerateOptions
   { optRealProgramFile      :: FilePath
   , optRealInputRangeFile   :: FilePath
   , targetFormat            :: String               
   } deriving Show

optionsParser :: Parser Options
optionsParser = Options <$> commandParser
   

commandParser :: Parser Command
commandParser = 
  subparser
    ( command "analyze"  (info (Analyze  <$> analyzeOptions)  ( progDesc "Analyze a FP program" ))
   <> command "gen-code" (info (Generate <$> generateOptions) ( progDesc "Generate a stable C FP version of a real PVS program" ))
    )

generateOptions :: Parser GenerateOptions
generateOptions =
    GenerateOptions
        <$> strArgument
           ( metavar "PROGRAM"
          <> help "Program to analyze")
        <*> strArgument
           ( metavar "INPUT"
          <> help "Input ranges for variables")
        <*> strOption
           ( long "format"
          <> short 'f'
          <> showDefault
          <> value "double"
          <> help "Target floating-point format (single or double)"
          <> metavar "PREC"
          ) 

analyzeOptions :: Parser AnalyzeOptions
analyzeOptions =
    AnalyzeOptions
        <$> strArgument
           ( metavar "PROGRAM"
          <> help "Program to analyze")
        <*> strArgument
           ( metavar "INPUT"
          <> help "Input ranges for variables")
        <*> strOption
          ( long "paths"
         <> showDefault
         <> value ""
         <> help "Decision paths of interest"
         <> metavar "PATHS"
         )
        <*> option auto
           (  long "max-depth"
           <> short 'd'
           <> showDefault
           <> value 7
           <> help "Maximum depth of branch-and-bound"
           <> metavar "BB_MAX_DEPTH"
           )            
        <*> option auto
          (  long "precision"
          <> short 'p'
          <> help "Precision"
          <> showDefault
          <> value 14
          <> metavar "BB_PREC" )
        <*> option auto
          (  long "max-lemmas"
          <> short 'l'
          <> help "Maximum number of lemmas"
          <> showDefault
          <> value 50
          <> metavar "MAX_N_LEMMAS" )
        <*> switch
          (  long "assume-stability"
          <> short 's'
          <> help "Enable stable test assumption" )
        <*> switch
          (  long "no-collapsed-unstables"
          <> short 'u'
          <> help "Do not collapse unstable cases" )
        <*> switch
          (  long "smt-optimization"
          <> help "Use SMT solvers to elimiate unfeasible cases" )


parseOptions :: IO Options
parseOptions = execParser parserOpts
    where parserOpts = info (optionsParser <**> helper)
                             ( fullDesc
                            <> progDesc "PRECiSA Floating Point analyzer"
                            <> header "...to fill with a header" )
