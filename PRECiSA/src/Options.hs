-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module Options where

import Options.Applicative

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
   , optWithPaving           :: Bool
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
        <*> switch
          (  long "paving"
          <> help "Generate a paving of the regions of unstability"
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
