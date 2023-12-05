-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module Options where

import Options.Applicative

data Options = Options
   { optProgramFile          :: FilePath
   , optInputRangeFile       :: FilePath
   , optPathFile             :: FilePath
   , optParseFPCore          :: Bool
   , optParseFPCoreSpec      :: Bool
   , optPrintFPCore          :: Bool
   , optImproveError         :: Bool
   , optWithPaving           :: Bool
   , optMaxDepth             :: Int
   , optPrecision            :: Int
   , optMaxNumLemma          :: Int
   , optAssumeStability      :: Bool
   , optNoCollapsedStables   :: Bool
   , optNoCollapsedUnstables :: Bool
   , optSMTOptimization      :: Bool
   } deriving Show

optionsParser :: Parser Options
optionsParser =
    Options
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
          (  long "fpcore"
          <> help "Use FPCore as input. This option does not yet fully support FPCore."
          )
        <*> switch
          (  long "fpcore-spec"
          <> help "Try to infer input ranges from FPCore preconditions. Must use FPCore as input as well."
          )
        <*> switch
          (  long "print-fpcore"
          <> help "Print program as FPCore."
          )
        <*> switch
          (  long "improve-accuracy"
          <> help "Use an optimized version of the round-off error expressions that models special cases such as the Sterbenz exact subtraction and the exact floor operation. This option may increase the analysis time."
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
          (  long "no-collapsed-stables"
          <> help "Do not collapse stable paths. It may lead to a more accurate analysis since each path is analyzed separately." )
        <*> switch
          (  long "smt-optimization"
          <> help "Use SMT solvers to elimiate unfeasible cases" )

parseOptions :: IO Options
parseOptions = execParser parserOpts
    where parserOpts = info (optionsParser <**> helper)
                             ( fullDesc
                            <> progDesc "PRECiSA Floating Point analyzer"
                            <> header "...to fill with a header" )
