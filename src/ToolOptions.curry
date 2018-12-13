-------------------------------------------------------------------------
--- The options of the residuation analysis tool.
---
--- @author Michael Hanus
--- @version December 2018
-------------------------------------------------------------------------

module ToolOptions
  ( Options(..), defaultOptions, processOptions
  , whenStatus, printWhenStatus, printWhenIntermediate, printWhenAll
  )
 where

import GetOpt
import ReadNumeric       ( readNat )
import System            ( exitWith )

import System.CurryPath  ( stripCurrySuffix )

data Options = Options
  { optVerb        :: Int  -- verbosity (0: quiet, 1: status, 2: intern, 3: all)
  , optHelp        :: Bool -- if help info should be printed
  , optTime        :: Bool -- show elapsed analysis time?
  , optShow        :: Bool -- show analysis results
  , optShowResOpts :: Bool -- show all possibly residuating operations
  , optShowStats   :: Bool -- show statistics in CSV format
  , optOutput      :: String -- output file (for term format)
  }

defaultOptions :: Options
defaultOptions = Options 1 False False False False False ""

--- Process the actual command line argument and return the options
--- and the name of the main program.
processOptions :: String -> [String] -> IO (Options,[String])
processOptions banner argv = do
  let (funopts, args, opterrors) = getOpt Permute options argv
      opts = foldl (flip id) defaultOptions funopts
  unless (null opterrors)
         (putStr (unlines opterrors) >> printUsage >> exitWith 1)
  when (optHelp opts) (printUsage >> exitWith 0)
  return (opts, map stripCurrySuffix args)
 where
  printUsage = putStrLn (banner ++ "\n" ++ usageText)

-- Help text
usageText :: String
usageText =
  usageInfo ("Usage: curry-anaresinfo [options] <module names>\n") options

-- Definition of actual command line options.
options :: [OptDescr (Options -> Options)]
options =
  [ Option "h?" ["help"]
           (NoArg (\opts -> opts { optHelp = True }))
           "print help and exit"
  , Option "q" ["quiet"]
           (NoArg (\opts -> opts { optVerb = 0 }))
           "run quietly (no output, only exit code)"
  , Option "v" ["verbosity"]
            (OptArg (maybe (checkVerb 2) (safeReadNat checkVerb)) "<n>")
            "verbosity level:\n0: quiet (same as `-q')\n1: show status messages (default)\n2: show intermediate results (same as `-v')\n3: show all details"
  , Option "o" ["output"]
           (ReqArg (\n opts -> opts { optOutput = n }) "<f>")
           "store analysis results as data term in <f>"
  , Option "s" ["show"]
           (NoArg (\opts -> opts { optShow = True }))
           "show analysis results"
  , Option "" ["resops"]
           (NoArg (\opts -> opts { optShowResOpts = True }))
           "show all possibly residuating operations"
  , Option "" ["stats"]
           (NoArg (\opts -> opts { optShowStats = True }))
           "show statistics in CSV format"
  , Option "t" ["time"]
           (NoArg (\opts -> opts { optTime = True }))
           "show total analysis time"
  ]
 where
  safeReadNat opttrans s opts =
   let numError = error "Illegal number argument (try `-h' for help)"
   in maybe numError
            (\ (n,rs) -> if null rs then opttrans n opts else numError)
            (readNat s)

  checkVerb n opts = if n>=0 && n<4
                     then opts { optVerb = n }
                     else error "Illegal verbosity level (try `-h' for help)"

-------------------------------------------------------------------------

whenStatus :: Options -> IO () -> IO ()
whenStatus opts = when (optVerb opts > 0)

printWhenStatus :: Options -> String -> IO ()
printWhenStatus opts s = whenStatus opts (putStrLn s)

printWhenIntermediate :: Options -> String -> IO ()
printWhenIntermediate opts s =
  when (optVerb opts > 1) (putStrLn s)

printWhenAll :: Options -> String -> IO ()
printWhenAll opts s =
 when (optVerb opts > 2) (putStrLn s)

---------------------------------------------------------------------------
