--------------------------------------------------------------------------
--- A tool to analyze the residuation behavior of a module.
--- It can just show the results, making some statistics, or writes
--- the results into a file which can be used by PAKCS to optimize
--- the implementation of non-residuating operations.
---
--- @author Michael Hanus
--- @version December 2018
--------------------------------------------------------------------------

import Directory    ( createDirectoryIfMissing )
import FilePath     ( (</>), takeDirectory )
import List         ( intercalate, partition )
import System       ( getArgs )

import FlatCurry.Types      ( QName, showQName )
import CASS.Server          ( analyzeGeneric )
import Analysis.ProgInfo    ( progInfo2Lists )
import Analysis.Residuation 
import System.CurryPath     ( addCurrySubdir, lookupModuleSourceInLoadPath
                            , modNameToPath )
import Text.CSV             ( showCSV )

import ToolOptions

------------------------------------------------------------------------

banner :: String
banner = unlines [bannerLine,bannerText,bannerLine]
 where
   bannerText = "Residuation Analysis Tool for Curry (Version of 12/09/18)"
   bannerLine = take (length bannerText) (repeat '=')

---------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  (opts,mods) <- processOptions banner args
  if null mods
    then putStrLn "ERROR: wrong arguments (try `--help' option)"
    else if optShowStats opts
           then countResOps2CSV mods
           else mapIO_ (genResInfo opts) mods

--- Returns the residuation information of all operations defined
--- in a module.
residuationInfoOf :: String -> IO [(QName,ResiduationInfo)]
residuationInfoOf modname = do
  analyzeGeneric residuationAnalysis modname
    >>= return . either (\pi -> let (i1,i2) = progInfo2Lists pi in i1 ++ i2)
                        error

genResInfo :: Options -> String -> IO ()
genResInfo opts mname =
  lookupModuleSourceInLoadPath mname >>=
  maybe (error $ "Source of module '" ++ mname ++ "' not found!")
    (\_ -> do
      printWhenStatus opts $ "Analyzing module " ++ mname ++ "..."
      resinfos <- residuationInfoOf mname
      when (optShow opts) $ putStrLn $
        "Analysis results (non-residuating if arguments...):\n" ++
        unlines (map showFunRI resinfos)
      when (optShowResOpts opts) $ putStrLn $
        "Possibly residuating operations: " ++ unwords (allResOps resinfos)
      unless (null (optOutput opts)) (writeResInfoFile opts resinfos)
    )
 where
  showFunRI (qn,ri) = snd qn ++ " " ++ showRI ri

  showRI MayResiduate       = "residuate"
  showRI NoResInfo          = "residuate"
  showRI (NoResiduateIf xs) = intercalate "," (map show xs)

--- Writes a file containing residuation information in Curry term format.
--- The term is a list of pairs consisting of the qualified function name
--- together with the list of argument positions which must be ground values
--- to ensure that the function call does not residuate and yields a
--- ground value. If the function might always residuate, the argument
--- position list is [0].
writeResInfoFile :: Options -> [(QName,ResiduationInfo)] -> IO ()
writeResInfoFile opts resinfo = do
  let rifile = optOutput opts
  createDirectoryIfMissing True (takeDirectory rifile)
  writeFile rifile (show (map fri2term resinfo) ++ "\n")
  printWhenStatus opts $ "Residuation info written into '" ++ rifile ++ "'"
 where
  fri2term (qn,ri) = (showQName qn,  ri2term ri)
   where
    ri2term MayResiduate       = [0]
    ri2term NoResInfo          = [0]
    ri2term (NoResiduateIf xs) = xs

--- Returns the list of possibly residuating operations.
allResOps :: [(QName,ResiduationInfo)] -> [String]
allResOps resinfo =
  map (snd . fst)
      (filter (\ (_,i) -> i==MayResiduate || i==NoResInfo) resinfo)

--- Counts all possibly residuating and non-residuating operations.
countResOps :: String -> IO [String]
countResOps mname = do
  resinfo <- residuationInfoOf mname
  let (nonresops,resops,unknowns) = foldr select ([],[],[]) resinfo
  return $ mname : map (show . length) [nonresops,resops,unknowns]
 where
  select (f,i) (nrs,res,unk) = case i of
    NoResiduateIf _ -> (f:nrs,res,unk)
    MayResiduate    -> (nrs,f:res,unk)
    NoResInfo       -> (nrs,res,f:unk)

countResOps2CSV :: [String] -> IO ()
countResOps2CSV mods = do
  stats <- mapIO countResOps mods
  let table = ["Module", "Non-residuating", "Residuating", "Unknown"] : stats
  putStr (showCSV table)
