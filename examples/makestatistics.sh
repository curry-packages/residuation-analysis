#!/bin/sh
# make residuation statistics for all system modules

SYSMODS="Control.Applicative Control.Monad Curry.Compiler.Distribution Data.Maybe Data.Function Data.IORef Data.Either Data.List Data.Functor.Identity Data.Char Debug.Trace Numeric Prelude System.Environment System.IO.Unsafe System.Console.GetOpt System.IO System.CPUTime Test.Prop.Types Test.Prop Text.Show"

CSVFILE=sysmodsstats.csv

curry-anaresinfo --stats $SYSMODS > $CSVFILE

