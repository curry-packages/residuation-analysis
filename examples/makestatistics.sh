#!/bin/sh
# make residuation statistics for all system modules

SYSMODS="AllSolutions AnsiCodes Array Char Combinatorial CPNS Debug Dequeue Directory Distribution Either ErrorState FileGoodies FilePath Findall FiniteMap Float Format Function FunctionInversion GetOpt Global Integer IO IOExts List Maybe NamedSocket Nat Prelude Profile PropertyFile Random Read ReadNumeric ReadShowTerm RedBlackTree SCC SearchTree SearchTreeGenerators SearchTreeTraversal SetFunctions SetRBT ShowS Socket Sort State System TableRBT Time Traversal Unsafe ValueSequence"

CSVFILE=sysmodsstats.csv

curry-anaresinfo --stats $SYSMODS > $CSVFILE

