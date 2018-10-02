An analyzer of the residuation behaviour of Curry programs
==========================================================

This package contains a tool to analyze the residuation
behavior of Curry programs. The residuation behavior
is abstracted in the form

* if arguments i1,i2,..,in are ground values, then the
  function does not residuate
* the function might residuate or evaluates to a non-ground value
  independent of the groundness of the arguments

The tool can simply show these analysis results or store the
results as a Curry term of type

    [(String,[Int])]

Thus, it is a list of pairs consisting of the
qualified function name and a list of argument indices
(numbered from 1).
The indices are either the required ground arguments or
`[0]` if the function might residuate.
This information can be used by Curry compiler (e.g., PAKCS)
to optimize the compilation of non-residuating operations.

The tool has also options to show all possibly residuating operations
of a module or to print some statistics in CSV format (see option
`--help`).
