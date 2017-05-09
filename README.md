Authors:
- Stefan Holdermans
- Wout Elsinghorst (wlelsing@gmail.com)
- Xander van der Goot (x.r.goot@students.uu.nl)

# Functionality 
This project supports the domain specific language for T-diagrams as described in the assignment and is extended to support the binding of diagrams to variables.
Binding diagrams to variables is done with:
TODO: beschrijf let / use syntax.
Variables can be used with

Furthermore the scoping of the variables is done on block level. Therefore reusing a variable name results in shadowing.
TODO: Iets te krom verwoord

# Installation
Required:
- GHC 8.0.2 or Haskell stack

To build and install this project using Cabal run `make`.

TODO: worden dependencies automatisch geïnstalleerd met Cabal? Anders die dependencies nog expliciet toevoegen?

The other possibility is to build and install the project within a virtual environment provided by stack. Stack will install all the required tools automatically.
Run `make -f stack-makefile` to build and install the project within the virtual environment. The makefile should be used instead of executing `stack build` directly due to required preprocessing steps.

# Running
This project consists out of four executables:
- parse-tdiag
- tc-tdiag
- tdiag2picture
- pp-picture

Programs can be chained together using the pipe command:
`parse-tdiag | tc-tdiag | tdiag2picture | pp-picture`
or within stack
`stack exec parse-tdiag | stack exec tc-tdiag | stack exec tdiag2picture | stack exec pp-picture`.

TODO: Wat is de input voor parse-tdiag

# Testing
To run the test suite use the following command:
`make -f stack-makefile test`

# Documentation
The documentation itself and the source files for the documentation can be found in `report/`. 