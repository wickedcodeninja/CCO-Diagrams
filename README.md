Authors:
- Stefan Holdermans
- Wout Elsinghorst (w.l.elsinghorst@students.uu.nl)
- Xander van der Goot (x.r.goot@students.uu.nl)

# Introduction

This project supports type checking and rendering of Diagrams written in the domain specific language for the T-Diagram language as described in the assignment. The Diagram language is extended to support a Let-binding construct which allows the user to locally bind a diagram to a name and then later reference this Diagram any number of times within the declaration scope. The language is further extended by us to allow for optional type annotations (which are checked by the Type Checker against the inferred types). 

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


# Functionality 

## Let-bindings

To allow the binding of Diagrams to variables, the grammar of the language was extended with the following production:

`| let [ (Ik = Dk)* ] in D`

This allows you to bind zero or more Diagrams `Dk` to identifiers `Ik` which can be refered to from within the diagram `D` by the use of `Use` statements, which are explained in the next section. Bindings introduced here shadow bindings introduced higher up in the diagram tree and diagrams within a binding group can also refer to other bindings introduced in the same group (both forward and backward references are allowed). No effort was taken to gracefully prevent cyclic references. Hopefully these will be caught with runtime exiting with an informative `<<loop>>` message, but no guaruantees are given.

## Use-statements

To allow references to previously bound Diagrams, the grammar of the language was extended with the following production:

`| use I`

Here `I` is any identifier that was previously introduced in a let block. If `I` is not in scope then an out-of-scope error message will be given. When `d` is in scope, then for all intends and purposes, every occurence of the statement `use d` is given the exact same semantics as the Diagram referenced to by `d`. Specifically, the statement `use d` has the same type, name and capabilities as the diagram referenced to by `d`. The output of the type checker replaces (inlines) all occurences of any `use` statement with the corresponding diagram which it refers to, thus eleminating any sharing from the original diagram. The Diagram to Picture converter currently relies on this behavior and does not directly support converting any diagrams still containing any `use` statements.

## Type Signatures

Our language supports optional type signatures which can be provided to each of the basic Diagram constructors. Type signatures (if present) are checked against the inferred types and an error is raised if the expected and inferred types don't match. While type signatures are never necessary and all Diagrams can be compiled without supplying any signatures, type signatures greatly enhance Diagram readability in Diagrams which make heavy use of Let-bound references. 
Type Signatures are introduced by following the first keyword of a Diagram constructor with `::` followed by the complete type that you want the whole Diagram to have. For example:

compile :: Program { Haskell }
  program hello in UUAG
on
  execute :: Compiler [ UUAG ] ~> { Haskell } / {! i686-linux !}
    compiler uuag from UUAG to Haskell in i686-windows
  on
    interpreter :: Interpreter [ i686-windows ] ~> {! i686-linux !}
      VMWare for i686-windows 
    in i686-linux
  end 
end

The second type signature indicate that executing a compiler written for the Windows platform on VMWare for Linux results in a compiler accepting the same languages as the original compiler but now running on the Linux platform.

The only two exceptions to this way of introducing type signatures are the `Use` and the `Platform` constructors. For these the type signature follows the same general format but introduction symbol `::` follows the first identifier instead of the keyword. For example:

execute
  use myProgram :: Program { Java }
on
  platform Java :: Platform {! Java !}
end
  
In the examples above, `[ l1 ]` indicates that the program accepts other programs written in language `l1`, `{! m !}`  indicates a non-executable platform `m` and `{ l3 }` indicates that the language `l3` can still serve as input for other compilers/interpreters (i.e. it is not already running). The `~>` arrow indicates that the program transforms its input in the designated way.

## Type Checking

We've implemented a type checker for the semantics defined below. Our type checker uses the Utrecht Attribute Grammar System to recursively calculate these properties for a diagram in terms of the properties calculated for each of its sub-diagrams. The error messages are expressed in terms of the expected functionality for its sub-diagrams, for example by complaining that the first argument to an `execute` statement is not (a subtype of) the required `Executable` type. Only the first error encountered is reported, with a group of `let`-bindings bindings being checked top-to-bottom. If the diagram is type-correct in terms of the types inferred for its children, the type for the diagram itself is synthesized and then compared to the (optional) type annotation. If this all succeeds, the diagram is deemed type-correct and resulting type is propagated upwards. 

## Picure Generation

We've also implemented a program to convert a program written in the T-Diagram language to a Picture representation which matches our chosen semantics. TODO

# Informal Semantics / 

We have designed a subtyping hierarchy for our diagram language which explains the meaning we give to a diagram. The exact inference rules are explained in the chapter `Semantics`, but basically our inference rules amount to the following observations:

 - Diagrams can be divided in two groups, those which are still compilable/executable and those who are not
 - Executing something which is executable makes the result non-executable and non-compilable (you cannot run or compile something which is already running) 
 - The result of compiling something which is executable/compilable is still executable/compilable
 - Programs, Compilers and Interpreters, when not running, are executable and compilable
 - Platforms are never executable/compilable
 - Compiling something doesn't change the semantics of the program being compiled, e.g. a compiler is still a compiler for the same source/target languages after compiling to run on a different architecture

## Formal Semantics

In these rules, `Executable` is meant to be equivalent to `Compilable`.

 - `Executable`s, and only `Executable`s, are executed by an `Executor`
 - `Executable`s, and only `Executable`s, are compiled by an `Compiler`
 
 - `Program`s, `Interpreter`s and `Compiler`s are initially `Executable`
 - `Platform`s are never `Executable`
 
 - `Platform`s and `Interpreter`s are `Executor`s
 
 - The statement `compile d1 on d2` is `Executable`
 - The statement `execute d1 on d2` is not `Executable`
 
 - The statement `execute d1 on d2` is an `Executor` if and only if `d1` is an `Executor` 
 - The statement `execute d1 on d2` is a `Compiler` if and only if `d1` is a `Compiler` 
 
 - The statement `compile d1 on d2` is an `Executor` if and only if `d1` is an `Executor` 
 - The statement `compile d1 on d2` is a `Compiler` if and only if `d1` is a `Compiler`
 
 - The `inputLanguage` of `platform m` is `m`
 - The `inputLanguage` of `interpreter c for l in m` is `l`
 - The `inputLanguage` of `compiler c from l1 to l2 in m` is `l2`
 
 - The `outputLanguage` of `compiler c from l1 to l2 in m` is `l2`
 
 - The `implementationLanguage` of `program p in l` is `l`
 - The `implementationLanguage` of `interpreter i for l in m` is `m`
 - The `implementationLanguage` of `compiler c from l1 to l2 in m` is `m`
 
 - The `implementationLanguage` of `compile d1 with d2` is the `outputLanguage` of `d2`

 - The `platformLanguage` of `platform m` is `m`
 - The `platformLanguage` of `execute d1 on d2` the `implementationLanguage` of `d2` if `d2` is `Executable`
 - The `platformLanguage` of `execute d1 on d2` the `platformLanguage` of `d2` if `d2` is not `Executable`
 
 - The basic `program`, `interpreter`, `compiler` and `platform` statements are type-correct
 - The statement `execute d1 on d2` is type-correct if `d1` and `d2` are both type-correct, `d1` is an `Executable`, `d2` is an `Executor` and the `implementationLanguage` of `d1` matches the `inputLanguage` of `d2` 
 - The statement `compile d1 with d2` is type-correct if `d1` and `d2` are both type-correct, `d1` is an `Executable`, `d2` is a `Compiler` and the `implementationLanguage` of `d1` matches the `inputLanguage` of `d2` 

# Implementation
 
## Type Checking



## Diagram to Picture Conversion


# Documentation
The documentation itself and the source files for the documentation can be found in `report/`. 