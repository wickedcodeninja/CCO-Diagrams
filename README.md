Authors:
- Stefan Holdermans
- Wout Elsinghorst (w.l.elsinghorst@students.uu.nl)
- Xander van der Goot (x.r.goot@students.uu.nl)

# Introduction

This project supports type checking and rendering of Diagrams written in the domain specific language for describing T-Diagrams, which was described in the assignment. The Diagram language is extended to support a Let-binding construct which allows the user to locally bind a diagram to a name and then later reference this Diagram any number of times within the declaration scope. The language is further extended by us to allow for optional type annotations (which are checked by the type checker against the inferred types). The inferred types for each node of the diagram are part of the output for the `tc-tdiag` program, and thus this output can be checked to help verify correctness of our compiler.

# Installation
Required:
- GHC 8.0.2 or Haskell stack
- pdfTeX 3.14159265-2.6-1.40.17 (pdflatex)
- extsizes package for Latex (often included with your TeX distribution)

To build and install this project using Cabal run `make && cabal install`.

The other possibility is to build and install the project within a virtual environment provided by stack. Stack will install all the required tools automatically.
Run `make -f stack-makefile` to build and install the project within the virtual environment. The makefile should be used instead of executing `stack build` directly due to required preprocessing steps.

# Running
This project consists out of four executables:
- parse-tdiag
- tc-tdiag
- tdiag2picture
- pp-picture

When installed, the programs can be chained together using the pipe command:

    .. | parse-tdiag | tc-tdiag | tdiag2picture | pp-picture | ..

For example, on a Unix system with a working LaTeX suite you can run the following to parse, typecheck, convert, print and render the test diagrams:
    
    cat test/bindings/test3.diag | parse-tdiag | tc-tdiag | tdiag2picture | pp-picture | pdflatex

Or within stack:
    
    cat test/bindings/test3.diag | stack exec parse-tdiag | stack exec tc-tdiag | stack exec tdiag2picture | stack exec pp-picture | pdflatex

# Testing
To run the test suite use the following command:

    make -f stack-makefile test

The directory `test` also contains a few prepared Diagram files which can be manually fed to our toolchain. Specifically, the subdirectory `basic` contains a few basic examples to get you going while the directories `bindings` and `signatures` respectively contain tests which demonstrate our implementation of `let`-bindings and of the optional type signature feature.
    
# Functionality 

## Type Checking

We've implemented a type checker for the semantics defined in the `Formal Semantics` chapter below. A more easiliy digestable version is presented in the chapter `Informal Semantics`. Our type checker uses the Utrecht Attribute Grammar System (UUAG) to recursively calculate these properties for a diagram in terms of the properties calculated for each of its sub-diagrams. 

## Error Diagnostics

The error messages are expressed in terms of the expected functionality for its sub-diagrams, for example by complaining that the first argument to an `execute` statement is not (a subtype of) the required `Executable` type. Only the first error encountered is reported, with a group of `let`-bindings bindings being checked top-to-bottom. If the diagram is type-correct in terms of the types inferred for its children, the type for the diagram itself is synthesized and then compared to the (optional) type annotation. If this all succeeds, the diagram is deemed type-correct and resulting type is propagated upwards. 

The following error message is shown when trying to execute an `Interpreter` which runs on `i686-linux` on another `Interpreter` which can only run executables written in `Haskell`:

    > line 1:column 1: Type error: /Execute/: The first diagram is inferred to have
    > been written in the language 'i686-linux' which is different from the execution
    > language 'Haskell' of the the second diagram.
    > 
    > ? Expected type: Executable { Haskell }
    > ? Inferred type: Interpreter [ arm-android ] ~> { i686-linux }

The error message was generated for the following incorrectly typed diagram:

    execute 
      interpreter google-dev-kit for arm-android in i686-linux
    on
      interpreter hugs for Haskell in i686-windows
    end

You can see that the type checker infers from the second argument of `execute` (the interpreter accepting `Haskell` programs) that the first argument should be any diagram that is a subtype of `Executable` for the language `Haskell`. In this case the first argument is indeed `Executable`, but for the different language `i686-linux`, and thus the types don't unify. 
    
## Picture Generation

We've also used the UUAG to implement a program to convert a program written in the T-Diagram language to a Picture representation which matches our chosen semantics. TODO

## Let-bindings

To allow the binding of Diagrams to variables, the grammar of the language was extended with the following production:

`| let [ (Ik = Dk)* ] in D`

This allows you to bind zero or more diagrams `Dk` to identifiers `Ik` which can be refered to from within the diagram `D` by the use of `Use` statements, which are explained in the next section. Bindings introduced here shadow bindings introduced higher up in the diagram tree and diagrams within a binding group can also refer to other bindings introduced in the same group (both forward and backward references are allowed). No effort was taken to gracefully prevent cyclic references. Hopefully these will be caught with runtime exiting with an informative `<<loop>>` message, but no guarantees are given.

## Use-statements

To allow references to previously bound diagrams, the grammar of the language was extended with the following production:

`| use I`

Here `I` is any identifier that was previously introduced in a `let` block. If `d` is any specific identifier which is out-of-scope, an error message will be given. When `d` is in scope, then for all intends and purposes, every occurence of the statement `use d` is given the exact same semantics as the diagram referenced to by `d`. Specifically, the statement `use d` has the same type, name and capabilities as the diagram referenced to by `d`. The output of the type checker replaces (inlines) all occurences of any `use` statement with the corresponding diagram which it refers to, thus eleminating any sharing from the original diagram. The Diagram-To-Picture converter currently relies on this behavior and does not directly support converting any diagrams still containing any `use` statements.

## Type Signatures

Our language supports optional type signatures which can be provided to each of the basic diagram constructors. Type signatures (if present) are checked against the inferred types and an error is raised if the expected and inferred types don't match. While type signatures are never necessary and all diagrams can be compiled without supplying any signatures, type signatures greatly enhance diagram readability in diagrams which make heavy use of `let`-bound references. 
Type signatures are introduced by following the first keyword of a diagram constructor with `::` followed by the complete type that you want the whole diagram to have. For example:

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

The only two exceptions to this way of introducing type signatures are the `use` and the `platform` constructors. For these the type signature follow the same general format but introduction symbol `::` follows the first identifier instead of the keyword. For example:
    
    ...
    execute
      use myProgram :: Program { Java }
    on
      platform Java :: Platform {! Java !}
    end
    ...
    
In the examples above, `[ l1 ]` indicates that the program accepts other programs written in language `l1`, `{! m !}`  indicates a non-executable platform `m` and `{ l3 }` indicates that the language `l3` can still serve as input for other compilers/interpreters (i.e. it is not already running). The `~>` arrow indicates that the program transforms its input in the designated way.

If the declaration of the diagram `myProgram` in the example above actually were to be of type `Program { PHP }` (i.e. of type different than the given annotation), then the following error message would inform the user of his error:

    line 7:column 3: Type error: The provided type signature for the diagram
    'myProgram' doesn't match the inferred type.
 
    ? Expected type: Program { Java }
    ? Inferred type: Program { PHP }
    
## Informal Semantics

We have designed a subtyping hierarchy for our diagram language which explains the meaning we give to a diagram. The exact inference rules are explained in the chapter `Formal Semantics`, but basically our inference rules amount to the following observations:

 - Diagrams can be divided in two groups, those which are still compilable/executable and those who are not
 - Executing something which is executable makes the result non-executable and non-compilable (you cannot run or compile something which is already running) 
 - The result of compiling something which is executable/compilable is still executable/compilable
 - Programs, Compilers and Interpreters, when not running, are executable and compilable
 - Platforms are never executable/compilable
 - Compiling something doesn't change the semantics of the program being compiled, e.g. a compiler is still a compiler for the same source/target languages after compiling to run on a different architecture

## Formal Semantics

In our chosen semantics, all compilable programs are executable and vice versa, so in the following we'll make the simplification of treating `Compilable` and `Executable` as synonyms. The following deductions are valid for diagrams written in the Diagram language:

 - `Executables`, and only `Executables`, can be executed by an `Executor`
 - `Executables`, and only `Executables`, can be compiled by an `Compiler`
 
 - `Programs`, `Interpreter`s and `Compilers` are initially `Executable`
 - `Platforms` are never `Executable`
 
 - `Platforms` and `Interpreters` are `Executors`
 
 - The statement `compile d1 on d2` is `Executable`
 - The statement `execute d1 on d2` is not `Executable`
 
 - The statement `execute d1 on d2` is an `Executor` if and only if `d1` is an `Executor` 
 - The statement `execute d1 on d2` is a `Compiler` if and only if `d1` is a `Compiler` 
 
 - The statement `compile d1 on d2` is an `Executor` if and only if `d1` is an `Executor` 
 - The statement `compile d1 on d2` is a `Compiler` if and only if `d1` is a `Compiler`
 
 - The `inputLanguage` of `platform m` is `m`
 - The `inputLanguage` of `interpreter c for l in m` is `l`
 - The `inputLanguage` of `compiler c from l1 to l2 in m` is `l1`
 
 - The `outputLanguage` of `compiler c from l1 to l2 in m` is `l2`
 
 - The `implementationLanguage` of `program p in l` is `l`
 - The `implementationLanguage` of `interpreter i for l in m` is `m`
 - The `implementationLanguage` of `compiler c from l1 to l2 in m` is `m`
 
 - The `implementationLanguage` of `compile d1 with d2` is the `outputLanguage` of `d2`

 - The `platformLanguage` of `platform m` is `m`
 - The `platformLanguage` of `execute d1 on d2` is the `implementationLanguage` of `d2` if `d2` is `Executable`
 - The `platformLanguage` of `execute d1 on d2` is the `platformLanguage` of `d2` if `d2` is not `Executable`
 
 - The basic `program`, `interpreter`, `compiler` and `platform` statements are type-correct
 - The statement `execute d1 on d2` is type-correct if `d1` and `d2` are both type-correct, `d1` is an `Executable`, `d2` is an `Executor` and the `implementationLanguage` of `d1` matches the `inputLanguage` of `d2` 
 - The statement `compile d1 with d2` is type-correct if `d1` and `d2` are both type-correct, `d1` is an `Executable`, `d2` is a `Compiler` and the `implementationLanguage` of `d1` matches the `inputLanguage` of `d2` 

# Implementation
 
## Type Checking

The type rules defined in the chapter `Formal Semantics` where implemented using the UUAG system. The implementation is a pretty straight forward translation from the type rules above to attributes which capture the intended inferences. The main attributes we've defined, corresponding to the chosen semantics, are 

 - `syn canExecuteLanguage :: Maybe Language`
 - `syn canCompileLanguage :: Maybe (Language, Language)`
 - `syn implementationLanguage :: Either (Platform, Language)`
 
 The attributes `canExecuteLanguage` and `canCompileLanguage` correspond respectively to the `Executor` and `Compiler` identities defined above. The `canExecuteLanguage` attribute has type `Maybe Language`, and it contains two pieces of information. Namely, it contains whether the diagram represented by this node is an `Executor` or not, and if it does it contains the precise `inputLanguage` which makes makes it an `Executor`. Similarly, the `canCompileLanguage` attribute contains both the fact of whether the diagram represented by this node is a `Compiler` or not, and, the `inputLanguage` and the `outputLanguage` if this is the case.
 The `implementationLanguage` attribute decides whether the diagram is `Executable` or not, and if so it gives the `implementationLanguage` in the `Right` constructor or the platform on which it is running in the `Left` constructor. For each node, the value of these attributes is easily expressed in terms of the values of any of the sub-diagrams of that node, with the implementation so obvious that we will not describe it here. 
 
 To implement `let`-binds, an environment is constructed for each of the attributes listed above, mapping the identifier for the diagram to the corresponding attribute. The attributes for a `use` statements are calculated by performing a simple lookup in the previously constructed environments. As is usual, the environments synthesized from a list of `let`-bound are fed back into that same list as inherited attributes so that the attributes for the declared diagrams become available within the list of declarations where they were originally declared in.
 
 With all these attributes available in a given node, the type checker then uses these attributes to check if the sub diagrams have compatible types, and if all is well it synthesizes a `diagType` attribute which neatly summarizes the diagram. This is used for later error diagnosis higher up the diagram tree.
 
 The correctness of the non-trivial `execute` and `compile` statements is checked in a seperate function which compares the inferred types for the subdiagrams with compatibility with the abstract `Executable` type and with respectively the `Executor` and `Compiler` types. These functions have the following types:
 
     checkTyExecute, checkTyCompile :: SourcePos -> Environment DiagType -> DiagType -> DiagType -> [Diagnostic]
     
 The subdiagrams are tried for coercion to the required types, and if all coercions succeed and the languages match then no error is given. If something is wrong, an error is reported, which indicates which of the arguments wasn't coercible to the required type. The `Environment DiagType` argument is used to show a list of relevent bindings currently in scope. The list of error messages generated for a given node is synthesised in the `typeDiagnostics` attribute, although only the first entry of this list is ever displayed, if any.
 
The implementation of the main semantic rules `canExecuteLanguage`, `canCompileLanguage` and `implementationLanguage` can be found in the file `CCO/Diag/AG/Semantics.ag`. The rules for `diagType` and `typeDiagnostics` and the implementations for `checkTyExecute` and `checkTyCompile` can be found in the file `CCO/Diag/AG/TypeChecking.ag`.

## Let Binding

To implement `let`-bindings the parser and lexer had to be extended to parse the concrete syntax for lists of type `[(String, Diag_)]`. The files `CCO/Diag/Parser.hs` and `CCO/Diag/Lexer.hs` were extended to include the relevant extensions. We choose to extend the `Diag` constructor of the `Diag` datatype with the possibly empty list of declarations which were attached to the accompanying diagram `Diag_`. The `Diag` datatype also gained a `Use` constructor containing the identifier of the referred diagram.

## Type Signatures

We also extended the constructors of the `Diag` datatype to `Maybe` contain a type signature. The parser was extended to parse the concrete syntax proposed above. New keywords were added to the lexer to allow a precise formulation of the type for each diagram. The type signature is completely optional and if no signature is available a type will always be inferred. If a type signature is available however, the inferred type is checked against the given type and an error is thrown when the two types do not match. This check is done with the following function:

    checkTyWithAnnotation :: SourcePos -> Ident -> DiagType -> Maybe DiagType -> [Diagnostic]

The first `DiagType` argument is the inferred type and the second argument is `Maybe` the type annotation given in the source file. If none was given the function will never return an error.
    
To support type annotations we also gave `Tree` instances for the `DiagType` datatype so that our annotated `Diag` diags have a complete `ATerm` representation. The output of the `tc-tdiag` program is actually a fully type-annotated `Diag` tree, so the output of this program can also be used to manually verify the inferred type for each and every subdiagram of the supplied input.

## Diagram to Picture Conversion

We still need to say something here. TODO.
