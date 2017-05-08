module CCO.Diag.TypeChecking where

import CCO.Component  (Component, component, printer, ioWrap)
import CCO.Feedback ( errorMessage, warn_ )
import CCO.Printing ( Doc, text )

import CCO.Diag.AG

import Data.List ( partition )
import Data.Maybe ( mapMaybe ) 
import Data.Foldable ( traverse_ )

import CCO.SourcePos
import CCO.Printing
import System.IO

typeCheckDiag :: Component Diag Diag
typeCheckDiag = component $ \diag -> 
  case checkDiagram diag of
    (Just checkedDiag, []     ) -> pure checkedDiag
    (_               , (err:_)) -> errorMessage . ppDiagnostic $ err

ppDiagnostic :: Diagnostic -> Doc
ppDiagnostic (TyError pos env inferred descr) = above [ppHeader, text " ", ppInferred, ppRelevant]
  where
    ppHeader   = wrapped $
                 describeSourcePos pos ++ ": Type error: " ++ descr
    ppInferred = text "? Inferred type: " >|< showable inferred
    ppRelevant = text "? Relevant bindings: " >|< showable env
ppDiagnostic (ScopeError pos env descr) = above [ppHeader, text " ", ppInferred]
  where
    ppHeader   = wrapped $
                 describeSourcePos pos ++ ": Scope error: " ++ descr
    ppInferred = text "? Relevant bindings: " >|< showable env

          
-- NOTE: This function was verbatimly stolen from the ArithBool example in the uu-cco-examples package.
describeSourcePos :: SourcePos -> String
describeSourcePos (SourcePos (File file) (Pos ln col)) = file ++ ":line " ++ show ln ++ ":column " ++ show col
describeSourcePos (SourcePos (File file) EOF)          = file ++ ":<at end of file>"
describeSourcePos (SourcePos Stdin (Pos ln col))       = "line " ++ show ln ++ ":column " ++ show col
describeSourcePos (SourcePos Stdin EOF)                = "<at end of input>"
