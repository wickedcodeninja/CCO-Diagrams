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
  let msgs = getTypeDiagnostics diag
      
      isError (TyError _ _ _) = True
      isError (Warning _ _)   = False
      
      (errors, warnings) =  partition isError msgs
     
      extractWarning (TyError _ _ _)   = Nothing
      extractWarning (Warning pos msg) = Just msg
     
  in do () <- traverse_ warn_ . mapMaybe extractWarning $ warnings
        
        case errors of
          []      -> pure diag
          (err:_) -> errorMessage . ppDiagnostic $ err

ppDiagnostic :: Diagnostic -> Doc
ppDiagnostic (TyError pos inferred descr)
  = above [ppHeader, text " ", ppInferred]
  where
    ppHeader   = wrapped $
                 describeSourcePos pos ++ ": Type error: " ++ descr
    ppInferred = text "? inferred type: " >|< showable inferred
    
          
-- NOTE: This function was verbatimly stolen from the ArithBool example in the uu-cco-examples package.
describeSourcePos :: SourcePos -> String
describeSourcePos (SourcePos (File file) (Pos ln col)) = file ++ ":line " ++ show ln ++ ":column " ++ show col
describeSourcePos (SourcePos (File file) EOF)          = file ++ ":<at end of file>"
describeSourcePos (SourcePos Stdin (Pos ln col))       = "line " ++ show ln ++ ":column " ++ show col
describeSourcePos (SourcePos Stdin EOF)                = "<at end of input>"
