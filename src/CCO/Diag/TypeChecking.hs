module CCO.Diag.TypeChecking where

import CCO.Component  (Component, component, printer, ioWrap)
import CCO.Feedback ( errorMessage, warn_ )
import CCO.Printing ( text )

import CCO.Diag.AG

import Data.List ( partition )
import Data.Foldable ( traverse_ )

typeCheckDiag :: Component Diag Diag
typeCheckDiag = component $ \diag -> 
  let msgs = getTypeDiagnostics diag
      
      isError Error   = True
      isError Warning = False
      
      (errors, warnings) =  partition (isError . fst) msgs
     
  in do () <- traverse_ (warn_ . snd) $ warnings
        
        case errors of
          []           -> pure diag
          ((_, err):_) -> errorMessage . text $ err
