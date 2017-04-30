-- (C) 2017 Wout Elsinghorst

import CCO.Component  (Component, component, printer, ioWrap)
import CCO.Diag       (Diag)
import CCO.Tree       (ATerm, Tree (fromTree, toTree), parser)
import Control.Arrow  (Arrow (arr), (>>>))

import CCO.Feedback ( errorMessage )
import CCO.Printing ( text )

import CCO.Diag.AG ( DiagramType, diagramType, isTypeCorrect )

typeCheckDiag :: Component Diag Diag
typeCheckDiag = component $ \a -> 
  case isTypeCorrect a of
    Right tree -> pure tree
    Left  err  -> errorMessage $ text err

main = 
  ioWrap (parser >>> (component toTree :: Component ATerm Diag) >>> typeCheckDiag >>> arr fromTree >>> printer)
