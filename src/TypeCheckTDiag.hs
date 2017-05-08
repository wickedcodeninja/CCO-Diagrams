-- (C) 2017 Wout Elsinghorst

import CCO.Component  (Component, component, printer, ioWrap)
import CCO.Diag       (Diag)
import CCO.Tree       (ATerm, Tree (fromTree, toTree), parser)
import Control.Arrow  (Arrow (arr), (>>>))

import CCO.Diag.TypeChecking ( typeCheckDiag )

main = 
  ioWrap (parser >>> (component toTree :: Component ATerm Diag) >>> typeCheckDiag >>> arr fromTree >>> printer)
