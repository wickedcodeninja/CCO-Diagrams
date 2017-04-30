import CCO.Component  (Component, component, printer, ioWrap)
import CCO.Diag       (Diag)
import CCO.Tree       (ATerm, Tree (fromTree, toTree), parser)
import Control.Arrow  (Arrow (arr), (>>>))

typeCheckDiag :: Component Diag Diag
typeCheckDiag = arr id

main = 
  ioWrap (parser >>> (component toTree :: Component ATerm Diag) >>> typeCheckDiag >>> arr fromTree >>> printer)
