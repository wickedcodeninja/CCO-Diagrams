import CCO.Component  (Component, component, printer, ioWrap)
import CCO.Diag       (Diag)
import CCO.Picture    (Picture (..))
import CCO.Tree       (ATerm, Tree (fromTree, toTree), parser)
import Control.Arrow  (Arrow (arr), (>>>))

diag2picture :: Component Diag Picture
diag2picture = arr $ \_ -> Picture (0, 0) [] 

main = 
  ioWrap (parser >>> (component toTree :: Component ATerm Diag) >>> diag2picture >>> arr fromTree >>> printer)
