import CCO.Component  (Component, component, printer, ioWrap, ioRun)
import CCO.Diag       (Diag (..), Diag_(..))
import CCO.Picture    (Picture (..))
import CCO.Tree       (ATerm, Tree (fromTree, toTree), parser)
import Control.Arrow  (Arrow (arr), (>>>))
import CCO.SourcePos (SourcePos(..), Source(..), Pos(..))
import CCO.D2P  (diag2picture)


sp = SourcePos Stdin EOF

d = Diag sp (Program "hello" "Haskell")

-- main = ioWrap (parser >>> (component toTree :: Component ATerm Diag) >>> diag2picture >>> arr fromTree >>> printer)


main = do a <- ioRun (diag2picture >>> arr fromTree >>> printer) d
          putStrLn a
