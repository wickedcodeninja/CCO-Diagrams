module CCO.D2P (diag2picture) where
import CCO.Diag.Base (Diag)
import CCO.Component  (Component)
import CCO.Picture (Picture(..))
import CCO.Diag.AG (pic_Root, Root(..))
import Control.Arrow  (Arrow (arr), (>>>))

diag2picture :: Component Diag Picture
diag2picture = arr $ \d -> pic_Root (Root d)
