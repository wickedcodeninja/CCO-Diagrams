import CCO.Component  (Component, component, printer, ioWrap, ioRun)
import CCO.Picture    (Picture)
import CCO.Tree       (ATerm, Tree (toTree), parser)
import Control.Arrow  ((>>>))

main =
  do diag <- getContents
     putStrLn $ "\\documentclass[8pt]{extarticle}"
     putStrLn $ "\\begin{document}"
     latex <- ioRun (parser >>> (component toTree :: Component ATerm Picture) >>> printer) diag
     putStrLn $ latex
     putStrLn $ "\\end{document}"
