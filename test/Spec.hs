import Test.Hspec
import CCO.D2P (diag2picture)
import CCO.SourcePos (SourcePos(..), Source(..), Pos(..))
import CCO.Diag.Base (Diag(..), Diag_(..))
import CCO.Component (ioRun, printer)
import Control.Arrow ((>>>), arr)
import Data.List
import Data.List.Split
import Data.Char
import System.Process

sp = SourcePos Stdin EOF

replace old new = intercalate new . splitOn old

generate_tex name text = do writeFile (name++".tex") ("\\documentclass{article}\\begin{document}" ++ text ++ "\\end{document}")
                            createProcess (proc "pdflatex" [name++".tex"])

test n d t = do a <- ioRun (diag2picture >>> printer >>> arr (filter (not . isSpace))) d
                generate_tex n a
                replace ".0" "" a `shouldBe` filter (not . isSpace) t

dprog = Diag sp (Program "hello" "Haskell")

dplat = Diag sp (Platform "i686-windows")

dint = Diag sp (Interpreter "hugs" "Haskell"  "i686-windows")

dcomp = Diag sp (Compiler "uuagc" "UUAG" "Haskell" "i686-windows")

main :: IO ()
main = hspec $ do
  describe "t2d" $ do
    it "Program" $ do 
      let t = "\\begin{picture}(65,30)\
              \  \\put(7.5,0){\\line(1,0){50}}\
              \  \\put(7.5,0){\\line(0,1){15}}\
              \  \\put(7.5,15){\\line(-1,2){7.5}}\
              \  \\put(57.5,15){\\line(1,2){7.5}}\
              \  \\put(57.5,0){\\line(0,1){15}}\
              \  \\put(0,30){\\line(1,0){65}}\
              \  \\put(7.5,15){\\makebox(50,15){hello}}\
              \  \\put(7.5,0){\\makebox(50,15){Haskell}}\
              \\\end{picture}"

       in test "prog" dprog t

  it "Platform" $ do
     let t = "\\begin{picture}(50,30) \
              \ \\put(0,15){\\line(5,-3){25}}\
              \ \\put(25,0){\\line(5,3){25}}\
              \ \\put(0,15){\\line(0,1){15}}\
              \ \\put(0,30){\\line(1,0){50}}\
              \ \\put(50,30){\\line(0,-1){15}}\
              \ \\put(0,15){\\makebox(50,15){i686-windows}}\
              \ \\end{picture}"
      in test "plat" dplat t

  it "Compiler" $ do
    let t = "\\begin{picture}(150,30)\
             \ \\put(50,0){\\line(0,1){20}}\
             \ \\put(50,20){\\line(-1,0){50}}\
             \ \\put(0,20){\\line(0,1){10}}\
             \ \\put(0,30){\\line(1,0){150}}\
             \ \\put(150,30){\\line(0,-1){10}}\
             \ \\put(150,20){\\line(-1,0){50}}\
             \ \\put(100,20){\\line(0,-1){20}}\
             \ \\put(100,0){\\line(-1,0){50}}\
             \ \\put(0,20){\\makebox(50,10){UUAG}}\
             \ \\put(50,20){\\makebox(50,10){$\\longrightarrow$}}\
             \ \\put(100,20){\\makebox(50,10){Haskell}}\
             \ \\put(50,10){\\makebox(50,10){uuagc}}\
             \ \\put(50,0){ \\makebox(50,10){i686-windows}}\
             \ \\end{picture}"
     in test "comp" dcomp t

  it "Interpreter" $ do
    let t = "\\begin{picture}(50,30)\
             \ \\put(0,0){\\framebox(50,30){}}\
             \ \\put(0,20){\\makebox(50,10){Haskell}}\
             \ \\put(0,10){\\makebox(50,10){hugs}}\
             \ \\put(0,0){\\makebox(50,10){i686-windows}}\
             \ \\end{picture}"
     in test "int" dint t

  it "Execute Program on Interpreter" $ do
    let d = Diag sp (Execute dprog dint)
        t = "\\begin{picture}(65,60)\
             \ \\put(7.5,30){\\line(1,0){50}}\
             \ \\put(7.5,30){\\line(0,1){15}}\
             \ \\put(7.5,45){\\line(-1,2){7.5}}\
             \ \\put(57.5,45){\\line(1,2){7.5}}\
             \ \\put(57.5,30){\\line(0,1){15}}\
             \ \\put(0,60){\\line(1,0){65}}\
             \ \\put(7.5,45){\\makebox(50,15){hello}}\
             \ \\put(7.5,30){\\makebox(50,15){Haskell}}\
             \ \\put(7.5,0){\\framebox(50,30){}}\
             \ \\put(7.5,20){\\makebox(50,10){Haskell}}\
             \ \\put(7.5,10){\\makebox(50,10){hugs}}\
             \ \\put(7.5,0){\\makebox(50,10){i686-windows}}\
             \\\end{picture}"
     in test "exe_proc_int" d t

  it "Compile Program on compiler" $ do
    let d = Diag sp (Compile (Diag sp (Program "hello" "UUAG")) dcomp)
    -- NOTE: width is 50 instead of 45. This seems to be wrong in the examples
        t = "\\begin{picture}(265,50)\ 
             \ \\put(7.5,20){\\line(1,0){50}}\
             \ \\put(7.5,20){\\line(0,1){15}}\
             \ \\put(7.5,35){\\line(-1,2){7.5}}\
             \ \\put(57.5,35){\\line(1,2){7.5}}\
             \ \\put(57.5,20){\\line(0,1){15}}\
             \ \\put(0,50){\\line(1,0){65}}\
             \ \\put(7.5,35){\\makebox(50,15){hello}}\
             \ \\put(7.5,20){\\makebox(50,15){UUAG}}\
             \ \\put(107.5,0){\\line(0,1){20}}\
             \ \\put(107.5,20){\\line(-1,0){50}}\
             \ \\put(57.5,20){\\line(0,1){10}}\
             \ \\put(57.5,30){\\line(1,0){150}}\
             \ \\put(207.5,30){\\line(0,-1){10}}\
             \ \\put(207.5,20){\\line(-1,0){50}}\
             \ \\put(157.5,20){\\line(0,-1){20}}\
             \ \\put(157.5,0){\\line(-1,0){50}}\
             \ \\put(57.5,20){\\makebox(50,10){UUAG}}\
             \ \\put(107.5,20){\\makebox(50,10){$\\longrightarrow$}}\
             \ \\put(157.5,20){\\makebox(50,10){Haskell}}\
             \ \\put(107.5,10){\\makebox(50,10){uuagc}}\
             \ \\put(107.5,0){\\makebox(50,10){i686-windows}}\
             \ \\put(207.5,20){\\line(1,0){50}}\
             \ \\put(207.5,20){\\line(0,1){15}}\
             \ \\put(207.5,35){\\line(-1,2){7.5}}\
             \ \\put(257.5,35){\\line(1,2){7.5}}\
             \ \\put(257.5,20){\\line(0,1){15}}\
             \ \\put(200,50){\\line(1,0){65}}\
             \ \\put(207.5,35){\\makebox(50,15){hello}}\
             \ \\put(207.5,20){\\makebox(50,15){Haskell}}\
             \ \\end{picture}"
      in test "comp_prog_comp" d t
