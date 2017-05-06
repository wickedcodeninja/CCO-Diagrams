import Test.Hspec
import CCO.D2P (diag2picture)
import CCO.SourcePos (SourcePos(..), Source(..), Pos(..))
import CCO.Diag.Base (Diag(..), Diag_(..))
import CCO.Component (ioRun, printer)
import Control.Arrow ((>>>), arr)
import Data.List
import Data.List.Split
import Data.Char

sp = SourcePos Stdin EOF

replace old new = intercalate new . splitOn old

test d t = do a <- ioRun (diag2picture >>> printer >>> arr (filter (not . isSpace))) d
              replace ".0" "" a `shouldBe` filter (not . isSpace) t

main :: IO ()
main = hspec $ do
  describe "t2d" $ do
    it "Program" $ do 
      let d = Diag sp (Program "hello" "Haskell")
          t = "\\begin{picture}(65,30)\
              \  \\put(7.5,0){\\line(1,0){50}}\
              \  \\put(7.5,0){\\line(0,1){15}}\
              \  \\put(7.5,15){\\line(-1,2){7.5}}\
              \  \\put(57.5,15){\\line(1,2){7.5}}\
              \  \\put(57.5,0){\\line(0,1){15}}\
              \  \\put(0,30){\\line(1,0){65}}\
              \  \\put(7.5,15){\\makebox(50,15){hello}}\
              \  \\put(7.5,0){\\makebox(50,15){Haskell}}\
              \\\end{picture}"

       in test d t

  it "Platform" $ do
     let d = Diag sp (Platform "i686-windows")
         t = "\\begin{picture}(50,30) \
              \ \\put(0,15){\\line(5,-3){25}}\
              \ \\put(25,0){\\line(5,3){25}}\
              \ \\put(0,15){\\line(0,1){15}}\
              \ \\put(0,30){\\line(1,0){50}}\
              \ \\put(50,30){\\line(0,-1){15}}\
              \ \\put(0,15){\\makebox(50,15){i686-windows}}\
              \ \\end{picture}"
      in test d t

  it "Compiler" $ do
    let d = Diag sp (Compiler "uuagc" "UUAG" "Haskell" "i686-windows")
        t = "\\begin{picture}(150,30)\
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
     in test d t

  it "Interpreter" $ do
    let d = Diag sp (Interpreter "hugs" "Haskell"  "i686-windows")
        t = "\\begin{picture}(50,30)\
             \ \\put(0,0){\\framebox(50,30){}}\
             \ \\put(0,20){\\makebox(50,10){Haskell}}\
             \ \\put(0,10){\\makebox(50,10){hugs}}\
             \ \\put(0,0){\\makebox(50,10){i686-windows}}\
             \ \\end{picture}"
     in test d t


