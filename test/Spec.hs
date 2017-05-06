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

main :: IO ()
main = hspec $ do
  describe "t2d" $ do
    it "Program" $
      let d = Diag sp (Program "hello" "Haskell")
          t = "\\begin{picture}(65,35)\
              \  \\put(7.5,0){\\line(1,0){50}}\
              \  \\put(7.5,0){\\line(0,1){15}}\
              \  \\put(7.5,15){\\line(-1,2){7.5}}\
              \  \\put(57.5,15){\\line(1,2){7.5}}\
              \  \\put(57.5,0){\\line(0,1){15}}\
              \  \\put(0,30){\\line(1,0){65}}\
              \  \\put(7.5,15){\\makebox(50,15){hello}}\
              \  \\put(7.5,0){\\makebox(50,15){Haskell}}\
              \\\end{picture}"

      in do a <- ioRun (diag2picture >>> printer >>> arr (filter (not . isSpace))) d
            replace ".0" "" a `shouldBe` filter (not . isSpace) t

