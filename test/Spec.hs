import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe
import Lib

main :: IO ()
main = do
  defaultMain (testGroup "Notakto tests" [decodeEncodeTest, decodeEncodeTest2, intParseTest, intParseTest2, moveTest, moveTest2, moveTest3, moveTest4, moveTest5, moveTest6])

decodeEncodeTest :: TestTree
decodeEncodeTest = testCase "Decode Encode test"
  (assertEqual "someMoves" "d1:cli0ei1ee2:id1:a4:prevd1:cli0ei0ee2:id1:b1:v1:xe1:v1:xe"
    (encode (fst (fromJust (parseDict "d1:cli0ei1ee2:id1:a4:prevd1:cli0ei0ee2:id1:b1:v1:xe1:v1:xe"))) ""))
 
decodeEncodeTest2 :: TestTree
decodeEncodeTest2 = testCase "Decode Encode empty board"
  (assertEqual "noMoves" "de"
    (encode (fst (fromJust (parseDict "de"))) ""))
    
intParseTest :: TestTree
intParseTest = testCase "parse single digit" (assertEqual "single digit" (Just (1, "")) (parseInt "i1e"))

intParseTest2 :: TestTree
intParseTest2 = testCase "parse more than 3" (assertEqual "More than 3" Nothing (parseInt "i21e"))

moveTest = testCase "Move on empty board" 
  (assertEqual "empty board" [Lib.Move (0, 0) "Ignas" "x"] (move [] "012345678"))
  
moveTest2 = testCase "Move when (0, 0) taken" 
  (assertEqual "empty board" [Lib.Move (1, 0) "Ignas" "x", Lib.Move (0, 0) "Ignas" "x"] (move [Lib.Move (0, 0) "Ignas" "x"] "x12345678"))

moveTest3 = testCase "Move when first row is bad" 
  (assertEqual "empty board" [Lib.Move (0, 1) "Ignas" "x"] (move [] "xx2345678"))
  
moveTest4 = testCase "Move when first column is bad" 
  (assertEqual "empty board" [Lib.Move (1, 0) "Ignas" "x"] (move [] "012x45x78"))
  
moveTest5 = testCase "Move when diagonal is bad" 
  (assertEqual "empty board" [Lib.Move (1, 0) "Ignas" "x"] (move [] "0123x567x"))
  
moveTest6 = testCase "Move when other diagonal is bad" 
  (assertEqual "empty board" [Lib.Move (2, 1) "Ignas" "x"] (move [] "x123x5xx8"))
  
