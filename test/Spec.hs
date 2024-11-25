-- #############################################################################
-- ########### YOUR UNIT TESTS                                    ##############
-- ########### Note: execute tests using "stack test ploy:units"  ##############
-- #############################################################################

import Board
import Ploy
import Test.Hspec

main :: IO ()
main = hspec $ do testAll

testBoard1 :: Board
testBoard1 = [[Empty, Piece White 84, Piece White 41, Piece White 56, Empty, Piece White 56, Piece White 41, Piece White 84, Empty], [Empty, Empty, Piece White 24, Piece White 40, Piece White 17, Piece White 40, Piece White 48, Empty, Empty], [Empty, Empty, Empty, Piece White 16, Piece White 16, Piece White 16, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Piece Black 1, Piece Black 1, Piece Black 1, Empty, Empty, Empty], [Empty, Empty, Piece Black 3, Piece Black 130, Piece Black 17, Piece Black 130, Piece Black 129, Empty, Empty], [Empty, Piece Black 69, Piece Black 146, Piece Black 131, Piece Black 170, Piece Black 131, Piece Black 146, Piece Black 69, Empty]]

testAll :: Spec
testAll =
  describe "to test eveything..." $ do
    it "10" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 12) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 2) 1)] :: [Move])
    it "11" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 192) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 2) 1)] :: [Move])
    it "12" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 5) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 2) 1)] :: [Move])
    it "13" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 130) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 2) 1)] :: [Move])
    it "24" $ do
      possibleMoves (Pos 'b' 1) (Empty) `shouldBe` ([] :: [Move])
    it "25" $ do
      dltDup [1, 1, 2] `shouldBe` ([1, 2] :: [Int])
    it "26" $ do
      ifShield (Empty) `shouldBe` (False :: Bool)
    it "27" $ do
      prepareBoard [] `shouldBe` ([] :: Board)
    it "28" $ do
      addEmptyCol [] `shouldBe` ([] :: Board)
    it "29" $ do
      addEmptyRow [] `shouldBe` ([] :: Board)
    it "30" $ do
      addEmptyRow testBoard1 `shouldBe` (([] : testBoard1) :: Board)
    it "31" $ do
      Black `shouldBe` (Black :: Player)
    it "32" $ do
      Black == White `shouldBe` (False :: Bool)
    it "33" $ do
      Pos 'a' 1 `shouldBe` (Pos 'a' 1 :: Pos)
    it "34" $ do
      Pos 'a' 1 == Pos 'a' 2 `shouldBe` (False :: Bool)
    it "41" $ do
      Pos 'a' 1 == Pos 'b' 1 `shouldBe` (False :: Bool)
    it "42" $ do
      Pos 'a' 1 == Pos 'b' 2 `shouldBe` (False :: Bool)
    it "43" $ do
      Pos 'a' 1 == Pos 'a' 1 `shouldBe` (True :: Bool)
    it "44" $ do
      Pos 'a' 1 == Pos 'b' 1 `shouldBe` (False :: Bool)
    it "45" $ do
      Pos 'a' 1 == Pos 'b' 2 `shouldBe` (False :: Bool)
    it "46" $ do
      Pos 'a' 1 == Pos 'a' 1 `shouldBe` (True :: Bool)
    it "47" $ do
      (Piece White 1) == Empty `shouldBe` (False :: Bool)
    it "48" $ do
      validateFigurs [] `shouldBe` (True :: Bool)
    it "49" $ do
      validateFigurs [""] `shouldBe` (True :: Bool)
    it "50" $ do
      validateFigurs ["x21"] `shouldBe` (False :: Bool)
    it "51" $ do
      validateFigurs ["21b"] `shouldBe` (False :: Bool)
    it "52" $ do
      validateFigurs ["21"] `shouldBe` (False :: Bool)
    it "53" $ do
      validateFigurs ["b500"] `shouldBe` (False :: Bool)
    it "54" $ do
      validateFigurs ["b "] `shouldBe` (False :: Bool)
    it "55" $ do
      validateFigurs ["b"] `shouldBe` (False :: Bool)
    it "56" $ do
      validateFigurs ["h"] `shouldBe` (False :: Bool)
    it "57" $ do
      validateFigurs ["h "] `shouldBe` (False :: Bool)
    it "58" $ do
      validateFigurs ["22"] `shouldBe` (False :: Bool)
    it "59" $ do
      validateFigurs [" 22"] `shouldBe` (False :: Bool)
    it "60" $ do
      validateFigurs ["500"] `shouldBe` (False :: Bool)
    it "61" $ do
      validateFigurs ["h500"] `shouldBe` (False :: Bool)
    it "62" $ do
      validateFigurs ["t 89"] `shouldBe` (False :: Bool)
    it "63" $ do
      validateFigurs [" 22"] `shouldBe` (False :: Bool)
    it "64" $ do
      validateFigurs ["500"] `shouldBe` (False :: Bool)
    it "65" $ do
      validateFigurs [" b500 "] `shouldBe` (False :: Bool)
    it "66" $ do
      validateFigurs [" b89"] `shouldBe` (False :: Bool)
    it "67" $ do
      validateFEN ",,,,,,,,,/,,,,,,,,," `shouldBe` (False :: Bool)
    it "68" $ do
      isValidMove testBoard1 (Move (Pos 'c' 1) (Pos 'c' 1) 1) `shouldBe` (True :: Bool)
    it "69" $ do
      isValidMove testBoard1 (Move (Pos 'c' 1) (Pos 'c' 1) 0) `shouldBe` (False :: Bool)
    it "70" $ do
      isValidMove testBoard1 (Move (Pos 'c' 1) (Pos '2' 1) 1) `shouldBe` (False :: Bool)
    it "71" $ do
      isValidMove testBoard1 (Move (Pos 'c' 1) (Pos '3' 1) 1) `shouldBe` (False :: Bool)
    it "72" $ do
      isValidMove testBoard1 (Move (Pos 'c' 1) (Pos 'c' 3) 0) `shouldBe` (False :: Bool)
    it "18" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 7) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 2) 1)] :: [Move])
    it "19 1" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 170) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 2) 1)] :: [Move])
    it "20" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 85) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 2) 1)] :: [Move])
    it "21" $ do
      possibleMoves (Pos 'e' 2) (Piece Black 17) `shouldContain` ([(Move (Pos 'e' 2) (Pos 'e' 1) 0)] :: [Move])
    it "22" $ do
      possibleMoves (Pos 'e' 1) (Piece Black 170) `shouldContain` ([(Move (Pos 'e' 1) (Pos 'd' 2) 0)] :: [Move])
    it "23" $ do
      possibleMoves (Pos 'c' 1) (Piece Black 146) `shouldContain` ([(Move (Pos 'c' 1) (Pos 'b' 2) 0)] :: [Move])
    it "73" $ do
      isValidMove testBoard1 (Move (Pos 'd' 7) (Pos 'd' 6) 7) `shouldBe` (True :: Bool)
    it "74" $ do
      isValidMove testBoard1 (Move (Pos 'd' 7) (Pos 'd' 6) 0) `shouldBe` (True :: Bool)
    it "75" $ do
      isValidMove testBoard1 (Move (Pos 'c' 8) (Pos 'd' 7) 1) `shouldBe` (False :: Bool)
    it "76" $ do
      listMoves testBoard1 Black `shouldBe` ([] :: [Move])
    it "77" $ do
      listMoves testBoard1 Black `shouldBe` ([] :: [Move])
    it "78" $ do
      validateCommasCount [] `shouldBe` (False :: Bool)
    it "79" $ do
      validateSlashs [] `shouldBe` (False :: Bool)
    it "80" $ do
      validateFEN [] `shouldBe` (False :: Bool)
    it "81" $
      validateFEN (",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` (True :: Bool)
    it "82" $
      validateFEN (",w84,w41,w56,w170,w56,w41,w300,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` (False :: Bool)
    it "83" $
      validateFEN (",w84,r41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` (False :: Bool)
    it "84" $
      validateFEN (",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` (False :: Bool)
    it "35" $ do
      Pos 'a' 1 == Pos 'b' 1 `shouldBe` (False :: Bool)
    it "36" $ do
      Pos 'a' 1 == Pos 'b' 2 `shouldBe` (False :: Bool)
    it "37" $ do
      Pos 'a' 1 == Pos 'a' 1 `shouldBe` (True :: Bool)
    it "38" $ do
      Pos 'a' 1 == Pos 'b' 1 `shouldBe` (False :: Bool)
    it "39" $ do
      Pos 'a' 1 == Pos 'b' 2 `shouldBe` (False :: Bool)
    it "40" $ do
      Pos 'a' 1 == Pos 'a' 1 `shouldBe` (True :: Bool)
    it "85" $
      validateFEN (",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,//,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` (False :: Bool)
    it "86" $
      validateFEN (",84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` (False :: Bool)
    it "87" $
      validateFEN (",w,w84,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` (False :: Bool)
    it "88" $
      validateFEN (",a84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` (False :: Bool)
    it "89" $
      validateFEN (",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,") `shouldBe` (True :: Bool)
    it "90" $
      validateFEN ("w13,w84,w41,w56,w170,w56,w41,w84,w11/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` (True :: Bool)
    it "91" $
      validateFEN (",w840,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` (False :: Bool)
    it "92" $
      validateFEN ("") `shouldBe` (False :: Bool)
    it "93" $
      validateFEN (",b840,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` (False :: Bool)
    it "94" $
      buildBoard (",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` [[Empty, Piece White 84, Piece White 41, Piece White 56, Piece White 170, Piece White 56, Piece White 41, Piece White 84, Empty], [Empty, Empty, Piece White 24, Piece White 40, Piece White 17, Piece White 40, Piece White 48, Empty, Empty], [Empty, Empty, Empty, Piece White 16, Piece White 16, Piece White 16, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Piece Black 1, Piece Black 1, Piece Black 1, Empty, Empty, Empty], [Empty, Empty, Piece Black 3, Piece Black 130, Piece Black 17, Piece Black 130, Piece Black 129, Empty, Empty], [Empty, Piece Black 69, Piece Black 146, Piece Black 131, Piece Black 170, Piece Black 131, Piece Black 146, Piece Black 69, Empty]]
    it "95" $
      buildBoard (",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,") `shouldBe` [[Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]]
    it "96" $
      buildBoard "" `shouldBe` []
    it "97" $
      line (Pos 'a' 1) (Pos 'd' 1) `shouldBe` [(Pos 'a' 1), (Pos 'b' 1), (Pos 'c' 1), (Pos 'd' 1)]
    it "98" $
      line (Pos 'a' 4) (Pos 'd' 1) `shouldBe` [(Pos 'a' 4), (Pos 'b' 3), (Pos 'c' 2), (Pos 'd' 1)]
    it "99" $
      line (Pos 'a' 1) (Pos 'd' 4) `shouldBe` [(Pos 'a' 1), (Pos 'b' 2), (Pos 'c' 3), (Pos 'd' 4)]
    it "100" $
      line (Pos 'd' 4) (Pos 'a' 4) `shouldBe` [(Pos 'd' 4), (Pos 'c' 4), (Pos 'b' 4), (Pos 'a' 4)]
    it "106" $
      show (Pos 'a' 1) `shouldBe` ("Pos {col = 'a', row = 1}" :: String)
    it "107" $
      show (Piece White 1) `shouldBe` ("Piece White 1" :: String)
    it "108" $
      show Empty `shouldBe` ("Empty" :: String)
    it "109" $
      show (Move (Pos 'a' 1) (Pos 'a' 1) 1) `shouldBe` ("a1-a1-1" :: String)
    it "110" $ do
      show (target (Move (Pos 'f' 4) (Pos 'f' 4) 5)) `shouldBe` ("Pos {col = 'f', row = 4}" :: String)
    it "111" $ do
      show (start (Move (Pos 'f' 4) (Pos 'f' 4) 5)) `shouldBe` ("Pos {col = 'f', row = 4}" :: String)
    it "112" $
      show (Piece Black 130) `shouldBe` ("Piece Black 130" :: String)
    it "113" $ do
      show (turn (Move (Pos 'i' 3) (Pos 'i' 3) 7)) `shouldBe` ("7" :: String)
    it "114" $ do
      show (col (Pos 'a' 1)) `shouldBe` ("'a'" :: String)
    it "115" $ do
      show (row (Pos 'a' 1)) `shouldBe` ("1" :: String)
    it "14" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 129) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 2) 1)] :: [Move])
    it "15" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 20) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 2) 1)] :: [Move])
    it "16" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 224) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 2) 1)] :: [Move])
    it "17" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 28) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 2) 1)] :: [Move])
    it "116" $ do
      show (row (Pos 'a' 1)) `shouldBe` ("1" :: String)
    it "117" $ do
      possibleMoves (Pos 'd' 3) (Piece White 31) `shouldBe` ([] :: [Move])
    it "118" $ do
      testTrn 'd' 3 31 31 `shouldBe` ([] :: [Move])
    it "119" $ do
      moveN 'z' 11 11 11 `shouldBe` ([] :: [Move])
    it "120" $ do
      qq2 testBoard1 Black 31 31 `shouldBe` ([] :: [Move])
    it "121" $ do
      qq3 testBoard1 Black 31 31 `shouldBe` ([] :: [Move])
    it "122" $ do
      qq5 Black (Piece White 1) 31 31 `shouldBe` ([] :: [Move])
    it "123" $
      gameFinished ([[Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]]) `shouldBe` (True :: Bool)
    it "124" $
      gameFinished ([[Empty, Piece White 84, Piece White 41, Piece White 56, Empty, Piece White 56, Piece White 41, Piece White 84, Empty], [Empty, Empty, Piece White 24, Piece White 40, Piece White 17, Piece White 40, Piece White 48, Empty, Empty], [Empty, Empty, Empty, Piece White 16, Piece White 16, Piece White 16, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Piece Black 1, Piece Black 1, Piece Black 1, Empty, Empty, Empty], [Empty, Empty, Piece Black 3, Piece Black 130, Piece Black 17, Piece Black 130, Piece Black 129, Empty, Empty], [Empty, Piece Black 69, Piece Black 146, Piece Black 131, Piece Black 170, Piece Black 131, Piece Black 146, Piece Black 69, Empty]]) `shouldBe` (True :: Bool)
    it "125" $
      gameFinished ([[Empty, Piece White 84, Piece White 41, Piece White 56, Empty, Piece White 56, Piece White 41, Piece White 84, Empty], [Empty, Empty, Piece White 24, Piece White 40, Piece White 17, Piece White 40, Piece White 48, Empty, Empty], [Empty, Empty, Empty, Piece White 16, Piece White 16, Piece White 16, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Piece Black 1, Piece Black 1, Piece Black 1, Empty, Empty, Empty], [Empty, Empty, Piece Black 3, Piece Black 130, Piece Black 17, Piece Black 130, Piece Black 129, Empty, Empty], [Empty, Piece Black 69, Piece Black 146, Piece Black 131, Piece White 170, Piece Black 131, Piece Black 146, Piece Black 69, Empty]]) `shouldBe` (True :: Bool)
    it "126" $
      gameFinished ([[Empty, Piece White 170, Piece White 41, Piece White 56, Empty, Piece White 56, Piece White 41, Piece White 84, Empty], [Empty, Empty, Piece White 24, Piece White 40, Piece White 17, Piece White 40, Piece White 48, Empty, Empty], [Empty, Empty, Empty, Piece White 16, Piece White 16, Piece White 16, Empty, Empty, Empty], [Empty, Piece Black 170, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Piece White 1, Piece White 1, Piece White 1, Empty, Empty, Empty], [Empty, Empty, Piece White 3, Empty, Piece White 17, Piece White 130, Piece White 129, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]]) `shouldBe` (True :: Bool)
    it "127" $
      gameFinished ([[Empty, Piece Black 84, Empty, Empty, Empty, Piece Black 56, Piece Black 41, Piece Black 84, Empty], [Empty, Empty, Piece Black 24, Empty, Empty, Piece Black 40, Piece White 48, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Piece Black 1, Piece Black 1, Piece Black 1, Empty, Empty, Empty], [Empty, Empty, Piece Black 3, Piece Black 130, Piece Black 17, Piece Black 130, Piece Black 129, Empty, Empty], [Empty, Piece Black 69, Piece Black 146, Piece Black 131, Piece Black 170, Piece Black 131, Piece Black 146, Piece Black 69, Empty]]) `shouldBe` (True :: Bool)
    it "128" $
      gameFinished ([[Piece Black 170, Empty, Piece White 170, Empty, Empty, Empty, Empty, Empty, Empty], [Piece White 17, Empty, Empty, Empty, Empty, Piece Black 18, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]]) `shouldBe` (False :: Bool)
    it "1" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 1) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 2) 1)] :: [Move])
    it "2" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 2) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 2) 1)] :: [Move])
    it "3" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 4) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 2) 1)] :: [Move])
    it "4" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 8) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 2) 1)] :: [Move])
    it "101" $
      line (Pos 'd' 4) (Pos 'a' 1) `shouldBe` [(Pos 'd' 4), (Pos 'c' 3), (Pos 'b' 2), (Pos 'a' 1)]
    it "102" $
      line (Pos 'd' 1) (Pos 'a' 4) `shouldBe` [(Pos 'd' 1), (Pos 'c' 2), (Pos 'b' 3), (Pos 'a' 4)]
    it "103" $
      line (Pos 'd' 4) (Pos 'd' 1) `shouldBe` [(Pos 'd' 4), (Pos 'd' 3), (Pos 'd' 2), (Pos 'd' 1)]
    it "104" $
      line (Pos 'd' 1) (Pos 'd' 4) `shouldBe` [(Pos 'd' 1), (Pos 'd' 2), (Pos 'd' 3), (Pos 'd' 4)]
    it "105" $
      line (Pos 'a' 1) (Pos 'a' 1) `shouldBe` [(Pos 'a' 1)]
    it "5" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 16) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 2) 1)] :: [Move])
    it "5" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 32) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 2) 1)] :: [Move])
    it "6" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 64) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 2) 1)] :: [Move])
    it "7" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 128) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 2) 1)] :: [Move])
    it "8" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 3) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 3) 0), (Move (Pos 'c' 2) (Pos 'd' 3) 0)] :: [Move])
    it "9" $ do
      possibleMoves (Pos 'c' 2) (Piece Black 48) `shouldContain` ([(Move (Pos 'c' 2) (Pos 'c' 2) 1)] :: [Move])
    it "129" $
      gameFinished ([[Empty, Piece White 170, Piece White 41, Piece White 56, Empty, Piece White 56, Piece White 41, Piece White 84, Empty], [Empty, Empty, Piece White 24, Piece White 40, Piece White 17, Piece White 40, Piece White 48, Empty, Empty], [Empty, Empty, Empty, Piece White 16, Piece White 16, Piece White 16, Empty, Empty, Empty], [Empty, Piece Black 170, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Piece White 1, Piece White 1, Piece White 1, Empty, Empty, Empty], [Empty, Empty, Piece White 3, Empty, Piece White 17, Piece White 130, Piece White 129, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]]) `shouldBe` (True :: Bool)
    it "130" $
      gameFinished ([[Empty, Piece Black 84, Empty, Empty, Empty, Piece Black 56, Piece Black 41, Piece Black 84, Empty], [Empty, Empty, Piece Black 24, Empty, Empty, Piece Black 40, Piece White 48, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Piece Black 1, Piece Black 1, Piece Black 1, Empty, Empty, Empty], [Empty, Empty, Piece Black 3, Piece Black 130, Piece Black 17, Piece Black 130, Piece Black 129, Empty, Empty], [Empty, Piece Black 69, Piece Black 146, Piece Black 131, Piece Black 170, Piece Black 131, Piece Black 146, Piece Black 69, Empty]]) `shouldBe` (True :: Bool)
    it "131" $
      gameFinished ([[Piece Black 170, Empty, Piece White 170, Empty, Empty, Empty, Empty, Empty, Empty], [Piece White 17, Empty, Empty, Empty, Empty, Piece Black 18, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]]) `shouldBe` (False :: Bool)
    it "132" $
      gameFinished ([[Piece Black 170, Empty, Piece White 170, Empty, Empty, Empty, Empty, Empty, Empty], [Piece White 17, Empty, Empty, Empty, Empty, Piece Black 18, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]]) `shouldBe` (False :: Bool)
    it "133" $
      toChar 1 `shouldBe` ('a' :: Char)
    it "134" $
      toChar 2 `shouldBe` ('b' :: Char)
    it "135" $
      toChar 3 `shouldBe` ('c' :: Char)
    it "136" $
      toChar 4 `shouldBe` ('d' :: Char)
    it "137" $
      toChar 5 `shouldBe` ('e' :: Char)
    it "138" $
      toChar 6 `shouldBe` ('f' :: Char)
    it "139" $
      toChar 7 `shouldBe` ('g' :: Char)
    it "140" $
      toChar 8 `shouldBe` ('h' :: Char)
    it "141" $
      toChar 9 `shouldBe` ('i' :: Char)
    it "142" $
      valueOfCol 'a' `shouldBe` (1 :: Int)
    it "143" $
      valueOfCol 'b' `shouldBe` (2 :: Int)
    it "144" $
      valueOfCol 'c' `shouldBe` (3 :: Int)
    it "145" $
      valueOfCol 'd' `shouldBe` (4 :: Int)
    it "146" $
      valueOfCol 'e' `shouldBe` (5 :: Int)
    it "147" $
      valueOfCol 'f' `shouldBe` (6 :: Int)
    it "148" $
      valueOfCol 'g' `shouldBe` (7 :: Int)
    it "149" $
      valueOfCol 'h' `shouldBe` (8 :: Int)
    it "150" $
      valueOfCol 'i' `shouldBe` (9 :: Int)
    it "151" $
      valueOfCol 'x' `shouldBe` (0 :: Int)
    it "152" $
      valueOfRow 1 `shouldBe` (1 :: Int)
    it "153" $
      valueOfRow 2 `shouldBe` (2 :: Int)
    it "154" $
      valueOfRow 3 `shouldBe` (3 :: Int)
    it "155" $
      valueOfRow 4 `shouldBe` (4 :: Int)
    it "156" $
      valueOfRow 5 `shouldBe` (5 :: Int)
    it "157" $
      valueOfRow 6 `shouldBe` (6 :: Int)
    it "158" $
      valueOfRow 7 `shouldBe` (7 :: Int)
    it "159" $
      valueOfRow 8 `shouldBe` (8 :: Int)
    it "160" $
      valueOfRow 9 `shouldBe` (9 :: Int)
    it "161" $
      valueOfRow 123 `shouldBe` (0 :: Int)
    it "162" $
      notOutofBorder 't' 11 'r' 56 `shouldBe` (False :: Bool)
    it "163" $
      rotate 1 11 == 5 `shouldBe` (False :: Bool)
    it "164" $
      listMoves [[Empty, Piece White 84, Piece White 41, Piece White 56, Piece White 170, Piece White 56, Piece White 41, Piece White 84, Empty], [Empty, Empty, Piece White 24, Piece White 40, Piece White 17, Piece White 40, Piece White 48, Empty, Empty], [Empty, Empty, Empty, Piece White 16, Piece White 16, Piece White 16, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Piece Black 1, Piece Black 1, Piece Black 1, Empty, Empty, Empty], [Empty, Empty, Piece Black 3, Piece Black 130, Piece Black 17, Piece Black 130, Piece Black 129, Empty, Empty], [Empty, Piece Black 69, Piece Black 146, Piece Black 131, Piece Black 170, Piece Black 131, Piece Black 146, Piece Black 69, Empty]] Black `shouldContain` ([(Move (Pos 'd' 3) (Pos 'd' 3) 1)] :: [Move])
    it "165" $
      listMoves [[Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Piece Black 69, Piece Black 146, Piece Black 131, Piece Black 10, Piece Black 131, Piece Black 146, Piece Black 69, Empty]] Black `shouldBe` ([] :: [Move])