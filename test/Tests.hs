import Test.Hspec        (Spec, it, describe, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Exercise

main :: IO ()
main = hspecWith defaultConfig specs

specs :: Spec
specs = do
          describe "Com casamento de padrões" $ do
            it "2 x 3" $
              multiplique 2 3 `shouldBe` 6
            it "3 x 3" $
              multiplique 3 3 `shouldBe` 9
            it "-3 x 3" $
              multiplique (-3) 3 `shouldBe` -9
            it "3 x -3" $
              multiplique 3 (-3) `shouldBe` -9
            it "-3 x -3" $
              multiplique (-3) (-3) `shouldBe` 9
            it "potência 2 3" $
              potência 2 3 `shouldBe` 8
            it "potência -3 2" $
              potência (-3) 2 `shouldBe` 9
            it "potência -3 4" $
              potência (-3) 3 `shouldBe` (-27)
            it "log2 100" $
              logBase2 100 `shouldBe` 6
            it "log2 16" $
              logBase2 16 `shouldBe` 4
            it "rotacionar 1 a 5, 2" $
              shouldBe (rotacionar (1,2,3,4,5) 2) (4,5,1,2,3)
            it "rotacionar 1 a 5, -1" $
              shouldBe (rotacionar (1,2,3,4,5) (-2)) (3,4,5,1,2)
            it "drop ," $
              shouldBe (jogarForaAté "Eu quis dizer, você não quis escutar." ',') ", voc\234 n\227o quis escutar."
            it "drop z" $
              shouldBe (jogarForaAté "Eu quis dizer, você não quis escutar." 'z') "zer, voc\234 n\227o quis escutar."
            it "drop v" $
              shouldBe (jogarForaAté "Eu quis dizer, você não quis escutar." 'v') "voc\234 n\227o quis escutar."
            it "pi 1" $
              shouldBe (piDeLeibniz 1) 4.0
            it "pi 2" $
              shouldBe (piDeLeibniz 2) 2.666666666666667
            it "pi 3" $
              shouldBe (piDeLeibniz 3) 3.466666666666667
            it "pi 2000" $
              shouldBe (piDeLeibniz 2000) 3.1410926536210413
            it "pi 3000" $
              shouldBe (piDeLeibniz 3000) 3.1412593202657186