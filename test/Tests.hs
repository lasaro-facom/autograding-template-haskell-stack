import Test.Hspec        (Spec, it, describe, shouldBe, shouldMatchList)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Exercise

main :: IO ()
main = hspecWith defaultConfig spec

spec::Spec
spec = do
        describe "máximo" $ do
                it "Ordem crescente" $ do
                        máximo [1, 2, 3, 4, 5] `shouldBe` (5 :: Int)
                it "Ordem decrescente" $ do
                        máximo [117, 56, 38, 11, 0] `shouldBe` (117 :: Int)
                it "Ordem aleatória" $ do
                        máximo [42, 11, 38, 75, 14] `shouldBe` (75 :: Int)

        describe "delta" $ do
                it "delta 1" $ do
                        delta (1,12,-13) `shouldBe` (196)
                it "delta 2" $ do
                        delta (2,-16,-18) `shouldBe` (400)
                it "delta 3" $ do
                        delta (1,3,-4) `shouldBe` (25)
                it "delta 4" $ do
                        delta (2,-4,0) `shouldBe` (16)
                it "delta 5" $ do
                        delta (1,-2,16) `shouldBe` (-60)
                it "delta 6" $ do
                        delta (100,0,0) `shouldBe` (0)
        describe "raízes" $ do
                it "raiz 1" $ do
                        raízes (1,12,-13) `shouldBe` [1,-13]
                it "raiz 2" $ do
                        raízes (2,-16,-18) `shouldBe` [9,-1]
                it "raiz 3" $ do
                        raízes (1,3,-4) `shouldBe` [1,-4]
                it "raiz 4" $ do
                        raízes (2,-4,0) `shouldBe` [2,0]
                it "raiz 5" $ do
                        raízes (1,-2,16) `shouldBe` []
                it "raiz 6" $ do
                        raízes (100,0,0) `shouldBe` [0]

        describe "valor final" $ do
                it "desconto 1" $ do
                        valorFinal 100 (1,1,1930) (1,1,2000) `shouldBe` (50::Float)
                it "desconto 2" $ do
                        valorFinal 1000 (1,1,1930) (1,1,2000) `shouldBe` (500::Float)
                it "desconto 3" $ do
                        valorFinal 100 (1,1,1990) (1,1,2000) `shouldBe` (100::Float)
                it "desconto 4" $ do
                        valorFinal 100 (1,1,1980) (1,1,2000) `shouldBe` (100::Float)
                it "desconto 5" $ do
                        valorFinal 100 (2,1,1990) (1,1,2000) `shouldBe` (40::Float)

        describe "filtragem" $ do
                it "vazia" $ do
                        filtre (Maior,3) [] `shouldBe` []
                it "vazia" $ do
                        filtre (Igual,3) [1,2,4] `shouldBe` []
                it "menores" $ do
                        filtre (Igual,3) [1,2,4] `shouldBe` []

        describe "compre" $ do
                it "vazia 1" $ do
                        compre ("BOVA11", 115) 100 [] `shouldMatchList` [("BOVA11", 115, 100) ]
                it "repete 1" $ do
                        compre ("BOVA11", 100) 100 [("BOVA11", 50, 100)] `shouldMatchList` [("BOVA11", 75, 200) ]
                it "vazia 2" $ do
                        compre ("VALE3", 30) 200 [("BOVA11", 50, 100)] `shouldMatchList` [("BOVA11",50, 100),("VALE3", 30, 200)]
                it "repete 2" $ do
                        compre ("VALE3", 40) 200 [("BOVA11", 50, 100),("VALE3", 30, 200)] `shouldMatchList` [("BOVA11",50, 100),("VALE3", 35, 400)]

        describe "venda" $ do
                it "vazia 1" $ do
                        venda ("BOVA11", 115) 100 [("BOVA11", 115, 100)] `shouldMatchList` []
                it "repete 1" $ do
                        venda ("BOVA11", 100) 100 [("BOVA11", 50, 200) ] `shouldMatchList` [("BOVA11", 50, 100)]
                it "vazia 2" $ do
                        venda ("VALE3", 30) 200 [("BOVA11",50, 100),("VALE3", 30, 200)] `shouldMatchList` [("BOVA11", 50, 100)]
                it "repete 2" $ do
                        venda ("VALE3", 40) 200 [("BOVA11",50, 100),("VALE3", 35, 400)] `shouldMatchList` [("BOVA11", 50, 100),("VALE3", 35, 200)]