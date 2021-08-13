import Test.Hspec        (Spec, it, describe, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Exercise

main :: IO ()
main = hspecWith defaultConfig specs

specs :: Spec
specs = do
          describe "Com casamento de padrões" $ do
            it "Naipe 1 " $
              naipe (1,Ouro) `shouldBe` Ouro
            it "Naipe 2 " $
              naipe (1,Copas) `shouldBe` Copas
            it "Valor 1 " $
              valor (1,Ouro) `shouldBe` 1
            it "Valor 2 " $
              valor (2,Copas) `shouldBe` 2
            it "Naipe igual 1" $
              naipeIgual (1,Ouro) (2,Ouro) `shouldBe` True
            it "Naipe igual 2" $
              naipeIgual (1,Ouro) (2,Paus) `shouldBe` False
            it "Valor por extenso 1" $
              valorPorExtenso (1,Ouro) `shouldBe` "Um"
            it "Valor por extenso 2" $
              valorPorExtenso (2,Ouro) `shouldBe` "Dois"
            it "Valor por extenso 3" $
              valorPorExtenso (12,Ouro) `shouldBe` "Dama"
            it "Naipe por extenso 1" $
              naipePorExtenso (1,Ouro) `shouldBe` "Ouro"
            it "Naipe por extenso 2" $
              naipePorExtenso (2,Paus) `shouldBe` "Paus"
            it "Naipe por extenso 3" $
              naipePorExtenso (12,Copas) `shouldBe` "Copas"
            it "Sequencia de naipes 1" $
              sequênciaDeNaipes (1,Paus) (2,Ouro) (7,Copas) `shouldBe` False
            it "Sequencia de naipes 2" $
              sequênciaDeNaipes (1,Paus) (7,Copas) (2,Ouro) `shouldBe` False
            it "Sequencia de naipes 3" $
              sequênciaDeNaipes (1,Paus) (2,Espada) (7,Copas) `shouldBe` False
            it "Sequencia de naipes 4" $
              sequênciaDeNaipes (1,Espada) (2,Copas) (7,Ouro) `shouldBe` False
            it "Sequencia de naipes 5" $
              sequênciaDeNaipes (1,Paus) (7,Copas)  (2,Espada) `shouldBe` True
            it "Sequencia de naipes 6" $
              sequênciaDeNaipes (2,Copas) (1,Espada) (7,Ouro) `shouldBe` True
            it "Data por extenso 1" $
              dataPorExtenso 1 1 2001 `shouldBe` "Primeiro de Janeiro de 2001"
            it "Data por extenso 2" $
              dataPorExtenso 10 3 2010 `shouldBe` "Dez de Março de 2010"
