import Test.Hspec        (Spec, it, describe, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Exercise

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
          describe "Teste de funcões simples" $ do
            it "Retorna uma string" $
              digaMeuNome `shouldBe` digaMeuNome
            it "Soma tres números " $
              somaTresNumeros 1 2 3 `shouldBe` 6
            it "Soma tres números " $
              somaTresNumeros 100 200 300 `shouldBe` 600
            it "Calcula a area do quadrado " $
              areaQuadrado 2 `shouldBe` 4
            it "Calcula a area do quadrado " $
              areaQuadrado 3 `shouldBe` 9
            it "Calcula a area do retangulo " $
              areaRetangulo 2 3 `shouldBe` 6
            it "Calcula a area do retangulo " $
              areaRetangulo 5 3 `shouldBe` 15
            it "Calcula a area do círculo " $
              areaCirculo 3 `shouldBe` pi * 3**2
            it "Calcula a area do triâgulo " $
              areaTriangulo 2 2 `shouldBe` 2
            it "Calcula a area do trapézio " $
              areaTrapezio 2 4 3 `shouldBe` 9
            it "Calcula a hipotenusa " $
              hipotenusa 3 4 `shouldBe` 5
            it "Converts C to F " $
              celsius2fahrenheit 0 `shouldBe` 32
            it "Converts C to F " $
              celsius2fahrenheit 100 `shouldBe` 212
            it "Converts F to C " $
              fahrenheit2celsius 212 `shouldBe` 100
            it "Converts F to C " $
              fahrenheit2celsius 32 `shouldBe` 0

          describe "Teste de funcões com if-then-else" $ do
            it "Retorna o maior de 3 números" $
              maiorDeTres 1 2 3 `shouldBe` 3
            it "Retorna o maior de 3 números" $
              maiorDeTres 3 2 1 `shouldBe` 3
            it "Retorna o maior de 3 números" $
              maiorDeTres (-3) (-2) (-1) `shouldBe` (-1)
            it "Ordenados?" $
              estaoOrdenados (-3) (-2) (-1) `shouldBe` False
            it "Ordenados?" $
              estaoOrdenados 3 2 1 `shouldBe` True 
            it "Ordenados?" $
              estaoOrdenados 10 8 9 `shouldBe` False
            it "Desconto" $
              precoComDesconto 1 1 3 0.1 `shouldBe` 1
            it "Desconto" $
              precoComDesconto  1 3 3 0.1 `shouldBe` 3
            it "Desconto" $
              precoComDesconto  1 4 3 0.1 `shouldBe` 3.6
            it "Pedra Tesoura Papel" $
              pedraTesouraPapel  0 1 `shouldBe` True
            it "Pedra Tesoura Papel" $
              pedraTesouraPapel  1 0 `shouldBe` False
            it "Pedra Tesoura Papel" $
              pedraTesouraPapel  0 2 `shouldBe` False
            it "Pedra Tesoura Papel" $
              pedraTesouraPapel  2 0 `shouldBe` True

          describe "Teste de funcões com if-then-else" $ do
            it "Pedra Tesoura Papel" $
              pedraTesouraPapelGuardas  0 1 `shouldBe` True
            it "Pedra Tesoura Papel" $
              pedraTesouraPapelGuardas  1 0 `shouldBe` False
            it "Pedra Tesoura Papel" $
              pedraTesouraPapelGuardas  0 2 `shouldBe` False
            it "Pedra Tesoura Papel" $
              pedraTesouraPapelGuardas  2 0 `shouldBe` True
            it "Sobe ou Desce?" $
              sobeDesceBagunca 1 2 3 `shouldBe` 1
            it "Sobe ou Desce?" $
              sobeDesceBagunca 3 2 1 `shouldBe` -1
            it "Sobe ou Desce?" $
              sobeDesceBagunca 1 3 2 `shouldBe` 0
            it "Dias por mes" $ do
              diasMes 1 `shouldBe` 31
            it "Dias por mes" $ do
              diasMes 12 `shouldBe` 31
            it "Dias por mes" $ do
              diasMes 11 `shouldBe` 30
