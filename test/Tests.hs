import Test.Hspec        (Spec, it, describe, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Exercise

main :: IO ()
main = hspecWith defaultConfig specs

specs :: Spec
specs = do
          describe "Teste de funções simples" $ do
            it "Soma tres números Int " $
              somaTresNumeros (1::Int) (2::Int) (3::Int) `shouldBe` (6::Int)
            it "Calcula a area do quadrado " $
              areaQuadrado (2.0::Float) `shouldBe` (4::Float)
            it "Calcula a area do retangulo " $
              areaRetangulo (2::Float) (3::Float) `shouldBe` (6::Float)
            it "Retorna o maior de 3 números" $
              maiorDeTres (1::Integer) (2::Integer) (3::Integer) `shouldBe` (3::Integer)
            it "Pedra Tesoura Papel" $
              pedraTesouraPapel  ("pedra"::String) ("tesoura"::String) `shouldBe` ("pedra"::String)
            it "Pedra Tesoura Papel" $
              pedraTesouraPapel  ("papel"::String) ("tesoura"::String) `shouldBe` ("tesoura"::String)
            it "Pedra Tesoura Papel" $
              pedraTesouraPapel  ("papel"::String) ("papel"::String) `shouldBe` ("empate"::String)
            it "Pedra Tesoura Papel" $
              pedraTesouraPapel  ("papel"::String) ("pedra"::String) `shouldBe` ("papel"::String)
            it "E logico" $
              eLógico  True True `shouldBe` True
            it "E logico" $
              eLógico  True False `shouldBe` False
            it "OU logico" $
              ouLógico  True False `shouldBe` True
            it "OU logico" $
              ouLógico  False False `shouldBe` False
            it "subStringDeAte" $
              subStringDeAte ("entrada1"::String) (2::Int) (2::Int) `shouldBe` ("tr"::String)
            it "subStringDeAte" $
              subStringDeAte ("entrada1") (2::Int) (20::Int) `shouldBe` "trada1"
            it "últimosU" $
              últimosU ("entrada1") (2::Int) `shouldBe` ("a1"::String)
            it "últimosU" $
              últimosU ("entrada1"::String) (100::Int) `shouldBe` ("entrada1"::String)
            it "subStringDeAteAppend" $
              subStringDeAteAppend ("entrada1"::String) ("entrada2"::String) 2 2 `shouldBe` ("trtr"::String)
            it "subStringDeAteAppend" $
              subStringDeAteAppend ("entr"::String) ("ada2"::String) 0 4 `shouldBe` ("entrada2"::String)

          describe "Teste de tuplas" $ do
            it "naipe" $
              naipe (3, "ouro") `shouldBe` "ouro"
            it "naipe" $
              naipe (3, "copas") `shouldBe` "copas"
            it "valor" $
              valor (3, "ouro") `shouldBe` 3
            it "valor" $
              valor (4, "ouro") `shouldBe` 4

            it "menor" $
              cartaÉMenor (1, "ouro") (1, "ouro") `shouldBe` False 
            it "menor" $
              cartaÉMenor (1, "copas") (1, "ouro") `shouldBe` True 
            it "menor" $
              cartaÉMenor (1, "ouro") (2, "ouro") `shouldBe` True

            it "igual" $
              cartaÉIgual (1, "ouro") (1, "ouro") `shouldBe` True 
            it "igual" $
              cartaÉIgual (1, "copas") (1, "ouro") `shouldBe` False 
            it "igual" $
              cartaÉIgual (1, "ouro") (2, "ouro") `shouldBe` False

            it "maior" $
              cartaÉMaior (1, "ouro") (1, "ouro") `shouldBe` False 
            it "maior" $
              cartaÉMaior (1, "copas") (1, "ouro") `shouldBe` False 
            it "maior" $
              cartaÉMaior (1, "ouro") (2, "ouro") `shouldBe` False
            it "maior" $
              cartaÉMaior (3, "ouro") (2, "ouro") `shouldBe` True

            it "jogo" $
              éJogo (1,"ouro") (2,"ouro") (3,"ouro") `shouldBe` True 
            it "jogo" $
              éJogo (1,"ouro") (1,"ouro") (1,"copas") `shouldBe` False 
            it "jogo" $
              éJogo (1,"ouro") (1,"paus") (1,"copas") `shouldBe` True
            it "jogo" $
              éJogo (6,"paus") (4,"ouro") (3,"copas") `shouldBe` False
              
            it "maior mao empate" $
              maiorMão ((5,"paus"),(4,"ouro"),(3,"copas")) ((5,"paus"),(4,"ouro"),(3,"copas")) `shouldBe` ((5,"paus"),(4,"ouro"),(3,"copas"))
            it "maior mao primeira" $
              maiorMão ((5,"paus"),(4,"ouro"),(3,"copas")) ((5,"paus"),(4,"ouro"),(2,"copas")) `shouldBe` ((5,"paus"),(4,"ouro"),(3,"copas"))
            it "maior mao primeira" $
              maiorMão ((5,"paus"),(4,"ouro"),(3,"copas")) ((5,"paus"),(4,"copas"),(3,"copas")) `shouldBe` ((5,"paus"),(4,"ouro"),(3,"copas")) 
            it "maior mao segunda" $
              maiorMão ((4,"paus"),(4,"ouro"),(3,"copas")) ((5,"paus"),(4,"ouro"),(2,"copas")) `shouldBe` ((5,"paus"),(4,"ouro"),(2,"copas")) 

            it "mãoGanhadora empate" $
             mãoGanhadora ((5,"paus"),(4,"ouro"),(3,"copas")) ((5,"paus"),(4,"ouro"),(3,"copas")) `shouldBe` ((0,""),(0,""),(0,""))
            it "maior mao primeira" $
              mãoGanhadora ((5,"paus"),(4,"ouro"),(3,"copas")) ((5,"paus"),(4,"ouro"),(2,"copas")) `shouldBe` ((5,"paus"),(4,"ouro"),(3,"copas"))
            it "maior mao primeira ganhadora segunda" $
              mãoGanhadora ((6,"paus"),(4,"ouro"),(3,"copas")) ((3,"paus"),(3,"ouro"),(3,"copas")) `shouldBe` ((3,"paus"),(3,"ouro"),(3,"copas")) 
            it "maior mao segunda" $
              mãoGanhadora ((4,"paus"),(4,"ouro"),(3,"copas")) ((5,"paus"),(4,"ouro"),(2,"copas")) `shouldBe` ((5,"paus"),(4,"ouro"),(2,"copas")) 
