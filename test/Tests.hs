import Test.Hspec        (Spec, it, describe, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Exercise

main :: IO ()
main = hspecWith defaultConfig specs

specs :: Spec
specs = do
          describe "Funções recursivas, com listas e where ou let-in" $ do
            it "palíndromo ana" $
              palíndromo "ana" `shouldBe` True
            it "palíndromo jose" $
              palíndromo "jose" `shouldBe` False
            it "palíndromo natan" $
              palíndromo "natan" `shouldBe` True
            it "palíndromo natan 2" $
              palíndromo "natan foi viajar" `shouldBe` False
            it "palindrome 123" $
              palindrome [1,2,3] `shouldBe` False
            it "palindrome 12321" $
              palindrome [1,2,3,2,1] `shouldBe` True
            it "palindrome 123321" $
              palindrome [1,2,3,3,2,1] `shouldBe` True
            it "palindrome boolean" $
              palindrome [True,False,False,True] `shouldBe` True

            it "trim 1" $
              trim "Implemente uma" `shouldBe` "Implemente uma"
            it "trim 2" $
              trim " Implemente uma " `shouldBe` "Implemente uma"
            it "trim 3" $
              trim "  Implemente uma  " `shouldBe` "Implemente uma"
            it "trim 4" $
              trim "   Implemente uma" `shouldBe` "Implemente uma"
            it "trim 5" $
              trim "Implemente uma   " `shouldBe` "Implemente uma"

            it "split 1" $
              splitTodos "Por exemplo, precisamos tirar os espaços no início e fim dos dados digitados, como em José de Abreu " ' ' `shouldBe` ["Por","exemplo,","precisamos","tirar","os","espa\231os","no","in\237cio","e","fim","dos","dados","digitados,","como","em","Jos\233","de","Abreu",""]
            it "split 2" $
              splitTodos "Por exemplo, precisamos tirar os espaços no início e fim dos dados digitados, como em José de Abreu " 'i' `shouldBe` ["Por exemplo, prec","samos t","rar os espa\231os no ","n\237c","o e f","m dos dados d","g","tados, como em Jos\233 de Abreu "]
            it "split 3" $
              splitTodos "Por exemplo, precisamos tirar os espaços no início e fim dos dados digitados, como em José de Abreu " 'a' `shouldBe` ["Por exemplo, precis","mos tir","r os esp","\231os no in\237cio e fim dos d","dos digit","dos, como em Jos\233 de Abreu "]

            it "combina 1" $
              combinaMetades [1,2,3,4,5,6] `shouldBe` [(1,4),(2,5),(3,6)]
            it "combina 2" $
              combinaMetades [1,2,3,4,5,6,7] `shouldBe` [(1,4),(2,5),(3,6)]

            it "descombina 1" $
              descombinaMetades [(1,4),(2,5),(3,6)] `shouldBe`  [1,2,3,4,5,6]
            it "descombina 2" $
              descombinaMetades [(1,4),(2,5),(3,6),(10,11)] `shouldBe` [1,2,3,10,4,5,6,11] 

            it "empacote 1" $
              empacote "aaaabccaadeeee" `shouldBe` ["aaaa","b","cc","aa","d","eeee"]
            it "empacote 2" $              
              empacote "" `shouldBe` []
            it "empacote 3" $
              empacote [1,1,12,2,2,3,3,3,4,4,4,3,3,3,2,2,2,1,1,1] `shouldBe` [[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3],[2,2,2],[1,1,1]]

            it "compacte 1" $
              compacte [[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3],[2,2,2],[1,1,1]] `shouldBe` [(1,2),(12,1),(2,2),(3,3),(4,3),(3,3),(2,3),(1,3)]
            it "compacte 2" $
              compacte ["aaaa","b","cc","aa","d","eeee"] `shouldBe` [('a',4),('b',1),('c',2),('a',2),('d',1),('e',4)]
            it "compacte 3" $
              compacte ([]::[String]) `shouldBe` ([]::[(Char,Int)])
            it "compacte 4" $
              compacte [[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3]] `shouldBe` [(1,2),(12,1),(2,2),(3,3),(4,3),(3,3)]

            it "descompacte 1" $
              descompacte  [(1,2),(12,1),(2,2),(3,3),(4,3),(3,3),(2,3),(1,3)] `shouldBe` [[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3],[2,2,2],[1,1,1]]
            it "descompacte 2" $
              descompacte [('a',4),('b',1),('c',2),('a',2),('d',1),('e',4)] `shouldBe` ["aaaa","b","cc","aa","d","eeee"]
            it "descompacte 3" $
              descompacte ([]::[(Char,Int)]) `shouldBe` ([]::[String])
            it "descompacte 4" $
              descompacte [(1,2),(12,1),(2,2),(3,3),(4,3),(3,3)] `shouldBe` [[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3]] 


            it "desempacote 1" $
              desempacote ["aaaa","b","cc","aa","d","eeee"] `shouldBe` "aaaabccaadeeee" 
            it "empacote 2" $              
              desempacote ([]::[String]) `shouldBe` ([]::String)
            it "desempacote 3" $
              desempacote [[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3],[2,2,2],[1,1,1]] `shouldBe` [1,1,12,2,2,3,3,3,4,4,4,3,3,3,2,2,2,1,1,1] 
