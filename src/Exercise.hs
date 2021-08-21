module Exercise where

{- 
Os seguintes exercícios, use listas para resolver os problemas propostos.
-}


{-
Um palíndromo é uma palavra que pode ser lida da esquerda para a direita ou da direita para a esquerda com o mesmo resultado, como por exemplo, ovo e Natan.
O conceito pode ser estendido para frases se ignorarmos espaços, acentos e sinais de pontuação, por exemplo, "Olé! Maracujá, caju, caramelo." mas, por enquanto, estamos interessados apenas palavras palíndromas.

Escreva uma função que teste se a entrada é uma palavra (não contém espaços).
Assuma que a entrada não tem acentos ou pontuações e que ou todas as letras são maiúsculas ou todas são minúsculas.

Entrada:
    - s - String

Resultado: 
    - True se s é uma palavra palíndroma; False caso contrário.

Exemplos:
>>>palíndromo "ana"
True
>>>palíndromo "jose"
False

>>>palíndromo "natan foi viajar"
False

>>>palíndromo "natan"
True
-}

palíndromo :: String -> Bool
palíndromo s = s == reverse s

{-
Na próxima semana, escreva uma função que teste se uma frase é palíndroma.
-}


{-
O conceito de palíndromo pode ser aplicado a qualquer lista, por exemplo de inteiros. Neste caso, [1,2,3,4,5,4,3,2,1] seria um palíndromo.
Escreva uma função que teste se uma lista qualquer é um palíndromo.
O tipo da função está definido para você e diz que a lista pode ser de qualquer tipo que se possa comparar com um ==

Entrada:
    - s - lista de inteiros

Resultado: 
    - True se s é uma lista palíndroma; False caso contrário.

Exemplos:
>>>palindrome "ana"
True
>>>palindrome "jose"
False

>>>palindrome [1,2,3]
False

>>>palindrome [True,False,False,True]
True

-}

palindrome :: Eq a => [a] -> Bool
palindrome s = s == reverse s


{-
Frequentemente precisamos limpar dados entrados por usuários em fomulários.
Por exemplo, precisamos tirar os espaços no início e fim dos dados digitados, como em " José de Abreu  "
Algumas linguagens tem uma função trim que remove tais espaços em branco.

Implemente uma função que elimine todos os espaços em branco no início e fim de uma string.

>>>trim "Implemente uma"
"Implemente uma"

>>>trim " Implemente uma "
"Implemente uma"

>>>trim "  Implemente uma  "
"Implemente uma"

>>>trim "   Implemente uma"
"Implemente uma"

>>>trim "Implemente uma   "
"Implemente uma"
-}
trim :: String -> String
trim s = reverse (dropWhile (== ' ') (reverse (dropWhile (== ' ') s)))


{-
Eu não tenho uma historia bonitinha para esta função, então vamos direto ao ponto.
Escreva uma função que quebre uma String em todo lugar em que aparecer um certo caractere.

Entrada:
    - l - String a ser dividida
    - d - delimitador

Saída:
    - Lista de strings resultante da divisão.

>>>splitTodos "Por exemplo, precisamos tirar os espaços no início e fim dos dados digitados, como em José de Abreu " ' '
["Por","exemplo,","precisamos","tirar","os","espa\231os","no","in\237cio","e","fim","dos","dados","digitados,","como","em","Jos\233","de","Abreu",""]

>>>splitTodos "Por exemplo, precisamos tirar os espaços no início e fim dos dados digitados, como em José de Abreu " 'i'
["Por exemplo, prec","samos t","rar os espa\231os no ","n\237c","o e f","m dos dados d","g","tados, como em Jos\233 de Abreu "]

>>>splitTodos "Por exemplo, precisamos tirar os espaços no início e fim dos dados digitados, como em José de Abreu " 'a'
["Por exemplo, precis","mos tir","r os esp","\231os no in\237cio e fim dos d","dos digit","dos, como em Jos\233 de Abreu "]
-}
splitTodos :: [Char] -> Char -> [[Char]]
splitTodos l d = if d `elem` l then primeiro : splitTodos segundo d else [l]
    where primeiro = takeWhile (/= d) l
          segundo = dropWhile (== d) (dropWhile (/= d) l)




{-
Escreva uma função que retorne duplas formadas pelos por elementos das duas metades da lista, sendo o primeiro elemento do resultado formado pelo 
primeiro elemento da primeira metade da lista mais o primeiro da segunda metade da lista, o segundo elemento formado pelo segundo elemento da primeira
metade mais o segundo elemento da segunda metade e assim por diante.

>>>combinaMetades [1,2,3,4,5,6]
[(1,4),(2,5),(3,6)]

>>>combinaMetades [1,2,3,4,5,6,7]
[(1,4),(2,5),(3,6)]

-}

combinaMetades :: [a] -> [(a,a)]
combinaMetades l = zip primeiraMetade segundaMetade
    where (primeiraMetade, segundaMetade) = splitAt (length l `div` 2) l


{-
Escreva uma função que reverta combinaMetades. Ou seja
>>>descombinaMetades [(1,4),(2,5),(3,6)] 
[1,2,3,4,5,6,7]
-}
descombinaMetades :: [(a,a)] -> [a]
descombinaMetades l = descombinaMetadesInterno l [] []
    where descombinaMetadesInterno [] p1 p2 = reverse p1 ++ reverse p2
          descombinaMetadesInterno ((e1,e2):es) p1 p2 = descombinaMetadesInterno es (e1:p1) (e2:p2)


{-
Escreva uma função separe repetições consecutivas dentro de uma lista.

>>>empacote "aaaabccaadeeee"
["aaaa","b","cc","aa","d","eeee"]

>>>empacote ""
[]


>>>empacote [1,1,12,2,2,3,3,3,4,4,4,3,3,3,2,2,2,1,1,1]
[[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3],[2,2,2],[1,1,1]]

-}

empacote :: (Eq a) => [a] -> [[a]]
empacote [] = []
empacote (x:xs) = pacote:resto
    where pacote = x:takeWhile (==x) xs
          resto = empacote (dropWhile (==x) xs)

empacote' :: (Eq a) => [a] -> [[a]]
empacote' l
    | null l = []
    | otherwise  = pacote:resto
    where
          cabeça = head l
          cauda = tail l
          pacote = cabeça:takeWhile (==cabeça) cauda
          resto = empacote (dropWhile (==cabeça) cauda)

{-
Dado uma lista empacotada, como a gerada pela função anterior, gere uma lista de duplas tal que:
- para cada pacote haja uma dupla no resultado.
- a dupla tem como primeiro elemento o dado repetido na lista correspondente e como segundo elemento o comprimento de tal lista.

>>>compacte [[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3],[2,2,2],[1,1,1]]
[(1,2),(12,1),(2,2),(3,3),(4,3),(3,3),(2,3),(1,3)]

>>>compacte ["aaaa","b","cc","aa","d","eeee"]
[('a',4),('b',1),('c',2),('a',2),('d',1),('e',4)]

>>>compacte []
Ambiguous type variable ‘a0’ arising from a use of ‘compacte’
prevents the constraint ‘(Show a0)’ from being solved.
Relevant bindings include
  it :: [(a0, Int)]
    (bound at /Users/lasarocamargos/ufu/github-classroom/autograding-template-haskell-stack/src/Exercise.hs:189:1)
Probable fix: use a type annotation to specify what ‘a0’ should be.
These potential instances exist:
  instance Show a => Show (ZipList a)
    -- Defined in ‘Control.Applicative’
  instance Show NestedAtomically
    -- Defined in ‘Control.Exception.Base’
  instance Show NoMethodError -- Defined in ‘Control.Exception.Base’
  ...plus 221 others
  (use -fprint-potential-instances to see them all)

>>>compacte [[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3]]
[(1,2),(12,1),(2,2),(3,3),(4,3),(3,3)]

-}
compacte :: [[a]] -> [(a, Int)]
compacte l
    | null l = []
    | otherwise = (head cabeca, length cabeca) : compacte cauda
        where cabeca = head l
              cauda = tail l


{-
Escreva uma função que reverta a função compacte, definida acima, ou seja, tal que
>>>descompacte (compacte [[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3]]) == [[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3]]
True

-}

descompacte :: [(a, Int)] -> [[a]]
descompacte l
    | null l = []
    | otherwise = pacote cabeca : descompacte cauda
        where cabeca = head l
              cauda = tail l
              pacote (c,cont) = replicate cont c


{-
Escreva uma função que reverta a função empacote, acima, definida acima, ou seja, tal que
>>>desempacote (empacote "aaaabccaadeeee") == "aaaabccaadeeee"
True

-}

desempacote :: [[a]] -> [a]
desempacote = concat
