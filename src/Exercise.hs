module Exercise where

-- Defina os tipos das seguintes funções de acordo com as descrições.


{-
- entrada: três números inteiros de precisão finita (Int)
- saída: Int
-}
somaTresNumeros :: Num a => a -> a -> a -> a
somaTresNumeros n1 n2 n3 = n1 + n2 + n3

{-
Esta função retorna a área do quadrado de lado l

areaQuadrado
- Entrada: l, Float
- Saída: l**2, Float
-}
areaQuadrado :: Floating a => a -> a
areaQuadrado l = l**2

{-
Esta função retorna a área do retângulo de lado l1 e l2

areaRetangulo
- Entrada: l1 e l2, Floats
- Saída: l1 * l2, Float
-}
areaRetangulo :: Num a => a -> a -> a
areaRetangulo l1 l2 = l1 * l2


-- Defina as seguintes funções usando if then else
{-
Esta função retorna o maior de tres números

maiorDeTres
- Entrada: a b c, Integer
- Saída: o maior dentre a b c, Integer
-}
maiorDeTres :: Ord a => a -> a -> a -> a
maiorDeTres a b c
  | a >= b && a >= c = a
  | b >= c = b
  | otherwise = c


-- Defina as seguintes funções, incluindo a definição de tipos.

{-
Esta função calcula quem ganha no jogo pedra/tesoura/papel

pedraTesouraPapel
- Entrada: m1 m2 - "pedra" "tesoura" "papel"
- Saída: O valor ganhador, ou "empate" no caso empate

>>>pedraTesouraPapel "pedra" "papel"
"papel"

>>>pedraTesouraPapel "pedra" "tesoura"
"pedra"

>>>pedraTesouraPapel "papel" "tesoura"
"tesoura"
-}
pedraTesouraPapel :: String -> String -> String
pedraTesouraPapel m1 m2
  | m1 == "pedra" && m2 == "papel" = "papel"
  | m1 == "pedra" && m2 == "tesoura" = "pedra"
  | m1 == "papel" && m2 == "pedra" = "papel"
  | m1 == "papel" && m2 == "tesoura" = "tesoura"
  | m1 == "tesoura" && m2 == "papel" = "tesoura"
  | m1 == "tesoura" && m2 == "pedra" = "pedra"
  | otherwise = "empate"


{-
Esta função calcula a conjunção do dois parâmetros (e lógico).
-}
eLógico :: Bool -> Bool -> Bool
eLógico p1 p2 = p1 && p2

{-
Esta função calcula a disjunção do dois parâmetros (ou lógico).
-}
ouLógico :: Bool -> Bool -> Bool
ouLógico p1 p2 = p1 || p2

-- Defina as seguintes funções usando ++ !! take e reverse.

{-
Defina função que retorne substring de t elementos começando na posição i
subStringDeAte "entrada1" 2 2 retorna "tr"

Entrada:
    - s: string
    - i: inteiro
    - t: inteiro
-}
subStringDeAte :: String -> Int -> Int -> String
subStringDeAte s i t = reverse (take t (reverse (take (i + t) s)))

{-
Defina função que retorne substring com os últimos u elementos

Entrada:
    - s: string
    - u: inteiro
-}
últimosU :: String -> Int -> String
últimosU s u = reverse (take u (reverse s))

{-
Defina função que receba duas strings e retorne a resultado da concatenação das substrings de t elementos começando na posição i

Entrada
    - s1: string
    - s2: string
    - i: inteiro
    - u: inteiro
-}

subStringDeAteAppend :: String -> String -> Int -> Int -> String
subStringDeAteAppend s1 s2 i u = subStringDeAte s1 i u ++ subStringDeAte s2 i u



-- Nas próximas funções, trabalharemos com definições de tipo e tuplas.

{-
Seja o tipo de dados Carta tupla em que 
 - o primeiro elemento é o valor da carta (1,2,3,4,5,6,7,8,9,10,11,12,13) 
 - o segundo é o naipe ("ouro", "copas", "espada", "paus").
-}
type Carta = (Int, String)

{-
Uma função que receba uma carta retorne seu naipe.

Entrada:
    - c1: carta

Resultado: naipe da carta
-}
naipe :: Carta -> String
naipe (_,n) = n

{-
Uma função que receba uma carta retorne seu valor.

Entrada:
    - c1: carta

Resultado: valor da carta
-}
valor :: Carta -> Int
valor (v,_) = v


{-
Uma função que receba duas cartas e diga se a primeira é menor que a segunda.
Uma carta c1 é menor que uma carta c2 se valor c1 < valor2 OU se valor c1 == valor c2 e naipe c1 < c2.
"copas" < "espada" < "ouro" < "paus"

Entrada:
    - c1, c2: Carta
Resultado: True ou False
-}

cartaÉMenor :: Carta -> Carta -> Bool
cartaÉMenor c1 c2 = c1 < c2

{-
Uma função que receba duas cartas e diga se a primeira é igual à segunda.

Entrada:
    - c1, c2: Carta
Resultado: True ou False
-}

cartaÉIgual :: Carta -> Carta -> Bool
cartaÉIgual c1 c2 = c1 == c2

{-
Uma função que receba duas cartas e diga se a primeira é maior que a segunda.

Entrada:
    - c1, c2: Carta
Resultado: True ou False
-}
cartaÉMaior :: Carta -> Carta -> Bool
cartaÉMaior c1 c2 = c1 > c2


{-
Uma função que receba três cartas c1 c2 c3 e diga se formam um jogo.
- seja m1 a maior dentre as cartas c1 c2 c3
- seja m2 a de valor mediano dentre as cartas c1 c2 c3
- seja m3 a menor dentre as cartas c1 c2 c3
m1 m2 e m3 formam um jogo se e somente si
    - OU naipe m1 == naipe m2 == naipe m3 E valor m1 == valor m2 + 1 == valor m3 + 2
    - OU naipe m1 =/= naipe m2 =/= naipe m3 =/= m1 E valor m1 == valor m2 == valor m3


Entrada:
    - c1, c2, c3: Carta
Resultado: True ou False
-}
éJogo :: Carta -> Carta -> Carta -> Bool
éJogo c1 c2 c3
    |naipe c1 == naipe c2 && naipe c2 == naipe c3 && valor (maiorDeTresC c1 c2 c3) == valor (medianaDeTres c1 c2 c3) + 1 && valor (medianaDeTres c1 c2 c3) == valor (menorDeTres c1 c2 c3) + 1 = True
    |naipe c1 /= naipe c2 && naipe c2 /= naipe c3 && naipe c3 /= naipe c2 && valor c1 == valor c2 && valor c2 == valor c3 = True
    |otherwise = False


maiorDeTresC :: Carta -> Carta -> Carta -> Carta
maiorDeTresC c1 c2 c3
    | c1 >= c2 && c1 >= c3 = c1
    | c2 >= c3 = c2
    | otherwise = c3

menorDeTres :: Carta -> Carta -> Carta -> Carta
menorDeTres c1 c2 c3
    | c1 <= c2 && c1 <= c3 = c1
    | c2 <= c3 = c2
    | otherwise = c3

cartaPraNumero :: Carta -> Int
cartaPraNumero (v, n)
    | n == "copas" = (v-1) * 4 + 1 
    | n == "espada" = (v-1) * 4 + 2
    | n == "ouro" = (v-1) * 4 + 3
    | otherwise = (v-1) * 4 + 4

numeroPraCarta :: Int -> Carta
numeroPraCarta i
    | (i-1) `rem` 4 == 0 = ((i-1) `div` 4 +1,"copas")
    | (i-1) `rem` 4 == 1 = ((i-1) `div` 4 +1,"espada")
    | (i-1) `rem` 4 == 2 = ((i-1) `div` 4 +1,"ouro")
    | otherwise  = ((i-1) `div` 4 +1,"paus")


medianaDeTres :: Carta -> Carta -> Carta -> Carta
medianaDeTres c1 c2 c3 = numeroPraCarta (cartaPraNumero c1 + cartaPraNumero c2 + cartaPraNumero c3 - cartaPraNumero aMenor - cartaPraNumero aMaior)
    where aMenor = menorDeTres c1 c2 c3
          aMaior = maiorDeTres c1 c2 c3

{-
Defina uma função que receba duas tuplas de 3 cartas, onde a primeira carta é maior ou igual à segunda, que é maior ou igual à terceira, 
e compare as tuplas para dizer qual é maior.
Uma tupla t1 é menor que uma tupla t2 se e somente se
- a primeira carta de t1 é menor que a primeira de t2 OU
- as primeiras cartas são iguais mas a segunda carta de t1 é menor que a segunda de t2
- as primeiras e segundas cartas são iguais mas a terceira carta de t1 é menor que a terceira de t2
-}
maiorMão :: (Carta, Carta, Carta) -> (Carta, Carta, Carta) -> (Carta, Carta, Carta)
maiorMão t1 t2
    |t1 >= t2 = t1
    |otherwise = t2




{-
Defina uma função que receba duas tuplas de 3 cartas, onde a primeira carta é maior ou igual à segunda, que é maior ou igual à terceira, 
e compare as tuplas para dizer qual ganha, ou se houve empate.
Uma tupla t1 ganha de uma tupla t2 se 
- t1 é um jogo mas t2 não é um jogo
- t1 e t2 são um jogo mas t1 é maior que t2.
- No caso de empate, retorne a tupla vazia ((0,""),(0,""),(0,""))
-}

mãoGanhadora :: (Carta, Carta, Carta) -> (Carta, Carta, Carta) -> (Carta, Carta, Carta)
mãoGanhadora (ce1, ce2, ce3) (cd1, cd2, cd3)
    |éJogo ce1 ce2 ce3 && not (éJogo cd1 cd2 cd3) = (ce1, ce2, ce3)
    |not (éJogo ce1 ce2 ce3) && éJogo cd1 cd2 cd3 = (cd1, cd2, cd3)
    |(ce1, ce2, ce3) == (cd1, cd2, cd3) = ((0,""),(0,""),(0,""))
    |otherwise = maiorMão (ce1, ce2, ce3) (cd1, cd2, cd3)
