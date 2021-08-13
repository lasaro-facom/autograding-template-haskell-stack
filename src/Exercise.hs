module Exercise where

{- 
Nos seguintes exercícios, implemente suas soluções de forma recursiva
Defina os tipos das funções.
-}


{-
Uma função que calcule x * y

Entrada:
    - x
    - y

Resultado: 
    - x * y

Exemplos:
>>>multiplique 2 3
6
>>>multiplique 3 3
9

>>>multiplique 3 (-3)
-9

>>>multiplique (-3) 3
-9

>>>multiplique (-3) (-3)
9

-}

multiplique :: Int -> Int -> Int
multiplique _ 0 = 0
multiplique x 1 = x
multiplique x n
    | n > 0 = x + multiplique x (n-1)
    | otherwise = negate (x + multiplique x (negate n -1))

{-
Uma função que calcule a n-ésima potência de um número x.

Entrada:
    - x: base
    - n: expoente

Resultado: 
    - x elevado a n

Exemplos:
>>>potência 2 3
8
>>>potência (-3) 2
9
-}

potência :: Int -> Int -> Int
potência _ 0 = 1
potência x n = x * potência x (n-1)

{-
Uma função que calcule log base 2 de n

Entrada:
    - n

Resultado: 
    - log_2 (n)

Exemplos:
>>>logBase2 100
6

>>>logBase2 16
4

-}
logBase2 1 = 0
logBase2 n = 1 + logBase2 (n `div` 2)


{-
Uma função que rotacione os elementos de uma tupla n vezes.

Entrada:
    - t: tupla de 5 inteiros.
    - n: número de rotações a ser feito. Rotacionar à direita se n é positivo e a esquerda se n é negativo.

Resultado: 
    - t rotacionado n vezes.

Exemplos:
>>>rotacionar (1,2,3,4,5) 2
(4,5,1,2,3)
>>>rotacionar (1,2,3,4,5) (-2)
(3,4,5,1,2)
-}

rotacionar :: (b, b, b, b, b) -> Int -> (b, b, b, b, b)
rotacionar t 0 = t
rotacionar (u,d,t,q,c) n
    | n > 0 = rotacionar (c,u,d,t,q) (n-1)
    | otherwise = rotacionar (d,t,q,c,u) (n+1)



{-
Uma função que jogue fora os caracteres inicias de uma string s até que o restante da string se inicie com um caractere c ou que a string fique fazia.

Entrada:
    - s: string
    - c: caractere.

Resultado: 
    - a string resultante.

Exemplos:
>>>jogarForaAté "Eu quis dizer, você não quis escutar." ','
", voc\234 n\227o quis escutar."

>>>jogarForaAté "Eu quis dizer, você não quis escutar." 'z'
"zer, voc\234 n\227o quis escutar."

>>>jogarForaAté "Eu quis dizer, você não quis escutar." 'v'
"voc\234 n\227o quis escutar."

-}

jogarForaAté :: [Char] -> Char -> [Char]
jogarForaAté s c
  | 0 == length s = s
  | s !! 0 == c = s
  | otherwise = jogarForaAté (drop 1 s) c


{-
A fórmula de Leibniz para pi (http://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80)
estabelece que a constante pode ser calculada como a série

pi = (4/1) - (4/3) + (4/5) - (4/7)...

Implemente uma função recursiva que calcule a constante até uma quantidade n de termos.

Entrada:
    - n: quantidade de termos

Resultado:
    - pi, calculado com n passos da série

Exemplos:

>>>piDeLeibniz 1
4.0

>>>piDeLeibniz 2
2.666666666666667

>>>piDeLeibniz 3
3.466666666666667

>>>piDeLeibniz 2000
3.1410926536210413

>>>piDeLeibniz 3000
3.1412593202657186

-}

piDeLeibniz :: Int -> Double
piDeLeibniz = piDeLeibniz' (4::Double) 1

piDeLeibniz' :: Double -> Int -> Int -> Double
piDeLeibniz' piAteAgora termo contador
    | termo == contador = piAteAgora
    | otherwise  = piDeLeibniz' (piAteAgora + if even termo  then 4.0 / (2.0 * termoAsDouble + 1.0) else negate 4.0/(2.0* termoAsDouble + 1.0)) (termo +1) contador
    where termoAsDouble = fromIntegral termo :: Double
