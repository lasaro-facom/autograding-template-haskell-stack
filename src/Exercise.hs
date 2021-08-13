module Exercise where


{-
Assim como Bool pode assumir os valores True e False, o tipo Naipe pode assumir um dos valores Copas, Espada, Ouro e Paus, tal que
Copas < Espada < Ouro < Paus

>>>Copas > Espada
False

>>> Espada > Ouro
False

>>> Paus > Copas
True

>>>maiorDeTres Copas Espada Paus
Paus

>>>maiorDeTres Copas Copas Copas
Copas

-}
data Naipe = Copas | Espada | Ouro | Paus deriving (Ord,Eq,Show)


{-
Seja o tipo de dados Carta tupla em que 
 - o primeiro elemento é o valor da carta (1,2,3,4,5,6,7,8,9,10,11,12,13) 
 - o segundo é um Naipe
-}
type Carta = (Int, Naipe)


-- Defina as seguintes funções usando apenas casamento de padrões do lado esquerdo das equações.

{-
Uma função que receba uma carta retorne seu Naipe.

Entrada:
    - c1: carta

Resultado: 
    - naipe da carta

Exemplos:
>>>naipe (1,Ouro)
Ouro
-}
naipe :: Carta -> Naipe
naipe (_,n) = n


{-
Uma função que receba uma carta retorne seu valor.

Entrada:
    - c1: carta

Resultado: 
    - valor da carta

Exemplos:
>>>valor (1,Ouro)
1
-}
valor :: Carta -> Int
valor (v,_) = v


{-
Uma função que receba duas cartas retorne se seus naipes são iguais.

Entrada:
    - c1: carta
    - c2: carta

Resultado: 
    - naipe de c1 igual a naipe de c2?

Exemplos:
>>>naipeIgual (1,Ouro) (2,Ouro)
True
>>>naipeIgual (1,Ouro) (2,Paus)
False
-}

naipeIgual :: Carta -> Carta -> Bool
naipeIgual (_,c1n) (_,c2n) = c1n == c2n

{-
Uma função que receba uma carta e retorne seu valor por extenso.

Entrada:
    - c1: carta

Resultado: 
    - valor de c1 por extenso

Exemplos:
>>>valorPorExtenso (1,Ouro)
"Um"
>>>valorPorExtenso (2,Paus)
"Dois"
>>>valorPorExtenso (12,Ouro)
"Dama"
-}


valorPorExtenso :: Carta -> String
valorPorExtenso (1,_) = "Um"
valorPorExtenso (2,_) = "Dois"
valorPorExtenso (3,_) = "Três"
valorPorExtenso (4,_) = "Quatro"
valorPorExtenso (5,_) = "Cinco"
valorPorExtenso (6,_) = "Seis"
valorPorExtenso (7,_) = "Sete"
valorPorExtenso (8,_) = "Oito"
valorPorExtenso (9,_) = "Nove"
valorPorExtenso (10,_) = "Dez"
valorPorExtenso (11,_) = "Valete"
valorPorExtenso (12,_) = "Dama"
valorPorExtenso (13,_) = "Rei"

{-
Uma função que receba uma carta e retorne seu naipe por extenso.

Entrada:
    - c1: carta

Resultado: 
    - naipe de c1 por extenso

Exemplos:
>>>naipePorExtenso (1,Ouro)
"Ouro"
>>>naipePorExtenso (2,Paus)
"Paus"
>>>naipePorExtenso (12,Ouro)
"Ouro"
-}
naipePorExtenso :: Carta -> String
naipePorExtenso (_,n) = show n

{-
Uma função que receba três cartas e retorne um booleano dizendo se formam uma sequencia, isto é, se estão
aparecem dentro da seguinte sequência: Copas Espada Ouro Paus Copas Espada

Entrada:
    - Carta
    - Carta
    - Carta

Resultado: 
    - Estão em sequência?

Exemplos:
>>>sequênciaDeNaipes (1,Paus) (2,Ouro) (7,Copas)
False

>>>sequênciaDeNaipes (1,Paus) (7,Copas) (2,Ouro) 
False

>>>sequênciaDeNaipes (1,Paus) (2,Espada) (7,Copas)
True

>>>sequênciaDeNaipes (1,Espada) (2,Copas) (7,Ouro)
True
-}
sequênciaDeNaipes :: Carta -> Carta -> Carta -> Bool
sequênciaDeNaipes (_,Copas) (_,Espada) (_,Ouro) = True
sequênciaDeNaipes (_,Espada) (_,Ouro) (_,Paus)= True
sequênciaDeNaipes (_,Ouro) (_,Paus) (_,Copas) = True
sequênciaDeNaipes (_,Paus) (_,Copas) (_,Espada)= True
sequênciaDeNaipes (_,_) (_,_) (_,_) = False 

{-
Uma função que recebe uma data na forma de três inteiros e retorna a da por extenso.

Entrada:
    - Dia
    - Mês
    - Ano

Resultado
    - Data por extenso
    - Quando o dia for 1, usar Primeiro como extenso.

Exemplos:
>>>dataPorExtenso 1 1 2001
"Primeiro de Janeiro de 2001"

>>>dataPorExtenso 10 3 2010
"Dez de Março de 2010"
-}

dataPorExtenso :: Int -> Int -> Int -> String
dataPorExtenso dia mes ano = error "Implementar"
