module Exercise where

import Debug.Trace ( trace )

{-
A função maximum procura retorna o maior elemento em uma lista.
Escreva uma função com comportamento similar, recursiva.
-}

máximo :: [Int] -> Int
máximo [] = error "Erro. Não há máximo de uma lista vazia."
máximo [x] = x
máximo (x:xs)
   | x > maxTail = x
   | otherwise = maxTail
   where maxTail = máximo xs


{-
A fórmula de Bhaskara permite calcular as raízes de uma equação de segundo grau
na forma ax^2 + bx + c = 0. A resolução é normalmente dividida em duas partes, o
cálculo do discriminante, Delta, e das raízes. O Delta é calculado pela equação
seguinte:
 Delta  = b^2 - 4ac

Calculado o delta, a seguinte equação calcula as raízes.
 raiz1 = (-b + Delta^(1/2)) / 2a
 raiz2 = (-b - Delta^(1/2)) / 2a

Observe que:
 Se Delta > 0, a equação do segundo grau tem 2 raízes.
 Se Delta = 0, 1 raiz.
 Se Delta < 0, tem 0 raízes reais.


1 - Escreva uma função que receba uma tripla com os coeficientes da equação,
isto é, (a,b,c), e retorne o valor de Delta.

2 - Escreva uma função que receba uma tripla com os coeficientes da equação,
isto é, (a,b,c), retorne uma lista com as raízes da equação de segundo
grau. Defina a função usando guardas. Utilize a função delta.
-}

delta :: Num a => (a, a, a) -> a
delta (a,b,c) = b^2 - 4*a*c

raízes :: (Ord t, Floating t, Show t) => (t, t, t) -> [t]
raízes (a,b,c)
  | d > 0 = [raiz1,raiz2]
  | d == 0 = [raiz1]
  | otherwise  = []
  where d = delta (a,b,c)
        raiz1 =  (negate b + sqrt d)/(2*a)
        raiz2 =  (negate b - sqrt d)/(2*a)


{-
Considere que o preço de uma passagem de ônibus intermunicipal pode variar dependendo
da idade do passageiro
- bebês (abaixo de 2 anos) pagam apenas 15%. 
- crianças menos de 10 anos pagam 40% 
- pessoas com 70 anos ou mais pagam apenas 50% do preço total. 
- os demais passageiros pagam a tarifa normal, 100%. 

Faça uma função que tenha como entrada:
- o valor total da passagem,
- a data atual e 
- a data de nascimento do passageiro. 

Como saída, a função retorna o valor a ser pago. 

Obs. 1: na solução, deve ser definido o tipo data para representar a tupla de inteiros (d,m,a).
Obs. 2: assuma que as datas estão corretas.
Obs. 3: assuma que todos os meses tem 30 dias e o ano tem 360 dias.
-}



type Data = (Int, Int, Int)

valorFinal :: Float -> Data -> Data -> Float
valorFinal preço (dn,mn,an) (da, ma, aa)
   | qtdDias < doisAnos = preço * 0.15
   | qtdDias < dezAnos = preço * 0.4
   | qtdDias < setentaAnos = preço
   | otherwise = preço * 0.5
   where doisAnos = 360*2
         dezAnos = 360*10
         setentaAnos = 360 * 70
         qtdDias = (da + (ma-1)*30 + (aa-1)*360) - (dn + (mn-1)*30 + (an-1)*360)




data Filtro = Menor | Maior | Igual deriving (Eq)

{-
O tipo Filtro pode ter um dos três valores definidos na linha anterior.
Escreva uma função recursiva que receba como entrada
- tupla com Filtro f na primeira posição e inteiro i na segunda posição.
- lista de inteiros l

Retorne
- Lista com todos os inteiros em l que são menores que i, se f for Menor, maiores que i se
f for Maior, e iguais a i, se f for Igual.
-}

filtre :: (Filtro,Int) -> [Int] -> [Int]
filtre (Maior,i) l = [e | e <- l, e > i]
filtre (Menor,i) l = [e | e <- l, e < i]
filtre (Igual,i) l = [e | e <- l, e == i]

{-
Sabendo que:
- no mercado de ações brasileiro, ações são negociadas em lotes de 100 unidades;
- cada ação é identificada por um nome único, o "ticker", por exemplo VALE3 ou BOVA11;
- quando se compra um lote de ações, ele vai para a "carteira" do comprador;
- os proprietários das ações usam o custo médio das ações para calcular lucros e prejuízos.

Implemente as seguintes funções:
* compre
  - Entrada
     + uma tupla com o ticker (String) e o preço da ação (por unidade)
     + a quantidade de ações a comprar (múltiplo do tamanho de um lote)
     + a carteira a atual, na forma de uma lista de tuplas com ticker e custo médio das ações.
  - Retorna
     + a nova carteira, corrigida pela adição das ações compradas e com preços médios atualizados.

* venda
  - Entrada
     + uma tupla com o ticker (String) e o preço da ação (por unidade)
     + a quantidade de ações a vender (múltiplo do tamanho de um lote)
     + a carteira a atual, na forma de uma lista de tuplas com ticker e custo médio das ações.
  - Retorna
     + a nova carteira, corrigida pela remoção das ações vendidas. Se a venda não for possível,
     a carteira permanece intacta.


-}

compre :: (String, Float) -> Int -> [(String, Float, Int)] -> [(String, Float, Int)]
compre (t,p) q [] = [(t, p, q)] 
compre c@(t,p) q (x@(tt,pt,qt):xs)
   | t /= tt = x: compre c q xs
   | otherwise = (tt,(pt* fromIntegral qt + p* fromIntegral q)/fromIntegral (qt+q), qt+q):xs

venda :: (String, Float) -> Int -> [(String, Float, Int)] -> [(String, Float, Int)]
venda _ _ [] = [] 
venda v@(t,_) q (x@(tt,pt,qt):xs)
   | t /= tt = x : venda v q xs
   | qt - q == 0 = xs
   | otherwise = (tt, pt, qt-q):xs
