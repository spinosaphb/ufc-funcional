----------------------------------------------QUESTS-------------------------------------------
--01.Escreva uma função soma que recebe dois parâmetros e devolve a soma dos dois parâmetros.
soma x y = x+y
--02.Defina a função interior tal que (interior xs) é uma lista obtida eliminando os extremos da lista xs.
interior xs = [i|i<-(init (tail(xs)))]
--03.Dado três valores a, b e c, escreva uma função iguais3 que retorne quantos dos três são iguais.
iguais3 a b c = do
    if a == b then
        if a == c then 3
        else 2
    else 
        if a == c then 2
        else
            if b == c then 2
            else 0
--04.Defina a função max3 tal que (max3 x y z) é o máximo entre x, y e z.
max3 x y z = max x (max y z)
--05.Defina uma função somaImpares tal que (somaImpares xs) devolve a soma dos elementos ímpares de uma lista.
somaMembros [] = 0
somaMembros xs = xs!!((length xs)-1) + somaMembros (init xs)
somaImpares xs = somaMembros [i|i<-xs,mod i 2/=0]
--06.Defina a função neglist xs que computa o número de elementos negativos em uma lista xs.
neglist xs = length [i|i<-xs,i<0]
--07.Defina a função final tal que (final xs) é uma lista formada pelos n elementos finais de xs. 
final n xs = reverse (take n [i|i<-reverse xs])
--08.screva uma função gangorra que recebe quatro inteiros P_1, C_1, P_2 e C_2...
gangorra p c p' c' = do
    if p*c > p'*c' then -1
    else
        if p*c < p'*c' then 1
        else 0 
--09.remove o miolo da lista
exterior xs = [head xs] ++ [last xs]


