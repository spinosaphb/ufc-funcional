--000 retorna o indice onde ocorre o elemento
indice' xs n i | i == (length xs)-1 = -1
indice' xs n i = if(xs!!i==n) then i else indice' xs n (i+1)
indice xs n = indice' xs n 0
--00 menor de 2
menor2 x y = if x > y then y else x
--01 menor de 3
menor3 x y z = menor2 (menor2 x y) z
--02 retorna o fatorial
fatorial 0 = 1
fatorial n = n* fatorial (n-1) 
--03 retorna a serie de fibonacci
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)
--04 lista o n-esimo elemento da lista
element xs n = xs!!n
--05 True n e xs,False caso contrario
pertence [] n = False
pertence xs n = do
    if(xs!!((length xs)-1)==n) then
        True
    else
        pertence (init xs) n
--06 retorna o numero de elementos de xs
len' [] n = n
len' xs n = len' (init xs) (n+1)
len xs = len' xs 0
--07 maximo valor de xs
maior xs | (length xs) == 1 = xs!!0
maior xs = do
    if(xs!!((length xs)-1) > xs!!0) then
        maior (tail xs)
    else
        maior (init xs)
--08 lista o total de ocorrencias de n em xs
frequence xs n = length[i|i<-xs,i==n]
--09 True se n ocorre exatamente 1 vez em xs
unico xs n = length [i|i<-xs,i==n] == 1
--10 sublista de xs tal que n < i onde i e xs
maiorQue xs n = [i|i<-xs,i>n]
--11 concat de a e b
concats a b = a++b
--12 calda
calda xs = drop 1 xs
--13 corpo
corpo xs = take ((length xs) - 1) xs
--14 lista xs sem repeticoes
unique' [] xn = xn
unique' xs xn = unique' (tail xs) (xn++[i|i<-[head xs],not(pertence xn i)])
unique xs = unique' xs []
--15 lista de n menores elementos de xs na ordem em que aparecem
menores xs n | n == (length xs) = xs
menores xs n = menores [i|i<-xs,i/=(maior xs)] n
--16 lista todos os inteiros alternados ate n
alter' xs n i | i == n = (xs++[i]++[i*(-1)])
alter' xs n i = alter' (xs++[i]++[i*(-1)]) n (i+1)
alter n = alter' [] n 1
--17 lista xs em ordem reversa
reverso' [] xv = xv
reverso' xs xv = reverso' (init xs) (xv++[last xs])
reverso xs = reverso' xs [] 
--18 Tupla de duas listas, (A, B), onde A é formada pelas n primeiras chaves de u e B pelos elementos restantes
tuplefy [xs,xn] = (xs,xn)
divide xs n = tuplefy([(take (n) xs)]++[(drop (n) xs)])
--19 itercala xs e xn
intercal' xs xn xv | (length (xs++xn)) == 0 = xv
intercal' xs xn xv = intercal' (tail xs) (tail xn) (xv++[head xs]++[head xn])
intercal xs xn = intercal' xs xn []
--20 duas listas a e b sem repeticoes
uniao xa xb = unique (xa++xb)
--21 intercao de duas listas
intersec xa xb = unique[i|i<-(xa++xb),not $ unico (xa++xb) i]
--22 comparar se duas listas sao iguais
igual xs [] = False
igual [] xs = False
igual [] [] = True
igual xs xz = do
    if(xs!!0 == xz!!0) then
        igual (tail xs) (tail xz)
    else
        False
--23 retorna -1 se o primeiro for menor , 1 se o primeiro for maior, 0 caso igual
strequal [] [] = 0
strequal xs xz = do
    if(xs!!0 == xz!!0) then
        strequal (tail xs) (tail xz)
    else
        if(xs!!0 < xz!!0) then
            (-1)
        else
            1
