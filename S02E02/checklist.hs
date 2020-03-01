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
unique' xs i | i == ((length xs) - 1) = xs
unique' xs i = do
    if(unico xs (xs!!i)) then
        unique' xs (i+1)
    else
        unique' (take (i+1) xs ++ [j|j<-[z|z<-drop i xs],if j == (xs!!i) then unico xs j else j==j]) (i+1)
unique xs = unique' xs 0
--15 lista de n menores elementos de xs na ordem em que aparecem
menores xs n | n == (length xs) = xs
menores xs n = menores [i|i<-xs,i/=(maior xs)] n
--16 lista todos os inteiros alternados ate n
alter' xs n i | i == n = (xs++[i]++[i*(-1)])
alter' xs i n = alter' (xs++[i]++[i*(-1)]) n (i+1)
alter n = alter' [] 1 n
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