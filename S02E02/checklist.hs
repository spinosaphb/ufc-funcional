import Data.Char (ord,chr) -- para a questao 28
--import Control.Monad (filterM)
--000 retorna o indice onde ocorre o elemento
indice' xs n i | i == (length xs)-1 = i
indice' xs n i = if(xs!!i==n) then i else indice' xs n (i+1)
indice xs n = indice' xs n 0
--001 retorna duas listas separadas por i
partire xs i = take i xs
partird xs i = drop (i+1) xs
--002 retira todas as ocorrencias de n na lista
partir xs i | pertence xs i == False = xs
partir xs i = (take (indice xs i) xs) ++ (partir (drop ((indice xs i)+1) xs) i)
--003 deixa em caixa alta a primeira letra da String
upperfst' xs i | i == ((length xs)-1) = xs
upperfst' xs i = do
    if((xs!!i==' ') && (xs!!(i+1)>='a')) then
        partire xs i ++" "++(upperfst(partird xs i))
    else
        upperfst' xs (i+1)
upperfst xs = do
    if(((head xs) /= ' ') && (head xs) >='a') then
       upperfst' ([(chr((ord (head xs))-32))] ++ (tail xs)) 0
    else
        upperfst' xs 0
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
--22 sequencia de n elementos partindo de m
sequencia n m | n == 0 = []
sequencia n m = [m] ++ sequencia (n-1) (m+1)
--23 inserir x em uma lista ordenada
inserir' x xs i | xs!!((length xs)-1) < x = xs ++[x]
inserir' x xs i = do
    if( x < xs!!i) then
        (take (i) xs) ++ [x] ++ (drop (i) xs)
    else
        inserir' x xs (i+1)
inserir x xs = inserir' x xs 0
--24 True se xs eh uma lista ordenada e False caso contrario
isSorted [x]  = True
isSorted xs = do
    if(xs!!0 <= xs!!1) then
        isSorted (tail xs) 
    else
        False
--25 Ordenar elementos de uma lista
---0.25 deleta um elemento da lista
delete' x i xs = do
    if(x == xs!!i) then
        (take (i-1) xs)++(drop (i+1) xs)
    else
        delete' x (i+1) xs
delete x xs = delete' x 0 xs; 

qsort xs = do
    if(isSorted xs) then
        xs
    else
        qsort (delete (maior xs) (xs ++ [maior xs]))
--26 rotaciona na lista xs n vezes a esquerda
rotesq 0 xs = xs 
rotesq n xs = rotesq (n-1) (tail (xs++[head xs])) 
--27 rotaciona na lista xs n vezes a direita
rotdir n xs = reverse(rotesq n (reverse xs))
--28 dada uma string, transformar em caixa alta
upper' str (-1) = str 
upper' str i = do
    if((str!!i /= ' ') && (str!!i >= 'a')) then
       upper' ((partire str i)++[chr((ord(str!!i))-32)]++(partird str i)) (i-1) 
    else
        upper' str (i-1)
upper str = upper' str ((length str)-1)
-- 0.28 dada uma string, transformar em caixa baixa
down' str (-1) = str
down' str i = do
    if(((str!!i)/= ' ') && ((str!!i)<'a')) then
        down' ((partire str i)++[chr((ord(str!!i))+32)]++(partird str i)) (i-1)
    else
        down' str (i-1)
down str = down' str ((length str)-1)
--29 dada uma string, transformar em caixa baixa execeto a letra inicial
titulo xs = upperfst (down xs)
--30 lista de xs cujos indices estao em xi
selec xs [] = []
selec xs xi = [xs!!(head xi)] ++ (selec xs (tail xi)) 
--31 retorna True se a lista eh palindroma e False caso contrario
isPalind xs | length xs == 1 || length xs == 0 = True  
isPalind xs = do
    if(last xs == head xs) then
        isPalind (init (tail xs))
    else
        False
--0.32 retorna uma lista de 1 a n com todos os numeros que dividem n
factor n = [i | i<-[1..n],(mod n i) ==0]
--32 retorna True se n primo e False caso contrario
primo n = factor n == [1,n]
--33 soma dos digitos de n
sdig 0 = 0
sdig n = (mod n 10) + (sdig (n/10))
--0.34 swap de  elementos em uma lista de 2 elementos
swap xs = (drop 1 xs) ++ (take 1 xs)
--34 bubblesort de uma lista
bubblesort' xs i | (length xs)-1 == i = do
    if(isSorted xs) then
        xs
    else
        bubblesort' xs 0
bubblesort' xs i = do
    if(xs!!i>xs!!(i+1)) then
        bubblesort' ((take (i) xs) ++ (swap ([xs!!i]++[xs!!(i+1)])) ++ (drop (i+2) xs)) (i+1)
    else
        bubblesort' xs (i+1) 
bubblesort xs = bubblesort' xs 0
--0.35 sublista de nx elementos e elemento x
sub' [xs] x i n = [x]
sub' xs x i n = do
    if(xs!!i == x) then
        if(i<((length xs)-1)) then
            sub' xs x (i+1) (n+1) 
        else
            [(n+1),x]     
    else    
        [n,x] 
sub xs x = sub' xs x (indice xs x) 0
--0.35.1 remove o indice onde o elemento e 1 [1,3] == [3]
removeone xs i | i == ((length xs)-1) = xs
removeone xs i = do
    if((head (xs!!i)) == 1) then
        removeone ((partire xs i)++[[last (xs!!i)]]++(partird xs i)) (i+1)
    else
        removeone xs (i+1)
--35 Cada lista-componente possui um ou dois elementos.
compac' [] xn = xn
compac' xs xn = compac' (drop (head (sub xs (head xs))) xs) (xn++[(sub xs (head xs))])
compac xs = removeone (compac' xs []) 0
--36 tupla composta por A e B onde A sao impares e B pares
splitints xs = tuplefy [[i|i<-xs,mod i 2==1],[i|i<-xs,mod i 2==0]]
--37 retorna True se n eh um quadrado perfeito
perfeito' [] x = False
perfeito' xi x = do
    if(sum xi == x) then
        True
    else
        perfeito' (init xi) x
perfeito x = perfeito' [i|i<-[0..x],mod i 2 ==1 && i < x] x
--38 representacao do inteiro n na base b
charToStr c = [c]
tochar x = charToStr(chr(65+(x-10)))  
testchar n = do
    if(n>9) then
        tochar n
    else
        show n
base n b | n < b = testchar n
base n b = (base (div n b) b) ++ (testchar(mod n b))
--39 todos os subconjunstos distintos e possiveis de xs
partes [] = [[]]
partes (x:xs) = let partes_xs = partes xs in partes_xs++[(x:z) | z <- partes_xs]



