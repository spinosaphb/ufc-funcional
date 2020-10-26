qtd' x [] i = i
qtd' x xs i = 
    if(head xs == x) then
        qtd' x (tail xs) (i+1)
    else
        qtd' x (tail xs) i
qtd x xs = qtd' x xs 0
    


delete x xs = [i|i<-xs,i/=x]

 
compac' [] xv = xv
compac' xs xv =
    if(qtd (xs!!0) xs == 1) then
        compac' (delete (xs!!0) xs) (xv++[[(xs!!0)]])
    else     
        compac' (delete (xs!!0) xs) (xv++[([(xs!!0)]++[(qtd (xs!!0) xs)])])
compac xs = tail (compac' xs [[]])


--compac (x:xs) = [[x]++[(qtd x xs)+1]]++compac(delete x xs)