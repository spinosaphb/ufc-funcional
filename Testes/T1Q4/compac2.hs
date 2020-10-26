
qtd' x [y] i = 
    if(y==x) then
        (i+1)
    else
        i
qtd' x xs i =
    if(head xs == x) then
        qtd' x (tail xs) (i+1)
    else
        i
qtd i xs = qtd' i xs 0

delete x [y] = 
    if(x==y) then
        []
    else
        [y]
delete x xs = 
    if(head xs == x) then
        delete x (tail xs)
    else
        xs

compac' [] xv = xv
compac' xs xv = 
    if(cont == 1) then
        compac' (delete (xs!!0) xs) (xv++[[xs!!0]])
    else
        compac' (delete (xs!!0) xs) (xv++[[xs!!0]++[cont]])
            where
            cont = qtd (xs!!0) xs

    

compac xs = tail (compac' xs [[]])

main = do
    a <- readLn :: IO [Int]
    print $ compac a