
busca' x xs i | i == (length xs) = -1 
busca' x xs i  = do
    if(x == (xs!!i)) then
        i
    else
        busca' x xs (i+1)

buscaPos x xs = busca' x xs 0