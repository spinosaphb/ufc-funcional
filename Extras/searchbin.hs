searchBin' xs x e d | xs !! meio == x = meio 
                    | e >= d = -1
                    where meio = div (e + d) 2 
searchBin' xs x e d = do
    if xs!!meio < x then
        searchBin' xs x (meio+1) d
    else
        searchBin' xs x e (meio-1)  
        
    where meio = div (e + d) 2 

searchBin xs x = searchBin' xs x 0 (length xs - 1) 