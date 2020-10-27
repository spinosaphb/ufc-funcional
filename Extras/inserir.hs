inserir' x xs i | length xs - 1 < i = take i xs ++ [x] ++ drop i xs  
inserir' x xs i = do
    if xs!!i < x then
        inserir' x xs (i+1)
    else
        take i xs ++ [x] ++ drop i xs  
inserir x xs = inserir' x xs 0