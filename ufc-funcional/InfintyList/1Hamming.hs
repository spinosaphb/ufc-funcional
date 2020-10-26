unique' [] xv = xv
unique' xs xv = do
    if ( head xs `elem` xv ) then
        unique' (tail xs) xv
    else
        unique' (tail xs) (xv ++ [head xs] )
unique xs = unique' xs []

merge xs ys = unique (xs++ys) 

hamming = 1 : merge (map (2*) hamming)
                                 (merge (map (3*) hamming)
                                              (map (5*) hamming))