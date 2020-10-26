foldrs [x] = x

concatFolder' xv [] = xv
concatFolder' xv xs = concatFolder' (xv ++ foldrs(take 1 xs)) (tail xs)  
concatFolder xs = concatFolder' [] xs