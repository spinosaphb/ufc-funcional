

pertence [] n = False
pertence xs n = 
    if(xs!!((length xs)-1)==n) then
        True
    else
        pertence (init xs) n
unique' [] xn = xn
unique' xs xn = unique' (tail xs) (xn++[i|i<-[head xs],not(pertence xn i)])
unique xs = unique' xs []
main = do
    a <- readLn :: IO [Int]
    print $ unique a