merge' [] [] xv = xv
merge' [] b xv = merge' [] (tail b) xv++[head b]
merge' a [] xv = merge' (tail a) [] xv++[head a] 
merge' a b xv = 
    if(a!!0>b!!0) then
        merge' a (tail b) xv++[head b]
    else
        merge' (tail a) b xv++[head a]
merge xa xb = reverse (merge' xa xb []) 



main = do
    a <- readLn :: IO [Int]
    b <- readLn :: IO [Int]
    print $ merge a b