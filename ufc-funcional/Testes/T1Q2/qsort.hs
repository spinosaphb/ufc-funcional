
qsort [] = []
qsort (pivo:xs) = qsort mepiv ++ mid ++ qsort mapiv
    where
        mepiv = [i | i<-xs, i<pivo]
        mid   = [i | i<-xs, i==pivo] ++ [pivo]
        mapiv = [i | i<-xs, i>pivo]
    

main = do
    a <- readLn :: IO [Int]
    print $ qsort a