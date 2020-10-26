perfeito' [] x = False
perfeito' xi x = 
    if(sum xi == x) then
        True
    else
        perfeito' (init xi) x
perfeito x = perfeito' [i|i<-[0..x],mod i 2 ==1 && i < x] x

main = do
    a <- readLn :: IO Int
    print $ perfeito a
