
   
sublist n n' xs | n' < 0 && n >= 0 = drop n (take ((length xs) + n') xs)  
sublist n n' xs | n < 0 && n' < 0 = sublist 0 n' (drop ((length xs) + n) xs)
sublist n n' xs = drop n (take n' xs)

main = do
    begin <- readLn :: IO Int
    end   <- readLn :: IO Int
    line  <- getLine
    print $ sublist begin end [read x :: Int | x <- words line]