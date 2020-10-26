collatz' xs | last xs == 1 = xs

collatz' xs = do
    if ( n `mod` 2 == 0 ) then
        collatz' ( xs ++ [ ( n `div` 2 ) ] )
    else
        collatz' ( xs ++ [ ( n * 3 + 1 ) ] )
    where n = (last xs)
collatz n = collatz' [n]
