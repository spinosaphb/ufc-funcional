vec' 0 xz = xz
vec' n xz = do
    vec' (div n 10)  ( xz ++ [ n `mod` 10] ) 
vec n = vec' n []

isPalind' 0 [] = True
isPalind' n xs = do
    if (mod n 10 == last xs) then
        isPalind' (n `div` 10) (init xs)
    else
        False 
isPalind n = isPalind' n (vec n)

primo n | n == 1 = False
 | otherwise = verifPrimo n == n
verifPrimo n = verifPrimoIni n 2
verifPrimoIni n k | divide n k = k
 | k^2 > n = n
 | otherwise = verifPrimoIni n (k+1)
divide n d = mod n d == 0


primPalind = [i | i <- [1..], isPalind i && primo i]