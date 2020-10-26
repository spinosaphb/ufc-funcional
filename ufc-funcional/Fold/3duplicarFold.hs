isVogal' _ [] = False
isVogal' x xs = do
    if x == head xs then
        True
    else
        isVogal' x (tail xs)
isVogal x = isVogal' x ['a','A','e','E','i','I','o','O','u','U']
dupFold' xv [] = xv
dupFold' xv xs = do
    if isVogal (head xs) then
        dupFold' ( xv ++ ([head xs] ++ [head xs] ) ) (tail xs)
    else
        dupFold' ( xv++[head xs] ) (tail xs)
dupFold xs = dupFold' [] xs
