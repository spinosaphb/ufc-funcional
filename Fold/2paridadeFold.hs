parFold' i [] = do
    if (mod i 2) == 0 then
        True
    else
        False
parFold' i xs = do
    if (head xs) == True then
        parFold' (i+1) (tail xs)
    else
        parFold' i (tail xs)
parFold xs = parFold' 0 xs
       