isSorted [x] = True

isSorted xs = do
    if(xs!!0 <= xs!!1) then
        isSorted (tail xs)
    else
        False 