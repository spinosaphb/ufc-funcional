position' x xs i =
    if(x==head xs) then
        i
    else
        position' x (tail xs) (i+1) 
position x xs = position' x xs 0
upper [x] =
    if(x `elem` ['a'..'z']) then
        [['A'..'Z']!!(position x ['a'..'z'])]
    else
        [x]
upper xs = upper [head xs]++(upper (tail xs))     