position' x xs i =
    if(x==head xs) then
        i
    else
        position' x (tail xs) (i+1) 
position x xs = position' x xs 0

upper [x] = if(x `elem` ['a'..'z']) then [['A'..'Z']!!(position x ['a'..'z'])] else [x]

lower [x] = if(x `elem` ['A'..'Z']) then [['a'..'z']!!(position x ['A'..'Z'])] else [x]

titulo' xs = 
    if(head xs == ' ') then
        [' ']++(upper [(xs!!1)])
    else
        (lower [(head xs)]) ++ (lower [(xs!!1)])
titulo' xs = [head xs]++titulo' (tail xs)
        
titulo xs = titulo' ([' ']++xs) 