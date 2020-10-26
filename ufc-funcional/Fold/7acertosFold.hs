acertosFold' [] [] i = i
acertosFold' xs ys i = do
    if (head xs) == (head ys) then
        acertosFold' (tail xs) (tail ys) (i+1)
    else
        acertosFold' (tail xs) (tail ys) i
acertosFold xs ys = acertosFold' xs ys 0 