--0000 comparar se duas listas sao iguais
igual xs [] = False
igual [] xs = False
igual [] [] = True
igual xs xz = do
    if(xs!!0 == xz!!0) then
        igual (tail xs) (tail xz)
    else
        False
--0000 retorna -1 se o primeiro for menor , 1 se o primeiro for maior, 0 caso igual
strequal [] [] = 0
strequal xs xz = do
    if(xs!!0 == xz!!0) then
        strequal (tail xs) (tail xz)
    else
        if(xs!!0 < xz!!0) then
            (-1)
        else
            1
