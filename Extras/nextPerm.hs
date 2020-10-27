geti' u i | length u - 2 < i = i - 1
geti' u i = if u!!i < u!!(i+1) then geti' u (i+1) else i - 1
geti u = geti' u 0 

getj' u j _ | length u - 1 == j = j
getj' u j i  = if u!!j > u!!i then getj' u (j+1) i else j
getj u = getj' u (i + 1) i where i = (geti u)

swap u i j = [get k x | (k, x) <- zip [0..length u - 1] u]
    where get k x | k == i = u !! j
                  | k == j = u !! i
                  | otherwise = x
reverseList' u i j | i > j = u
reverseList' u i j  = reverseList' (swap u i j) (i+1) (j-1)

reverseList u = reverseList' u 0 j
    where j = (length u) - 1

nextPerm u = do
    take (i+1) z ++ reverseList ( drop (i+1) z )
    where i = geti u
          j = getj u
          z = swap u i j 