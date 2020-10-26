
removeLista [] ys = ys
removeLista xs ys = removeLista (tail xs) ( [i | i <- ys, (head xs) /= i ] ); 