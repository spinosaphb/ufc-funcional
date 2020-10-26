inverteFold' xv [] = xv
inverteFold' xv xs = inverteFold' (xv++[last xs]) (init xs)

inverteFold xs = inverteFold' [] xs
