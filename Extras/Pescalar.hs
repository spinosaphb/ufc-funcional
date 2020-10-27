
prodescalar' [] valor = valor
prodescalar' xx valor = do
    prodescalar' (tail xx) x
    where
        x = valor + fst(head xx) * snd(head xx) 
prodescalar xs xz = prodescalar' (zip xs xz) 0