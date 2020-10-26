--10. cria uma sublista      
sublist n n' xs = do
    if(n'>0) then
        [i|i<-drop n (take n' xs)]
    else
        drop n $ reverse[i|i<-drop (abs n') (reverse xs)]