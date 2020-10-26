foldrs (x,_) = x
foldrs' (_,x) = x 
descompactaFold xs = [[foldrs i | i <- xs]] ++ [[foldrs' i | i <- xs]]