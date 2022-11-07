n = a `div` length xs
    where
       a = 10
       xs = [1,2,3,4,5]

last' xs = xs !! ( length xs - 1 )

last'' xs = head ( reverse xs )

last''' [x] = x
last''' (x:xs) = last''' xs

last'''' xs = drop (length xs -1 ) xs !! 0

first = head

last''''' = head.reverse 

init' xs = reverse(tail(reverse xs))

init'' = reverse.tail.reverse

init''' xs = [ xs !! i | i<-[0.. length xs -2] ]

init'''' [x] = []
init'''' (x:xs) = x : init'''' xs