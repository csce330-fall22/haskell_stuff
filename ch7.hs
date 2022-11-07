--Q1 What are higher-order functions that return functions as 
--   results better known as?

--curried

filtmap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filtmap p f xs = map f (filter p xs)

-- --with function composition
-- filtmap' :: (a -> Bool) -> (a -> b) -> [a] -> [b] 
filtmap' p f = (map f).(filter p)

-- -- --with foldr
filtmap'' p f xs = foldr (\x ys-> if p x then f x:ys else ys) [] xs

--with foldr
map' :: (a->b) -> [a] -> [b]
map' f xs = foldr (\x ys->f x : ys) [] xs 



-- -- --with a l.c.
map'' :: (a->b) -> [a] -> [b] 
map'' f xs = [f x | x<-xs]


-- -- -- --with foldl
map''' :: (a->b) -> [a] -> [b] 
map''' f zs = foldl (\xs y-> xs ++ [f y ]) []  zs


-- -- --foldr with conditional'
filter' :: (a->Bool) -> [a] -> [a]
filter' p xs = foldr (\x ys-> if p x then x:ys else ys) [] xs

-- -- --with a l.c.
filter'' :: (a->Bool) -> [a] -> [a]
filter'' p xs = [ x | x<-xs, p x]
