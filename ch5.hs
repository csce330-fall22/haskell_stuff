factors :: Int -> [Int]
factors n =    [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]


positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']


pyth x y z = x^2 + y^2 == z^2

pyths n = [ (x,y,z) | x<-[1..n], y<-[1..n],z<-[1..n], pyth x y z ]
pyths' n = [ (x,y,z) | x<-[1..n], y<-[x..n],z<-[1..n], pyth x y z ]
pyths'' n = [ (x,y,z) | x<-[1..n], y<-[x..n],z<-[x+1..n], pyth x y z ]

perfects n = [ x | x<-[1..n], sum (init (factors x)) == x]

-- perfect :: Int -> Bool
-- perfect x = sum (init (factors x)) == x

-- perfects n = [ x | x<-[1..n], perfect x]

-- perfects n = [ x | x<-[1..n], perfect x]
--     where 
--         perfect x = sum (init (factors x)) == x

sp xs ys = sum [ fst xy * snd xy | xy<- zip xs ys] 
sp' xs ys = sum [ x * y | (x,y)<- zip xs ys]

sp'' [] [] = 0
sp'' (x:xs) (y:ys) = x*y + sp'' xs ys

sp''' xs ys = sum $ map (\(x,y)->x*y) (zip xs ys)

sp'''' xs ys = foldr ( \(x,y) rest -> x*y+rest )  0 (zip xs ys)
