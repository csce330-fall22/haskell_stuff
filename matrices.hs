type Vector = [Double]

type Matrix = [Vector] -- row-major, list of rows

sp :: Vector -> Vector -> Double
sp [] [] = 0
sp (x:xs) (y:ys) = x*y + sp xs ys

sp' xs ys = sum [ x*y | (x,y)<-zip xs ys]

row :: Matrix -> Int -> Vector
row = (!!)

col :: Matrix -> Int -> Vector
col dss i = [ ds !! i | ds <- dss]

n_rows :: Matrix -> Int
n_rows = length

n_cols :: Matrix -> Int 
n_cols m = length (m!!0)

matmul a b = [ [sp r c | c<-cols] | r<-a] 
    where
        cols = [ col b i | i<-[0..n_cols b -1] ]

safe_mm :: Matrix -> Matrix -> Maybe Matrix
safe_mm a b 
    | n_cols a == n_rows b = Just (matmul a b)
    | otherwise            = Nothing

square_matrix a = safe_mm a a

squarable :: Matrix -> Bool
squarable m = helper (safe_mm m m) 

helper :: Maybe Matrix -> Bool
helper Nothing = False
helper (Just m) = True

square_mat :: Matrix -> String
square_mat m = msgOrMatrix m2
    where
        m2 = safe_mm m m
        msgOrMatrix Nothing   = "Input must be square"
        msgOrMatrix (Just n) = show n

m3x3 :: Matrix
m3x3 = [ [1,2,3],[4,5,6],[7,8,9]]

m3x2 :: Matrix
m3x2 = [ [1,2],[4,5],[7,8]]

eye3x3 :: Matrix
eye3x3 = [ [1,0,0],[0,1,0],[0,0,1]]