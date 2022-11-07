
null' :: [a] -> Bool
null' [] = True
null' _  = False

safeTail xs = if null xs then [] else tail xs

safeTail' xs 
    | null xs   = []
    | otherwise = tail xs

safeTail'' [] = []
safeTail'' xs = tail xs

(|||) :: Bool -> Bool -> Bool
-- True  ||| True  = True
-- True  ||| False = True
-- False ||| True  = True
-- False ||| False = False

-- False ||| False = False
-- _     ||| _     = True

False ||| b  = b
True  ||| _  = True

(&&&) :: Bool -> Bool -> Bool
-- (&&&) a b = if a then if b then True else False else False
(&&&) a b  = if a then b else False

