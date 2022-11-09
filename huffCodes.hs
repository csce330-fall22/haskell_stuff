import Data.List

data HTree =
    CNode Double Char |
    HNode Double HTree HTree deriving Show

type HuffTable = [(Char,String)]

freq :: HTree -> Double
freq (CNode f _)   = f
freq (HNode f _ _) = f

merge :: HTree -> HTree -> HTree
merge l r = HNode (freq l + freq r) l r

cfs2htrees :: [(Char,Double)] -> [HTree]
cfs2htrees cfs = [ CNode f c | (c,f)<-cfs ]

cfs2htrees' cfs = map (\(c,f)->CNode f c) cfs

mergeAllTrees' [t] = t
mergeAllTrees' ts = mergeAllTrees' (t:rest)
    where 
        (f:s:rest) = sortOn freq ts
        t = merge f s

mergeAllTrees [t] = t
mergeAllTrees ts = mergeAllTrees (t:rest)
    where
        (t1, ts') = removeMin ts
        (t2, rest) = removeMin ts'
        t = merge t1 t2

removeMin :: [HTree] -> (HTree,[HTree])
removeMin ts = (minT, rest)
    where
        minIndex = mini ts
        minT = ts !! minIndex
        rest = removei ts minIndex


mini :: [HTree] -> Int
mini ts =  head [ i | (t,i)<-zip ts [0..], freq t == fm ]
    where
        fm = freq ( min' ts )

min' :: [HTree] -> HTree
min' [t] = t
min' (t:ts) = if freq t < freq minTs then t else minTs
    where
        minTs = min' ts

removei :: [HTree] -> Int -> [HTree]
removei (_:ts) 0 = ts
removei (t:ts) i = t: removei ts (i-1)
--zip approach fine

decodeTree :: String -> HTree -> [(Char,String)]
decodeTree code (CNode _ c) = [(c,code)]
decodeTree code (HNode _ l r) = lCodes ++ rCodes
    where 
        lCodes = decodeTree (code++"0") l
        rCodes = decodeTree (code++"1") r

getCodes :: [(Char,Double)] -> [(Char,String)]
getCodes cfs = decodeTree ""  ((mergeAllTrees (cfs2htrees cfs)) )



encodeText :: HuffTable -> String -> String
encodeText tbl text = concat (map (encodeChar tbl) text)

encodeChar :: HuffTable -> Char -> String
encodeChar codes c = snd ( head (filter (\(ch,s)->ch==c) codes ) )

table = getCodes get350Slide38

get350Slide38 = [('A',0.35),('B',0.1),('C',0.2),('D',0.2),('_',0.15)]