import Data.Char  

-- Szöveg tisztítása és szavakra bontása
tokenize :: [Char] -> [String]
tokenize = words . map (irasjelHelyettesit . toLower)

irasjelHelyettesit :: Char -> Char
irasjelHelyettesit c
    | notElem c ",.;:!?\"'()[]<>" = c
    | otherwise = ' '

lengthLista ls=map length ls

talalat x ls = l1
        where 
            zipls=zip ls [0..]
            l1=map snd $ filter(\y->fst y==x) zipls

main = do
    let a=5
    let l1= [3,13,5,6,7,12,5,8,5]
    let t1 = talalat a l1
    let a2='e'
    let l2="Bigeri-vizeses"
    let t2 = talalat a2 l2
    let c1=concatMap ((<> " ") . show) t1
    putStrLn $ show a <> "talalat pozicioi: " <> c1
    let c2=concatMap ((<> " ") . show) t2
    putStrLn $ show a2 <> "talalat pozicioi: " <> c2

    let c3=concatMap ((<> " ") . show) t1
    print c3