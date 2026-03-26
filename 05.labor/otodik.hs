import Data.Char  

-- Szöveg tisztítása és szavakra bontása
tokenize :: [Char] -> [String]
tokenize = words . map (irasjelHelyettesit . toLower)

-- Írásjelek cseréje szóközre
irasjelHelyettesit :: Char -> Char
irasjelHelyettesit c
    | notElem c ",.;:!?\"'()[]<>" = c
    | otherwise = ' '

-- Megadja a lista elemeinek hosszát
lengthLista :: [String] -> [Int]
lengthLista ls = map length ls

-- Megkeresi a minimum értéket és annak összes indexét
myMinimum2 :: (Ord a, Enum b, Num b) => [a] -> (a, [b])
myMinimum2 ls = (m, map snd $ filter (\k -> fst k == m) $ zip ls [0..])
    where m = minimum ls

-- Megkeresi a maximum értéket és annak összes indexét
myMaximum2 :: (Ord a, Enum b, Num b) => [a] -> (a, [b])
myMaximum2 ls = (m, map snd $ filter (\k -> fst k == m) $ zip ls [0..])
    where m = maximum ls

main :: IO ()
main = do
    let lista = "ez egy PrOBA szoveg. ez egy masik proBa! Tobbfele irasJEL ::Hasznalat i"
    let l1 = tokenize lista
    let l2 = lengthLista l1
    let l3 = zip l1 l2  -- Előbb hozzuk létre a párosított listát
    
    putStrLn "--- Alapadatok ---"
    putStr "Szavak: " >> print l1
    putStr "Hosszak: " >> print l2
    
    -- Minimum keresése
    let (minHossz, minIndexek) = myMinimum2 l2
    putStrLn $ "\nLegrovidebb hossz: " ++ show minHossz
    putStr "Indexei: " >> print minIndexek
    
    -- A legrovidebb szavak kigyűjtése listagenerátorral
    let legrövidebbSzavak = [ l1 !! i | i <- minIndexek ]
    putStr "Konkret szavak: " >> print legrövidebbSzavak

    -- Maximum keresése
    let (maxHossz, maxIndexek) = myMaximum2 l2
    putStrLn $ "\nLeghosszabb hossz: " ++ show maxHossz
    putStr "Indexei: " >> print maxIndexek
    
    -- A leghosszabb szavak kigyűjtése
    let leghosszabbSzavak = [ l1 !! i | i <- maxIndexek ]
    putStr "Konkret szavak: " >> print leghosszabbSzavak

    putStrLn "\nSzavak a hosszukkal parositva (l3):"
    print l3






