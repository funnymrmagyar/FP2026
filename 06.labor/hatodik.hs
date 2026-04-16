import Data.Char (isAlpha, isDigit)
import Data.List (intercalate, sort)

vanNulla :: Int -> Bool
vanNulla x = '0' `elem` show x

szuro :: [Int] -> [Int]
szuro xs = filter (not . vanNulla) xs

kiir :: [Int] -> IO ()
kiir xs
  | null eredmeny = putStrLn "Nincsenek olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet."
  | otherwise     = putStrLn $ "A 0 szamjegyet nem tartalmazo szamok a kovetkezok: "
                              ++ intercalate " " (map show eredmeny)
  where
    eredmeny = szuro xs

nincszam :: String -> Bool
nincszam = not . any isDigit

nincszam2 :: String -> Bool
nincszam2 = all isAlpha

fel3 :: IO ()
fel3 = do
    let ls = ["2023tuple", "function", "float", "higher-order", "variable10",
              "may13be", "0recursion", "monad", "class"]
        ls1 = filter nincszam ls
    if null ls1
        then putStrLn "Nincsenek olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet."
        else do
            putStrLn "a karakterlancban nincsenek szamjegyek: "
            mapM_ putStrLn (sort ls1)

ketoldaliszomszedok :: (Eq a, Ord a) => a -> [a] -> [a]
ketoldaliszomszedok s lsS =
  aux (sort lsS)
  where
    aux (x:y:z:ve)
      | x == s    = [y]
      | y == s    = [x,z]
      | otherwise = aux (z:ve)
    aux _ = []

fel4 :: IO ()
fel4 = do
    let s = "feri"
        lsS = ["feri", "gabor", "feri", "laci", "feri", "peter", "feri"]
        eredmeny = ketoldaliszomszedok s lsS
    print eredmeny
    putStrLn $ "A(z) " ++ s ++ " szomszedai: " ++ intercalate ", " eredmeny

maxEladas :: [(String, Int, Int)] -> Int
maxEladas xs = maximum [eladas | (_, eladas, _) <- xs]

maxTelefonok :: [(String, Int, Int)] -> [String]
maxTelefonok xs = [nev | (nev, eladas, _) <- xs, eladas == maxE]
  where
    maxE = maxEladas xs

kiirTelefonok :: [(String, Int, Int)] -> IO ()
kiirTelefonok xs = do
  let maxE = maxEladas xs
      telefonok = sort (maxTelefonok xs)
  putStrLn $ "A maximalis eladasi ertek " ++ show maxE ++ ". A telefonok, amelyeknek ennyi az eladasi erteke a kovetkezok:"
  mapM_ (\t -> putStrLn ("- " ++ t)) telefonok

main :: IO ()
main = do
    kiirTelefonok [("iphoneS1", 20, 2500), ("huaweiS1", 30, 1700), ("huaweiS2", 25, 3100),
                   ("samsungA1", 30, 2000), ("nokia", 10, 1900), ("iphoneS2", 10, 2200),
                   ("samsungA2", 15, 1650), ("iphone3", 30, 1800)]