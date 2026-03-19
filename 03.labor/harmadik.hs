-- 1. Lista elemszáma (2 módszer)
-- 1. módszer: Egyszerű rekurzió
myLength1 :: [a] -> Int
myLength1 [] = 0
myLength1 (_:xs) = 1 + myLength1 xs

-- 2. módszer: Farokrekurzió akkumulátorral
myLength2 :: [a] -> Int
myLength2 xs = helper xs 0
  where
    helper [] n = n
    helper (_:ts) n = helper ts (n + 1)

-- 2. Lista elemeinek szorzata (2 módszer)
-- 1. módszer: Egyszerű rekurzió
myProduct1 :: Num a => [a] -> a
myProduct1 [] = 1
myProduct1 (x:xs) = x * myProduct1 xs

-- 2. módszer: Farokrekurzió akkumulátorral
myProduct2 :: Num a => [a] -> a
myProduct2 xs = helper xs 1
  where
    helper [] acc = acc
    helper (t:ts) acc = helper ts (acc * t)

-- 3. Lista legkisebb eleme
myMinimum :: Ord a => [a] -> a
myMinimum [x] = x
myMinimum (x:xs) = 
    let m = myMinimum xs 
    in if x < m then x else m

-- 4. Lista legnagyobb eleme
myMaximum :: Ord a => [a] -> a
myMaximum [x] = x
myMaximum (x:xs) = 
    let m = myMaximum xs 
    in if x > m then x else m

-- 5. N-edik elem (!!) (0-tól indexelve)
myNth :: [a] -> Int -> a
myNth (x:_) 0 = x
myNth (_:xs) n = myNth xs (n - 1)

-- 6. Két lista összefűzése (++)
myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (x:xs) ys = x : myAppend xs ys

-- Segédfüggvény: Lista megfordítása (a palindromhoz kell)
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myAppend (myReverse xs) [x]

-- 7. Palindrom-e a lista
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs

-- 8. Egész szám számjegyeinek listája
digits :: Int -> [Int]
digits n
    | n < 10    = [n]
    | otherwise = myAppend (digits (n `div` 10)) [n `mod` 10]

-- 9. Az első elem a lista végére költözik
moveToEnd :: [a] -> [a]
moveToEnd [] = []
moveToEnd (x:xs) = myAppend xs [x]

-- Segédfüggvény: Lista összege
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

-- 10. Egész elemű lista átlaga
myAverage :: [Int] -> Double
myAverage [] = 0
myAverage xs = fromIntegral (mySum xs) / fromIntegral (myLength1 xs)

-- 11. 10-es számrendszerbeli szám p számrendszerbeli alakja (lista formájában)
toBaseP :: Int -> Int -> [Int]
toBaseP 0 _ = [0]
toBaseP n p
    | n < p     = [n]
    | otherwise = myAppend (toBaseP (n `div` p) p) [n `mod` p]

-- 12. P számrendszerbeli alak (lista) 10-es számrendszerbe (Horner-módszer)
fromBaseP :: [Int] -> Int -> Int
fromBaseP digits p = helper digits 0
  where
    helper [] acc = acc
    helper (d:ds) acc = helper ds (acc * p + d)
--V. Ha adva van egy P pont koordinátája a kétdimenziós síkban, és adott az lsP pontok egy listája, írjunk egy Haskell függvényt, amely meghatározza azt az lsP-beli P1 pontot, amely legközelebb van a P ponthoz.
type Pont= (Double, Double)

lsP:: [Pont]
lsP=[(4.5,2.7),(6,7),(2.1,7.9)]

p::Pont
p=(5.7,2.3)
tavolsag(x1,y1)(x2,y2)=sqrt((x1-x2)^2+(y1-y2)^2)

minTavolsag lsP p = foldl1 aux lsP
    where aux p1 p2=if tavolsag p1 p<tavolsag p2 p then p1 else p2

minTavolsag2 (p1:p2:lsP)p
    |tavolsag p1 p < tavolsag p2 p = minTavolsag2 (p1:lsP) p
    |otherwise = minTavolsag2 (p2:lsP) p