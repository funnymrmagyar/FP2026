import  Data.List (sort)
import Data.Char (isDigit,isUpper,ord,digitToInt)
import System.IO (hSetEncoding, stdout, utf8)

getSorted::FilePath -> IO [String]
getSorted filePath = do
    contents <- readFile filePath
    return $ sort (lines contents)

binarySearch :: [String] -> String -> Bool
binarySearch [] _ = False
binarySearch xs target
  | midEl == target = True
  | midEl < target  = binarySearch (drop (mid + 1) xs) target
  | otherwise       = binarySearch (take mid xs) target
  where
    mid   = length xs `div` 2
    midEl = xs !! mid


    -- Karakterek helyettesítése számokkal (A -> 10, B -> 11...)
charToVal :: Char -> String
charToVal c
    | isDigit c = [c]
    | isUpper c = show (ord c - ord 'A' + 10)
    | otherwise = ""

-- Modulo 97 ellenőrzés
checkModulo :: String -> Bool
checkModulo iban =
    let rearranged = drop 4 iban ++ take 4 iban
        substituted = concatMap charToVal rearranged
        numericValue = read substituted :: Integer
    in numericValue `mod` 97 == 1

-- A teljes validációs folyamat
isValid :: [(String, Int)] -> String -> Bool
isValid lengths iban =
    let country = take 2 iban
        expectedLen = lookup country lengths
    in case expectedLen of
        Nothing -> False -- Ismeretlen országkód
        Just len -> 
            length iban == len && 
            all (\c -> isDigit c || isUpper c) iban &&
            checkModulo iban

-- Fő vezérlőfüggvény a feldolgozáshoz
processIbans :: IO ()
processIbans = do
    -- Hosszok beolvasása (Pl: "RO 24")
    lengthContent <- readFile "ibanLength.txt"
    let countryLengths = [(w!!0, read (w!!1)) | l <- lines lengthContent, let w = words l]
    
    -- IBAN-ok beolvasása és rendezése
    sortedIbans <- getSorted "iban.txt"
    
    -- Validálás
    let okIbans = filter (isValid countryLengths) sortedIbans
    
    -- Kiírás állományba
    writeFile "okIban.txt" (unlines okIbans)
    putStrLn "A validalt IBAN kodok mentve az okIban.txt fajlba."

main :: IO ()
main = do
    hSetEncoding stdout utf8
    putStrLn "Folyamat inditasa..."
    
    -- 1. Országhosszok beolvasása és tisztítása
    lengthContent <- readFile "09.labor/ibanLength.txt"
    let countryLengths = [ (w !! 0, read (w !! 1) :: Int) 
                         | l <- lines lengthContent
                         , let w = words l
                         , length w >= 2 ] -- Csak a valid sorokat vesszük figyelembe
    
    -- 2. IBAN kódok beolvasása és rendezése ábécé sorrendbe [cite: 27, 98]
    putStrLn "IBAN kodok beolvasasa..."
    -- Megjegyzés: Ha az iban.txt még nincs kész, hozz létre egyet üresen vagy teszt adatokkal!
    sortedIbans <- getSorted "09.labor/iban.txt"
    
  
    putStrLn "Validalas es szures folyamatban..."
    let okIbans = filter (isValid countryLengths) sortedIbans
    
   
    writeFile "okIban.txt" (unlines okIbans)
    
    putStrLn $ "Siker! Az ervenyes k0dok szama: " ++ show (length okIbans)
    putStrLn "Az eredmenyeket az okIban.txt fajlban talalod."