import System.Win32.Info.Computer (OSVERSIONINFOEX(szCSDVersion))
import Control.Monad.Trans.Cont (reset)
-- # 2. labor

-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza:

-- - egy szám számjegyeinek szorzatát (2 módszerrel),
szamJegySzorzat 0=1
szamJegySzorzat n =mod n 10*szamJegySzorzat(div n 10)

-- szjSzorzat n

--     n<0=szjSzorzat (abs n)

-- - egy szám számjegyeinek összegét (2 módszerrel),
szjOsszeg n
    |n<0=szjOsszeg (abs n)
    |div n 10==0=mod n 10
    |otherwise=mod n 10+szjOsszeg (div n 10)


szjOsszeg2 n res
    | n < 0     = szjOsszeg2 (abs n) res
    | n < 10    = res + n
    | otherwise = szjOsszeg2 (div n 10) (res + mod n 10)
-- - egy szám számjegyeinek számát (2 módszerrel),
szjSzam n
    | n<0 = szjSzam (abs n)
    | div n 10==0 = 1
    | otherwise = 1 + szjSzam (div n 10)

szjSzam2 n = szjSzam2Acc n 0
  where
    szjSzam2Acc n res
        | n < 0     = szjSzam2Acc (abs n) res
        | n < 10    = res + 1
        | otherwise = szjSzam2Acc (div n 10) (res + 1)
-- - egy szám azon számjegyeinek összegét, mely paraméterként van megadva, pl. legyen a függvény neve fugv4, ekkor a következő meghívásra, a következő eredményt kell kapjuk:
--   ```haskell
--   > fugv4 577723707 7
--   35
--   ```
fugv4 n szj 
    | szj > 9 = error "Nem számjegy"
    | div n 10 == 0 = if mod n 10 == szj then szj else 0
    | otherwise = if utolsoSzj == szj 
                  then szj + fugv4 (div n 10) szj 
                  else fugv4 (div n 10) szj
    where
        utolsoSzj = mod n 10
-- - egy szám páros számjegyeinek számát,
parosSzj n
    |n<0=parosSzj(abs n)
    |n<10=if even n then 1 else 0
    |otherwise=if even (mod n 10)then 1+parosSzj(div n 10) else parosSzj(div n 10)
-- - egy szám legnagyobb számjegyét,
legnagyobbSzj n lg
    |n<0=legnagyobbSzj (abs n) lg
    |n<10=max lg n
    |otherwise=if mod n 10 >lg then legnagyobbSzj(div n 10) (mod n 10) else legnagyobbSzj(div n 10) lg
-- - egy szám $b$ számrendszerbeli alakjában a $d$-vel egyenlő számjegyek számát (például a $b = 10$-es számrendszerben a $d = 2$-es számjegyek száma),
--   Példák függvényhívásokra:
bSzamSzj n b d
    | n < 0 = error "neg szam"
    |n < b=if n==d then 1 else 0
    |otherwise=if mod n b==d then 1+bSzamSzj(div n b)b d else bSzamSzj(div n b) b d


--   ```haskell
--   fugv 7673573 10 7 -> 3
--   fugv 1024 2 1 -> 1
--   fugv 1023 2 1 -> 10
--   fugv 345281 16 4 -> 2
--   ```
-- - az 1000-ik Fibonacci számot.
fiboN n=fibo 0 1 0 n
    where
        fibo _ _ res 0=res
        fibo a b res n1=fibo b res(b+res) (n1-1)
-- II. Alkalmazzuk a map függvényt a I.-nél megírt függvényekre.

szjOsszegLs ls=map szjOsszeg ls


-- **Megoldott feladatok:**

-- - Határozzuk meg egy szám számjegyeinek összegét:
--   I. módszer:

--   ```haskell
--   szOsszeg :: Int -> Int
--   szOsszeg 0 = 0
--   szOsszeg x = ( x `mod` 10 ) + szOsszeg (x `div` 10)

--   > szOsszeg 123
--   ```

--   II. módszer:

--   ```haskell
--   szOsszeg1 :: Int -> Int -> Int
--   szOsszeg1 0 t = t
--   szOsszeg1 x t = szOsszeg1 (x `div` 10) ( t + x `mod` 10 )

--   > szOsszeg1 123 0
--   ```
