;; # 1. labor

;; I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza



;; - két argumentuma közül a maximumot,
;; - két argumentuma közül a minimumot,
;; - egy másodfokú egyenlet gyökeit,
;; - hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől.
;;   Például: $$(6, 7)$$ egyenlő $$(7,6)$$-al, de $$(6, 7)$$ nem egyenlő $$(4, 7)$$-el.
;; - az n szám faktoriálisát (3 módszer),
;; - az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer).

;; II. Könyvtárfüggvények használata nélkül, illetve halmazkifejezéseket alkalmazva, definiáljuk azt a függvényt, amely meghatározza:

;; - az első n természetes szám negyzetgyökét,
;; - az első n négyzetszámot,
;; - az első n természetes szám köbét,
;; - az első n olyan természetes számot, amelyben nem szerepelnek a négyzetszámok,
;; - x hatványait adott n-ig,
;; - egy szám páros osztóinak listáját,
;; - n-ig a prímszámok listáját,
;; - n-ig az összetett számok listáját,
;; - n-ig a páratlan összetett számok listáját,
;; - az n-nél kisebb Pitágorászi számhármasokat,
;; - a következő listát: $$[(\texttt{a},0), (\texttt{b},1),\ldots, (\texttt{z}, 25)]$$,
;; - a következő listát: $$[(0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)]$$, majd általánosítsuk a feladatot.
;; - azt a listát, ami felváltva tartalmaz True és False értékeket.

;; **Megoldott feladatok:**

;; - Határozzuk meg egy szám osztóinak listáját:

;;   ```haskell
;;   osztok :: Int -> [ Int ]
;;   osztok n = [ i | i <- [1..n] , n `mod` i ==0]

;;   > osztok 100
;;   ```

;; - Határozzuk meg a következő listát: $$[(\texttt{a},0), (\texttt{b},1), \ldots, (\texttt{z}, 25)]$$:

;;   ```haskell
;;   import Data.Char
;;   lista = [(chr(i + 97), i) | i<-[0..25]]

;;   lista_ = zip ['a'..'z'] [1..26]
;;   ```
