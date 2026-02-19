import GHC.Exts.Heap (ClosureType(BCO))
-- - két szám összegét, különbségét, szorzatát, hányadosát, osztási maradékát

osszeg:: Int->Int->Int
osszeg a b=a+b

kulonbseg::Num a=>a->a->a
kulonbseg a b = a-b

szorzas::Int->Int->Int
szorzas a b=a*b

hanyados::Fractional a=>a->a->a
hanyados a b=a/b

osztmar::Int->Int->Int
osztmar a b=a `mod` b

-- - egy első fokú egyenlet gyökét
-- a*x+b=0->a,b->x=(-b)/a

elsoF a b=(-b)/a

-- - egy szám abszulút értékét(abs)
abszolut a = if a < 0 then -a else a
--abszolut a
-- |a<0=-a
-- |otherwise=a

-- - egy szám előjelét(signum)
elojel n|n<0="negativ"
    |n>0="pozitiv"
    |otherwise="egal cu zero"


-- - két argumentuma közül a maximumot,

nagyobb a b=if a>b then a else b

-- - két argumentuma közül a minimumot,
kisebb a b= if a<b then a else b
-- - egy másodfokú egyenlet gyökeit,


-- - hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől.
--   Például: $$(6, 7)$$ egyenlő $$(7,6)$$-al, de $$(6, 7)$$ nem egyenlő $$(4, 7)$$-el.
-- - az n szám faktoriálisát (3 módszer),
-- - az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer).
