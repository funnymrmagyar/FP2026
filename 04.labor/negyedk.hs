parosNegyzet n = take n [i^2|i<-[2,4..n]]

parosNegyzet2 n = [i^2|i<-[2,4..n]]

szamokLs n
    |n /=1= szamokLs(n-1)++replicate n n
    |otherwise=replicate n n

szamokLs2 n i
    |i/= n=replicate i i ++ szamokLs2 n (i+1)
    |otherwise=replicate i i


szamokLs3 n i
    |i/= n=replicate i (i*2) ++ szamokLs3 n (i+1)
    |otherwise=replicate i (i*2)

szamokLs4 n =[n,n-1..1]++[1..n]

szamokLs5 n=reverse [1..n]++[1..n]

tfLs n=take n ls
    where
        ls=[True,False]++ls

szamokLs6 n = take n ls
    where
        ls=[0,1,-1]++ls

osztok n =[i|i<-[1..n],mod n i ==0]

osztokSzama n=length $ osztok n

osztokSzama2 n=myLength $ osztok n 
    where
        myLength[]=0
        myLength(_:ls)=1+myLength ls

osztokSzama3 n=foldl (\res i -> if mod n i == 0 then res +1 else res) 0 [1..n]

maxParatlanOszto n = last $ filter odd $ osztok n

maxParatlanOszto2 n 
    |odd n=n
    |otherwise= last[i|i <-[1,3 ..n],mod n i==0]

decP x p
    |x < p= [x]
    |otherwise=decP(div x p)p ++[mod x p]

decPSzam x p =length $ decP x p

decPMax x p = maximum $ decP x p

fibo=fiboSg 0 1 0
    where
        fiboSg a b res=res:fiboSg b res(res+b)


fiboAB a b=dropWhile(<a) $ takeWhile(<b) fibo

atlag ls= sum ls/fromIntegral(length ls)

pozAtlag ls=atlag[i|i <-ls,i>0]

pozAtlag2 ls=(atlag . filter (>0))ls

listaN ls n=[i | (idx,i) <-zip[1..] ls,mod idx n==0]


listaN2 ls n i
    |mod i n==0=ls!!i:listaN2 ls n (i+1)
    |otherwise=listaN2 ls n (i+1)
