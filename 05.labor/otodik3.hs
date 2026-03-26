ps ls=sum[t2 | (t1,t2,t3)<-ls]
ps2 ls r=sum[t2 | (t1,t2,t3)<-ls, t3==r]
main = do
    let ls=[("golya",120,"ms"),("fecske",85,"cj"),("cinege",132,"ms")]
    let result=ps ls
    let madarls=concatMap (<> " ") [t1|(t1,t2,t3)<-ls]
    putStrLn $ madarls <> "pop szama: " <> show result
    let result2=ps2 ls "ms"
    putStrLn $ "madarls " <> "ms" <> " pop szama: " <> show result2