



ms :: Ord a => [a] -> [a]
ms [] = []
ms (a:[]) = [a]
ms as = 
 let h = (length as) `div` 2
     a = ms $ take h as
     b = ms $ drop h as
 in combine a b []
 where combine [] b r = r ++ b
       combine a [] r = r ++ a
       combine (a:as) (b:bs) r =
        if a < b then combine as (b:bs) (r ++ [a])
        else combine (a:as) bs (r ++ [b])
