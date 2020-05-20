

simple :: Integer -> Integer -> Integer
simple 0 _ = 0
simple _ 0 = 0
simple 1 b = b
simple a b = b + simple (a-1) b

karatsuba :: Integer -> Integer -> Integer
karatsuba a b =
 if a < 10 || b < 10 then simple a b else
 let ra = show a :: String
     rb = show b :: String
     la = length ra `div` 2
     lb = length rb `div` 2
     a1 = read (take la ra) :: Integer
     a2 = read (drop la ra) :: Integer
     b1 = read (take lb rb) :: Integer
     b2 = read (drop lb rb) :: Integer
     a1b1 = karatsuba a1 b1
     a2b2 = karatsuba a2 b2
     abcd = karatsuba (a1 + a2) (b1 + b2) - a1b1 - a2b2
  in 
   10^(2*la) * a1b1 + 10^la * abcd + a2b2
    
main = print $ karatsuba 2718281828459045235360287471352662497757247093699959574966967627 3141592653589793238462643383279502884197169399375105820974944592

