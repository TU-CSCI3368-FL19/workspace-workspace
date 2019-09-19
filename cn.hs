type CN a = (a -> a) -> a -> a

c0 = \f x -> x
c1 = \f x -> f x
c2 = \f x -> f (f x)

inc x = x + 1

fromCN :: CN Int -> Int
fromCN cn = cn inc 0


toCN :: Int -> CN a
toCN 0 = \f x -> x
toCN n = 
      let CNm1 = toCN (n-1)
      in \f x -> f (CNm1 f x)
toCN2 0 f x = x
toCN2 n f x = f (toCN2 (n-1) f x)


showCN cn = "C"++(show $ fromCN cn) 
