x :: Integer
x = 99

f :: Integer -> Integer
f r = r + 10

ff :: Integer -> Integer -> Integer
ff a b = (a + b) * 2

fff :: Integer -> Integer -> Integer
fff = \a b -> (a + b) * 2

ffff = \a -> \b -> (a + b) * 2
fffff a = \b -> (a + b) * 2

(.+.) :: Integer -> Integer -> Integer
(.+.) a b = a + (b * 2)

data Shape =
  Circle Integer 
  | Rectangle Integer Integer
  | Triangle Integer Integer Integer
  deriving (Eq, Show)

pie :: Integer
pie = 3

perimeter :: Shape -> Integer
perimeter = \s -> case s of 
                    Circle r -> r * 2 * pie
                    Rectangle w h -> (w + h) * 2
                    Triangle a b c -> a + b + c

perimeter2 :: Shape -> Integer 
perimeter2 (Circle r) = r * 2 * pie
perimeter2 (Rectangle w h) = (w + h) * 2
perimeter2 (Triangle a b c) = a + b + c

class Equal a where
  (===) :: a -> a -> Bool

instance Equal Shape where
  (===) (Circle r1) (Circle r2) = r1 == r2
  (===) (Rectangle w1 h1) (Rectangle w2 h2) = (w1 == w2) && (h1 == h2)
  (===) (Triangle a1 b1 c1) (Triangle a2 b2 c2) = (a1 == a2) && (b1 == b2) && (c1 == c2) 
  (===) _ _ = False