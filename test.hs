doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x*2

doubleSmallNumber' x = (if x > 100 then x else doubleMe x) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

luckyNum :: (Integral a) => a -> String
luckyNum 7 = "Lucky number 7!"
luckyNum x = "No luck..."

factorial :: (Integral a) => a -> a
-- Order of pattern definition is important (recursion would not terminate if statements were reversed)
factorial 0 = 1
factorial n = n * factorial (n - 1)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z 

head' :: [a] -> a
head' [] = error "Cannot get head of empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "Empty list"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has 2 elements: " ++ show x ++ ", " ++ show y
tell (x:y:_) = "This list is too long. First 2 elements: " ++ show x ++ ", " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x] 

bmiTell :: (RealFloat a) => a ->  a -> String
bmiTell weight height
    | bmi ^ 2 <= underweight = "You are underweight"
    | bmi ^ 2 <= normal = "You are normal"
    | bmi ^ 2 <= overweight = "You are overweight"
    | otherwise   = "You are obese"
    where bmi = weight / height ^ 2
          (underweight, normal, overweight) = (18.5, 25.0, 30.0)

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

initials' :: String -> String -> String
initials' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs ]
    where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea * topArea * 2

-- multInlineLet = (let a = 100; b = 200; c = 300 in a*b*c, let foo = "Hey "; bar = "there!" in foo ++ bar)

-- Only return the BMIs of overweight people or bigger
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

