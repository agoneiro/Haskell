-- Typ funkcji generującej wartość t(n)
t :: Integer -> Integer
t n = 2 * n * n - 1

-- Typ funkcji sprawdzającej, czy liczba jest pierwsza
isPrime :: Integer -> Bool
isPrime k
  | k < 2     = False
  | otherwise = not (hasDivisors k 2)
  where
    -- Funkcja pomocnicza sprawdzająca dzielniki liczby
    hasDivisors :: Integer -> Integer -> Bool
    hasDivisors n d
      | d * d > n      = False
      | n `mod` d == 0 = True
      | otherwise      = hasDivisors n (d + 1)

-- Typ funkcji generującej liczby t(n) dla n < x
generateTValuesForN :: Integer -> [Integer]
generateTValuesForN x = [t n | n <- [2..(x-1)]]

-- Typ funkcji obliczającej liczbę t(n) będących liczbami pierwszymi dla n < x
countPrimeTValuesForN :: Integer -> Int
countPrimeTValuesForN x = length [tValue | tValue <- generateTValuesForN x, isPrime tValue]

-- Przykład użycia: policz ile liczb t(n) jest pierwszych dla n < x
main :: IO ()
main = do
  let x = 9
  print (countPrimeTValuesForN x)
