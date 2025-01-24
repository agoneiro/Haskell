-- funkcja t(n)
t :: Int -> Int
t n = 2*n*n - 1

-- sprawdza czy liczba jest pierwsza
isPrime :: Int -> Bool
isPrime k
  | k <= 1 = False
  | k == 2 = True
  | even k = False
  | otherwise = checkDivisors 3
  where 
    -- sprawdza czy istnieją dzielniki do pierwiastka z k, zwraca False jesli znajdzie dzielnik
    checkDivisors d
      | d*d > k = True
      | k `mod` d == 0 = False
      | otherwise = checkDivisors (d + 2) -- dla optymalizacji pomijamy parzyste

-- oblicza liczbę wyrazów ciągu t(n) będących liczbami pierwszymi dla n < x
countPrimeTValuesForX :: Int -> Int
countPrimeTValuesForX x
  | x <= 2 = 0
  | isPrime (t (x-1)) = 1 + countPrimeTValuesForX (x-1)
  | otherwise = countPrimeTValuesForX (x-1)

-- main
main :: IO ()
main = do
  putStrLn "Podaj liczbę x:"
  input <- getLine
  let x = read input :: Int
  print (countPrimeTValuesForX x)
