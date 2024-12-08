-- największy wspólny dzielnik
greatestCommonDivisor :: Int -> Int -> Int
greatestCommonDivisor a 0 = abs a
greatestCommonDivisor a b = greatestCommonDivisor b (a `mod` b)

-- sprawdza czy dwie liczby są względnie pierwsze
coprime :: Int -> Int -> Bool
coprime a b = greatestCommonDivisor a b == 1 

-- generuje pierwotne trójki pitagorejskie takie, że a + b + c = n
pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples n = 
  [ 
    (a, b, c) | 
    m <- [2..n `div` 2], -- max. wartość m = n/2, bo n = 2m^2 + 2mk
    k <- [1..m-1], -- k < m
    coprime m k, 
    (m + k) `mod` 2 == 1, 
    let a = m*m - k*k, 
    let b = 2*m*k, 
    let c = m*m + k*k, 
    a + b + c == n 
  ]

-- szuka trójki dla najwiekszego m < n
findTriple :: Int -> (Int, Int, Int)
findTriple n
  | n < 12 = error "Nie znaleziono trójki pitagorejskiej"
  | otherwise = 
    case pythagoreanTriples n of
      (x:_) -> x
      []    -> findTriple (n-1)

-- main
main :: IO ()
main = do
  putStrLn "Podaj liczbę n:"
  input <- getLine
  let n = read input :: Int
  print (findTriple n)
