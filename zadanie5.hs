-- Sprawdza, czy liczby są względnie pierwsze
coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

-- Generuje pierwotne trójki pitagorejskie
pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples n = 
  [ (a, b, c) | 
    m <- [2..n `div` 2], 
    k <- [1..m-1], 
    coprime m k, 
    (m - k) `mod` 2 == 1, 
    let a = m*m - k*k, 
    let b = 2*m*k, 
    let c = m*m + k*k, 
    a + b + c == n 
  ]

-- Szuka największego m < n, jeśli nie ma trójki dla n
findTriple :: Int -> (Int, Int, Int)
findTriple n = 
  case pythagoreanTriples n of
    (x:_) -> x
    []    -> findTriple (n-1)

-- Funkcja główna
main :: IO ()
main = do
  putStrLn "Podaj liczbę n:"
  input <- getLine
  -- Konwertujemy wejście na liczbę całkowitą
  let n = read input :: Int
  print (findTriple n)
