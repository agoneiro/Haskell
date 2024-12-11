-- funkcja tworząca wektor o zadanym rozmiarze
createVector :: Int -> [Double]
createVector 0 = []
createVector size = 0.0 : createVector (size - 1)

-- funkcja dodająca wartości pod zadane indeksy w wektorze
updateVector :: [Double] -> [(Int, Double)] -> [Double]
updateVector vec [] = vec
updateVector vec ((index, value):xs) = 
    updateVector (updateAtIndex vec index value) xs

-- funkcja powiększająca o wartość w wektorze na danym indeksie
updateAtIndex :: [Double] -> Int -> Double -> [Double]
updateAtIndex (x:xs) 0 value = (x + value) : xs 
updateAtIndex (x:xs) index value = x : updateAtIndex xs (index - 1) value 

-- funkcja aktualizująca wartość w wektorze na danym indeksie
setAtIndex :: [Double] -> Int -> Double -> [Double]
setAtIndex (x:xs) 0 value = value : xs 
setAtIndex (x:xs) index value = x : setAtIndex xs (index - 1) value 

-- funkcja dzieląca wektor przez stałą
divideVector :: [Double] -> Double -> [Double]
divideVector [] _ = []  
divideVector (x:xs) divisor = (x / divisor) : divideVector xs divisor

-- funkcja tworząca wektor z prawdopodobieństwami przejść dla stanu i
expectedVector :: Int -> Int -> [Double]
expectedVector n i = setAtIndex (updateVector (createVector (n `div` 2 + 1)) probabilities) 0 1.0
-- tworzymy pusty wektor i zwiększamy na odpowiednich indeksach prawdopodobieństwa zmiany stanu (probabilities)
    where
    probabilities = 
        [ 
        (i, 1/2), -- brak zmiany stanu
        (abs (i - 2), 1/36),  -- ruch pierwszej kostki w prawo i drugiej w lewo
        (if (i + 2) > n `div` 2 then abs (n - i - 2) else i + 2, 1/36), -- ruch pierwszej kostki w lewo i drugiej w prawo
        (if (i + 1) > n `div` 2 then abs (n - i - 1) else i + 1, 2/9), -- ruch pierwszej kostki w lewo lub drugiej w prawo
        (abs (i - 1), 2/9) -- ruch pierwszej kostki w prawo lub drugiej w lewo
        ]

-- funkcja pomocnicza do podstawiania jednego wektora do drugiego
updateExpectedVector :: [Double] -> [Double] -> Double -> [Double]
updateExpectedVector [] [] _ = []
updateExpectedVector (v:vs) (p:ps) multiplier = 
    (v + p * multiplier) : updateExpectedVector vs ps multiplier

-- funkcja do normalizacji wektora (przeniesienie s-tego indeksu i normalizacja)
normalize :: [Double] -> Int -> [Double]
normalize vec s = 
    divideVector (setAtIndex vec s 0.0) temp
    where
        temp = 1 - (vec !! s)

-- normalizuje wektor prawdopodobieństw dla i
normalizeExpected :: Int -> Int -> [Double]
normalizeExpected n 1 = normalize (expectedVector n 1) 1
normalizeExpected n i = normalizeExpectedRecursive n 1 i (expectedVector n i)

-- rekurencyjna funkcja która podstawia wektory z wszystkich wcześniejszych stanów (i - najmniejszy podstawiany indeks)
normalizeExpectedRecursive :: Int -> Int -> Int -> [Double] -> [Double]
normalizeExpectedRecursive n i 1 vec = normalize vec 1
normalizeExpectedRecursive n i s vec
    | i >= s = vec
    | otherwise = normalizeExpectedRecursive n (i + 1) s (normalize (setAtIndex updatedVec i 0.0) s)
    where 
        previous = normalizeExpectedRecursive n 1 i (expectedVector n i)
        multiplier = vec !! i
        updatedVec = updateExpectedVector vec previous multiplier

expectedTurns :: Int -> Double
expectedTurns n = head (normalizeExpected n (n `div` 2))

-- main
main :: IO ()
main = do
  putStrLn "Podaj liczbę n:"
  input <- getLine
  let n = read input :: Int
  if even n && n > 2 then print (expectedTurns n)
  else error "Liczba graczy powinna być parzysta i większa od 2"
