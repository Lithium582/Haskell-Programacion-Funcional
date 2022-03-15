-- Definir la función leapYear, que toma un año (del calendario Gregoriano) y retorna True si es bisiesto
leapYear :: Int -> Bool
leapYear x = if (((x `mod` 4) == 0) && ((x `mod` 100) /= 0) && ((x `mod` 400) == 0))
                then True
                else False

leapYear' :: Int -> Bool
leapYear' x = ((x `mod` 4) == 0) && 
    (
        ((x `mod` 100) /= 0) || ((x `mod` 400) == 0)
    )

-- Definir la función bracketChars, que toma dos caracteres y retorna True si se pueden usar para encerrar texto (en el orden dado).
-- Estos caracteres son comillas, paréntesis y signos de exclamación e interrogación.

bracketChars :: Char -> Char -> Bool
bracketChars '(' ')' = True
bracketChars '{' '}' = True
bracketChars '[' ']' = True
bracketChars '\'' '\'' = True
bracketChars '"' '"' = True
bracketChars '¡' '!' = True
bracketChars '¿' '?' = True
bracketChars _ _ = False

-- Factorial
factorial :: Int -> Int
factorial n = if (n == 0) then 1
    else if (n >= 1) then n * (factorial (n-1))
    else error "No definimos el Factorial de números negativos"

-- Usando patrones y guardas
factorial' :: Int -> Int
factorial' 0 = 1
factorial' n | n > 0 = n * (factorial' (n-1))
factorial' n | n < 0 = error "No definimos el Factorial de números negativos"

-- Definir la función power, que toma una base b (Double) y un exponente e (Int), y retorna b^e
power :: Double -> Int -> Double
power b 0 = 1
power 0 e = 0
power b e = b * (power b (e - 1))

-- Definir la función fibonacci, que dado un número natural n retorna el enésimo número en la secuencia de Fibonacci
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x | x > 0 = (fibonacci (x - 1) + fibonacci (x - 2))
fibonacci x | x < 0 = error "No definimos Fibonacci para números negativos"

fibonacci' :: Int -> Int
fibonacci' x | x < 0 = error "No definimos Fibonacci para números negativos"
fibonacci' x | x >= 0 && x < 2 = x
fibonacci' x | x >= 2 = (fibonacci' (x - 1) + fibonacci' (x - 2))

-- Definir la función nZeroes, que dado un número n genera una lista de largo n con todos sus elementos en 0
nZeroes :: Int -> [Int]
nZeroes 0 = []
nZeroes n | n < 0 = error "No definimos nZeroes para números negativos"
nZeroes n = 0:(nZeroes(n-1))

-- Definir la función oneHot, que toma un largo n y un índice i.
-- Retorna una lista de largo n con todos sus elementos en 0, salvo un 1 en la posición i (si existe).
oneHot :: Int -> Int -> [Int]
oneHot 0 _ = []
oneHot x _ | x < 0 = error "oneHot no definido para números negativos"
oneHot x i = _oneHot x i 0

_oneHot :: Int -> Int -> Int -> [Int]
_oneHot 0 _ _ = []
_oneHot x i iter = if (iter == i) then 1:(_oneHot (x - 1) i (iter + 1))
    else 0:(_oneHot (x - 1) i (iter + 1))

oneHot' :: Int -> Int -> [Int]
oneHot' n _ | n < 0 = error "Negative length!"
oneHot' 0 _ = []
oneHot' n i = (if i == 0 then 1 else 0):(oneHot' (n - 1) (i - 1))

-- Definir la función whichAreHot, que toma una lista de enteros.
-- Retorna la lista de los índices de los valores distintos de 0.
whichAreHot :: [Int] -> [Int]
whichAreHot [] = [] -- Lista vacía
whichAreHot x = _whichAreHot x 0

_whichAreHot :: [Int] -> Int -> [Int]
_whichAreHot (x:[]) ind = if(x /= 0) then [ind] else [] -- Cola vacía
_whichAreHot (0:xs) ind = _whichAreHot xs (ind + 1)
_whichAreHot (x:xs) ind = ind:_whichAreHot xs (ind + 1)

-- Definir la función whichIsHot, que toma una lista de enteros.
-- Retorna el índice del único valor distinto de 0, o falla

whichIsHot :: [Int] -> Int
whichIsHot [] = error "LISTA VACÍA"
whichIsHot lista = _whichIsHot lista (-1) (0)

-- Lista -> PosiciónEncontrada -> PosiciónActual -> Retorno
_whichIsHot :: [Int] -> Int -> Int -> Int
_whichIsHot (x:[]) (-1) p | (x /= 0) = p -- Único elemento distinto de cero es el último
_whichIsHot (x:[]) (-1) p | (x == 0) = error "Lista de ceros"
_whichIsHot (x:[]) pEnc p | (x == 0) = pEnc
_whichIsHot (x:[]) pEnc p | (x == 0) = error "Más de un elemento distinto de cero"
_whichIsHot (x:xs) (-1) p | (x /= 0) = _whichIsHot xs p (p + 1) -- Encontré el primer elemento distinto de cero
_whichIsHot (x:xs) (-1) p | (x == 0) = _whichIsHot xs (-1) (p + 1) -- Sigo sin encontrar elementos distintos de cero
_whichIsHot (x:xs) pEnc p | (x == 0) = _whichIsHot xs pEnc (p + 1) -- Ya encontré uno, y el actual es cero
_whichIsHot (x:xs) pEnc p | (x /= 0) = error "Más de un elemento distinto de cero"

whichIsHot' :: [Int] -> Int
whichIsHot' ns = if (length is == 1) then (head is) else error "!!"
  where is = whichAreHot ns


-- Definir la función listRange, que dada una lista de Doubles retorna un par con los valores mínimo y máximo
listRange :: [Double] -> (Double, Double)
listRange [] = error "Lista Vacía"
listRange (x:xs) = _listRange xs x x

-- Lista -> Mín -> Máx -> Tupla
_listRange :: [Double] -> Double -> Double -> (Double, Double)
_listRange [] minimo maximo = (minimo, maximo)
_listRange (x:[]) minimo maximo = ((min minimo x), (max maximo x))
_listRange (x:xs) minimo maximo = _listRange xs (min minimo x) (max maximo x)

listRange' :: [Double] -> (Double, Double)
listRange' [] = error "Empty list!"
listRange' [n] = (n, n)
listRange' (n:ns) = (min minNs n, max maxNs n)
  where (minNs, maxNs) = listRange' ns


--Definir la función mergeRanges, que toma dos pares de Doubles definiendo rangos.
--Retorna el rango mínimo que incluye ambos rangos dados.

mergeRanges :: (Double, Double) -> (Double, Double) -> 
  (Double, Double)
mergeRanges (l1, r1) (l2, r2) = listRange [l1, r1, l2, r2]


--Definir la función filterRange, que toma una lista de Doubles y una tupla que define un rango.
--Retorna la lista con los valores que están en el rango dado.

filterRanges :: [Double] -> (Double, Double) -> [Double]
filterRanges [] (_, _) = []
filterRanges (x:xs) (minimo, maximo) = if (x >= minimo && x <= maximo) then x:(filterRanges xs (minimo, maximo))
    else (filterRanges xs (minimo, maximo))

filterRange' :: [Double] -> (Double, Double) -> [Double]
filterRange' _ (l, r) | l > r = error "Invalid range!"
filterRange' [] _ = []
filterRange' (n:ns) (l, r)
  | l <= n && n <= r = n:(filterRange' ns)
  | otherwise = filterRange' ns
