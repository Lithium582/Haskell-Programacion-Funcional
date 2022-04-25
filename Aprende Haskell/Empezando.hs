-- función 'succ' retorna el siguiente número al que se pasa por parámetro
-- Ej: succ 8 --> 9
sucesor :: Int -> Int
sucesor a = succ a

-- función "max" retorna el mayor valor entre dos valores comparables y ordenables del mismo tipo
-- Ej: max 8 9 --> 9
-- max "abc" "cba" --> "cba"
maximo :: String -> String -> String
maximo a b = max a b

maximoInt :: Int -> Int -> Int
maximoInt a b = max a b

-- función "min" retorna el menor valor entre dos valores comparables y ordenables del mismo tipo
-- Ej: max 8 9 --> 9
-- max "abc" "cba" --> "cba"
minimo :: String -> String -> String
minimo a b = min a b

minimoInt :: Int -> Int -> Int
minimoInt a b = min a b

-- función "div" realiza una división entera entre dos valores Int
divisionEntera :: Int -> Int -> Int
divisionEntera a b = div a b

divisionEnteraInfija :: Int -> Int -> Int
divisionEnteraInfija a b = a `div` b

-- LISTAS

-- "++" permite unir dos listas
unirDosListas :: [Int] -> [Int] -> [Int]
unirDosListas lA lB = lA ++ lB

-- ":" permite agregar un elemento al principio de una lista
nuevoElementoPrincipio :: Int -> [Int] -> [Int]
nuevoElementoPrincipio a l = a:l

nuevoElementoPrincipioString :: Char -> String -> String
nuevoElementoPrincipioString a l = a:l

-- "head" retorna el primer elemento de una lista
cabeza :: [Int] -> Int
cabeza a = head a

-- "tail" retorna la lista sin el primer elemento
cola :: [Int] -> [Int]
cola a = tail a

-- "last" retorna el último elemento de una lista
ultimo :: [Int] -> Int
ultimo a = last a

-- "head" retorna la lista sin el primer elemento
inicio :: [Int] -> [Int]
inicio a = init a

-- "length" retorna la cantidad de elementos de una lista
largo :: [Int] -> Int
largo a = length a

-- "!!" permite obtener el elemento en una posición específica de una lista
posicion :: [Int] -> Int -> Int
posicion a pos = a !! pos

-- "null" comprueba si una lista está vacía
listaEstaVacia :: [Int] -> Bool
listaEstaVacia a = null a

-- "reverse" da vuelta una lista
revertir :: [Int] -> [Int]
revertir a = reverse a

-- "take" toma cierta cantidad de elementos de una lista, comenzando desde el principio
tomar :: Int -> [Int] -> [Int]
tomar cant a = take cant a

-- "drop" elimina cierta cantidad de elementos de una lista
borrar :: Int -> [Int] -> [Int]
borrar cant a = drop cant a

-- "maximum" toma el máximo elemento de una lista de tipo comparable
maximoLista :: [Int] -> Int
maximoLista a = maximum a

-- "minimum" toma el mínimo elemento de una lista de tipo comparable
minimoLista :: [Int] -> Int
minimoLista a = minimum a

-- "sum" retorna la suma de todos los elementos de la lista
sumatoria :: [Int] -> Int
sumatoria a = sum a

-- "product" retorna el producto de todos los elementos de la lista
productoria :: [Int] -> Int
productoria a = product a

-- "elem" confirma si cierto elemento se encuentra en la lista
elementoEnLista :: Int -> [Int] -> Bool
elementoEnLista a l = elem a l 

-- RANGOS
listaDeUnoAVeinte :: [Int]
listaDeUnoAVeinte = [1..20]

listaDeAAZeta :: [Char]
listaDeAAZeta = ['a'..'z']

listaDeKaAErre :: [Char]
listaDeKaAErre = ['K'..'R']

listaMultiplosDeTres :: [Int]
listaMultiplosDeTres = [3,6..81]

-- LISTAS INTENCIONALES
listaIntencional :: [Int]
listaIntencional = [x*2 | x <- [0..5]]

listaIntencionalDos :: [Int]
listaIntencionalDos = [x*2 | x <- [0..10], x*2 >=12]

listaIntencionalTres :: [Int]
listaIntencionalTres = [x | x <- [50..100], x `mod` 7 == 3]

boomBangs :: [Int] -> [String]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- TUPLAS
zippeandoTuplas :: [Int] -> [Int] -> [(Int, Int)]
zippeandoTuplas a b = zip a b

-- QUICKSORT
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort menores) ++ [x] ++ (quicksort mayores)
    where menores = [elem | elem <- xs, elem <= x]
          mayores = [elem | elem <- xs, elem > x]

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = (quicksort' menores) ++ [x] ++ (quicksort' mayores)
    where menores = filter (<=x) xs
          mayores = filter (>x) xs

