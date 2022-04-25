import System.Win32 (fileTimeToSystemTime)
-- Definir la función addTop que toma una lista de números,
-- y retorna una copia con los dos primeros elementos reemplazados por su suma.

addTop :: [Double] -> [Double]
addTop ([]) = [0]
addTop (a:[]) = [0]
addTop (a:b:[]) = [(a+b)]
addTop (a:b:xs) = (a+b):xs

addTop' :: [Double] -> [Double]
addTop' (a:b:xs) = (a+b):xs
addTop' _ = [0]

-- Definir la función dup, que toma una lista, y un valor por defecto.
-- Retorna la concatenación de la lista con su primer elemento, o con el valor por defecto si es vacía
dup :: [a] -> a -> [a]
dup [] a = [a]
dup (x:xs) _ = x:[x] ++ xs

-- Definir la función hugList, que toman una tupla de dos elementos y una lista.
-- El resultado es una lista dada con el primer elemento de la tupla al principio, y el segundo elemento de la tupla al final.
hugList :: [Double] -> (Double, Double) -> [Double]
hugList lista (a,b) = (a:lista) ++ [b]

-- Definir la función lastFirst que toma una lista, y retorna una nueva lista igual a la dada, pero con el último elemento puesto al inicio.
lastFirst :: [a] -> [a]
lastFirst [] = error "Lista vacía"
lastFirst lista = (tail lista) ++ [(head lista)]

-- Definir la función ifTop que toma una lista de números.
-- Si el primer valor no es ni cero ni NaN, retorna el segundo valor. Si lo es, retorna el tercer valor. Si no hay valores disponibles, retorna NaN.
ifTop :: [Double] -> Double
ifTop [] = (0.0/0.0)
ifTop [_] = (0.0/0.0)
ifTop (x:y:xs)
  | (xs == []) = if (x /= 0 && not (isNaN x)) then y else (0.0/0.0)
  | (length xs > 0) = if (x /= 0 && not (isNaN x)) then y else xs !! 0

-- Definir la función splice, que toma una lista, una posición p, una cantidad d y otra lista.
-- El resultado es la primera lista con d elemento eliminados en la posición p, y la segunda lista concatenada en dicha posición.
splice :: [a] -> Int -> Int -> [a] -> [a]
splice [] _ _ lista = lista
splice lista pos cant concat = (take pos lista) ++ (concat) ++ (drop (pos + cant) lista)

--Definir la función filterNaN, que toma una lista de Double y retorna una copia sin valores NaN.
filterNaN :: [Double] -> [Double]
filterNaN lista = [x | x <- lista, not (isNaN x)]

--Definir la función de Rastrigin, definida de la siguiente forma:
--f(x)=10n+ (i=0∑n−1) [x2i−10cos(2πxi)]

rastrigin :: [Double] -> Double
rastrigin [] = 0
rastrigin lista = 10*(fromIntegral(length lista)) + (rastriginSumatoria lista)

rastriginSumatoria :: [Double] -> Double
rastriginSumatoria [] = 0
rastriginSumatoria (x:xs) = ((x*x) - (10 * cos(2*pi*x))) + rastriginSumatoria xs

rastrigin' :: [Double] -> Double
rastrigin' xs = 10.0 * (fromIntegral (length xs)) + 
  (sum [x * x - 10.0 * (cos (2.0 * pi * x)) | x <- xs])

-- Definir la función vectorProduct, que toma una lista de números y un número, y calcula su producto vectorial
vectorProduct :: [Double] -> Double -> [Double]
vectorProduct lista numero = [x * numero | x <- lista]

-- Definir la función vectorSum, que toma dos listas de números, y calcula su suma vectorial.
-- Si las listas son de distinto largo, se toma el largo de la más corta.

vectorSum :: [Double] -> [Double] -> [Double]
vectorSum [] _ = []
vectorSum _ [] = []
vectorSum (x:xs) (y:ys) = (x+y):(vectorSum xs ys)

vectorSum' :: [Double] -> [Double] -> [Double]
vectorSum' v1 v2 = [x + y | (x, y) <- zip v1 v2]


oneHot :: Int -> Int -> [Double]
oneHot len pos = [if i == pos then 1 else 0 | i <- [0..len]]

-- Definir la función delta, que toma una lista de números, una posición y un desplazamiento.
-- El resultado sera una modificación de la lista dada en la posición dada con el desplazamiento sumado.

delta :: [Double] -> Double -> Double -> [Double]
delta lista pos desp = [if(y == pos) then (x + desp) else x | (x,y) <- zip lista [0..]]

-- Definir la función neighborhood, que toma un vector (lista de números) y un desplazamiento.
-- El resultado será una lista de dos modificaciones por posición, una sumando y otra restando el desplazamiento. Esto es el entorno del vector dado
neighborhood :: [Double] -> Double -> [[Double]]
neighborhood lista desp = [delta lista y d | (x,y) <- xzip, d <- [desp, (-desp)]]
    where xzip = zip lista [0..]

-- Definir la función minRastrigin, que toma una lista de listas de números.
-- Retorna el menor valor para la función de Rastrigin de todos los elementos de la lista, y uno de los elementos que lo generan.
minRastrigin :: [[Double]] -> (Double, [Double])
minRastrigin [] = error "Lista vacía"
minRastrigin lista = minimum [x | x <- listaRastrigin]
  where listaRastrigin = (zip ([rastrigin x | x <- lista]) lista)

minRastrigin' :: [[Double]] -> (Double, [Double])
minRastrigin' vs = minimum [(rastrigin v, v) | v <- vs]


-- Implementar la función "UnirListas", que recibe dos listas de números y retorna una nueva lista producto de sumar los elementos en cada posición
unirListas :: [Double] -> [Double] -> [Double]
unirListas a b = [a + b | (a,b) <- listaTupla]
  where listaTupla = zip a b

-- Dados un array de enteros y un número encontrar si existe una pareja de números dentro del array cuya suma sea el número dado, ejemplo:
-- {2, 6, 3, 9, 11}, dado 9 el resultado sería {6, 3}
buscarSuma :: [Double] -> Double -> (Double, Double)
buscarSuma [] _ = error "Lista vacía!"
buscarSuma lista numero = if (null listaSec) then (0,0) else head listaSec
  where listaSec = [(x, y) | x <- lista, y <- lista, (x + y) == numero]

-- Implementar la función nOf, la cual toma un número entero n y un valor v de un tipo cualquiera.
-- Retorna una lista de largo n, con todos sus elementos iguales a v. Si n es menor o igual que cero se retorna la lista vacía
nOf :: Int -> a -> [a]
nOf n v 
  | n <= 0 = []
  | otherwise = take n (repeat v)

-- Escribir una función de Haskell que tome una lista de enteros (no necesariamente ordenada) y retorne el menor número entero que no esté en la lista,
-- pero que sea mayor al mínimo número de la lista. Si la lista está vacía se debe arrojar un error.

minAfterMin :: [Int] -> Int 
minAfterMin [] = error "Lista vacía"
minAfterMin lista = getMinimum lista (min + 1)
  where min = minimum lista

getMinimum :: [Int] -> Int -> Int
getMinimum lista num
  | elem num lista = getMinimum lista (num + 1)
  | otherwise = num

minAfterMin' :: [Int] -> Int
minAfterMin' is = head [i | i <- [(m+1)..], not (elem i is)]
  where m = minimum is

-- Escribir una función de Haskell que tome una tripleta con tres valores del mismo tipo, una posición en esa tripleta y un nuevo valor.
-- El resultado debe ser una nueva tripleta con el valor en la posición dada reemplazado por el nuevo valor.
-- Si la posición no está entre 1 y 3, se debe retornar la tripleta sin modificar.

updateTriplet :: (a,a,a) -> Int -> a -> (a,a,a)
updateTriplet (a,b,c) 1 d = (d,b,c)
updateTriplet (a,b,c) 2 d = (a,d,c)
updateTriplet (a,b,c) 3 d = (a,b,d)
updateTriplet trip _ _ = trip

-- Definir la función invertirYSumar, que toma una lista numérica, invierte todos sus valores, y a continuación retorna la suma de todos los valores
invertirYSumar :: (Num a) => [a] -> a
invertirYSumar lista = sum [(-x) | x <- lista]

-- Definir la función isIdentifier que toma un String y determina si cumple con la sintaxis para un identificador.
-- Un identificador empieza con una letra, y puede seguir con letras o números.
isIdentifier :: String -> Bool
isIdentifier (x:xs) = (x `elem` (['A'..'Z'] ++ ['a'..'z'])) && isIdentifier2 xs

isIdentifier2 :: String  -> Bool
isIdentifier2 [] = True
isIdentifier2 (x:xs) = x `elem` (['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z']) && (isIdentifier2 xs)

-- Definir la función isDouble que toma un String y determina si cumple con la sintaxis para Doubles.
-- Un Double se escribe como uno o más dígitos decimales, y opcionalmente un punto y uno o más dígitos decimales
isDouble :: String -> Bool
isDouble [] = False
isDouble (x:[]) = False
isDouble (x:y:[]) = False
isDouble lista@(x:xs)
  | (x == '.') = False
  | otherwise = isDouble' xs False

isDouble' :: String -> Bool -> Bool
isDouble' [] encPunto = encPunto
isDouble' (x:xs) encPunto
  | x == '.' = (not encPunto) && isDouble' xs True
  | x `elem` ['0'..'9'] = isDouble' xs encPunto
  | otherwise = False


-- Definir la función evalOp que toma un String y una lista de Double.
-- Evalúa el operador del String, quitando los operandos de la lista y concatenando el resultado
evalOp :: String -> [Double] -> [Double]
evalOp val lista | (isDouble val) = ([read val ::Double] ++ lista)
evalOp "+" (x:y:xs) = (x+y) : xs
evalOp "-" (x:y:xs) = (x-y) : xs
evalOp "*" (x:y:xs) = (x*y) : xs
evalOp "/" (x:y:xs) = (x/y) : xs
evalOp valor (x:[]) = error "Valor sin sentido"
evalOp _ _ = error "Hubo un error mala tuya"


evalOp' :: String -> [Double] -> [Double]
evalOp' op ds | isDouble op = (read op):ds
evalOp' "+" (d1:d2:ds) = (d1 + d2):ds
evalOp' "-" (d1:d2:ds) = (d1 - d2):ds
evalOp' "*" (d1:d2:ds) = (d1 * d2):ds
evalOp' "/" (d1:d2:ds) = (d1 / d2):ds
evalOp' op _ = error ("Invalid operator "++ op ++"!")

-- Definir la función eval que toma un String y una lista de Double.
-- Evalúa los operadores en el String, llamando a evalOp.

-- eval :: String -> [Double] -> [Double]
-- eval lista numeros = 
--   where palabras = words lista,
--         numeros = [x | x <- ]


codigoA :: Int -> [String]
codigoA y = take y (repeat (show y))

codigoB :: [Integer] -> [Integer]
codigoB [] = []
codigoB [x] = [x]
codigoB x = (last x): codigoB(init x)