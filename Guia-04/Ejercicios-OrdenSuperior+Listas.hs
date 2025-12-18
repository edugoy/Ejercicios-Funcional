--traida propia
doble :: Int -> Int
doble = (*2)

esMultiploDe :: Int -> Int -> Bool
esMultiploDe y = (== 0) . (`mod` y)

--Funciones dadas
paresEntre :: Int -> Int -> [Int]
paresEntre n1 n2 = filter even [n1..n2]

sumarN :: Int -> [Int] -> [Int]
sumarN n = map (+n)


sumarElDobleDeN :: Int -> [Int] -> [Int]
sumarElDobleDeN n = map (+ doble n)

aplicar :: Int -> (Int -> Int) -> Int
aplicar n f = f n

--

esMultiploDeAlguno :: Int -> [Int] -> Bool
esMultiploDeAlguno numero = any (esMultiploDe numero)

promedios :: [[Int]] -> [Float]
promedios = map promedioLista

promedioLista :: [Int] -> Float
promedioLista lista = fromIntegral(sum lista) / fromIntegral(length lista)

promediosSinAplazos :: [[Int]] -> [Float]
promediosSinAplazos lista = promedios $ map listaSinAplazos lista

listaSinAplazos :: [Int] -> [Int]
listaSinAplazos = filter (>4)

mejoresNotas :: [[Int]] -> [Int]
mejoresNotas = map maximum

aprobo :: [Int] -> Bool
aprobo = (>= 6) . minimum

aprobaron :: [[Int]] -> [[Int]]
aprobaron = filter aprobo

divisores :: Int -> [Int]
divisores num = filter (esDivisorDe num) [1..num]

esDivisorDe :: Int -> Int -> Bool
esDivisorDe y = (== 0) . ( y `mod`)

exists :: (a -> Bool) -> [a] -> Bool
exists = any

aplicarFunciones :: [a -> b] -> a -> [b]
aplicarFunciones funciones numero = map ($ numero) funciones