esMultiploDeTres :: Int -> Bool
esMultiploDeTres x = x `mod` 3 == 0

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = y `mod` x == 0 

cubo :: Int -> Int
cubo x = x * x * x

area :: Int -> Int -> Int
area x y = x * y

esBisiesto ::  Int -> Bool
esBisiesto x =  esMultiploDe 400 x || (esMultiploDe 4 x && not(esMultiploDe 100 x))

celsiusToFahr :: Int -> Float
celsiusToFahr x = (fromIntegral x * 9 / 5) + 32

fahrToCelsius :: Float -> Float
fahrToCelsius x = (x - 32) * 5 / 9

haceFrioF :: Float -> Bool
haceFrioF x = x < celsiusToFahr 8 

mcm :: Int -> Int -> Int
mcm x y =  (x * y) `div` gcd x y