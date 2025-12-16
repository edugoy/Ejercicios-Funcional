-- COMPOSICION
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x = (== 0) . (`mod` x)

inversaRaizCuadrada :: Float -> Float
inversaRaizCuadrada = inversa . sqrt
    
inversa :: Float -> Float
inversa = (1/)

incrementMCuadradoN ::  Int -> Int -> Int
incrementMCuadradoN m = (+) (cuadrado m) 

cuadrado :: Int -> Int
cuadrado x = x * x

esResultadoPar :: Int -> Int -> Bool
esResultadoPar n = even . (n ^)