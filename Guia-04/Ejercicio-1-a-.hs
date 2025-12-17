import Data.ByteString.Builder (FloatFormat)
sumaLista :: [Int] -> Int
sumaLista = sum

frecuenciaCardiaca :: [Int]
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125]

promedioFrecuenciaCardiaca :: Float
promedioFrecuenciaCardiaca = fromIntegral(sumaLista frecuenciaCardiaca) / fromIntegral(length frecuenciaCardiaca)

frecuenciaCardiacaMinuto :: Int -> Int
frecuenciaCardiacaMinuto minuto = frecuenciaCardiaca !! indice minuto

indice :: Int -> Int
indice numero 
    | numero == 10 = 0
    | otherwise = numero `div` 10

frecuenciasHastaMomento :: Int -> [Int]
frecuenciasHastaMomento numero = (take . cantidadFrecuencias) numero frecuenciaCardiaca

cantidadFrecuencias :: Int -> Int
cantidadFrecuencias num = (num `div` 10) + 1