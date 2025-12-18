--LISTAS

frecuenciaCardiaca :: [Int]
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125]

promedioFrecuenciaCardiaca :: Float
promedioFrecuenciaCardiaca = fromIntegral(sum frecuenciaCardiaca) / fromIntegral(length frecuenciaCardiaca)

frecuenciaCardiacaMinuto :: Int -> Int
frecuenciaCardiacaMinuto minuto = frecuenciaCardiaca !! indice minuto

indice :: Int -> Int
indice numero 
    | numero < 10 = 0
    | otherwise = numero `div` 10

frecuenciasHastaMomento :: Int -> [Int]
frecuenciasHastaMomento numero = take ((indice . (+ 1)) numero ) frecuenciaCardiaca

esCapicua :: [[String]] -> Bool
esCapicua lista = concat lista == (reverse . concat) lista

duracionLlamadas :: ((String, [Int]), (String, [Int]))
duracionLlamadas = (("horarioReducido",[20,10,25,15]), ("horarioNormal",[10,5,8,2,9,10]))

cuandoHabloMasMinutos :: String
cuandoHabloMasMinutos 
    | tiempoLlamada (fst duracionLlamadas) > tiempoLlamada (snd duracionLlamadas) = fst (fst duracionLlamadas)
    | otherwise = fst (snd duracionLlamadas)


tiempoLlamada :: (String, [Int]) -> Int
tiempoLlamada = sum . snd 