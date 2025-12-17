fst3 :: (a, b, c) -> a
fst3 (a, _ , _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b , _) = b

trd3 :: (a, b, c) -> c
trd3 (_, _ , c) = c

aplicar :: (Int -> Int, Int -> Int) -> Int -> (Int, Int)
aplicar (funcion1, funcion2) x = (funcion1 x, funcion2 x)

cuentaBizarra :: (Int, Int) -> Int
cuentaBizarra (a, b)
    | a > b = a + b
    | diferencia a b > 10 = b - a
    | otherwise = a * b

diferencia :: Int -> Int -> Int
diferencia a b = b - a

esNotaBochazo :: Int -> Bool
esNotaBochazo = (6>)

aprobo :: (Int, Int) -> Bool
aprobo (a, b) = not(esNotaBochazo a) && not(esNotaBochazo b)

promociono :: (Int, Int) -> Bool
promociono (a, b) = a + b == 15 && mayoresA8 (a,b)

mayoresA8 :: (Int, Int) -> Bool
mayoresA8 (a,b) = a >= 8 && b >= 8

notasFinales :: ((Int, Int),(Int, Int)) -> (Int, Int)
notasFinales ((par1, par2),(recu1, recu2)) = (max par1 recu1 , max par2 recu2)

recuperaDeGusto :: ((Int, Int),(Int, Int)) -> Bool
recuperaDeGusto ((par1, par2),(recu1, recu2)) = promociono (par1, par2) && not(recuperoParciales(recu1, recu2))

recuperoParciales :: (Int, Int) -> Bool
recuperoParciales (nota1, nota2) = nota1 >= 1 && nota2 >= 1

esMayorDeEdad :: (String, Int) -> Bool
esMayorDeEdad = (>21).snd

calcular :: (Int, Int) -> (Int, Int)
calcular (a, b) = (criterioPar a, criterioImpar b)

criterioPar :: Int -> Int
criterioPar a
    | even a = a * 2
    | otherwise = a

criterioImpar :: Int -> Int
criterioImpar a
    | odd a = a + 1
    | otherwise = a