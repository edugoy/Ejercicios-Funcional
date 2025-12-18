existsAny :: (Int -> Bool) -> (Int, Int, Int) -> Bool
existsAny fun (a,b,c) = fun a || fun b || fun c

mejor :: (Int -> Int) -> (Int -> Int) -> Int -> Int
mejor fun1 fun2 numero
    | fun1 numero > fun2 numero = fun1 numero
    | otherwise = fun2 numero

aplicarPar :: (Int -> a) -> (Int, Int) -> (a, a)
aplicarPar fun (a,b) = (fun a, fun b)

parDeFns :: (Int -> a) -> (Int -> b) -> Int -> (a,b)
parDeFns fun1 fun2 numero = (fun1 numero , fun2 numero)

-- solo lo usamos para testear en consola
doble :: Int -> Int
doble = (*2)