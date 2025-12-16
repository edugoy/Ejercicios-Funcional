pesoPino :: Int -> Int
pesoPino x 
    | x <= 300 = x * 3
    | x > 300 = (300 * 3) + pesoMayor3Mt x

pesoMayor3Mt :: Int -> Int
pesoMayor3Mt x = (x - 300) * 2

esPesoUtil :: Int -> Bool
esPesoUtil x = pesoPino x >= 400 && pesoPino x <= 1000

sirvePino :: Int -> Bool
-- sirvePino x = esPesoUtil(pesoPino x) forma sin composicion 
sirvePino x = (esPesoUtil . pesoPino) x -- forma con composicion (se pueden sacar las x de ambos lados del '=')
-- sirvePino = esPesoUtil . pesoPino asi seria sin las x de ambos lados