dispersion :: Int -> Int -> Int -> Int
dispersion a b c = numeroMaximo a b c - numeroMinimo a b c 

numeroMaximo :: Int -> Int -> Int -> Int
numeroMaximo a b c = max a (max b c)

numeroMinimo :: Int -> Int -> Int -> Int
numeroMinimo a b c = min a (min b c)

diasParejos :: Int -> Int -> Int -> Bool
diasParejos a b c = dispersion a b c < 30

diasLocos :: Int -> Int -> Int -> Bool
diasLocos a b c = dispersion a b c > 100

diasNormales :: Int -> Int -> Int -> Bool
diasNormales a b c = not (diasLocos a b c) && not (diasParejos a b c)