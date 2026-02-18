type Cancion = [Nota]

data Nota = Nota {
    tono :: Float,
    volumen :: Float,
    duracion :: Float
} deriving (Eq, Show)

cambiarVolumen :: (Float -> Float) -> Nota -> Nota
cambiarVolumen delta nota = nota { volumen = delta (volumen nota)}

cambiarTono :: (Float -> Float) -> Nota -> Nota
cambiarTono delta nota = nota { tono = delta (tono nota)}

cambiarDuracion :: (Float -> Float) -> Nota -> Nota
cambiarDuracion delta nota = nota { duracion = delta (duracion nota)}

promedio :: [Float] -> Float
promedio lista = sum lista / fromIntegral (length lista)

-- 1) a)
esAudible :: Nota -> Bool
esAudible nota = tonoEntre 20 20000 nota && volumen nota > 10

tonoEntre :: Float -> Float -> Nota -> Bool
tonoEntre min max nota = tono nota >= min && tono nota <= max

-- b)
esMolesta :: Nota -> Bool
esMolesta nota
    | not (esAudible nota) = False
    | tono nota < 250 = volumen nota > 85
    | otherwise = volumen nota > 55

-- 2) a)
silencioTotal :: Cancion -> Float
silencioTotal = sum . map duracion . filter(not . esAudible)

-- b)
sinInterrupciones :: Cancion -> Bool 
sinInterrupciones cancion = all esAudible (filter ((> 0.1) . duracion) cancion)

-- c) 
peorMomento :: Cancion -> Float 
peorMomento = maximum . map volumen . filter esMolesta

-- 3)
type Filtro = Cancion -> Cancion

trasponer :: Float -> Filtro
trasponer escalar = map (cambiarTono (* escalar))

acotarVolumen :: Float -> Float -> Filtro
acotarVolumen max min cancion = 

acotar :: Float -> Float -> Float -> Float
acotar max min volumen 
    | volumen < min = 
    | otherwise = 