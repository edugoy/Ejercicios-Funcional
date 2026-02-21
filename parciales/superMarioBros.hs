import Data.Char (toUpper, isUpper)
-- 1)
data Plomero = Plomero {
    nombre :: String,
    caja :: [Herramienta],
    reparaciones :: [Reparacion],
    dinero :: Float
}

data Herramienta = Herramienta {
    denominacion :: String,
    precio :: Float,
    material :: Material
} deriving (Eq, Show)

data Material = Hierro | Madera  | Goma | Plastico deriving (Eq, Show)

llaveInglesa, martillo :: Herramienta
llaveInglesa = Herramienta "Llave inglesa" 200 Hierro
martillo = Herramienta "Martillo" 20 Madera

mario, wario :: Plomero
mario = Plomero "Mario" [llaveInglesa, martillo] [] 1200
wario = Plomero "Wario" (map hacerLlaveFrancesa [1..]) [] 0.5

hacerLlaveFrancesa :: Float -> Herramienta
hacerLlaveFrancesa numeroLlave = Herramienta "Llave francesa" numeroLlave Hierro

-- 2)
tieneHerramienta :: String -> Plomero -> Bool
tieneHerramienta denominacionHerramienta plomero = any ((== denominacionHerramienta) . denominacion) (caja plomero)

esMalvado :: Plomero -> Bool
esMalvado plomero = take 2 (nombre plomero) == "Wa"

puedeComprar :: Plomero -> Herramienta -> Bool
puedeComprar plomero herramienta = dinero plomero >= precio herramienta

-- 3)
esBuena :: Herramienta -> Bool
esBuena herramienta = 
    (material herramienta == Hierro && precio herramienta > 10000) || 
    (denominacion herramienta == "Martillo" && (material herramienta == Madera || material herramienta == Goma))

--4)
comprarHerramienta :: Herramienta -> Plomero -> Plomero
comprarHerramienta herramienta plomero 
    | puedeComprar plomero herramienta = plomero {dinero = dinero plomero - precio herramienta, caja = herramienta : caja plomero}
    | otherwise = plomero 

--5)
data Reparacion = Reparacion {
    descripcion :: String,
    requerimiento :: Plomero -> Bool
}

filtracionDeAgua :: Reparacion
filtracionDeAgua = Reparacion "Filtracion de Agua" (tieneHerramienta "Llave inglesa")

esDificil :: Reparacion -> Bool
esDificil reparacion = length (descripcion reparacion) > 100 && estaEnMayusculas (descripcion reparacion)

estaEnMayusculas :: String -> Bool
estaEnMayusculas texto = texto == map toUpper texto

presupuesto :: Reparacion -> Float
presupuesto reparacion = fromIntegral(3 * length (descripcion reparacion)) 

--6)
hacerReparacion :: Reparacion -> Plomero -> Plomero 
hacerReparacion reparacion plomero
    | not (puedeHacer reparacion plomero) = cobrar 100 plomero
    | otherwise =  accionesACumplir reparacion (agregarReparacion reparacion (cobrar (presupuesto reparacion) plomero))

cobrar :: Float -> Plomero -> Plomero
cobrar cantidad plomero = plomero {dinero = dinero plomero + cantidad} 

agregarReparacion :: Reparacion -> Plomero -> Plomero
agregarReparacion reparacion plomero = plomero { reparaciones = reparacion : reparaciones plomero}

puedeHacer :: Reparacion -> Plomero -> Bool
puedeHacer reparacion plomero = requerimiento reparacion plomero || (esMalvado plomero && tieneHerramienta "Martillo" plomero)

accionesACumplir :: Reparacion -> Plomero -> Plomero
accionesACumplir reparacion plomero 
    | esMalvado plomero = plomero {caja = destornilladorRobado : caja plomero}
    | not (esMalvado plomero) && esDificil reparacion = pierdeBuenasHerramientas plomero
    | otherwise = plomero {caja = tail (caja plomero)}

destornilladorRobado :: Herramienta
destornilladorRobado = Herramienta "Destornillador" 0 Plastico

pierdeBuenasHerramientas :: Plomero -> Plomero
pierdeBuenasHerramientas plomero = plomero {caja = filter (not . esBuena) (caja plomero)}

--7)
jornada :: [Reparacion] -> Plomero -> Plomero
jornada reps plomero = foldl aplicarReparacion plomero reps

aplicarReparacion :: Plomero -> Reparacion -> Plomero
aplicarReparacion plomero reparacion = hacerReparacion reparacion plomero

--8)
