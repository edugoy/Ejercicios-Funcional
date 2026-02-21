--1)
data Plomero = Plomero {
    nombre :: String,
    caja :: [Herramienta],
    reparaciones :: [Reparacion],
    dinero :: Float
} deriving (Eq, Show)

data Herramienta = Herramienta {
    denominacion :: String,
    precio :: Float,
    material :: Material
} deriving (Eq, Show)

data Material = Hierro | Madera  | Goma | Plastico deriving (Eq, Show)

llaveInglesa, martillo :: Herramienta
llaveInglesa = Herramienta "Llave Inglesa" 200 Hierro
martillo = Herramienta "Martillo" 20 Madera

mario, wario :: Plomero
mario = Plomero "Mario" [llaveInglesa, martillo] [] 1200
wario = Plomero "Wario" (map hacerLlaveFrancesa [1..]) [] 0.5

hacerLlaveFrancesa :: Float -> Herramienta
hacerLlaveFrancesa numeroLlave = Herramienta "Llave francesa" numeroLlave Hierro

--2)
tieneHerramienta :: String -> Plomero -> Bool
tieneHerramienta denHerramienta plomero = any ((== denHerramienta) . denominacion) (caja plomero)