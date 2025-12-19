data Auto = Auto Color Velocidad Distancia
type Color = String
type Carrera = [Auto]
type Velocidad = Int
type Distancia = Int

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = sonDistintos auto1 auto2 && distanciaEntre auto1 auto2 < 10

sonDistintos :: Auto -> Auto -> Bool
sonDistintos (Auto color1 _ _) (Auto color2 _ _) = color1 /= color2

distanciaEntre :: Auto -> Auto -> Int
distanciaEntre (Auto _ _ distancia1) (Auto _ _ distancia2 ) = abs (distancia1 - distancia2)

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera = not (any(estaCerca auto) (otrosAutos auto carrera)) && all(leGana auto) (otrosAutos auto carrera)

leGana :: Auto -> Auto -> Bool
leGana (Auto _ _ distancia1) (Auto _ _ distancia2 ) = distancia1 > distancia2

otrosAutos :: Auto -> Carrera -> Carrera
otrosAutos auto = filter(sonDistintos auto)

puesto :: Auto -> Carrera -> Int
puesto auto carrera = 1 + cantidadAutosQueLeGana auto carrera

cantidadAutosQueLeGana :: Auto -> Carrera -> Int
cantidadAutosQueLeGana auto carrera = length(filter(leGananA auto)carrera)

leGananA :: Auto -> Auto -> Bool
leGananA auto rival = leGana rival auto

correDurante :: Int -> Auto -> Auto
correDurante tiempo (Auto color velocidad distancia) = Auto color velocidad (distancia + tiempo * velocidad)

alterarVelocidad :: (Int -> Int) -> Auto -> Auto
alterarVelocidad funcion (Auto color velocidad distancia) = Auto color (funcion velocidad) distancia

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad disminuir = alterarVelocidad ( max 0 . subtract disminuir)