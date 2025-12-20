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

-- funcion dada
afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

terremoto :: Auto -> Carrera -> Carrera
terremoto auto = afectarALosQueCumplen (estaCerca auto) (bajarVelocidad 50)

miguelitos :: Auto -> Int -> Carrera -> Carrera
miguelitos auto cantidad = afectarALosQueCumplen (leGananA auto) (bajarVelocidad cantidad)

jetpack :: Int -> Auto -> Carrera -> Carrera
jetpack tiempo auto = afectarALosQueCumplen (esElMismo auto) (restaurarVelocidad . efectoJetpack tiempo)

esElMismo :: Auto -> Auto -> Bool
esElMismo (Auto color1 _ _) (Auto color2 _ _) = color1 == color2

efectoJetpack :: Int -> Auto -> Auto
efectoJetpack tiempo auto = correDurante tiempo (alterarVelocidad (*2) auto)

restaurarVelocidad :: Auto -> Auto
restaurarVelocidad = alterarVelocidad (`div` 2)

simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, Color)]
simularCarrera carrera eventosCarrera = tablaDePosiciones (aplicarEventos carrera eventosCarrera)

aplicar :: Carrera -> (Carrera ->Carrera) -> Carrera
aplicar carrera evento = evento carrera

aplicarEventos :: Carrera -> [Carrera ->Carrera] -> Carrera
aplicarEventos = foldl aplicar

tablaDePosiciones :: Carrera -> [(Int,Color)]
tablaDePosiciones carrera = map(armarFila carrera) carrera

armarFila :: Carrera -> Auto -> (Int, Color)
armarFila carrera auto = (puesto auto carrera, colorDe auto)

colorDe :: Auto -> Color
colorDe (Auto color _ _) = color