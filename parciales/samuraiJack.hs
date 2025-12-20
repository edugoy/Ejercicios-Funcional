-- DATOS
data Elemento = UnElemento { tipo :: String,
ataque :: (Personaje-> Personaje),
 defensa :: (Personaje-> Personaje)}

data Personaje = UnPersonaje { nombre :: String,
salud :: Float,
elementos :: [Elemento],
anioPresente :: Int }

-- Punto 1
mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio anio personaje = personaje { anioPresente = anio}

meditar :: Personaje -> Personaje
meditar personaje = personaje { salud = salud personaje * 1.5}

causarDanio :: Float -> Personaje -> Personaje
causarDanio danio personaje = personaje { salud = max 0 (salud personaje - danio)}

--Punto 2
esMalvado :: Personaje -> Bool
esMalvado personaje = any esDeTipoMaldad (elementos personaje)

esDeTipoMaldad :: Elemento -> Bool
esDeTipoMaldad elemento = tipo elemento == "Maldad"

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce personaje elemento = salud personaje - salud (ataque elemento personaje)

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje enemigos = filter (puedenMatarA personaje) enemigos

puedenMatarA :: Personaje -> Personaje -> Bool
puedenMatarA personaje enemigo = any (elementoPuedeMatarA personaje) (elementos enemigo)

danioRecibido :: Personaje -> Elemento -> Float
danioRecibido personaje elemento = danioQueProduce personaje elemento

elementoPuedeMatarA :: Personaje -> Elemento -> Bool
elementoPuedeMatarA personaje elemento = danioRecibido personaje elemento >= salud personaje