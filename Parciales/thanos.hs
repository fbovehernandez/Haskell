-- !
data Personaje = Personaje {
    nombre :: String,
    planetaQueVive :: String,
    edad :: Int,
    energia :: Int,
    habilidades :: [String]
} deriving (Show, Eq)

type Gema = Personaje -> Personaje 
type Universo = [Personaje]

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
} 

atenea :: Personaje
atenea = Personaje "Juan" "planeta" 19 20 ["hijaDeThanos"]

-- Punto 1
chasquidoDeThanos :: Guantelete -> Universo -> Universo
chasquidoDeThanos guantelete universo 
    | puedeChasquear guantelete = take (div (length universo) 2) . reverse $ universo
    | otherwise = universo

puedeChasquear :: Guantelete -> Bool
puedeChasquear guantelete = material guantelete == "uru" && length (gemas guantelete) == 6 

-- Punto 2
esAptoParaPendex :: Universo -> Bool
esAptoParaPendex = any ((<45) . edad)

energiaTotal :: Universo -> Int
energiaTotal  = sum . map energia . filter ((>1) . length. habilidades)

-- Punto 3
laMente :: Int -> Gema
laMente n = modEnergia (+ negate n)

elAlma :: String -> Gema 
elAlma habilidad = modEnergia (+ negate 10) . cambiarHabilidad (filter (/=habilidad))

elEspacio :: String -> Gema
elEspacio planeta = modEnergia (+ negate 20) . cambiarPlaneta planeta

elPoder :: Gema
elPoder personaje 
    | (<= 2) . length . habilidades $ personaje = modEnergia (*0) . vaciarLista $ personaje 
    | otherwise = personaje

elTiempo :: Gema
elTiempo = reducirEdad . modEnergia ( + negate 50)

gemaLoca :: Gema -> Personaje -> Personaje
gemaLoca gema = gema . gema   

reducirEdad :: Personaje -> Personaje
reducirEdad personaje = personaje {edad =  max (div (edad personaje) 2) 18}  

cambiarPlaneta :: String -> Personaje -> Personaje
cambiarPlaneta planeta personaje = personaje {planetaQueVive = planeta}

cambiarHabilidad :: ([String] -> [String]) -> Personaje -> Personaje
cambiarHabilidad f personaje = personaje {habilidades = f $ habilidades personaje}

vaciarLista :: Personaje -> Personaje
vaciarLista personaje = personaje {habilidades = []}

modEnergia :: (Int -> Int) -> Personaje -> Personaje
modEnergia f personaje = personaje {energia = f $ energia personaje}

-- Punto 4
guantelete :: Guantelete
guantelete = Guantelete "goma" [elTiempo, elAlma "usar mjolnir", gemaLoca (elAlma "programacionEnHaskell")]

-- Punto 5
utilizar :: [Gema] -> Personaje -> Personaje
utilizar listaGemas personaje = foldr ($) personaje listaGemas

-- Punto 5 plus (Hacer una funcion agregarHabilidades, que dada una lista de habilidades y un personaje las añade a las habilidades de un personaje)
agregarHabilidades :: [String] -> Personaje -> Personaje
agregarHabilidades listaHabilidades personaje = foldr agregarHabilidad personaje listaHabilidades

agregarHabilidad :: String -> Personaje -> Personaje
agregarHabilidad habilidad personaje = personaje {habilidades = habilidad : habilidades personaje}

-- Punto 6 recursivo 
gemasMasFuerteConGuantelete :: Guantelete -> Personaje -> Gema
gemasMasFuerteConGuantelete guantelete = gemaMasPoderosa (gemas guantelete) 

gemaMasPoderosa :: [Gema] -> Personaje -> Gema
gemaMasPoderosa [gema] _ = gema -- No olvidar caso base.
gemaMasPoderosa (x:y:xs) personaje 
    | (energia . x) personaje < (energia . y) personaje = gemaMasPoderosa (x:xs) personaje
    | otherwise = gemaMasPoderosa (y:xs) personaje

-- Punto 7 Teorico

-- La primera nunca devolveria la gema, ya que nunca termina de comparar, porque son infinitas las gemas de ese guantelete. Por otro lado, la segunda si, ya que yo puedo trabajar con lista infinitas tomando algunos elementos de la misma como hace el take, aunque no podria hacer consultas sobre la misma, como por ejemplo la cantidad de elementos que tiene. el concepto teorico podria ser lazy evaluation, no estoy del seguro si es esto, sistema de tipos, o alguna cosa mas. 
