----------------- PARCIAL ESCUELA THANOS
-- 1
data Personaje = UnPersonaje {
    edad :: Int,
    nombre :: String,
    planeta :: String,
    energia :: Int,
    habilidades :: [String]
} deriving (Eq,Show)

data Guantelete = UnGuantelete {
    material :: String,
    gemas :: [Gemas]
}

type Gemas = Personaje -> Personaje

type Universo = [Personaje]

chasquido :: Universo -> Guantelete -> Universo
chasquido unUniverso unGuantelete 
    | puedeUsarGuantelete unGuantelete && tiene6Gemas unGuantelete = take (div (length unUniverso) 2 ) unUniverso
    | otherwise = unUniverso

puedeUsarGuantelete :: Guantelete -> Bool
puedeUsarGuantelete unGuantelete = material unGuantelete == "uru"

tiene6Gemas :: Guantelete -> Bool
tiene6Gemas  = (==6) . length . gemas

-- 2

pendex :: Universo -> Bool
pendex  =  any ((< 45) . edad)
-- any (<45) . map edad
-- pendex  = (>0) . length . (filter (>45) . map edad)

sumarEnergia :: Universo -> Int
sumarEnergia  = sum . map energia . filter ((>1) . length .habilidades)

-- SEGUNDA PARTE .. 

laMente :: Int -> Gemas -- (Int -> Unpersonaje ) -> UnPersonaje
laMente = sacarEnergia -- valor y unPersonaje

sacarEnergia :: Int -> Personaje -> Personaje
sacarEnergia valor unPersonaje = unPersonaje {energia = energia unPersonaje - valor}
 
elAlma :: String -> Gemas
elAlma habilidad  = sacarHabilidad habilidad . sacarEnergia 10 

sacarHabilidad :: String -> Personaje -> Personaje
sacarHabilidad habilidad unPersonaje = unPersonaje {habilidades = filter (/= habilidad) (habilidades unPersonaje)} -- e1

elEspacio :: String -> Personaje -> Personaje
elEspacio planeta = cambiarPlaneta planeta . sacarEnergia 20

cambiarPlaneta :: String -> Personaje -> Personaje 
cambiarPlaneta planeta unPersonaje = unPersonaje {planeta = planeta}

elPoder :: Personaje -> Personaje
elPoder unPersonaje  
    | tienePocasHabilidades unPersonaje  = (sacarEnergia (energia unPersonaje) . vaciarHabilidades) unPersonaje
    | otherwise = sacarEnergia (energia unPersonaje)  unPersonaje

vaciarHabilidades :: Personaje -> Personaje 
vaciarHabilidades unPersonaje = unPersonaje {habilidades = []} -- e2

tienePocasHabilidades :: Personaje -> Bool
tienePocasHabilidades = (>2) . length . habilidades

elTiempo :: Personaje -> Personaje 
elTiempo = reducirTiempo . sacarEnergia 50

reducirTiempo :: Personaje -> Personaje 
reducirTiempo unPersonaje = unPersonaje {edad = max (div (edad unPersonaje) 2) 18} 

laGemaLoca :: Gemas -> Personaje -> Personaje
laGemaLoca gema unPersonaje = gema $ gema unPersonaje 
                        --  = gema . gema

implementarGemas :: Personaje -> [Gemas] -> Personaje 
implementarGemas = foldr ($) 