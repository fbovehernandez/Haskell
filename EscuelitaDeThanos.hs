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
type Enemigo = Personaje
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

gemaMente :: Int -> Gemas -- (Int -> Unpersonaje ) -> UnPersonaje
gemaMente = sacarEnergia -- valor y unPersonaje

sacarEnergia :: Int -> Personaje -> Personaje
sacarEnergia valor unPersonaje = unPersonaje {energia = energia unPersonaje - valor}
 
gemaAlma :: String -> Gemas
gemaAlma habilidad  = sacarHabilidad habilidad . sacarEnergia 10 

sacarHabilidad :: String -> Personaje -> Personaje
sacarHabilidad habilidad unPersonaje = unPersonaje {habilidades = filter (/= habilidad) (habilidades unPersonaje)} -- e1

gemaEspacio :: String -> Personaje -> Personaje
gemaEspacio planeta = cambiarPlaneta planeta . sacarEnergia 20

cambiarPlaneta :: String -> Personaje -> Personaje 
cambiarPlaneta planeta unPersonaje = unPersonaje {planeta = planeta}

gemaPoder :: Personaje -> Personaje
gemaPoder unPersonaje  
    | tienePocasHabilidades unPersonaje  = (sacarEnergia (energia unPersonaje) . vaciarHabilidades) unPersonaje
    | otherwise = sacarEnergia (energia unPersonaje)  unPersonaje

vaciarHabilidades :: Personaje -> Personaje 
vaciarHabilidades unPersonaje = unPersonaje {habilidades = []} -- e2

tienePocasHabilidades :: Personaje -> Bool
tienePocasHabilidades = (>2) . length . habilidades

gemaTiempo :: Personaje -> Personaje 
gemaTiempo = reducirTiempo . sacarEnergia 50

reducirTiempo :: Personaje -> Personaje 
reducirTiempo unPersonaje = unPersonaje {edad = max (div (edad unPersonaje) 2) 18} 

gemaLoca :: Gemas -> Personaje -> Personaje
gemaLoca gema unPersonaje = gema $ gema unPersonaje 
                        --  = gema . gema

implementarGemas :: Personaje -> [Gemas] -> Personaje 
implementarGemas = foldr ($) 

-- 4

guanteleteGoma :: Guantelete
guanteleteGoma = UnGuantelete "Goma" [gemaTiempo, gemaAlma "usar Mjolnir", gemaLoca (gemaAlma "programacion en Haskell")] 

-- 5

-- UTILIZAR es la funcion implementarGemas pero con un set nuevo de gemas para ejecutar sobre el personaje, cuando yo aplico este conjunto de gemas se genera un nuevo personaje, y al que, gracias a la funcion FOLD, se le vuelve a aplicar otra gema (de la lista de gemas) y asi hasta que no queden gemas, por eso la funcion utilizada por fold es justamente el ($), que obvia los parentesis a la hora de "aplicar" algo.

-- 6




-- 7

punisher :: Personaje
punisher = UnPersonaje 1 "Punisher" "Neptuno" 50 ["Matar a Hulk, Usar la espada"]

-- No va a funcionar, ya que va a quedarse evaluar infinitamente que gema produce la perdida.  

-- va a aplicar las 3 primeras gemas de la lista sobre el personaje, sin importar que sea una lista infinita. Gracias al "lazy evaluation" , haskell analiza solo aquello que necsita
