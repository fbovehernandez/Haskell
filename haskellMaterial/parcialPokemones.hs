--
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use section" #-}

data Investigador = Investigador {
    nombre :: String,
    experiencia :: Float,
    pokemonCompañero :: Pokemon,
    mochila :: [Item], -- MOMENTANEAMENNTE
    pokemonesCapturados :: [Pokemon]
} 

data Pokemon = Pokemon {
    apodo :: String,
    desc :: String,
    nivel :: Int,
    puntosQueOtorga :: Float
} deriving (Eq, Show)

data Rango = Cielo | Estrella | Constelacion | Galaxia deriving (Eq, Show)

pasarDeRango :: Investigador -> Rango
pasarDeRango investigador
    | experiencia investigador > 2000 = Galaxia
    | experiencia investigador > 500 = Constelacion
    | experiencia investigador > 100 = Estrella 
    | otherwise = Cielo

tieneTodasLasVocales :: String -> Bool -- uso del flip para generar el match. 
tieneTodasLasVocales apodo = all (flip elem apodo) "aeiou"

esAlfa :: Pokemon -> Bool
esAlfa pokemon = take 4 (apodo pokemon) == "alfa" || tieneTodasLasVocales (apodo pokemon)

-- PUNTO 1

akari :: Investigador
akari = Investigador "Akari" 1499 oshawott [] [oshawott]

oshawott :: Pokemon
oshawott = Pokemon "oshawott" "una nutria que pelea con el caparazón de su pecho" 5 3

-- PUNTO 2 HECHO ARRIBA
-- PUNTO 3 ACTIVIDADES

type Actividad = Investigador -> Investigador
type Item = (Float -> Float)

obtenerUnItem :: Item -> Actividad
obtenerUnItem item = modMochila (item :) . modExperiencia item

modMochila :: ([Item] -> [Item]) -> Investigador -> Investigador
modMochila f investigador = investigador {mochila = f (mochila investigador)}

-- Opcion valida pero menos generica y funcional
agregarEnMochila :: Item -> Investigador -> Investigador
agregarEnMochila unItem investigador = investigador {mochila = unItem : mochila investigador}

modExperiencia :: (Float -> Float) -> Investigador -> Investigador
modExperiencia f investigador = investigador {experiencia = f (experiencia investigador)}

-- MODELADO DE ITEMS
bayas :: Item
bayas = (^2) . (+1) 

apricorns :: Item
apricorns = (*1.5) -- Fractional Int

guijarros :: Item
guijarros = (+2)

fragmentosDeHierro :: Float -> Item
fragmentosDeHierro n = (/ n)

admirarElPaisaje :: Actividad
admirarElPaisaje  = modMochila (drop 3) . modExperiencia (* 0.95)

capturarUnPokemon :: Pokemon -> Actividad
capturarUnPokemon pokemon = pasaASerAmigo pokemon . agregarALista pokemon . modExperiencia (+ (puntosQueOtorga pokemon))

pasaASerAmigo :: Pokemon -> Investigador -> Investigador
pasaASerAmigo pokemon investigador 
    | puntosQueOtorga pokemon > 20 = investigador {pokemonCompañero = pokemon}
    | otherwise = investigador

agregarALista :: Pokemon -> Investigador -> Investigador
agregarALista pokemon investigador = investigador {pokemonesCapturados = pokemon : pokemonesCapturados investigador}

combatirPokemon :: Pokemon -> Investigador -> Investigador 
combatirPokemon pokemon investigador 
    | leGana pokemon (pokemonCompañero investigador) = modExperiencia (+ (puntosQueOtorga pokemon * 0.5))  investigador

leGana :: Pokemon -> Pokemon -> Bool
leGana pokemon pokemonCompi = nivel pokemonCompi > nivel pokemon

-- PUNTO 4
type Expedicion = [Actividad] 

reporteGenerico :: [Investigador] -> Expedicion -> (Investigador -> Bool) -> (Investigador -> algo) -> [algo] 
reporteGenerico investigadores expedicion condicion transformacion = (map (transformacion) . filter condicion . map(flip realizarExpedicion expedicion)) investigadores

realizarExpedicion :: Investigador -> Expedicion -> Investigador
realizarExpedicion = foldr ($) -- Investigador -> Expedicion

-- reporte1 :: Investigadores -> Expedicion -> masDe3AlfasReporte1 -> nombre -> [String]

-- reporte2 :: Investigadores -> Expedicion -> esGalaxiaReporte2 -> experiencia -> [Int]

-- MODELADO DE CONDICIONES PARA APLICACION (FALTAN 2 CONDICIONES REPORTE 3 Y 4)
esGalaxiaReporte2 :: Investigador -> Bool
esGalaxiaReporte2 = (== Galaxia) . pasarDeRango

masDe3AlfasReporte1 :: Investigador -> Bool
masDe3AlfasReporte1 = (>3) . length . esAlfa2

esAlfa2 :: Investigador -> [Pokemon]
esAlfa2 investigador = filter (esAlfa) (pokemonesCapturados investigador)

-- 5 -> Concepto de ""lazy evaluation"" , los reportes que necesitan el uso de la lista de pokemones capturados nunca van a poder realizarse ya que no terminaria nunca de evaluar la lista por la condicion aplicada segun el reporte. En este caso, el 2 y 4 no los podria aplicar. El 3 si, ya que no es una lista, sino los pokemones compañeros

