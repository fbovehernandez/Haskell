-- Parcial Pokemon (rehecho con algunas cositas que vi en la resolucion que se podrian cambiar para mejor!)

data Investigador = UnInvestigador {
    nombre :: String,
    experiencia :: Float, 
    compañero :: Pokemon,
    mochila :: [Item], 
    pokemones :: [Pokemon]
}

data Rango = Cielo | Estrella | Constelacion | Galaxia

data Pokemon = UnPokemon {
    apodo :: String,
    descripcion :: String,
    nivel :: Int,
    puntosDeInvestigacion :: Float
}

esAlfa :: Pokemon -> Bool
esAlfa unPokemon = take 4 (apodo unPokemon) == "alfa"  

-- Punto 1

akari :: Investigador
akari = UnInvestigador "Akari" (2000 - 501) oshawott [] [oshawott] 

oshawott :: Pokemon
oshawott = UnPokemon "Oshawott" "una nutria que pelea con el caparazón de su pecho" 5 3

-- Punto 2
saberRango :: Investigador -> Rango 
saberRango unInvestigador 
    | experiencia unInvestigador > 2000 = Galaxia
    | experiencia unInvestigador > 500 = Constelacion
    | experiencia unInvestigador > 100 = Estrella
    | otherwise = Cielo

-- Punto 3 
type Actividad = Investigador -> Investigador
type Item = (Float -> Float)
-- Va de float en float porque asi puedo aplicar bien los items abajo. 

--------------------------------------------
obtenerItem :: Item -> ([Item] -> [Item]) -> Actividad
obtenerItem unItem f = modificarMochila (unItem :) . modExperiencia unItem

modificarMochila :: ([Item] -> [Item]) -> Investigador -> Investigador
modificarMochila f unInvestigador = UnInvestigador {mochila = f (mochila unInvestigador)}

modExperiencia :: Item -> Investigador -> Investigador
modExperiencia f unInvestigador = UnInvestigador {experiencia = f (experiencia unInvestigador)}

bayas :: Item
bayas = (^2). (+1)

apicorns :: Item
apicorns = (*2) --1.5 

guijarros :: Item
guijarros = (+2)

fragmentosDeHierro :: Float -> Item
fragmentosDeHierro cantidad = (/cantidad)
----------------------------------------------

admirarPaisaje :: Actividad
admirarPaisaje = modExperiencia (negate . (+5)) . modificarMochila (drop 3)

capturarPokemon :: Pokemon -> Actividad
capturarPokemon unPokemon  = modExperiencia (+ puntosDeInvestigacion unPokemon) . agregarLista unPokemon . nuevoCompañero unPokemon

agregarLista :: Pokemon -> Investigador -> Investigador
agregarLista unPokemon unInvestigador = UnInvestigador {pokemones = unPokemon : pokemones unInvestigador}

nuevoCompañero :: Pokemon -> Investigador -> Investigador
nuevoCompañero unPokemon unInvestigador 
    | puntosDeInvestigacion unPokemon > 20 = UnInvestigador {compañero = unPokemon}
    | otherwise = unInvestigador

combatirUnPokemon :: Pokemon -> Actividad 
combatirUnPokemon unPokemon unInvestigador 
    | leGana unPokemon (compañero unInvestigador) = modExperiencia (+ (0.5 * puntosDeInvestigacion unPokemon)) unInvestigador
    | otherwise = unInvestigador

leGana :: Pokemon -> Pokemon -> Bool
leGana pokemon1 pokemon2 = nivel pokemon2 > nivel pokemon1

-- 4
type Expedicion = [Actividad]

-- Esta funcion podria ser usada para todos los reportes. 
reportes :: (Investigador -> a) -> (Investigador -> Bool) -> Expedicion -> [Investigador] -> [a]
reportes f condicion unaExpedicion  = map f . filter condicion . map (realizarExpedicion unaExpedicion) 

reporte1 :: Expedicion -> [Investigador] -> [String]
reporte1 unaExpedicion = map nombre . filter ((>3). length . esAlfa2 . pokemones) .  map (realizarExpedicion unaExpedicion) 

reporte2 :: Expedicion -> [Investigador] -> [Float]
reporte2 unaExpedicion = map experiencia . filter tieneRangoGalaxia .  map (realizarExpedicion unaExpedicion) 

reporte3 :: Expedicion -> [Investigador] -> [Pokemon] 
reporte3 unaExpedicion = map compañero . filter ((>10) .nivel . compañero) .  map (realizarExpedicion unaExpedicion) 

--reporte 4 (Hacer!)

-- Realizar Expedicion
realizarExpedicion :: Expedicion -> Investigador -> Investigador
realizarExpedicion unaExpedicion unInvestigador = foldl (flip ($)) unInvestigador unaExpedicion 

tieneRangoGalaxia :: Investigador -> Bool
tieneRangoGalaxia unInvestigador = experiencia unInvestigador > 2000

esAlfa2 :: [Pokemon] -> [Pokemon]
esAlfa2 = filter esAlfa
