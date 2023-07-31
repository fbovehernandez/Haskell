-- !
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use section" #-}

data Ley = UnaLey {
    tema :: String,
    presupuesto :: Int,
    sectores :: [String]
} deriving (Show, Eq)

leyCannabis :: Ley 
leyCannabis = UnaLey "Uso medicinal cannabis" 5 ["Partido Cambio de todos" , "Sector Financiero"]

leyEducacion :: Ley
leyEducacion = UnaLey "educacion" 30 ["Sector Docentes universitarios" , "Partido Centro federal"]

leyTenisMesa :: Ley
leyTenisMesa = UnaLey "Profesionalizacion del tenista de mesa" 1 ["Partido Centro Federal", "Sector Liga deportistas autonomos" , "Sector Club paleta veloz"]

leyTenis :: Ley
leyTenis = UnaLey "tenis" 2 ["Sector Liga deportistas autonomos"]

-- Primera Parte 
esCompatible :: Ley -> Ley -> Bool
esCompatible ley1 ley2 = interseccionLista (sectores ley1) (sectores ley2) /= [] && interseccionLista (tema ley1) (tema ley2) /= []
   
interseccionLista :: Eq algo => [algo] -> [algo] -> [algo]
interseccionLista [] lista2 = []
interseccionLista (x:xs:xss) lista2
    | notElem x lista2 = interseccionLista (xs:xss) lista2
    | otherwise = [x] -- No se que tan bien este la recursividad 

-- Segunda Parte

type CorteSuprema = [Juez]

agenda :: [Ley] 
agenda = [leyCannabis, leyEducacion, leyTenisMesa, leyTenis] 

data Juez = UnJuez {
    criterio :: Criterio
} 

type Criterio = Ley -> Bool -- Declara constitucional

criterioOpinionPublica :: Criterio 
criterioOpinionPublica ley = elem ley agenda

criterioSectorFinanciero :: Criterio 
criterioSectorFinanciero = estaEnLosSectores "Sector Financiero" 

criterioPartidoConservador :: Criterio 
criterioPartidoConservador = estaEnLosSectores "Partido Conservador"

criterioSegunPresupuesto :: Int -> Criterio
criterioSegunPresupuesto unidades  = (< unidades) . presupuesto

estaEnLosSectores :: String -> Criterio
estaEnLosSectores sector ley = elem sector (sectores ley)

esConstitucional :: CorteSuprema -> Ley -> Bool
esConstitucional corte ley = hayMasAFavor . map (votaJuez ley) $ corte

hayMasAFavor :: [Bool] -> Bool
hayMasAFavor listaBool = length (filter id listaBool) > length (filter not listaBool)

-- Modelar Jueces
juezOpinionPublica :: Juez
juezOpinionPublica = UnJuez {criterio = criterioOpinionPublica}

juezFinanciero :: Juez
juezFinanciero = UnJuez {criterio = criterioSectorFinanciero}

juezBajoPresupuesto :: Juez
juezBajoPresupuesto = UnJuez {criterio = criterioSegunPresupuesto 10}

juezPresupuestoModerado :: Juez
juezPresupuestoModerado = UnJuez {criterio = criterioSegunPresupuesto 20}

juezConservador :: Juez
juezConservador = UnJuez {criterio = criterioPartidoConservador}

juezPositivo :: Juez
juezPositivo = UnJuez {criterio = criterioPositivo}

criterioPositivo :: Criterio
criterioPositivo _ = True

juezDeportista :: Juez
juezDeportista = UnJuez {criterio = criterioDeportista}

criterioDeportista :: Criterio 
criterioDeportista ley = elem (tema ley) deportesQueLeGustan

deportesQueLeGustan :: [String]
deportesQueLeGustan = ["tenis", "futbol"]

-- 3

votaJuez :: Ley -> (Juez -> Bool)
votaJuez ley juez = criterio juez ley

-- seriaConstitucional :: [Ley] -> CorteSuprema -> [Juez] -> [Ley] 
-- seriaConstitucional leyes corte1 juecesAD = length . filter id . map (votaJuez ley) corte1

borocotizar :: CorteSuprema -> CorteSuprema
borocotizar = map votarDeFormaContraria

votarDeFormaContraria :: Juez ->  Juez
votarDeFormaContraria unJuez = unJuez {criterio = not . criterio unJuez}

votaParecido :: Juez -> String -> [Ley] -> Bool
votaParecido juez sector = all (votoAlMismoSector sector) . filter (flip votaJuez juez) 

votoAlMismoSector :: String -> Ley -> Bool
votoAlMismoSector sector ley = elem sector (sectores ley)