-- Parcial Fmi
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

data Pais = Pais {
    ingresoPerCapita :: Float,
    poblacionActivaSpubl :: Float,
    poblacionActivaSpriv :: Float,
    recursosNaturales :: [String],
    deuda :: Float
}

type Receta = Pais -> Pais

prestarMillones :: Float -> Receta
prestarMillones n = cambiarDeuda (+ (1.5*n))

cambiarDeuda :: (Float -> Float) -> Pais -> Pais
cambiarDeuda f pais = pais {deuda = f $ deuda pais}

reducirPuestos :: Float -> Receta
reducirPuestos n  = reducirPerCapita . cambiarSectorPublico (+ (negate n))

cambiarSectorPublico :: (Float -> Float) -> Pais -> Pais
cambiarSectorPublico f pais = pais {poblacionActivaSpubl = f $ poblacionActivaSpubl pais}

cambiarIngresoPerCapita :: (Float -> Float) -> Pais -> Pais
cambiarIngresoPerCapita f pais = pais {ingresoPerCapita = f $ ingresoPerCapita pais}

reducirPerCapita :: Pais -> Pais
reducirPerCapita pais
    | (>100) . poblacionActivaSpubl $ pais = cambiarIngresoPerCapita (*0.8) pais
    | otherwise = cambiarIngresoPerCapita (*0.85) pais

darRecurso :: String -> Pais -> Pais
darRecurso recursoNatural = cambiarDeuda (+ negate 2) . modRecursos (filter (/= recursoNatural))

modRecursos :: ([String] -> [String]) -> Pais -> Pais
modRecursos f pais = pais {recursosNaturales = f $ recursosNaturales pais}

establecerBlindaje :: Pais -> Pais
establecerBlindaje pais = prestarMillones ((pbi pais) *0.5) . cambiarSectorPublico (+ negate 500) $ pais

pbi :: Pais -> Float
pbi pais = ingresoPerCapita pais * (poblacionActivaSpriv pais + poblacionActivaSpubl pais)

-- Punto 1
-- A TAD Data Pais
-- B
namibia :: Pais
namibia = Pais 4140 400000 650000 ["Petroleo", "Ecoturismo"] 50

-- Punto 2 -- Estrategias definidas arriba
-- Punto 3
type SuperRecetas = [Receta]

recetaSuprema :: SuperRecetas
recetaSuprema = [prestarMillones 200, darRecurso "Mineria"]

aplicarReceta :: Pais -> SuperRecetas -> Pais
aplicarReceta = foldr ($) 

--  Punto 4
puedeZafar :: [Pais] ->  [Pais]
puedeZafar = filter (elem "Petroleo" . recursosNaturales)  

totalDeuda :: [Pais] -> Float
totalDeuda = sum . map (deuda) 

-- Punto 5
estaOrdenada :: Pais -> [Receta] -> Bool
estaOrdenada pais [] = True 
estaOrdenada pais (x:xs:xss) = (pbi . x)  pais <= (pbi . xs) pais && estaOrdenada pais (xs:xss)








