-----------------  PARCIAL SAMURAI 2016 ----------------- 1

data Auto = UnAuto {
    color :: String,
    velocidad :: Int,
    distancia :: Int,
    posicion :: Int -- Totalmente puesta en duda (para el punto 1c)
   -- powerUps :: [String] -- Lista de powerUps
} deriving (Eq,Show)

type Carrera = [Auto]

-- 1
estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = distanciaEntreAutos auto1 auto2 < 10  && esDiferenteAuto auto1 auto2 

esDiferenteAuto :: Auto -> Auto -> Bool
esDiferenteAuto auto1 auto2 = color auto1 /= color auto2

distanciaEntreAutos :: Auto -> Auto -> Int 
distanciaEntreAutos auto1 auto2 = abs(distancia auto1 - distancia auto2)

vaPrimero :: Auto -> Carrera -> Bool
vaPrimero _ [] = error "No hay autos corriendo"
vaPrimero auto (x:xs) = distancia auto > distancia x && (not . estaCerca auto) x && vaPrimero auto (auto:xs)

-- c No se como plantearlo... 
--puesto :: Auto -> Carrera -> Int
--puesto _ [] = error "No hay autos corriendo"
--puesto auto (x:xs) 
  --  | distanciaEntreAutos auto x > 0 = modificarPosicion --auto && puesto auto xs
   -- | otherwise = posicion auto

modificarPosicion :: Auto -> Auto
modificarPosicion unAuto = unAuto {posicion = posicion unAuto + 1 }

-- 2
correr :: Int -> Auto -> Auto
correr tiempo unAuto = unAuto {distancia = distancia unAuto + tiempo * velocidad unAuto }

-- b
