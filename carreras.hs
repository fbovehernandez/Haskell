-----------------  PARCIAL CARRERAS 2021 ----------------- 1

data Auto = UnAuto {
    color :: String,
    velocidad :: Int,
    distancia :: Int,
    posicion :: Int -- Puede ser cualquier (mirar 1c)
} 
type PowerUp = [Auto] -> [Auto]
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
vaPrimero auto (x:xs) = (not. estaDetras auto) x && (not . estaCerca auto) x && vaPrimero auto (auto:xs)

modificarPosicion :: Int -> Auto -> [Auto] -> Auto
modificarPosicion puestos unAuto lista = unAuto {posicion = 1+ cantidadAutosDelante unAuto lista}

cantidadAutosDelante :: Auto -> [Auto] -> Int
cantidadAutosDelante unAuto = length . filter (not . estaDetras unAuto) 

-- 2
correr :: Int -> Auto -> Auto
correr tiempo unAuto = unAuto {distancia = distancia unAuto + tiempo * velocidad unAuto }

-- b
aplicarModificador :: (Int -> Int) -> Auto -> Auto
aplicarModificador f unAuto = unAuto {velocidad = (f  . velocidad) unAuto }

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad cantidad = aplicarModificador (negate . (+ cantidad))

restar :: Int -> Int -> Int
restar valor1 valor2 = min (valor2 - valor1) 0

-- 3
afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

terremoto :: Auto ->  PowerUp 
terremoto unAuto = afectarALosQueCumplen (estaCerca unAuto) (aplicarModificador (negate . (+50)))

miguelitos :: Int -> Auto -> PowerUp -- [Auto] -> [Auto]
miguelitos cantidad unAuto = afectarALosQueCumplen (estaDetras unAuto) (aplicarModificador (negate . (+cantidad)))

estaDetras :: Auto -> Auto -> Bool
estaDetras unAuto1 unAuto2 = distancia unAuto1 > distancia unAuto2
