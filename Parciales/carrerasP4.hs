-----------------  PARCIAL CARRERAS 2021 ----------------- 1

data Auto = UnAuto {
    color :: Color,
    velocidad :: Int,
    distancia :: Int
} deriving (Eq,Show)

data Color = Rojo | Azul | Negro | Blanco  deriving (Eq,Show)

type PowerUp = [Auto] -> [Auto] -- o carrera -> carrera
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

cantidadDelante :: Auto -> [Auto] -> Int
cantidadDelante unAuto = (+1) . length . filter (not . estaDetras unAuto) 

-- 2
correr :: Int -> Auto -> Auto
correr tiempo unAuto = unAuto {distancia = distancia unAuto + tiempo * velocidad unAuto }

-- b
aplicarModificador :: (Int -> Int) -> Auto -> Auto
aplicarModificador f unAuto = unAuto {velocidad = (f  . velocidad) unAuto }

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad cantidad = aplicarModificador (negate . (+ cantidad))

-- 3
afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

terremoto :: Auto -> PowerUp 
terremoto unAuto = afectarALosQueCumplen (estaCerca unAuto) (aplicarModificador (negate . (+50)))

miguelitos :: Int -> Auto -> PowerUp -- Auto -> [Auto] -> [Auto]
miguelitos cantidad unAuto = afectarALosQueCumplen (estaDetras unAuto) (aplicarModificador (negate . (+cantidad)))

-- jetpack es el resultado de aplicar una funcion filter (elem) que me diga si el elemento esta, modificarlo y concatenarlo a la lista de autos de donde previamente se filtro al auto que gatillo el poder (HACER!)

estaDetras :: Auto -> Auto -> Bool
estaDetras unAuto1 unAuto2 = distancia unAuto1 > distancia unAuto2

--4 
type Eventos = Carrera -> Carrera -- ([Autos] -> [Autos])

--simularCarrera :: Carrera -> [Eventos] -> [(Int, Color)]
--simularCarrera carrera unEvento = tablaFinal . aplicarEvento unEvento $ carrera 

aplicarEvento :: [Eventos] -> Carrera -> Carrera
aplicarEvento f unosAutos = foldl realizarEvento unosAutos f
-- o simplemente foldl ($) unosAutos f ... el tan hermoso signo ($) que implementa el evento en la carrera recibida ;)

realizarEvento :: Carrera -> Eventos -> Carrera
realizarEvento unaCarrera unEvento = unEvento unaCarrera

--tablaFinal :: Carrera -> [(Int, Color)]
--tablaFinal = map transformarEnTupla 

--transformarEnTupla :: Auto -> (Int,Color)
--transformarEnTupla unAuto  = (posicion ¿? unAuto , color unAuto)

-- b
correnTodos :: Int -> Eventos
correnTodos tiempo  =  map (correr tiempo)

-- terremoto :: PowerUp 
-- terremoto unAuto = afectarALosQueCumplen (estaCerca unAuto) (aplicarModificador (negate . (+50)))

-- miguelitos :: Int -> PowerUp -- [Auto] -> [Auto]
-- miguelitos cantidad unAuto = afectarALosQueCumplen (estaDetras unAuto) (aplicarModificador (negate . (+cantidad)))

--usaPowerUp :: Color -> PowerUp -> [Auto] -> [Auto]
--usaPowerUp unColor powerUp unosAutos = powerUp (unAuto unosAutos) -- powerUp no recibe un color, sino un auto, como lo aplico? 

esColor :: Color -> Auto -> Bool
esColor unColor unAuto = unColor == color unAuto 

-- Terminarrrrrrr (heavy)