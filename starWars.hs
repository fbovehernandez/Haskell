-- Parcial StarWars --- 

type Accion = Nave -> Nave
type Poder = [Accion]

data Nave = UnaNave {
    nombreDeNave :: String,
    durabilidad :: Int,
    escudo :: Int,
    ataque :: Int,
    poder :: Poder
}

-- Modelado de poderes

turbo :: Accion
turbo  = modificarAtaque 25 

modificarAtaque :: Int -> Nave -> Nave
modificarAtaque ataqueNuevo unaNave = UnaNave {ataque = ataque unaNave + ataqueNuevo} 

reparacionDeEmergencia :: Accion 
reparacionDeEmergencia = modificarDurablidad 50 . modificarAtaque (-30)

modificarDurablidad :: Int -> Nave -> Nave 
modificarDurablidad nuevaDurabilidad unaNave =  UnaNave {durabilidad = durabilidad unaNave + nuevaDurabilidad} 

-- superTurbo :: Accion
-- superTurbo = repetir 3 turbo

incrementarEscudos :: Int -> Accion
incrementarEscudos escudoNuevo unaNave = UnaNave {escudo = escudo unaNave + escudoNuevo}

----------------------------------------------------------
-- Respecto a la parte de arriba (modelado de poderes), podria haber alguna forma de mejorarlo, respecto al uso de logica repetida, ya que tanto modDurabilidad como modAtaque tienen los mismos tipos, pero en realidad modifican cosas diferentes (?
-- Tambien falta realizar el superTurbo, no estoy muy seguro de como se haria este, el resto pareceria esta todo OK! 
-----------------------------------------------------------

-- Modelado de Naves
millenniumFalcon :: Nave
millenniumFalcon = UnaNave "Millennium Falcon" 1000 500 50 [reparacionDeEmergencia, incrementarEscudos 100]

xWing :: Nave 
xWing = UnaNave "X Wing" 300 150 100 [reparacionDeEmergencia]

tieFighter :: Nave
tieFighter = UnaNave "TIE Fighter" 200 100 50 [turbo]

--naveDarthVader :: Nave
--naveDarthVader = UnaNave "DarthVader" 500 300 200 [superTurbo]

-- 2
durabilidadTotal :: Flota -> Int
durabilidadTotal = sum . map durabilidad 

-- 3

comoQueda :: Nave -> Nave -> Nave
comoQueda naveAtacada naveAtacante = nuevasNaves (activarPoder naveAtacada) (activarPoder naveAtacante) 

nuevasNaves :: Nave -> Nave -> Nave
nuevasNaves naveAtacada naveAtacante 
    | escudo naveAtacada < ataque naveAtacante = modificarNaveAtacada naveAtacada naveAtacante
    | otherwise = naveAtacada

modificarNaveAtacada :: Nave -> Nave -> Nave
modificarNaveAtacada naveAtacada  = reducirDurabilidad naveAtacada . calcularDaño naveAtacada 

reducirDurabilidad :: Nave -> Int -> Nave
reducirDurabilidad naveAtacada daño = UnaNave {durabilidad = max (durabilidad naveAtacada - daño) 0}

calcularDaño :: Nave -> Nave -> Int
calcularDaño naveAtacada naveAtacante = ataque naveAtacante  - escudo naveAtacada

activarPoder :: Nave -> Nave
activarPoder unaNave = foldr ($) unaNave (poder unaNave)
-- 4
fueraDeCombate :: Nave -> Bool
fueraDeCombate nave = durabilidad nave == 0

-- 5
type Flota = [Nave]
type Estrategia = Nave -> Bool

-- La mision : 
comoQuedaLaFlota :: Flota -> Nave -> Estrategia -> Flota
comoQuedaLaFlota flota naveAtacante estrategia = map (atacarFlota estrategia naveAtacante) flota

atacarFlota :: Estrategia -> Nave -> Nave -> Nave
atacarFlota estrategia naveAtacante naveAtacada 
    | estrategia naveAtacada = nuevasNaves naveAtacada naveAtacante 
    | otherwise = naveAtacada

-- 6 

minimizaDurabilidadTotal :: Flota -> Nave -> Estrategia -> Estrategia -> Flota
minimizaDurabilidadTotal flota naveAtacante estrategia1 estrategia2 
    | minimizaDurabilidadE1 flota naveAtacante estrategia1 estrategia2 = comoQuedaLaFlota flota naveAtacante estrategia1
    | otherwise = comoQuedaLaFlota flota naveAtacante estrategia2

minimizaDurabilidadE1 :: Flota -> Nave -> Estrategia -> Estrategia -> Bool
minimizaDurabilidadE1 flota nave estrategia1 estrategia2 = calcularDurabilidadTotal flota nave estrategia1  < calcularDurabilidadTotal flota nave estrategia2

calcularDurabilidadTotal :: Flota -> Nave -> Estrategia -> Int 
calcularDurabilidadTotal flota nave estrategia = durabilidadTotal (comoQuedaLaFlota flota nave estrategia)

-- 7
-- a) No es posible, ya que para eso necesito evaluar todas las naves, y si son infinitas, esto es imposible. 
-- b) Va a quedarse mapeando todo el tiempo, ya que la mision devuelve en si una flota, que nunca termine de mapearse.  
