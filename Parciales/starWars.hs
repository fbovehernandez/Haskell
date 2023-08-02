{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Eta reduce" #-}

  -- Parcial Star Wars !!
data Nave = UnaNave {
    nombre :: String,
    durabilidad :: Int,
    escudo :: Int,
    ataque :: Int,
    poder :: [Poder]
} 

-- funciones especiales que nos van a ayudar con el modelado de los poderes
cambiarDurabilidad :: (Int -> Int) -> Nave -> Nave
cambiarDurabilidad f nave = nave {durabilidad = max (f $ durabilidad nave) 0}

cambiarEscudo :: (Int -> Int) -> Nave -> Nave
cambiarEscudo f nave = nave {escudo = max(f $ escudo nave) 0}

cambiarAtaque :: (Int -> Int) -> Nave -> Nave
cambiarAtaque f nave = nave {ataque = max(f $ ataque nave) 0}

cambiarPoderes :: ([Poder] -> [Poder]) -> Nave -> Nave
cambiarPoderes f nave = nave {poder = f $ poder nave}

---------------------------------- RESOLUCION ----------------------------------
type Poder = (Nave -> Nave) 

tieFighter :: Nave
tieFighter = UnaNave "TIE Fighter" 200 100 50 [movimientoTurbo]

movimientoTurbo :: Poder
movimientoTurbo = cambiarAtaque (+25)

xWing :: Nave
xWing = UnaNave "X WING" 300 150 100 [reparacionEmergencia]

reparacionEmergencia :: Poder
reparacionEmergencia = cambiarDurabilidad (+50) . cambiarAtaque (+ (negate 30))

naveDeDarthVader :: Nave
naveDeDarthVader = UnaNave "Nave de Lord Vader" 500 300 200 [superTurbo]

superTurbo :: Poder
superTurbo = aplicarAccionANave (replicate 3 movimientoTurbo) . cambiarDurabilidad (+ (negate 45))

aplicarAccionANave :: [Poder] -> Nave -> Nave
aplicarAccionANave poderes nave = foldr ($) nave poderes 

millenniumFalcon :: Nave
millenniumFalcon = UnaNave "Millennium Falcon" 1000 500 50 [reparacionEmergencia, cambiarEscudo(+100)]

-- Nave Artificial 
naveInvisible :: Nave
naveInvisible = UnaNave "La Nave Invisible" 0 0 20000 [debilitarDefensasTotales]

debilitarDefensasTotales :: Poder
debilitarDefensasTotales = cambiarEscudo(*0) -- La nave invisible se infiltra en la flota enemiga y les quita todo el escudo ya que es invisible, nadie la puede ver!

-- Punto 2
type Flota = [Nave]

durabilidadTotal :: Flota -> Int
durabilidadTotal flota = sum . map durabilidad $ flota

-- Punto 3
comoQueda :: Nave -> Nave -> Nave
comoQueda naveAtacante naveAtacada = dañoRecibido (aplicarAccionANave (poder naveAtacante) naveAtacante) ((aplicarAccionANave (poder naveAtacada) naveAtacada))

dañoRecibido :: Nave -> Nave -> Nave
dañoRecibido naveAtacante naveAtacada 
    | escudo naveAtacada < ataque naveAtacante = cambiarDurabilidad(+ negate(ataque naveAtacante - escudo naveAtacada)) naveAtacada
    | otherwise = naveAtacada

-- Punto 4
fueraDeCombate :: Nave -> Bool
fueraDeCombate  = (==0) . durabilidad

-- Punto 5
type Estrategia = Nave -> Bool

flota :: [Nave]
flota = [tieFighter, xWing, naveDeDarthVader]

-- Idea mas algoritma, pero que funciona a fin. 
misionSorpresa :: Nave -> Estrategia -> Flota -> Flota
misionSorpresa naveAtacante estrategia flota =  (filter(not . estrategia) flota) ++  (map (comoQueda naveAtacante) . filter estrategia) flota

-- Funcion mas entedible y delegable.
misionSorpresa2 :: Nave -> Estrategia -> Flota -> Flota
misionSorpresa2 naveAtacante estrategia  = map(realizarAtaqueSiCumpleEstrategia naveAtacante estrategia)

realizarAtaqueSiCumpleEstrategia :: Nave -> Estrategia -> Nave -> Nave
realizarAtaqueSiCumpleEstrategia naveAtacante estrategia naveAtacada
    | estrategia naveAtacada = comoQueda naveAtacante naveAtacada
    | otherwise = naveAtacada

navesDebiles :: Estrategia
navesDebiles nave = escudo nave < 200

navePeligrosa :: Int -> Estrategia 
navePeligrosa n = (>n) . ataque

naveOutCombate :: Nave -> Estrategia 
naveOutCombate naveAtacante = fueraDeCombate . comoQueda naveAtacante   

-- Punto 6
llevarAdelanteLaMinima :: Nave -> Estrategia -> Estrategia -> Flota -> Flota
llevarAdelanteLaMinima naveAt e1 e2 flota = misionSorpresa2 naveAt (estrategiaMasEfectiva naveAt e1 e2 flota) flota

estrategiaMasEfectiva :: Nave -> Estrategia -> Estrategia -> Flota -> Estrategia
estrategiaMasEfectiva nave e1 e2 flota 
    | (durabilidadTotal . misionSorpresa2 nave e1) flota < (durabilidadTotal . misionSorpresa nave e2) flota = e1
    | otherwise = e2

-- 7
flotaInfinita :: Flota
flotaInfinita = xWing : flotaInfinita  -- Lista Infinita de xWing 

-- flotaInfinita = repeat xWing. -- Otra forma usando funciones generadoras de listas

-------- TEORIA : 
-- No es posible determinar la durabilidad total, ya que nunca voy a poder aplicar sum a la lista infinita. 
-- El resultado es una lista infinita donde queda infinitamente analizando y mapeando cada una de las naves 
-- de la flota aplicando la estrategia elegida. 
