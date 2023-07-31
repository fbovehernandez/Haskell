-- Recuperatorio series 
data Serie = UnaSerie {
    nombreSerie :: String,
    actores :: [Actor],
    presupuesto :: Int,
    temporadas :: Int,
    rating :: Float,
    estaCancelada :: Bool
} deriving (Eq,Show)

data Actor = UnActor {
    nombreActor :: String,
    sueldo :: Int,
    restricciones :: [Bool]
} deriving (Eq,Show)

--1 A
estaRoja :: Serie -> Bool
estaRoja unaSerie = presupuesto unaSerie < (sum . map sueldo)(actores unaSerie)

--1 B
esProblematica :: Serie -> Bool
esProblematica unaSerie = (>3) . length . filter ((>1) . length . restricciones) $ actores unaSerie 

-- 2
type Productor = Serie -> Serie

conFavoritismos :: [Actor] -> Productor
conFavoritismos actores = agregarFavoritos actores . sacar2Primeros 

agregarFavoritos :: [Actor] -> Serie -> Serie
agregarFavoritos actoresNuevos unaSerie = unaSerie {actores = actoresNuevos ++ actores unaSerie}

sacar2Primeros :: Serie -> Serie
sacar2Primeros unaSerie = unaSerie {actores = drop 2 (actores unaSerie)}

-- una Forma quiza mas sencilla pero sin la aplicacion de composicion, que lo hace mas bonito
-- agregarFavoritos :: [Actor] -> Serie -> Serie
-- agregarFavoritos actoresNuevos unaSerie = unaSerie {actores = actoresNuevos ++ drop 2 (actores unaSerie)}

-- B
timBurton :: Productor
timBurton = conFavoritismos [jonnyDepp, helenaBonham]

jonnyDepp :: Actor
jonnyDepp = UnActor "jonny Depp" 20 []

helenaBonham :: Actor
helenaBonham = UnActor "helena Bonham" 15 []

-- D
estireitor :: Productor
estireitor unaSerie = unaSerie {temporadas = temporadas unaSerie * 2}
-- E
desespereitor :: [Productor] -> Serie -> Serie
desespereitor productor unaSerie = foldr ($) unaSerie productor --CHEQUEAR

-- F
canceleitor :: Float -> Productor
canceleitor rating unaSerie 
    | estaRoja unaSerie || ratingMenor rating unaSerie = unaSerie {estaCancelada = True}
    | otherwise = unaSerie

ratingMenor :: Float -> Serie -> Bool
ratingMenor minimo unaSerie = minimo > rating unaSerie

-- 3 
type Bienestar = Serie -> Int
bienestarLongitud :: Bienestar
bienestarLongitud unaSerie
    | cantidadTemporadas unaSerie = 5
    | otherwise = (*2) (10 - cantidadActoresReestricciones unaSerie)   

bienestarReparto :: Bienestar
bienestarReparto unaSerie
  | cantidadDeActores unaSerie = 3
  | otherwise = 10 - min (cantidadActoresReestricciones unaSerie) 2

bienestar :: Serie -> Int
bienestar serie
    | estaCancelada serie = 0
    | otherwise = bienestarLongitud serie + bienestarReparto serie

cantidadActoresReestricciones :: Serie -> Int
cantidadActoresReestricciones unaSerie = length . filter (not . null . restricciones) $ actores unaSerie 

cantidadTemporadas :: Serie -> Bool
cantidadTemporadas unaSerie = temporadas unaSerie > 4

cantidadDeActores :: Serie -> Bool
cantidadDeActores = (< 10) . length . actores 

-- 4
--aplicarSegunEfectividad :: [Productor] -> [Serie] -> [Serie]
--aplicarSegunEfectividad productores series = ???? 
-- funcion recursiva que compare bienestar con los demas productores... (resolucion)

-- 5
-- si (lazy evaluation) y si, ya que puedo operar con una lista infinita sacando los primeros 2 y contatenando adelente los otros 2, obviamente esta lista no las podrias nunca recorrer, o ver.

-- 6
esControvertida :: Serie -> Bool
esControvertida unaSerie = not $ cobraMas (actores unaSerie)

cobraMas :: [Actor] -> Bool
cobraMas [] = True
cobraMas (x:xs:xss) = sueldo x > sueldo xs && cobraMas (xs:xss)

-- o/ podria hacer lo siguiente (sacado de la resolucion)
-- cobraMasQueElSiguiente (x:xs) = (sueldo x) > (sueldo $ head xs) 

-- 7
funcionLoca :: (Integral b, Foldable t) => (Int -> b) -> (a1 -> t a2) -> [a1] -> [Int]
funcionLoca x y = filter (even.x) . map (length.y)

-- Deduccion: 