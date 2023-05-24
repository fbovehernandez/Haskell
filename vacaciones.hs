----------------- Parcial Vacaciones
data Turista = UnTurista {
    cansancio :: Int,
    stress :: Int,
    viajaSolo :: Bool,
    idiomas :: [Idiomas]
} deriving (Eq,Show)

type Idiomas = String
type Excursion = Turista -> Turista

data Marea = Fuerte | Moderada | Tranquila deriving (Eq,Show)

irALaPlaya :: Excursion
irALaPlaya unTurista
    | viajaSolo unTurista         = aumentarCansancio (-5) unTurista
    | (not . viajaSolo) unTurista = modificarStress (-1) unTurista

apreciarElemento :: String -> Excursion
apreciarElemento elemento  = modificarStress (- length elemento) 

salirAHablarIdioma :: String -> Excursion 
salirAHablarIdioma idioma unTurista = unTurista {idiomas = idioma : idiomas unTurista, viajaSolo = False}

caminar :: Int -> Excursion
caminar minutos  = aumentarCansancio (intensidadCaminata minutos) . modificarStress (- intensidadCaminata minutos) 

intensidadCaminata :: Int -> Int -- Podria usar directamente (div minutos 4)
intensidadCaminata cantMinutos = div cantMinutos 4

paseoEnBarco :: Marea -> Excursion
paseoEnBarco marea unTurista 
    | marea == Fuerte    = modificarStress 6 . aumentarCansancio 10 $ unTurista
    | marea == Moderada  = unTurista
    | marea == Tranquila = caminar 10 . apreciarElemento "Mar" . salirAHablarIdioma "Aleman" $ unTurista

modificarStress :: Int -> Turista -> Turista
modificarStress cantidad unTurista = unTurista {stress = stress unTurista + cantidad} 


aumentarCansancio :: Int -> Turista -> Turista
aumentarCansancio unidades unTurista = unTurista {cansancio = cansancio unTurista + unidades} 

ana :: Turista
ana = UnTurista 0 21 False ["Espaniol"]

beto :: Turista
beto = UnTurista 15 15 True ["Aleman"]

cathi :: Turista
cathi= UnTurista 15 15 True ["Aleman","Catalan"]

-- 2 

hacerExcursion :: Turista -> Excursion -> Turista
hacerExcursion unTurista unaExcursion = modificarStress (-div (stress unTurista)  10) . unaExcursion $ unTurista

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

type Indice = Turista -> Int -- Cansancion y stress

deltaExcursionSegun :: Indice -> Turista -> Excursion -> Int
deltaExcursionSegun indice unTurista unaExcursion = indice unTurista - indice (hacerExcursion unTurista unaExcursion)

esEducativa :: Turista -> Excursion -> Bool
esEducativa unTurista unaExcursion  = deltaExcursionSegun cantidadIdioma unTurista unaExcursion /= 0

cantidadIdioma :: Turista -> Int
cantidadIdioma = length . idiomas -- a la espera de unTurista

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion] 
excursionesDesestresantes unTurista = filter ((>3) . deltaExcursionSegun stress unTurista)

-- excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion] 
-- excursionesDesestresantes unTurista = filter (esDesestrasente unTurista)

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante unTurista unaExcursion = deltaExcursionSegun stress unTurista unaExcursion >= 3

-- 3
type Tour = [Excursion] -- [Turista -> Turista]

completo :: Tour
completo = [caminar 20, apreciarElemento "cascada", caminar 40, irALaPlaya, salirAHablarIdioma "melmacquiano"]

ladoB :: Excursion -> Tour
ladoB excursion = [paseoEnBarco Tranquila, excursion, caminar 120]

islaVecina :: Marea -> Tour
islaVecina unaMarea 
    | unaMarea == Fuerte = [paseoEnBarco unaMarea, apreciarElemento "lago", paseoEnBarco unaMarea]
    | otherwise = [paseoEnBarco unaMarea, irALaPlaya, paseoEnBarco unaMarea]
    
hacerTour :: Turista -> Tour -> Turista
hacerTour unTurista unTour = foldr (flip hacerExcursion)(modificarStress (length unTour) unTurista) unTour 

-- hacerTour :: Turista -> Tour -> Turista
-- hacerTour unTurista unTour = foldr ($) (modificarStress (length unTour) unTurista) unTour -- Esto no estaria mal, pero en si no estaria reduciendo el stess por excursion aplicada

esConvincente :: Turista -> [Tour] -> Bool
esConvincente unTurista = any (tourConvincente unTurista)
    
tourConvincente :: Turista -> Tour -> Bool -- Tour = [Excursion]
tourConvincente unTurista = any (tedejaAcompaniado unTurista) . excursionesDesestresantes unTurista

tedejaAcompaniado :: Turista -> Excursion -> Bool --No termino de enteder esta funcion (resolucion)
tedejaAcompaniado unTurista = not . viajaSolo . hacerExcursion unTurista 

efectividad :: Tour -> [Turista] -> Int
efectividad unTour = sum . map (espiritualidad unTour) . filter ((flip tourConvincente) unTour)

-- map (a -> b) -> [a] -> [b]

espiritualidad ::  Tour -> Turista -> Int
espiritualidad unTour  = negate . deltaRutina unTour

deltaRutina :: Tour -> Turista -> Int
deltaRutina tour turista = deltaSegun nivelDeRutina (hacerTour turista tour) turista

nivelDeRutina :: Turista -> Int
nivelDeRutina turista = cansancio turista + stress turista

-- deltaSegun :: (a -> Int) -> a -> a -> Int
-- deltaSegun f algo1 algo2 = f algo1 - f algo2

-- 4