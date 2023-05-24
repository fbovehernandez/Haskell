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

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion unaExcursion unTurista = modificarStress (-div (stress unTurista)  10) . unaExcursion $ unTurista

-- deltaSegun :: (a -> Int) -> a -> a -> Int
-- deltaSegun f algo1 algo2 = f algo1 - f algo2

type Indice = Turista -> Int -- Cansancion y stress

deltaExcursionSegun :: Indice -> Turista -> Excursion -> Int
deltaExcursionSegun indice unTurista unaExcursion = indice unTurista - indice (hacerExcursion unaExcursion unTurista)

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
