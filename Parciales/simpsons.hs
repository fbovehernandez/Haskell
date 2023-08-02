
-- 1
data Personaje = UnPersonaje {
    nombre :: String,
    dinero :: Int,
    felicidad :: Int
} deriving Show

type Actividad = Personaje -> Personaje

irEscuelaElemental:: Personaje -> Personaje
irEscuelaElemental unPersonaje 
    | nombre unPersonaje == "lisa" = aumentoFelicidad 20 unPersonaje
    | otherwise = aumentoFelicidad (-20) unPersonaje

comerDona :: Int -> Personaje -> Personaje 
comerDona cantidadDonas = aumentoFelicidad (cantidadDonas *10) . gastarDinero (-10)

irAtrabajar :: String -> Personaje -> Personaje
irAtrabajar lugar = gastarDinero (length lugar) 

irComoDirector :: Personaje -> Personaje
irComoDirector = irAtrabajar "escuela elemental" . aumentoFelicidad (-20)

aumentoFelicidad :: Int -> Personaje -> Personaje
aumentoFelicidad n unPersonaje = unPersonaje {felicidad =  max (felicidad unPersonaje + n) 0}

gastarDinero :: Int -> Personaje -> Personaje
gastarDinero cantidad unPersonaje = unPersonaje {dinero = dinero unPersonaje + cantidad}

-- Modelado de personajes
homero :: Personaje
homero = UnPersonaje "Homero Simpson" 50 100
skinner :: Personaje
skinner = UnPersonaje "Skinner" 10 500
lisa :: Personaje
lisa = UnPersonaje "Lisa Simpson" 100 0

-- comerDona 3 homero 
-- homero = UnPersonaje "Homero Simpson" 20 130

-- Punto 2 

type Logro = Personaje -> Bool

serMillonario :: Logro
serMillonario unPersonaje = dinero unPersonaje > dinero burns

burns :: Personaje
burns = UnPersonaje "Burns" 50 1000000

-- Completar