-- Parcial Simpsons --
-- 1
data Personaje = UnPersonaje {
    nombre :: String,
    dinero :: Int,
    felicidad :: Int
}

type Actividad = Personaje -> Personaje

irEscuelaElemental:: Personaje -> Personaje
irEscuelaElemental unPersonaje 
    | nombre unPersonaje == "lisa" = aumentoFelicidad 20 unPersonaje
    | otherwise = aumentoFelicidad (-20) unPersonaje

comerDona :: Int -> Personaje -> Personaje 
comerDona cantidadDonas = aumentoFelicidad (cantidadDonas *10) . gastarDinero (-10)

irAtrabajar :: String -> Personaje -> Personaje
irAtrabajar lugar = gastarDinero (length lugar) 

irComoDirector = irAtrabajar "escuela elemental"

aumentoFelicidad :: Int -> Personaje -> Personaje
aumentoFelicidad n unPersonaje = UnPersonaje {felicidad =  max (felicidad unPersonaje + n) 0}

gastarDinero :: Int -> Personaje -> Personaje
gastarDinero cantidad unPersonaje = UnPersonaje {dinero = dinero unPersonaje + cantidad}

 