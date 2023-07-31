data Persona = Persona {
  nombreH :: String,
  suerte :: Int,
  inteligencia :: Int,
  fuerza :: Int
} deriving (Show, Eq)

data Pocion = Pocion {
  nombrePocion :: String,
  ingredientes :: [Ingrediente]
}

type Efecto = Persona -> Persona

data Ingrediente = Ingrediente {
  nombreIngrediente :: String,
  efectos :: [Efecto]
}

nombresDeIngredientesProhibidos :: [String]
nombresDeIngredientesProhibidos = ["sangre de unicornio","veneno de basilisco", "patas de cabra", "efedrina"]

maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun _ [ x ] = x
maximoSegun  f ( x : y : xs)
  | f x > f y = maximoSegun f (x:xs)
  | otherwise = maximoSegun f (y:xs)

-- PUNTO 1
sumaDeNiveles :: Persona -> Int
sumaDeNiveles persona = fuerza persona + suerte persona + inteligencia persona 

-- diferenciaDeNiveles :: Persona -> Int
-- diferenciaDeNiveles persona = maximoNivel persona - minimoNivelPersona

maximoNivel :: Persona -> Int
maximoNivel persona = maximum [suerte persona, inteligencia persona, fuerza persona]