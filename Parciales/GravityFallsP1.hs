----------------------- Gravity Falls sin recursividad --------

data Personaje = UnPersonaje {
    edad :: Int,
    items :: [String],
    experiencia :: Int
}

data Criatura = UnaCriatura {
    peligrosidad :: Int, 
    debilidad :: [Debilidad]
}

type Debilidad = Personaje -> Bool

siempreDetras :: Criatura
siempreDetras = UnaCriatura 0 []

gnomos :: Int -> Criatura
gnomos cantidad = UnaCriatura (2 ^ cantidad) [tieneItem "soplador de hojas", tieneItem "asesino de gnomos"]

tieneItem :: String -> Debilidad
tieneItem item = elem item . items 

fantasma :: Int -> Debilidad -> Criatura
fantasma n debilidad = UnaCriatura (n * 20) [debilidad]

cumpleDebilidades :: Criatura -> Personaje -> Bool
cumpleDebilidades criatura unPersonaje  = all (cumpleUnaDebilidad unPersonaje) (debilidad criatura)

cumpleUnaDebilidad :: Personaje -> Debilidad -> Bool
cumpleUnaDebilidad unPersonaje unaDebilidad = unaDebilidad unPersonaje  

-- 2 
enfrentarCriatura :: Criatura -> Personaje -> Personaje
enfrentarCriatura criatura unPersonaje
    | cumpleDebilidades criatura unPersonaje = cambiarExperiencia (peligrosidad criatura) unPersonaje
    | not (cumpleDebilidades criatura unPersonaje) = cambiarExperiencia 1 unPersonaje

cambiarExperiencia :: Int -> Personaje -> Personaje
cambiarExperiencia cantidad unPersonaje = unPersonaje {experiencia = experiencia unPersonaje + cantidad}

--3 
enfrentaVariasCriaturas :: Personaje -> [Criatura] -> Personaje
enfrentaVariasCriaturas  = foldr enfrentarCriatura -- Queda a la espera de lo demas parametros, lo que hace es aplicar al personaje una serie de criaturas que le varian la exp. 

