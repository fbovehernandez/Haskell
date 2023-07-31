-- PRACTICA LAMDA PROP

-----------------------
-- ESTRUCTURAS
-----------------------
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use section" #-}

type Barrio = String
type Mail = String
type Busqueda = [Requisito]
type Requisito = Depto -> Bool

data Depto = Depto {
    ambientes :: Int,
    superficie :: Int,
    precio :: Int,
    barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
   mail :: Mail,
   busquedas :: [Busqueda]
}

ordenarSegun :: (a -> a -> Bool) -> [a] -> [a] -- Def
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = (ordenarSegun criterio . filter (not . criterio x)) xs ++ [x] ++ (ordenarSegun criterio . filter (criterio x)) xs

between :: Ord a => a -> a -> a -> Bool -- Def
between cotaInferior cotaSuperior valor = valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo :: [Depto]
deptosDeEjemplo = [Depto 3 80 7500 "Palermo", Depto 1 45 3500 "Villa Urquiza", Depto 2 50 5000 "Palermo", Depto 1 45 5500 "Recoleta"]

------------------- RESOLUCION ---------------
-- 1 a
mayor :: Ord b => (a -> b) -> a -> a -> Bool
mayor f n1 n2 = f n1 > f n2

menor :: Ord b => (a -> b) -> a -> a -> Bool
menor f n1 n2 = f n1 < f n2

-- b
ordenamiento = ordenarSegun (mayor length) ["1", "dos", "tres"]

-- 2
ubicadoEn :: [Barrio] -> Depto -> Bool
ubicadoEn listaBarrios = flip elem listaBarrios . barrio

cumpleRango :: Ord a => (Depto -> a) -> a -> a -> (Depto -> Bool)
cumpleRango f cotaInf cotaSup = between cotaInf cotaSup . f 

-- 3 a
cumpleLaBusqueda :: Depto -> (Busqueda -> Bool)
cumpleLaBusqueda unDepto = all ($ unDepto)

--
cumpleCondicion :: Depto -> Requisito -> Bool
cumpleCondicion unDepto requisito = requisito unDepto

-- b
buscar :: Busqueda -> (Depto -> Depto -> Bool) -> [Depto] -> [Depto]
buscar requisitos f = ordenarSegun f . filter (flip cumpleLaBusqueda requisitos) 

-- c
requisitosPuntoC :: Busqueda
requisitosPuntoC = [(<6000) . precio, between 1 2 . ambientes, flip elem ["Recoleta", "Palermo"] . barrio]

-- 4
mailsDePersonasInteresadas :: Depto -> [Persona] -> [Mail]
mailsDePersonasInteresadas depto = map mail  . filter (any (cumpleLaBusqueda depto) . busquedas)

-- PRACTICA MONSTER INC ---

type Grito = (String, Int, Bool)

onomatopeya :: Grito -> String
onomatopeya (onematopeya, _, _) = onematopeya

intensidad :: Grito -> Int
intensidad (_, intesidad, _) = intesidad

mojoLaCama :: Grito -> Bool
mojoLaCama (_, _, mojoCama) = mojoCama

-- 1
obtenerEnergia :: Grito -> Int
obtenerEnergia grito 
    | mojoLaCama grito = nivelDelTerror grito + intensidad grito ^ 2 
    | otherwise = nivelDelTerror grito * 3 + intensidad grito

nivelDelTerror :: Grito -> Int
nivelDelTerror = length . onomatopeya

-- 2
---- MODELAJE

type Monstruo = Victima -> Grito
type Victima = (String, Int, Int)

juana :: Victima
juana = ("juana", 4, 1)

nombre :: Victima -> String
nombre (nombreNinio, _, _) = nombreNinio
edad :: Victima -> Int
edad (_, edadNinio, _) = edadNinio
altura :: Victima -> Int
altura (_, _, alturaNinio) = alturaNinio

-- sullivan :: Monstruo
-- sullivan victima = (tipoGrito victima, intesidad victima, mojoCama victima)  

tipoGrito :: Victima -> String
tipoGrito victima = replicate (length (nombre victima)) 'A' ++ "GH"

----------------------------------
 

