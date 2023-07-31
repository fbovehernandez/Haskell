-- !
data Plomero = UnPlomero {
    nombre :: String,
    cajaHerramientas :: [Herramienta],
    reparaciones :: [Herramienta],
    dinero :: Float
} deriving (Show, Eq)

data Herramienta = UnaHerramienta {
    nombreHerramienta :: String,
    precio :: Float,
    materialEmpuñadura :: Material
} deriving (Show, Eq)

data Material = Hierro | Madera | Goma | Plastico deriving (Show, Eq)

-- PUNTO 1
mario :: Plomero
mario = UnPlomero "Mario" [UnaHerramienta "llaveInglesa" 200 Hierro, UnaHerramienta "martillo" 20 Madera] [] 1200

wario :: Plomero
wario = UnPlomero "Wario" (infinitasHerramientas (UnaHerramienta "llaveFrancesa" 1 Hierro)) [] 0.5

infinitasHerramientas :: Herramienta -> [Herramienta]
infinitasHerramientas = iterate cambiarPrecio 

cambiarPrecio :: Herramienta -> Herramienta
cambiarPrecio herramienta = herramienta {precio = precio herramienta + 1} 

-- POSIBLE OPCION RECURSIVA
-- infinitasHerramientas :: Herramienta -> [Herramienta]
-- infinitasHerramientas herramienta = herramienta : infinitasHerramientas (herramienta {precio = precio herramienta + 1})

-- PUNTO 2
tieneHerramientaConDenominacion :: String -> Plomero -> Bool
tieneHerramientaConDenominacion denominacion = any((== denominacion) . nombreHerramienta) . cajaHerramientas 

esMalvado :: Plomero -> Bool
esMalvado  = (== "wa") . take 2 . nombre

puedeComprar :: Herramienta -> Plomero -> Bool
puedeComprar unaHerramienta = (> precio unaHerramienta) . dinero 

-- PUNTO 3
-- Opcion pedorra. 
esBuena :: Herramienta -> Bool
esBuena unaHerramienta = (materialEmpuñaduraF Hierro unaHerramienta && precio unaHerramienta > 10000) || (nombreHerramienta unaHerramienta == "Martillo" && (materialEmpuñaduraF Madera unaHerramienta ||materialEmpuñaduraF Goma unaHerramienta)) 

materialEmpuñaduraF :: Material -> Herramienta -> Bool
materialEmpuñaduraF material = (== material). materialEmpuñadura  

--------------------- FORMA PATERN MATCHING
esBuena2 :: Herramienta -> Bool
esBuena2 (UnaHerramienta _ precio Hierro) = precio > 10000
esBuena2 (UnaHerramienta "martillo" precio empuñadura) = empuñadura == Goma || empuñadura == Madera

-- PUNTO 4
comprarHerramienta :: Herramienta -> Plomero -> Plomero
comprarHerramienta unaHerramienta unPlomero
    | puedeComprar unaHerramienta unPlomero = pagarPrecio (precio unaHerramienta) . agregarHerramienta unaHerramienta $ unPlomero
    | otherwise = unPlomero

pagarPrecio :: Float -> Plomero -> Plomero
pagarPrecio precio = transformarDinero (negate . (+ precio))

agregarHerramienta :: Herramienta -> Plomero -> Plomero
agregarHerramienta unaHerramienta = transformarHerramientas (unaHerramienta :)

transformarDinero :: (Float -> Float) -> Plomero -> Plomero
transformarDinero f unPlomero = unPlomero {dinero = f $ dinero unPlomero}

transformarHerramientas :: ([Herramienta] -> [Herramienta]) -> Plomero -> Plomero
transformarHerramientas f unPlomero = unPlomero {cajaHerramientas = f $ cajaHerramientas unPlomero}

-- PUNTO 5