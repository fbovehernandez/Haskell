-- Parcial Super Mario --
import Text.Show.Functions
import Data.List(genericLength)
import Data.Char(isUpper)
import GHC.ExecutionStack (Location(functionName))
--
-- * genericLength :: Num i => [a] -> i
-- -- Esta función es exactamente igual que length,
-- -- con la única diferencia que no devuelve un Int, sino un número
-- -- fácil de operar con otro número que pueden o no ser enteros.
-- --
-- -- -- ghci> length "Luigi Mario" / 2
-- -- -- error:
-- -- --     • No instance for (Fractional Int) arising from a use of ‘/’
-- -- --     • In the expression: length "Luigi Mario" / 2
-- -- --       In an equation for ‘it’: it = length "Luigi Mario" / 2
-- -- -- ghci> genericLength "Luigi Mario" / 2
-- -- -- 5.5
--
-- * isUpper :: Char -> Bool
-- -- Esta función me dice si una letra es mayúscula o no.
-- --
-- -- -- ghci> isUpper 'B'
-- -- -- True
-- -- -- ghci> isUpper 'b'
-- -- -- False
--
--------------
-- Punto 01 --
--------------

data Plomero = UnPlomero {
    nombre :: String,
    cajaHerramientas :: [Herramienta],
    reparacion :: [Reparacion],
    dinero :: Int
}

data Reparacion = UnaReparacion {
    descripcion :: String,
    requirimiento :: Plomero -> Bool
}

data Material = Hierro | Madera | Goma | Plastico deriving Eq

data Herramienta = UnaHerramienta {
    denominacion :: String,
    precio :: Int,
    materialEmpuñadura :: Material
} deriving Eq

mario :: Plomero
mario = UnPlomero "Mario" [llaveInglesa, martillo] [] 1200  

wario :: Plomero 
wario = UnPlomero "Wario" (infinitasHerramientas llaveFrancesa) [] 50 -- 0.5

infinitasHerramientas :: Herramienta -> [Herramienta]
infinitasHerramientas unaHerramienta = unaHerramienta : infinitasHerramientas (unaHerramienta {precio = 1 + precio unaHerramienta})

llaveFrancesa :: Herramienta 
llaveFrancesa = UnaHerramienta "Llave Francesa" 1 Hierro

llaveInglesa :: Herramienta 
llaveInglesa = UnaHerramienta "LLave Inglesa" 200 Hierro

martillo :: Herramienta
martillo = UnaHerramienta "Martillo" 20 Madera

--------------
-- Punto 02 --
--------------

tieneHerramienta :: Herramienta -> Plomero -> Bool
tieneHerramienta unaHerramienta unPlomero = (elem unaHerramienta) (cajaHerramientas unPlomero)

esMalvado :: Plomero -> Bool
esMalvado unPlomero = take 2 (nombre unPlomero) == "Wa"

puedeComprar :: Herramienta -> Plomero -> Bool
puedeComprar unaHerramienta unPlomero = dinero unPlomero > precio unaHerramienta
--------------
-- Punto 03 --
--------------

esBuena :: Herramienta -> Bool
esBuena unaHerramienta = tieneEsaEmpuñadura unaHerramienta Hierro && precio unaHerramienta > 10000 || (materialEmpuñadura martillo == Madera ||  materialEmpuñadura martillo == Goma)

tieneEsaEmpuñadura :: Herramienta -> Material -> Bool
tieneEsaEmpuñadura unaHerramienta unMaterial = materialEmpuñadura unaHerramienta == unMaterial

--------------
-- Punto 04 --
--------------

comprarHerramienta :: Plomero -> Herramienta -> Plomero
comprarHerramienta unPlomero unaHerramienta 
    | puedeComprar unaHerramienta unPlomero = (cambiarDinero (- precio unaHerramienta). agregarHerramienta unaHerramienta) unPlomero
    | otherwise = unPlomero

cambiarDinero :: Int -> Plomero -> Plomero
cambiarDinero preciox unPlomero = UnPlomero {dinero = dinero unPlomero + preciox}

agregarHerramienta :: Herramienta -> Plomero -> Plomero
agregarHerramienta unaHerramienta unPlomero = UnPlomero {cajaHerramientas = unaHerramienta : cajaHerramientas unPlomero}
--------------
-- Punto 05 --
--------------

filtracionDeAgua :: Herramienta ->  Reparacion --Plomero -> Bool (Queda a la espera del plomero para devolver un booleano)
filtracionDeAgua unaHerramienta  = UnaReparacion "Filtrado de agua" (tieneHerramienta unaHerramienta)

esDificil :: Reparacion -> Bool
esDificil unaReparacion = length (descripcion unaReparacion) > 100 && all isUpper (descripcion unaReparacion)

saberPresupuesto :: Reparacion -> Int
saberPresupuesto unaReparacion = genericLength (descripcion unaReparacion) * 30 
--------------
-- Punto 06 --
--------------
type Requirimiento =  Plomero -> Bool
hacerReparacion :: Reparacion -> Plomero -> Plomero
hacerReparacion unaReparacion unPlomero 
    | puedeHacerla unPlomero (requirimiento unaReparacion) = cambiarDinero (saberPresupuesto unaReparacion) . agregarReparacion unaReparacion $ unPlomero
    | otherwise  = cambiarDinero 100 unPlomero

agregarReparacion :: Reparacion -> Plomero -> Plomero
agregarReparacion unaReparacion unPlomero = UnPlomero {reparacion = unaReparacion : reparacion unPlomero}

puedeHacerla :: Plomero -> Requirimiento -> Bool 
puedeHacerla unPlomero f = f unPlomero || esMalvado unPlomero && tieneHerramienta martillo unPlomero

reparar :: Reparacion -> Plomero ->  Plomero
reparar  unaReparacion unPlomero
    | esMalvado unPlomero = (agregarHerramienta destornillador . hacerReparacion unaReparacion) unPlomero
    | not(esMalvado unPlomero) && esDificil unaReparacion = sacarHerramienta (cantidadHerramientasBuenas (cajaHerramientas unPlomero)) unPlomero
    | not(esMalvado unPlomero) && not (esDificil unaReparacion) = sacarHerramienta 1 unPlomero

cantidadHerramientasBuenas :: [Herramienta] -> Int
cantidadHerramientasBuenas  = length .  filter esBuena 

sacarHerramienta :: Int -> Plomero -> Plomero -- Siempre saca una Cantidad de Herramientas
sacarHerramienta n  unPlomero = UnPlomero {cajaHerramientas = drop n (cajaHerramientas unPlomero)}


destornillador :: Herramienta
destornillador = UnaHerramienta "Destornillador" 0 Plastico

--------------
-- Punto 07 --
--------------

jornadaLaboral :: Plomero -> [Reparacion] ->  Plomero
jornadaLaboral = foldl (flip hacerReparacion) 

jornadaLaboral2 :: [Reparacion] -> Plomero ->  Plomero
jornadaLaboral2 unasRepeticiones unPlomero = foldr hacerReparacion unPlomero unasRepeticiones


--------------
-- Punto 08 --
--------------

--quienReparoMas :: [Reparacion] ->  [Plomero] -> Plomero
--quienReparoMas unasReparaciones =  empleadoMasReparador . map (jornadaLaboral2 unasReparaciones)   

-- Donde esta empleadoMasReparador tambien podria ir empleadoMasAdinerado y MasInvertido (pero no se muy bien como hacer las funciones recursivas)
-- empleadoMasReparador :: [Plomero] -> Plomero
-- empleadoMasReparador (x:xs)
--   | length reparacion x > length reparacion xs =  ?
--   |
