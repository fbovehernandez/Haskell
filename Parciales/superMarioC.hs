-- Parcial SuperMario !!
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use section" #-}
import Text.Show.Functions ()
import Data.List(genericLength)
import Data.Char(isUpper)

-- isUpper -- Me dice si el caracter esta en MAYUS.
-- Generic Length -- Saber el length pero con Floats. 

data Plomero = UnPlomero {
    nombre :: String,
    cajaHerramientas :: [Herramienta],
    reparaciones :: [Reparacion],
    dinero :: Float
} deriving Show

data Herramienta = UnaHerramienta {
    denominacion :: String,
    material :: Material,
    precio :: Float
} deriving (Eq, Show)

data Material = Goma | Madera | Hierro | Plastico deriving (Eq, Show) -- Modelado de material x constructores

-- Funciones auxiliares 
cambiarPrecio :: (Float -> Float) -> Herramienta -> Herramienta
cambiarPrecio f herramienta = herramienta {precio = f $ precio herramienta}

cambiarReparaciones :: ([Reparacion] -> [Reparacion]) -> Plomero -> Plomero
cambiarReparaciones f plomero = plomero {reparaciones = f $ reparaciones plomero}

cambiarHerramienta :: ([Herramienta] -> [Herramienta]) -> Plomero -> Plomero
cambiarHerramienta f plomero = plomero {cajaHerramientas = f $ cajaHerramientas plomero}

cambiarDinero :: (Float -> Float) -> Plomero -> Plomero
cambiarDinero f plomero = plomero {dinero = f $ dinero plomero}

-- Punto 1
mario :: Plomero
mario = UnPlomero "Mario" [UnaHerramienta "LlaveInglesa" Hierro 200, UnaHerramienta "Martillo" Madera 20] [] 1200

wario :: Plomero
wario = UnPlomero "Wario" (iterate (cambiarPrecio (+1)) llaveFrancesa) [] 0.5 -- Definicion como Float

-- Modelado de herramienta para generar la lista. 
llaveFrancesa :: Herramienta
llaveFrancesa = UnaHerramienta "LlaveFrancesa" Hierro 1

-- Punto 2

-- Saber si un plomero:
tieneHerramienta :: String -> Plomero -> Bool
tieneHerramienta denominacion1 = any((==denominacion1) . denominacion) . cajaHerramientas 

esMalvado :: Plomero -> Bool
esMalvado = (== "wa") . take 2 . nombre

puedeComprarHerramienta :: Herramienta -> Plomero -> Bool
puedeComprarHerramienta herramienta = (> precio herramienta) . dinero

-- EsBuenaV1 correcta
esBuenaV1 :: Herramienta -> Bool
esBuenaV1 (UnaHerramienta _ Hierro precio) = precio > 10000
esBuenaV1 (UnaHerramienta "Martillo" material _) = material == Madera || material == Goma

-- EsBuenaV2 Version menos copada
esBuenaV2 :: Herramienta -> Bool
esBuenaV2 unaHerramienta = (materialEmpuñaduraF Hierro unaHerramienta && precio unaHerramienta > 10000) || (denominacion unaHerramienta == "Martillo" && (materialEmpuñaduraF Madera unaHerramienta ||materialEmpuñaduraF Goma unaHerramienta)) 

materialEmpuñaduraF :: Material -> Herramienta -> Bool
materialEmpuñaduraF materialh = (== materialh) . material

-- Punto 4 / Podria hacer una funcion "pagarPrecio" que sea mas descriptiva usando cambiarDinero
comprarHerramienta :: Herramienta -> Plomero -> Plomero
comprarHerramienta herramienta plomero
    | puedeComprarHerramienta herramienta plomero = cambiarDinero(precio herramienta -) . cambiarHerramienta(herramienta :) $ plomero
    | otherwise = plomero

-- Punto 5
data Reparacion = UnaReparacion {
    descripcion :: String,
    requerimiento :: Plomero -> Bool
} deriving Show

filtracionDeAgua :: Reparacion 
filtracionDeAgua = UnaReparacion "Filtracion de agua" (tieneHerramienta "llaveInglesa")

esDificil :: Reparacion -> Bool
esDificil  reparacion = length (descripcion reparacion) > 100 && esUnGrito (descripcion reparacion)

esUnGrito :: String -> Bool
esUnGrito = all isUpper

presupuesto :: Reparacion -> Float
presupuesto  = (*3) . genericLength . descripcion

-- Punto 6
hacerReparacion :: Reparacion -> Plomero -> Plomero
hacerReparacion reparacion plomero
    | puedeResolver plomero reparacion = flip plomeroEspecial reparacion . cambiarDinero (+ presupuesto reparacion) . cambiarReparaciones(reparacion :) $ plomero
    | otherwise = flip plomeroEspecial reparacion . cambiarDinero (+100) $ plomero 

puedeResolver :: Plomero -> Reparacion -> Bool
puedeResolver plomero reparacion = requerimiento reparacion plomero && esMalvadoConMartillo plomero

esMalvadoConMartillo :: Plomero -> Bool
esMalvadoConMartillo plomero = esMalvado plomero && tieneHerramienta "maratillo" plomero

plomeroEspecial :: Plomero -> Reparacion -> Plomero
plomeroEspecial plomero reparacion
    | esMalvado plomero = cambiarHerramienta(destornillador :) plomero 
    | esDificil reparacion = cambiarHerramienta(filter (not . esBuenaV1)) plomero
    | otherwise = cambiarHerramienta tail plomero -- o (drop 1) = tail

destornillador :: Herramienta
destornillador = UnaHerramienta "Destornillador" Plastico 0

-- Punto 7
type JornadaLaboral = [Reparacion]

jornadaLaboral :: Plomero -> JornadaLaboral -> Plomero
jornadaLaboral = foldr hacerReparacion

-- Punto 8
empleadoMas :: [Plomero] -> [Reparacion] -> Criterio -> Plomero
empleadoMas plomeros reparaciones criterio = empleadoMasSegunCriterio criterio (map (flip jornadaLaboral reparaciones) plomeros)

type Criterio = Plomero -> Float

empleadoMasSegunCriterio :: Criterio -> [Plomero] -> Plomero
empleadoMasSegunCriterio criterio [plomero] = plomero
empleadoMasSegunCriterio criterio (x:xs:xss)
    | criterio x > criterio xs = empleadoMasSegunCriterio criterio (x:xss)
    | otherwise = empleadoMasSegunCriterio criterio (xs:xss)

-- Definicion de criterios
masReparador :: Criterio
masReparador = genericLength . reparaciones 

masAdinerado :: Criterio
masAdinerado = dinero 

masInvirtio :: Criterio 
masInvirtio plomero = sum . map precio $ cajaHerramientas plomero

-- Como seria el llamado de las funciones por linea de comando, o simplemente definidas en haskell ?
-- empleadoMasReparador  plomeros jornadaLaboral masReparador
-- empleadoMasAdinerado  plomeros jornadaLaboral masAdinerado
-- empleadoMasInvertidor plomeros jornadaLaboral masInvirtio

-- plomeros -> Lista de plomeros, jornadaLaboral -> Lista de reparaciones