-- Parcial
import Text.Show.Functions
import Data.List(genericLength)
import Data.Char(isUpper)
import GHC.ExecutionStack (Location(functionName))

-- 1 
data Plomero = UnPlomero {
    nombre :: String,
    cajaHerramientas :: [Herramienta],
    reparaciones :: [Reparacion],
    dinero :: Int
}

data Material = Hierro | Madera | Goma | Plastico deriving Eq

data Herramienta = UnaHerramienta {
    denominacion :: String,
    materialEmpuñadura :: Material,
    precio :: Int
} deriving Eq

mario :: Plomero
mario = UnPlomero "Mario" [llaveInglesa, martillo] [] 1200

{-
wario :: Plomero
wario = UnPlomero "Wario" (listaInfinitasLLaves llaveFrancesa) [] 1

listaInfinitasLLaves :: Herramienta -> [Herramienta]
listaInfinitasLLaves unaHerramienta = unaHerramienta : listaInfinitasLLaves (unaHerramienta {precio = 1 + precio unaHerramienta})
-}

-- Modelado Herramientas
llaveFrancesa :: Herramienta
llaveFrancesa = UnaHerramienta "Llave Francesa" Hierro 1

llaveInglesa :: Herramienta
llaveInglesa = UnaHerramienta "Llave Inglesa"  Hierro 200

martillo :: Herramienta 
martillo = UnaHerramienta "Martillo" Madera 20

-- Punto 2 
tieneHerramienta :: String -> Plomero -> Bool
tieneHerramienta nombreH unPlomero = (elem nombreH . map denominacion) (cajaHerramientas unPlomero)

-- any ((== unaDenominacion) . denominacion) . herramientas

esMalvado :: Plomero -> Bool
esMalvado = (== "Wa") . take 2 . nombre 

puedeComprar :: Plomero -> Herramienta -> Bool
puedeComprar unPlomero unaHerramienta = (>= precio unaHerramienta) . dinero $ unPlomero

-- 3 
esBuena :: Herramienta -> Bool
esBuena unaHerramienta = precio unaHerramienta > 10000 && tieneEmpuñadura unaHerramienta Hierro || (tieneEmpuñadura martillo Goma || tieneEmpuñadura martillo Madera)

-- Tambien se podria usar Patern Matching ya que solo recibe una Herramienta donde el precio importa solo si es de Hierro

tieneEmpuñadura :: Herramienta -> Material -> Bool
tieneEmpuñadura unaHerramienta material = materialEmpuñadura unaHerramienta == material

-- Punto 4
comprarHerramienta :: Herramienta -> Plomero -> Plomero
comprarHerramienta unaHerramienta unPlomero
    | puedeComprar unPlomero unaHerramienta = pagarPrecio (precio unaHerramienta) . modificarCajaHerramienta (unaHerramienta :)  $ unPlomero
    | otherwise = unPlomero    

pagarPrecio :: Int -> Plomero -> Plomero
pagarPrecio n unPlomero = unPlomero {dinero = dinero unPlomero + n}

modificarCajaHerramienta :: ([Herramienta] -> [Herramienta]) -> Plomero -> Plomero
modificarCajaHerramienta f unPlomero = unPlomero {cajaHerramientas = f (cajaHerramientas unPlomero)}

-- Punto 5
data Reparacion = UnaReparacion {
    descripcion :: String,
    requirimiento :: Plomero -> Bool
}

filtracionAgua :: Reparacion 
filtracionAgua = UnaReparacion "Filtracion de agua" (tieneHerramienta "Llave Inglesa") 

esDificil :: Reparacion -> Bool
esDificil unaReparacion =  length (descripcion unaReparacion) > 100 && all isUpper (descripcion unaReparacion) 

saberPresupuesto :: Reparacion -> Int
saberPresupuesto = (*30) . genericLength . descripcion 

-- 6 
--reparar :: Reparacion -> Plomero -> Plomero
--reparar unaReparacion unPlomero 
  --  | puedeHacerla unaReparacion unPlomero = pagarPrecio (saberPresupuesto unaReparacion) . agregarReparacion unaReparacion . hacerSegun unaReparacion $ unPlomero
   -- | otherwise = pagarPrecio 100 unPlomero

-- hacerSegun .. Reparacion -> Plomero -> Plomero

agregarReparacion :: Reparacion -> Plomero -> Plomero
agregarReparacion unaReparacion unPlomero = unPlomero { reparaciones = unaReparacion : reparaciones unPlomero }

puedeHacerla :: Reparacion -> Plomero -> Bool
puedeHacerla unaReparacion unPlomero = requirimiento  unaReparacion unPlomero || (esMalvado unPlomero && tieneHerramienta "Martillo" unPlomero) 

-- 7 
--jornadaLaboral :: Plomero -> [Reparacion] ->  Plomero
--jornadaLaboral = foldl (flip reparar) 

-- 8 
