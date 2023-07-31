--  esto es una composicion de funciones (calcula la mitad del siguiente valor "n") 
import Data.List ( genericLength )
siguiente :: Float -> Float 
siguiente y = 1 + y

mitad :: Float -> Float
mitad x = x/2

mitadDelSiguiente :: Float -> Float
mitadDelSiguiente = mitad . siguiente 

-- 2 Funciones de la practica 1
triple :: Int -> Int
triple x = x*3

esPositivo :: Float -> Bool
esPositivo x = x >= 0 

-- EJERCICIO 6: (Esta al revez, pero con el flip lo arreglo, Aca si hay aplicacion parcial)
longitud :: String -> Int
longitud = length

esMultiploDe :: Int -> Int -> Bool
esMultiploDe divisor x = mod x divisor == 0

esMultiploTotal :: Int -> Bool
esMultiploTotal = esMultiploDe 2

tieneLongPar :: String -> Bool
tieneLongPar = esMultiploDe 2 . longitud 

-- EJERCICIO 9 (INCREMENTA N AL CUADRADO DE UN VALOR, Aca )
cuadrado :: Int -> Int
cuadrado num = num^2

incrementoN :: Int -> Int -> Int
incrementoN num1 num2 = cuadrado num1 + num2

-- EJERCICIO 8 (LA INVERSA DE LA RAIZ)
inversa ::Float -> Float
inversa inv = 1/inv

raiz :: Float -> Float
raiz num = num**(0.5)

inversaRaizCuadrada :: Float -> Float
inversaRaizCuadrada = inversa . raiz 
-- EJERCICIO 7 (CALCULO DE AÑO BISIESTO)
esBisiesto :: Int -> Bool
esBisiesto anio = esMultiploDe 400 anio || esMultiploDe 4 anio && not(esMultiploDe 100 anio)

-- EJ 10
elevar :: Int -> Int -> Int 
elevar num1 num2 = num1^num2

esPar :: Int -> Bool
esPar x = mod x 2 == 0

elevadoEsPar :: Int -> Int -> Bool
elevadoEsPar num1 num2 =  esPar (elevar num1 num2) 



-- SEGUNDA PARTE

-- dispersion :: (Num a, Ord a) => a -> a -> a -> a
-- dispersion a b c = max a (max b c) - min a (min b c)

-- Testeo de lista y algunos ej (hasta el 7 inclusive, creo)
notasCami :: [Int]
notasCami = [9,4,6]

esNotaprobada :: (Ord a, Num a) => a -> Bool
esNotaprobada nota = nota >= 6

notasAprobadas :: [Float] -> [Float]
notasAprobadas  = filter esNotaprobada 

esLongImpar :: Foldable t => t a -> Bool -- no visto todavia
esLongImpar palabra = odd (length palabra) 


listaTransformada :: Integral  a => [a] -> [Bool]
listaTransformada   = map even 

listaModify :: [Integer] -> [Integer]
listaModify  = filter (>30) . map ((*8).(*4))

-- Ejercicios listas
-- 3) Definir la función esCapicua/1, si dada una lista de listas, me devuelve si la concatenación de las sublistas es una lista capicua.

esCapicua :: Eq a => [[a]] -> Bool
esCapicua lista = concat lista == reverse (concat lista)

esMultiploDeX :: Int -> Int -> Bool
esMultiploDeX divisor x  = mod x divisor == 0

prom :: [Float] -> Float
prom lista = sum lista / genericLength lista


promfiltrada :: [Float] -> Float
promfiltrada = prom . filter (>=6)

transform :: [[Float]] -> [Float]
transform  = map promfiltrada

transform2:: [[Float]] -> [Float]
transform2  = map maximum

estaAprobado :: [Int] -> Bool
estaAprobado  = all esNotaprobada 

aprobaron1 :: [[Int]] -> [[Int]]
aprobaron1  = filter estaAprobado

divde60 :: Integral a => a -> Bool
divde60 x = mod 60 x == 0

tobi :: (String, [Integer])
tobi = ("Tobias Sandler", [1,2,3])

notas :: (a, b) -> b
notas = snd

-- Definicion de tipos - Func type
type TipoPersona = (String, Int)
saludar :: String -> String
saludar nombre = "hola " ++ nombre

comoTeLlamas :: TipoPersona -> String
comoTeLlamas (nombre, _) = nombre

iracomprar :: TipoPersona -> String
iracomprar alguien = saludar (comoTeLlamas alguien)
-- 
-- Ahora empieza lo raro... Data + tuplas anywhere 

data Persona = UnaPersona {
    nombre :: String,
    edad :: Int,
    nota :: Int
} deriving (Show)

juan :: Persona
juan = UnaPersona "Juan" 22 2

maria :: Persona
maria = UnaPersona "Maria" 19 5

irAcomprar :: Persona -> String
irAcomprar alguien = saludar (nombre alguien)

irAcomprar2 :: Persona -> String
irAcomprar2 (UnaPersona nombre _ _) = saludar nombre

cambiarAnios :: Persona -> Persona
cambiarAnios alguien = alguien {edad =  edad alguien +1} 

-- ccambiarAnios :: Persona -> Persona
-- ccambiarAnios (UnaPersona nombre edad) = UnaPersona nombre (edad +1)

cambiarNota :: Int -> Persona -> Persona
cambiarNota nuevaNota (UnaPersona nombre edad _ ) = UnaPersona nombre edad nuevaNota

subirNota :: Persona -> Persona
subirNota alguien = cambiarNota (nota alguien +1) alguien

--cambiarBarrio :: String -> Persona -> Persona
--cambiarBarrio nuevobarrio alguien = alguien {barrio = nuevobarrio} 

esMayorEdad :: Persona -> Bool
esMayorEdad   = mayorDeEdad . edad  

edadyNota :: Persona -> Int
edadyNota alguien = edad alguien + nota alguien

mayorDeEdad :: Int -> Bool
mayorDeEdad edad = edad >= 18

-- TP 

data Sustancia 
    = Simple {
        elemento :: (String,Int,String),
        grupo :: String 
    } 
    | Compuesto {
        lista :: [(Sustancia,Int)],
        nombreCompuesto :: String,
        simboloQuimicoCompuesto :: String,
        grupo :: String
    } deriving (Show)


vocal::[Char]
vocal=['a', 'e', 'i', 'o', 'u']
nombreUnion :: [Char] -> [Char]
nombreUnion sustanciaLoca
    | elem (last sustanciaLoca) vocal =  (nombreUnion.init) sustanciaLoca
    | otherwise                         =  sustanciaLoca ++ "uro"

oxigeno :: Sustancia
oxigeno = Simple ("Oxigeno",8,"O") "No Metal"

hidrogeno :: Sustancia
hidrogeno = Simple ("Hidrogeno",1,"H") "No Metal"

agua :: Sustancia
agua = Compuesto [(hidrogeno,2),(oxigeno,1)] "agua" "H2O" "No Metal" 


conduceBien :: Sustancia -> Bool
conduceBien sustancia = grupo sustancia == "Gas Noble" ||  grupo sustancia == "Halogeno" || grupo sustancia == "Metal" 


-- combinarNombre :: String -> String -> String
--combinarNombre primeraS segundaS = nombreUnion primeraS ++ " de " ++ segundaS

--------------------------------------------------------------TP FUNCIONAL INTEGRADOR DE 2020 (DESPUES VER EL DE FRAN)
type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)

-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year

data Auto = UnAuto {
    patente :: Patente,
    desgasteLlantas :: [Desgaste],
    rpm :: Int,
    temperaturaAgua :: Int,
    ultimoArreglo :: Fecha
} deriving (Show, Eq)

-- Modelo un tipo a modo de ejemplo
fiat :: Auto
fiat = UnAuto "4TYHGSA" [0.4,1.2,0.2] 2220 25 (2,5,6)

cambioRev :: Int -> Int
cambioRev revoluciones 
    |revoluciones > 2000 = 2000
    |otherwise = revoluciones

alfa :: Auto -> Auto
alfa auto = auto {rpm = cambioRev (rpm auto)}

-- alfa2 :: Auto -> Auto (Otra manera de hacerlo, un poquito mas heavy, pero mas intuitiva)
-- alfa2 (UnAuto patente desgasteLlantas rpm temperaturaAgua ultimoArreglo) = UnAuto patente desgasteLlantas (cambioRev rpm) temperaturaAgua ultimoArreglo  


-- Modelar Bravo




--------------------- -------------------- PARCIAL FMI (PRACTICA) PARCIAL A SUBIR AL GIT:.. 


--1 a
data Pais = UnPais {
    ingresoPerCapita :: Float,
    poblacionActivaSPublico :: Float,
    poblacionActivaSPrivvado ::Float,
    recursosNaturales :: [String],
    deuda :: Float
} deriving (Eq,Show)

type Estrategia = Pais -> Pais

-- 1 b

namibia :: Pais
namibia = UnPais 4140 400000 650000 ["Petroleo", "Ecoturismo"] 50

-- 2 -- Implementacion estrategias del FMI

prestarPlata :: Float -> Estrategia
prestarPlata cantidadM unPais = unPais {deuda = deuda unPais + calculoIntereses cantidadM }

calculoIntereses :: Float -> Float
calculoIntereses cuanto = cuanto * 1.5

---------------------------- 
-- hay una forma mas linda de aplicar a esta parte del codigo, esta en la resolucion de mati
reducirxPuestosTrabajoSPublico :: Float -> Estrategia
reducirxPuestosTrabajoSPublico cantidadPuestos unPais = unPais {poblacionActivaSPublico = poblacionActivaSPublico unPais - cantidadPuestos, ingresoPerCapita = modificarIngresoPerCapita cantidadPuestos unPais }

modificarIngresoPerCapita :: Float -> Pais -> Float
modificarIngresoPerCapita cantidadTrabajos unPais = ingresoPerCapita unPais - ingresoPerCapita unPais * reduccionIngreso cantidadTrabajos

reduccionIngreso :: Float -> Float
reduccionIngreso cantidadTrabajos
    | cantidadTrabajos > 100 = 0.2
    | otherwise = 0.15 
------------------------------
ofrecerRecursoNatural :: String -> Estrategia
ofrecerRecursoNatural recursoNatural  = prestarPlata (-2) . sacarLista recursoNatural  -- queda a la espera del pais

sacarLista :: String -> Pais -> Pais
sacarLista recursoNatural unPais = unPais {recursosNaturales = filter (/= recursoNatural) (recursosNaturales unPais) }

blindaje :: Estrategia
blindaje unPais = prestarPlata (calcularPBI unPais * 0.5) . reducirxPuestosTrabajoSPublico 500 $ unPais

calcularPBI :: Pais -> Float
calcularPBI unPais = ingresoPerCapita unPais * poblacionActiva unPais

poblacionActiva :: Pais -> Float
poblacionActiva unPais = poblacionActivaSPublico unPais + poblacionActivaSPrivvado unPais

-- 3 a

type Receta = [Estrategia]

receta :: Receta
receta = [prestarPlata 200, ofrecerRecursoNatural  "Mineria"]

-- 3 b

aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta unReceta unPais = foldr ($) unPais unReceta 
                           -- = foldl (flip ($))
--4 a
zafa :: [Pais] -> [Pais] 
zafa  = filter (elem "Petroleo" . recursosNaturales) -- composicion + orden superior (fliter) + aplicacion parcial (elem "string")

-- b
totalDeuda :: [Pais] -> Float
totalDeuda  =  foldr ((+). deuda)  0
        --  =  sum . map deuda (idea Mati) composicion + orden superior
        -- aclaracion : la  lista la recibe y modifica map, luego sum devuelve un Int
-- 5 a

estaOrdenado :: Pais -> [Estrategia] -> Bool 
estaOrdenado _ [] = True    
estaOrdenado unPais (estrategia1:estrategia2:estrategias) = (calcularPBI . estrategia1) unPais <= (calcularPBI . estrategia2) unPais && estaOrdenado unPais (estrategia2:estrategias) -- tira el warning pero la funcion funciona realmente asi 

comparPBI :: Pais -> Receta -> Float
comparPBI unPais unareceta = (calcularPBI . aplicarReceta unareceta) unPais 

--------------------------
autoDanger :: Auto -> Bool
autoDanger auto = autoPeligroso (desgasteLlantas auto)

autoPeligroso :: [Float] -> Bool
autoPeligroso lista = head lista > 0.5

-- saberCostoReparacion :: [Char] -> Int
-- saberCostoReparacion patente
--    | length patente == 7 = 15000 (Como no entiendo a que se refiere exactamente lo voy a comentar)