        -----------------  PARCIAL FMI 2020 -----------------

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

reducirPuestosTrabajo :: Float -> Estrategia 
reducirPuestosTrabajo cantidad unPais
    | ((>100) . poblacionActivaSPublico) unPais = (reducirIPC 0.2 . reducirPuestosPublic cantidad) unPais
    | otherwise = (reducirIPC 0.15 . reducirPuestosPublic cantidad) unPais

reducirPuestosPublic :: Float -> Pais -> Pais
reducirPuestosPublic cantidadPuestos unPais = unPais{poblacionActivaSPublico = poblacionActivaSPublico unPais - cantidadPuestos}

reducirIPC :: Float -> Pais -> Pais 
reducirIPC cantidad unPais = unPais {ingresoPerCapita = ingresoPerCapita unPais - ingresoPerCapita unPais * cantidad}

---------------- Otra alternativa mas rancia a la 2da estrategia del punto 2) 

reducirxPuestosTrabajoSPublico :: Float -> Estrategia
reducirxPuestosTrabajoSPublico cantidadPuestos unPais = unPais {poblacionActivaSPublico = poblacionActivaSPublico unPais - cantidadPuestos, ingresoPerCapita = modificarIngresoPerCapita cantidadPuestos unPais}

modificarIngresoPerCapita :: Float -> Pais -> Float
modificarIngresoPerCapita cantidadTrabajos unPais = ingresoPerCapita unPais - ingresoPerCapita unPais * reduccionIngreso cantidadTrabajos

reduccionIngreso :: Float -> Float
reduccionIngreso cantidadTrabajos
    | cantidadTrabajos > 100 = 0.2
    | otherwise = 0.15 
------------------------------

ofrecerRecursoNatural :: String -> Estrategia
ofrecerRecursoNatural recursoNatural  = prestarPlata (-2) . sacarLista recursoNatural  -- queda a la espera de unPais

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
receta = [prestarPlata 200, ofrecerRecursoNatural  "Mineria"] -- Modelado

-- 3 b

aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta unReceta unPais = foldr ($) unPais unReceta 
                           -- = foldl (flip ($))
--4 a
zafa :: [Pais] -> [Pais] 
zafa  = filter (elem "Petroleo" . recursosNaturales) -- composicion + orden superior (fliter) + aplicacion parcial (elem "string")

-- b
totalDeuda :: [Pais] -> Float
totalDeuda  = foldr ((+). deuda)  0
        --  =  sum . map deuda / composicion + orden superior
        -- aclaracion : la  lista la recibe y modifica map, luego sum devuelve un Int

-- 5 a

estaOrdenado :: Pais -> [Estrategia] -> Bool 
estaOrdenado _ [] = True    
estaOrdenado unPais (estrategia1:estrategia2:estrategias) = (calcularPBI . estrategia1) unPais <= (calcularPBI . estrategia2) unPais && estaOrdenado unPais (estrategia2:estrategias) 

comparPBI :: Pais -> Receta -> Float
comparPBI unPais unareceta = (calcularPBI . aplicarReceta unareceta) unPais

-- 6 
-- a : La funcion no va a devolver nada ya que al ser una lista de recursos de energia, nunca va a encontrar "petroleo". 
-- b : No habria problema, ya que gracias a lazy evaluation, la funcion no necesita evaluar la lista de recursos infinita