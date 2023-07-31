--- 
doble :: Int -> Int
doble numero = numero * 2

maxB :: Int -> Int -> Int
maxB x y 
    | x > y = x 
    | otherwise = y
    
cuadrupleNumero :: Int -> Int
cuadrupleNumero  = doble . doble

longitudPar :: String -> Bool
longitudPar  = odd . length

idX :: a -> a
idX x = x

-- Ejercicios
esMultiploDe :: Int -> Int -> Bool
esMultiploDe n m = mod n m == 0

esMultiploDe3 :: Int -> Bool
esMultiploDe3 = esMultiploDe 3 

cubo :: Int -> Int
cubo n = n ^ 3

esBisiesto :: Int ->  Bool
esBisiesto anio  = esMultiploDe anio 400  || esMultiploDe4YNoDe100 anio 

esMultiploDe4YNoDe100 :: Int -> Bool
esMultiploDe4YNoDe100 n  = esMultiploDe 4 n && (not . esMultiploDe 100) n

esNumeroPositivo :: Float -> Bool
esNumeroPositivo n = n >= 0

esMultiploDeX :: Int -> Int -> Bool
esMultiploDeX n = (0 ==) . mod n 

esResultadoPar :: Int -> Int -> Bool
esResultadoPar n = even . eleve n 

eleve :: Int -> Int -> Int
eleve n m = n ^ m

notasAprobadasDeLaLista :: Persona -> [Int]
notasAprobadasDeLaLista  = filter notaAprobada . snd 

notaAprobada :: Int -> Bool
notaAprobada n = n >= 6

type Persona = (String, [Int])

-- Caso ejemplo. 
cami :: Persona
cami = ("Cami", [1,2,4])

notas :: Persona -> [Int]
notas (_, n) = n

-- Listas 
head :: [a] -> a
head (x:_) = x

-- Ejercicio Tuplas
-- 2
aplicar :: (Int -> Int , Int -> Int) ->  Int -> (Int, Int) 
aplicar (f, g) n = (f n, g n)

-- 3
cuentaBizarra :: (Int, Int) -> Int
cuentaBizarra (n, m) 
    | n > m = n + m
    | m > n + 10 = m - n 
    | otherwise = n * m -- m > n + x / x < 10. 

-- 4
esNotaBochazo :: Int -> Bool
esNotaBochazo n = n < 6 

aprobo :: (Int, Int) -> Bool
aprobo (nota1, nota2) = (not . esNotaBochazo) nota1 && (not . esNotaBochazo) nota2

-- promociono
-- nota1 + nota2 > 15 && (nota1 > 7 && nota2 > 7) 

-- Listas
frecuenciaCardiaca :: [Int]
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125]

promedioFrecuenciaCardiaca :: [Int] -> Int
promedioFrecuenciaCardiaca listaInt = div (sum listaInt) (length listaInt) 

frecuenciaCardiacaMinuto :: [Int] -> Int -> Int
frecuenciaCardiacaMinuto listaInt minuto = listaInt !! cantidadDeMinutosDiv10 minuto

frecuenciasHastaMomento :: Int -> [Int] -> [Int]
frecuenciasHastaMomento n = take (cantidadDeMinutosDiv10 n) -- Espera a la lista

cantidadDeMinutosDiv10 :: Int -> Int
cantidadDeMinutosDiv10 n = div n 10 

-- 4
duracionLlamadas :: ((String, [Int]), (String, [Int]))
duracionLlamadas = (("horarioReducido", [20,10,25,15]),("horarioNormal" ,[10,5,8,2,9,10]))

type Horario = (String, [Int])

cuandoHabloMasMinutos :: ((String, [Int]), (String, [Int])) -> String
cuandoHabloMasMinutos ((horario, listaInt), (horario2, listaInt2)) 
    | sum listaInt > sum listaInt2 = horario 
    | otherwise = horario2

-- El b es muy parecido usando length

esMultiploDeAlguno :: Int -> [Int] -> [Int]
esMultiploDeAlguno n = filter (esMultiploDe n)
  
promedios :: [[Int]] -> [Int]
promedios = map promedio 

promedio :: [Int] -> Int
promedio lista = div (sum lista) (length lista) 

-- 4
mejoresNotas :: [[Int]] -> [Int]
mejoresNotas  = map maximum

-- 5
aproboAlumno :: [Int] -> Bool
aproboAlumno = all notaAprobada 

-- 6
filterAprobaron :: [Int] -> [Int]
filterAprobaron = filter notaAprobada
-- forma 1
aprobaron :: [[Int]] -> [[Int]]
aprobaron = map filterAprobaron 

-- forma 2
aprobaron2 :: [[Int]] -> [[Int]]
aprobaron2 = filter aproboAlumno

-- 10
aplicarFunciones :: Int -> [Int -> Int] -> [Int]
aplicarFunciones n = map ($ n) -- o ApFuncion n

apFuncion :: Int -> (Int -> Int) -> Int
apFuncion valor f = f valor

-- 11
sumaF :: Int -> [Int -> Int] -> Int
sumaF n = sum . aplicarFunciones n  

-- 12
subirHabilidad :: Int -> [Int] -> [Int]
subirHabilidad n = map (min 12 . (+ n)) 

-- 13
flimitada :: (Int -> Int) -> Int -> Int
flimitada f n 
    | f n > 12 = 12
    | f n < 0 = 0 
    | otherwise = f n

cambiarHabilidad :: (Int -> Int) -> [Int] -> [Int]
cambiarHabilidad f = map (flimitada f)

-- 14 y 15 son ejercicios de funciones del prelude, que no las vimos 

-- Datas
data Alumno = Alumno {
    nombre :: String,
    edad :: Int, -- Alumno -> Int
    notaFuncional :: Nota
}

data Nota = Nota {
    valor :: Int,
    desc :: String
}

-- Definicion por constructores 
data Materiales = Madera | Hierro | Cobre | Oro

--notaFinal :: Alumno -> Int
--notaFinal unAlumno = valor (notaFuncional unAlumno) + valor (notaLogico unAlumno) 

juan :: Alumno
juan = Alumno "Juan" 9 (Nota 9 "bien")

-- Pasaje por Pattern Matching
-- nota :: Alumno -> Int
-- nota (Alumno nombre _ nota) = nota

saludar :: Alumno -> String
saludar unAlumno = saludarA (nombre unAlumno)

saludarA :: String -> String
saludarA nombre = "hola " ++ nombre

-- Patern matching avanzando 

-- Como "Modifico" una data 
subir :: Nota -> Nota
subir nota = nota {valor = valor nota + 1}

-- Ejercicio Practico uso de datas con + conceptos Jue Noche. 

data Rambo = Rambo {
    armaPrincipal :: Arma,
    armaSecundaria :: Arma
}

data Arma = Arma {
    cargador :: Int,
    balas :: Int
}

cantidadBalasRambo :: Rambo -> Int
cantidadBalasRambo unRambo = balas (armaPrincipal unRambo) + balas (armaSecundaria unRambo)

disparar :: Arma -> Arma
disparar unArma 
    | balas unArma == cargador unArma = unArma {balas = balas unArma -2}
    | balas unArma > 0 = unArma {balas = balas unArma - 1}
    | otherwise = unArma -- No hace nada. 

dispararTodo :: Rambo -> Rambo
dispararTodo rambo = rambo {
    armaPrincipal = disparar (armaPrincipal rambo),
    armaSecundaria = disparar (armaSecundaria rambo)
}

-- Notacion point free. 
-- Es el famoso queda a la esperar de .. .
-- Practica de integracion composicion y ap parcial + datas. 

data Cliente = Cliente {
    saldo :: Float,
    vip :: Bool,
    nombreC :: String
}

data Producto = Producto {
    tipo :: String,
    precio :: Float
}

cambiarSaldo :: Float -> Cliente -> Cliente
cambiarSaldo delta unCliente = unCliente {saldo = saldo unCliente + delta}

-- 
nuevoClienteVip :: String -> Cliente
nuevoClienteVip = Cliente 0 True

comprar :: Producto -> Cliente -> Cliente
comprar producto cliente = (flip cambiarSaldo cliente . negate . precioNeto) producto 
  
precioNeto :: Producto -> Float
precioNeto = (*1.21) . precio

-- Recursividad
estaOrdenada :: Ord a => [a] -> Bool
estaOrdenada [] = True
estaOrdenada (x : []) = True -- o [x]
estaOrdenada (x:xs:xss) = x < xs && estaOrdenada (xs:xss)

-- Pdf recursividad
factorial :: Int -> Int
factorial n 
    | n == 0 = 1
    | n > 0 = n * factorial (n -1)

factorialX :: Int -> Int
factorialX 0 = 1
factorialX n = n * factorial (n-1)

-- listas infinitas
muchosDe :: t -> [t]
muchosDe = replicate 100

listaInfinite :: t -> [t]
listaInfinite = repeat -- queda a la espera del t gracias a la notacion point free.  

-- Practica foldr 
data Carta = Carta {
    nombreCarta :: String,
    tags :: [String],
    velocidad :: Int,
    altura :: Int,
    peso :: Int,
    fuerza :: Int,
    peleas :: Int
}

ponerTag :: String -> Carta -> Carta
ponerTag tag carta = carta {tags = tag : tags carta}

quitarTag :: String -> Carta -> Carta
quitarTag tag carta = carta {tags = filter(/= tag) (tags carta)}

batinombres :: [Carta] -> [String]
batinombres = map nombreCarta . filter((== "bat") . take 3 . nombreCarta) 

-- Ahora la bomba -> uso del fold
-- sum usando fold -> fold (+) 0 listaNumeros
-- map usando fold -> foldr ((:) . f) []

------------------------------------------------------------


