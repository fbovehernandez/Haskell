-- Parcial Uber (70 lineas) --
data Remis = UnRemis {
    nombre :: String,
    kilometraje :: Int, 
    viajes :: [Viaje],
    condicion  :: Condicion
}

type Condicion = Viaje -> Bool

data Cliente = UnCliente {
    nombreCliente :: String,
    vivienda :: String
}

data Viaje = UnViaje {
    costo :: Int,
    cliente :: Cliente,
    fecha :: (Int, Int)
}

-- condiciones
tomaCualquiera :: Condicion
tomaCualquiera unCliente = True

salenMasde200 :: Viaje -> Condicion
salenMasde200 viaje unCliente = costo viaje > 200

-- salenMasde200 :: Condicion
-- salenMasde200 viaje = costo viaje > 200

cantidadNletras :: Int -> Condicion
cantidadNletras  cantidad = (> cantidad) . length . nombreCliente . cliente

noViveEnZona :: String -> Condicion
noViveEnZona zona  = (/= zona) . vivienda . cliente 

-- Funcion saber si toma viaje (4)
tomaViaje :: Viaje -> Remis -> Bool
tomaViaje unViaje unRemis = condicion unRemis $ unViaje

-- 5
liquidacionRemis :: Remis -> Int
liquidacionRemis unRemis = (sum . map costo) (viajes unRemis)

-- 6 a
hacerViaje :: Viaje -> [Remis] -> Remis
hacerViaje unViaje = agregarViaje unViaje . menosViajes .  quienesLoToman unViaje
-- Faltaria la funcion recursiva, que no se muy bien como se hace

quienesLoToman :: Viaje -> [Remis] -> [Remis] 
quienesLoToman unViaje = filter (tomaViaje unViaje) 

agregarViaje :: Viaje -> Remis -> Remis
agregarViaje unViaje unRemis = UnRemis {viajes = unViaje : viajes unRemis}


-- menosViajes :: [Remis] -> Remis
-- menosViajes listaRemises = unRemis
-- menosViajes (x:xs:xss)
   -- | length (viajes x) < length (viajes xs) = x
  --  | otherwise = menosViajes unViaje (xs:xss)
