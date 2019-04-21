--Integrantes:
--Mauro Corengia mauro_corengia@hotmail.com
--Andrew Sosa andrew.wsosa@gmail.com

--3.1
data Auto = Auto {
    nombre :: String,
    nivelDeNafta :: Int,
    velocidad :: Int,
    nombreEnamorade :: String,
    truco :: (Auto -> Auto)
}


deReversaRocha :: Auto -> Auto
deReversaRocha (Auto nombre nivelDeNafta velocidad nombreEnamorade truco) =  
    Auto nombre (nivelDeNafta +200) velocidad nombreEnamorade truco

impresionar :: Auto -> Auto
impresionar (Auto nombre nivelDeNafta velocidad nombreEnamorade truco) =  
    (Auto nombre nivelDeNafta (velocidad *2) nombreEnamorade truco)

nitro :: Auto -> Auto
nitro (Auto nombre nivelDeNafta velocidad nombreEnamorade truco) =  
    (Auto nombre nivelDeNafta (velocidad +15) nombreEnamorade truco)

fingirAmor :: Auto -> Auto
fingirAmor (Auto nombre nivelDeNafta velocidad nombreEnamorade truco) =  
    (Auto nombre nivelDeNafta velocidad "Petra" truco)


rochaMcQueen = Auto {
    nombre = "RochaMcQueen",
    nivelDeNafta = 300,
    velocidad = 0,
    nombreEnamorade = "Ronco",
    truco = deReversaRocha
}

biankerr = Auto {
    nombre = "Biankerr",
    nivelDeNafta = 500,
    velocidad = 20,
    nombreEnamorade = "Tinch",
    truco = impresionar
}

gushtav = Auto {
    nombre = "Gushtav",
    nivelDeNafta = 200,
    velocidad = 130,
    nombreEnamorade = "PetiLaLinda",
    truco = nitro
}

rodra = Auto {
    nombre = "Rodra",
    nivelDeNafta = 0,
    velocidad = 50,
    nombreEnamorade = "Taisa",
    truco = fingirAmor
}

--3.2

esVocal :: Char -> Bool
esVocal letra | letra == 'a' = True
              | letra == 'e' = True
              | letra == 'i' = True
              | letra == 'o' = True
              | letra == 'u' = True
              | otherwise = False

cantidadDeVocales :: String -> Int
cantidadDeVocales nombreEnamorade = length (filter esVocal nombreEnamorade)

tamanioDelIncremento :: Int -> Int
tamanioDelIncremento vocales | vocales > 4 = 30
                             | vocales >= 3 = 20
                             | vocales >= 1 = 15
                             | otherwise = 0

incrementarVelocidad :: Auto -> Auto
incrementarVelocidad (Auto nombre nivelDeNafta velocidad nombreEnamorade truco) =  
    (Auto nombre nivelDeNafta (velocidad + (tamanioDelIncremento.cantidadDeVocales$ nombreEnamorade)) nombreEnamorade truco)


--3.3

puedeRealizarTruco :: Auto -> Bool
puedeRealizarTruco auto = nivelDeNafta auto > 0 && velocidad auto < 100

--3.4

comboLoco :: Auto -> Auto
comboLoco = deReversaRocha.nitro

queTrucazo :: Auto -> Auto
queTrucazo = incrementarVelocidad.fingirAmor

turbo :: Auto -> Auto
turbo (Auto nombre nivelDeNafta velocidad nombreEnamorade truco) =  
    (Auto nombre 0 (velocidad + nivelDeNafta*10) nombreEnamorade truco)

-----
--------------------------------
-----------------------------------------------
-----------------------------------------------------------
-----------------------------------------------
--------------------------------
-----

--Casos de Prueba

--Poner el texto en la consola para realizar los casos de pruebas

informacionAuto :: Auto -> String
informacionAuto auto = "El auto " ++ nombre auto  ++ " tiene "  ++ show (nivelDeNafta auto) ++ " litros, su velocidad es " ++ show (velocidad auto) ++ " y su enamorado es " ++ nombreEnamorade auto


--3.1
--(informacionAuto.deReversaRocha) rochaMcQueen
--(informacionAuto.impresionar) biankerr
--(informacionAuto.nitro) gushtav
--(informacionAuto.fingirAmor) rodra


--3.2
--(informacionAuto.incrementarVelocidad) rochaMcQueen
--(informacionAuto.incrementarVelocidad) biankerr
--(informacionAuto.incrementarVelocidad) gushtav
--(informacionAuto.incrementarVelocidad) rodra

--3.23
--puedeRealizarTruco rochaMcQueen
--puedeRealizarTruco gushtav
--puedeRealizarTruco rodra

--3.4
--(informacionAuto.comboLoco) rochaMcQueen

--(informacionAuto.queTrucazo2) rodra
queTrucazo2 :: Auto -> Auto
queTrucazo2 = incrementarVelocidad.fingirAmor2

fingirAmor2 :: Auto -> Auto
fingirAmor2 (Auto nombre nivelDeNafta velocidad nombreEnamorade truco) =  
    (Auto nombre nivelDeNafta velocidad "Murcielago" truco)

--(informacionAuto.turbo) gushtav
--(informacionAuto.turbo) rodra