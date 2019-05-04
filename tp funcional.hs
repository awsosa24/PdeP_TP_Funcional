--Pdep Lunes mañana 2019 1C
--Integrantes:
--Mauro Corengia mauro_corengia@hotmail.com
--Andrew Sosa andrew.wsosa@gmail.com

--Cambios V2

--Definicion de tipos
type Truco = Auto -> Auto
type NivelDeNafta = Int


data Auto = Auto {
    nombre :: String,
    nivelDeNafta :: NivelDeNafta,
    velocidad :: Int,
    nombreEnamorade :: String,
    truco :: Truco
}

--Trucos V1.x
modificarVelocidadAuto :: Int -> Auto -> Auto
modificarVelocidadAuto speed unAuto = unAuto { velocidad = ((+speed).velocidad) unAuto}

deReversaRocha :: Truco
deReversaRocha  unAuto = unAuto { nivelDeNafta = ((+200).nivelDeNafta) unAuto}

deReversa :: Truco
deReversa  unAuto = unAuto { nivelDeNafta = ((div 5).velocidad) unAuto} --V2.0 Punto 3.0

impresionar :: Truco
impresionar unAuto = unAuto { velocidad = (velocidad.modificarVelocidadAuto (velocidad unAuto) ) unAuto}

nitro :: Truco
nitro unAuto = unAuto { velocidad = (velocidad.modificarVelocidadAuto 15) unAuto}

fingirAmor :: String -> Truco
fingirAmor nombreNueveEnamorade  unAuto =   unAuto { nombreEnamorade = nombreNueveEnamorade}

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
    truco = fingirAmor "Petra"
}

--3.2

esVocal :: Char -> Bool

esVocal 'a' = True
esVocal 'e' = True
esVocal 'i' = True
esVocal 'o' = True
esVocal 'u' = True
esVocal _ = False


cantidadDeVocales :: String -> Int
cantidadDeVocales  = length.filter esVocal


velocidadDelIncremento :: Int -> Int
velocidadDelIncremento vocales | vocales > 4 = 30
                             | vocales >= 3 = 20
                             | vocales >= 1 = 15
                             | otherwise = 0


incrementarVelocidad :: Truco
incrementarVelocidad unAuto =  
    unAuto {velocidad= (velocidad.modificarVelocidadAuto (velocidadDelIncremento.cantidadDeVocales$ nombreEnamorade unAuto) ) unAuto }
--unAuto {velocidad= (velocidad unAuto + (velocidadDelIncremento.cantidadDeVocales$ nombreEnamorade unAuto))}--Cambios V1.2

--3.3

puedeRealizarTruco :: Auto -> Bool
puedeRealizarTruco auto = ((>0).nivelDeNafta) auto && ((<100).velocidad) auto 
--puedeRealizarTruco auto = nivelDeNafta auto > 0 && velocidad auto < 100



--3.4

comboLoco :: Truco
comboLoco = deReversaRocha.nitro

queTrucazo :: String -> Truco
queTrucazo nombreNueveEnamorade unAuto= (incrementarVelocidad.fingirAmor nombreNueveEnamorade) unAuto

turbo :: Truco
turbo unAuto = unAuto {nivelDeNafta = 0} {velocidad = (velocidad.modificarVelocidadAuto ((*10).nivelDeNafta$ unAuto) ) unAuto}
--turbo unAuto = unAuto {nivelDeNafta = 0} {velocidad = velocidad unAuto + ((*10).nivelDeNafta) unAuto}--Cambios V1.2


------- Entrega 2 --------
type CantidadVueltas = Int
type LongitudVuelta = Int 
type IntegrantesPublico = [String]
type Participantes = [Auto]
type Trampa = Participantes -> Participantes

------------ 3.1 ---------------
--V2.0 Punto 3.1
data Carrera = Carrera {
    cantidadVueltas :: CantidadVueltas,
    longitudVuelta :: LongitudVuelta,
    integrantesPublico :: IntegrantesPublico,
    trampa :: Trampa,
    participantes :: Participantes
}

--V2.0 Punto 3.1
potreroFunes = Carrera {
    cantidadVueltas = 3,
    longitudVuelta = 5,
    integrantesPublico = ["Ronco", "Tinch", "Dodain"],
    trampa = sacarAlPistero,
    participantes = [rochaMcQueen, biankerr, gushtav, rodra] 
} 

-------------- 3.2 ---------------
--Funciones aux para tramptype Participantes = [Auto]as
inutilidad :: Truco
inutilidad unAuto = unAuto

modificarTrucoAuto :: Truco -> Auto -> Auto
modificarTrucoAuto nuevoTruco unAuto = unAuto {truco = nuevoTruco}


--Modelado de trampas --V2.0 Punto 3.1
sacarAlPistero :: Trampa
sacarAlPistero = tail

lluvia :: Trampa
lluvia = map (modificarVelocidadAuto (-10))

neutralizarTrucos :: Trampa
neutralizarTrucos = map (modificarTrucoAuto inutilidad)

--pocaReserva
pocaReserva :: Trampa
pocaReserva = filter ((< 30).nivelDeNafta)

--podio
podio :: Trampa
podio (x:xs) = take 3 xs


--------------- 3.3 -----------------
aplicarAParticipantes :: (Auto -> Auto) -> Carrera -> Carrera
aplicarAParticipantes modificador unaCarrera = unaCarrera {participantes = map modificador (participantes unaCarrera)}

restarCombustible :: Carrera -> Auto -> Auto
restarCombustible unaCarrera unAuto = unAuto {nivelDeNafta = nivelDeNafta unAuto - div (longitudVuelta unaCarrera) ((*10).velocidad$ unAuto)}

realizarTruco :: Auto -> Auto
realizarTruco unAuto = (truco unAuto) unAuto

aplicarTrampa :: Carrera -> Carrera
aplicarTrampa unaCarrera = unaCarrera {participantes = (trampa unaCarrera) (participantes unaCarrera)}


darVuelta :: Carrera -> Carrera
darVuelta unaCarrera = (aplicarTrampa).(aplicarAParticipantes realizarTruco).(aplicarAParticipantes (restarCombustible unaCarrera))$ unaCarrera

restarVuelta :: Carrera -> Carrera
restarVuelta unaCarrera = unaCarrera {cantidadVueltas = cantidadVueltas unaCarrera - 1}

correrCarrera :: Carrera -> Carrera
correrCarrera unaCarrera | cantidadVueltas unaCarrera > 0 = darVuelta (restarVuelta.darVuelta$ unaCarrera)
                         | otherwise = unaCarrera


----------- 3.4 ------------
--mayorVelocidad :: [Auto] -> Auto


--quienGana :: Carrera -> Auto
--quienGana unaCarrera = mayorVelocidad (participantes unaCarrera)


------------ 3.5 ------------
--elGranTruco :: [Truco] -> Auto -> Auto
--elGranTruco trucos unAuto = map unAuto trucos

-- Hay un ejercicio casi igual en mumuki

------------ 3.6 --------------




--naftaMenorA30 :: [NivelDeNafta] -> [NivelDeNafta]
--naftaMenorA30= filter (>30) 



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
informacionAuto auto = "El auto " ++ nombre auto  ++ " tiene "  ++ show (nivelDeNafta auto) ++ " litros, su velocidad es " ++ show (velocidad auto) ++ " y su enamorade es " ++ nombreEnamorade auto

informacionCarrera :: Carrera -> String
--informacionCarrera carrera = " y sus competidores son " ++ show (map nombre (participantes carrera))
informacionCarrera carrera = "La carrera "  ++ " tiene "  ++ show (cantidadVueltas carrera) ++ " vueltas, su longitud es " ++ show (longitudVuelta carrera) ++ ",sus participantes son" ++ show (integrantesPublico carrera)++" y sus competidores son " ++ show (map nombre (participantes carrera))



--3.1
--(informacionAuto.deReversaRocha) rochaMcQueen
--(informacionAuto.impresionar) biankerr
--(informacionAuto.nitro) gushtav
--(informacionAuto.truco rodra) rodra


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
--(informacionAuto.queTrucazo "Murcielago") rodra
--(informacionAuto.turbo) gushtav
--(informacionAuto.turbo) rodra

--Cambios
--reordenar segun la entrega
--cambiar el tipo de las trampas a participantes -> participantes
--cambio de longitud de vuelta de float a int 