--Pdep Lunes mañana 2019 1C
--Integrantes:
--Mauro Corengia mauro_corengia@hotmail.com
--Andrew Sosa andrew.wsosa@gmail.com

--Cambios V1.1
--1) Incorporacion  type alias

--2)Sintaxis Ej Pasamos de 
----deReversaRocha (Auto nombre nivelDeNafta velocidad nombreEnamorade truco) =  
----    Auto nombre (nivelDeNafta +200) velocidad nombreEnamorade truco
----	a
--	--deReversaRocha unAuto = unAuto { nivelDeNafta = ((+200).nivelDeNafta) unAuto}

--3)fingirAmor ahora recibe por parametro su enamorade

--4)Incluimos Pattern Matching para esVocal

--Cambios V1.2
--Se incorpora la funcion modificarVelocidadAuto

--3.1

type Truco = Auto -> Auto


data Auto = Auto {
    nombre :: String,
    nivelDeNafta :: Int,
    velocidad :: Int,
    nombreEnamorade :: String,
    truco :: Truco
}

modificarVelocidadAuto :: Int -> Truco
modificarVelocidadAuto speed unAuto = unAuto { velocidad = ((+speed).velocidad) unAuto}


deReversaRocha :: Truco
deReversaRocha  unAuto = unAuto { nivelDeNafta = ((+200).nivelDeNafta) unAuto}


impresionar :: Truco
impresionar unAuto = unAuto { velocidad = (velocidad.modificarVelocidadAuto (velocidad unAuto) ) unAuto}
--impresionar unAuto = unAuto { velocidad = ((*2).velocidad) unAuto}--Cambios V1.2

nitro :: Truco
nitro unAuto = unAuto { velocidad = (velocidad.modificarVelocidadAuto 15) unAuto}
--nitro unAuto = unAuto { velocidad = ((+15).velocidad) unAuto}--Cambios V1.2

fingirAmor :: String -> Truco
fingirAmor nombreNueveEnamorade  unAuto =  
    unAuto { nombreEnamorade = nombreNueveEnamorade}

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