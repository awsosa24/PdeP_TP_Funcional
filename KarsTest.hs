import Kars
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
--https://hspec.github.io/ 	
--Se tiene que abrir la consola en donde tengas unicado el TP
--cd C:\Users\asosa\Desktop\PdeP\TP funcional 
--runhaskell KarsTest.hs


main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "3.0" $ do
       (nivelDeNafta.truco rochaMcQueen) rochaMcQueen `shouldBe` (500 :: Int)
       (nivelDeNafta.impresionar) rodra `shouldBe` (0 :: Int)
    it "3.2" $ do 
     (length.sacarAlPistero.participantes) potreroFunes `shouldBe` (3 :: Int)
--ESTA FALTANDO ESTE TEST (map nombre.pocaReserva.sacarAlPistero) (participantes potreroFunes) `shouldBe` (["Biankerr", "Gushtav", "Rodra"] :: [String])	 
     (length.pocaReserva.participantes) potreroFunes `shouldBe` (3 :: Int)
--ESTA FALTANDO ESTE TEST (map nombre.pocaReserva) (participantes potreroFunes) `shouldBe` (["RochaMcQueen","Biankerr", "Gushtav"] :: [String])	 
     (length.podio.participantes) potreroFunes `shouldBe` (3 :: Int)
     (velocidad.last.lluvia.participantes) potreroFunes `shouldBe` (40 :: Int)

