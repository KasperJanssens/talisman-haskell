module UnitTest where

import Test.Hspec
import Board.BoardTest as BoardTest

main::IO()
main = hspec BoardTest.placingSpec

{-lijsteken :: [Character]
lijsteken = [OgreChieftain ogreChieftain, Wizard wizard]

geefDeTovenaarEenCentje::IO ()
geefDeTovenaarEenCentje = do
    let aangepastLijsteken = over (each._Wizard.gold) (+1) lijsteken
    print aangepastLijsteken


kijkEensWaarDeOgreStaat:: IO ()
kijkEensWaarDeOgreStaat = do
    let s = view (singular (each._OgreChieftain)) lijsteken
    print s

main::IO()
main = do
    geefDeTovenaarEenCentje
    kijkEensWaarDeOgreStaat
-}