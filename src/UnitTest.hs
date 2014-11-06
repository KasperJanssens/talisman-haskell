module UnitTest where

import Test.Hspec
import qualified Board.BoardTest as BoardTest

main::IO()
main= hspec BoardTest.placingSpec
