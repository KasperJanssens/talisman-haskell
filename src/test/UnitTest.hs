{-# LANGUAGE CPP #-}

#ifdef FPHC
module UnitTest where
#else
module Main where
#endif

import Test.Hspec
import qualified Board.BoardTest as BoardTest

main::IO()
main= hspec $
 describe "Unit tests" $ do
  BoardTest.placingSpec
  BoardTest.movingSpec
