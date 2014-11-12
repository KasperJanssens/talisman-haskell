{-# LANGUAGE CPP #-}

#ifdef FPHC
module UnitTest where
#else
module Main where
#endif

import Test.Hspec
import qualified Board.BoardTest as BoardTest
import qualified DungeonMaster.DungeonMasterTest as DungeonMasterTest

main::IO()
main= hspec $
 describe "Unit tests" $ do
  BoardTest.placingSpec
  BoardTest.movingSpec
  DungeonMasterTest.roundSpec
