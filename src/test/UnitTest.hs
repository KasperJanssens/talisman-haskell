{-# LANGUAGE CPP #-}

#ifdef FPHC
module UnitTest where
#else
module Main where
#endif

import Test.Hspec
import qualified Board.BoardTest as BoardTest

main::IO()
main= hspec BoardTest.placingSpec
