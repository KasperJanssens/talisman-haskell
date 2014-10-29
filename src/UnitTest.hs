module UnitTest where

import Test.Hspec
import Data.Graph
import Control.Monad.State

board::State Graph Int
board = undefined

--graph = buildG (1, 6) [(1, 2), (1, 3), (2, 4), (5, 6)]
graph::Graph
--graph = buildG (1,6) [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3),(4,5),(5,4),(5,6),(6,5),(6,1),(1,6)]
graph = buildG (1,6) [(1,2),(2,3),(3,4),(4,5),(2,6)]

--graph = buildG (1,6) [(1,2),(2,3),(3,4),(4,5),(5,6),(6,1)]

goRight::Int -> Graph -> Int -> Int
goRight steps g startNode =  head $ drop steps $ reachable g startNode

goLeft::Int -> Graph -> Int -> Int
goLeft steps g startNode =  head $ drop steps $  reachable (transposeG g) startNode



main::IO()
main = do
    print $ dfs graph [1]
    --print $ reachable graph 2

{-main::IO()
main = hspec spec

spec::Spec
spec = describe "Board actions" $ do
         it "should go right" $ do
             let board = graph
             goRight 1 board 1 `shouldBe` 2
         it "should go left" $ do
             let board = graph
             goLeft 1 board 1 `shouldBe` 6
-}