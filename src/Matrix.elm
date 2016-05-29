module Matrix exposing (Matrix, init, size, initRnd, getCellWithDefault)


import Array exposing (Array)
import Maybe
import Random


type alias Matrix a = Array (Array a)


init : a -> Int -> Matrix a
init el size =
  Array.repeat size (Array.repeat size el)


size : Matrix a -> Int
size = Array.length


initRnd : (Float -> a) -> Int -> Int -> Matrix a
initRnd decisionFct seed size =
  Array.initialize size identity
    |> Array.foldl (\_ (list, seedIter) -> concatRndRow list size decisionFct seedIter) ([], Random.initialSeed seed)
    |> fst
    |> List.map Array.fromList
    |> Array.fromList


concatRndRow : List (List a) -> Int -> (Float -> a) -> Random.Seed -> (List (List a), Random.Seed)
concatRndRow list size decision seed =
  let
    (newRow, newSeed) = rndRow size decision seed
  in
    (newRow :: list, newSeed)


rndRow : Int -> (Float -> a) -> Random.Seed -> (List a, Random.Seed)
rndRow size decision seed =
  Random.float 0 1
    |> Random.map decision
    |> Random.list size
    |> (flip Random.step) seed


getCellWithDefault: a -> Matrix a -> Int -> Int -> a
getCellWithDefault default matrix rowIdx colIdx =
  let
    maybeRow = Array.get rowIdx matrix
  in
    Maybe.andThen maybeRow (Array.get colIdx)
    |> Maybe.withDefault default