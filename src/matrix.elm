port module Matrix exposing (Matrix, Msg(..), init, initRnd, update, size)


import Array exposing (Array)
import Random
import Dict


type alias Matrix = Array (Array Bool)


init : Int -> Matrix
init size =
  Array.repeat size (Array.repeat size False)


size : Matrix -> Int
size = Array.length


initRnd : Int -> Int -> Matrix
initRnd baseSeed size =
  Array.initialize size identity
    |> Array.foldl (\_ (list, seed) -> concatRndRow list size (\f -> f > 0.75) seed) ([], Random.initialSeed baseSeed)
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


type Msg =
  Toggle Int Int
  | NextGeneration
  | Ship String Int Int


tuple3fst : (a,b,c) -> a
tuple3fst (x,_,_) = x


maybeToList : Maybe a -> List a
maybeToList m =
  case m of
    Just x -> [x]
    Nothing -> []


update : Msg -> Matrix -> (Matrix, List (Int, Float), List (Int, Float))
update msg matrix =
  case msg of
    Toggle row col ->
      updateRowCol row col matrix
    NextGeneration ->
      nextGenerationPlus matrix
    Ship ship row col ->
      (addShip matrix ship row col, [], [])


updateRowCol : Int -> Int -> Matrix -> (Matrix, List (Int, Float), List (Int, Float))
updateRowCol rowIdx colIdx matrix =
  let
    mSize = size matrix
    oldValue = getCellWithDefault True matrix rowIdx colIdx
    onCells = if oldValue then [] else [colIdx]
    toList onCells = Maybe.map (\f -> (rowIdx, f)) (toBalance mSize onCells)
      |> maybeToList
  in
    (mapElement rowIdx matrix (\row -> mapElement colIdx row not), toList onCells, toList onCells)


mapElement: Int -> Array a -> (a -> a) -> Array a
mapElement idx arr updater =
  Array.get idx arr
    |> Maybe.map (\value -> Array.set idx (updater value) arr)
    |> Maybe.withDefault arr


nextGeneration: Matrix -> Matrix
nextGeneration matrix =
  Array.indexedMap (nextRow matrix) matrix
    |> Array.map (\(arr,_,_,_) -> arr)


maybePrepend : List (Int, a) -> Int -> Maybe a -> List (Int, a)
maybePrepend l idx m = Maybe.withDefault l (Maybe.map (\v -> (idx, v) :: l) m)


nextGenerationPlus: Matrix -> (Matrix, List (Int, Float), List (Int, Float))
nextGenerationPlus matrix =
  let
    (matAsList, onCells, freshOnCells) =
      Array.indexedMap (nextRow matrix) matrix
        |> Array.foldr (\(row, rowIdx, rowOns, rowFreshOns) (list, ons, freshOns) -> (row :: list, maybePrepend ons rowIdx rowOns, maybePrepend freshOns rowIdx rowFreshOns)) ([],[],[])
  in
    (Array.fromList matAsList, onCells, freshOnCells)


nextRow : Matrix -> Int -> Array Bool -> (Array Bool, Int, Maybe Float, Maybe Float)
nextRow matrix rowIdx row =
  let
    size = Array.length matrix
    (newRowList, onCells, freshOnCells) =
      Array.indexedMap (\colIdx _ -> (colIdx, nextCell matrix rowIdx colIdx)) row
        |> Array.foldr (\(colIdx, (oldValue, newValue)) (list, ons, freshOns) -> (newValue :: list, if newValue then colIdx :: ons else ons, if newValue && not oldValue then colIdx :: freshOns else freshOns) ) ([], [], [])
  in
    (Array.fromList newRowList, rowIdx, toBalance size onCells, toBalance size freshOnCells)


-- given a set of indices (from a row), and the size of the row, computes the 'balance' of the row, as a float between -1 and 1.
-- for size 3, [0] will return -1, [1] will return 0, [2] will return 1.
toBalance : Int -> List Int -> Maybe Float
toBalance rowSize rowIndices =
  let
    norm = (toFloat (rowSize-1)) / 2
  in
    case rowIndices of
      [] -> Maybe.Nothing
      _ ->
        List.map (\x -> (toFloat x) - norm) rowIndices
          |> List.foldl (\x y -> x + y) 0
          |> Maybe.Just
          |> Maybe.map (\x -> x / (toFloat (List.length rowIndices)) / norm)


nextCell: Matrix -> Int -> Int -> (Bool, Bool)
nextCell matrix rowIdx colIdx =
  let
    oldValue = getCell matrix rowIdx colIdx
    size = Array.length matrix
    newValue = List.concatMap (\n -> List.map (\m -> ((rowIdx + n + size) % size, (colIdx + m + size) % size)) [-1, 0, 1]) [-1, 0, 1]
      |> List.filter (\p -> (not ((fst p) == rowIdx && (snd p) == colIdx)))  --filter if m and n == 0
      |> List.filter (\p -> getCell matrix (fst p) (snd p))
      |> List.length
      |> (\l -> (oldValue && l > 1 && l < 4) || l == 3)
    in
      ( oldValue, newValue )


getCell =
  getCellWithDefault False


getCellWithDefault: Bool -> Matrix -> Int -> Int -> Bool
getCellWithDefault default matrix rowIdx colIdx =
  matrix
    |> Array.get rowIdx
    |> Maybe.withDefault (Array.repeat (Array.length matrix) False)
    |> Array.get colIdx
    |> Maybe.withDefault default


diff : Matrix -> Matrix -> List (Int, Int)
diff oldMatrix newMatrix =
  List.map2 (\(row, oldRow) (_, newRow) -> diffRows row oldRow newRow) (Array.toIndexedList oldMatrix) (Array.toIndexedList newMatrix)
    |> List.concatMap identity


diffRows : Int -> Array Bool -> Array Bool -> List (Int, Int)
diffRows row oldRow newRow =
  List.map2 (\(col, oldValue) (_, newValue) -> if newValue && oldValue /= newValue then [(row, col)] else []) (Array.toIndexedList oldRow) (Array.toIndexedList newRow)
    |> List.concatMap identity


addShip: Matrix -> String -> Int -> Int -> Matrix
addShip matrix ship row col =
  case (Dict.get ship ships) of
    Maybe.Nothing -> matrix
    Maybe.Just structure -> applyShip matrix (size matrix) row col structure


applyShip: Matrix -> Int -> Int -> Int -> Structure -> Matrix
applyShip matrix mSize row col ship =
  case ship of
    [] -> matrix
    (rowMod, colMod) :: tail -> applyShip (tuple3fst (updateRowCol ((row + rowMod+ mSize) % mSize) ((col + colMod+ mSize) % mSize) matrix)) mSize row col tail


type alias Structure = List (Int, Int)


-- Structures turned DOWN
gliderDOWN = [(0,0), (-1,0), (-2, 0), (0, 1), (-1, 2)]
gunDOWN = [(1,5), (1,6), (2,5), (2,6), (11,5), (11,6), (11,7), (12,4), (12,8), (13,3), (13,9), (14,3), (14,9), (15,6), (16,4), (16,8), (17,5), (17,6), (17,7),
            (18, 6), (21,3), (21,4), (21,5),(22,3), (22,4), (22,5), (23,2), (23,6), (25,2), (25,6), (25,1), (25,7), (35,3), (35,4), (36,3), (36,4)]


-- Dict String Structure
ships = Dict.fromList [("Glider NE", rotateLeft (rotateLeft gliderDOWN)), ("Glider SW", gliderDOWN), ("Glider NW", rotateRight gliderDOWN), ("Glider SE", rotateLeft gliderDOWN),
            ("Gun UP", rotateLeft (rotateLeft gunDOWN)), ("Gun DOWN", gunDOWN), ("Gun LEFT", rotateRight gunDOWN), ("Gun RIGHT", rotateLeft gunDOWN)]


rotateLeft: Structure -> Structure
rotateLeft =
  List.map (\(x, y) -> (-1*y, x))


rotateRight: Structure -> Structure
rotateRight =
  List.map (\(x, y) -> (y, -1*x))


mirror: Structure -> Structure
mirror =
  List.map (\(x, y) -> (-1*x, y))