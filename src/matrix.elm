module Matrix exposing (Matrix, Msg(..), init, initRnd, update, updateNoDiff, size)


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


updateNoDiff : Msg -> Matrix -> Matrix
updateNoDiff msg matrix = case msg of
  Toggle row col ->
      updateRowCol row col matrix
  NextGeneration ->
    nextGeneration matrix
  Ship ship row col ->
    addShip matrix ship row col


msgWithShip : Msg ->  Maybe(String) -> Msg
msgWithShip msg ship =
    case msg of
      Toggle row col ->
        ship
            |> Maybe.map (\s -> Ship s row col)
            |> Maybe.withDefault msg
      _ -> msg


update : Msg -> Matrix -> Maybe(String) -> String -> (Matrix, List (Int, Int))
update msg matrix ship mode =
  let newMatrix = updateNoDiff (msgWithShip msg ship) matrix
  in
    if mode == "Diff"
    then ( newMatrix, diff matrix newMatrix )
    else if mode == "Full"
    then ( newMatrix, listActiveCells newMatrix )
    else (newMatrix, [])


updateRowCol : Int -> Int -> Matrix -> Matrix
updateRowCol rowIdx colIdx matrix =
  mapElement rowIdx matrix (\row -> mapElement colIdx row not)


mapElement: Int -> Array a -> (a -> a) -> Array a
mapElement idx arr updater =
  Array.get idx arr
    |> Maybe.map (\value -> Array.set idx (updater value) arr)
    |> Maybe.withDefault arr


nextGeneration: Matrix -> Matrix
nextGeneration matrix =
  Array.indexedMap (\x row ->
    Array.indexedMap (\y _ ->
      nextCell matrix x y) row) matrix


nextCell: Matrix -> Int -> Int -> Bool
nextCell matrix rowIdx colIdx =
  let
    originalValue = getCell matrix rowIdx colIdx
    size = Array.length matrix
  in
    List.concatMap (\n -> List.map (\m -> ((rowIdx + n + size) % size, (colIdx + m + size) % size)) [-1, 0, 1]) [-1, 0, 1]
      |> List.filter (\p -> (not ((fst p) == rowIdx && (snd p) == colIdx)))  --filter if m and n == 0
      |> List.filter (\p -> getCell matrix (fst p) (snd p))
      |> List.length
      |> (\l -> (originalValue && l > 1 && l < 4) || l == 3)


getCell =
  getCellWithDefault False


getCellWithDefault: Bool -> Matrix -> Int -> Int -> Bool
getCellWithDefault default matrix rowIdx colIdx =
  matrix
    |> Array.get rowIdx
    |> Maybe.withDefault (Array.repeat (Array.length matrix ) False)
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


listActiveCells : Matrix -> List (Int, Int)
listActiveCells matrix =
     matrix
        |> Array.toIndexedList
        |> List.map (\(idx, arr) -> (idx, Array.toIndexedList arr))
        |> List.concatMap (\(row, list) -> List.map (\(col, alive) -> (row, col, alive)) list)
        |> List.filter (\(row, col, alive) -> alive)
        |> List.map  (\(row, col, alive) -> (row, col))


addShip: Matrix -> String -> Int -> Int -> Matrix
addShip matrix ship row col =
    case (Dict.get ship ships) of
        Maybe.Nothing -> matrix
        Maybe.Just structure -> applyShip matrix row col structure


applyShip: Matrix -> Int -> Int -> Structure -> Matrix
applyShip matrix row col ship =
    let
        siz = (size matrix)
    in case ship of
        [] -> matrix
        (rowMod, colMod) :: tail -> applyShip (updateRowCol ((row + rowMod+ siz) % siz) ((col + colMod+ siz) % siz) matrix) row col tail


type alias Structure = List (Int, Int)

-- Structures turned DOWN
gliderDOWN = [(0,0), (-1,0), (-2, 0), (0, 1), (-1, 2)]
gunDOWN = [(1,5), (1,6), (2,5), (2,6), (11,5), (11,6), (11,7), (12,4), (12,8), (13,3), (13,9), (14,3), (14,9), (15,6), (16,4), (16,8), (17,5), (17,6), (17,7),
            (18, 6), (21,3), (21,4), (21,5),(22,3), (22,4), (22,5), (23,2), (23,6), (25,2), (25,6), (25,1), (25,7), (35,3), (35,4), (36,3), (36,4)]


-- Dict String Structure
ships = Dict.fromList [("Glider UP", rotateLeft (rotateLeft gliderDOWN)), ("Glider DOWN", gliderDOWN), ("Glider LEFT", rotateRight gliderDOWN), ("Glider RIGHT", rotateLeft gliderDOWN),
            ("Gun UP", rotateLeft (rotateLeft gunDOWN)), ("Gun DOWN", gunDOWN), ("Gun LEFT", rotateRight gunDOWN), ("Gun RIGHT", rotateLeft gunDOWN)]


rotateLeft: Structure -> Structure
rotateLeft struct =
    struct
        |> List.map (\(x, y) -> (-1*y, x))


rotateRight: Structure -> Structure
rotateRight struct =
    struct
        |> List.map (\(x, y) -> (y, -1*x))


mirror: Structure -> Structure
mirror struct =
    struct
        |> List.map (\(x, y) -> (-1*x, y))
