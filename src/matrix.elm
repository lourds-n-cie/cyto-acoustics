module Matrix exposing (Matrix, Msg(..), init, update, updateNoDiff, size)


import Array exposing (Array)


type alias Matrix = Array (Array Bool)


init : Int -> Matrix
init size =
  Array.repeat size (Array.repeat size False)


size : Matrix -> Int
size = Array.length


--initRnd : Int -> Matrix
--initRnd size =
--  ???


type Msg =
  Toggle Int Int
  | NextGeneration


updateNoDiff : Msg -> Matrix -> Matrix
updateNoDiff msg matrix = case msg of
  Toggle row col ->
      updateRowCol row col matrix
  NextGeneration ->
    nextGeneration matrix


update : Msg -> Matrix -> (Matrix, List (Int, Int))
update msg matrix =
  let newMatrix = updateNoDiff msg matrix
  in
    ( newMatrix, diff matrix newMatrix )


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