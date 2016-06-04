module GameGrid exposing (Model, init, initRnd, Msg(NextGeneration, MouseDown), update, view)


import Array exposing (Array)
import Html exposing (Html, table, tr, td)
import Html.Events exposing (onClick, onMouseEnter, onMouseDown, onMouseUp)
import Html.Attributes exposing (class)
import Matrix exposing (Matrix)
import Patterns


type alias BMatrix = Matrix Bool


type alias Model = { matrix: BMatrix, clicked: Bool, pattern: Patterns.Model }


init size = Model (Matrix.init False size) False Patterns.init


initRnd seed size = Model (Matrix.initRnd (\f -> f > 0.75) seed size) False Patterns.init


type Msg =
  NextGeneration
  | MouseDown Bool
  | ToggleCell Int Int
  | PatternSelection Patterns.Msg


update : Msg -> Model -> (Model, List (Int, Float), List (Int, Float))
update msg model =
  case msg of
    NextGeneration ->
      matrixUpdate model nextGenerationPlus

    ToggleCell row col ->
      case model.pattern of
        Patterns.None ->
          let
            ( newMatrix, onCells, freshOnCells ) = updateRowCol row col model.matrix
          in
            ( {model | matrix = newMatrix}, onCells, freshOnCells)          
        _ ->
          ( {model | matrix = applyPattern model.matrix model.pattern row col}, [], [])

    MouseDown bool ->
      ( { model | clicked = bool }, [], [])

    PatternSelection pattern ->
      ( { model | pattern = pattern }, [], [])


applyPattern: BMatrix -> Patterns.Model -> Int -> Int -> BMatrix
applyPattern matrix pattern row col =
  Patterns.structureOf pattern
    |> applyStructure matrix (Matrix.size matrix) row col


applyStructure: BMatrix -> Int -> Int -> Int -> Patterns.Structure -> BMatrix
applyStructure matrix mSize row col ship =
  case ship of
    [] -> matrix
    (rowMod, colMod) :: tail -> applyStructure (tuple3fst (updateRowCol ((row + rowMod+ mSize) % mSize) ((col + colMod+ mSize) % mSize) matrix)) mSize row col tail


matrixUpdate model updateFct =
  let
    (newMatrix, onCells, freshOnCells) = updateFct model.matrix
  in
    ( {model | matrix = newMatrix}, onCells, freshOnCells)


updateRowCol : Int -> Int -> BMatrix -> (BMatrix, List (Int, Float), List (Int, Float))
updateRowCol rowIdx colIdx matrix =
  let
    mSize = Matrix.size matrix
    oldValue = Matrix.getCellWithDefault True matrix rowIdx colIdx
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


nextGeneration: BMatrix -> BMatrix
nextGeneration matrix =
  Array.indexedMap (nextRow matrix) matrix
    |> Array.map (\(arr,_,_,_) -> arr)


maybePrepend : List (Int, a) -> Int -> Maybe a -> List (Int, a)
maybePrepend l idx m = Maybe.withDefault l (Maybe.map (\v -> (idx, v) :: l) m)


nextGenerationPlus: BMatrix -> (BMatrix, List (Int, Float), List (Int, Float))
nextGenerationPlus matrix =
  let
    (matAsList, onCells, freshOnCells) =
      Array.indexedMap (nextRow matrix) matrix
        |> Array.foldr (\(row, rowIdx, rowOns, rowFreshOns) (list, ons, freshOns) -> (row :: list, maybePrepend ons rowIdx rowOns, maybePrepend freshOns rowIdx rowFreshOns)) ([],[],[])
  in
    (Array.fromList matAsList, onCells, freshOnCells)


nextRow : BMatrix -> Int -> Array Bool -> (Array Bool, Int, Maybe Float, Maybe Float)
nextRow matrix rowIdx row =
  let
    size = Matrix.size matrix
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


nextCell: BMatrix -> Int -> Int -> (Bool, Bool)
nextCell matrix rowIdx colIdx =
  let
    oldValue = getCell matrix rowIdx colIdx
    size = Matrix.size matrix
    newValue = List.concatMap (\n -> List.map (\m -> ((rowIdx + n + size) % size, (colIdx + m + size) % size)) [-1, 0, 1]) [-1, 0, 1]
      |> List.filter (\p -> (not ((fst p) == rowIdx && (snd p) == colIdx)))  --filter if m and n == 0
      |> List.filter (\p -> getCell matrix (fst p) (snd p))
      |> List.length
      |> (\l -> (oldValue && l > 1 && l < 4) || l == 3)
    in
      ( oldValue, newValue )


getCell =
  Matrix.getCellWithDefault False


tuple3fst : (a,b,c) -> a
tuple3fst (x,_,_) = x


maybeToList : Maybe a -> List a
maybeToList m =
  case m of
    Just x -> [x]
    Nothing -> []


-- VIEW


viewCell : Bool -> Int -> Int -> Bool -> Html Msg
viewCell mouseDown row col cell =
  let
    allAttributes =
      [ onMouseEnter (ToggleCell row col)
      , class (if cell then "on noselect" else "off noselect")
      , onClick (ToggleCell row col)
      ]
    attributes = (if mouseDown then identity else (List.drop 1)) allAttributes
  in
    td
      attributes
      []


viewRow : Bool -> Int -> Array Bool -> Html Msg
viewRow mouseDown row cells =
  cells
    |> Array.indexedMap (viewCell mouseDown row)
    |> Array.toList
    |> tr [ class "noselect" ]


view : Model -> Html Msg
view model =
  Array.indexedMap (viewRow model.clicked) model.matrix
    |> Array.toList
    |> table
      [ class "noselect"
      , onMouseDown (MouseDown True)
      , onMouseUp (MouseDown False)
      ]