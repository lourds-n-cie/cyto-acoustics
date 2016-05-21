port module Cytoacoustics exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onMouseEnter)
import Array exposing (Array)
import Debug exposing (log)
import Time exposing (..)
import Mouse
import Set exposing (Set)


main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Matrix = Array (Array Bool)
type alias Model = { matrix: Matrix, clicked: Bool, size: Int, live: Bool }


init : Int -> (Model, Cmd a)
init size =
  (Model (Array.repeat size (Array.repeat size False)) False size False, Cmd.none)


-- UPDATE


type alias Switch =
  { row : Int, col : Int }


type Msg = SwitchMsg Switch
  | DragMsg Switch
  | MickeyDown
  | MickeyUp
  | Clear
  | NextStep
  | Tick
  | ToggleLive


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear -> init model.size
    SwitchMsg sw ->
      updateHelper sw model
    DragMsg sw ->
      if model.clicked then
        updateHelper sw model
      else
        (model, Cmd.none)
    MickeyDown -> ( { model | clicked = True }, Cmd.none)
    MickeyUp -> ( { model | clicked = False }, Cmd.none)
    NextStep -> updateNextGen model
    Tick -> if model.live then updateNextGen model else (model, Cmd.none)
    ToggleLive -> ( { model | live = not model.live }, Cmd.none)


updateHelper : Switch -> Model -> (Model, Cmd Msg)
updateHelper sw model =
  let
    newModel = { model | matrix = mapElement sw.row model.matrix (\row -> mapElement sw.col row not) }
  in
  ( newModel, diff model.matrix newModel.matrix )


updateNextGen : Model -> (Model, Cmd Msg)
updateNextGen model =
  let
    newModel = { model | matrix = nextGeneration model.matrix}
  in
    (newModel, diff model.matrix newModel.matrix)


diff : Matrix -> Matrix -> Cmd Msg
diff oldMatrix newMatrix =
  List.map2 (\(row, oldRow) (_, newRow) -> diffRows row oldRow newRow) (Array.toIndexedList oldMatrix) (Array.toIndexedList newMatrix)
    |> List.concatMap identity
    |> newCells


diffRows : Int -> Array Bool -> Array Bool -> List (Int, Int)
diffRows row oldRow newRow =
  List.map2 (\(col, oldValue) (_, newValue) -> if newValue && oldValue /= newValue then [(row, col)] else []) (Array.toIndexedList oldRow) (Array.toIndexedList newRow)
    |> List.concatMap identity


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


-- SUBSCRIPTIONS


-- incoming values
port reset : (String -> msg) -> Sub msg


port toggleLive : (String -> msg) -> Sub msg


port nextStep : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [
    Mouse.downs (always MickeyDown),
    Mouse.ups (always MickeyUp),
    reset (always Clear),
    nextStep (always NextStep),
    every (150*millisecond) (always Tick),
    toggleLive (always ToggleLive)
  ]


-- OUT PORTS


port newCells : List (Int, Int) -> Cmd msg


-- VIEW


viewCell : Int -> Int -> Bool -> Html Msg
viewCell row col cell =
  let
    msg = Switch row col
  in
    td [class (if cell then "on" else "off"), onClick (SwitchMsg msg), onMouseEnter (DragMsg msg)] []


viewRow : Int -> Array Bool -> Html Msg
viewRow row cells =
  cells
    |> Array.indexedMap (viewCell row)
    |> Array.toList
    |> tr []


view : Model -> Html Msg
view model =
  model.matrix
    |> Array.indexedMap viewRow
    |> Array.toList
    |> table []
