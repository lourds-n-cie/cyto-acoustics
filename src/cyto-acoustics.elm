port module Cytoacoustics exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onClick, onMouseEnter)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Array exposing (Array)
import Debug exposing (log)
import Mouse


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model = { matrix: Array (Array Bool), clicked: Bool }


init : (Model, Cmd a)
init =
  (Model (Array.repeat 16 (Array.repeat 16 False)) False, Cmd.none)


-- UPDATE


type alias Switch = { row : Int, col : Int }


type Msg = SwitchMsg Switch
  | DragMsg Switch
  | MickeyDown
  | MickeyUp
  | Clear


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear -> init
    SwitchMsg sw ->
      ( { model | matrix = mapElement sw.row model.matrix (\row -> mapElement sw.col row not) }, Cmd.none)
    DragMsg sw ->
      if model.clicked then
        ( { model | matrix = mapElement sw.row model.matrix (\row -> mapElement sw.col row not) }, Cmd.none)
      else
        (model, Cmd.none)
    MickeyDown -> ( { model | clicked = True }, Cmd.none)
    MickeyUp -> ( { model | clicked = False }, Cmd.none)


mapElement: Int -> Array a -> (a -> a) -> Array a
mapElement idx arr updater =
    Array.get idx arr
      |> Maybe.map (\value -> Array.set idx (updater value) arr)
      |> Maybe.withDefault arr


-- SUBSCRIPTIONS


-- incoming values
port reset : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [ Mouse.downs (always MickeyDown), Mouse.ups (always MickeyUp), reset (always Clear) ]


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
