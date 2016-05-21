module Cytoacoustics exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Array exposing (Array)
import Debug exposing (log)



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


-- MODEL


type alias Model = Array (Array Bool)


init : (Model, Cmd a)
init =
  (Array.repeat 16 (Array.repeat 16 False), Cmd.none)


-- UPDATE


type alias Switch = { row : Int, col : Int }

type Msg = SwitchMsg Switch

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    xx = log "onClick" msg
  in
    case msg of
      SwitchMsg sw ->
        (mapElement sw.row model (\row -> mapElement sw.col row not), Cmd.none)


mapElement: Int -> Array a -> (a -> a) -> Array a
mapElement idx arr updater =
    Array.get idx arr
      |> Maybe.map (\value -> Array.set idx value arr)
      |> Maybe.withDefault arr


-- SUBSCRIPTIONS


--subscriptions : Model -> Sub Msg
--subscriptions model =
--  Time.every second Tick



-- VIEW
viewCell : Int -> Int -> Bool -> Html Msg
viewCell row col cell =
    let
      msg = Switch row col
    in
    td [class (if cell then "on" else "off"), onClick (SwitchMsg msg)] []


viewRow : Int -> Array Bool -> Html Msg
viewRow row cells =
     cells
        |> Array.indexedMap (viewCell row)
        |> Array.toList
        |> tr []

view : Model -> Html Msg
view model =
      model
        |> Array.indexedMap viewRow
        |> Array.toList
        |> table []
