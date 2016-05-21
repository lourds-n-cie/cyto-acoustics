module Cytoacoustics exposing (..)

import Html exposing (..)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)



main =
  Html.program
    { init = init
    , view = view
    , update = \_ m -> (m, Cmd.none)
    , subscriptions = \_ -> Sub.none
    }


-- MODEL


type alias Model = List (List Bool)


init : (Model, Cmd a)
init =
  (List.repeat 16 (List.repeat 16 False), Cmd.none)


-- UPDATE


--type Msg
--  = Tick Time
--
--
--update : Msg -> Model -> (Model, Cmd Msg)
--update action model =
--  case action of
--    Tick newTime ->
--      (newTime, Cmd.none)



-- SUBSCRIPTIONS


--subscriptions : Model -> Sub Msg
--subscriptions model =
--  Time.every second Tick



-- VIEW
viewCell : Bool -> Html a
viewCell cell =
    td [class (if cell then "on" else "off")] []


viewRow : List Bool -> Html a
viewRow cells =
     cells
        |> List.map viewCell
        |> tr []

view : Model -> Html a
view model =
      model
        |> List.map viewRow
        |> table []
