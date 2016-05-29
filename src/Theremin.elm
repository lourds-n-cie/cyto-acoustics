port module Theremin exposing (Model, Msg, init, update, subscriptions, view)


import Html exposing (..)
import Html.Attributes exposing (class, id, type', checked, for)
import Html.Events exposing (onCheck)
import Mouse exposing (Position)
import Window exposing (Size)
import Task


type Msg =
  MouseMove Position
  | ScreenResize Size
  | ToggleTheremin


type alias NormedMousePosition =
  { x : Float
  , y : Float
  }


type alias Model = 
  { screenSize: Size
  , mousePosition: Position
  , status: Bool
  }


init : ( Model, Cmd Msg )
init = ( Model (Size 0 0) (Position 0 0) False, getCurrentScreenSize )


getCurrentScreenSize =
  Task.perform (\_ -> ScreenResize (Size 0 0)) ScreenResize Window.size


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MouseMove position ->
      notifyAudio { model | mousePosition = position }

    ScreenResize size ->
      notifyAudio { model | screenSize = size }

    ToggleTheremin ->
      ( { model | status = not model.status }, modulation (NormedMousePosition (1/3) 1) )


notifyAudio : Model -> (Model, Cmd Msg)
notifyAudio model =
  (model, modulation (normed model.mousePosition model.screenSize))


normed : Position -> Size -> NormedMousePosition
normed {x, y} {width, height} =
  let
    xNormed = (toFloat x) / (toFloat width)
    yNormed = (toFloat (height - y)) / (toFloat height)
  in
    NormedMousePosition xNormed yNormed


port modulation : NormedMousePosition -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.status then
    Sub.batch [ Mouse.moves MouseMove, Window.resizes ScreenResize ]
  else
    Sub.none


view : Model -> Html Msg
view model =
  section
    []
    [ h3 [] [ text "Theremin" ]
    , div
      [ class "onoffswitch" ]
      [ input
        [ type' "checkbox"
        , id "thereminswitch"
        , class "onoffswitch-checkbox"
        , onCheck (always ToggleTheremin)
        , checked model.status
        ]
        []
      , label
        [ class "onoffswitch-label"
        , for "thereminswitch" ]
        [ span [ class "onoffswitch-inner" ] []
        , span [ class "onoffswitch-switch" ] []
        ]
      ]
    , small [] [ text ( if model.status then "Move the mouse to modulate the sounds" else "Moving the mouse has no effect on the sound" ) ]
    ]