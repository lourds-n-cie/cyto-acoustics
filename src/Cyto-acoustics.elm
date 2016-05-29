port module Cytoacoustics exposing (..)


import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (class, autocomplete, method, id, style, for, value, type', disabled, checked)
import Html.Events exposing (onClick, onMouseUp, onCheck, on)
import Time exposing (every, millisecond)
import Task
import Array exposing (Array)
import GameGrid
import Matrix
import Patterns
import Theremin
import Json.Decode as Json
import String
import Mouse


main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { grid: GameGrid.Model
  , live: Bool
  , playAll: Bool
  , theremin: Theremin.Model
  }


init : Int -> (Model, Cmd Msg)
init size =
  let
    (theremin, thereminCmd) = Theremin.init
    grid = GameGrid.init size
  in
    ( Model grid False False theremin, Cmd.batch [getCurrentCentiseconds, Cmd.map ThereminMsg thereminCmd] )


getCurrentCentiseconds =
  Task.perform (\_ -> Clear) (\time -> CurrentSeconds (floor (time / 100) ) ) Time.now


-- UPDATE


type Msg =
  Tick
  | ResizeGame Int
  | Clear
  | Randomize
  | CurrentSeconds Int
  | ToggleLive
  | SwitchMode Bool
  | GridMsg GameGrid.Msg
  | PatternMsg Patterns.Msg
  | ThereminMsg Theremin.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ResizeGame size ->
      init size

    Randomize ->
      ( model, getCurrentCentiseconds )

    Clear ->
      ( { model | grid = GameGrid.init (Matrix.size model.grid.matrix) }, Cmd.none )

    CurrentSeconds timestamp ->
      ( { model | grid = GameGrid.initRnd timestamp (Matrix.size model.grid.matrix) }, Cmd.none )

    Tick ->
      update (GridMsg GameGrid.NextGeneration) model

    ToggleLive ->
      ( { model | live = not model.live }, Cmd.none)

    SwitchMode newMode ->
      ( { model | playAll = newMode }, Cmd.none )

    GridMsg gridMsg ->
      let
        (newGrid, onCells, freshOnCells) = GameGrid.update gridMsg model.grid
      in
        ( { model | grid = newGrid }, cellNotification model.playAll onCells freshOnCells )

    PatternMsg patternMsg ->
      let
        oldGrid = model.grid
      in
        ( { model | grid = { oldGrid | pattern = patternMsg } }, Cmd.none )

    ThereminMsg thereminMsg ->
      let
        (newTheremin, thereminCmd) = Theremin.update thereminMsg model.theremin
      in
        ( { model | theremin = newTheremin }, Cmd.map ThereminMsg thereminCmd)


cellNotification : Bool -> List (Int, Float) -> List (Int, Float) -> Cmd Msg
cellNotification playAll onCells freshOnCells =
  case playAll of
    False -> newCells freshOnCells
    True -> newCells onCells


port newCells : List (Int, Float) -> Cmd msg


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  let
    thereminSub = Theremin.subscriptions model.theremin
      |> Sub.map ThereminMsg
    ticks =
      if model.live then
        [ every (150*millisecond) (always Tick) ]
      else
        []
  in
    Sub.batch (
      thereminSub ::
      ticks ++
      [ Mouse.ups (always (GridMsg (GameGrid.MouseDown False))) ]
    )


-- VIEW


view : Model -> Html Msg
view model =
  div
    [ class "container" ]
    [ div
      [ class "col col1-3" ]
      [ form
        [ autocomplete False, method "GET" ]
        [ section
          []
          [ h3 [] [ text "Grid" ]
          , button
            [ type' "button"
            , onClick Randomize
            ]
            [ i [] [ text "⟳" ], text " New random grid" ]
          , button
            [ type' "button"
            , onClick Clear
            ]
            [ text "Clear" ]
          , div
            [ class "modeControl" ]
            [ div
              [ class "onoffswitch" ]
              [ input
                [ type' "checkbox"
                , id "playAllNotesToggle"
                , class "onoffswitch-checkbox"
                , onCheck SwitchMode
                , checked model.playAll
                ]
                []
              , label
                [ class "onoffswitch-label"
                , for "playAllNotesToggle"
                ]
                [ span [ class "onoffswitch-inner" ] []
                , span [ class "onoffswitch-switch" ] []
                ]
              ]
            , small [] [ em [] [ text ( if model.playAll then "Play sound for all cells" else "Play only sound of new cells" ) ] ]
            ]
          , label
            [ for "gridSizeSelector" ]
            [ text "Size: "]
          , select
            [ id "gridSizeSelector"
            , value (toString (Matrix.size model.grid.matrix))
            , on "change" (Json.map ResizeGame targetSelectedValue)
            ]
            [ option [ value "8" ] [ text "8 × 8" ]
            , option [ value "16" ] [ text "16 × 16" ]
            , option [ value "24" ] [ text "24 × 24" ]
            , option [ value "32" ] [ text "32 × 32" ]
            , option [ value "48" ] [ text "48 × 48" ]
            , option [ value "64" ] [ text "64 × 64" ]
            ]
          ]
        , section
          []
          [ h3 [] [ text "Iterations" ]
          , div
            [ class "controller" ]
            [ text "Auto: "
            , button
              [ type' "button"
              , if model.live then class "fa fa-pause" else class "fa fa-play"
              , onClick ToggleLive
              ]
              []
            , br [] []
            , text "Step-by-step: "
            , button
              [ type' "button"
              , class "fa fa-step-forward"
              , if model.live then disabled True else onClick Tick
              ]
              []
            ]
          ]
        , Html.map ThereminMsg (Theremin.view model.theremin)
        , Html.map PatternMsg (Patterns.view model.grid.pattern)
        ]
      ]
    , div
      [ class "col col2-3" ]
      [ div
        [ id "matrix"
        , class "matrix"
        ]
        [ Html.map GridMsg (GameGrid.view model.grid) ]
        ]
    ]


targetSelectedValue : Json.Decoder Int
targetSelectedValue =
  Json.at ["target", "value"] Json.string
    |> Json.map (String.toInt)
    |> Json.map (Result.withDefault 24)