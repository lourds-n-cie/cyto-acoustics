module Patterns exposing (Model, Msg, Pattern(None), Structure, view, structureOf, init)


import List
import Html exposing (..)
import Html.Attributes exposing (class, type', id, name, value, for, checked)
import Html.Events exposing (onCheck)


type alias Structure = List (Int, Int)


type Rotation =
  Rot0
  | Rot90
  | Rot180
  | Rot270


type Pattern =
  None
  | Glider Rotation
  | Gun Rotation


type alias Model = Pattern


init : Model
init = None


type alias Msg = Pattern


gliderSW =
  [(0,0), (-1,0), (-2, 0), (0, 1), (-1, 2)]


gunS =
  [(1,5), (1,6), (2,5), (2,6), (11,5), (11,6), (11,7), (12,4), (12,8), (13,3), (13,9), (14,3), (14,9), (15,6), (16,4), (16,8), (17,5), (17,6), (17,7),
  (18, 6), (21,3), (21,4), (21,5),(22,3), (22,4), (22,5), (23,2), (23,6), (25,2), (25,6), (25,1), (25,7), (35,3), (35,4), (36,3), (36,4)]


structureOf pattern =
  case pattern of
    None ->
      [(0,0)]

    Glider rot ->
      applyRot rot gliderSW

    Gun rot ->
      applyRot rot gunS


applyRot : Rotation -> Structure -> Structure
applyRot rot structure =
  case rot of
    Rot0 ->
      structure

    Rot90 ->
      rotateLeft structure

    Rot180 ->
      rotate180 structure

    Rot270 ->
      rotateRight structure


rotateLeft =
  List.map (\(x, y) -> (-1*y, x))


rotateRight =
  List.map (\(x, y) -> (y, -1*x))


rotate180 =
  rotateLeft >> rotateLeft


view : Model -> Html Msg
view pattern =
  section
    [ class "brush" ]
    [ h3
      []
      [ text "Patterns" ]
    , small
      []
      [ em
        []
        [ text "What will be drawn on the grid when you click" ] ]
    , radioDiv pattern [ (None, "1 cell", "patternsingle") ] "Cells"
    , radioDiv pattern gliders "Gliders"
    , radioDiv pattern guns "Guns"
    ]


radioDiv : Pattern -> List (Pattern, String, String) -> String -> Html Msg
radioDiv selected patterns title =
  div
    [ class "radio-group" ]
    ( ( h4 [] [ text title ] ) :: ( List.concatMap (patternRadio selected) patterns ) )


patternRadio : Pattern -> (Pattern, String, String) -> List (Html Msg)
patternRadio selected (pattern, txt, ident) =
  [ input
    [ type' "radio"
    , name "brush"
    , class "patternradio"
    , id ident
    , value ident
    , checked (selected == pattern)
    , onCheck (always pattern)
    ]
    []
  , label
    [ class "pattern-selector"
    , for ident
    ]
    [ text txt ]
  ]


gliders =
  [ (Glider Rot180, "↗", "patterngne")
  , (Glider Rot90, "↘", "patterngse")
  , (Glider Rot0, "↙", "patterngsw")
  , (Glider Rot270, "↖", "patterngnw")
  ]


guns =
  [ (Gun Rot180, "↑", "patterngn")
  , (Gun Rot90, "→", "patternge")
  , (Gun Rot0, "↓", "patterngs")
  , (Gun Rot270, "←", "patterngw")
  ]