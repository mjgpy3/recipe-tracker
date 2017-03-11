module Component.Welcome exposing (view, Msg(..))

import Html exposing (button, div, text, header, h1)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import User exposing (..)

type Msg
  = SelectAddRecipe
  | SelectFollowRecipe

roleActions (name, role) =
  let
    actionButton (label, action) =
      button [class "btn btn-primary btn-block", onClick action] [text label]
  in
    case role of
      Chef ->
        List.map actionButton
          [ ("Follow Recipe", SelectFollowRecipe)
          , ("Add Recipe", SelectAddRecipe)
          ]

      Diner ->
        [
        ]

view (name, role) =
  div []
    [ header [class "bar bar-nav"]
      [h1 [class "title"] [text ("Welcome " ++ roleText role ++ " " ++ name)]]
    , div [class "content content-padded"]
      (roleActions (name, role))
    ]
