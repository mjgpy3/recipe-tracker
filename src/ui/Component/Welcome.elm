module Component.Welcome exposing (view, Msg(..), Model)

import Html exposing (button, div, text, header, h1, span)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import User exposing (..)

type alias Model = User

type Msg
  = SelectAddRecipe
  | SelectFollowRecipe

roleActions (name, role) =
  let
    actionButton (icon, label, action) =
      button [class "btn btn-primary btn-block", onClick action]
        [ span [class ("icon icon-" ++ icon)] []
        , text (" " ++ label)
        ]
  in
    case role of
      Chef ->
        List.map actionButton
          [ ("search", "Find A Recipe", SelectFollowRecipe)
          , ("plus", "Add Recipe", SelectAddRecipe)
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
