module Component.SelectUser exposing (view, Msg(SelectUser))

import Html exposing (button, div, text, header, h1)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import User exposing (..)
import Config exposing (..)

type Msg
  = SelectUser User

selectUserButton (name, role) = button [class "btn btn-primary btn-block", onClick (SelectUser (name, role))] [text name]

view =
  div []
    [ header [class "bar bar-nav"]
      [h1 [class "title"] [text "Who Are You?"]]
    , div [class "content content-padded"]
      (List.map selectUserButton users)
    ]
