import Html exposing (Html, button, div, text, header, h1)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

main =
  Html.beginnerProgram { model = NoneYet, view = view, update = update }

type alias ShortName = String
type Role = Chef | Diner

type alias User = (ShortName, Role)

type Model
  = NoneYet
  | Welcome User

type Msg = SelectUser User

update msg model =
  case msg of
    SelectUser user -> Welcome user

users =
  [ ("Thelma", Chef)
  , ("Jimmy", Diner)
  , ("Guest", Diner)
  ]

view model =
  case model of
    NoneYet -> viewSelectUser model

    Welcome user -> viewWelcome user

selectUserButton (name, role) = button [class "btn btn-primary btn-block", onClick (SelectUser (name, role))] [text name]

viewSelectUser model =
  div []
    [ header [class "bar bar-nav"]
      [h1 [class "title"] [text "Who Are You?"]]
    , div [class "content content-padded"]
      (List.map selectUserButton users)
    ]

roleText role =
  case role of
    Diner -> "diner"
    Chef -> "chef"

viewWelcome (name, role) =
  div []
    [ header [class "bar bar-nav"]
      [h1 [class "title"] [text ("Welcome " ++ roleText role ++ " " ++ name)]]
    , div [class "content content-padded"]
      []
    ]
