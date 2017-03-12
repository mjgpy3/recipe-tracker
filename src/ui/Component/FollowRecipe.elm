module Component.FollowRecipe exposing (Model(..), Msg(..), load, view)

import Html exposing (div)

type Model
  = Hole

type Msg
  = HoleMsg

load recipeName =
  (Hole, Cmd.none)

view model =
  div []
    [
    ]
