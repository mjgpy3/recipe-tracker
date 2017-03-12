import Html exposing (Html, div, text, header, h1, span, ul, li)
import Html.Attributes exposing (class)

import User exposing (..)

import Component.AddRecipe as AddRecipe
import Component.SelectUser as SelectUser
import Component.SelectRecipeToFollow as SelectRecipeToFollow
import Component.Welcome as Welcome

main =
  Html.program { init = (NoneYet, Cmd.none), view = view, update = update, subscriptions = subscriptions }

subscriptions model = Sub.none

type Model
  = NoneYet
  | Welcome User
  | AddRecipeFor AddRecipe.Model
  | FindRecipeToFollow SelectRecipeToFollow.Model
  | Error String

type Msg
  = SelectUserMsg SelectUser.Msg
  | WelcomeMsg Welcome.Msg
  | AddRecipeMsg AddRecipe.Msg
  | SelectRecipeToFollowMsg SelectRecipeToFollow.Msg
  | ErrorOccured String

update msg model =
  case (msg, model) of
    (SelectUserMsg (SelectUser.SelectUser user), _) ->
      (Welcome user, Cmd.none)

    (WelcomeMsg Welcome.SelectFollowRecipe, Welcome user) ->
      let
        results = SelectRecipeToFollow.load user
      in
        (FindRecipeToFollow <| Tuple.first results, Cmd.map SelectRecipeToFollowMsg <| Tuple.second results)

    (SelectRecipeToFollowMsg SelectRecipeToFollow.AddRecipe, FindRecipeToFollow (SelectRecipeToFollow.Loaded user _)) ->
      (AddRecipeFor (user, { name="", ingredients=[], steps=[] }), Cmd.none)

    (SelectRecipeToFollowMsg msg, FindRecipeToFollow model) ->
      let
        results = SelectRecipeToFollow.update msg model
      in
        (FindRecipeToFollow <| Tuple.first results, Cmd.map SelectRecipeToFollowMsg <| Tuple.second results)

    (AddRecipeMsg AddRecipe.RecipeSaved, AddRecipeFor (user, _)) ->
      (Welcome user, Cmd.none)

    (AddRecipeMsg msg, AddRecipeFor model) ->
      let
        results = AddRecipe.update msg model
      in
        (AddRecipeFor <| Tuple.first results, Cmd.map AddRecipeMsg <| Tuple.second results)

    (WelcomeMsg Welcome.SelectAddRecipe, Welcome user) ->
      (AddRecipeFor (user, { name="", ingredients=[], steps=[] }), Cmd.none)

    (ErrorOccured message, _) ->
      (Error message, Cmd.none)

    v ->
      (Error "Something went wrong", Cmd.none)

view model =
  case model of
    NoneYet -> Html.map SelectUserMsg SelectUser.view

    Welcome user -> Html.map WelcomeMsg <| Welcome.view user

    FindRecipeToFollow model -> Html.map SelectRecipeToFollowMsg <| SelectRecipeToFollow.view model
    AddRecipeFor model -> Html.map AddRecipeMsg <| AddRecipe.view model

    Error message -> viewError message

viewError message =
  div []
    [ header [class "bar bar-nav"]
      [h1 [class "title"] [text "Whoops..."]]
    , div [class "content content-padded"]
      [
        div [class "card"]
          [ ul [class "table-view"]
              [ li [class "table-view-cell"]
                  [ span [class "icon icon-info"] []
                  , text message
                  ]
              ]
          ]
      ]
    ]
