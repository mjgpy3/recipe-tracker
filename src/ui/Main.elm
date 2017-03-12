import Html exposing (Html)

import User exposing (..)

import Component.AddRecipe as AddRecipe
import Component.Error as Error
import Component.FollowRecipe as FollowRecipe
import Component.SelectRecipeToFollow as SelectRecipeToFollow
import Component.SelectUser as SelectUser
import Component.Welcome as Welcome

main =
  Html.program { init = (NoneYet, Cmd.none), view = view, update = update, subscriptions = subscriptions }

subscriptions model = Sub.none

type Model
  = NoneYet
  | Welcome Welcome.Model
  | AddRecipeFor AddRecipe.Model
  | FindRecipeToFollow SelectRecipeToFollow.Model
  | FollowRecipe FollowRecipe.Model
  | Error String

type Msg
  = SelectUserMsg SelectUser.Msg
  | WelcomeMsg Welcome.Msg
  | AddRecipeMsg AddRecipe.Msg
  | SelectRecipeToFollowMsg SelectRecipeToFollow.Msg
  | FollowRecipeMsg FollowRecipe.Msg
  | ErrorOccured String

disp fModel fCmd =
  Tuple.mapFirst fModel >> Tuple.mapSecond (Cmd.map fCmd)

update msg model =
  case (msg, model) of
    (SelectUserMsg (SelectUser.SelectUser user), _) ->
      (Welcome user, Cmd.none)

    (WelcomeMsg Welcome.SelectFollowRecipe, Welcome user) ->
      disp FindRecipeToFollow SelectRecipeToFollowMsg <| SelectRecipeToFollow.load user

    (SelectRecipeToFollowMsg (SelectRecipeToFollow.FollowRecipeNamed user name), _) ->
      disp FollowRecipe FollowRecipeMsg <| FollowRecipe.load user name

    (SelectRecipeToFollowMsg SelectRecipeToFollow.AddRecipe, FindRecipeToFollow (SelectRecipeToFollow.Loaded user _)) ->
      (AddRecipeFor (user, AddRecipe.empty, Nothing), Cmd.none)

    (SelectRecipeToFollowMsg msg, FindRecipeToFollow model) ->
      disp FindRecipeToFollow SelectRecipeToFollowMsg <| SelectRecipeToFollow.update msg model

    (AddRecipeMsg AddRecipe.RecipeSaved, AddRecipeFor (user, _, _)) ->
      (Welcome user, Cmd.none)

    (FollowRecipeMsg FollowRecipe.CookTracked, FollowRecipe (FollowRecipe.Following row)) ->
      (Welcome row.user, Cmd.none)

    (AddRecipeMsg msg, AddRecipeFor model) ->
      disp AddRecipeFor AddRecipeMsg <| AddRecipe.update msg model

    (FollowRecipeMsg FollowRecipe.ErrorWhileLoading, _) ->
      (Error "An error occured while loading the recipe", Cmd.none)

    (FollowRecipeMsg msg, FollowRecipe model) ->
      disp FollowRecipe FollowRecipeMsg <| FollowRecipe.update msg model

    (WelcomeMsg Welcome.SelectAddRecipe, Welcome user) ->
      (AddRecipeFor (user, AddRecipe.empty, Nothing), Cmd.none)

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
    FollowRecipe model -> Html.map FollowRecipeMsg <| FollowRecipe.view model

    Error message -> Error.view message
