import Html exposing (Html, button, div, text, header, h1, h2, form, input, label, span, ul, li)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Dec
import User exposing (..)
import Component.AddRecipe as AddRecipe
import Component.SelectUser as SelectUser
import Component.Welcome as Welcome

main =
  Html.program { init = (NoneYet, Cmd.none), view = view, update = update, subscriptions = subscriptions }

subscriptions model = Sub.none

type Model
  = NoneYet
  | Error String
  | Welcome User
  | AddRecipeFor AddRecipe.Model
  | FindRecipeToFollow User (List String)

type Msg
  = SelectUserMsg SelectUser.Msg
  | WelcomeMsg Welcome.Msg
  | AddRecipeMsg AddRecipe.Msg
  | ErrorOccured String
  | SelectFollowRecipeFrom User (List String)

getRecipeNames : Http.Request (List String)
getRecipeNames =
  Http.get "http://localhost:3000/recipes" (Dec.list Dec.string)

update msg model =
  case (msg, model) of
    (SelectUserMsg (SelectUser.SelectUser user), _) ->
      (Welcome user, Cmd.none)

    (WelcomeMsg Welcome.SelectFollowRecipe, Welcome user) ->
      let
        handle response =
          case response of
            Ok recipeNames -> SelectFollowRecipeFrom user recipeNames
            Err _ -> ErrorOccured "Sorry! Something broke while I was finding recipes."
      in
      (FindRecipeToFollow user [], Http.send handle getRecipeNames)

    (SelectFollowRecipeFrom user recipeNames, _) ->
      (FindRecipeToFollow user recipeNames, Cmd.none)

    (AddRecipeMsg AddRecipe.RecipeSaved, AddRecipeFor (user, _)) ->
      (Welcome user, Cmd.none)

    (AddRecipeMsg msg, AddRecipeFor model) ->
      let
        results = AddRecipe.update msg model
      in
        (AddRecipeFor <| Tuple.first results, Cmd.map AddRecipeMsg <| Tuple.second results)

    (WelcomeMsg Welcome.SelectAddRecipe, Welcome user) ->
      (AddRecipeFor (user, { name="", ingredients=[], steps=[] }), Cmd.none)

    (WelcomeMsg Welcome.SelectAddRecipe, FindRecipeToFollow user _) ->
      (AddRecipeFor (user, { name="", ingredients=[], steps=[] }), Cmd.none)

    (ErrorOccured message, _) ->
      (Error message, Cmd.none)

    v ->
      (Error "Something went wrong", Cmd.none)

view model =
  case model of
    NoneYet -> Html.map SelectUserMsg SelectUser.view

    Welcome user -> Html.map WelcomeMsg <| Welcome.view user

    FindRecipeToFollow user recipes -> viewFindRecipeToFollow user recipes
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

followRecipeButton recipe =
  li [class "table-view-cell"]
    [ text recipe
    , button [class "btn", attribute "type" "button"] [text "Follow"]
    ]

recipiesHeading =
  li [class "table-view-cell"] [h2 [] [text "Recipes"]]

withEmptyMessage rows =
  case rows of
    [header] ->
      header::[
        li [class "table-view-cell"]
          [ text "It looks like you don't have any recipes yet!"
          , button [class "btn btn-primary", onClick (WelcomeMsg Welcome.SelectAddRecipe)] [text "Add One"]
          ]
      ]
    all -> all

viewFindRecipeToFollow user recipes =
  div []
    [ header [class "bar bar-nav"]
      [h1 [class "title"] [text "Find Recipe to Follow"]]
    , div [class "content content-padded"]
      [
        div [class "card"]
          [ ul [class "table-view"]
              <| withEmptyMessage <| recipiesHeading::List.map followRecipeButton recipes
          ]
      ]
    ]
