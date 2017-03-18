module Component.SelectRecipeToFollow exposing (Model(Loaded), view, Msg(..), update, load)

import Html exposing (button, div, text, header, h1, h2, ul, li)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Dec
import User exposing (..)
import Config exposing (route)

type Model
  = Unloaded
  | Loaded User (List Summary)

type alias Summary = { name: String, overallTime: String }

makeSummary name time = { name=name, overallTime=time }

type Msg
  = AddRecipe
  | RecipesLoaded User (List Summary)
  | FollowRecipeNamed User String
  | ErrorWhileLoading

decodeSummary =
  Dec.map2
    makeSummary
    (Dec.field "name" Dec.string)
    (Dec.field "overallTime" Dec.string)

getRecipeSummaries : Http.Request (List Summary)
getRecipeSummaries =
  Http.get (route "/recipes") (Dec.list decodeSummary)

followRecipeButton user recipe =
  li [class "table-view-cell"]
    [ text (recipe.name ++ " (" ++ recipe.overallTime ++ ")")
    , button [class "btn", attribute "type" "button", onClick (FollowRecipeNamed user recipe.name)] [text "Follow"]
    ]

recipesHeading =
  li [class "table-view-cell"] [h2 [] [text "Recipes"]]

withEmptyMessage rows =
  case rows of
    [header] ->
      header::[
        li [class "table-view-cell"]
          [ text "It looks like you don't have any recipes yet!"
          , button [class "btn btn-primary", onClick AddRecipe] [text "Add One"]
          ]
      ]
    all -> all

load user =
  let
    handle response =
      case response of
        Ok recipes -> RecipesLoaded user recipes
        Err _ -> ErrorWhileLoading
  in
    (Unloaded, Http.send handle getRecipeSummaries)
  
update msg model =
  case msg of
    RecipesLoaded user recipes ->
      (Loaded user recipes, Cmd.none)
    _ ->
      (model, Cmd.none)

view model =
  div []
    [ header [class "bar bar-nav"]
      [h1 [class "title"] [text "Find Recipe to Follow"]]
    , div [class "content content-padded"]
      [
        div [class "card"]
          [ ul [class "table-view"]
              (case model of
                Unloaded -> [li [class "table-view-cell"] [text "Loading"]]
                Loaded user recipes -> withEmptyMessage <| recipesHeading::List.map (followRecipeButton user) recipes)
          ]
      ]
    ]
