module Component.SelectRecipeToFollow exposing (Model(Loaded), view, Msg(..), update, load)

import Html exposing (button, div, text, header, h1, h2, ul, li)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Dec
import User exposing (..)

type Model
  = Unloaded
  | Loaded User (List String)

type Msg
  = AddRecipe
  | RecipesLoaded User (List String)
  | ErrorWhileLoading

getRecipeNames : Http.Request (List String)
getRecipeNames =
  Http.get "http://localhost:3000/recipes" (Dec.list Dec.string)

followRecipeButton recipe =
  li [class "table-view-cell"]
    [ text recipe
    , button [class "btn", attribute "type" "button"] [text "Follow"]
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
        Ok recipeNames -> RecipesLoaded user recipeNames
        Err _ -> ErrorWhileLoading
  in
    (Unloaded, Http.send handle getRecipeNames)
  
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
                Unloaded -> [li [class "table-view-cell"] [h2 [] [text "Loading"]]]
                Loaded user recipes -> withEmptyMessage <| recipesHeading::List.map followRecipeButton recipes)
          ]
      ]
    ]
