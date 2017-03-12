module Component.FollowRecipe exposing (Model(..), Msg(..), load, view, update)

import Html exposing (div, text, h1, ul, li, header, label, i, button)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (field, map7, list, string, maybe, int)
import Json.Decode as Dec
import Json.Encode exposing (null)
import Fp exposing (..)
import User exposing (..)

type alias Recipe = {
  name: String,
  ingredients: List String,
  steps: List String,
  serves: Maybe Int,
  cookTime: Maybe Int,
  prepTime: Maybe Int,
  overallTime: Maybe Int
}

makeRecipe name ingredients steps serves cookTime prepTime overallTime =
  {
    name=name,
    ingredients=ingredients,
    steps=steps,
    serves=serves,
    cookTime=cookTime,
    prepTime=prepTime,
    overallTime=overallTime
  }

any = Dec.map (const ()) (maybe int)

postRecipeCooked : String -> Http.Request ()
postRecipeCooked recipeName =
  Http.post
    ("http://localhost:3000/recipe/" ++ recipeName ++ "/cooked")
    (Http.jsonBody null)
    any

postStartedToFollow : String -> Http.Request ()
postStartedToFollow recipeName =
  Http.post
    ("http://localhost:3000/recipe/" ++ recipeName ++ "/follow")
    (Http.jsonBody null)
    any

type Model
  = Loading User String
  | Following User Recipe

type Msg
  = Loaded User Recipe
  | ErrorWhileLoading
  | ActionTracked
  | RecipeCooked Recipe
  | CookTracked

decoder =
  map7
    makeRecipe
    (field "name" string)
    (field "ingredients" (list string))
    (field "steps" (list string))
    (field "serves" (maybe int))
    (field "cookTime" (maybe int))
    (field "prepTime" (maybe int))
    (field "overallTime" (maybe int))

getRecipeNamed : String -> Http.Request Recipe
getRecipeNamed name =
  Http.get ("http://localhost:3000/recipe/" ++ name) decoder

handleRecipeLoaded user response =
  case response of
    Ok recipe -> Loaded user recipe
    Err _ -> ErrorWhileLoading

load user recipeName =
  (Loading user recipeName, Http.send (handleRecipeLoaded user) <| getRecipeNamed recipeName)

update msg model =
  case msg of
    Loaded user recipe ->
      (Following user recipe, Http.send (const ActionTracked) <| postStartedToFollow recipe.name)
    RecipeCooked recipe ->
      (model, Http.send (const CookTracked) <| postRecipeCooked recipe.name)
    _ ->
      (model, Cmd.none)

loadingCell = 
  [ div [class "card"]
    [ ul [class "table-view"]
      [ li [class "table-view-cell"] [text "Loading..."]
      ]
    ]
  ]

listView name values =
  let
    rowEdit value =
      li [class "table-view-cell"]
        [text value]
  in
    div [class "card"]
      [ ul [class "table-view"]
          ([ li [class "table-view-cell"]
              [label [] [text name]]
          , li [class "table-view-divider"] []
          ] ++ (List.map rowEdit values))
      ]

getName model =
  case model of
    Loading _ name -> name
    Following _ recipe -> recipe.name

view model =
  div []
    [ header [class "bar bar-nav"]
      [h1 [class "title"]
        [ text "Follow ", i [] [ text <| getName model ]
        ]
      ]
    , div [class "content content-padded"]
      (case model of
        Loading _ _ ->
          loadingCell
        Following _ recipe ->
          [ listView "Ingredients" recipe.ingredients
          , listView "Steps" recipe.steps
          , button [class "btn btn-positive btn-block", attribute "type" "button", onClick (RecipeCooked recipe)] [text "Cooked It!"]
          ]
      )
    ]
