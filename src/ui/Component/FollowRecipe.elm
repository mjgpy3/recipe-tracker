module Component.FollowRecipe exposing (Model(..), Msg(..), load, view, update)

import Html exposing (div, text, h1, ul, li, header)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing (field, map7, list, string, maybe, int)

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

type Model
  = Loading String
  | Following Recipe

type Msg
  = Loaded Recipe
  | ErrorWhileLoading

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

handle response =
  case response of
    Ok recipe -> Loaded recipe
    Err _ -> ErrorWhileLoading

load recipeName =
  (Loading recipeName, Http.send handle <| getRecipeNamed recipeName)

update msg model =
  case msg of
    Loaded recipe ->
      (Following recipe, Cmd.none)
    _ ->
      (model, Cmd.none)

view model =
  div []
    [ header [class "bar bar-nav"]
      [h1 [class "title"]
        [ case model of
            Loading name -> text ("Follow " ++ name)
            Following recipe -> text ("Follow " ++ recipe.name)
        ]
      ]
    , div [class "content content-padded"]
      [
        div [class "card"]
          [ ul [class "table-view"]
              (case model of
                Loading _ -> [li [class "table-view-cell"] [text "Loading..."]]
                Following recipe -> [text "hi"])
          ]
      ]
    ]
