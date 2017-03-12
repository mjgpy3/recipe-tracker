module Component.FollowRecipe exposing (Model(..), Msg(..), load, view, update)

import Html exposing (div, text, h1, ul, li, header, label, i, button, span)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (field, map7, list, string, maybe, int)
import Json.Decode as Dec
import Json.Encode exposing (null)
import Fp exposing (..)
import User exposing (..)

type Items = Ingredients | Steps

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
  | Following {user: User, recipe: Recipe, ingredientsExpanded: Bool, stepsExpanded: Bool }

type Msg
  = Loaded User Recipe
  | ErrorWhileLoading
  | ActionTracked
  | RecipeCooked Recipe
  | CookTracked
  | Expand Items
  | Collapse Items

decoder =
  map7
    makeRecipe
    (field "name" string)
    (field "ingredients" (list string))
    (field "steps" (list string))
    (maybe (field "serves" int))
    (maybe (field "cookTime" int))
    (maybe (field "prepTime" int))
    (maybe (field "overallTime" int))

getRecipeNamed : String -> Http.Request Recipe
getRecipeNamed name =
  Http.get ("http://localhost:3000/recipe/" ++ name) decoder

handleRecipeLoaded user response =
  case response of
    Ok recipe -> Loaded user recipe
    Err _ -> ErrorWhileLoading

load user recipeName =
  (Loading user recipeName, Http.send (handleRecipeLoaded user) <| getRecipeNamed recipeName)

init user recipe = 
  Following {user=user, recipe=recipe, ingredientsExpanded=True, stepsExpanded=True }

update msg model =
  case (msg, model) of
    (Loaded user recipe, _) ->
      (init user recipe, Http.send (const ActionTracked) <| postStartedToFollow recipe.name)

    (RecipeCooked recipe, _) ->
      (model, Http.send (const CookTracked) <| postRecipeCooked recipe.name)

    (Expand Ingredients, Following row) ->
      (Following { row | ingredientsExpanded=True }, Cmd.none)

    (Collapse Ingredients, Following row) ->
      (Following { row | ingredientsExpanded=False }, Cmd.none)

    (Expand Steps, Following row) ->
      (Following { row | stepsExpanded=True }, Cmd.none)

    (Collapse Steps, Following row) ->
      (Following { row | stepsExpanded=False }, Cmd.none)

    _ ->
      (model, Cmd.none)

loadingCell = 
  [ div [class "card"]
    [ ul [class "table-view"]
      [ li [class "table-view-cell"] [text "Loading..."]
      ]
    ]
  ]

listView name disc expanded values =
  let
    rowEdit value =
      li [class "table-view-cell"] [ text value ]
  in
    div [class "card"]
      [ ul [class "table-view"]
          (
            case (expanded, values) of
              (_, []) ->
                [ li [class "table-view-cell"]
                    [ label [] [text ("Recipe has no " ++ name)] ]
                ]
              (True, _) ->
                ([ li [class "table-view-cell"]
                    [ label [] [text name]
                    , button [class "btn", attribute "type" "button", onClick (Collapse disc)] [span [class "icon icon-down"] []]
                    ]
                , li [class "table-view-divider"] []
                ] ++ (List.map rowEdit values))
              (False, _) ->
                [ li [class "table-view-cell"]
                    [ label [] [text name]
                    , button [class "btn", attribute "type" "button", onClick (Expand disc)] [span [class "icon icon-right"] []]
                    ]
                ]
          )
      ]

getName model =
  case model of
    Loading _ name -> name
    Following row -> row.recipe.name

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
        Following row ->
          [ listView "Ingredients" Ingredients row.ingredientsExpanded row.recipe.ingredients
          , listView "Steps" Steps row.stepsExpanded row.recipe.steps
          , button [class "btn btn-positive btn-block", attribute "type" "button", onClick (RecipeCooked row.recipe)] [text "Cooked It!"]
          ]
      )
    ]
