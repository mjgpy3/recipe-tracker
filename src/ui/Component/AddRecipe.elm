module Component.AddRecipe exposing (update, Model, view, Msg(..))

import Html exposing (button, div, text, header, h1, form, input, label, span, ul, li)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Dec
import Json.Encode as Enc
import User exposing (..)
import Fp exposing (..)

type alias RecipeName = String

type Msg
  = SaveNewRecipe
  | ChangeNewRecipeName String
  | AddEmpty ItemDiscriminator
  | ChangeItem ItemDiscriminator Int String
  | RemoveItem ItemDiscriminator Int
  | RecipeSaved
  | ErrorWhileSaving

type ItemDiscriminator = IngredientItem | StepItem

type alias NewRecipe = {
  name: RecipeName,
  ingredients: List (Maybe (Int, String)),
  steps: List (Maybe (Int, String))
}

type alias Model = (User, NewRecipe)

decoder =
  Dec.string
    |> Dec.field "message"
    |> Dec.map (\message -> { message=message })

encode body =
  let
    encodeItems =
      List.filterMap id >>
      List.map Tuple.second >>
      List.map Enc.string >>
      Enc.list
  in
    Enc.object
      [ ("userName", Enc.string <| Tuple.first body.user)
      , ("role", Enc.string <| roleText <| Tuple.second body.user)
      , ("recipe", Enc.object <|
          [ ("name", Enc.string <| body.recipe.name)
          , ("ingredients", encodeItems body.recipe.ingredients)
          , ("steps", encodeItems body.recipe.steps)
          ])
      ]

type alias PostRecipeResponse = {
  message: String
}

type alias PostRecipeBody = { user: User, recipe: NewRecipe }

postRecipe : PostRecipeBody -> Http.Request PostRecipeResponse
postRecipe body =
  Http.post "http://localhost:3000/recipe" (Http.jsonBody <| encode body) decoder

listEdit title example discriminator items =
  let
    rowEdit item =
      case item of
        Just (index, _) ->
          li [class "table-view-cell"]
            [
              input [onInput (ChangeItem discriminator index), attribute "type" "text", attribute "placeholder" example] []
            , button [attribute "type" "button", onClick (RemoveItem discriminator index), class "btn btn-negative"] [span [class "icon icon-trash"] []]
            ]
        Nothing -> text ""
    addButton =
      li [class "table-view-cell"]
        [button [attribute "type" "button", class "btn", onClick (AddEmpty discriminator)] [span [class "icon icon-plus"] []]]
  in
    div [class "card"]
      [ ul [class "table-view"]
          ([ li [class "table-view-cell"]
              [label [] [text title]]
          , li [class "table-view-divider"] []
          ] ++ (List.map rowEdit items) ++ [addButton])
      ]

mapItems disc recipe fn =
  case disc of
    IngredientItem -> { recipe | ingredients=fn recipe.ingredients }
    StepItem -> { recipe | steps=fn recipe.steps }

update msg (user, recipe) =
  case msg of
    ChangeNewRecipeName newName ->
      ((user, { recipe | name=newName }), Cmd.none)

    ChangeItem disc valueIndex newValue ->
      let
        replaceValue = Maybe.map (\(index, value) -> if index == valueIndex then (index, newValue) else (index, value))
      in
        ((user, mapItems disc recipe (List.map replaceValue)), Cmd.none)

    AddEmpty disc ->
      let
        addEmptyToEnd values = values ++ [Just (List.length values+1, "")]
      in
        ((user, mapItems disc recipe addEmptyToEnd), Cmd.none)

    RemoveItem disc index ->
      let
        remove (i, v) = if i == index then Nothing else Just (i, v)
      in
        ((user, mapItems disc recipe <| List.map (Maybe.andThen remove)), Cmd.none)

    SaveNewRecipe ->
      let
        handle response =
          case response of
            Ok _ -> RecipeSaved
            Err _ -> ErrorWhileSaving
      in
        ((user, recipe), Http.send handle <| postRecipe {recipe=recipe, user=user})

    _ ->
      ((user, recipe), Cmd.none)

view ((name, _), recipe) =
  div []
    [ header [class "bar bar-nav"]
      [h1 [class "title"] [text "Add Recipe"]]
    , div [class "content content-padded"]
      [ form [class "input-group"]
          [ div [class "input-row"]
              [ label [] [text "Recipe name"]
              , input [onInput ChangeNewRecipeName, attribute "type" "text", attribute "placeholder" "ex. Mom's famous mac'n cheese"] []
              , div [] [text recipe.name]
              ]
          , listEdit "Ingredients" "ex. 2 eggs" IngredientItem recipe.ingredients
          , listEdit "Steps" "ex. mix dry ingredients" StepItem recipe.steps
          , button [attribute "type" "button", class "btn btn-block", onClick SaveNewRecipe] [text "Save"]
          ]
      ]
    ]
