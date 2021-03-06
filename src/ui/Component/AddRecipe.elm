module Component.AddRecipe exposing (update, Model, view, Msg(..), empty)

import Html exposing (button, div, text, header, h1, form, input, label, span, ul, li)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Dec
import Json.Encode as Enc
import User exposing (..)
import Config exposing (route)
import Fp exposing (..)

type alias RecipeName = String

type Msg
  = SaveNewRecipe
  | ChangeNewRecipeName String
  | ChangeServing String
  | ChangePrepTime String
  | ChangeCookTime String
  | ChangeOverallTime String
  | AddEmpty ItemDiscriminator
  | ChangeItem ItemDiscriminator Int String
  | RemoveItem ItemDiscriminator Int
  | RecipeSaved
  | ErrorWhileSaving String

type ItemDiscriminator = IngredientItem | StepItem

type alias NewRecipe = {
  name: RecipeName,
  ingredients: List (Maybe (Int, String)),
  steps: List (Maybe (Int, String)),
  serves: Maybe Int,
  cookTime: Maybe Int,
  prepTime: Maybe Int,
  overallTime: Maybe Int
}

empty =
  {
    name="",
    ingredients=[],
    steps=[],
    serves=Nothing,
    cookTime=Nothing,
    prepTime=Nothing,
    overallTime=Nothing
  }

type alias Model = (User, NewRecipe, Maybe String)

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
    possibleInt name v =
      case v of
        Nothing -> []
        Just count -> [(name, Enc.int count)]
  in
    Enc.object
      [ ("userName", Enc.string <| Tuple.first body.user)
      , ("role", Enc.string <| roleText <| Tuple.second body.user)
      , ("recipe", Enc.object <|
          [ ("name", Enc.string <| body.recipe.name)
          , ("ingredients", encodeItems body.recipe.ingredients)
          , ("steps", encodeItems body.recipe.steps)
          ]
            ++ possibleInt "serves" body.recipe.serves
            ++ possibleInt "cookTime" body.recipe.cookTime
            ++ possibleInt "prepTime" body.recipe.prepTime
            ++ possibleInt "overallTime" body.recipe.overallTime
        )
      ]

type alias PostRecipeResponse = {
  message: String
}

type alias PostRecipeBody = { user: User, recipe: NewRecipe }

postRecipe : PostRecipeBody -> Http.Request PostRecipeResponse
postRecipe body =
  Http.post (route "/recipe") (Http.jsonBody <| encode body) decoder

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

handle response =
  case response of
    Ok _ -> RecipeSaved
    Err (Http.BadStatus e) ->
      case (e.status.code, Dec.decodeString decoder e.body) of
        (400, Ok err) ->
          ErrorWhileSaving err.message
        _ ->
          ErrorWhileSaving "Failed to save. Please try again later."
    Err _ ->
      ErrorWhileSaving "Failed to save. Please try again later."

update msg (user, recipe, err) =
  case msg of
    ChangeNewRecipeName newName ->
      ((user, { recipe | name=newName }, Nothing), Cmd.none)

    ChangeServing newServing ->
      ((user, { recipe | serves=Result.toMaybe (String.toInt newServing) }, Nothing), Cmd.none)

    ChangeCookTime newCookTime ->
      ((user, { recipe | cookTime=Result.toMaybe (String.toInt newCookTime) }, Nothing), Cmd.none)

    ChangePrepTime newPrepTime ->
      ((user, { recipe | prepTime=Result.toMaybe (String.toInt newPrepTime) }, Nothing), Cmd.none)

    ChangeOverallTime newOverallTime ->
      ((user, { recipe | overallTime=Result.toMaybe (String.toInt newOverallTime) }, Nothing), Cmd.none)

    ChangeItem disc valueIndex newValue ->
      let
        replaceValue = Maybe.map (\(index, value) -> if index == valueIndex then (index, newValue) else (index, value))
      in
        ((user, mapItems disc recipe (List.map replaceValue), Nothing), Cmd.none)

    AddEmpty disc ->
      let
        addEmptyToEnd values = values ++ [Just (List.length values+1, "")]
      in
        ((user, mapItems disc recipe addEmptyToEnd, Nothing), Cmd.none)

    RemoveItem disc index ->
      let
        remove (i, v) = if i == index then Nothing else Just (i, v)
      in
        ((user, mapItems disc recipe <| List.map (Maybe.andThen remove), Nothing), Cmd.none)

    ErrorWhileSaving message ->
      ((user, recipe, Just message), Cmd.none)

    SaveNewRecipe ->
      case String.trim recipe.name of
        "" ->
          ((user, recipe, Just "A recipe name must be given."), Cmd.none)
        _ ->
          ((user, recipe, Nothing), Http.send handle <| postRecipe {recipe=recipe, user=user})

    _ ->
      ((user, recipe, Nothing), Cmd.none)

viewError error =
  case error of
    Nothing ->
      text ""
    Just message ->
      div [class "card"]
        [ ul [class "table-view"]
            [ li [class "table-view-cell"]
                [ span [class "icon icon-info"] []
                , text message
                ]
            ]
        ]

view ((name, _), recipe, err) =
  div []
    [ header [class "bar bar-nav"]
      [h1 [class "title"] [text "Add Recipe"]]
    , div [class "content content-padded"]
      [ form [class "input-group"]
          [ div [class "input-row"]
              [ label [] [text "Name"]
              , input [onInput ChangeNewRecipeName, attribute "type" "text", attribute "placeholder" "ex. Mom's famous mac'n cheese"] []
              ]
          , div [class "input-row"]
              [ label [] [text "Serves"]
              , input [onInput ChangeServing, attribute "type" "number", attribute "placeholder" "number of people"] []
              ]
          , div [class "input-row"]
              [ label [] [text "Cook Time"]
              , input [onInput ChangeCookTime, attribute "type" "number", attribute "placeholder" "minutes"] []
              ]
          , div [class "input-row"]
              [ label [] [text "Prep Time"]
              , input [onInput ChangePrepTime, attribute "type" "number", attribute "placeholder" "minutes"] []
              ]
          , div [class "input-row"]
              [ label [] [text "Total Time"]
              , input [onInput ChangeOverallTime, attribute "type" "number", attribute "placeholder" "minutes"] []
              ]
          , listEdit "Ingredients" "ex. 2 eggs" IngredientItem recipe.ingredients
          , listEdit "Steps" "ex. mix dry ingredients" StepItem recipe.steps
          , viewError err
          , button [attribute "type" "button", class "btn btn-block", onClick SaveNewRecipe] [text "Save"]
          ]
      ]
    ]
