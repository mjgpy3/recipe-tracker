import Html exposing (Html, button, div, text, header, h1, form, input, label, span, ul, li)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Dec
import Json.Encode as Enc
import Array
import User exposing (..)
import Fp exposing (..)

main =
  Html.program { init = (NoneYet, Cmd.none), view = view, update = update, subscriptions = subscriptions }

subscriptions model = Sub.none

type alias NewRecipe = {
  name: String,
  ingredients: List (Maybe (Int, String)),
  steps: List (Maybe (Int, String))
}

type Model
  = NoneYet
  | Construction
  | Welcome User
  | AddRecipeFor User NewRecipe

type ItemDiscriminator = IngredientItem | StepItem

type Msg
  = SelectUser User
  | SelectAddRecipe
  | SelectFollowRecipe
  | SaveNewRecipe
  | ChangeNewRecipeName String
  | AddEmpty ItemDiscriminator
  | ChangeItem ItemDiscriminator Int String
  | RemoveItem ItemDiscriminator Int

mapItems disc recipe fn =
  case disc of
    IngredientItem -> { recipe | ingredients=fn recipe.ingredients }
    StepItem -> { recipe | steps=fn recipe.steps }

type alias PostRecipeResponse = {
  message: String
}

type alias PostRecipeBody = { user: User, recipe: NewRecipe }

encode body =
  let
    encodeItems =
      List.filterMap id >>
      List.map Tuple.second >>
      List.map Enc.string >>
      Array.fromList >>
      Enc.array
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

decoder =
  Dec.string
    |> Dec.field "message"
    |> Dec.map (\message -> { message=message })

postRecipe : PostRecipeBody -> Http.Request PostRecipeResponse
postRecipe body =
  Http.post "http://localhost:3000/recipe" (Http.jsonBody <| encode body) decoder

update msg model =
  case (msg, model) of
    (SelectUser user, _) ->
      (Welcome user, Cmd.none)

    (SelectAddRecipe, Welcome user) ->
      (AddRecipeFor user { name="", ingredients=[], steps=[] }, Cmd.none)

    (ChangeNewRecipeName newName, AddRecipeFor user recipe) ->
      (AddRecipeFor user { recipe | name=newName }, Cmd.none)

    (ChangeItem disc valueIndex newValue, AddRecipeFor user recipe) ->
      let
        replaceValue = Maybe.map (\(index, value) -> if index == valueIndex then (index, newValue) else (index, value))
      in
        (mapItems disc recipe (List.map replaceValue) |> AddRecipeFor user, Cmd.none)

    (AddEmpty disc, AddRecipeFor user recipe) ->
      let
        addEmptyToEnd values = values ++ [Just (List.length values+1, "")]
      in
        (mapItems disc recipe addEmptyToEnd |> AddRecipeFor user, Cmd.none)

    (RemoveItem disc index, AddRecipeFor user recipe) ->
      let
        remove (i, v) = if i == index then Nothing else Just (i, v)
      in
        (AddRecipeFor user <| mapItems disc recipe <| List.map (Maybe.andThen remove), Cmd.none)

    (SaveNewRecipe, AddRecipeFor user recipe) ->
      let
        handle response =
          case response of
            Ok _ -> SelectUser user
            Err _ -> SelectUser user
      in
        (model, Http.send handle <| postRecipe {recipe=recipe, user=user})

    v ->
      (Construction, Cmd.none)

view model =
  case model of
    NoneYet -> viewSelectUser model

    Welcome user -> viewWelcome user

    AddRecipeFor user recipe -> viewAddRecipe user recipe

    Construction -> viewUnderConstruction

selectUserButton (name, role) = button [class "btn btn-primary btn-block", onClick (SelectUser (name, role))] [text name]

viewSelectUser model =
  div []
    [ header [class "bar bar-nav"]
      [h1 [class "title"] [text "Who Are You?"]]
    , div [class "content content-padded"]
      (List.map selectUserButton users)
    ]

viewWelcome (name, role) =
  div []
    [ header [class "bar bar-nav"]
      [h1 [class "title"] [text ("Welcome " ++ roleText role ++ " " ++ name)]]
    , div [class "content content-padded"]
      (roleActions (name, role))
    ]

roleActions (name, role) =
  let
    actionButton (label, action) =
      button [class "btn btn-primary btn-block", onClick action] [text label]
  in
    case role of
      Chef ->
        List.map actionButton
          [ ("Follow Recipe", SelectFollowRecipe)
          , ("Add Recipe", SelectAddRecipe)
          ]

      Diner ->
        [
        ]

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

viewAddRecipe (name, role) recipe =
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

viewUnderConstruction =
  div []
    [ header [class "bar bar-nav"]
      [h1 [class "title"] [text "Whoops..."]]
    , div [class "content content-padded"]
      [
        div [class "card"]
          [ ul [class "table-view"]
              [ li [class "table-view-cell"]
                  [ span [class "icon icon-info"] []
                  , text "This page is under construction!"
                  ]
              ]
          ]
      ]
    ]
