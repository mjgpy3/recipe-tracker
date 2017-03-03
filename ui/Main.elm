import Html exposing (Html, button, div, text, header, h1, form, input, label, span, ul, li)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Dec
import Json.Encode as Enc
import Array

main =
  Html.program { init = (NoneYet, Cmd.none), view = view, update = update, subscriptions = subscriptions }

subscriptions model = Sub.none

type alias ShortName = String
type Role = Chef | Diner

type alias User = (ShortName, Role)

type alias NewRecipe = {
  name: String,
  ingredients: List (Maybe (Int, String)),
  steps: List (Maybe (Int, String))
}

type Model
  = NoneYet
  | Welcome User
  | AddRecipeFor User NewRecipe

newRecipe model =
  case model of
    AddRecipeFor _ recipe -> Just recipe
    _ -> Nothing

currentUser model =
  case model of
    AddRecipeFor user _ -> Just user
    Welcome user -> Just user
    NoneYet -> Nothing

type ItemDiscriminator = IngredientItem | StepItem

type Msg
  = SelectUser User
  | SelectAddRecipe
  | RecipeAdded (Result Http.Error PostRecipeResponse)
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
      List.filterMap (\x -> x) >>
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
  Http.post "http://localhost/recipe" (Http.jsonBody <| encode body) decoder

update msg model =
  case msg of
    SelectUser user -> (Welcome user, Cmd.none)

    SelectAddRecipe ->
      case currentUser model of
        Just user -> (AddRecipeFor user { name="", ingredients=[], steps=[] }, Cmd.none)
        Nothing -> (NoneYet, Cmd.none)
    ChangeNewRecipeName newName ->
      case (newRecipe model, currentUser model) of
        (Just recipe, Just user) -> (AddRecipeFor user { recipe | name=newName }, Cmd.none)
        _ -> (NoneYet, Cmd.none)
    ChangeItem disc valueIndex newValue ->
      case (newRecipe model, currentUser model) of
        (Just recipe, Just user) ->
          let
            replaceValue = Maybe.map (\(index, value) -> if index == valueIndex then (index, newValue) else (index, value))
          in
            (mapItems disc recipe (List.map replaceValue) |> AddRecipeFor user, Cmd.none)
        _ -> (NoneYet, Cmd.none)
    AddEmpty disc ->
      case (newRecipe model, currentUser model) of
        (Just recipe, Just user) ->
          let
            addEmptyToEnd values = values ++ [Just (List.length values+1, "")]
          in
            (mapItems disc recipe addEmptyToEnd |> AddRecipeFor user, Cmd.none)
        _ -> (NoneYet, Cmd.none)
    RemoveItem disc index ->
      case (newRecipe model, currentUser model) of
        (Just recipe, Just user) ->
          let
            remove (i, v) = if i == index then Nothing else Just (i, v)
          in
            (AddRecipeFor user <| mapItems disc recipe <| List.map (Maybe.andThen remove), Cmd.none)
        _ -> (NoneYet, Cmd.none)
    RecipeAdded (Err _) ->
      (model, Cmd.none)
    RecipeAdded (Ok added) ->
      case currentUser model of
        Just user -> (Welcome user, Cmd.none)
        Nothing -> (NoneYet, Cmd.none)
    SaveNewRecipe ->
      case (newRecipe model, currentUser model) of
        (Just recipe, Just user) -> (model, Http.send (\_ -> SelectAddRecipe) <| postRecipe {recipe=recipe, user=user})
        _ -> (NoneYet, Cmd.none)

users =
  [ ("Thelma", Chef)
  , ("Jimmy", Diner)
  , ("Guest", Diner)
  ]

view model =
  case model of
    NoneYet -> viewSelectUser model

    Welcome user -> viewWelcome user

    AddRecipeFor user recipe -> viewAddRecipe user recipe

selectUserButton (name, role) = button [class "btn btn-primary btn-block", onClick (SelectUser (name, role))] [text name]

viewSelectUser model =
  div []
    [ header [class "bar bar-nav"]
      [h1 [class "title"] [text "Who Are You?"]]
    , div [class "content content-padded"]
      (List.map selectUserButton users)
    ]

roleText role =
  case role of
    Diner -> "diner"
    Chef -> "chef"

viewWelcome (name, role) =
  div []
    [ header [class "bar bar-nav"]
      [h1 [class "title"] [text ("Welcome " ++ roleText role ++ " " ++ name)]]
    , div [class "content content-padded"]
      (roleActions (name, role))
    ]

roleActions (name, role) =
  case role of
    Chef ->
      [
        button [class "btn btn-primary btn-block", onClick SelectAddRecipe] [text "Add Recipe"]
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
