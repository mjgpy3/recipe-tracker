import Html exposing (Html, button, div, text, header, h1, form, input, label, span, ul, li)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (onClick, onInput)

main =
  Html.beginnerProgram { model = NoneYet, view = view, update = update }

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
  | ChangeNewRecipeName String
  | AddEmpty ItemDiscriminator
  | ChangeItem ItemDiscriminator Int String
  | RemoveItem ItemDiscriminator Int

mapItems disc recipe fn =
  case disc of
    IngredientItem -> { recipe | ingredients=fn recipe.ingredients }
    StepItem -> { recipe | steps=fn recipe.steps }

update msg model =
  case msg of
    SelectUser user -> Welcome user

    SelectAddRecipe ->
      case currentUser model of
        Just user -> AddRecipeFor user { name="", ingredients=[], steps=[] }
        Nothing -> NoneYet
    ChangeNewRecipeName newName ->
      case (newRecipe model, currentUser model) of
        (Just recipe, Just user) -> AddRecipeFor user { recipe | name=newName }
        _ -> NoneYet
    ChangeItem disc valueIndex newValue ->
      case (newRecipe model, currentUser model) of
        (Just recipe, Just user) ->
          let
            replaceValue = Maybe.map (\(index, value) -> if index == valueIndex then (index, newValue) else (index, value))
          in
            mapItems disc recipe (List.map replaceValue) |> AddRecipeFor user
        _ -> NoneYet
    AddEmpty disc ->
      case (newRecipe model, currentUser model) of
        (Just recipe, Just user) ->
          let
            addEmptyToEnd values = values ++ [Just (List.length values+1, "")]
          in
            mapItems disc recipe addEmptyToEnd |> AddRecipeFor user
        _ -> NoneYet
    RemoveItem disc index ->
      case (newRecipe model, currentUser model) of
        (Just recipe, Just user) ->
          let
            remove (i, v) = if i == index then Nothing else Just (i, v)
          in
            AddRecipeFor user <| mapItems disc recipe <| List.map (Maybe.andThen remove)
        _ -> NoneYet

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
          , div [] [text (toString recipe)]
          ]
      ]
    ]
