import Html exposing (Html, button, div, text, header, h1, form, input, label, span, ul, li)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (onClick, onInput)

main =
  Html.beginnerProgram { model = NoneYet, view = view, update = update }

type alias ShortName = String
type Role = Chef | Diner

type alias User = (ShortName, Role)

type alias NewRecipe = { name: String, ingredients: List (Maybe (Int, String)) }

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

type Msg
  = SelectUser User
  | SelectAddRecipe
  | ChangeNewRecipeName String
  | AddEmptyIngredient
  | ChangeIngredient Int String
  | RemoveIngredient Int

update msg model =
  case msg of
    SelectUser user -> Welcome user

    SelectAddRecipe ->
      case currentUser model of
        Just user -> AddRecipeFor user { name="", ingredients=[] }
        Nothing -> NoneYet
    ChangeNewRecipeName newName ->
     case (newRecipe model, currentUser model) of
        (Just recipe, Just user) -> AddRecipeFor user { recipe | name=newName }
        _ -> NoneYet
    ChangeIngredient valueIndex newValue ->
     case (newRecipe model, currentUser model) of
        (Just recipe, Just user) ->
          let
            replaceValue = Maybe.map (\(index, value) -> if index == valueIndex then (index, newValue) else (index, value))
          in
            AddRecipeFor user { recipe | ingredients=List.map replaceValue recipe.ingredients }
        _ -> NoneYet
    AddEmptyIngredient ->
     case (newRecipe model, currentUser model) of
        (Just recipe, Just user) ->
          let
            newIngredients = recipe.ingredients ++ [Just (List.length recipe.ingredients+1, "")]
          in
            AddRecipeFor user { recipe | ingredients=newIngredients }
        _ -> NoneYet
    RemoveIngredient index ->
     case (newRecipe model, currentUser model) of
        (Just recipe, Just user) ->
          let
            newIngredients = List.map (Maybe.andThen (\(i, v) -> if i == index then Nothing else Just (i, v))) recipe.ingredients
          in
            AddRecipeFor user { recipe | ingredients=newIngredients }
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

ingredientEdit maybeIngredient =
  case maybeIngredient of
    Just (index, ingredient) ->
      li [class "table-view-cell"]
        [
          input [onInput (ChangeIngredient index), attribute "type" "text", attribute "placeholder" "ex. 5 eggs"] []
        , button [attribute "type" "button", onClick (RemoveIngredient index), class "btn btn-negative"] [span [class "icon icon-trash"] []]
        ]
    Nothing -> text ""

addButton =
  li [class "table-view-cell"]
    [button [attribute "type" "button", class "btn", onClick AddEmptyIngredient] [span [class "icon icon-plus"] []]]

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
          , div [class "card"]
              [ ul [class "table-view"]
                  ([ li [class "table-view-cell"]
                      [label [] [text "Ingredients"]]
                  , li [class "table-view-divider"] []
                  ] ++ (List.map ingredientEdit recipe.ingredients) ++ [addButton])
              ]
          , div [] [text (toString recipe)]
          ]
      ]
    ]
