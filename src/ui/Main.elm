import Html exposing (Html, button, div, text, header, h1, h2, form, input, label, span, ul, li)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Dec
import Json.Encode as Enc
import User exposing (..)
import Fp exposing (..)
import Component.SelectUser as SelectUser

main =
  Html.program { init = (NoneYet, Cmd.none), view = view, update = update, subscriptions = subscriptions }

subscriptions model = Sub.none

type alias RecipeName = String

type alias NewRecipe = {
  name: RecipeName,
  ingredients: List (Maybe (Int, String)),
  steps: List (Maybe (Int, String))
}

type Model
  = NoneYet
  | Error String
  | Welcome User
  | AddRecipeFor User NewRecipe
  | FindRecipeToFollow User (List RecipeName)

type ItemDiscriminator = IngredientItem | StepItem

type Msg
  = SelectUserMsg SelectUser.Msg
  | ErrorOccured String
  | SelectAddRecipe
  | SelectFollowRecipe
  | SelectFollowRecipeFrom User (List RecipeName)
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

decoder =
  Dec.string
    |> Dec.field "message"
    |> Dec.map (\message -> { message=message })

postRecipe : PostRecipeBody -> Http.Request PostRecipeResponse
postRecipe body =
  Http.post "http://localhost:3000/recipe" (Http.jsonBody <| encode body) decoder

getRecipeNames : Http.Request (List RecipeName)
getRecipeNames =
  Http.get "http://localhost:3000/recipes" (Dec.list Dec.string)

update msg model =
  case (msg, model) of
    (SelectUserMsg (SelectUser.SelectUser user), _) ->
      (Welcome user, Cmd.none)

    (SelectFollowRecipe, Welcome user) ->
      let
        handle response =
          case response of
            Ok recipeNames -> SelectFollowRecipeFrom user recipeNames
            Err _ -> ErrorOccured "Sorry! Something broke while I was finding recipes."
      in
      (FindRecipeToFollow user [], Http.send handle getRecipeNames)

    (SelectFollowRecipeFrom user recipeNames, _) ->
      (FindRecipeToFollow user recipeNames, Cmd.none)

    (SelectAddRecipe, Welcome user) ->
      (AddRecipeFor user { name="", ingredients=[], steps=[] }, Cmd.none)

    (SelectAddRecipe, FindRecipeToFollow user _) ->
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
            Ok _ -> SelectUserMsg (SelectUser.SelectUser user)
            Err _ -> ErrorOccured "Could not save recipe"
      in
        (model, Http.send handle <| postRecipe {recipe=recipe, user=user})

    (ErrorOccured message, _) ->
      (Error message, Cmd.none)

    v ->
      (Error "Something went wrong", Cmd.none)

view model =
  case model of
    NoneYet -> Html.map SelectUserMsg SelectUser.view

    Welcome user -> viewWelcome user

    FindRecipeToFollow user recipes -> viewFindRecipeToFollow user recipes
    AddRecipeFor user recipe -> viewAddRecipe user recipe

    Error message -> viewError message

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

viewError message =
  div []
    [ header [class "bar bar-nav"]
      [h1 [class "title"] [text "Whoops..."]]
    , div [class "content content-padded"]
      [
        div [class "card"]
          [ ul [class "table-view"]
              [ li [class "table-view-cell"]
                  [ span [class "icon icon-info"] []
                  , text message
                  ]
              ]
          ]
      ]
    ]

-- <li class="table-view-cell">Item 2 <button class="btn btn-primary">Button</button></li>

followRecipeButton recipe =
  li [class "table-view-cell"]
    [ text recipe
    , button [class "btn", attribute "type" "button"] [text "Follow"]
    ]

recipiesHeading =
  li [class "table-view-cell"] [h2 [] [text "Recipes"]]

withEmptyMessage rows =
  case rows of
    [header] ->
      header::[
        li [class "table-view-cell"]
          [ text "It looks like you don't have any recipes yet!"
          , button [class "btn btn-primary", onClick SelectAddRecipe] [text "Add One"]
          ]
      ]
    all -> all

viewFindRecipeToFollow user recipes =
  div []
    [ header [class "bar bar-nav"]
      [h1 [class "title"] [text "Find Recipe to Follow"]]
    , div [class "content content-padded"]
      [
        div [class "card"]
          [ ul [class "table-view"]
              <| withEmptyMessage <| recipiesHeading::List.map followRecipeButton recipes
          ]
      ]
    ]
