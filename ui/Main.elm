import Html exposing (Html, button, div, text, header, h1)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

main =
  Html.beginnerProgram { model = 0, view = view, update = update }

type Msg = Increment | Decrement

update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

view model =
  div []
    [ header [class "bar bar-nav"]
        [h1 [class "title"] [text "Who Are You?"]]
    , div [class "content content-padded"]
        [ button [class "btn btn-primary btn-block"] [text "Her"]
        , button [class "btn btn-primary btn-block"] [text "Him"]
        , button [class "btn btn-primary btn-block"] [text "Guest"]
        ]
    ]
