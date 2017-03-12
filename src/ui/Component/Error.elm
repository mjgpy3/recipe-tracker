module Component.Error exposing (view)

import Html exposing (div, text, header, h1, span, ul, li)
import Html.Attributes exposing (class)

view message =
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
