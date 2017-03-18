module Config exposing (users, route)

import User exposing (Role(..))

users =
  [ ("Thelma", Chef)
  , ("Jimmy", Diner)
  , ("Guest", Diner)
  ]

route v = "http://localhost:3000" ++ v
