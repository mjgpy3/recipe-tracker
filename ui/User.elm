module User exposing (..)

type alias ShortName = String
type Role = Chef | Diner

type alias User = (ShortName, Role)

users =
  [ ("Thelma", Chef)
  , ("Jimmy", Diner)
  , ("Guest", Diner)
  ]

roleText role =
  case role of
    Diner -> "diner"
    Chef -> "chef"
