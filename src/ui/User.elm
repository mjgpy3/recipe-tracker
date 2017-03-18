module User exposing (..)

type alias ShortName = String
type Role = Chef | Diner

type alias User = (ShortName, Role)

roleText role =
  case role of
    Diner -> "diner"
    Chef -> "chef"
