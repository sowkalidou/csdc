module Field exposing
  ( Field (..)
  , Status (..)
  , name
  , raw
  , status
  , set
  , reload
  , errors
  , validate
    -- Common
  , make
  , optional
  , required
  , requiredString
  , requiredId
  )

import CSDC.Types exposing (Id (..))
import Validation exposing (Validation)

type Status b
  -- The field was incorrectly parsed.
  = Invalid (List String)
  -- The field was correctly parsed.
  | Valid b
  -- Exists so that initial values do not show errors from the start.
  | Initial

type Field a b = Field
  { -- Name of the field, used for UI and errors.
    name : String
    -- The value that was input by the user.
  , raw : a
    -- The validation status of the field.
  , status : Status b
    -- The validation function of the field.
  , validate : a -> Validation b
  }

name : Field a b -> String
name (Field f) = f.name

raw : Field a b -> a
raw (Field f) = f.raw

status : Field a b -> Status b
status (Field f) = f.status

set : a -> Field a b -> Field a b
set a (Field f) = Field
  { name = f.name
  , raw = a
  , status =
      case Validation.validate (f.validate a) of
        Err e -> Invalid e
        Ok b -> Valid b
  , validate = f.validate
  }

reload : Field a b -> Field a b
reload field = set (raw field) field

errors : Field a b -> List String
errors (Field f) =
  case f.status of
    Invalid e -> e
    _ -> []

validate : Field a b -> Validation b
validate (Field f) =
  let
    addPrefix s = f.name ++ ": " ++ s
  in
    case f.status of
      Invalid e -> Validation.make <| Err (List.map addPrefix e)
      Valid a -> Validation.make <| Ok a
      Initial -> Validation.make <| Err [addPrefix "Initial field: should not happen"]

--------------------------------------------------------------------------------
-- Fields

make : String -> a -> (a -> Validation b) -> Field a b
make n r v = Field
  { name = n
  , raw = r
  , validate = v
  , status = Initial
  }

optional : String -> Field (Maybe a) (Maybe a)
optional n = make n Nothing Validation.valid

required : String -> Field (Maybe a) a
required n = make n Nothing <| \m ->
  case m of
    Nothing -> Validation.invalid "This field is required."
    Just a -> Validation.valid a

requiredString : String -> Field String String
requiredString n = make n "" <| \s ->
  if String.isEmpty s
  then Validation.invalid "This field is required."
  else Validation.valid s

requiredId : String -> Field String (Id a)
requiredId n = make n "" <| \s ->
  case String.toInt s of
    Nothing -> Validation.invalid "This is not a valid Id."
    Just k -> Validation.valid (Id k)
