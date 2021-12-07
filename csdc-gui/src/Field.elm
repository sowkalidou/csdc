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
  , with
    -- Common
  , make
  , optional
  , required
  , requiredString
  , requiredId
  )

import CSDC.Types exposing (Id (..))

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
  , validate : a -> Result (List String) b
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
  , status = case f.validate a of
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

validate : Field a b -> Result (List String) b
validate (Field f) =
  let
    addPrefix s = f.name ++ ": " ++ s
  in
    case f.status of
      Invalid e -> Err (List.map addPrefix e)
      Valid a -> Ok a
      Initial -> Err [addPrefix "Initial field: should not happen"]

with : Field a b -> (b -> Result (List String) c) -> Result (List String) c
with field f =
  validate field |> Result.andThen f

--------------------------------------------------------------------------------
-- Fields

make : String -> a -> (a -> Result (List String) b) -> Field a b
make n r v = Field
  { name = n
  , raw = r
  , validate = v
  , status = Initial
  }

optional : String -> Field (Maybe a) (Maybe a)
optional n = make n Nothing Ok

required : String -> Field (Maybe a) a
required n = make n Nothing <| \m ->
  case m of
    Nothing -> Err ["This field is required."]
    Just a -> Ok a

requiredString : String -> Field String String
requiredString n = make n "" <| \s ->
  if String.isEmpty s
  then Err ["This field is required."]
  else Ok s

requiredId : String -> Field String (Id a)
requiredId n = make n "" <| \s ->
  case String.toInt s of
    Nothing -> Err ["This is not a valid Id."]
    Just k -> Ok (Id k)
