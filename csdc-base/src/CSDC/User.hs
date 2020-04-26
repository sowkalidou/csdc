module CSDC.User
  ( User (..)
  , checkUser
  ) where

import CSDC.Data.Id (Id)
import CSDC.Network.Types (Person)

-- | The current user of the server API. It can be either an admin or an user.
data User = Admin | User (Id Person)
  deriving (Show, Eq)

checkUser :: Applicative m => User -> (Id Person -> m ()) -> m ()
checkUser Admin _ = pure ()
checkUser (User u) f = f u
