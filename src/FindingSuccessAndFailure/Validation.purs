module Validation where

import Data.Validation.Semigroup (V(..), invalid, unV)
import Data.Validation.Semigroup as Semigroup
import Data.Newtype (class Newtype)
import Data.Either (Either(..))
import FindingSuccessAndFailure as FSAF
import Data.Functor ((<$>), map)
import Control.Apply ((*>), (<*>), apply)
import Control.Bind (bind, (>>=))
import Control.Applicative (pure)
import Control.Semigroupoid ((<<<))

-- <$>  map   :: forall a b. (a -> b) -> f a -> f b
-- <*>  apply :: forall a b. f (a -> b) -> f a -> f b

-- | The `V` functor, used for applicative validation
-- |
-- | The `Applicative` instance collects multiple failures in
-- | an arbitrary `Semigroup`.
-- |
-- | For example:
-- |
-- | ```purescript
-- | validate :: Person -> V (Array Error) Person
-- | validate person = { first: _, last: _, email: _ }
-- |   <$> validateName person.first
-- |   <*> validateName person.last
-- |   <*> validateEmail person.email

-- validateName :: String -> V (Array Error) String
-- createPerson :: String -> String -> String -> Person
-- createPerson = { first: _, last: _, email: _ }

-- apply (apply (map createPerson (validateName person.first)) (validateName person.last)) (validateEmail person.email)
-- | ```
--  (map createPerson (validateName person.first)) :: V (Array String) (String -> String -> Person)


newtype V' err result = V' (Either err result)
derive instance newtypeV' :: Newtype (V' err result) _

newtype V'' err result = V'' (V err result)
derive instance newtypeV'' :: Newtype (V'' err result) _


-- newtype V err result = V (Semigroup.V err result)
-- derive instance newtypeV :: Newtype (V err result) _

-- | A type class for `newtype`s to enable convenient wrapping and unwrapping,
-- | and the use of the other functions in this module.
-- |
-- | The compiler can derive instances of `Newtype` automatically:
-- |
-- | ``` purescript
-- | newtype EmailAddress = EmailAddress String
-- |
-- | derive instance newtypeEmailAddress :: Newtype EmailAddress _
-- | ```
-- |
-- | Note that deriving for `Newtype` instances requires that the type be
-- | defined as `newtype` rather than `data` declaration (even if the `data`
-- | structurally fits the rules of a `newtype`), and the use of a wildcard for
-- | the wrapped type.
-- |
-- | Instances must obey the following laws:
-- | ``` purescript
-- | unwrap <<< wrap = id
-- | wrap <<< unwrap = id
-- | ```
class Newtype' t a | t -> a where
  wrap :: a -> t
  unwrap :: t -> a

type Errors = Array String

mapError :: (Either String String) -> (Either (Array String) String)
mapError (Left e) = Left ([e])
mapError (Right s) = Right s

-- bar = ((V <<< FSAF.requireAlphaNum') "ciao") :: ?o -- V String String

-- validatePassword :: String -> V FSAF.Error FSAF.Password
validatePassword :: String -> V Errors FSAF.Password
validatePassword password = -- V (Left ["errore"])
   ((V <<< (mapError <<< FSAF.cleanWhitespace')) password) 
     -- :: V Error Password
     *> ((V <<< (mapError <<< FSAF.requireAlphaNum')) password) -- :: Password -> V Error Password
     -- :: 
     *> ((V <<< (mapError <<< FSAF.checkPasswordLength' 8 30)) password) -- :: Password -> V Error Password
     -- :: V Error Password

-- *> if the first went well, ignore it and take the second
--    if the first went bad, check the second and remember the first

cleanWhitespace =     V <<< mapError <<< FSAF.cleanWhitespace'
requireAlphaNum =     V <<< mapError <<< FSAF.requireAlphaNum' -- :: String -> V (Array String) String
checkPasswordLength = V <<< mapError <<< FSAF.checkPasswordLength' 8 30

-- *>   applySecond :: forall a b f. Apply f => f a -> f b -> f b
-- <$>  map   :: forall a b. (a -> b) -> f a -> f b
-- <*>  apply :: forall a b. f (a -> b) -> f a -> f b
-- >>=  bind  :: forall a b. m a -> (a -> m b) -> m b
--      unV   :: forall err result r. (err -> r) -> (result -> r) -> V err result -> r
-- Unpack the V type constructor, providing functions to handle the error and success cases.

--      either :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c

validatePassword' :: String -> V Errors FSAF.Password
validatePassword' password = -- V (Left ["errore"])
   unV (\err -> V (Left(err))) 
       (\pass -> requireAlphaNum pass *> checkPasswordLength pass) 
       (cleanWhitespace password)


validatePassword'' :: String -> V Errors FSAF.Password
validatePassword'' password = -- V (Left ["errore"])
   cleanWhitespace password
   *> requireAlphaNum password
   *> checkPasswordLength  password 
       
       


-- bindV 
   
-- (>>=) :: m a -> (a ->  m b) -> m b
-- FSAF.requireAlphaNum' :: String -> Either Error String
-- V <<< FSAF.requireAlphaNum' :: String -> V Error String

validateUsername :: String -> V Errors FSAF.Username
validateUsername u = requireAlphaNum u

makeUser :: FSAF.Username -> FSAF.Password -> V Errors FSAF.User
makeUser u p = FSAF.UserConstructor <$> validateUsername u
                                    <*> validatePassword'' p


