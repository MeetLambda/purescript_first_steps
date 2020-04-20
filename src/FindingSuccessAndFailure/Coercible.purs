module Coercible where

import Data.String.CodeUnits as String
import Data.Array as Data.Array

-- =================================================================
-- module Control.Coercible
--   ( class Coercible
--   , coerce ) where

-- import Prelude
import Control.Bind (class Applicative, class Bind, join, pure)
import Control.Comonad (class Comonad, extract)
-- import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Function (const, identity, (<<<), ($))
import Data.Functor (class Functor, map)
import Data.Int (toNumber)
import Data.List (List(..), (:), reverse)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype,unwrap,wrap)
import Data.Semigroup (class Semigroup)
import Data.String.CodeUnits (fromCharArray, toCharArray, uncons)
import Data.Unit (Unit, unit)
import Data.Validation.Semigroup (V(..))
import Data.Void (Void, absurd)
import Data.Show

import Effect.Console
import Effect

class Coercible a b where
  coerce :: a -> b

instance coercibleUnit :: Coercible a Unit where
  coerce _ = unit
else
instance coercibleVoid :: Coercible Void a where
  coerce = absurd
else
instance coercibleImage :: Coercible a (b -> a) where
  coerce = const
else
instance coercibleIntNumber :: Coercible Int Number where
  coerce = toNumber
else
instance coercibleCharString :: Coercible Char String where
  coerce = fromChar
else
instance coercibleArrayCharString :: Coercible (Array Char) String where
  coerce = fromCharArray
else
instance coercibleStringArrayChar :: Coercible String (Array Char) where
  coerce = toCharArray
else
instance coercibleFunctor :: (Functor f, Coercible a b) => Coercible (f a) (f b) where
  coerce = map coerce
else
instance coercibleApplicative :: Applicative f => Coercible a (f a) where
  coerce = pure
else
instance coercibleBind :: Bind m => Coercible (m (m a)) (m a) where
  coerce = join
else
instance coercibleComonad :: Comonad m => Coercible (m a) a where
  coerce = extract
else
instance coercableListCharString :: Coercible (List Char) String where
  coerce = foldMap fromChar
else
instance coercibleStringListChar :: Coercible String (List Char) where
  coerce = reverse <<< coerce' Nil where
    coerce' acc str =
      case uncons str of
           Just { head, tail } -> coerce' (head : acc) str
           _ -> acc

else
instance coercibleId :: Coercible a a where
  coerce = identity

else
-- b is more specific, like newtype Something = Something String
-- a is just String
instance coercibleF :: (Functor f, Newtype b a) => Coercible (a -> f a) (b -> f b) where
    coerce fa = \bt -> map wrap (fa (unwrap bt))


fromChar :: Char -> String
fromChar c = fromCharArray [c]

-- =================================================================

-- module Test.Main where

-- import Prelude
-- import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Console (CONSOLE, logShow)

-- import Data.List ((:), List(Nil))
-- import Control.Coercible (class Coercible, coerce)

-- data N = Z | S N

-- instance showN :: Show N where
--   show Z = "Z"
--   show (S n) = "S" <> show n

-- instance coerceNInt :: Coercible N Int where
--   coerce Z = 0
--   coerce (S n) = 1 + coerce n

-- instance coerceNString :: Coercible N String where
--   coerce = show

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
-- main = do
--   let three = S (S (S Z))
--   logShow $ coerce three :: String
--   logShow $ coerce three :: Int
--   logShow $ coerce 3 :: Number
--   logShow $ coerce '3' :: String
--   logShow $ coerce 3 :: Unit
--   logShow $ coerce 3 :: Array Int
--   logShow $ coerce (coerce 3 :: Unit) :: Array Unit
--   logShow $ coerce ('a' : 'b' : 'c' : Nil) :: String
--   logShow $ coerce ['a', 'b', 'c'] :: String


-- =================================================================


sort :: String -> String
sort = String.fromCharArray <<< Data.Array.sort <<< String.toCharArray 

sort' :: String -> String
sort' = (coerce :: (Array Char) -> String) <<< Data.Array.sort <<< (coerce :: String -> (Array Char))

chars = coerce "prova" :: (Array Char)
sortedChars :: (Array Char)
sortedChars = Data.Array.sort chars

----------------------------------

newtype Name = Name String
derive instance newtypeName :: Newtype Name _

v :: Name
v = Name "awf"

unwrappedV :: String
unwrappedV = unwrap v

s :: String
s = "asfasd"

nameS = wrap s :: Name

----------------------------------

newtype Username = Username String
derive instance newtypeUsername :: Newtype Username _
newtype Password = Password String
derive instance newtypePassword :: Newtype Password _
newtype Error = Error (Array String)
-- derive instance newtypeError:: Newtype Error _
derive newtype instance semigroupError :: Semigroup Error
derive newtype instance showError :: Show Error
derive newtype instance showUsername :: Show Username

type Rule a = (a -> (V Error a))

alphachar :: Rule String
alphachar = pure


--     coerce fa = \bt -> map wrap (fa (unwrap bt))

-- map :: (a -> b) -> f a -> f b
--  Coercible.Coercible (String -> V Error String) (Username -> V Error Username)
--  Coercible.Coercible (a      -> V Error a     ) (b        -> V Error b       )
--  Coercible.Coercible (a      -> f       a     ) (b        -> f       b       )   --  f :: Functor
--  a -> b

-- instance coercible (Newtype)

--newtype EmailAddress = EmailAddress String
-- derive instance newtypeEmailAddress :: Newtype EmailAddress _


-- Data.Newtype.Newtype (String -> V Error String) (Username -> V Error Username)
-- Data.Newtype.Newtype (a      -> V Error a     ) (b        -> V Error b       )
-- Data.Newtype.Newtype (a      -> f       a     ) (b        -> f       b       )   --  f :: Functor
--     unwrap fa = \bt -> let at = unwrap bt
--                          in map wrap (fa at)

isAlphaChar :: Rule Username -- Username -> V Error Username
isAlphaChar = coerce alphachar


-- newtype App eff a = App (StateT Int (ExceptT String (Eff eff)) a)
-- derive newtype instance functorApp :: Functor (App eff)
-- derive newtype instance applyApp :: Apply (App eff)
-- derive newtype instance applicativeApp :: Applicative (App eff)
-- derive newtype instance bindApp :: Bind (App eff)
-- derive newtype instance monadApp :: Monad (App eff)

main :: Effect Unit
main = logShow $ (isAlphaChar (Username "pino") )