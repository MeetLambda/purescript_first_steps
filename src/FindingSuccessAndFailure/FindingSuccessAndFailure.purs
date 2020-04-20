module FindingSuccessAndFailure where

--import Prelude -- core functions

-- import Control.Apply ((*>))
import Control.Applicative (pure)
import Control.Apply ((<*>), apply)
import Control.Bind (bind, (>>=))
import Data.Semigroup ((<>))
import Control.Semigroupoid ((<<<))
import Data.Array as Data.Array
import Data.Char.Unicode as Data.Char.Unicode
import Data.Char.Unicode.Internal (uGencat)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Foldable as Data.Foldable
import Data.Function (($), flip)
import Data.Functor as Data.Functor
import Data.Functor ((<$>), map)
import Data.HeytingAlgebra ((&&))
import Data.Maybe (Maybe(..))
import Data.Ord ((<), (>), (>=), (<=), class Ord)
import Data.Ring (class Ring, (-), negate)
import Data.Show (class Show, show)
import Data.String.CodeUnits as String
import Data.String.Common (null)
import Data.String.Common as Data.String.Common

-- ===================================================
--  Finding Success and Failure
-- ===================================================



-- absVal :: (Num a, Ord a) => a -> a
-- absVal :: forall a. Ring a => Ord a => a -> a
absVal :: Number -> Number
absVal x = if (x < 0.0) then (negate x) else x

validateUsernamePassword :: String -> String -> String
validateUsernamePassword username password =
  if null username
  then (
      if null password
      then "Empty username and password"
      else "Empty username"
  ) else (
    if null password
    then "Empty password"
    else "Okay"
  )

sort :: String -> String
sort = String.fromCharArray <<< Data.Array.sort <<< String.toCharArray 

reverse :: String -> String
reverse = String.fromCharArray <<< Data.Array.reverse <<< String.toCharArray 

isAllChar :: (Char -> Boolean) -> String -> Boolean
isAllChar f w = Data.Foldable.all f (String.toCharArray w)

isWord :: String -> Maybe String
isWord word =
  case (null word) of
    true  -> Nothing
    false -> case (isAllChar (Data.Char.Unicode.isAlpha) word) of
      false -> Nothing
      true  -> Just word

isAnagram :: String -> String -> Boolean
isAnagram word1 word2 = (sort word1) == (sort word2)

checkAnagram :: String -> String -> String
checkAnagram word1 word2 = 
  case (isWord word1) of
    Nothing -> "The first word is invalid."
    Just word1 -> 
      case (isWord word2) of
        Nothing -> "The second word is invalid."
        Just word2 ->
          case (isAnagram word1 word2) of
            false -> "These words are not anagrams."
            true  -> "These words are anagrams."

substituteChar :: Char -> Char 
substituteChar c = 
  case c of 
    'e'        -> '3'
    'a'        -> '@'
    'i'        -> '1'
    'o'        -> '0'
    otherwise  ->  c

translateWord :: String -> String
translateWord w = String.fromCharArray $ Data.Functor.map substituteChar (String.toCharArray w)

checkPasswordLength :: Int -> Int -> String -> Maybe String
checkPasswordLength min max s =
  case ((min <= String.length s) && (String.length s <= max)) of
    true  -> Just s
    false -> Nothing

checkPasswordLength' :: Int -> Int -> String -> Either Error String
checkPasswordLength' min max s =
  if (String.length s <= min)
  then Left "Value too short"
  else  if (max <= String.length s)
        then Left "Value too long"
        else Right s

requireAlphaNum :: String -> Maybe String
requireAlphaNum word =
  case (isAllChar Data.Char.Unicode.isAlphaNum word) of
    true -> Just word
    false -> Nothing

requireAlphaNum' :: String -> Either Error String
requireAlphaNum' word =
  case (isAllChar Data.Char.Unicode.isAlphaNum word) of
    true -> Right word
    false -> Left "Non alphanum char present"

requireAlphaNum'' :: String -> Either (Array String) String
requireAlphaNum'' word =
  case (isAllChar Data.Char.Unicode.isAlphaNum word) of
    true -> Right word
    false -> Left ([ "Non alphanum char present" ] )

cleanWhitespace :: String -> Maybe String 
cleanWhitespace word = 
  let tw = Data.String.Common.trim word
   in if (null tw) then Nothing else Just tw


-- verifica se la stringa contiene almeno un carattere che non sia spazio
cleanWhitespace' :: String -> Either Error String
cleanWhitespace' word = 
  let tw = Data.String.Common.trim word
   in if (null tw) then Left "Empty value" else Right tw

cleanWhiteSpaceFabio :: String -> Maybe String
cleanWhiteSpaceFabio = isWord <<< Data.String.Common.trim

validatePassword :: String -> Maybe String
validatePassword password = 
  cleanWhitespace password
  >>= requireAlphaNum
  >>= (checkPasswordLength 8 30)

validatePassword' :: String -> Either Error String
validatePassword' password = 
  cleanWhitespace' password
  >>= requireAlphaNum'
  >>= (checkPasswordLength' 8 30)

type Username = String
type Password = String
type Error    = String

data User = UserConstructor Username Password
-- derive instance userShow :: Show User
instance showUser :: Show User where
  show (UserConstructor u p) = "Username: " <> show u <> ", Password: " <> show p

makeUser :: Username -> Password -> Either Error User 
makeUser u p = ado
    username <- requireAlphaNum' u  
    password <- validatePassword' p
    -- Right (User username password)
    in UserConstructor username password

makeUser''' :: Username -> Password -> Either Error User
makeUser''' u p = do
    username <- requireAlphaNum' u  
    password <- validatePassword' p
    -- Right (UserConstructor username password)
    pure (UserConstructor username password)


-- map   :: forall a b. Functor f =>   (a -> b) -> f a -> f b   -- <$>  Functor <= Apply
-- apply :: forall a b. Apply   f => f (a -> b) -> f a -> f b   -- <*>  Control.Apply.apply   Apply <= Applicative, Apply <= Bind
-- bind  :: forall a b. Monad   m => m a -> (a -> m b) -> m b   -- >>=  Bind, Applicative <= Monad

makeUser' :: Username -> Password -> Either Error User
makeUser' u p = UserConstructor <$> requireAlphaNum' u
                                <*> validatePassword' p

makeUser'' :: Password -> Username -> Either Error User
makeUser'' p u = (flip UserConstructor) <$> validatePassword' p
                                        <*> requireAlphaNum' u


-- Either <=?= Data.Validation  =?=> Either (Array Error) Result
-- Data.Validation.Semigroup -> associativa => List / Array ..<.. Control.Alt ..<.. Plus  ..<.. Alternative
-- Data.Validation.Semiring  -> %%%%%% => Free a == (List(List a))

-- -- | `Free` is left adjoint to the forgetful functor from `Semiring`s to types.
-- liftFree :: forall a s. Semiring s => (a -> s) -> Free a -> s
-- liftFree f (Free xss) = sum (map (product <<< map f) xss)

-- -- | `Free` is left adjoint to the forgetful functor from `Semiring`s to types.
-- lowerFree :: forall a s. Semiring s => (Free a -> s) -> a -> s
-- lowerFree f a = f (free a)


-- u' = requireAlphaNum' u :: Either Error String
-- UserConstructor :: Username -> (Password -> User)

-- UserConstructor <$> requireAlphaNum' u :: Either Error (Password -> User)
-- (a -> b) == a (User -> User)
-- map UserConstructor u' => 

-- a = User <$> requireAlphaNum' u

-- Either Error (Password -> User) -> Either Error Password -> Either Error User  -- this is apply
-- a <*> c :: Either Error User
-- c :: Either Error Password
-- c = validatePassword' p

-- f (a -> b) => Either Error (a -> User)
-- a :: Password => (User s) -- s :: Username
-- f => Either Error
-- b => User


-- pure :: forall a f. Applicative f => a -> f a
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

