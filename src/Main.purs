module Main where

--import Prelude -- core functions

import Control.Apply ((*>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.MonadZero (guard)
import Control.Semigroupoid ((<<<))
import Data.Array as Data.Array
import Data.Array ((..))
import Data.Boolean (otherwise)
import Data.Char.Unicode as Data.Char.Unicode
import Data.Either (Either(..))
import Data.Eq ((==), class Eq)
import Data.HeytingAlgebra ((&&))
import Data.String.Common (null)
import Data.Foldable as Data.Foldable
import Data.Function (flip, ($))
import Data.Functor as Data.Functor
import Data.Maybe (Maybe(..))
import Data.Ord ((<), (>), (>=), (<=), class Ord)
import Data.Ring (class Ring, (-), negate)
import Data.Semigroup ((<>))
import Data.Semiring ((+), (*))
import Data.Show (class Show, show)
import Data.String.Common as Data.String.Common
--import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple, Tuple(..))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Console (log)
import Data.Array as Array
import Data.String.CodeUnits as String

-------------------------------

-- data definition with multiple contructors (Tagged Unions)
data Data = Void | One String | Two Int String
-- it needs pattern matching
showData :: Data -> String
showData Void = "It's a data"
showData (One s) = "One " <> s
showData (Two i s) = "Two " <> s <> ", two " <> show i

-------------------------------

-- newtype: alias for number with different type 
-- (same at runtime, but different at compile time)
newtype Percentage = Percentage Number
-- defining the behaviour of show when Percentage is passed as a value
instance showPercentage :: Show Percentage where
  show (Percentage n) = show n <> "%"
-- variable of type Percentage
perc :: Percentage
perc = Percentage 4.0

-------------------------------

-- data definition: define PersonADT (Algebric Data Type) as new type with a single constructor expecting
-- a record with 'name' and 'age' fields
data PersonADT = Person { name :: String, age :: Int }

-- type definition: gives a name to a record with a 'name' and 'age' fields
type Person = { name :: String, age :: Int }

-- variable of type Person
newPersonADT :: PersonADT
newPersonADT = Person { name: "Mario", age: 4 }

-- variable of type Persona
newPerson :: Person
newPerson = { name: "Luigi", age: 4 }

-- function that takes any record that has the fields "name", "age" (and possibly more) and returns a String
showPersonaLikeRecord :: forall r. { name :: String, age :: Int | r } -> String
showPersonaLikeRecord p = p.name <> " is " <> show p.age <> " years old"

-- function that takes a "Person" and returns a String
showPersonADT :: PersonADT -> String
-- to access the fields of a Person, we need to match agains the type constructor
showPersonADT (Person r) = r.name <> " IS " <> show r.age <> " YEARS OLD"

-------------------------------
mainWithApplicativeDo :: Effect Unit
mainWithApplicativeDo = do
  log ("- applicative 'do'")
  log (showPersonaLikeRecord newPerson)
  log (showPersonADT newPersonADT)
  log (showPersonaLikeRecord { name: "Bombo", age: 6 , super: "Super"})

mainWithBind :: Effect Unit
mainWithBind = 
  log ("- bind") >>=
  \ _ -> log (showPersonaLikeRecord newPerson) >>= 
  \ _ -> log (showPersonADT newPersonADT) >>=
  \ _ -> log (showPersonaLikeRecord { name: "Bombo", age: 6 , super: "Super"})

mainWithApplySecond :: Effect Unit
mainWithApplySecond =
  log ("- applySecond") *>
  log (showPersonaLikeRecord newPerson) *>
  log (showPersonADT newPersonADT) *>
  log (showPersonaLikeRecord { name: "Bombo", age: 6 , super: "Super"})


--poly :: (forall a. a -> a) -> Boolean
--poly f = (f 0 < 1) == f true

poly :: (forall a. Ord a => Eq a => a -> a) -> Boolean
poly f = (f 0 < 1) == (f 0.2 < 1.0)

buco :: Int -> Int
buco = (_ + 2)

foo x y = x * y + y
fooBy2 = (_ `foo` 2)
fooBy2' = (flip foo) 2

f :: Maybe Boolean -> Either Boolean Int -> String
f a b = case a, b of
  Just true,  Left _     -> "Just is true"
  Just false, Left _     -> "Just is true"
  Nothing,    Left _     -> "Just is true"
--Just true,  Right 0    -> "Both true (0)"
  Just true,  Right x    | x == 0 -> "Both true (0)"
                         | x > 0  -> "Both true (> 0)"
                         | otherwise  -> "Both true (< 0)"
  Just false, Right _    -> "Just false"
  Nothing,    Right 0    -> "Right is true (1)"
  Nothing,    Right _    -> "Right is true (_)"
--  _,         _          -> "Else …"

bar :: Int
bar = if true then 1 else 0
-- f (Just true) (Right true)

g :: Maybe Boolean -> Either Boolean Boolean -> String
g (Just true) (Right true)  = "Both true"
g (Just true) (Left _)      = "Just is true"
g Nothing     (Right true)  = "Right is true"
g _           _             = "Both are false"

factorial :: Int -> Int
factorial =
  let
    go :: Int -> Int -> Int
    go acc 1 = acc
    go acc n = go (acc * n) (n - 1)
  in
    go 1

factorial' :: Int -> Int
factorial' = go 1
  where
    go :: Int -> Int -> Int
    go acc 1 = acc
    go acc n = go (acc * n) (n - 1)


-- bar_1 = factorial 10
-- bar_2 = go 1 1

maybeSum :: Maybe Number -> Maybe Number -> Maybe Number
maybeSum a b = do
  n <- a
  m <- b
  let result = n + m 
  pure result

maybeSum' :: Maybe Number -> Maybe Number -> Maybe Number
maybeSum' a b =
  bind a \n ->
    bind b \m ->
      let result = n + m
      in pure result


-- show' xs = "[" <> joinWith ", " (Data.Functor.map show xs) <> "]"
-- show'' xs = "[" <> arrayAsList <> "]"
--   where
--     arrayAsList = joinWith ", " (Data.Functor.map show xs)

-- ============================================
--  Instance Chains
--  https://liamgoodacre.github.io/purescript/instance/chain/2017/08/18/purescript-instance-chain.html

class MyShow a where
  myShow :: a -> String

data MysteryItem = MysteryItem

instance showString :: MyShow String where
  myShow s = s
else instance showBoolean :: MyShow Boolean where
  myShow true = "true"
  myShow false = "false"
else instance showA :: MyShow a where
   myShow _ = "Invalid"

-- else instance customMyShow :: MyShow MysteryItem where
--   myShow MysteryItem = "Mystery Item"

f' :: forall a. MyShow a => a -> String
f' x = myShow x

f'' :: MysteryItem -> String
f'' x = myShow x

-- ===================================================
--  https://leanpub.com/purescript/read#leanpub-auto-multi-parameter-type-classes
--  Multi-Parameter Type Classes => PureScript by Example, page 72 (?)
--  Functional Dependencies ?

class Stream stream element | stream -> element where
  uncons :: stream -> Maybe { head :: element, tail :: stream }

genericTail :: forall stream element. Stream stream element => stream -> Maybe stream
genericTail xs = Data.Functor.map (\a -> a.tail) (uncons xs)
genericTail' xs = Data.Functor.map _.tail (uncons xs)

factors :: Int -> Array (Tuple Int Int)
factors n = do
  a <- 1 .. n
  b <- 1 .. a
  guard $ a * b == n
  pure $ Tuple a b

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

checkPasswordLength' :: Int -> Int -> String -> Either String String
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

requireAlphaNum' :: String -> Either String String
requireAlphaNum' word =
  case (isAllChar Data.Char.Unicode.isAlphaNum word) of
    true -> Right word
    false -> Left "Non alphanum char present"

cleanWhitespace :: String -> Maybe String 
cleanWhitespace word = 
  let tw = Data.String.Common.trim word
   in if (null tw) then Nothing else Just tw

cleanWhitespace' :: String -> Either String String
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

validatePassword' :: String -> Either String String
validatePassword' password = 
  cleanWhitespace' password
  >>= requireAlphaNum'
  >>= (checkPasswordLength' 8 30)

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- bind :: forall a b. m a -> (a -> m b) -> m b

-- ===================================================

main :: Effect Unit
main = do
  -- mainWithApplicativeDo
  -- mainWithBind
  -- mainWithApplySecond
  -- log ("fooBy2: " <> show (fooBy2 10))
  -- log ("fooBy2': " <> show (fooBy2' 10))
  -- log (g (Just true) (Right true))
  -- log (show (maybeSum (Just 1.0) (Just 3.0)))
  -- log (show (maybeSum (Nothing) (Just 3.0)))
  -- log $ f' "ciao" -- hello
  -- log $ f' true -- true
  -- log $ f' MysteryItem
  -- log $ f'' MysteryItem -- Invalid
  -- log $ validateUsernamePassword "joe" "clipperz"
  -- log $ validateUsernamePassword "joe" ""
  -- log $ show (checkAnagram "Filippo" "Giulio")
  -- log $ show (checkAnagram "cat" "tac")
  -- log $ translateWord "letsspeak"
  log $ show $ checkPasswordLength 8 30 "ciao"
  log $ show (checkPasswordLength 8 30 "letsspeak")
  log $ show $ validatePassword' "arrivederci1234"
  log $ show $ validatePassword' "@rrivederci1234"
  log $ show $ validatePassword' "1234"
  -- log $ show (requireAlphaNum "ciao54321")
  -- log $ show (requireAlphaNum "ciao. ciao.")
  -- log $ show (requireAlphaNum "letsspeak")
  -- log $ show (requireAlphaNum $ translateWord "letsspeak")
  -- log $ show (cleanWhiteSpaceFabio "  prova  ")
  -- log $ show (cleanWhiteSpaceFabio "       ")
  -- log $ show (isWord "ciao")
  -- log $ show (isWord "")
  -- log $ show (isWord "pippo")
  -- log $ show (isWord "pipp0")
  
