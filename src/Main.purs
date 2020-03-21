module Main where

-- import Prelude -- core functions
import Control.Applicative (pure)
import Control.Apply ((*>))
import Control.Bind (bind, discard, (>>=))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Console (log)
import Data.Semiring ((+), (*))
import Data.Maybe (Maybe (..))

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

-------------------------------

foo :: Int -> Int -> Int
foo x y = x * y + y
test :: Int
test = 10 `foo` 20
test1 :: Int
test1 = foo 10 20
fooBy2 :: Int -> Int
fooBy2 = (_ `foo` 2)

-------------------------------

f :: Int -> String
f = 
  case _ of
    0 -> "None"
    1 -> "One"
    _ -> "Some"

-------------------------------

maybeSum :: Maybe Number -> Maybe Number -> Maybe Number
maybeSum a b = do
  n <- a
  m <- b
  let result = n + m
  pure result

------------------------------- 

mainTest :: Effect Unit
mainTest = do
  log (show test)
  log (show test1)
  log (show (fooBy2 50))
  log (f 1)
  log (show (maybeSum (Just 5.0) (Just 5.0)))

-------------------------------

main :: Effect Unit
main = do
  mainWithApplicativeDo
  mainWithBind
  mainWithApplySecond
  mainTest