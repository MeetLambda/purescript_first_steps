module Lenses where

import Control.Semigroupoid ((<<<), (>>>))
import Data.Function (($))
import Data.Lens.Lens (lens)
import Data.Lens.Types (Lens, Lens')
import Data.Lens.At (at)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Profunctor.Strong (class Strong) 
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Console (logShow)


-- lens :: forall s t a b. (s -> a) -> (s -> b -> t) -> Lens s t a b
-- lens' :: forall s s a a. (s -> a) -> (s -> a -> s) -> Lens' s s a a

-- class Index m a b | m -> a, m -> b where
-- ix :: a -> Traversal' m b

-- class (Index m a b) <= At m a b | m -> a, m -> b where
-- at :: a -> Lens' m (Maybe b)
-- 
type Model = { aField :: String, animals :: Map.Map Int Animal, anotherField :: String }
type Animal = { id :: Int, name :: String, tags :: Array String}

giulio :: Animal
giulio = { id:38, name:"Giulio Cesare", tags:["a", "b"] }

pica :: Animal 
pica = { id:13, name:"Pica", tags:["gatto"]}

animals :: Map.Map Int Animal
animals = Map.union (Map.singleton 3838 giulio) (Map.singleton 1313 pica)

model :: Model
model = { aField: ""
        , animals: animals
        , anotherField: "other" 
        }

_animal' :: Model -> Int  -> Maybe Animal
_animal' m id = Map.lookup id m.animals

_animal'' :: Int -> Model ->  Maybe Animal
_animal'' id = _.animals >>> Map.lookup id

-- _animals :: forall a b c d. Strong a => a c d -> a { animals :: c | b }
-- _animals :: Model -> (Model -> Map.Map Int Animal -> Model)  -> Lens' Model (Map.Map Int Animal)
-- _animals :: forall a b c d. Strong a => a c d -> a { animals :: c | b }
-- _animals    :: forall a b c d. Strong a => a c d -> a { animals :: c | b} { animals :: d | b}
_animals    :: forall a . Strong a => a (Map.Map Int Animal) (Map.Map Int Animal) -> a Model Model
_animals = lens _.animals $ (_ { animals = _ })

-- Lens' s a = Lens s s a a 
-- Lens s t a b = forall p. Strong p => Optic p s t a b
-- Setter s t a b = Optic Function s t a b
-- _animal :: Int -> Setter s t a b

-- (<<<) :: forall b c d. a c d -> a b c -> a b d
-- forall b c d. Lens' c d -> Lens' b c -> Lens' b d

-- il risultato di _animal id dovrebbe essere          Lens' b d 

-- Lens' b d
-- Lens b b d d
-- Lens s t a b = Strong p => Optic p s t a b
-- Optic Function b b d d
-- Setter s t a b
-- b = Model

-- lens :: forall s t a b. (s -> a) -> (s -> b -> t) -> Lens s t a b

-- s = Model
-- t = Model
-- a = Map.Map Int Animal
-- b = Map.Map Int Animal
-- _ { animals = _ } :: s -> b -> t :: Model -> Map.Map Int Animal -> Model
-- Model -> Model -> Map.Map Int Animal ->

-- at :: forall b a m. At m a b => a -> Lens' m (Maybe b)
-- (<<<) :: forall m b d. a b d -> a m b -> a m d
-- a = Lens'
-- _animals :: Lens' b d
-- Int -> Lens' m d

_animal id = _animals <<< (at id)

-- over :: forall s t a b. Setter s t a b -> (a -> b) -> s -> t
--  over (_animal 3838) (map $ Animal.addTag "new tag") model

main :: Effect Unit
main = logShow $  (_animal' model 3838)
    