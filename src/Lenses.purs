module Lenses where

import Control.Bind (discard)
import Control.Semigroupoid ((<<<), (>>>))
import Data.Either (Either(..))
import Data.Function (($))
import Data.Lens.Fold (preview)
import Data.Lens.Getter (view)
import Data.Lens.Index (ix)
import Data.Lens.Lens (lens, lens')
import Data.Lens.Lens.Tuple (_2)
import Data.Lens.Prism (review)
import Data.Lens.Prism.Either (_Left, _Right)
import Data.Lens.Prism.Maybe (_Just)
import Data.Lens.Record (prop)
import Data.Lens.Setter (set, over)
import Data.Lens.Types (Lens')
import Data.Lens.At (at)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (class Strong) 
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
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
type Animal = { id :: Int, name :: String, tags :: Array String, vaccinated :: Maybe String }

giulio :: Animal
giulio = { id:38, name:"Giulio Cesare", tags:["a", "b"], vaccinated:Just "true" }

pica :: Animal 
pica = { id:13, name:"Pica", tags:["gatto"], vaccinated:Nothing }

animals :: Map.Map Int Animal
animals = Map.union (Map.singleton 3838 giulio) (Map.singleton 1313 pica)

animals' :: Map.Map String Animal
animals' = Map.fromFoldable [(Tuple "3838" giulio), (Tuple "1313" pica)]

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

_name :: forall a r. Lens' { name :: a | r } a
_name = prop (SProxy :: SProxy "name")

_name' :: forall a r. Lens' { name :: a | r } a
_name' = lens' \record -> Tuple record.name (\new -> record { name = new })

_1' :: forall a b. Lens' (Tuple a b) a
_1' = lens' \(Tuple f s) -> Tuple f (\f' -> Tuple f' s)

-- lens' :: forall s a. (s -> Tuple a (a -> s)) -> Lens' s a
_2' :: forall a b. Lens' (Tuple a b) b
_2' = lens' \(Tuple f s) -> Tuple s (\s' -> Tuple f s')

-- at :: forall b a m. At m a b => a -> Lens' m (Maybe b)
-- _animal :: Int -> Lens' (Map.Map Int Animal) Animal
-- _animal = at

-- getLeft'
e :: Either Int String
e = Right "ciao"

_setString :: String -> Either Int String
_setString s = Right s

values :: Array Int
values = [1, 3, 5]

main :: Effect Unit
main = do
    logShow $ "[ 1] " <> show (_animal' model 3838)
    logShow $ "[ 2] " <> view _1' (Tuple "five" 10)
    logShow $ "[ 3] " <> view _1' (set _1' "four" (Tuple "five" 10))
    logShow $ "[ 4] " <> show (view _2' (Tuple "five" 10))
    logShow $ "[ 5] " <> view _name' giulio
    logShow $ "[ 6] " <> view _name' (set _name' "fabio" giulio)
    logShow $ "[ 7] " <> show (view (prop (SProxy::SProxy "id")) (set _name' "fabio" giulio))
    logShow $ "[ 8] " <> view _name' (over _name' (\n -> n <> " - " <> n) giulio)
    logShow $ "[ 9] " <> show (view (at 3838) animals)
    logShow $ "[10] " <> show (Map.lookup 3838 animals)
    logShow $ "[11] " <> show (view (at 3839) animals)
    logShow $ "[12] " <> show (Map.lookup 3839 animals)
    logShow $ "[13] " <> show (view (at "3838") animals')

    logShow $ "[20] " <> show (set (at "4040") (Just pica) animals')
    logShow $ "[21] " <> show (((set (at "4040")) <<< Just) pica animals')
    logShow $ "[22] " <> show (set (at "3838") Nothing animals')
    
    -- logShow $ "[30] " <> show (view _Just (view (prop (SProxy::SProxy "vaccinated")) giulio ))

    logShow $ "[40] " <> view _Left (Right "ciao")
    -- logShow $ "[41] " <> show (preview _Left (Right "ciao")) -- what could a be in "Either a String"?
    logShow $ "[41] " <> show (preview _Left e)
    -- logShow $ "[42] " <> show (view _Left e) -- view wants data types with null values (monoid instance) otherwise it breaks if the value is not present
    logShow $ "[50] " <> show (review _Right "ciao" :: Either Int String)
    logShow $ "[51] " <> show (review _Left  10 :: Either Int String)

    logShow $ "[60] " <> show (set (ix 2) 100 values)
    -- logShow $ "[61] " <> show (set (ix 10) 100 values)

    -- animal => view (at 3838) animals
    -- vaccinated => view (prop (SProxy :: SProxy "vaccinated")) animal
    -- _Just => view _Just vaccinated