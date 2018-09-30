module System.Monopati.Posix where

import "base" Data.Foldable (Foldable (foldr))
import "base" Data.Function ((.), ($), flip)
import "base" Data.Kind (Type)
import "base" Data.List (init)
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.Semigroup (Semigroup ((<>)))
import "base" Data.String (String)
import "base" Text.Show (Show (show))
import "free" Control.Comonad.Cofree (Cofree ((:<)))

data Points = Directory | File -- What the path points to?
data Origin = Root | Home | Vague -- What is the beginning of the path?
data To -- Dummy type needed only for beauty type declarations

type Path = Cofree Maybe String

-- | The internal type of path representation
newtype Outline (origin :: Origin) (points :: Points) = Outline { outline :: Path }

instance Show (Outline Root Directory) where
	show = flip (<>) "/" . foldr (\x acc -> acc <> "/" <> x) "" . outline

instance Show (Outline Root File) where
	show = foldr (\x acc -> acc <> "/" <> x) "" . outline

instance Show (Outline Home Directory) where
	show = (<>) "~/" . foldr (\x acc -> x <> "/" <> acc) "" . outline

instance Show (Outline Home File) where
	show = (<>) "~/" . init . foldr (\x acc -> x <> "/" <> acc) "" . outline

instance Show (Outline Vague Directory) where
	show = foldr (\x acc -> x <> "/" <> acc) "" . outline

instance Show (Outline Vague File) where
	show = init . foldr (\x acc -> x <> "/" <> acc) "" . outline

type family Absolute (path :: Type) (to :: Type) (points :: Points) :: Type where
	Absolute Path To points = Outline Root points

type family Homeward (path :: Type) (to :: Type) (points :: Points) :: Type where
	Homeward Path To points = Outline Home points

type family Relative (path :: Type) (to :: Type) (points :: Points) :: Type where
	Relative Path To points = Outline Vague points

part :: String -> Outline origin points
part x = Outline $ x :< Nothing

(<^>) :: Relative Path To Directory -> Relative Path To points -> Relative Path To points
Outline (x :< Nothing) <^> Outline that = Outline $ x :< Just that
Outline (x :< Just this) <^> Outline that = part x <^> (Outline this <^> Outline that)

(</>) :: Absolute Path To Directory -> Relative Path To points -> Absolute Path To points
Outline absolute </> Outline (x :< Nothing) = Outline . (:<) x . Just $ absolute
Outline absolute </> Outline (x :< Just xs) = (Outline . (:<) x . Just $ absolute) </> Outline xs

(<~/>) :: Absolute Path To points -> Homeward Path To points -> Absolute Path To points
Outline absolute <~/> Outline (x :< Nothing) = Outline . (:<) x . Just $ absolute
Outline absolute <~/> Outline (x :< Just xs) = (Outline . (:<) x . Just $ absolute) <~/> Outline xs
