module System.Monopati.Posix where

import "base" Data.Kind (Type)
import "base" Data.Maybe (Maybe)
import "base" Data.String (String)
import "free" Control.Comonad.Cofree (Cofree)

data Points = Directory | File -- What the path points to?
data Origin = Root | Home | Vague -- What is the beginning of the path?
data To -- Dummy type needed only for beauty type declarations

type Stack = Cofree Maybe

-- | The internal type of path representation
newtype Outline (origin :: Origin) (points :: Points)
	= Outline { outline :: Stack String }

newtype Path = Path { path :: Stack String }

type family Absolute (path :: Type) (to :: Type) (points :: Points) :: Type where
	Absolute Path To points = Outline Root points

type family Homeward (path :: Type) (to :: Type) (points :: Points) :: Type where
	Homeward Path To points = Outline Home points

type family Relative (path :: Type) (to :: Type) (points :: Points) :: Type where
	Relative Path To points = Outline Vague points
