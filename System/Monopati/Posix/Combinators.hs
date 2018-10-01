module System.Monopati.Posix.Combinators
	( Points (..), Origin (..), To, Path, Outline (..)
	, Absolute (..), Homeward (..), Relative (..)
	, part, parent, (<^>), (</>), (<~/>)) where

import "base" Data.Eq (Eq ((==)))
import "base" Data.Foldable (Foldable (foldr))
import "base" Data.Function ((.), ($), flip)
import "base" Data.Functor ((<$>))
import "base" Data.Kind (Type)
import "base" Data.List (filter, init)
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.Semigroup (Semigroup ((<>)))
import "base" Data.String (String)
import "base" Text.Show (Show (show))
import "free" Control.Comonad.Cofree (Cofree ((:<)), unwrap)

-- | What the path points to?
data Points = Directory | File

-- | What is the beginning of the path?
data Origin
	= Root -- ^ (@/@) Starting point for absolute path
	| Home -- ^ (@~/@) Indication of home directory
	| Vague -- ^ Uncertain relative path

-- | Dummy type needed only for beautiful type declarations
data To

-- | Path is non-empty sequence of folders or file (in the end)
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

-- | Immerse string into a path, filter slashes
part :: String -> Outline origin points
part x = Outline $ (filter (== '/') x) :< Nothing

{-| @
"usr//local///" + "etc///" = "usr///local///etc//"
@ -}
(<^>) :: Relative Path To points -> Relative Path To Directory -> Relative Path To points
Outline this <^> Outline (x :< Nothing)= Outline $ x :< Just this
Outline this <^> Outline (x :< Just that) = (Outline this <^> Outline that) <^> part x

{-| @
"//usr///bin///" + "git" = "///usr///bin//git"
@ -}
(</>) :: Relative Path To points -> Absolute Path To Directory -> Absolute Path To points
Outline (x :< Nothing) </> Outline absolute = Outline . (:<) x . Just $ absolute
Outline (x :< Just xs) </> Outline absolute = Outline xs </> (Outline . (:<) x . Just $ absolute)

{-| @
"//usr///local///" + "~///etc///" = "///usr///local///etc//"
@ -}
(<~/>) :: Homeward Path To points -> Absolute Path To points -> Absolute Path To points
Outline (x :< Nothing) <~/> Outline absolute= Outline . (:<) x . Just $ absolute
Outline (x :< Just xs) <~/> Outline absolute= Outline xs <~/> (Outline . (:<) x . Just $ absolute)

-- | Take parent directory of current pointed entity
parent :: Absolute Path To points -> Maybe (Absolute Path To Directory)
parent = (<$>) Outline . unwrap . outline
