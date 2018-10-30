module System.Monopati.Posix.Combinators
	( Points (..), Origin (..), To, Path, Outline (..)
	, Absolute, Currently, Homeward, Relative, Incompleted
	, deeper, part, parent, (<^>), (<.^>), (<~^>), (<^^>), (</>), (</.>), (</~>), (</^>)) where

import "base" Control.Applicative (pure)
import "base" Data.Eq (Eq ((/=)))
import "base" Data.Foldable (Foldable (foldr))
import "base" Data.Function ((.), ($), (&), flip)
import "base" Data.Functor ((<$>))
import "base" Data.Kind (Constraint, Type)
import "base" Data.List (filter, init, reverse)
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Data.Semigroup (Semigroup ((<>)))
import "base" Data.String (String)
import "base" Text.Read (Read (readsPrec))
import "base" Text.Show (Show (show))
import "free" Control.Comonad.Cofree (Cofree ((:<)), unwrap)
import "split" Data.List.Split (endBy, splitOn)

-- | What the path points to?
data Points = Directory | File

-- | What is the beginning of the path?
data Origin
	= Root -- ^ (@/@) Starting point for absolute path
	| Current -- ^ (@~/@) Relatively current working directory
	| Home -- ^ (@~/@) Indication of home directory
	| Vague -- ^ Uncertain relative path

-- | Dummy type needed only for beautiful type declarations
data To

-- | Path is non-empty sequence of folders or file (in the end)
type Path = Cofree Maybe String

-- | The internal type of path representation
newtype Outline (origin :: Origin) (points :: Points) =
	Outline { outline :: Path } deriving Eq

instance Show (Outline Root Directory) where
	show = flip (<>) "/" . foldr (\x acc -> acc <> "/" <> x) "" . outline

instance Show (Outline Root File) where
	show = foldr (\x acc -> acc <> "/" <> x) "" . outline

instance Show (Outline Current Directory) where
	show = (<>) "./" . foldr (\x acc -> x <> "/" <> acc) "" . outline

instance Show (Outline Current File) where
	show = (<>) "./" . init . foldr (\x acc -> x <> "/" <> acc) "" . outline

instance Show (Outline Home Directory) where
	show = (<>) "~/" . foldr (\x acc -> x <> "/" <> acc) "" . outline

instance Show (Outline Home File) where
	show = (<>) "~/" . init . foldr (\x acc -> x <> "/" <> acc) "" . outline

instance Show (Outline Vague Directory) where
	show = foldr (\x acc -> x <> "/" <> acc) "" . outline

instance Show (Outline Vague File) where
	show = init . foldr (\x acc -> x <> "/" <> acc) "" . outline

instance Read (Outline Root Directory) where
	readsPrec _ ('/':[]) = []
	readsPrec _ ('/':rest) = foldr (\el -> Just . (:<) el) Nothing
		(reverse $ endBy "/" rest) & maybe [] (pure . (,[]) . Outline)
	readsPrec _ _ = []

instance Read (Outline Root File) where
	readsPrec _ ('/':[]) = []
	readsPrec _ ('/':rest) = foldr (\el -> Just . (:<) el) Nothing
		(reverse $ splitOn "/" rest) & maybe [] (pure . (,[]) . Outline)
	readsPrec _ _ = []

instance Read (Outline Current Directory) where
	readsPrec _ ('.':'/':[]) = []
	readsPrec _ ('.':'/':rest) = foldr (\el -> Just . (:<) el) Nothing
		(endBy "/" rest) & maybe [] (pure . (,[]) . Outline)
	readsPrec _ _ = []

instance Read (Outline Current File) where
	readsPrec _ ('.':'/':[]) = []
	readsPrec _ ('.':'/':rest) = foldr (\el -> Just . (:<) el) Nothing
		(splitOn "/" rest) & maybe [] (pure . (,[]) . Outline)
	readsPrec _ _ = []

instance Read (Outline Home Directory) where
	readsPrec _ ('~':'/':[]) = []
	readsPrec _ ('~':'/':rest) = foldr (\el -> Just . (:<) el) Nothing
		(endBy "/" rest) & maybe [] (pure . (,[]) . Outline)
	readsPrec _ _ = []

instance Read (Outline Home File) where
	readsPrec _ ('~':'/':[]) = []
	readsPrec _ ('~':'/':rest) = foldr (\el -> Just . (:<) el) Nothing
		(splitOn "/" rest) & maybe [] (pure . (,[]) . Outline)
	readsPrec _ _ = []

instance Read (Outline Vague Directory) where
	readsPrec _ [] = []
	readsPrec _ string = foldr (\el -> Just . (:<) el) Nothing
		(endBy "/" string) & maybe [] (pure . (,[]) . Outline)

instance Read (Outline Vague File) where
	readsPrec _ [] = []
	readsPrec _ string = foldr (\el -> Just . (:<) el) Nothing
		(splitOn "/" string) & maybe [] (pure . (,[]) . Outline)

type family Absolute (path :: Type) (to :: Type) (points :: Points) :: Type where
	Absolute Path To points = Outline Root points

type family Currently (path :: Type) (to :: Type) (points :: Points) :: Type where
	Currently Path To points = Outline Current points

type family Homeward (path :: Type) (to :: Type) (points :: Points) :: Type where
	Homeward Path To points = Outline Home points

type family Relative (path :: Type) (to :: Type) (points :: Points) :: Type where
	Relative Path To points = Outline Vague points

type family Incompleted (outline :: Origin) :: Constraint where
	Incompleted Current = ()
	Incompleted Home = ()
	Incompleted Vague = ()

-- | Immerse string into a path, filter slashes
part :: String -> Outline origin points
part x = Outline $ (filter (/= '/') x) :< Nothing

-- Add relative path to uncompleted path
(<^>) :: forall origin points . Incompleted origin =>
	Outline origin Directory -> Relative Path To points -> Outline origin points
Outline (x :< Nothing) <^> Outline that = Outline $ x :< Just that
Outline (x :< Just this) <^> Outline that = (<^>) (part @origin x)
	$ (<^>) @Vague (Outline this) (Outline that)

{-| @
".//etc///" + "usr///local///" + = ".///etc///usr///local//"
@ -}
(<.^>) :: Currently Path To Directory -> Relative Path To points -> Currently Path To points
currently <.^> relative = currently <^> relative

{-| @
"~//etc///" + "usr///local///" + = "~///etc///usr///local//"
@ -}
(<~^>) :: Homeward Path To Directory -> Relative Path To points -> Homeward Path To points
homeward <~^> relative = homeward <^> relative

{-| @
"//etc///" + "usr///local///" + = "~///etc///usr///local//"
@ -}
(<^^>) :: Relative Path To Directory -> Relative Path To points -> Relative Path To points
relative' <^^> relative = relative' <^> relative

-- | Absolutize uncompleted path
(</>) :: forall origin points . Incompleted origin =>
	Absolute Path To Directory-> Outline origin points -> Absolute Path To points
Outline absolute </> Outline (x :< Nothing) = Outline . (:<) x . Just $ absolute
Outline absolute </> Outline (x :< Just xs) = (</>) @origin (Outline . (:<) x . Just $ absolute) $ Outline xs

{-| @
"//usr///local///" + ".///etc///" = "///usr///local///etc//"
@ -}
(</.>) :: Absolute Path To Directory -> Currently Path To points -> Absolute Path To points
absolute </.> currently = absolute </> currently

{-| @
"//usr///local///" + "~///etc///" = "///usr///local///etc//"
@ -}
(</~>) :: Absolute Path To Directory -> Homeward Path To points -> Absolute Path To points
absolute </~> homeward = absolute </> homeward

{-| @
"//usr///bin///" + "git" = "///usr///bin//git"
@ -}
(</^>) :: Absolute Path To Directory -> Relative Path To points -> Absolute Path To points
absolute </^> relative = absolute </> relative

-- | Take parent directory of current pointed entity
parent :: Absolute Path To points -> Maybe (Absolute Path To Directory)
parent = (<$>) Outline . unwrap . outline

-- | Take the next piece of relative path
deeper :: Relative Path To points -> Maybe (Relative Path To points)
deeper = (<$>) Outline . unwrap . outline
