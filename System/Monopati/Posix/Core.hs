module System.Monopati.Posix.Core (Points (..), Origin (..), To, Path, Outline (..)) where

import "base" Control.Applicative (pure)
import "base" Data.Eq (Eq)
import "base" Data.Foldable (Foldable (foldr))
import "base" Data.Function ((.), ($), (&), flip)
import "base" Data.List (init, reverse)
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Data.Semigroup (Semigroup ((<>)))
import "base" Data.String (String)
import "base" Text.Read (Read (readsPrec))
import "base" Text.Show (Show (show))
import "free" Control.Comonad.Cofree (Cofree ((:<)))
import "split" Data.List.Split (endBy, splitOn)

-- | What the path points to?
data Points = Directory | File

-- | What is the beginning of the path?
data Origin
	= Root -- ^ (@/@) Starting point for absolute path
	| Now -- ^ (@~/@) Relatively current working directory
	| Home -- ^ (@~/@) Indication of home directory
	| Parent -- ^ (@../@) Parent of current working directory
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

instance Show (Outline Now Directory) where
	show = (<>) "./" . foldr (\x acc -> x <> "/" <> acc) "" . outline

instance Show (Outline Now File) where
	show = (<>) "./" . init . foldr (\x acc -> x <> "/" <> acc) "" . outline

instance Show (Outline Home Directory) where
	show = (<>) "~/" . foldr (\x acc -> x <> "/" <> acc) "" . outline

instance Show (Outline Home File) where
	show = (<>) "~/" . init . foldr (\x acc -> x <> "/" <> acc) "" . outline

instance Show (Outline Parent Directory) where
	show = (<>) "../" . foldr (\x acc -> x <> "/" <> acc) "" . outline

instance Show (Outline Parent File) where
	show = (<>) "../" . init . foldr (\x acc -> x <> "/" <> acc) "" . outline

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

instance Read (Outline Now Directory) where
	readsPrec _ ('.':'/':[]) = []
	readsPrec _ ('.':'/':rest) = foldr (\el -> Just . (:<) el) Nothing
		(endBy "/" rest) & maybe [] (pure . (,[]) . Outline)
	readsPrec _ _ = []

instance Read (Outline Now File) where
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

instance Read (Outline Parent Directory) where
	readsPrec _ ('.':'.':'/':[]) = []
	readsPrec _ ('.':'.':'/':rest) = foldr (\el -> Just . (:<) el) Nothing
		(endBy "/" rest) & maybe [] (pure . (,[]) . Outline)
	readsPrec _ _ = []

instance Read (Outline Parent File) where
	readsPrec _ ('.':'.':'/':[]) = []
	readsPrec _ ('.':'.':'/':rest) = foldr (\el -> Just . (:<) el) Nothing
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
