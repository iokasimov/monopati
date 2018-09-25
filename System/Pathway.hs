module System.Pathway (Path (..), Points (..), Reference (..)
	, part, (<^>), (</>)) where

import "base" Data.Function ((.), ($))
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.String (String)
import "base" Data.Semigroup ((<>))
import "base" Text.Show (Show (show))
import "free" Control.Comonad.Cofree (Cofree ((:<)))

type Stack = Cofree Maybe

-- | Relative paths defined in direct order, absolute - in reverse
data Reference = Absolute | Relative

-- | Path may points to a directory or a file
data Points = Directory | File

-- | These two phantom paramaters needs for statis analysis
data Path (reference :: Reference) (points :: Points) = Path { path :: Stack String }

instance Show (Path Absolute points) where
	show (Path (x :< Just xs)) = show (Path @Absolute xs) <> "/" <> x
	show (Path (x :< Nothing)) = "/" <> x

instance Show (Path Relative points) where
	show (Path (x :< Just xs)) = x <> "/" <> show (Path @Relative xs)
	show (Path (x :< Nothing)) = x

-- | Immerse some string into a path component
part :: String -> Path referece points
part x = Path $ x :< Nothing

-- | Concatenate Relative and Relative paths
(<^>) :: Path Relative Directory -> Path Relative points -> Path Relative points
Path (x :< Nothing) <^> Path that = Path $ x :< Just that
Path (x :< Just this) <^> Path that = part x <^> (Path this <^> Path that)

-- | Concatenate Absolute and Relative paths
(</>) :: Path Absolute Directory -> Path Relative points -> Path Absolute points
Path absolute </> Path (x :< Nothing) = Path . (:<) x . Just $ absolute
Path absolute </> Path (x :< Just xs) = (Path . (:<) x . Just $ absolute) </> Path xs
