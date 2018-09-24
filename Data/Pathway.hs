module Data.Pathway where

import "base" Data.Function (($))
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.String (String)
import "free" Control.Comonad.Cofree (Cofree ((:<)))

type Stack = Cofree Maybe

-- | It helps define some rules for merging paths
data Reference = Absolute | Relative

-- | Path may points to a directory or a file
data Points = Directory | File

-- | These two phantom paramaters needs for statis analysis
data Path (reference :: Reference) (points :: Points) = Path (Stack String)

-- | Immerse some string into a path component
part :: String -> Path referece points
part x = Path $ x :< Nothing
