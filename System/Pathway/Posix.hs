module System.Pathway.Posix where

import "base" Control.Applicative ((*>))
import "base" Data.Bool (Bool (True))
import "base" Data.Foldable (foldr)
import "base" Data.Function ((.), ($), flip)
import "base" Data.Functor ((<$>), ($>))
import "base" Data.List (reverse, tail)
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Data.String (String)
import "base" System.IO (IO)
import "base" Text.Show (show)
import "directory" System.Directory
	(createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)
import "free" Control.Comonad.Cofree (Cofree ((:<)))
import "split" Data.List.Split (splitOn)

import System.Pathway (Path (Path, path), Reference (Absolute, Relative), Points (Directory), (</>))

-- | Return Nothing, if current working directory is root (cwd)
current :: IO (Maybe (Path Absolute Directory))
current = parse <$> getCurrentDirectory where

	parse :: String -> Maybe (Path Absolute Directory)
	parse "/" = Nothing
	parse directory = (<$>) Path
		. foldr (\el -> Just . (:<) el) Nothing
		. reverse . splitOn "/" . tail $ directory

-- | Change current working directory (cd)
change :: Path Absolute Directory -> IO (Path Absolute Directory)
change directory = setCurrentDirectory (show directory) $> directory

-- | Create an absolute path that points to directory (mkdir)
create :: Path Relative Directory -> IO (Path Absolute Directory)
create directory = createDirectoryIfMissing True (show directory) *>
	(maybe (Path @Absolute . path $ directory) (flip (</>) directory) <$> current)
