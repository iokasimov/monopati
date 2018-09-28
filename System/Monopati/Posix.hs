module System.Monopati.Posix where

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
import "directory" System.Directory (createDirectoryIfMissing, getCurrentDirectory, getHomeDirectory, setCurrentDirectory)
import "free" Control.Comonad.Cofree (Cofree ((:<)))
import "split" Data.List.Split (splitOn)

import System.Monopati (Path (Path, path), Reference (Absolute, Relative), Points (Directory), (</>))

data Starts = Root | Home

newtype Entry (starts :: Starts) = Entry
	{ entry :: Maybe (Path Absolute Directory) }

(*/*) :: Entry Root -> Path Relative points -> Path Absolute points
Entry Nothing */* Path relative = Path @Absolute relative
Entry (Just absolute) */* relative = absolute </> relative

(*~*) :: Entry Home -> Path Relative points -> Path Absolute points
Entry Nothing *~* Path relative = Path @Absolute relative
Entry (Just absolute) *~* relative = absolute </> relative

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
