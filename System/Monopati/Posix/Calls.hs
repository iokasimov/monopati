module System.Monopati.Posix.Calls (current, home, create, change, remove) where

import "base" Data.Bool (Bool (True))
import "base" Data.Foldable (Foldable (foldr))
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Data.List (tail, reverse)
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.String (String)
import "base" System.IO (IO)
import "base" Text.Show (show)
import "directory" System.Directory (createDirectoryIfMissing, getCurrentDirectory
	, getHomeDirectory, removeDirectoryRecursive, setCurrentDirectory)
import "free" Control.Comonad.Cofree (Cofree ((:<)))
import "split" Data.List.Split (splitOn)

import System.Monopati.Posix.Combinators (Points (Directory), To, Path, Origin (Root), Outline (Outline), Absolute, Homeward, Relative)

-- | Return Nothing, if current working directory is root (cwd)
current :: IO (Maybe (Absolute Path To Directory))
current = parse <$> getCurrentDirectory

-- | Retrieve absolute path of home directory (echo ~)
home :: IO (Maybe (Absolute Path To Directory))
home = parse <$> getHomeDirectory

parse :: String -> Maybe (Absolute Path To Directory)
parse "/" = Nothing
parse directory = (<$>) Outline
	. foldr (\el -> Just . (:<) el) Nothing
	. reverse . splitOn "/" . tail $ directory

-- | Create a directory (mkdir)
create :: Absolute Path To Directory -> IO ()
create = createDirectoryIfMissing True . show

-- | Change directory (cd)
change :: Absolute Path To Directory -> IO ()
change = setCurrentDirectory . show

-- | Remove directory (rm -rf)
remove :: Absolute Path To Directory -> IO ()
remove = removeDirectoryRecursive . show