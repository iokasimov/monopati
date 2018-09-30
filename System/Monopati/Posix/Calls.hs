module System.Monopati.Posix.Calls (current, home) where

import "base" Data.Foldable (Foldable (foldr))
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Data.List (tail, reverse)
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.String (String)
import "base" System.IO (IO)
import "directory" System.Directory (getCurrentDirectory, getHomeDirectory)
import "free" Control.Comonad.Cofree (Cofree ((:<)))
import "split" Data.List.Split (splitOn)

import System.Monopati.Posix.Combinators (Points (Directory), To, Path, Outline (Outline), Absolute, Homeward, Relative)

current :: IO (Maybe (Absolute Path To Directory))
current = parse <$> getCurrentDirectory

home :: IO (Maybe (Absolute Path To Directory))
home = parse <$> getHomeDirectory

parse :: String -> Maybe (Absolute Path To Directory)
parse "/" = Nothing
parse directory = (<$>) Outline
	. foldr (\el -> Just . (:<) el) Nothing
	. reverse . splitOn "/" . tail $ directory
