module System.Pathway.Posix where

import "base" Data.Foldable (foldr)
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Data.List (reverse, tail)
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.String (String)
import "base" System.IO (IO)
import "directory" System.Directory (getCurrentDirectory)
import "free" Control.Comonad.Cofree (Cofree ((:<)))
import "split" Data.List.Split (splitOn)

import System.Pathway (Path (Path), Reference (Absolute), Points (Directory))

-- | Return Nothing, if current working directory is root
current :: IO (Maybe (Path Absolute Directory))
current = parse <$> getCurrentDirectory where

	parse :: String -> Maybe (Path Absolute Directory)
	parse "/" = Nothing
	parse directory = (<$>) Path
		. foldr (\el -> Just . (:<) el) Nothing
		. reverse . splitOn "/" . tail $ directory
