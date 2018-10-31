module Test.Monopati.Posix (show_then_read_absolute, show_then_read_relative) where

import "base" Control.Applicative (pure, (<*>))
import "base" Control.Monad ((>>=))
import "base" Data.Bool (Bool (True))
import "base" Data.Foldable (foldr)
import "base" Data.Function (flip, (.), ($))
import "base" Data.Functor ((<$>))
import "base" Data.String (String)
import "base" System.IO (print)
import "base" Text.Read (read)
import "base" Text.Show (show)
import "hedgehog" Hedgehog (Gen, Property, assert, forAll, property, success, (===))
import "hedgehog" Hedgehog.Gen (enum, list, string)
import "hedgehog" Hedgehog.Range (linear)

import System.Monopati.Posix (Absolute, Relative, Path, To, Origin (Root, Vague), Points (Directory, File), part, (</^>), (<^^>))

points :: Gen String
points = string (linear 0 100) (enum 'a' 'z')

generate_relative :: Gen (Relative Path To 'Directory)
generate_relative = do
	accumulator <- part <$> points
	raw <- list (linear 1 10) (part @Vague @Directory <$> points)
	pure $ foldr (<^^>) accumulator raw

generate_absolute :: Gen (Absolute Path To 'Directory)
generate_absolute = do
	accumulator <- part <$> points
	raw <- list (linear 1 10) (part @Vague @Directory <$> points)
	pure $ foldr (flip (</^>)) accumulator raw

concatenate_absolute_and_relative :: Property
concatenate_absolute_and_relative = property $ success

show_then_read_absolute :: Property
show_then_read_absolute = property $ do
	absolute <- forAll generate_absolute
	(read . show) absolute === absolute

show_then_read_relative :: Property
show_then_read_relative = property $ do
	relative <- forAll generate_relative
	(read . show) relative === relative
