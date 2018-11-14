module Test.Monopati.Posix
	( show_then_read_absolute
	, show_then_read_current
	, show_then_read_homeward
	, show_then_read_relative
	, generate_parental_current
	, generate_parental_homeward
	, generate_parental_relative
	) where

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

import System.Monopati.Posix
	( Absolute, Current, Homeward, Relative, Parental, Parent (Parent)
	, Path, Dummy (To, For), Origin (Root, Vague), Points (Directory, File)
	, part, (</^>), (<.^>), (<~^>), (<^^>))

points :: Gen String
points = string (linear 0 100) (enum 'a' 'z')

generate_absolute :: Gen (Absolute Path To 'Directory)
generate_absolute = do
	accumulator <- part <$> points
	raw <- list (linear 1 10) (part @Vague @Directory <$> points)
	pure $ foldr (flip (</^>)) accumulator raw

generate_current :: Gen (Current Path To 'Directory)
generate_current = do
	accumulator <- part <$> points
	raw <- list (linear 1 10) (part @Vague @Directory <$> points)
	pure $ foldr (flip (<.^>)) accumulator raw

generate_homeward :: Gen (Homeward Path To 'Directory)
generate_homeward = do
	accumulator <- part <$> points
	raw <- list (linear 1 10) (part @Vague @Directory <$> points)
	pure $ foldr (flip (<~^>)) accumulator raw

generate_relative :: Gen (Relative Path To 'Directory)
generate_relative = do
	accumulator <- part <$> points
	raw <- list (linear 1 10) (part @Vague @Directory <$> points)
	pure $ foldr (<^^>) accumulator raw

generate_parental_current :: Gen (Parental For (Current Path To 'Directory))
generate_parental_current = Parent <$> enum 1 10 <*> generate_current

generate_parental_homeward :: Gen (Parental For (Homeward Path To 'Directory))
generate_parental_homeward = Parent <$> enum 1 10 <*> generate_homeward

generate_parental_relative :: Gen (Parental For (Relative Path To 'Directory))
generate_parental_relative = Parent <$> enum 1 10 <*> generate_relative

show_then_read_absolute :: Property
show_then_read_absolute = property $ do
	absolute <- forAll generate_absolute
	(read . show) absolute === absolute

show_then_read_current :: Property
show_then_read_current = property $ do
	current <- forAll generate_current
	(read . show) current === current

show_then_read_homeward :: Property
show_then_read_homeward = property $ do
	homeward <- forAll generate_homeward
	(read . show) homeward === homeward

show_then_read_relative :: Property
show_then_read_relative = property $ do
	relative <- forAll generate_relative
	(read . show) relative === relative
