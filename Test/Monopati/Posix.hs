module Test.Monopati.Posix (concatenate_absolute_and_relative) where

import "base" Control.Applicative ((<*>))
import "base" Control.Monad ((>>=))
import "base" Data.Bool (Bool (True))
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Data.String (String)
import "base" System.IO (print)
import "hedgehog" Hedgehog (Gen, Property, assert, forAll, property)
import "hedgehog" Hedgehog.Gen (string, enum)
import "hedgehog" Hedgehog.Range (linear)
import "transformers" Control.Monad.Trans.Class (lift)

import System.Monopati.Posix (Absolute, Relative, Path, To, Points (Directory, File), part, (</>), (<^>))

points :: Gen String
points = string (linear 0 100) (enum 'a' 'z')

relative :: Gen (Relative Path To 'File)
relative = (<^>) <$> (part <$> points) <*> (part <$> points)

absolute :: Gen (Absolute Path To 'Directory)
absolute = (</>) <$> (part <$> points) <*> (part <$> points)

concatenate_absolute_and_relative :: Property
concatenate_absolute_and_relative = property $ do
	(</>) <$> (forAll absolute) <*> (forAll relative)
	assert True
