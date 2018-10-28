module Main where

import "base" Data.Function (($))
import "base" System.IO (BufferMode(..), hSetBuffering, stdout, stderr)
import "hedgehog" Hedgehog (Group (..), checkParallel)

import Test.Monopati.Posix (concatenate_absolute_and_relative)

main = do
	hSetBuffering stdout LineBuffering
	hSetBuffering stderr LineBuffering
	checkParallel $ Group "Simple Posix tests" [
		( "concatenate_absolute_and_relative"
		, concatenate_absolute_and_relative)
		]
