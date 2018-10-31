module Main where

import "base" Data.Function (($))
import "base" System.IO (BufferMode(..), hSetBuffering, stdout, stderr)
import "hedgehog" Hedgehog (Group (..), checkParallel)

import Test.Monopati.Posix (show_then_read_absolute, show_then_read_relative)

main = do
	hSetBuffering stdout LineBuffering
	hSetBuffering stderr LineBuffering
	checkParallel $ Group "Simple Posix tests" [
		("Show then read absolute", show_then_read_absolute),
		("Show then read relative", show_then_read_relative)]
