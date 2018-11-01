module System.Monopati.Posix.Combinators
	( Absolute, Current, Homeward, Parental, Relative, Incompleted
	, deeper, part, parent
	, (<^>), (<.^>), (<~^>), (<..^>), (<^^>)
	, (</>), (</.>), (</~>), (</..>), (</^>)) where

import "base" Data.Eq (Eq ((/=)))
import "base" Data.Function ((.), ($), (&), flip)
import "base" Data.Functor ((<$>))
import "base" Data.Kind (Constraint, Type)
import "base" Data.List (filter)
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.String (String)
import "free" Control.Comonad.Cofree (Cofree ((:<)), unwrap)

import System.Monopati.Posix.Core (Points (..), Origin (..), To, Path, Outline (..))

type family Absolute (path :: Type) (to :: Type) (points :: Points) :: Type where
	Absolute Path To points = Outline Root points

type family Current (path :: Type) (to :: Type) (points :: Points) :: Type where
	Current Path To points = Outline Now points

type family Homeward (path :: Type) (to :: Type) (points :: Points) :: Type where
	Homeward Path To points = Outline Home points

type family Parental (path :: Type) (to :: Type) (points :: Points) :: Type where
	Parental Path To points = Outline Parent points

type family Relative (path :: Type) (to :: Type) (points :: Points) :: Type where
	Relative Path To points = Outline Vague points

type family Incompleted (outline :: Origin) :: Constraint where
	Incompleted Now = ()
	Incompleted Home = ()
	Incompleted Parent = ()
	Incompleted Vague = ()

-- | Immerse string into a path, filter slashes
part :: String -> Outline origin points
part x = Outline $ (filter (/= '/') x) :< Nothing

-- | Add relative path to uncompleted path
(<^>) :: forall origin points . Incompleted origin =>
	Outline origin Directory -> Relative Path To points -> Outline origin points
Outline (x :< Nothing) <^> Outline that = Outline $ x :< Just that
Outline (x :< Just this) <^> Outline that = (<^>) (part @origin x)
	$ (<^>) @Vague (Outline this) (Outline that)

{-| @
".//etc///" + "usr///local///" + = ".///etc///usr///local//"
@ -}
(<.^>) :: Current Path To Directory -> Relative Path To points -> Current Path To points
currently <.^> relative = currently <^> relative

{-| @
"~//etc///" + "usr///local///" + = "~///etc///usr///local//"
@ -}
(<~^>) :: Homeward Path To Directory -> Relative Path To points -> Homeward Path To points
homeward <~^> relative = homeward <^> relative

{-| @
"..//etc///" + "usr///local///" + = "..///etc///usr///local//"
@ -}
(<..^>) :: Parental Path To Directory -> Relative Path To points -> Parental Path To points
homeward <..^> relative = homeward <^> relative

{-| @
"etc//" + "usr///local///" + = "etc///usr///local//"
@ -}
(<^^>) :: Relative Path To Directory -> Relative Path To points -> Relative Path To points
relative' <^^> relative = relative' <^> relative

-- | Absolutize uncompleted path
(</>) :: forall origin points . Incompleted origin =>
	Absolute Path To Directory-> Outline origin points -> Absolute Path To points
Outline absolute </> Outline (x :< Nothing) = Outline . (:<) x . Just $ absolute
Outline absolute </> Outline (x :< Just xs) = (</>) @origin (Outline . (:<) x . Just $ absolute) $ Outline xs

{-| @
"//usr///local///" + ".///etc///" = "///usr///local///etc//"
@ -}
(</.>) :: Absolute Path To Directory -> Current Path To points -> Absolute Path To points
absolute </.> currently = absolute </> currently

{-| @
"//usr///local///" + "~///etc///" = "///usr///local///etc//"
@ -}
(</~>) :: Absolute Path To Directory -> Homeward Path To points -> Absolute Path To points
absolute </~> homeward = absolute </> homeward

{-| @
"//usr///local///" + "..///etc///" = "///usr///local///etc//"
@ -}
(</..>) :: Absolute Path To Directory -> Parental Path To points -> Absolute Path To points
absolute </..> homeward = absolute </> homeward

{-| @
"//usr///bin///" + "git" = "///usr///bin//git"
@ -}
(</^>) :: Absolute Path To Directory -> Relative Path To points -> Absolute Path To points
absolute </^> relative = absolute </> relative

-- | Take parent directory of current pointed entity
parent :: Absolute Path To points -> Maybe (Absolute Path To Directory)
parent = (<$>) Outline . unwrap . outline

-- | Take the next piece of relative path
deeper :: Relative Path To points -> Maybe (Relative Path To points)
deeper = (<$>) Outline . unwrap . outline
