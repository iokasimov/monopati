# Well-typed paths: revisited

Despite the fact that there are several “path” libraries in Haskell, I decided to write a new one I would like to use.

Problem description
--------------------------------------------------------------------------------
Often (when you write a useful program) you need to do something to the filesystem. Using temporary files, reading directory contents, writing logs - in all of these cases you need to clarify the path. But path can be specified either in absolute or relative form. And it can point either to a directory or a file. Okay, let's encode these cases in types.

```haskell
{-# language DataKinds, KindSignatures #-}

data Reference = Absolute | Relative

data Points = Directory | File

type Stack = Cofree Maybe

data Path (reference :: Reference) (points :: Points) = Path { path :: Stack String }
```

We use stack as a core data structure for path - it will become clear later why it was chosen. Now we need some rules that can help us build valid paths depending on theirs types. So, we can do this:

```haskell
Path Relative Directory + Path Relative Directory = Path Relative Directory
"usr/local/" + "etc/" = "usr/local/etc/"
Path Relative Directory + Path Relative File = Path Relative Directory
"bin/" <> "git" = "bin/git"
Path Absolute Directory + Path Relative Directory = Path Absolute Directory
"/usr/local/" <> "etc/" = "/usr/local/etc/" =
Path Absolute Directory + Path Relative File = Path Absolute File
"/usr/bin/" <> "git" = "/usr/bin/git"
```

But we can't do this:

```haskell
Path _ File + Path _ File = ???
Path _ File + Path _ Directory = ???
Path Absolute _ + Path Absolute _ = ???
Path Relative _ + Path Absolute _ = ???
```

Based on these rules we can define two main combinators.

```haskell
(<^>) :: Path Relative Directory -> Path Relative points -> Path Relative points
(</>) :: Path Absolute Directory -> Path Relative points -> Path Absolute points
```

Get our hands dirty
--------------------------------------------------------------------------------

There are some functions in [`System.Monopati.Posix`](https://github.com/iokasimov/monopati/blob/master/System/Monopati/Posix.hs) that work with our Path definition:

* As you may remember, we use Stack - it's an not empty inductive data structure. When we are in a root, we have no directory or file to point in - we are in the starting point, so `current` returns `Nothing`. We return absolute path because we actually want to know where we are exactly in the filesystem:
```haskell
current :: IO (Maybe (Path Absolute Directory))
```
* Sometimes we want to change our current working directory for some reason. As documentation of [`System.Directory`](http://hackage.haskell.org/package/directory-1.3.3.1/docs/System-Directory.html#v:setCurrentDirectory) says: it's highly recommended to use absolute rather than relative paths cause of current working directory is a global state shared among all threads:
```haskell
change :: Path Absolute Directory -> IO (Path Absolute Directory)
```

* Often I need to create some folder and get an absolute path of it:
```haskell
create :: Path Relative Directory -> IO (Path Absolute Directory)
```

Simple example of usage
--------------------------------------------------------------------------------

Let's imagine that we need to save some content in temporary files grouped on folders based on some prefix.

```haskell
mkdir :: String -> IO (Path Absolute Directory)
mkdir prefix = create $ part "Temporary" <^> part prefix

filepath :: String -> String -> IO (Path Absolute File)
filepath filename prefix = (\dir -> dir </> part filename) <$> mkdir prefix
```

In this example, `part` function is like a `pure` for `Path` but it takes strings only. We create a directory and then construct a full path to the file.

Motivation of using this library
--------------------------------------------------------------------------------

Well, it's easier for me to define what exactly I don't like in another "path"-libraries:

* [`filepath`](https://hackage.haskell.org/package/filepath) - The most popular, but using raw strings
* [`path`](https://hackage.haskell.org/package/path) - TemplateHaskell (I really hate it), using raw strings in internals
* [`posix-paths`](https://github.com/JohnLato/posix-paths) - Focusing on performance instead of usage simplicity
