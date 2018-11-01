# 0.1.1
* Remove `System.Monopati` module
* Rewrite core of the library
* Remove functions that do system calls

# 0.1.2
* Split platform-depends modules on Calls and Combinators
* Add `current`, `home`, `create`, `change` and `remove` calls
* Add `parent` and `deeper` combinators
* Add `Read` instances for paths

# 0.1.3
* Add property tests for checking Show and Read instances
* Rename combinator `<~/>` to `</~>` - absolutizing of homeward path
* Generalize `<^>` and `</>` with `Incompleted` type family
* Add new type of path - `Currently`, depends on current working directory

# 0.1.4
* Add new type of path - `Parental` (`../`)
* Split `Combinators` module on `Core` and `Combinators`
