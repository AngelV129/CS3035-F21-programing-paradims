{- Modules -}

{-
Outline:
1. Introduction of modules
2. How to load modules
3. Review of a few useful modules in the standard library
4. How to make/use our own modules
-}




{- 1. Introduction of modules -}

{-
A Haskell MODULE is a collection of related functions, types
and typeclasses.

A Haskell PROGRAM is a collection of modules where the MAIN
module loads up the other modules and then uses the functions
defined in them to do something.

Advantages of modules:
-- separation of concerns
-- reuse
-- structure code (in different files)
-- etc.

The Haskell STANDARD LIBRARY is split into modules.

All the functions, types and typeclasses, which we've
dealt with so far, are part of the "Prelude" module.
The "Prelude" module is imported/loaded by default.

USEFUL LINKS:
-- explore modules in the standard library
    https://downloads.haskell.org/~ghc/latest/docs/html/libraries/
-- Hoogle: Haskell API search engine
    https://hoogle.haskell.org/
-}




{- 2. How to load modules -}

{-
(1) Load modules in GHCI
":m + Data.List Data.Map Data.Set"

(2) Load modules in Haskell script (.hs file)

The "import" must be done before defining any functions,
so imports are usually done at the top of the file.
One script can, of course, import several modules.
Just put each import statement into a separate line.

-- "import <module name>"
    import the ENTIRE module (ALL functions)
    e.g. "import Data.List"

-- "import <module name> (f1, f2, ..., fn)"
    ONLY import functions f1, f2, ..., fn from a module
    e.g. "import Data.List (nub, sort)"

-- "import <module name> hiding (f1, f2, ..., fn)"
    import all other functions from a module EXCEPT f1, f2, ..., fn
    e.g. "import Data.List hiding (nub)"

    AVOID function name conflicts when several imported modules define
    functions with the same name.    

-- "import qualified <module name>"
    QUALIFIED import to handle name conflicts
    e.g. "import qualified Data.Map"    "import qualified Data.List"

    All three modules "Prelude", "Data.Map" and "Data.List"
    define a bunch of functions with the same name, such as
    "null", "map", "filter", "foldl", "foldr" etc.

    With qualified imports, we can EXPLICITLY a function from a SPECIFIC module.
    "Prelude.filter f xs" -- call the "filter" function defined in "Prelude" module
    "filter f xs" -- call the "filter" function defined in "Prelude" module (default)
    "Data.Map.filter f xs" -- call the "filter" function defined in "Data.Map" module
    "Data.List.filter f xs" -- call the "filter" function defined in "Data.List" module

    Note that the function with same name in different modules may have DIFFERENT TYPE.
    Prelude.filter :: (a -> Bool) -> [a] -> [a]
    Data.List.filter :: (a -> Bool) -> [a] -> [a]
    Data.Map.filter :: (a -> Bool) -> Map k a -> Map k a

-- "import qualified <module name> as <alias>"
    QUALIFIED import with module ALIAS
    e.g. "import qualified Data.Map as M"    "import qualified Data.List as L"

    "filter f xs": call the "filter" function defined in "Prelude" module (default)
    "M.filter f xs": call the "filter" function defined in "Data.Map" module
    "L.filter f xs": call the "filter" function defined in "Data.List" module
-}

{-
Example:
create a function to return the number of UNIQUE elements in the given list

"Data.List.nub" -- remove duplicate elements of the given list
-}
import Data.List  
  
numUniques :: (Eq a) => [a] -> Int  
numUniques xs = length (nub xs)
{-
numUniques [1,2,3,4,2,4,3,5] == 5
numUniques "Hello World" == 8
-}




{- 3. Review of a few useful modules in the standard library -}

{-
(1) "Data.List": module dealing with LIST data structure
https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.14.0.0/Data-List.html

(2) "Data.Char": module dealing with CHARACTERs
https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.14.0.0/Data-Char.html
-- It's also helpful when filtering and mapping over strings because they're just lists of characters.

(3) "Data.Map": module dealing with MAP/DICTIONARY data structure (key-value pairs)
https://downloads.haskell.org/~ghc/latest/docs/html/libraries/containers-0.6.2.1/Data-Map-Internal.html

(4) "Data.Set": module dealing with SET data structure (all elements are unique)
https://downloads.haskell.org/~ghc/latest/docs/html/libraries/containers-0.6.2.1/Data-Set.html




You do NOT need to remember all functions of every module in Haskell standard library.
When you need a function or a module, just go to the documentation (https://downloads.haskell.org/~ghc/latest/docs/html/libraries/)
or Hoogle (https://hoogle.haskell.org/) or Google.

Chapter 7: Modules of the textbook (http://learnyouahaskell.com/modules)
list/introduce several functions in the above four modules.

You will learn more about the four modules with Thursday's lab/quiz.
-}




{- 4. How to make/use our own modules -}

{-
Example: 
Make a module to provide some functions for calculating
the volume and area of a few geometric objects.

Two implementation styles:
(1) one module: "Geometry.hs" file (first character file name is CAPITALIZED)
-- testing file: TestGeometryOneModule.hs (same folder with Geometry.hs)
-- see "GeometryOneModule.zip" file

(2) multiple modules: "Geometry" folder (first character folder name is CAPITALIZED, )
-- "Geometry" folder contains three module files: Sphere.hs, Cuboid.hs, and Cube.hs (first character file name is CAPITALIZED)
-- testing file: TestGeometryMulModules.hs (at the same level with "Geometry" folder)
-- see "GeometryMulModules.zip" file
-}