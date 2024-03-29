# TLT

TLT is a Haskell test framework oriented towards stacked monad
transformers.  TLT has no explicit, static test specifications.  Tests
are run where declared, with results accumulated and reported at the
end.  Tests can run on a stack of arbitrary monad transformers so long
as the `TLT` transformer is included (currently, at the top).  Some
control of the results display is available.

See also the TLT Haddock page for additional examples (linked from the
`Test/TLT` module on 
[TLT's Hackage entry](https://hackage.haskell.org/package/TLT)).

# Overview

A TLT test is a command in the `TLT` monad transformer.  There is no
separation between the specification and execution of a test; TLT
makes no record of an executable test itself, only of its result.  So
in the main instance for testing, the core `IO` monad, and whatever
other layers are also to be tested, should be wrapped in the `TLT`
transformer.

In TLT, all tests are associated with a string which names or
otherwise describes the test.  Each test is introduced with one of the
`~:`, `~::`, or `~::-` infix operators.

The simplest tests simply look for a `True` boolean value.  These
tests are introduced with `~::` or `~::-`.  The difference between the
two is whether the boolean value is the result of a pure `Bool`
expression, or whether it is returned as the result of a computation.
In TLT, we distinguish between the two cases by including a trailing
hyphen `-` to operators on pure expressions, and omitting the hyphen
from operators on monadic arguments.  So these two tests will both
pass,

    "2 is 2 as single Bool" ~::- 2 == 2
    "2 is 2 a returned Bool" ~:: return $ 2 == 2

The `~:` operator introduces a more general form of test.  The
right-hand side of `~:` should be an `Assertion` formed with one of
TLT's built-in assertion operators, or returned from a package's
custom assertions.  `Assertion`s can give more detailed failure
information then simple `Bool`s.

Syntactically, most assertions are infix operators which start with a
`@` character.  The value to the left of the operator is the expected
value, and the symbol to the right is (or returns) the value under
test.  A hyphen or `P` suffixes assertion operators which operate on
pure values; for operators without the trailing hyphen, the value
under test should is expected to be returned as the result of a
monadic computation (as with `~::` and `~::-`).

TLT provides these assertion operators:

| Operator                       | Meaning                               |
| ------------------------------ | ------------------------------------- |
| `expected @== monadic`     | The actual result must be equal to the given expected result.       |
| `expected @==- expr`       |                                       |
| `unexpected @/= monadic`  | The actual result must differ from the given unexpected result.        |
| `unexpected @/=- expr`    |                                       |
| `expected @< monadic`      | The actual result must be greater than the given lower bound.  |
| `expected @<- expr`        |                                       |
| `expected @    monadic`    | The actual result must be less than the given upper bound.        |
| `expected @>- expr`        |                                       |
| `expected @<= monadic`     | The actual result must be greater than or equal to the given lower bound.     |
| `expected @<=- expr`       |                                 |
| `expected @>= monadic`     | The actual result must be less than or equal to the given upper bound.   |
| `expected @>=- expr`       |                                       |
| `empty monadic`              | The actual result must be an empty `Traversable` structure.    |
| `emptyP expr`                |                                       |
| `nonempty monadic`           | The actual result must be a nonempty `Traversable` structure.  |
| `nonemptyP expr`             |                                       |
| `nothing monadic`            | The actual result must be `Nothing` (in a `Maybe`-typed value)   |
| `nothingP expr`              |                                       |
| `assertFailed message`       | Trivial assertions, intended for the less interesting branches of conditional and selection expressions.  |
| `assertSuccess`                | |

Note that although the assertions are in pairs of one for testing a
pure expression value, and one for testing the result returned from a
monadic computation, in all of the builtin binary assertions the
/expected/ value argument is always a pure value, not itself monadic.

The `inGroup` function allows related tests to be reported as a group.
The function takes two arguments, a `String` name for the group, and
the `TLT` computation housing its tests.  Groups have impact only in
terms of organizing the output you see in the final report of tests
run.

It is straightforward to write new `Assertion`s for project-specific
test criteria: they are simply functions returning monadic values.
There are several functions in the final section of this document
which transform pure predicates into `Assertion`s, or which transform
one form of `Assertion` into another.

There are also special forms for validating the normal termination of
computations including an `ExceptT` monad transformer layer:

 - `noUncaught` and `noUncaught_` each take an `ExceptT` computation
   (which may itself contain additional TLT tests) as an argument, and
   expects that computation to finish without finding an uncaught
   exception thrown from it.  The `noUncaught` function requires the
   exception type to be an instance of `Show`; the `noUncaught_`
   function operates on any exception.

 - `uncaught` also takes an `ExceptT` computation as an argument, but
   does expects it to throw an uncaught exception.  This computation
   may contain additional TLT tests, but it should be noted that any
   tests which would have been executed after a `throwE` will either
   be executed, nor be recorded for test result reporting.

 - `uncaughtWith` is like `uncaught`, but takes an additional function
   argument which receives the thrown exception for additional
   examination.
   
These three functions generally require a declaration of an instance
of `MonadTLTExcept` for the monadic type under test.  This class
decribes the relationship among the overall monad stack, the `ExceptT`
layer with it, and the TLT transformer within the `ExceptT` layer.
With these inclusion requirements, these functions do require a more
specific monad stack structure than the rest of TLT.

# Examples

These examples are from the sample executables and test suite of
the `TLT` package.

## A simple example

The tests in this example are vacuous, but they show a simple
setup with both passing and failing tests.

    main :: IO ()
    main = do
      tlt test

    test :: Monad m =    TLT m ()
    test = do
      "True passes" ~::- True
      "2 is 3 as single Bool" ~::- 2 == 3
      "2 is 2 as single Bool" ~::- 2 == 2
      inGroup "== assertions" $ do
        inGroup "pure" $ do
          "2 is 3 as pure assertion" ~: 2 @==- 3
          "2 is 2 as pure assertion" ~: 2 @==- 2
        inGroup "monadic" $ do
          "2 is 3 as result" ~: 2 @== return 3
          "2 is 2 as result" ~: 2 @== return 2
      inGroup "/= pure assertions" $ do
        "2 not 3" ~: 2 @/=- 3
        "2 not 2" ~: 2 @/=- 2
      "2 not 3 as result" ~: 2 @/= return 3
      "2 not 2 as result" ~: 2 @/= return 2

Running these tests should give:

    Running tests:
    - 2 is 3 as single Bool: FAIL Expected True but got False
    - == assertions:
      - pure:
        - 2 is 3 as pure assertion: FAIL Expected 2 but got 3
      - monadic:
        - 2 is 3 as result: FAIL Expected 2 but got 3
    - /= pure assertions:
      - 2 not 2: FAIL Expected other than 2 but got 2
    - 2 not 2 as result: FAIL Expected other than 2 but got 2
    Found 5 errors in 11 tests; exiting

Note that only failing tests appear.  This can be configured in the
`test` command: add a call at the beginning of `test` to
`reportAllTestResults` to control this behavior:

    test :: Monad m =    TLT m ()
    test = do
      reportAllTestResults True
      "True passes" ~::- True
      ...

and the output will be

    Running tests:
    - True passes: Pass
    - 2 is 3 as single Bool: FAIL Expected True but got False
    - 2 is 2 as single Bool: Pass
    - == assertions:
      - pure:
        - 2 is 3 as pure assertion: FAIL Expected 2 but got 3
        - 2 is 2 as pure assertion: Pass
      - monadic:
        - 2 is 3 as result: FAIL Expected 2 but got 3
        - 2 is 2 as result: Pass
    - /= pure assertions:
      - 2 not 3: Pass
      - 2 not 2: FAIL Expected other than 2 but got 2
    - 2 not 3 as result: Pass
    - 2 not 2 as result: FAIL Expected other than 2 but got 2
    Found 5 errors in 11 tests; exiting

## Testing with exceptions --- TO UPDATE

These tests will pass:

    do
      noUncaught "extest1" $ do
        "6 is 6 as pure assertion" ~: 6 @==- 6
        "7 is 7 as pure assertion" ~: 7 @==- 7
      uncaught "extest2" $ do
        "8 is 8 as pure assertion" ~: 8 @==- 8
        throwE "Boom"
        "9 is 9 as pure assertion" ~: 9 @==- 9
      uncaughtWith "extest3"
	    (do "10 is 10 as pure assertion" ~: 10 @==- 10
            throwE "Boom"
            11 is 11 as pure assertion" ~: 11 @==- 11) h
        (\e -> "The exception should be \"Boom\""
                 ~: "Boom" @==- e)

This test will fail because it expects no thrown exceptions, but does
observe one:

    noUncaught "extest1x" $ do
      "6 is 6 as pure assertion" ~: 6 @==- 6
      throwE "Boom"
      "7 is 7 as pure assertion" ~: 7 @==- 7

This test will fail because it expects a thrown exception, but the
subcomputation ends normally:

    uncaught "extest2x" $ do
      "8 is 8 as pure assertion" ~: 8 @==- 8
      "9 is 9 as pure assertion" ~: 9 @==- 9

Finally, this test will record an error 
against `"The exception should be \"Boom\""`,
although no error will be logged against `"extest3x"`
since only the absence of an exception would be logged at that point.

    uncaughtWith "extest3x"
	  (do throwE "Bang"
          "10 is 10 as pure assertion" ~: 10 @==- 10
          "11 is 11 as pure assertion" ~: 11 @==- 11)
	  (\e -> lift ("The exception should be \"Boom\"" ~: "Boom" @==- e)
