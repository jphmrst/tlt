# Changelog for TLT

- 0.5.0 :: Add a variation of @noUncaught@ to disguish @Show@ and
  non-@Show@ exception types in TLT failure reporting.
  
  Various improvement to the documentation.

- 0.4.0 :: Adding a class, instance declarations, and functions for
  inspecting exceptions thrown from an `ExceptT` transformer layer.
  
  Corrections to the Haddock documentation.

- 0.3.0 :: Significant refactoring, organizing the contents of the
  module @Test.TLT@ into submodules, with @Test.TLT@ only re-exporting
  the core functionality.

- 0.2.0 :: Divided the `tlt` function for running tests, to separate
  `tltCore` for just running tests from formatted output of test
  results.  The former is intended for running TLT in other
  frameworks.  Also prunes some unneeded dependencies.

- 0.1.0 :: First release.  Patch 1 re-arranged documentation.

## Unreleased changes

None in this version.
