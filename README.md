# boston-clojure-group-talk

Code from my (virtual) talk for the Boston Clojure Group, 12 Feb 02015

* `boston-transcript.scm` contains code and commentary from the talk.  Please start here!

* `mk.scm` contains the implementation of miniKanren.

* `interp-with-variadic-lambda.scm` contains the relational Scheme interpreter.

* `test-check.scm` contains a simple test macro.

* `variadic-lambda-tests.scm` contains a number of tests for the
  relational interpreter, many of which are out of date or which
  otherwise no longer work.  Sigh.  These tests accreted over time,
  while I was experimenting with different versions of the relational
  interpreter.  As a result, many of the tests won't run correctly
  without modifying the interpreter.  This test file is intended
  mostly for inspiration, and as an example of the sorts of queries
  can be run against the relational interpreter.  Please experiment on
  your own!  You may want to comment out or reorder various language
  forms in the relational interpreter in order to speed up or bias the
  search.  'boston-transcript.scm' contains a number of queries which
  work well with this version of the relational interpeter.