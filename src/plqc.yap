/*************************************************************************
*
*   PrologCheck
*
*   PrologCheck was developed at NCCUP - Universidade do Porto
*
*   Copyright
*   C. Amaral, A. M. Florido, V.S.Costa, P. B. Vasconcelos
*   and Universidade do Porto 2012-2019
*
**************************************************************************
*
* File:     plqc.yap
* Last rev: 2019/04/04
* mods:
* comments: PrologCheck module main file;
*           File to be included/consulted in prolog projects for property
*           based testing
*
*************************************************************************/

/** @defgroup PrologCheck A library for pseudo-random testing of properties of logic prorgrams.

  An automatic tool for property based testing of Prolog programs with
  randomised test data generation.
  The tool is inspired by the well known QuickCheck, originally designed
  for the functional programming language Haskell.
  It includes features that deal with specific characteristics of Prolog
  such as its relational nature and the absence of a strong type discipline.

  PrologCheck expressiveness stems from describing properties as Prolog goals.
  It enables the definition of custom test data generators for random testing
  tailored for the property to be tested.
  Further, it allows the use of a predicate specification language that
  supports types, modes and constraints on the number of successful computations.

  @{

*/

:- module(plqc,
         [
         ]).


%% * @}
