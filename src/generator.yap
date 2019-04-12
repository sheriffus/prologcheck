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
* File:     generator.yap
* Last rev: 2019/04/04
* mods:
* comments: PrologCheck built-in definitions of generator and
*           matching shrinker generator combinators for PrologCheck
*
*************************************************************************/


/** @defgroup Generator Pseudo-random Value Generator Predicates and Combinators
@{
@ingroup PrologCheck

Input for testing properties is randomly generated through explicitly defined
procedures: generators.
There are differences between PrologCheck generators and the generators in a
strongly typed version of the tool.
In Haskell QuickCheck, or any language with strong types, generators pick
values inside a preexisting type according to some criteria.
In PrologCheck generators represent procedures that randomly construct elements
according to the shape of the term.
In fact, the generators themselves define a set by the elements they generate,
with nonzero probability.
Thus, they define a set of terms, here denoted as a type.
Note that this set of terms is not necessarily composed of only ground terms,
instead it exactly represents the form of an input parameter to a property.

*/






%% @}
