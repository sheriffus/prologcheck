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
           run/3
         ]).

:- reconsult(context).
:- reconsult(result).

%% a leaf in the property syntax tree - a predicate call

/**
@pred run(+ _Test_, + _Context_, ? _OutContext_)

Predicate that runs a property, _Test_, according to a property execution
context, _Context_.

The result of the property is added to the given context in the _OutContext_.

*/

/**
@pred run(+ prop(_Label_), + _Context_, ? _OutContext_)

If a property being tested is a labelled property, it needs to be unfolded
(labelled property clause body) in the context module and recursively addressed.

'Unfolding' requires compilation with 'source'.
_Label_ is either the identifier (atom that identifies the property) or
a tuple with the identifier and the arguments.
*/
run(prop(Label), Context, OutContext) :-
  !, context:module(Context, Module),
  clause(Module:prop(Label), Body),
  run(Body, Context, OutContext).
/**
@pred run(+ _Module_:_Test_, + _Context_, ? _OutContext_)

If properties or goals to be executed in the scope of the test are defined in
other modules, the executing property represented by _Test_ is recursively ran
with the _Module_ as its calling module context.
*/
run(Module:Test, Context, OutContext) :-
  !, context:new_module(Context, Module, Context1),
  run(Test, Context1, OutContext).
/**
@pred run(+ _LeafTest_, + _Context_, ? _OutContext_)

For _leaf_ properties, the goal represented by _LeafTest_ is _called_ on the
module specified by _Context_.
*/
run(LeafTest, Context, OutContext) :-
  context:module(Context, M),
  (
    M:call(LeafTest), !,
    %% (call(M:Test), !,
    result:create_result(pass, {reason, true_goal}, Result)
  ;
    result:create_result(fail, {reason, false_goal}, Result)
  ),
  context:add_result(Context, Result, OutContext)
.


%% @}
