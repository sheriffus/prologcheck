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
* File:     plqc_run_test.yap
* Last rev: 2019/05/15
* mods:
* comments: PrologCheck module single/pure test run;
*
*************************************************************************/

/** @defgroup PrologCheckRun Single testcase execution.
@ingroup PrologCheck
@{

PrologCheck internal module that deals with individual testcase execution.

A single testcase execution can be directly used for _testing_ purposes. It is
used under the hood of PrologCheck library.

*/

/**
 * @file   plqc_run.yap
 * @author Claudio Amaral <coa@dcc.fc.up.pt>
 * @date   Mon May 15 16:15 2019
 *
 * @brief  PrologCheck module single/pure test run;
 *
*/
:- module(plqc_run,
         [
           run_prop/3
         ]).

:- reconsult(context).
:- reconsult(result).
% :- reconsult(plqc_common). TODO

run_prop(Property, Context, OutContext) :-
  duplicate_term(Property, Test),
  run(Test, Context, OutContext)
.

/** @pred run(+ _Test_, + _Context_, ? _OutContext_)

Predicate that runs a property, _Test_, according to a property execution
context, _Context_.

The result of the property is added to the given context in the _OutContext_.

*/

/** @pred run(+ _Module_:_Test_, + _Context_, ? _OutContext_)

If properties or goals to be executed in the scope of the test are defined in
other modules, the executing property represented by _Test_ is recursively ran
with the _Module_ as its calling module context.
*/
run(Module:Test, Context, OutContext) :-
  !,
  context:new_module(Context, Module, Context1),
  run(Test, Context1, OutContext)
.
/** @pred run(+ for_all(_Gen_, _Var_, _Test_), + _Context_, ? _OutContext_)

Run a test with universal quantification of a parameter.

Recursively run _Test_ [potentially] with occurring unbound variable _Var_ by binding a value
from the domain of the generator _Gen_ to _Var_.
*/
run(for_all(Gen, Var, Test), Context, OutContext) :-
  !,
  context:size(Context, Size),
  context:bind_forall(Gen, Context, Var, Size, Context1),
  run(Test, Context1, OutContext)
.
/** @pred run(+ prop(_Label_), + _Context_, ? _OutContext_)

If a property being tested is a labelled property, it needs to be unfolded
(labelled property clause body) in the context module and recursively addressed.

'Unfolding' requires compilation with 'source'.
_Label_ is either the identifier (atom that identifies the property) or
a tuple with the identifier and the arguments.
*/
run(prop(Label), Context, OutContext) :-
  !,
  context:module(Context, Module),
  clause(Module:prop(Label), Body),
  run(Body, Context, OutContext)
.
/** @pred run(+ _LeafTest_, + _Context_, ? _OutContext_)

For _leaf_ properties, the goal represented by _LeafTest_ is _called_ on the
module specified by _Context_.
*/
run(LeafTest, Context, OutContext) :-
  % writeln({run, 'DEBUG', leaf}),
  context:module(Context, M),
  (
    M:call(LeafTest), !,
    %% (call(M:Test), !,
    result:create_result(test_pass, {reason, true_goal}, Result)
  ;
    result:create_result(test_fail, {reason, false_goal}, Result)
  ),
  context:add_test_result(Context, Result, OutContext)
.

%% @}
