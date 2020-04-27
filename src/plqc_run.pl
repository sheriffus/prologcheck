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
* Last rev: 2020/03/13
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
 * @date   Thu Mar 12 11:55 2020
 *
 * @brief  PrologCheck module single/pure test run;
 *
*/
:- module(plqc_run,
         [
           run_prop/3
         ]).

:- use_module(plqc_common).

:- use_module(context).
:- use_module('result.yap').

run_prop(Property, Context, OutContext) :-
  plqc_common:plqc_write([{very_verbose, 'PrologCheck property single test generation start (run_prop/3). '}
                        , {debug, [indent], 'Property to be tested: '}
                        , {debug, [indent, indent], Property}
                        , {very_verbose, [indent], 'Inbound context: '}
                        , {very_verbose, [indent, indent], Context}
                        , {debug, [indent], 'Outbound context (to be unified with): '}
                        , {debug, [indent, indent], OutContext}
                        ]),
  plqc_common:check_variable_W(OutContext, 'OutContext parameter in run_prop/3'),
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
  plqc_common:plqc_write([{very_verbose, 'PrologCheck single test run (run/3). Named context module.'}
                        , {debug, [indent], 'Property to be tested: '}
                        , {debug, [indent, indent], Module:Test}
                        , {very_verbose, [indent], 'Inbound context: '}
                        , {very_verbose, [indent, indent], Context}
                        , {debug, [indent], 'Outbound context (to be unified with): '}
                        , {debug, [indent, indent], OutContext}
                        ]),
  plqc_common:check_variable_W(OutContext, 'OutContext parameter in run/3'),
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
  plqc_common:plqc_write([{very_verbose, 'PrologCheck single test run (run/3). Binding quantified variables.'}
                        , {debug, [indent], 'Property to be tested: '}
                        , {debug, [indent, indent], for_all(Gen, Var, Test)}
                        , {very_verbose, [indent], 'Inbound context: '}
                        , {very_verbose, [indent, indent], Context}
                        , {debug, [indent], 'Outbound context (to be unified with): '}
                        , {debug, [indent, indent], OutContext}
                        ]),
  plqc_common:check_variable_W(OutContext, 'OutContext parameter in run/3'),
  !,
  plqc_common:plqc_write([{info, 'DEBUG result (run/3). '}
    , {info, [indent, indent], for_all(Gen, Var, Test)}
  ]),
  context:size(Context, Size),
  plqc_common:plqc_write([{info, 'DEBUG result (run/3). '}
    , {info, [indent, indent], for_all(Gen, Var, Test)}
  ]),
  context:bind_forall(Gen, Context, Var, Size, Context1),
  plqc_common:plqc_write([{info, 'DEBUG result (run/3). '}
    , {info, [indent, indent], for_all(Gen, Var, Test)}
  ]),
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
  plqc_common:plqc_write([{very_verbose, 'PrologCheck single test run (run/3). Labelled property.'}
                        , {debug, [indent], 'Property to be tested: '}
                        , {debug, [indent, indent], prop(Label)}
                        , {very_verbose, [indent], 'Inbound context: '}
                        , {very_verbose, [indent, indent], Context}
                        , {debug, [indent], 'Outbound context (to be unified with): '}
                        , {debug, [indent, indent], OutContext}
                        ]),
  plqc_common:check_variable_W(OutContext, 'OutContext parameter in run/3'),
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
  plqc_common:plqc_write([{very_verbose, 'PrologCheck single test run (run/3). Leaf test.'}
                        , {debug, [indent], 'Property to be tested: '}
                        , {debug, [indent, indent], LeafTest}
                        , {very_verbose, [indent], 'Inbound context: '}
                        , {very_verbose, [indent, indent], Context}
                        , {debug, [indent], 'Outbound context (to be unified with): '}
                        , {debug, [indent, indent], OutContext}
                        ]),
  plqc_common:check_variable_W(OutContext, 'OutContext parameter in run/3'),
  % writeln({run, 'DEBUG', leaf}),
  context:module(Context, M),
  (
    M:call(LeafTest), !,
    %% (call(M:Test), !,
    result:create_result(test_pass, {reason, true_goal}, Result)
  ;
    result:create_result(test_fail, {reason, false_goal}, Result)
  ),
  plqc_common:plqc_write([{very_verbose, ' - PrologCheck single test run (run/3). Leaf test. (cont.)'}
                        , {debug, [indent], 'Property to be tested: '}
                        , {debug, [indent, indent], M:LeafTest}
                        , {very_verbose, [indent], 'Result: '}
                        , {very_verbose, [indent, indent], Result}
                        , {debug, [indent], 'Outbound context: '}
                        , {debug, [indent, indent], OutContext}
                        ]),
  context:add_test_result(Context, Result, OutContext)
.

%% @}
