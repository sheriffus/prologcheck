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
* Last rev: 2019/05/15
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

/**
 * @file   plqc.yap
 * @author Claudio Amaral <coa@dcc.fc.up.pt>
 * @date   Mon May 15 15:15 2019
 *
 * @brief  PrologCheck module main file
 *
*/
:- module(plqc,
         [ prologcheck/1
         , prologcheck/2
         , prologcheck_result/2
         , prologcheck_result/3
         ])
.

:- reconsult(plqc_run).

/** @pred prologcheck(+ _Property_)

Tests the property _Property_ and prints the results to 'stdout'.

Uses prologcheck/2 with an empty list of user options.
*/
prologcheck(Property) :-
  prologcheck(Property, []).

/** @pred prologcheck(+ _Property_, +_UserOptions_)

Tests the property _Property_, changing the pre-defined behaviour according to
the user options provided in the list of option _UserOptions_, and prints the
results to 'stdout'.

Uses prologcheck_result/3, passing _Property_ and _UserOptions_ and discarding
the result.
*/
prologcheck(Property, UserOpts)  :-
  prologcheck_result(Property, UserOpts, _Result).

/** @pred prologcheck_result(+ _Property_, - _Result_)

Tests the property _Property_ and produces a test result _Result_, which it also
prints 'stdout'.

Uses  prologcheck_result/3 with an empty list of user options.
*/
prologcheck_result(Property, Result) :-
  prologcheck_result(Property, [], Result).

/** @pred prologcheck_result(+ _Property_, +_UserOptions_, - _Result_)

Tests the property _Property_ and produces a test result _Result_, changing the
pre-defined behaviour according to the user options provided in the list of
option _UserOptions_, and processes the results (default is to print to 'stdout').

Uses prologcheck/2 with an empty list of user options.
Tests a property, produces a test result, and prints the results to 'stdout'.
Tests the property _Property_, changing the pre-defined behaviour according to
the user options provided in the list of option _UserOptions_, and prints the
results to 'stdout'.

'Contextualises' the [potentially empty] list of user options given and calls
the test suite execution start predicate with context start_test_suite/4.
It presents the result according to the directives in resulting context.
*/
prologcheck_result(Property, UserOpts, Result) :-
  context:parse(UserOpts, Context),
  start_test_suite(Property, Context, OutContext, Result),
  process_result(OutContext, Result)
.


/** @pred start_test_suite(+ _Property_, +_Context_, -_OutContext_, - _Result_)

Tests the property _Property_ based on a context _Context_ and produces a test
result _Result_ together with the resulting context _OutContext_.

*/
start_test_suite(Property, Context, OutContext, Result) :-
  % writeln({start_test_suite, property, Property}),
  % writeln({start_test_suite, context, Context}),
  %% retrieve specific attributes for testing behaviour
  context:num_tests(Context,  NumTests),
  context:tries_calc_predicate(Context, TriesCalcPredicate),
  call(TriesCalcPredicate, NumTests, NumTries),
  % writeln({start_test_suite, num_tries, NumTries}),
  %% perform tests on Property
  run_test_suite({0,0}, {NumTests, NumTries}, Property, Context, OutContext, Result),
  true
.
  %% report result
%   call(Print,"~n", []),
%   report_result(Result1, Opts),
%   %% polish gross result into long/short testing result (possible shrinking)
%   refine_result(Result1, Property, Opts, ShortRes, LongRes),
%   (ReturnLong == true, !,
%   Result = LongRes
% ;
%   Result = ShortRes
%   )

/** @pred run_test_suite_(+ _TestAttempt_, + _Limit_, + _Property_, +_Context_, -_OutContext_, - _Result_)

Runs a test suite on _Property_.

The test suite is mainly characterised by the limits imposed in the _Limit_ pair,
where the first element states the number of successfull tests requested and the
second element states the maximum number of attempts to make. The limits are
checked against the test number pair, _TestAttempt_, with the test case number
in the first element and the total number of attempts in the second. Other defined
behaviours on how to run the test suite are stated in the accumulator context
_Context_.

*/
%run_test_suite({TestNumber, TryNumber}, {NumTests, NumTries}, Property, Context, _OutContext, Result).

/** @pred run_test_suite_({+ _TestNumber_, + _TryNumber_}, {+ _NumTests_, + _NumTries_}, + _Property_, +_Context_, -_OutContext_, - _Result_)

When the test case number _TestNumber_ (number of performed tests) matches the
number of tests limit required _NumTests_, result and outbound context are
compiled from context.
*/
run_test_suite({NumTests, TryNumber}, {NumTests, _NumTries}, _Property, Context, OutContext, Result) :-
  writeln({run_test_suite, suite_complete, {NumTests, TryNumber}}),
  !,
  contextualise_result(suite_complete, {NumTests, TryNumber}, Context, OutContext, Result)
.
/** @pred run_test_suite_({+ _TestNumber_, + _TryNumber_}, {+ _NumTests_, + _NumTries_}, + _Property_, +_Context_, -_OutContext_, - _Result_)

When the number of attempts _TryNumber_ matches the number of tries limit
required _NumTries_, result and outbound context are compiled from context.
*/
run_test_suite({TestNumber, NumTries}, {_NumTests, NumTries}, _Property, Context, _OutContext, Result) :-
  writeln({run_test_suite, suite_try_limit, {TestNumber, NumTries}}),
  !,
  contextualise_result(suite_try_limit, {TestNumber, NumTries}, Context, OutContext, Result)
.
run_test_suite({TestNumber, TryNumber}, {NumTests, NumTries}, Property, Context, OutContext, Result) :-
  writeln({run_test_suite, testcase, {TestNumber, TryNumber}}),
  writeln({run_test_suite, testcase_context, Context}),
  plqc_run:run_prop(Property, Context, Context1),
  writeln({run_test_suite, testcase_context1, Context1}),
  context:pop_test_result(Context1, TestResult, Context2),
  writeln({run_test_suite, testcase_context2, Context2}),
  context:reset_bindings_trace(Context2, Trace, ResetContext0),
  context:increment_size(ResetContext0, ResetContext),
  writeln({run_test_suite, testcase_context4, ResetContext}),
  test_suite_step({TestNumber, TryNumber}, TestResult, {TestNumber1, TryNumber1}),
  check_for_suite_result(TestResult, ResetContext, StepResult),
  writeln({run_test_suite, testcase_step, StepResult}),
  writeln({run_test_suite, testcase_result1, Result}),
  (
    writeln({run_test_suite, testcase_result2, Result}),
    StepResult == continue, !,
    writeln({run_test_suite, testcase_result3, Result}),
    run_test_suite({TestNumber1, TryNumber1}, {NumTests, NumTries}, Property, ResetContext, OutContext, Result)
  ;
    StepResult == fail, !,
    contextualise_result(suite_halted, {TestNumber1, TryNumber1}, Trace, ResetContext, OutContext, Result)
  )
  , nl, writeln({run_test_suite, testcase_RESULT, Result}), nl
.

%run_test_suite({TestNumber, TryNumber}, {NumTests, NumTries}, Property, Context, _OutContext, Result).
% perform(Passed, ToPass, TriesLeft, Property, Opts, IState, OState, Result) :-
%         duplicate_term(Property, Test),
%         run(Test, Opts, IState, State1, Res),


contextualise_result(SuiteStatus, {TestNumber, TryNumber}, Trace, Context, OutContext, Result) :-
  writeln({contextualise_result, suite_status, SuiteStatus}),
  context:expected_result(Context, ExpectedResult),
  % add TryNumber and number of tests reached to Context to make OutContext
  context:add_tests_passed(Context, TestNumber, Context1),
  context:add_tests_tried(Context1, TryNumber, Context2),
  % get Result from OutContext
  writeln({contextualise_result, expected_result, ExpectedResult}),
  build_result(SuiteStatus, ExpectedResult, TestNumber, Trace, Context2, Result),
  writeln({contextualise_result, result, Result}),
  context:add_suite_result(Context2, Result, OutContext)
.

% TODO -- get counterexample from context when suite is halted
build_result(suite_complete, pass, TestsPassed, LastTrace, Context, Result) :-
  % writeln({build_result, suite_complete, pass, TestsPassed, Context}),
  result:create_result(suite_pass, {tests_passed, TestsPassed}, Result)
.
build_result(suite_complete, fail, TestsPassed, LastTrace, Context, Result) :-
  result:create_result(suite_evidence_not_found, {tests_passed, TestsPassed}, Result)
.
build_result(suite_try_limit, pass, TestsPassed, LastTrace, Context, Result) :-
  result:create_result(suite_try_limit, {tests_passed, TestsPassed}, Result)
.
build_result(suite_try_limit, fail, TestsPassed, LastTrace, Context, Result) :-
  result:create_result(suite_evidence_not_found, {tests_passed, TestsPassed}, Result)
.
build_result(suite_halted, pass, TestsPassed, Counterexample, Context, Result) :-
  result:create_result(suite_fail, [{tests_passed, N}, {counterexample, Counterexample}], Result)
.
build_result(suite_halted, fail, TestsPassed, Counterexample, Context, Result) :-
  result:create_result(suite_evidence_found, [{tests_passed, N}, {counterexample, Counterexample}], Result)
.

test_suite_step(Now, TestResult, Next) :-
  writeln({test_suite_step, result, TestResult}),
  (
    result:check(TestResult, error, cant_satisfy), !,
    step_inc_try(Now, Next)
  ;
    step_inc_test(Now, Next)
  )
  , writeln({test_suite_step, Now, Next})
  % TODO -- validate input for test increment
.

step_inc_try({TestNumber, TryNumber}, {TestNumber, TryNumber1}) :- TryNumber1 is TryNumber + 1.
step_inc_test({TestNumber, TryNumber}, {TestNumber1, TryNumber}) :- TestNumber1 is TestNumber + 1.

check_for_suite_result(TestResult, Context, StepResult) :-
  writeln({check_for_suite_result, result, TestResult, Context}),
  result:check(TestResult, fail, false_goal), !,
  context:expected_result(Context, ExpectedResult),
  (
    ExpectedResult == pass, !,
    StepResult = fail
  ;
    ExpectedResult == fail, !,
    StepResult = counterexample_found
  ).
check_for_suite_result(TestResult, Context, continue).



process_result(Context, Result) :-
  writeln({process_result, context, Context}),
  writeln({process_result, result, Result}),
  nl
.


%% @}
