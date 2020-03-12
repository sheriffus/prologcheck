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
* File:     result.yap
* Last rev: 2020/03/12
* mods:
* comments: PrologCheck result handling module
*
*************************************************************************/


/** @defgroup Result Property execution result handling
@ingroup PrologCheck
@{

PrologCheck internal module that implements and manages auxiliary records
of property execution results and their manipulation.

A result record is a record of the format
~~~~~~~~~~~~~~~~~~~~~{.prolog}
{
  result,
  Result :: 'pass' | 'fail',
  [
    {reason, Reason :: result_reason()}
  ]
}
~~~~~~~~~~~~~~~~~~~~~
where
- _Result_ states the gross test result of one execution of a property;
- _Reason_ states the reason for the gross result obtained.
and where
~~~~~~~~~~~~~~~~~~~~~{.bnf}
result_reason() :=
 | 'true_prop'
 | 'false_prop'
~~~~~~~~~~~~~~~~~~~~~

*/

/**
 * @file   result.pl
 * @author Claudio Amaral <coa@dcc.fc.up.pt>
 * @date   Thu Mar 12 11:55 2020
 *
 * @brief  PrologCheck result handling module.
 *
*/
:- module(result,
          [
            create_result/3
          % generic methods
          , get/3%, replace/4, add/4, pop/4
          , level/2
          , raw/2
          , reason/2
          , tests_passed/2
          , counterexample/2
          ]
         ).

:- use_module(generic_records).

% TODO - documentation
% TODO - use generic_records to create the result record

% %% Valid result level and attribute pairs
result_attributes(test_case, level).
result_attributes(test_case, reason).
result_attributes(test_case, raw).
%% suite
result_attributes(test_suite, level).
result_attributes(test_suite, raw).
result_attributes(test_suite, reason).
result_attributes(test_suite, tests_passed).
%% Valid result level, reason and attribute trio
result_conditional_attributes(test_suite, counterexample_found, counterexample).
result_conditional_attributes(test_suite, failing_evidence_found, counterexample).
%result_conditional_attributes(_, _, _).

create_result(test_pass, {reason, true_goal}, Result) :-
  Result = {result, [{level, test_case}, {raw, pass}, {reason, true_goal}]}.
create_result(test_fail, {reason, false_goal}, Result) :-
  Result = {result, [{level, test_case}, {raw, fail}, {reason, false_goal}]}.
create_result(test_precond_fail, {reason, cant_satisfy}, Result) :-
  Result = {result, [{level, test_case}, {raw, error}, {reason, cant_satisfy}]}.

create_result(suite_try_limit, {tests_passed, 0}, Result) :- !,
  Result = {result, [{level, test_suite}, {raw, error}, {reason, cant_satisfy}, {tests_passed, 0}]}.
create_result(suite_try_limit, {tests_passed, N}, Result) :-
  Result = {result, [{level, test_suite}, {raw, pass_partial}, {reason, max_tries_reached}, {tests_passed, N}]}.
create_result(suite_pass, {tests_passed, N}, Result) :-
  Result = {result, [{level, test_suite}, {raw, pass}, {reason, suite_complete}, {tests_passed, N}]}.
create_result(suite_fail, [{tests_passed, N}, {counterexample, C}], Result) :-
  Result = {result, [{level, test_suite}, {raw, fail}, {reason, counterexample_found}, {tests_passed, N}, {counterexample, C}]}.
create_result(suite_evidence_found, [{tests_passed, N}, {counterexample, C}], Result) :-
  Result = {result, [{level, test_suite}, {raw, pass}, {reason, failing_evidence_found}, {tests_passed, N}, {counterexample, C}]}.
create_result(suite_evidence_not_found, {tests_passed, N}, Result) :-
  Result = {result, [{level, test_suite}, {raw, fail}, {reason, failing_evidence_not_found}, {tests_passed, N}]}.


/** @section Generics (Generic record wrapper predicates) */

/** @pred get_(+ _Key_, + _ResultRecord_, ? _Value_)
Predicate that succeeds when _ResultRecord_ is unifiable with a
result record containing a result attribute _Key_ and _Value_ is unifiable
with said result attribute stored data.

Uses generic_records' 'get' predicate applied to 'result'.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
get(+ _Key_, + _ResultRecord_, - _Value_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the _Value_ parameter is bound to the result attribute
data of the given _ResultRecord_ attribute _Key_.
This is the generic 'getter' of the result record module.
*/
get(Key, ResultRecord, Value) :-
  generic_records:get(result, Key, ResultRecord, Value).

/** @pred replace_(+ _Key_, + _ResultRecord_, ? _NewValue_, ? _ReplacedRecord_)
Predicate that succeeds when the given _ResultRecord_ is unifiable with a
result record containing a result attribute _Key_ and _ReplacedRecord_ is
unifiable with _ResultRecord_ with the data for its _Key_ attribute replaced
with the _NewValue_ parameter term.

Uses generic_records' 'replace' predicate applied to 'result'.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
replace(+ _Key_, + _ResultRecord_, + _NewValue_, - _ReplacedRecord_)
~~~~~~~~~~~~~~~~~~~~~
The free variable _ReplacedRecord_ is built/copied from _ResultRecord_ where
the value of the _Key_ attribute is replaced with the given _NewValue_.
This is the generic 'setter' of the result record module.
*/
% replace(Key, ResultRecord, NewValue, ReplacedRecord) :-
%   generic_records:replace(result, Key, ResultRecord, NewValue, ReplacedRecord).

/** @pred add_(+ _Key_, + _ResultRecord_, ? _NewValue_, ? _ExtendedResultRecord_)
Predicate that succeeds when the given _ResultRecord_ is unifiable with a
result record NOT containing a result attribute _Key_ and _ExtendedResultRecord_
is an extension of _ResultRecord_ with a result attribute where the data is
unified with the _NewValue_ parameter term.

Uses generic_records' 'add' predicate applied to 'result'.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
add(+ _Key_, + _ResultRecord_, + _NewValue_, - _ExtendedResultRecord_)
~~~~~~~~~~~~~~~~~~~~~
The free variable _ExtendedResultRecord_ is built/copied from _ResultRecord_
where the value of the _Key_ attribute is added at the head with the given
_NewValue_ as the attribute data.
This is an optional extension of the _Key_ attribute of a result object
without _Key_.

This is the generic extension predicate of the result record module.
*/

% add(Key, ResultRecord, NewValue, ExtendedResultRecord) :-
%   generic_records:add(result, Key, ResultRecord, NewValue, ExtendedResultRecord).

% pop(Key, ResultRecord, Value, SubtractedResultRecord) :-
%   generic_records:pop(result, Key, ResultRecord, Value, SubtractedResultRecord).

/** @section Level (Level of the result) */

level(Result, LevelResult) :- get(level, Result, LevelResult).

/** @section Raw (Raw result of an execution) */

/** @pred raw_(+ _Result_, ? _RawResult_)
Predicate that succeeds when the given _Result_ is unifiable with a
result record with a raw property and _RawResult_ is unifiable with said
raw property.

Uses result generic 'get' predicate applied to raw.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
raw(+ _Result_, - _RawResult_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the _RawResult_ parameter is bound to the raw property of
the given _Result_.
This is a 'getter' of the raw attribute of a result object.
*/
raw(Result, RawResult) :- get(raw, Result, RawResult).

/** @section Reason (Reason for the raw result of an execution) */

/** @pred reason_(+ _Result_, ? _Reason_)
Predicate that succeeds when the given _Result_ is unifiable with a
result record with a reason property and _Reason_ is unifiable with said
reason property.

Uses result generic 'get' predicate applied to reason.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
reason(+ _Result_, - _Reason_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the _Reason_ parameter is bound to the reason property of
the given _Result_.
This is a 'getter' of the reason attribute of a result object.
*/
reason(Result, Reason) :- get(reason, Result, Reason).

/** @section Tries (Number of attempts to test the property) */

/** @pred tries_(+ _Result_, ? _Tries_)
Predicate that succeeds when the given _Result_ is unifiable with a
result record with a tries property and _Tries_ is unifiable with said
tries property.

Uses result generic 'get' predicate applied to tries.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
tries(+ _Result_, - _Tries_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the _Tries_ parameter is bound to the tries property of
the given _Result_.
This is a 'getter' of the tries attribute of a result object.
*/
% tries(Result, Tries) :- get(reason, Result, Tries).

/** @section Tests Passed (Number of tests passed in the result of a test suite) */

tests_passed(Result, TestsPassedResult) :- get(tests_passed, Result, TestsPassedResult).

/** @section Counter Example (Evidence that a property does not hold extracted from the result of a test suite) */

counterexample(Result, CounterExample) :- get(counterexample, Result, CounterExample).


% check(Result, ExpectedRaw, ExpectedReason) :-
%   writeln({check_result_result, result, Result, ExpectedRaw, ExpectedReason}),
%   raw(Result, Raw),
%   reason(Result, Reason),
%   writeln({check_result_result, result, Result, Raw, Reason}),
%   {Raw, Reason} == {ExpectedRaw, ExpectedReason}
% .

/** @} */
