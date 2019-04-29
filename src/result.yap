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
* Last rev: 2019/04/23
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
 * @file   result.yap
 * @author Claudio Amaral <coa@dcc.fc.up.pt>
 * @date   Mon Apr 23 17:55 2019
 *
 * @brief  PrologCheck result handling module.
 *
*/
:- module(result,
          [
            create_result/3,
          %  get/3,
          %  replace/4,
          %  add/4,
            raw/2,
            reason/2
          ]
         ).

:- reconsult(generic_records).

% TODO - documentation
% TODO - use generic_records to create the result record

create_result(pass, {reason, true_goal}, Result) :-
  Result = {result, [{raw, pass}, {reason, true_goal}]}.
create_result(fail, {reason, false_goal}, Result) :-
  Result = {result, [{raw, fail}, {reason, false_goal}]}.


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
replace(Key, ResultRecord, NewValue, ReplacedRecord) :-
  generic_records:replace(result, Key, ResultRecord, NewValue, ReplacedRecord).

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
add(Key, ResultRecord, NewValue, ExtendedResultRecord) :-
  generic_records:add(ctx, Key, ResultRecord, NewValue, ExtendedResultRecord).


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

/** @} */
