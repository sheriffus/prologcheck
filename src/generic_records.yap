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
* File:     generic_records.yap
* Last rev: 2019/06/06
* mods:
* comments: PrologCheck generic record handling shared module
*
*************************************************************************/


/** @defgroup GenericRecord PrologCheck generic records
@ingroup PrologCheck
@{

PrologCheck internal module that implements and manages a standardised format
of records with data attribute getting and setting and attribute dynamic
extension (optional additions).

A PrologCheck generic record is a record of the format
~~~~~~~~~~~~~~~~~~~~~{.prolog}
  {
    _Name_ :: atom,
    [
      {
        AttrName :: atom,
        AttrData :: term
      }
    ]
  }
~~~~~~~~~~~~~~~~~~~~~
where each element of the attribute list is a pair with the property/attribute
name in the first element and the data in the second element.

*/

/**
 * @file   generic_records.yap
 * @author Claudio Amaral <coa@dcc.fc.up.pt>
 * @date   Mon Apr 24 14:55 2019
 *
 * @brief  PrologCheck generic record handling shared module.
 *
*/
:- module(generic_records,
          [ get/4
          , replace/5
          , add/5
          , pop/5
          ]
         )
.


%% TODO - tests and documentation


get_first(Key, [{Key, Head}|Tail], Value, UnusedRecordAttrs) :-
  !,
  Value = Head,
  UnusedRecordAttrs = Tail.
get_first(Key, [{_OtherKey, _Head}|Tail], Value, UnusedRecordAttrs) :-
  get_first(Key, Tail, Value, UnusedRecordAttrs).

get(RecordName, Key, {RecordName, RecordAttrs}, Value) :-
  get_first(Key, RecordAttrs, Value, UnusedRecordAttrs),
  not(get_first(Key, UnusedRecordAttrs, _, _)).


replace_first(Key, [{Key, _Head}|Tail], NewValue, ReplacedL, UnusedRecordAttrs) :-
  !,
  ReplacedL = [{Key, NewValue} | Tail],
  UnusedRecordAttrs = Tail.
replace_first(Key, [{OtherKey, Head}|Tail], NewValue, [{OtherKey, Head}|ReplacedTail], UnusedRecordAttrs) :-
  replace_first(Key, Tail, NewValue, ReplacedTail, UnusedRecordAttrs).

replace(RecordName, Key, {RecordName, RecordAttrs}, NewValue, {RecordName, ReplacedL}) :-
  replace_first(Key, RecordAttrs, NewValue, ReplacedL, UnusedRecordAttrs),
  not(replace_first(Key, UnusedRecordAttrs, NewValue, _, _)).


add(RecordName, Key, {RecordName, RecordAttrs}, Value, {RecordName, ReplacedAttrs}) :-
  not(get_first(Key, RecordAttrs, _, _, _)),
  !,
  ReplacedAttrs = [ {Key, Value} | RecordAttrs].


pop_first(Key, [{Key, Head}|Tail], Value, UnusedRecordAttrs) :-
  !,
  Value = Head,
  UnusedRecordAttrs = Tail.
pop_first(Key, [{OtherKey, Head}|Tail], Value, [{OtherKey, Head}|UnusedRecordAttrs]) :-
  pop_first(Key, Tail, Value, UnusedRecordAttrs).

pop(RecordName, Key, {RecordName, RecordAttrs}, Value, {RecordName, SubtractedAttrs}) :-
  pop_first(Key, RecordAttrs, Value, SubtractedAttrs),
  not(get_first(Key, SubtractedAttrs, _, _)).


/** @} */
