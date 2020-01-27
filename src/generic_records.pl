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


/** @section Get (Generic record wrapper get predicates) */

/** @pred get_first_(? _Key_, ? _RecordDataDict_, ? _Value_, ? _UnusedRecordDataDict_)
Predicate that succeeds when _RecordDataDict_ is unifiable with a
record data dictionary (list) containing an attribute with key unifiable with
_Key_ and _Value_ is unifiable with said attribute's stored data.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
get_first(+ _Key_, + _RecordDataDict_, - _Value_, - _RecordDataDict_, - _UnusedRecordDataDict_)
~~~~~~~~~~~~~~~~~~~~~
_Value_ is unified with the first possible attribute data (dictionary as a
sequence of attributes) and the _UnusedRecordDataDict_ is unified with the tail
of the attribute list where {_Key_, _Value_} is the head.
*/
get_first(Key, [{Key, Head}|Tail], Value, UnusedRecordAttrs) :-
  !,
  Value = Head,
  UnusedRecordAttrs = Tail.
get_first(Key, [{_OtherKey, _Head}|Tail], Value, UnusedRecordAttrs) :-
  get_first(Key, Tail, Value, UnusedRecordAttrs).

/** @pred get_(? _RecordName_, ? _Key_, ? {_RecordName_, _RecordDataDict_}, ? _Value_)
Predicate that succeeds when _RecordDataDict_ is unifiable with a
record data dictionary (list) containing an attribute with key unifiable with
_Key_ and _Value_ is unifiable with said attribute's stored data.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
get(+ _RecordName_, + _Key_, + {_RecordName_, _RecordDataDict_}, - _Value_)
~~~~~~~~~~~~~~~~~~~~~
Clause head matches expected record name and actual record name (unifies
_RecordName_ with record name field) and delegates main behaviour to internal
predicate, ensuring there is only one occurrence.
*/
get(RecordName, Key, {RecordName, RecordAttrs}, Value) :-
  get_first(Key, RecordAttrs, Value, UnusedRecordAttrs),
  not(get_first(Key, UnusedRecordAttrs, _, _)).


/** @section Replace (Generic record wrapper replace predicates) */

/** @pred replace_first_(? _Key_, ? _RecordDataDict_, ? _NewValue_, ? _ReplacedRecordDataDict_, ? _UnusedRecordDataDict_)
Predicate that succeeds when _RecordDataDict_ is unifiable with a
record data dictionary (list) containing an attribute with key unifiable with
_Key_ and _ReplacedRecordDataDict_ is unifiable with _RecordDataDict_ with the
data for its _Key_ attribute replaced with the _NewValue_ parameter term.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
replace_first(+ _Key_, + _RecordDataDict_, - _NewValue_, - _ReplacedRecordDataDict_, ? _UnusedRecordDataDict_)
~~~~~~~~~~~~~~~~~~~~~
The free variable _ReplacedRecordDataDict_ is built/copied from _RecordDataDict_
where the value of the _Key_ attribute is replaced with the given _NewValue_.
the _UnusedRecordDataDict_ is unified with the tail of the attribute list where
{_Key_, _Value_} is the head.
*/
replace_first(Key, [{Key, _Head}|Tail], NewValue, ReplacedL, UnusedRecordAttrs) :-
  !,
  ReplacedL = [{Key, NewValue} | Tail],
  UnusedRecordAttrs = Tail.
replace_first(Key, [{OtherKey, Head}|Tail], NewValue, [{OtherKey, Head}|ReplacedTail], UnusedRecordAttrs) :-
  replace_first(Key, Tail, NewValue, ReplacedTail, UnusedRecordAttrs).

/** @pred replace_(? _RecordName_, ? _Key_, ? {_RecordName_, _RecordDataDict_}, ? _NewValue_, ? {_RecordName_, _ReplacedRecordDataDict_})
Predicate that succeeds when _RecordDataDict_ is unifiable with a
record data dictionary (list) containing an attribute with key unifiable with
_Key_ and _ReplacedRecordDataDict_ is unifiable with _RecordDataDict_ with the
data for its _Key_ attribute replaced with the _NewValue_ parameter term.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
replace(+ _RecordName_, + _Key_, + {_RecordName_, _RecordDataDict_}, - _NewValue_, {? _RecordName_, - _ReplacedRecordDataDict_})
~~~~~~~~~~~~~~~~~~~~~
Clause head matches expected record name and actual record name (unifies
_RecordName_ with record name field) and delegates main behaviour to internal
predicate, ensuring there is only one occurrence.
*/
replace(RecordName, Key, {RecordName, RecordAttrs}, NewValue, {RecordName, ReplacedL}) :-
  replace_first(Key, RecordAttrs, NewValue, ReplacedL, UnusedRecordAttrs),
  not(replace_first(Key, UnusedRecordAttrs, NewValue, _, _)).


/** @section Add (Generic record wrapper add predicate) */

/** @pred add_(? _RecordName_, ? _Key_, ? {_RecordName_, _RecordDataDict_}, ? _NewValue_, ? {_RecordName_, _ExtendedRecordDataDict_})
Predicate that succeeds when _RecordDataDict_ is unifiable with a
record data dictionary (list) NOT containing an attribute with key unifiable with
_Key_ and _ExtendedRecordDataDict_ is an extension of _RecordDataDict_ with an
attribute with key _Key_ and the data is unified with the _NewValue_ parameter.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
add(+ _Key_, + _Record_, + _NewValue_, - _ExtendedRecord_)
~~~~~~~~~~~~~~~~~~~~~
Predicate body ensures _Key_ does not occur in the record attributes of
_Record_. The free variable _ExtendedRecord_ is built/copied from _Record_
where the value of the _Key_ attribute is added at the head with the given
_NewValue_ as the attribute data.

*/
add(RecordName, Key, {RecordName, RecordAttrs}, Value, {RecordName, ExtendedAttrs}) :-
  not(get_first(Key, RecordAttrs, _, _)),
  !,
  ExtendedAttrs = [ {Key, Value} | RecordAttrs].


/** @section Add (Generic record wrapper add predicate) */

/** @pred add_(? _RecordName_, ? _Key_, ? {_RecordName_, _RecordDataDict_}, ? _NewValue_, ? {_RecordName_, _ExtendedRecordDataDict_})
Predicate that succeeds when _RecordDataDict_ is unifiable with a
record data dictionary (list) NOT containing an attribute with key unifiable with
_Key_ and _ExtendedRecordDataDict_ is an extension of _RecordDataDict_ with an
attribute with key _Key_ and the data is unified with the _NewValue_ parameter.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
add(+ _Key_, + _Record_, + _NewValue_, - _ExtendedRecord_)
~~~~~~~~~~~~~~~~~~~~~
Predicate body ensures _Key_ does not occur in the record attributes of
_Record_. The free variable _ExtendedRecord_ is built/copied from _Record_
where the value of the _Key_ attribute is added at the head with the given
_NewValue_ as the attribute data.

*/
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
