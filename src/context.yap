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
* File:     context.yap
* Last rev: 2019/04/24
* mods:
* comments: PrologCheck runtime state of executing properties
*
*************************************************************************/


/** @defgroup Context Property execution runtime state
@ingroup PrologCheck
@{

PrologCheck internal module that implements and manages auxiliary records
containing the state needed for executing a test of a property.

A context record is a record of the format
    {ctx, _PropertyList_}
where each element of _PropertyList_ is a pair with the context property name
in the first element and the property data in the second element.

Context Properties:
- module -- name of the module/file where the property being executed is
defined and/or its goals are to be executed;
- result (optional) -- the information gathered after a test execution.

~~~~~~~~~~~~~~~~~~~~~{.prolog}
Record =
{
  ctx,
  [
    {module, Module :: module_name}
  ]
}.
~~~~~~~~~~~~~~~~~~~~~
\internal
TODO, mode         :: 'new' | 'try_shrunk' | 'try_cexm'
TODO, bound        :: imm_testcase  () | counterexample ()
TODO, actions      :: fail_actions()                    % not used
TODO, samples      :: [sample()]                        % not used
TODO, printers     :: [stats_printer()]                 % not used
\endinternal
*/

/**
 * @file   context.yap
 * @author Claudio Amaral <coa@dcc.fc.up.pt>
 * @date   Mon Apr 24 14:55 2019
 *
 * @brief  PrologCheck runtime state of executing properties.
 *
*/
:- module(context).

:- reconsult(generic_records).


/** @pred default_(? _Context_)
Predicate that succeeds when the given _Context_ parameter is unifiable with a
context record of the default values defined for the runtime property context.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
:- default (- _Context_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the parameter is bound to the default context properties.
This is the constructor of context objects.

Defaults:
- module -- _plqc_, the framework main module --, module form which the test
goals shall be executed from

TODO: see if caller module is retrievable for module default value
*/
default({ctx,
         [
           {module, plqc}
         ]
        }).
%        , {mode, new}
%        , {[bindings, []]}
%        , {[actions, []]}
%        , {[samples, []]}
%        , {[printers, []]}

/** @section Generics (Generic record wrapper predicates) */

/** @pred get_(+ _Key_, + _ContextRecord_, ? _Value_)
Predicate that succeeds when _ContextRecord_ is unifiable with a
context record containing a context attribute _Key_ and _Value_ is unifiable
with said context attribute stored data.

Uses generic_records' 'get' predicate applied to 'ctx'.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
get(+ _Key_, + _ContextRecord_, - _Value_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the _Value_ parameter is bound to the context attribute
data of the given _ContextRecord_ attribute _Key_.
This is the generic 'getter' of the context record module.
*/
get(Key, ContextRecord, Value) :-
  generic_records:get(ctx, Key, ContextRecord, Value).

/** @pred replace_(+ _Key_, + _ContextRecord_, ? _NewValue_, ? _ReplacedRecord_)
Predicate that succeeds when the given _ContextRecord_ is unifiable with a
context record containing a context attribute _Key_ and _ReplacedRecord_ is
unifiable with _ContextRecord_ with the data for its _Key_ attribute replaced
with the _NewValue_ parameter term.

Uses generic_records' 'replace' predicate applied to 'ctx'.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
replace(+ _Key_, + _ContextRecord_, + _NewValue_, - _ReplacedRecord_)
~~~~~~~~~~~~~~~~~~~~~
The free variable _ReplacedRecord_ is built/copied from _ContextRecord_ where
the value of the _Key_ attribute is replaced with the given _NewValue_.
This is the generic 'setter' of the context record module.
*/
replace(Key, ContextRecord, NewValue, ReplacedRecord) :-
  generic_records:replace(ctx, Key, ContextRecord, NewValue, ReplacedRecord).

/** @pred add_(+ _Key_, + _ContextRecord_, ? _NewValue_, ? _ExtendedContextRecord_)
Predicate that succeeds when the given _ContextRecord_ is unifiable with a
context record NOT containing a context attribute _Key_ and _ExtendedContextRecord_
is an extension of _ContextRecord_ with a result attribute where the data is
unified with the _NewValue_ parameter term.

Uses generic_records' 'add' predicate applied to 'ctx'.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
add(+ _Key_, + _ContextRecord_, + _NewValue_, - _ExtendedContextRecord_)
~~~~~~~~~~~~~~~~~~~~~
The free variable _ExtendedContextRecord_ is built/copied from _ContextRecord_
where the value of the _Key_ attribute is added at the head with the given
_NewValue_ as the attribute data.
This is an optional extension of the _Key_ attribute of a context object
without _Key_.

This is the generic extension predicate of the context record module.
*/
add(Key, ContextRecord, NewValue, ExtendedContextRecord) :-
  generic_records:add(ctx, Key, ContextRecord, NewValue, ExtendedContextRecord).

/** @section Module (Module context attribute) */

/** @pred module_(+ _Context_, ? _Module_)
Predicate that succeeds when the given _Context_ is unifiable with a
context record with a module property and _Module_ is unifiable with said
module property.

Uses context generic 'get' predicate applied to module.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
module(+ _Context_, - _Module_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the _Module_ parameter is bound to the module property of
the given _Context_.
This is a 'getter' of the module attribute of a context object.
*/
module(Context, Module) :- get(module, Context, Module).

/** @pred new_module_(+ _Context_, ? _NewModule_, ? _NewContext_)

Predicate that succeeds when the given _Context_ is unifiable with a
context record with a module property and _NewContext_ is unifiable with
_Context_ with the module data replaced with the _Module_ parameter term.

Uses context generic 'replace' predicate applied to module.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
module(+ _Context_, + _NewModule_, - _NewContext_)
~~~~~~~~~~~~~~~~~~~~~
The free variable _NewContext_ is built/copied from _Context_ where
the value of the module property is replaced with the given _NewModule_.
This is a 'setter' of the module attribute of a context object.
*/
new_module(Context, NewModule, NewContext) :- replace(module, Context, NewModule, NewContext).


/** @section Result (Result context attribute) */

/** @pred result_(+ _Context_, ? _Result_)
Predicate that succeeds when the given _Context_ is unifiable with a
context record with a result property and _Result_ is unifiable with said
result property.

Uses context generic 'get' predicate applied to result.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
result(+ _Context_, - _Result_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the _Result_ parameter is bound to the result property of
the given _Context_.
This is a 'getter' of the result attribute of a context object.
*/
result(Context, Result) :- get(result, Context, Result).

/** @pred new_result_(+ _Context_, ? _NewResult_, ? _NewContext_)
Predicate that succeeds when the given _Context_ is unifiable with a
context record with a result property and _NewContext_ is unifiable with
_Context_ with the result data replaced with the _NewResult_ parameter term.

Uses context generic 'replace' predicate applied to result.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
new_result(+ _Context_, + _NewResult_, - _NewContext_)
~~~~~~~~~~~~~~~~~~~~~
The free variable _NewContext_ is built/copied from _Context_ where
the value of the result property is replaced with the given _NewResult_.
This is a 'setter' of the result attribute of a context object.
*/
new_result(Context, NewResult, NewContext) :- replace(result, Context, NewResult, NewContext).

/** @pred add_result_(+ _Context_, ? _NewResult_, ? _NewContext_)
Predicate that succeeds when the given _Context_ is unifiable with a
context record WITHOUT a result property and _NewContext_ is an extension of
_Context_ with a the result attribute with data unified with the _NewResult_
parameter term.

Uses context generic 'add' predicate applied to result.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
add_result(+ _Context_, + _NewResult_, - _NewContext_)
~~~~~~~~~~~~~~~~~~~~~
The free variable _NewContext_ is built/copied from _Context_ where
the value of the result property is added at the head with the given _NewResult_
as the attribute data.
This is an optional extension of the result attribute of a context object
without result.
*/
add_result(Context, Result, NewContext) :- add(result, Context, Result, NewContext).

/** @} */
