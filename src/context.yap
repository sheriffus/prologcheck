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
* Last rev: 2019/04/22
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
- module -- name of the module/file where the propertybeing executed is defined.

~~~~~~~~~~~~~~~~~~~~~{.prolog}
{
  ctx,
  [
    {module, Module :: module_name()}
  ]
}
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
 * @date   Mon Apr 22 17:55 2019
 *
 * @brief  PrologCheck runtime state of executing properties.
 *
*/
:- module(context).


/**
Predicate that succeeds when the given _Context_ parameter is unifiable with a
context record of the default values defined for the runtime property context.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
default_(- _Context_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the parameter is bound to the default context properties.
This is the constructor of context objects.

Defaults:
- module -- _plqc_, the framework main module

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

/** @pred default_(? _Context_)
Succeeds with _Context_ unified to a context record of the default values
defined for the runtime property context.
*/

get_first(Key, [{Key, Head}|Tail], Value, UnusedContext) :-
    !,
    Value = Head,
    UnusedContext = Tail.
get_first(Key, [{_OtherKey, _Head}|Tail], Value, UnusedContext) :-
    get_first(Key, Tail, Value, UnusedContext).

get(Key, {ctx, Context}, Value) :-
    get_first(Key, Context, Value, UnusedContext),
    not(get_first(Key, UnusedContext, _, _)).


replace_first(Key, [{Key, _Head}|Tail], NewValue, ReplacedL, UnusedContextL) :-
    !,
    ReplacedL = [{Key, NewValue} | Tail],
    UnusedContextL = Tail.
replace_first(Key, [{OtherKey, Head}|Tail], NewValue, [{OtherKey, Head}|ReplacedTail], UnusedContextL) :-
    replace_first(Key, Tail, NewValue, ReplacedTail, UnusedContextL).

replace(Key, {ctx, ContextL}, NewValue, {ctx, ReplacedL}) :-
    replace_first(Key, ContextL, NewValue, ReplacedL, UnusedContextL),
    not(replace_first(Key, UnusedContextL, NewValue, _, _)).


/**
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

/**
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



/** @} */
