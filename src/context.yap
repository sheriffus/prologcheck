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
* Last rev: 2019/05/15
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
 * @date   Mon May 15 15:15 2019
 *
 * @brief  PrologCheck runtime state of executing properties.
 *
*/
:- module(context).

:- reconsult(generic_records).

start_size(1).
default_binder(bind).
default_shrinker(shrink).
default_generator(generator).

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
         , {size, StartSize}
         , {bindings, [{trace, L-L}, {binder, bind}, {shrinker, shrink_size}, {generator, generator}]}
         ]
        }) :-
  start_size(StartSize)
.
%        , {mode, new}
%        , {[bindings, []]}
%        , {[actions, []]}
%        , {[samples, []]}
%        , {[printers, []]}
% %% default options
% default({opts,
%     format,            %% function for output  :: output_fun()
%     false,             %% present long result  :: boolean(),
%     100,               %% number of tests      :: pos_integer(),
%     1,                 %% initial size param   :: size(),
%     some_seed,         %% seed for random      :: seed(),  % TODO
%     42,                %% maximum size param   :: size(),
%     500,               %% max num of shrinks   :: non_neg_integer(),
%     false,             %% not shrinkin    g    :: boolean(),
%     50,                %% times to try to
%                        %%   gen constraint     :: pos_integer(),
%     false,             %% expect test fail     :: boolean(),
%     {type, plqc:int},  %% default 'any' type   :: {'type', types:any_type()},
%     infinity}) :-      %% max time to test :: timeout()}).
%         %% plqc_types:any_type(T).
%         T=fix_default_any__opts.
% init(Opts, State) :-
%         opts:start_size(Opts, StartSize),
%         opts:constraint_tries(Opts, CTries),
%         opts:any_type(Opts, AnyType),
%         %% opts:seed(Opts, Seed),
%         OSize is StartSize - 1,
%         put_size(OSize, [], State1),
%         put_left(0, State1, State2),
%         grow_size(Opts, State2, State3),
%         put_constraint_tries(CTries, State3, State4),
%         put_any_type(AnyType, State4, State5),
%         % {{{ TODO when choosing a seed is possible
%         % }}}
%         State = State5.

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


/** @section Size (Generated Test Data Size context attribute) */

/** @pred size_(+ _Context_, ? _Size_)
Predicate that succeeds when the given _Context_ is unifiable with a
context record with a size property and _Size_ is unifiable with said
size property.

Uses context generic 'get' predicate applied to size.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
size(+ _Context_, - _Size_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the _Size_ parameter is bound to the size property of
the given _Context_.
This is a 'getter' of the size attribute of a context object.
*/
size(Context, Size) :- get(size, Context, Size).

/** @pred new_size_(+ _Context_, ? _NewSize_, ? _NewContext_)

Predicate that succeeds when the given _Context_ is unifiable with a
context record with a size property and _NewContext_ is unifiable with
_Context_ with the size data replaced with the _Size_ parameter term.

Uses context generic 'replace' predicate applied to size.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
new_size(+ _Context_, + _NewSize_, - _NewContext_)
~~~~~~~~~~~~~~~~~~~~~
The free variable _NewContext_ is built/copied from _Context_ where
the value of the size property is replaced with the given _NewSize_.
This is a 'setter' of the size attribute of a context object.
*/
new_size(Context, NewSize, NewContext) :- replace(size, Context, NewSize, NewContext).

/** @pred increment_size(+ _Context_, ? _NewContext_)

Predicate that succeeds when the given _Context_ is unifiable with a
context record with a size property that is a number and _NewContext_ is
unifiable with _Context_ with the size data increased by 1 (_Size_ +1).

Uses increment_size/3 with step 2.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
increment_size(+ _Context_, - _NewContext_)
~~~~~~~~~~~~~~~~~~~~~
The free variable _NewContext_ is built/copied from _Context_ where
the value of the size property is replaced with (_Size_ + 1) where _Size_ is
the current size attribute data of _Context_.
This is a method for the manipulation of the size attribute of a context object.
*/
increment_size(Context, NewContext) :- increment_size(Context, NewContext, 1).

/** @pred increment_size(+ _Context_, ? _NewContext_, + _Step_)

Predicate that succeeds when the given _Context_ is unifiable with a
context record with a size property that is a number and _NewContext_ is
unifiable with _Context_ with the size data increased by _Step_ (_Size_ + _Step_).

Uses context 'size' and 'new_size' predicates and relies on 'is' predicate for
size value increment.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
increment_size(+ _Context_, - _NewContext_, + _Step_)
~~~~~~~~~~~~~~~~~~~~~
The free variable _NewContext_ is built/copied from _Context_ where
the value of the size property is replaced with (_Size_ + _Step_) where _Size_
is the current size attribute data of _Context_.
This is a method for the manipulation of the size attribute of a context object.
*/
increment_size(Context, NewContext, Step) :-
  size(Context, Size),
  NewSize is Size + Step,
  new_size(Context, NewSize, NewContext).


/** @section Bindings (Generated Test Data bindings context attribute) */

/** @pred bindings_(+ _Context_, ? _Bindings_)
Predicate that succeeds when the given _Context_ is unifiable with a
context record with a bindings property and _Bindings_ is unifiable with said
bindings property.

Uses context generic 'get' predicate applied to bindings.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
bindings(+ _Context_, - _Bindings_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the _Bindings_ parameter is bound to the bindings property
of the given _Context_.
This is a 'getter' of the bindings attribute of a context object.
*/
bindings(Context, Bindings) :- get(bindings, Context, Bindings).


% TODO rework bindings and move methods to appropriate modules
new_bindings(Context, NewBindings, NewContext) :- replace(bindings, Context, NewBindings, NewContext).

bind_forall(Module:Gen, Context, X, Size, OutContext) :-
  writeln({'DEBUG', bind_for_all}),
  !, call(Module:Gen, X, TestDatum, Size),
  bindings(Context, Bindings),
  Bindings = [{trace, Binds-Rest} | Tail],
  Rest = [ TestDatum | NewRest],
  NewBindings = [{trace, Binds-NewRest} | Tail],
  new_bindings(Context, NewBindings, OutContext)
.
bind_forall(Gen, Context, Var, Size, OutContext) :-
  context:module(Context, Module),
  bind_forall(Module:Gen, Context, Var, Size, OutContext)
.
/** @} */
