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
* Last rev: 2019/07/18
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
 * @date   Mon July 18 15:15 2019
 *
 * @brief  PrologCheck runtime state of executing properties.
 *
*/
:- module(context, [
                     default/2
                   , configuration/2
                   , init/1
                   % generic methods
                   % , get/3, replace/4, add/4, pop/4
                   , module/2, new_module/3
                   , expected_result/2, new_expected_result/3
                   , test_result/2% , add_test_result/3, pop_test_result/3
                   % , suite_result/2, add_suite_result/3
                   , size/2, new_size/3, increment_size/2, increment_size/3, size_step/2
                   , tries_calc_predicate/2
                   , num_tests/2
                   % , parse/2, parse/3
                   % , tests_passed/2, add_tests_passed/3
                   % , tests_tried/2, add_tests_tried/3
                   % modular functions' getters
                   , binder/2
                   , shrinker/2
                   , generator/2

                   % , bindings/2, reset_bindings_trace/3, new_bindings/3
                   % , times_five/2
                   % , bind_forall/5

]).

:- use_module('../src/generic_records.pl').

/** @pred default_(? _Attribute_, ? _Value_)
Predicate that succeeds when the given _Value_ parameter is unifiable with the
defined default value of the attribute that the parameter _Attribute_ is
unifiable to.
It should be equivalent to a key-value dictionary, with only one value per
attribute (the attribute's default value).

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
:- default (+_Attribute_, - _Value_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the _Value_ parameter is bound to the default defined value
for the configuration attribute _Attribute_.
This is the accessor of default context configuration attributes.

Defaults:
- module -- _plqc_, the framework main module -- module form which the test
goals shall be executed from
- start_size -- _1_ -- default start size
- size_step -- _1_ -- default size increment between tests
- num_tests -- 100 -- default number of tests in a (successfull) test suite execution
- tries_calc_predicate -- x5 -- predicate to be used, by default, to calculate the maximum number of attempts to test a property (with valid input/pre-conditions)
- expected_result -- pass -- result that is expected of the test/property
- binder -- TBD
- shrinker -- TBD
- generator -- TBD

TODO: see if caller module is retrievable for module default value
*/
default(module, plqc).
default(start_size,1).
default(size_step,1).
default(num_tests,100).
default(tries_calc_predicate,context:times_five). % TODO - move to config module
default(expected_result,pass).
default(binder,bind).
default(shrinker,shrink_size).
default(generator,generator).


:- dynamic configuration/2.

/** @pred configuration_(? _Attribute_, ? _Value_)
Predicate that succeeds when the given _Value_ parameter is unifiable with the
defined value of the attribute that the parameter _Attribute_ is unifiable to.
It should be equivalent to a key-value dictionary, with only one value per
attribute (the attribute's currently defined value).

This is a dynamic predicate to allow the redefinition of configuration
attributes of PrologCheck and is initially defined int erms of the default
values of the attributes given by default/2.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
:- configuration (+_Attribute_, - _Value_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the _Value_ parameter is bound to the currently defined
value for the configuration attribute _Attribute_.
This is the accessor of currently defined context configuration attributes.
*/
configuration(module, Module) :-
  default(module, Module).
configuration(start_size,StartSize) :-
   default(start_size,StartSize).
configuration(size_step,SizeStep) :-
   default(size_step,SizeStep).
configuration(num_tests,NumTests) :-
   default(num_tests,NumTests).
configuration(tries_calc_predicate,TriesCalcPredicate) :-
   default(tries_calc_predicate,TriesCalcPredicate).
configuration(expected_result,ExpectedResult) :-
   default(expected_result,ExpectedResult).
configuration(binder,Binder) :-
   default(binder,Binder).
configuration(shrinker,Shrinker) :-
   default(shrinker,Shrinker).
configuration(generator,Generator) :-
   default(generator,Generator).
%% end of configuration/2 predicate

/** @pred init_(? _Context_)
Predicate that succeeds when the given _Context_ parameter is unifiable with a
context record of the currently defined values for the runtime property context.

The values in the context initialisation are taken from the configuration/2
predicate of currently defined context attributes.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
:- default (- _Context_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the parameter is bound to the currently defined context
properties.
This is the constructor of context objects.

*/
init({ctx,
         [ {module, Module}
         , {size, StartSize}
         , {size_step, SizeStep}
         , {num_tests, NumTests}
         , {tries_calc_predicate, TriesCalcPredicate}
         , {expected_result, ExpectedResult}
         , {bindings, [{trace, L-L}, {binder, Binder
         }, {shrinker, Shrinker}, {generator, Generator}]}
         ]
        }) :-
  configuration(module, Module),
  configuration(start_size, StartSize),
  configuration(size_step, SizeStep),
  configuration(num_tests, NumTests),
  configuration(tries_calc_predicate, TriesCalcPredicate),
  configuration(expected_result, ExpectedResult),
  configuration(binder, Binder),
  configuration(shrinker, Shrinker),
  configuration(generator, Generator)
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
% %% {pass,  reason    :: pass_reason(),
% %%         samples   :: [sample()],
% %%         printers  :: [stats_printer()],
% %%         performed :: pos_integer()}.
% default_pass({pass, Reason, Bound, Samples, Printers, Performed}).
% output_fun() :- quiet(opts:quietfun), verbose(opts:verbosefun),
%              {to_file,IoDev}( opts:tofilefun(IoDev) )% TODO io:format(IoDev, S, F)
%              {to_strem,Strem}( opts:tostreamfun(Stream) ).
%              {on_output,Print}( Print ).
% long_result( true ).
% {numtests,N}, OptsA, Opts) :- new_numtests(OptsA, N, Opts).
% {start_size,Size}, OptsA, Opts) :- new_start_size(OptsA, Size, Opts).
% {max_size,Size}, OptsA, Opts) :- new_max_size(OptsA, Size, Opts).
% {max_shrinks,N}, OptsA, Opts) :- new_max_shrinks(OptsA, N, Opts).
% noshrink, OptsA, Opts) :- new_noshrink(OptsA, true, Opts).
% {constraint_tries,N}, OptsA, Opts) :- new_constraint_tries(OptsA, N, Opts).
% fails, OptsA, Opts) :- new_expect_fail(OptsA, true, Opts).
% any_to_integer, OptsA, Opts) :-
%         proper_types:integer(I),
%         new_any_type(OptsA, {type,I}, Opts).
% {spec_timeout,N}, OptsA, Opts) :- new_spec_timeout(OptsA, N, Opts).

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

% pop(Key, ContextRecord, Value, SubtractedContextRecord) :-
%   generic_records:pop(ctx, Key, ContextRecord, Value, SubtractedContextRecord).

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


/** @section ExpectedResult (Expected Result context attribute) */

/** @pred expected_result_(+ _Context_, ? _ExpectedResult_)
Predicate that succeeds when the given _Context_ is unifiable with a context
record with a expected_result property and _ExpectedResult_ is unifiable with
said expected_result property.

Uses context generic 'get' predicate applied to expected_result.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
expected_result(+ _Context_, - _ExpectedResult_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the _ExpectedResult_ parameter is bound to the
expected_result property of the given _Context_.
This is a 'getter' of the expected_result attribute of a context object.
*/
expected_result(Context, ExpectedResult) :- get(expected_result, Context, ExpectedResult).

/** @pred new_expected_result_(+ _Context_, ? _NewExpectedResult_, ? _NewContext_)

Predicate that succeeds when the given _Context_ is unifiable with a
context record with a expected_result property and _NewContext_ is unifiable with
_Context_ with the expected_result data replaced with the _NewExpectedResult_
parameter term.

Uses context generic 'replace' predicate applied to expected_result.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
expected_result(+ _Context_, + _NewExpectedResult_, - _NewContext_)
~~~~~~~~~~~~~~~~~~~~~
The free variable _NewContext_ is built/copied from _Context_ where the value
of the expected_result property is replaced with the given _NewExpectedResult_.
This is a 'setter' of the expected_result attribute of a context object.
*/
new_expected_result(Context, NewExpectedResult, NewContext) :- replace(expected_result, Context, NewExpectedResult, NewContext).


/** @section Result (Result context attribute) */

/** @pred test_result_(+ _Context_, ? _TestResult_)
Predicate that succeeds when the given _Context_ is unifiable with a
context record with a test_result property and _TestResult_ is unifiable
with said test_result property.

Uses context generic 'get' predicate applied to test_result.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
test_result(+ _Context_, - _TestResult_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the _TestResult_ parameter is bound to the test_result
property of the given _Context_.
This is a 'getter' of the test_result attribute of a context object.
*/
test_result(Context, TestResult) :- get(test_result, Context, TestResult).

% /** @pred new_test_result_(+ _Context_, ? _NewResult_, ? _NewContext_)
% Predicate that succeeds when the given _Context_ is unifiable with a
% context record with a result property and _NewContext_ is unifiable with
% _Context_ with the result data replaced with the _NewResult_ parameter term.
%
% Uses context generic 'replace' predicate applied to result.
%
% Intended use:
% ~~~~~~~~~~~~~~~~~~~~~{.prolog}
% new_result(+ _Context_, + _NewResult_, - _NewContext_)
% ~~~~~~~~~~~~~~~~~~~~~
% The free variable _NewContext_ is built/copied from _Context_ where
% the value of the result property is replaced with the given _NewResult_.
% This is a 'setter' of the result attribute of a context object.
% */
% new_result(Context, NewResult, NewContext) :- replace(result, Context, NewResult, NewContext).

/** @pred add_test_result_(+ _Context_, ? _NewTestResult_, ? _NewContext_)
Predicate that succeeds when the given _Context_ is unifiable with a
context record WITHOUT a test_result property and _NewContext_ is an extension
of _Context_ with a the test_result attribute with data unified with the
_NewTestResult_ parameter term.

Uses context generic 'add' predicate applied to test_result.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
add_test_result(+ _Context_, + _NewTestResult_, - _NewContext_)
~~~~~~~~~~~~~~~~~~~~~
The free variable _NewContext_ is built/copied from _Context_ where the value
of the test_result property is added at the head with the given _NewTestResult_
as the attribute data.
This is an optional extension of the test_result attribute of a context object
without test_result.
*/
add_test_result(Context, NewTestResult, NewContext) :- add(test_result, Context, NewTestResult, NewContext).

/** @pred pop_test_result_(+ _Context_, ? _TestResult_, ? _NewContext_)
Predicate that succeeds when the given _Context_ is unifiable with a
context record with a test_result property, with data unified with the
_TestResult_ parameter variable, and _NewContext_ is a subset
of _Context_ WITHOUT the test_result attribute.

Uses context generic 'pop' predicate applied to test_result.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
pop_test_result(+ _Context_, - _TestResult_, - _NewContext_)
~~~~~~~~~~~~~~~~~~~~~
The free variable _NewContext_ is built/copied from _Context_ where the value
of the test_result property is removed and the attribute data in said
test_result is unified with the given variable _TestResult_.
This is an attribute removal method of the test_result attribute of a context
object with test_result.
*/
% pop_test_result(Context, TestResult, NewContext) :- pop(test_result, Context, TestResult, NewContext).

/** @pred suite_result_(+ _Context_, ? _SuiteResult_)
Predicate that succeeds when the given _Context_ is unifiable with a
context record with a suite_result property and _SuiteResult_ is unifiable with
said suite_result property.

Uses context generic 'get' predicate applied to suite_result.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
suite_result(+ _Context_, - _SuiteResult_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the _SuiteResult_ parameter is bound to the suite_result
property of the given _Context_.
This is a 'getter' of the suite_result attribute of a context object.
*/
suite_result(Context, SuiteResult) :- get(suite_result, Context, SuiteResult).

/** @pred add_suite_result_(+ _Context_, ? _NewTestResult_, ? _NewContext_)
Predicate that succeeds when the given _Context_ is unifiable with a
context record WITHOUT a suite_result property and _NewContext_ is an extension
of _Context_ with a the suite_result attribute with data unified with the
_NewTestResult_ parameter term.

Uses context generic 'add' predicate applied to suite_result.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
add_suite_result(+ _Context_, + _NewTestResult_, - _NewContext_)
~~~~~~~~~~~~~~~~~~~~~
The free variable _NewContext_ is built/copied from _Context_ where the value
of the suite_result property is added at the head with the given _NewTestResult_
as the attribute data.
This is an optional extension of the suite_result attribute of a context object
without suite_result.
*/
add_suite_result(Context, TestResult, NewContext) :- add(suite_result, Context, TestResult, NewContext).


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
increment_size(Context, NewContext) :-
  get(size_step, Context, Step),
  increment_size(Context, NewContext, Step)
  .
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

/** @pred size_step_(+ _Context_, ? _SizeStep_)
Predicate that succeeds when the given _Context_ is unifiable with a
context record with a size_step property and _SizeStep_ is unifiable with said
size_step property.

Uses context generic 'get' predicate applied to size_step.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
size_step(+ _Context_, - _SizeStep_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the _SizeStep_ parameter is bound to the size_step property of
the given _Context_.
This is a 'getter' of the size_step attribute of a context object.
*/
size_step(Context, SizeStep) :- get(size_step, Context, SizeStep).


/** @pred tries_calc_predicate_(+ _Context_, ? _TriesCalcPredicate_)
Predicate that succeeds when the given _Context_ is unifiable with a context
record with a tries_calc_predicate property and _TriesCalcPredicate_ is
unifiable with said tries_calc_predicate property.

Uses context generic 'get' predicate applied to tries_calc_predicate.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
tries_calc_predicate(+ _Context_, - _TriesCalcPredicate_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the _TriesCalcPredicate_ parameter is bound to the
tries_calc_predicate property of the given _Context_.
This is a 'getter' of the tries_calc_predicate attribute of a context object.
*/
tries_calc_predicate(Context, TriesCalcPredicate) :-
  get(tries_calc_predicate, Context, TriesCalcPredicate).

/** @pred num_tests_(+ _Context_, ? _NumTests_)
Predicate that succeeds when the given _Context_ is unifiable with a context
record with a num_tests property and _NumTests_ is
unifiable with said num_tests property.

Uses context generic 'get' predicate applied to num_tests.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
num_tests(+ _Context_, - _NumTests_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the _NumTests_ parameter is bound to the
num_tests property of the given _Context_.
This is a 'getter' of the num_tests attribute of a context object.
*/
num_tests(Context, NumTests) :-
  get(num_tests, Context, NumTests).


/** @pred parse(+ _UserOptions_, ? _Context_)

Predicate that succeeds when the given _UserOptions_ is unifiable with a list
of context record attributes and _Context_ is unifiable with a context record
built from the default and where each element of _UserOptions_ replaced the
defaults.

Defaults are kept if there is no option to replace it and if multiple options
refer to the same context attribute the last one will be used.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
parse(+ _UserOptions_, ? _Context_)
~~~~~~~~~~~~~~~~~~~~~
*/
% parse(UserOptions, Context) :-
%   init(ContextInit),
%   parse(UserOptions, ContextInit, Context)
% .

/** @pred parse(+ _UserOptions_, + _ContextAcc_, ? _Context_)

Predicate that succeeds when the given _UserOptions_ is unifiable with a list
of context record attributes and _Context_ is unifiable with a context record
built from the context accumulator _ContextAcc_ and where each element of
_UserOptions_ replaced the defaults.

Defaults are kept if there is no option to replace it and if multiple options
refer to the same context attribute the last one will be used.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
parse(+ _UserOptions_, + _ContextAcc_, - _Context_)
~~~~~~~~~~~~~~~~~~~~~

When the _UserOptions_ is nil, the result context is the accumulator context.

When there is an user option, that option data replaces the corresponding
attribute in the context accumulator resulting in an intermediate context
accumulator. The intermediate accumulator is recursively passed to the parse
of the rest of the user options (list tail).
*/
% parse([], Context, Context).
% parse([ {AttrName, AttrData} | UserOptions ], ContextAcc, Context) :-
%   replace(AttrName, ContextAcc, AttrData, ContextAcc2),
%   parse(UserOptions, ContextAcc2, Context)
% .

/** @section SuiteData (Number of tests performed context attribute) */

/** @pred tests_passed_(+ _Context_, ? _TestsPassed_)
Predicate that succeeds when the given _Context_ is unifiable with a context
record with a tests_passed property and _TestsPassed_ is unifiable with said
tests_passed property.

Uses context generic 'get' predicate applied to tests_passed.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
tests_passed(+ _Context_, - _TestsPassed_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the _TestsPassed_ parameter is bound to the tests_passed
property of the given _Context_.
This is a 'getter' of the tests_passed attribute of a context object.
*/
%tests_passed(Context, TestsPassed) :- get(tests_passed, Context, TestsPassed).

/** @pred add_tests_passed_(+ _Context_, ? _NewTestsPassed_, ? _NewContext_)
Predicate that succeeds when the given _Context_ is unifiable with a
context record WITHOUT a tests_passed property and _NewContext_ is an extension
of _Context_ with a the tests_passed attribute with data unified with the
_NewTestsPassed_ parameter term.

Uses context generic 'add' predicate applied to tests_passed.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
add_tests_passed(+ _Context_, + _NewTestsPassed_, - _NewContext_)
~~~~~~~~~~~~~~~~~~~~~
The free variable _NewContext_ is built/copied from _Context_ where
the value of the tests_passed property is added at the head with the given
_NewTestsPassed_ as the attribute data.
This is an optional extension of the tests_passed attribute of a context object
without tests_passed.
*/
%add_tests_passed(Context, TestsPassed, NewContext) :- add(tests_passed, Context, TestsPassed, NewContext).
/** @section TestsPassed (Number of tests performed context attribute) */

/** @pred tests_tried_(+ _Context_, ? _TestsTried_)
Predicate that succeeds when the given _Context_ is unifiable with a context
record with a tests_tried property and _TestsTried_ is unifiable with said
tests_tried property.

Uses context generic 'get' predicate applied to tests_tried.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
tests_tried(+ _Context_, - _TestsTried_)
~~~~~~~~~~~~~~~~~~~~~
The free variable in the _TestsTried_ parameter is bound to the tests_tried
property of the given _Context_.
This is a 'getter' of the tests_tried attribute of a context object.
*/
%tests_tried(Context, TestsTried) :- get(tests_tried, Context, TestsTried).

/** @pred add_tests_tried_(+ _Context_, ? _NewTestsTried_, ? _NewContext_)
Predicate that succeeds when the given _Context_ is unifiable with a
context record WITHOUT a tests_tried property and _NewContext_ is an extension
of _Context_ with a the tests_tried attribute with data unified with the
_NewTestsTried_ parameter term.

Uses context generic 'add' predicate applied to tests_tried.

Intended use:
~~~~~~~~~~~~~~~~~~~~~{.prolog}
add_tests_tried(+ _Context_, + _NewTeststried_, - _NewContext_)
~~~~~~~~~~~~~~~~~~~~~
The free variable _NewContext_ is built/copied from _Context_ where
the value of the tests_tried property is added at the head with the given
_NewTestsTried_ as the attribute data.
This is an optional extension of the tests_tried attribute of a context object
without tests_tried.
*/
%add_tests_tried(Context, TestsTried, NewContext) :- add(tests_tried, Context, TestsTried, NewContext).

/** @section Dependency Abstraction (references for modular functions ontext attributes) */
binder(Context, Binder) :-
  get(bindings, Context, Bindings),
  generic_records:get(bindings, binder, {bindings, Bindings}, Binder).
shrinker(Context, Shrinker) :-
  get(bindings, Context, Bindings),
  generic_records:get(bindings, shrinker, {bindings, Bindings}, Shrinker).
generator(Context, Generator) :-
  get(bindings, Context, Bindings),
  generic_records:get(bindings, generator, {bindings, Bindings}, Generator).


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
%bindings(Context, Bindings) :- get(bindings, Context, Bindings).

% reset_bindings_trace(Context, Trace, OutContext) :-
%   get(bindings, Context, Bindings),
%   % deceive context get method to retrieve trace from the bindings data
%   get(trace, {ctx, Bindings}, Trace-[]),
%   % deceive context replace method to replace trace from the bindings data
%   replace(trace, {ctx, Bindings}, L-L, {ctx, ResetBindings}),
%   replace(bindings, Context, ResetBindings, OutContext)
% .


% % TODO rework bindings and move methods to appropriate modules
% new_bindings(Context, NewBindings, NewContext) :- replace(bindings, Context, NewBindings, NewContext).
%
% bind_forall(Module:Gen, Context, X, Size, OutContext) :-
%   writeln({'DEBUG', bind_for_all0, Module:Gen, X, TestDatum, Size}),
%   !, call(Module:Gen, X, TestDatum, Size),
%   writeln({'DEBUG', bind_for_all1, Module:Gen, X, TestDatum, Size}),
%   bindings(Context, Bindings),
%   writeln({'DEBUG', bind_for_all2, Module:Gen, X, TestDatum, Size}),
%   Bindings = [{trace, Binds-Rest} | Tail],
%   Rest = [ TestDatum | NewRest],
%   NewBindings = [{trace, Binds-NewRest} | Tail],
%   new_bindings(Context, NewBindings, OutContext)
% .
% bind_forall(Gen, Context, Var, Size, OutContext) :-
%   writeln({'DEBUG', bind_for_all, no_mod}),
%   context:module(Context, Module),
%   bind_forall(Module:Gen, Context, Var, Size, OutContext)
% .


%times_five(N,M) :- M is N * 5.

/** @} */
