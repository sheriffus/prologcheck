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
* File:     context_test.yap
* Last rev: 2019/05/15
* mods:
* comments: PrologCheck runtime state of executing properties
*
*************************************************************************/

%:- reconsult('../src/.yap').
:- use_module("../src/context.pl").

%% -----------------------------------------------------------------------
%% -----------------------------------------------------------------------

:- begin_tests(context_init).

default_attr_test(Attr, ContextTerm, DefaultTerm) :-
  default_attr_test(Attr, Attr, ContextTerm, DefaultTerm).

default_attr_test(ContextAttr, ConfigAttr, ContextTerm, DefaultTerm) :-
  init(Context),
  call(ContextAttr, Context, ContextTerm),
  default(ConfigAttr, DefaultTerm)
.
chconf_attr_test(Attr, NewAttrTerm, ContextTerm, NewAttrTerm) :-
  chconf_attr_test(Attr, Attr, NewAttrTerm, ContextTerm, NewAttrTerm).

chconf_attr_test(ContextAttr, ConfigAttr, NewAttrTerm, ContextTerm, NewAttrTerm) :-
  call(context:configuration, ConfigAttr, OldContextTerm),
  retractall(context:configuration(ConfigAttr, OldContextTerm)),
  assert(context:configuration(ConfigAttr, NewAttrTerm)),
  init(Context),
  call(ContextAttr, Context, ContextTerm),
  retractall(context:configuration(ConfigAttr, NewAttrTerm)),
  assert(context:configuration(ConfigAttr, OldContextTerm))
.

test(init_succeeds) :-
  init(_Context).
test(init_module_default, [true(ContextTerm =@= DefaultTerm)]) :-
  default_attr_test(module, ContextTerm, DefaultTerm).
test(init_module_new_config, [true(ContextTerm =@= NewTerm)]) :-
  NewModule = new_test_module,
  chconf_attr_test(module, NewModule, ContextTerm, NewTerm).
test(init_size_default, [true(ContextTerm =@= DefaultTerm)]) :-
  default_attr_test(size, start_size, ContextTerm, DefaultTerm).
test(init_size_new_config, [true(ContextTerm =@= NewTerm)]) :-
  NewSize = -1,
  chconf_attr_test(size, start_size, NewSize, ContextTerm, NewTerm).
test(init_size_step_default, [true(ContextTerm =@= DefaultTerm)]) :-
  default_attr_test(size_step, ContextTerm, DefaultTerm).
test(init_size_step_new_config, [true(ContextTerm =@= NewTerm)]) :-
  NewSizeStep = -2,
  chconf_attr_test(size_step, NewSizeStep, ContextTerm, NewTerm).
test(init_num_tests_default, [true(ContextTerm =@= DefaultTerm)]) :-
  default_attr_test(num_tests, ContextTerm, DefaultTerm).
test(init_num_tests_new_config, [true(ContextTerm =@= NewTerm)]) :-
  NewNumTests = -3,
  chconf_attr_test(num_tests, NewNumTests, ContextTerm, NewTerm).
test(init_tries_calc_predicate_default, [true(ContextTerm =@= DefaultTerm)]) :-
  default_attr_test(tries_calc_predicate, ContextTerm, DefaultTerm).
test(init_tries_calc_predicate_new_config, [true(ContextTerm =@= NewTerm)]) :-
  NewTriesCalc = -3,
  chconf_attr_test(tries_calc_predicate, NewTriesCalc, ContextTerm, NewTerm).
test(init_expected_result_default, [true(ContextTerm =@= DefaultTerm)]) :-
  default_attr_test(expected_result, ContextTerm, DefaultTerm).
test(init_expected_result_new_config, [true(ContextTerm =@= NewTerm)]) :-
  NewExpectedResult = fail,
  chconf_attr_test(expected_result, NewExpectedResult, ContextTerm, NewTerm).
% TODO
% test(init_binding) :-
%   init(_Context).

:- end_tests(context_init).

%% -----------------------------------------------------------------------
%% -----------------------------------------------------------------------

:- begin_tests(context_change).

% context_test(Attribute, Default, NewValue) :- (
%   writeln({Attribute, tests}),
%   writeln({default, Attribute, Default}),
%   writeln({newvalue, Attribute, NewValue}),
%   context:init(Context),
%   call(context:Attribute, Context, AttrValue1),
%   % context:module(Context, Module1),
%   writeln({Attribute, 1, AttrValue1}),
%   AttrValue1 == Default,
%   atom_concat(new_, Attribute, New_Attribute),
%   call(context:New_Attribute, Context, NewValue, NewContext),
%   % context:new_module(Context, newmodule, NewContext),
%   call(context:Attribute, NewContext, AttrValue2),
%   % context:module(NewContext, Module2),
%   writeln({Attribute, 2, AttrValue2}),
%   AttrValue2 == NewValue,
%   nl,nl,writeln({Attribute, tests, 'PASSED'}),nl,nl
%   ) ;
%   nl,nl,writeln({Attribute, tests, 'FAILED'}),nl,nl
% .
context_test_new(Attribute, NewValue, DefaultTerm, AttrValue1, AttrValue2) :-
  context_test_new(Attribute, Attribute, NewValue, DefaultTerm, AttrValue1, AttrValue2).

context_test_new(ContextAttr, ConfigAttr, NewValue, DefaultTerm, AttrValue1, AttrValue2) :-
  default(ConfigAttr, DefaultTerm),
  init(Context),
  call(context:ContextAttr, Context, AttrValue1),
  atom_concat(new_, ContextAttr, New_ContextAttr),
  call(context:New_ContextAttr, Context, NewValue, NewContext),
  call(context:ContextAttr, NewContext, AttrValue2)
.

test(context_new_module, [true({ContextTerm1,ContextTerm2} =@= {DefaultTerm,NewModule})]) :-
  NewModule = newmodule,
  context_test_new(module, NewModule, DefaultTerm, ContextTerm1, ContextTerm2).
test(context_new_size, [true({ContextTerm1,ContextTerm2} =@= {DefaultTerm,NewSize})]) :-
  NewSize = 3,
  context_test_new(size, start_size, NewSize, DefaultTerm, ContextTerm1, ContextTerm2).
test(context_new_expected_result, [true({ContextTerm1,ContextTerm2} =@= {DefaultTerm,NewExpectedResult})]) :-
  NewExpectedResult = fail,
  context_test_new(expected_result, NewExpectedResult, DefaultTerm, ContextTerm1, ContextTerm2).
% test(context_add_test_result)
% test(context_add_suite_result)
% test(context_add_tests_passed)
% test(context_add_tests_tried)

:- end_tests(context_change).

%% -----------------------------------------------------------------------
%% -----------------------------------------------------------------------

:- begin_tests(context_size_mngt).

check_size(Context, ExpectedSize) :-
  context:size(Context, Size),
  writeln({size, context, Size}),
  writeln({size, expected, ExpectedSize}),
  Size == ExpectedSize
.

test(context_size_inc1, [true({Size1,Size2} =@= {1,2})]) :-
  context:init(Context1),
  context:size(Context1, Size1),
  context:increment_size(Context1, Context2),
  context:size(Context2, Size2)
.
test(context_size_inc2, [true({Size1,Size2,Size3} =@= {1,2,3})]) :-
  context:init(Context1),
  context:size(Context1, Size1),
  context:increment_size(Context1, Context2),
  context:size(Context2, Size2),
  context:increment_size(Context2, Context3),
  context:size(Context3, Size3)
.
test(context_size_inc1_plus_n, [true({Size1,Size2,Size4} =@= {1,2,4})]) :-
  context:init(Context1),
  context:size(Context1, Size1),
  context:increment_size(Context1, Context2),
  context:size(Context2, Size2),
  context:increment_size(Context2, Context4, 2),
  context:size(Context4, Size4)
.
test(context_size_inc_n, [true({Size1,Size5} =@= {1,5})]) :-
  context:init(Context1),
  context:size(Context1, Size1),
  context:increment_size(Context1, Context5, 4),
  context:size(Context5, Size5)
.


:- end_tests(context_size_mngt).
