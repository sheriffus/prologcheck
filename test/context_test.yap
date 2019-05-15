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
* Last rev: 2019/04/22
* mods:
* comments: PrologCheck runtime state of executing properties
*
*************************************************************************/

:- reconsult('../src/context.yap').

context_test(Attribute, Default, NewValue) :- (
  writeln({Attribute, tests}),
  writeln({default, Attribute, Default}),
  writeln({newvalue, Attribute, NewValue}),
  context:default(Context),
  call(context:Attribute, Context, AttrValue1),
  % context:module(Context, Module1),
  writeln({Attribute, 1, AttrValue1}),
  AttrValue1 == Default,
  atom_concat(new_, Attribute, New_Attribute),
  call(context:New_Attribute, Context, NewValue, NewContext),
  % context:new_module(Context, newmodule, NewContext),
  call(context:Attribute, NewContext, AttrValue2),
  % context:module(NewContext, Module2),
  writeln({Attribute, 2, AttrValue2}),
  AttrValue2 == NewValue,
  nl,nl,writeln({Attribute, tests, 'PASSED'}),nl,nl
  ) ;
  nl,nl,writeln({Attribute, tests, 'FAILED'}),nl,nl
.

:- context_test(module, plqc, newmodule).

:- context_test(size, 1, 3).

check_size(Context, ExpectedSize) :-
  context:size(Context, Size),
  writeln({size, context, Size}),
  writeln({size, expected, ExpectedSize}),
  Size == ExpectedSize
.

:- (
  writeln({size, increment, tests}),
  context:default(Context1),
  check_size(Context1, 1),
  context:increment_size(Context1, Context2),
  check_size(Context2, 2),
  context:increment_size(Context2, Context3),
  check_size(Context3, 3),
  context:increment_size(Context2, Context4, 2),
  check_size(Context4, 4),
  context:increment_size(Context1, Context5, 4),
  check_size(Context5, 5),
  nl,nl,writeln({size, increment_size, tests, 'PASSED'}),nl,nl
  ) ;
  nl,nl,writeln({size, increment_size, tests, 'FAILED'}),nl,nl
.
