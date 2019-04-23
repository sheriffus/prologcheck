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

:-
  writeln({module, tests}),
  context:default(Context),
  context:module(Context, Module1),
  writeln({module1, Module1}),
  Module1 == plqc,
  context:new_module(Context, newmodule, NewContext),
  context:module(NewContext, Module2),
  writeln({module2, Module2}),
  Module2 == newmodule,
  writeln({module, tests, ok})
.
