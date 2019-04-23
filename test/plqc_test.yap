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
* File:     plqc_test.yap
* Last rev: 2019/04/23
* mods:
* comments: PrologCheck module main file;
*           File to be included/consulted in prolog projects for property
*           based testing
*
*************************************************************************/

:- reconsult('../src/plqc.yap').

:-
  writeln({run, leaf, tests}),
  context:default(Context),
  plqc:run((X is 1+2, X == 3), Context, _OutContext),
  % context:result(Context, ResultData1),
  % writeln({result1, ResultData1}),
  % plqc:result(ResultData1, Result1),
  % Result1 == pass,
  % plqc:result_reason(ResultData1, Result1Reason),
  % Result1Reason == true_prop,
  % context:new_module(Context, newmodule, NewContext),
  % context:module(NewContext, Module2),
  % writeln({module2, Module2}),
  % Module2 == newmodule,
  writeln({run, leaf, tests, ok})
.
