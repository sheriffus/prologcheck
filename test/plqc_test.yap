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
  writeln({context, default, no_result}),
  not(context:result(Context,_Res)),
  writeln({run, leaf, goal}),
  plqc:run((X is 1+2, X == 3), Context, OutContext1),
  writeln({context, default, result}),
  context:result(OutContext1,Result1),
  writeln({result1, Result1}),
  result:raw(Result1, RawResult1),
  RawResult1 == pass,
  result:reason(Result1, Reason1),
  Reason1 == true_goal,
  writeln({run, leaf, tests, ok})
.
