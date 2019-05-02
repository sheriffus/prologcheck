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

:- (
  writeln({run, leaf, tests}),
  % build context and check there is no result
  context:default(Context),
  writeln({context, default, no_result}),
  not(context:result(Context,_Res)),
  writeln({run, leaf, goal}),
  % test a successful goal
  plqc:run((X is 1+2, X == 3), Context, OutContext1),
  writeln({context, default, result}),
  context:result(OutContext1,Result1),
  writeln({result1, Result1}),
  result:raw(Result1, RawResult1),
  RawResult1 == pass,
  result:reason(Result1, Reason1),
  Reason1 == true_goal,
  % test a failing goal
  plqc:run((X is 1+2, X == 4), Context, OutContext2),
  writeln({context, default, result}),
  context:result(OutContext2,Result2),
  writeln({result2, Result2}),
  result:raw(Result2, RawResult2),
  RawResult2 == fail,
  result:reason(Result2, Reason2),
  Reason2 == false_goal,
  nl,nl,writeln({run, leaf, tests, 'PASSED'}),nl,nl
  ) ;
  nl,nl,writeln({run, leaf, tests, 'FAILED'}),nl,nl
.

:- (
  writeln({run, module, tests}),
  context:default(Context),
  writeln({context, default, no_result}),
  not(context:result(Context,_Res)),
  writeln({load, module, testprops}),
  consult(testprops),
  % module true goal
  writeln({run, module, tgoal}),
  plqc:run(testprops:successfull_goal, Context, OutContext1),
  writeln({context, ok, result}),
  context:result(OutContext1,Result1),
  writeln({result1, Result1}),
  result:raw(Result1, RawResult1),
  RawResult1 == pass,
  result:reason(Result1, Reason1),
  Reason1 == true_goal,
  % module fail goal
  writeln({run, module, fgoal}),
  plqc:run(testprops:failing_goal, Context, OutContext2),
  writeln({context, nok, result}),
  context:result(OutContext2,Result2),
  writeln({result2, Result2}),
  result:raw(Result2, RawResult2),
  RawResult2 == fail,
  result:reason(Result2, Reason2),
  Reason2 == false_goal,
  nl,nl,writeln({run, module, tests, 'PASSED'}),nl,nl
  ) ;
  nl,nl,writeln({run, module, tests, 'FAILED'}),nl,nl
.

:- (
  writeln({run, labelled, tests}),
  context:default(Context),
  writeln({context, default, no_result}),
  not(context:result(Context,_Res)),
  writeln({load, module, testprops}),
  consult(testprops),
  % module true goal
  writeln({run, labelled, tgoal}),
  plqc:run(testprops:prop(successfull_prop), Context, OutContext1),
  writeln({context, ok, result}),
  context:result(OutContext1,Result1),
  writeln({result1, Result1}),
  result:raw(Result1, RawResult1),
  RawResult1 == pass,
  result:reason(Result1, Reason1),
  Reason1 == true_goal,
  % module fail goal
  writeln({run, labelled, fgoal}),
  plqc:run(testprops:prop(failing_prop), Context, OutContext2),
  writeln({context, nok, result}),
  context:result(OutContext2,Result2),
  writeln({result2, Result2}),
  result:raw(Result2, RawResult2),
  RawResult2 == fail,
  result:reason(Result2, Reason2),
  Reason2 == false_goal,
  nl,nl,writeln({run, labelled, tests, 'PASSED'}),nl,nl
  ) ;
  nl,nl,writeln({run, labelled, tests, 'FAILED'}),nl,nl
.
