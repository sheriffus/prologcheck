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
* File:     plqc_run_test.yap
* Last rev: 2019/05/15
* mods:
* comments: PrologCheck module single/pure test run;
*
*************************************************************************/

:- reconsult('../src/plqc_run.yap').

expectation(raw,pass,{pass,_}).
expectation(reason,true_goal,{_,true_goal}).
expectation(raw,fail,{fail,_}).
expectation(reason,false_goal,{_,false_goal}).


test_run(Prop, TestName, ExpectedResult) :- (
  writeln({run, TestName, tests}),
  % build context and check there is no result
  context:default(Context),
  writeln({context, default, no_result}),
  (\+ context:result(Context,_Res)),
  writeln({run, TestName, goal}),
  % test the prop
  plqc_run:run(Prop, Context, OutContext1),
  writeln({context, result}),
  context:result(OutContext1,Result1),
  writeln({result1, Result1}),
  result:raw(Result1, RawResult1),
  expectation(raw,RawResult1,ExpectedResult),
  result:reason(Result1, Reason1),
  expectation(reason,Reason1,ExpectedResult),
  % tracing results
  nl,nl,writeln({run, TestName, tests, 'PASSED'}),nl,nl
  ) ;
  nl,nl,writeln({run, TestName, tests, 'FAILED'}),nl,nl
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5


:- test_run((X is 1+2, X == 3), leaf1, {pass,true_goal}).
:- test_run((X is 1+2, X == 4), leaf2, {fail,false_goal}).

:- consult(testprops), test_run(testprops:successfull_goal, module1, {pass,true_goal}).
:- consult(testprops), test_run(testprops:failing_goal, module2, {fail,false_goal}).

:- consult(testprops), test_run(testprops:prop(successfull_prop), labelled1, {pass,true_goal}).
:- consult(testprops), test_run(testprops:prop(failing_prop), labelled2, {fail,false_goal}).

:- consult(testprops), consult(testgens), test_run(testprops:prop(succ_univ_quant), for_all1, {pass,true_goal}).
:- consult(testprops), consult(testgens), test_run(testprops:prop(fail_univ_quant), for_all2, {fail,false_goal}).
