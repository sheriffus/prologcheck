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
* Last rev: 2019/05/15
* mods:
* comments: PrologCheck module main file;
*           File to be included/consulted in prolog projects for property
*           based testing
*
*************************************************************************/

:- reconsult('../src/plqc.yap').

test_plqc_top(Property, TestName, ExpectedResult) :- (
  writeln({test_plqc_top, TestName, property, (Property)}),
  plqc:prologcheck_result(Property, [], Result),
  writeln({test_plqc_top, TestName, result, Result}),
    % tracing results
  nl,nl,writeln({run, TestName, tests, 'PASSED'}),nl,nl
  ) ;
  nl,nl,writeln({run, TestName, tests, 'FAILED'}),nl,nl
.

% :- test_plqc_top((X is 1+2, X == 3), leaf1, {pass,true_goal}).
% :- test_plqc_top((X is 1+2, X == 4), leaf1, {pass,true_goal}).

:- consult(testprops), consult(testgens), test_plqc_top(testprops:succ_univ_quant_sized, module1, {pass,true_goal}).
:- consult(testprops), consult(testgens), test_plqc_top(testprops:fail_univ_quant_sized, module2, {fail,false_goal}).
