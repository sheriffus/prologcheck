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
* File:     result_test.yap
* Last rev: 2019/04/22
* mods:
* comments: PrologCheck runtime state of executing properties
*
*************************************************************************/

:- reconsult('../src/result.yap').

:- (
  writeln({result, tests}),
  writeln({result, pass, tests}),
  TestReason1 = true_goal,
  TestRaw1 = pass,
  atom_concat(test_, TestRaw1, ResultTestRaw1),
  result:create_result(ResultTestRaw1, {reason, TestReason1}, Result1),
  writeln({result1, Result1}),
  result:reason(Result1, Reason1),
  writeln({reason1, Reason1}),
  Reason1 == TestReason1,
  result:raw(Result1, Raw1),
  writeln({raw1, Raw1}),
  Raw1 == TestRaw1,
  writeln({result, fail, tests}),
  TestReason2 = false_goal,
  TestRaw2 = fail,
  atom_concat(test_, TestRaw2, ResultTestRaw2),
  result:create_result(ResultTestRaw2, {reason, TestReason2}, Result2),
  writeln({result2, Result2}),
  result:reason(Result2, Reason2),
  writeln({reason2, Reason2}),
  Reason2 == TestReason2,
  result:raw(Result2, Raw2),
  writeln({raw2, Raw2}),
  Raw2 == TestRaw2,
  nl,nl,writeln({result, tests, 'PASSED'}),nl,nl
  ) ;
  nl,nl,writeln({result, tests, 'FAILED'}),nl,nl
.
