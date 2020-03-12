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
* Last rev: 2020/03/12
* mods:
* comments: PrologCheck runtime state of executing properties
*
*************************************************************************/

:- use_module("../src/result.pl").
:- use_module("../src/plqc_common.pl").

% :- (
%   writeln({result, tests}),
%   writeln({result, pass, tests}),
%   TestReason1 = true_goal,
%   TestRaw1 = pass,
%   atom_concat(test_, TestRaw1, ResultTestRaw1),
%   result:create_result(ResultTestRaw1, {reason, TestReason1}, Result1),
%   writeln({result1, Result1}),
%   result:reason(Result1, Reason1),
%   writeln({reason1, Reason1}),
%   Reason1 == TestReason1,
%   result:raw(Result1, Raw1),
%   writeln({raw1, Raw1}),
%   Raw1 == TestRaw1,
%   writeln({result, fail, tests}),
%   TestReason2 = false_goal,
%   TestRaw2 = fail,
%   atom_concat(test_, TestRaw2, ResultTestRaw2),
%   result:create_result(ResultTestRaw2, {reason, TestReason2}, Result2),
%   writeln({result2, Result2}),
%   result:reason(Result2, Reason2),
%   writeln({reason2, Reason2}),
%   Reason2 == TestReason2,
%   result:raw(Result2, Raw2),
%   writeln({raw2, Raw2}),
%   Raw2 == TestRaw2,
%   nl,nl,writeln({result, tests, 'PASSED'}),nl,nl
%   ) ;
%   nl,nl,writeln({result, tests, 'FAILED'}),nl,nl
% .

:- begin_tests(result_init).

create_result_test(RawResultPattern, Reason, Result, OutLevel, OutRaw, OutReason) :-
  result:create_result(RawResultPattern, {reason, Reason}, Result),
  result:get(level, Result, OutLevel),
  result:get(raw, Result, OutRaw),
  result:get(reason, Result, OutReason)
.
%
test(create_test_pass, [true({OutLevel, OutRaw, OutReason} =@= {test_case, pass, Reason})]) :-
  RawResultPattern = test_pass,
  Reason = true_goal,
  create_result_test(RawResultPattern, Reason, _Result, OutLevel, OutRaw, OutReason)
.
test(create_test_fail, [true({OutLevel, OutRaw, OutReason} =@= {test_case, fail, Reason})]) :-
  RawResultPattern = test_fail,
  Reason = false_goal,
  create_result_test(RawResultPattern, Reason, _Result, OutLevel, OutRaw, OutReason)
.
test(create_test_precond_fail, [true({OutLevel, OutRaw, OutReason} =@= {test_case, error, Reason})]) :-
  RawResultPattern = test_precond_fail,
  Reason = cant_satisfy,
  create_result_test(RawResultPattern, Reason, _Result, OutLevel, OutRaw, OutReason)
.
% create_result(test_pass, {reason, true_goal}, Result) :- Result = {result, [{level, test_case}, {raw, pass}, {reason, true_goal}]}.
% create_result(test_fail, {reason, false_goal}, Result) :- Result = {result, [{level, test_case}, {raw, fail}, {reason, false_goal}]}.
% create_result(test_precond_fail, {reason, cant_satisfy}, Result) :- !, Result = {result, [{level, test_case}, {raw, error}, {reason, cant_satisfy}]}.
%
create_result_suite(RawResultPattern, TestsPassed, Result, OutLevel, OutRaw, OutReason, OutTestsPassed) :-
  result:create_result(RawResultPattern, {tests_passed, TestsPassed}, Result),
  result:get(level, Result, OutLevel),
  result:get(raw, Result, OutRaw),
  result:get(reason, Result, OutReason),
  result:get(tests_passed, Result, OutTestsPassed)
.
%
test(create_suite_tries_no_sat, [true({OutLevel, OutRaw, OutReason, OutTestsPassed} =@= {test_suite, error, Reason, TestsPassed})]) :-
  RawResultPattern = suite_try_limit,
  Reason = cant_satisfy,
  TestsPassed = 0,
  create_result_suite(RawResultPattern, TestsPassed, _Result, OutLevel, OutRaw, OutReason, OutTestsPassed)
.
test(create_suite_tries_some_sat, [true({OutLevel, OutRaw, OutReason, OutTestsPassed} =@= {test_suite, pass_partial, Reason, TestsPassed})]) :-
  RawResultPattern = suite_try_limit,
  Reason = max_tries_reached,
  TestsPassed = 1,
  create_result_suite(RawResultPattern, TestsPassed, _Result, OutLevel, OutRaw, OutReason, OutTestsPassed)
.
test(create_suite_pass, [true({OutLevel, OutRaw, OutReason, OutTestsPassed} =@= {test_suite, pass, Reason, TestsPassed})]) :-
  RawResultPattern = suite_pass,
  Reason = suite_complete,
  TestsPassed = 10,
  create_result_suite(RawResultPattern, TestsPassed, _Result, OutLevel, OutRaw, OutReason, OutTestsPassed)
.
%
create_result_suite_ce(RawResultPattern, TestsPassed, CounterExample, Result, OutLevel, OutRaw, OutReason, OutTestsPassed, OutCounterExample) :-
  % plqc_common:plqc_write([{info, 'DEBUG result plt (create_result_suite_ce/8). '}
  %   , {info, [indent, indent], RawResultPattern}
  % ]),
  result:create_result(RawResultPattern, [{tests_passed, TestsPassed}, {counterexample, CounterExample}], Result),
  % plqc_common:plqc_write([{info, [indent, indent], Result}
  % ]),
  result:get(level, Result, OutLevel),
  result:get(raw, Result, OutRaw),
  result:get(reason, Result, OutReason),
  result:get(tests_passed, Result, OutTestsPassed),
  result:get(counterexample, Result, OutCounterExample)
.
%
test(create_suite_fail, [true({OutLevel, OutRaw, OutReason, OutTestsPassed, OutCounterExample} =@= {test_suite, fail, Reason, TestsPassed, CounterExample})]) :-
  RawResultPattern = suite_fail,
  Reason = counterexample_found,
  TestsPassed = 10,
  CounterExample = {some, counterexample, term, []},
  create_result_suite_ce(RawResultPattern, TestsPassed, CounterExample, _Result, OutLevel, OutRaw, OutReason, OutTestsPassed, OutCounterExample)
.
test(create_suite_w_evidence, [true({OutLevel, OutRaw, OutReason, OutTestsPassed, OutCounterExample} =@= {test_suite, pass, Reason, TestsPassed, CounterExample})]) :-
  RawResultPattern = suite_evidence_found,
  Reason = failing_evidence_found,
  TestsPassed = 10,
  CounterExample = {some, counterexample, term, []},
  create_result_suite_ce(RawResultPattern, TestsPassed, CounterExample, _Result, OutLevel, OutRaw, OutReason, OutTestsPassed, OutCounterExample)
.
%%
test(create_suite_wo_evidence, [true({OutLevel, OutRaw, OutReason, OutTestsPassed} =@= {test_suite, fail, Reason, TestsPassed})]) :-
  RawResultPattern = suite_evidence_not_found,
  Reason = failing_evidence_not_found,
  TestsPassed = 10,
  create_result_suite(RawResultPattern, TestsPassed, _Result, OutLevel, OutRaw, OutReason, OutTestsPassed)
.
:- end_tests(result_init).

%% -----------------------------------------------------------------------
%% -----------------------------------------------------------------------

:- begin_tests(result_access).
fold_attrs_get_data([Attr], Result, {Datum}) :-
  !,
  call(result:Attr, Result, Datum)
.
fold_attrs_get_data([Attr|AttrTail], Result, {Datum, TailDatum}) :-
  call(result:Attr, Result, Datum),
  fold_attrs_get_data(AttrTail, Result, {TailDatum})
.
access_result_kinded_(RawResultPattern, Kind, KindedTerm, Level, Result, OutTerm) :-
  % plqc_common:plqc_write([{debug, 'DEBUG result (access_result_kinded_/6). '}
  %   , {debug, [indent, indent], RawResultPattern}
  %   , {debug, [indent, indent], {Kind, KindedTerm}}
  %   , {debug, [indent, indent], Level}
  %
  % ]),
  create_result(RawResultPattern, {Kind, KindedTerm}, Result),
  findall(Attr, (result:result_attributes(Level, Attr)), AttrL),
  fold_attrs_get_data(AttrL, Result, OutTerm)
.
access_result_test(RawResultPattern, Reason, Level, Result, OutTerm) :-
  access_result_kinded_(RawResultPattern, reason, Reason, Level, Result, OutTerm)
.
%
test(access_test_pass, [true(OutTerm =@= {Level, Reason, pass})]) :-
  Level = test_case,
  RawResultPattern = test_pass,
  Reason = true_goal,
  access_result_test(RawResultPattern, Reason, Level, _Result, OutTerm)
.
test(access_test_fail, [true(OutTerm =@= {Level, Reason, fail})]) :-
  Level = test_case,
  RawResultPattern = test_fail,
  Reason = false_goal,
  access_result_test(RawResultPattern, Reason, Level, _Result, OutTerm)
.
test(access_test_precond_fail, [true(OutTerm =@= {test_case, Reason, error})]) :-
  Level = test_case,
  RawResultPattern = test_precond_fail,
  Reason = cant_satisfy,
  access_result_test(RawResultPattern, Reason, Level, _Result, OutTerm)
.
%
access_result_suite(RawResultPattern, TestsPassed, Level, Result, OutTerm) :-
  access_result_kinded_(RawResultPattern, tests_passed, TestsPassed, Level, Result, OutTerm)
.
%
test(access_suite_tries_no_sat, [true(OutTerm =@= {test_suite, error, Reason, TestsPassed})]) :-
  Level = test_suite,
  RawResultPattern = suite_try_limit,
  Reason = cant_satisfy,
  TestsPassed = 0,
  access_result_suite(RawResultPattern, TestsPassed, Level, _Result, OutTerm)
.
test(access_suite_tries_some_sat, [true(OutTerm =@= {test_suite, pass_partial, Reason, TestsPassed})]) :-
  Level = test_suite,
  RawResultPattern = suite_try_limit,
  Reason = max_tries_reached,
  TestsPassed = 1,
  access_result_suite(RawResultPattern, TestsPassed, Level, _Result, OutTerm)
.
test(access_suite_pass, [true(OutTerm =@= {Level, pass, Reason, TestsPassed})]) :-
  Level = test_suite,
  RawResultPattern = suite_pass,
  Reason = suite_complete,
  TestsPassed = 10,
  access_result_suite(RawResultPattern, TestsPassed, Level, _Result, OutTerm)
.
%
% access_result_suite_ce(RawResultPattern, TestsPassed, CounterExample, Result, OutLevel, OutRaw, OutReason, OutTestsPassed, OutCounterExample) :-
%   access_result_kinded_(RawResultPattern, tests_passed, TestsPassed, Level, Reason, Result, OutTerm)
%   % plqc_common:plqc_write([{info, 'DEBUG result plt (create_result_suite_ce/8). '}
%   %   , {info, [indent, indent], RawResultPattern}
%   % ]),
%   result:create_result(RawResultPattern, [{tests_passed, TestsPassed}, {counterexample, CounterExample}], Result),
%   % plqc_common:plqc_write([{info, [indent, indent], Result}
%   % ]),
%   result:get(level, Result, OutLevel),
%   result:get(raw, Result, OutRaw),
%   result:get(reason, Result, OutReason),
%   result:get(tests_passed, Result, OutTestsPassed),
%   result:get(counterexample, Result, OutCounterExample)
% .
access_result_suite_ce(RawResultPattern, TestsPassed, CounterExample, Level, Reason, Result, OutTerm) :-
  % plqc_common:plqc_write([{debug, 'DEBUG result (access_result_kinded_/6). '}
  %   , {debug, [indent, indent], RawResultPattern}
  %   , {debug, [indent, indent], TestsPassed}
  %   , {debug, [indent, indent], CounterExample}
  %
  % ]),
  create_result(RawResultPattern, [{tests_passed, TestsPassed}, {counterexample, CounterExample}], Result),
  % plqc_common:plqc_write([
  %     {debug, [indent], create_result}
  %   , {debug, [indent, indent], Result}
  % ]),
  findall(Attr, (result:result_attributes(Level, Attr)), AttrL, AttrLRest),
  findall(Attr, (result:result_conditional_attributes(Level, Reason, Attr)), AttrLRest),
  % plqc_common:plqc_write([
  %     {debug, [indent], findall}
  %   , {debug, [indent, indent], AttrL}
  %   , {debug, [indent, indent], AttrLRest}
  % ]),
  fold_attrs_get_data(AttrL, Result, OutTerm)
.
%
test(access_suite_fail, [true(OutTerm =@= {Level, fail, Reason, TestsPassed, CounterExample})]) :-
  Level = test_suite,
  RawResultPattern = suite_fail,
  Reason = counterexample_found,
  TestsPassed = 10,
  CounterExample = {some, counterexample, term, []},
  %access_result_suite_ce(RawResultPattern, TestsPassed, CounterExample, _Result, OutLevel, OutRaw, OutReason, OutTestsPassed, OutCounterExample)
  access_result_suite_ce(RawResultPattern, TestsPassed, CounterExample, Level, Reason, _Result, OutTerm)
.
test(access_suite_w_evidence, [true(OutTerm =@= {Level, pass, Reason, TestsPassed, CounterExample})]) :-
  Level = test_suite,
  RawResultPattern = suite_evidence_found,
  Reason = failing_evidence_found,
  TestsPassed = 10,
  CounterExample = {some, counterexample, term, []},
  access_result_suite_ce(RawResultPattern, TestsPassed, CounterExample, Level, Reason, _Result, OutTerm)
.
%%
test(access_suite_wo_evidence, [true(OutTerm =@= {Level, fail, Reason, TestsPassed})]) :-
  Level = test_suite,
  RawResultPattern = suite_evidence_not_found,
  Reason = failing_evidence_not_found,
  TestsPassed = 10,
  access_result_suite(RawResultPattern, TestsPassed, Level, _Result, OutTerm)
.
:- end_tests(result_access).

%% -----------------------------------------------------------------------
%% -----------------------------------------------------------------------

% :- begin_tests(result_change).
%
% :- end_tests(result_change).
%
% %% -----------------------------------------------------------------------
% %% -----------------------------------------------------------------------
