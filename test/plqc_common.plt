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
* File:     plqc_common_test.yap
* Last rev: 2019/06/06
* mods:
* comments: PrologCheck shared helper and auxiliary predicates module
*
*************************************************************************/
:- use_module('../src/plqc_common.yap').

:- begin_tests(plqc_common).

test(member_head) :-
  member(a, [a,b,c]).
test(member_last) :-
  member(c, [a,b,c]).
test(member_middle) :-
  member(b, [a,b,c]).
test(member_singleton) :-
  member(a, [a]).
test(member_many) :-
  member(b, [a,b,c,b,d]).
test(member_var_in_ground, [fail]) :-
  member(_X, [a,b,c]).
test(member_var) :-
  member(X, [a,X,c]).
test(member_var_other_var, [fail]) :-
  member(_X, [a,_Y,c]).
test(member_of_var_before) :-
  member(a, [a,_X,c]).
test(member_of_var_after) :-
  member(c, [a,_X,c]).
test(member_not, [fail]) :-
  member(d, [a,b,c]).
test(member_singleton_not, [fail]) :-
  member(b, [a]).
test(member_empty_not, [fail]) :-
  member(a, []).
test(member_of_var_not, [fail]) :-
  member(a, [_X,b,c]).

:- end_tests(plqc_common).

% test_plqc_write(VerbosityLevel, VerbosityTermPairs, Printer, TestName, ExpectedResult)
% test_plqc_top(Property, TestName, ExpectedResult) :- (
%   writeln({test_plqc_top, TestName, property, (Property)}),
%   plqc:prologcheck_result(Property, [], Result),
%   writeln({test_plqc_top, TestName, result, Result}),
%     % tracing results
%   nl,nl,writeln({run, TestName, tests, 'PASSED'}),nl,nl
%   ) ;
%   nl,nl,writeln({run, TestName, tests, 'FAILED'}),nl,nl
% .
%
% % :- test_plqc_top((X is 1+2, X == 3), leaf1, {pass,true_goal}).
% % :- test_plqc_top((X is 1+2, X == 4), leaf1, {pass,true_goal}).
%
% :- consult(testprops), consult(testgens), test_plqc_top(testprops:succ_univ_quant_sized, module1, {pass,true_goal}).
% :- consult(testprops), consult(testgens), test_plqc_top(testprops:fail_univ_quant_sized, module2, {fail,false_goal}).
%
% test_member(X, [X | _XS]).
% member(X, [Y | XS]) :- X \= Y, member(X,XS).
%
% verbosity_levels([quiet,info,verbose,very_verbose,debug]).
%
% verbosity_check(RunLevel,CallLevel,[]) :-
%   writeln({'INVALID_VERBOSITY', run_lvl, RunLevel, call_lvl, CallLevel}), % TODO make error
%   fail
% .
% verbosity_check(_RunLevel,CallLevel,[CallLevel | _]) :- !.
% verbosity_check(RunLevel,_CallLevel,[RunLevel | _]) :- !, fail.
% verbosity_check(RunLevel,CallLevel,[_ | Lvls]) :- !, verbosity_check(RunLevel,CallLevel,Lvls).
%
% verbosity_check(RunLevel,CallLevel) :-
%   verbosity_levels(VerbosityLevels),
%   verbosity_check(RunLevel,CallLevel,VerbosityLevels)
% .
%
%
% plqc_write_term(VerbosityLevel, Verbosity, Term) :-
%   verbosity_check(VerbosityLevel, Verbosity), !,
%   writeln(Term).
% plqc_write_term(_VerbosityLevel, _Verbosity, _Term).
%
% plqc_write(_VerbosityLevel, []).
