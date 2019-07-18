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
* File:     plqc_common.plt
* Last rev: 2019/07/18
* mods:
* comments: PrologCheck shared helper and auxiliary predicates module plunit
*     test file
*
*************************************************************************/


:- use_module('../src/plqc_common.pl').
% :- use_module('../src/plqc_common.pl', [member/2]).
% :- use_module('../src/plqc_common.pl', [verbosity_check/2]).
% :- use_module('../src/plqc_common.pl', [plqc_write/2]).
% :- use_module('./plqc_test_aux.pl').

%% -----------------------------------------------------------------------
%% -----------------------------------------------------------------------

:- begin_tests(plqc_common_member).

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

:- end_tests(plqc_common_member).

%% ----------------------------------------------------------------------------
%% ----------------------------------------------------------------------------

:- begin_tests(plqc_common_verbosity_check2).

%% --- quiet run level ---
test(verbosity_check2_quiet_checks_only_quiet_quiet) :-
  verbosity_check(quiet, quiet).
test(verbosity_check2_quiet_checks_only_quiet_info, [fail]) :-
  verbosity_check(quiet, info).
test(verbosity_check2_quiet_checks_only_quiet_verbose, [fail]) :-
  verbosity_check(quiet, verbose).
test(verbosity_check2_quiet_checks_only_quiet_very_verbose, [fail]) :-
  verbosity_check(quiet, very_verbose).
test(verbosity_check2_quiet_checks_only_quiet_debug, [fail]) :-
  verbosity_check(quiet, debug).
%% --- debug run level ---
test(verbosity_check2_debug_checks_all_quiet) :-
  verbosity_check(debug, quiet).
test(verbosity_check2_debug_checks_all_info) :-
  verbosity_check(debug, info).
test(verbosity_check2_debug_checks_all_verbose) :-
  verbosity_check(debug, verbose).
test(verbosity_check2_debug_checks_all_very_verbose) :-
  verbosity_check(debug, very_verbose).
test(verbosity_check2_debug_checks_all_debug) :-
  verbosity_check(debug, debug).
%% --- X run level check is reflexive ---
test(verbosity_check2_reflexive_quiet) :-
  verbosity_check(quiet, quiet).
test(verbosity_check2_reflexive_info) :-
  verbosity_check(info, info).
test(verbosity_check2_reflexive_verbose) :-
  verbosity_check(verbose, verbose).
test(verbosity_check2_reflexive_very_verbose) :-
  verbosity_check(very_verbose, very_verbose).
test(verbosity_check2_reflexive_debug) :-
  verbosity_check(debug, debug).
%% --- X run level NOT checks lower priority X-1 level ---
test(verbosity_check2_level_not_checks_next_quiet_info, [fail]) :-
  verbosity_check(quiet, info).
test(verbosity_check2_level_not_checks_next_info_verbose, [fail]) :-
  verbosity_check(info, verbose).
test(verbosity_check2_level_not_checks_next_verbose_very_verbose, [fail]) :-
  verbosity_check(verbose, very_verbose).
test(verbosity_check2_level_not_checks_next_very_verbose_debug, [fail]) :-
  verbosity_check(very_verbose, debug).
%% --- X run level checks higher priority X+1 level ---
test(verbosity_check2_level_checks_prior_info_quiet) :-
  verbosity_check(info, quiet).
test(verbosity_check2_level_checks_prior_verbose_info) :-
  verbosity_check(verbose, info).
test(verbosity_check2_level_checks_prior_very_verbose_verbose) :-
  verbosity_check(very_verbose, verbose).
test(verbosity_check2_level_checks_prior_debug_very_verbose) :-
  verbosity_check(debug, very_verbose).
%% --- nonexisting run and call level ---
test(verbosity_check2_inexisting_call_error, [error(type_error({run_level:verbosity,call_level:verbosity},{quiet,not_a_verbosity}),_Context)]) :-
  verbosity_check(quiet, not_a_verbosity).
test(verbosity_check2_inexisting_run_error, [error(type_error({run_level:verbosity,call_level:verbosity},{not_a_verbosity,quiet}),_Context)]) :-
  verbosity_check(not_a_verbosity, quiet).

:- end_tests(plqc_common_verbosity_check2).

%% ----------------------------------------------------------------------------
%% ----------------------------------------------------------------------------

:- begin_tests(plqc_common_plqc_write2).

%% --- nonexisting run and call level ---
test(plqc_write2_not_a_level_run, [error(type_error({run_level:verbosity,call_level:verbosity},{not_a_verbosity, info}),_Context)]) :-
  plqc_write(not_a_verbosity, [{info, info_term}]).
test(plqc_write2_not_a_level_call, [error(type_error({run_level:verbosity,call_level:verbosity},{debug, not_a_verbosity}),_Context)]) :-
  plqc_write(debug, [{not_a_verbosity, not_to_print_term}]).
%% --- each run level with all call levels ---
%% --- debug run and each call level ---
test(plqc_write2_from_debug, true(ResultOutput =@= "quiet_term\ninfo_term\nverbose_term\nvery_verbose_term\ndebug_term\n")) :-
  with_output_to(string(ResultOutput),
      plqc_write(debug, [{quiet, quiet_term}, {info, info_term}, {verbose, verbose_term}, {very_verbose, very_verbose_term}, {debug, debug_term}])
  ).
%% --- very_verbose run and each call level ---
test(plqc_write2_from_very_verbose, true(ResultOutput =@= "quiet_term\ninfo_term\nverbose_term\nvery_verbose_term\n")) :-
  with_output_to(string(ResultOutput),
      plqc_write(very_verbose, [{quiet, quiet_term}, {info, info_term}, {verbose, verbose_term}, {very_verbose, very_verbose_term}, {debug, debug_term}])
  ).
%% --- verbose run and each call level ---
test(plqc_write2_from_verbose, true(ResultOutput =@= "quiet_term\ninfo_term\nverbose_term\n")) :-
  with_output_to(string(ResultOutput),
      plqc_write(verbose, [{quiet, quiet_term}, {info, info_term}, {verbose, verbose_term}, {very_verbose, very_verbose_term}, {debug, debug_term}])
  ).
%% --- info run and each call level ---
test(plqc_write2_from_info, true(ResultOutput =@= "quiet_term\ninfo_term\n")) :-
  with_output_to(string(ResultOutput),
      plqc_write(info, [{quiet, quiet_term}, {info, info_term}, {verbose, verbose_term}, {very_verbose, very_verbose_term}, {debug, debug_term}])
  ).
%% --- quiet run and each call level ---
test(plqc_write2_from_quiet, true(ResultOutput =@= "quiet_term\n")) :-
  with_output_to(string(ResultOutput),
      plqc_write(quiet, [{quiet, quiet_term}, {info, info_term}, {verbose, verbose_term}, {very_verbose, very_verbose_term}, {debug, debug_term}])
  ).
%% --- each run level with only OK call levels ---
%% --- debug run and each OK call level ---
test(plqc_write2_from_debug, true(ResultOutput =@= "quiet_term\ninfo_term\nverbose_term\nvery_verbose_term\ndebug_term\n")) :-
  with_output_to(string(ResultOutput),
      plqc_write(debug, [{quiet, quiet_term}, {info, info_term}, {verbose, verbose_term}, {very_verbose, very_verbose_term}, {debug, debug_term}])
  ).
%% --- very_verbose run and each OK call level ---
test(plqc_write2_from_very_verbose, true(ResultOutput =@= "quiet_term\ninfo_term\nverbose_term\nvery_verbose_term\n")) :-
  with_output_to(string(ResultOutput),
      plqc_write(very_verbose, [{quiet, quiet_term}, {info, info_term}, {verbose, verbose_term}, {very_verbose, very_verbose_term}])
  ).
%% --- verbose run and each OK call level ---
test(plqc_write2_from_verbose, true(ResultOutput =@= "quiet_term\ninfo_term\nverbose_term\n")) :-
  with_output_to(string(ResultOutput),
      plqc_write(verbose, [{quiet, quiet_term}, {info, info_term}, {verbose, verbose_term}])
  ).
%% --- info run and each OK call level ---
test(plqc_write2_from_info, true(ResultOutput =@= "quiet_term\ninfo_term\n")) :-
  with_output_to(string(ResultOutput),
      plqc_write(info, [{quiet, quiet_term}, {info, info_term}])
  ).
%% --- quiet run and each OK call level ---
test(plqc_write2_from_quiet, true(ResultOutput =@= "quiet_term\n")) :-
  with_output_to(string(ResultOutput),
      plqc_write(quiet, [{quiet, quiet_term}])
  ).
%% --- each run level with only NOT OK call levels ---
%% --- debug run and no call level ---
test(plqc_write2_from_debug, true(ResultOutput =@= "")) :-
  with_output_to(string(ResultOutput),
      plqc_write(debug, [])
  ).
%% --- very_verbose run and debug call level ---
test(plqc_write2_from_very_verbose, true(ResultOutput =@= "")) :-
  with_output_to(string(ResultOutput),
      plqc_write(very_verbose, [{debug, debug_term}])
  ).
%% --- verbose run and each NOT OK call level ---
test(plqc_write2_from_verbose, true(ResultOutput =@= "")) :-
  with_output_to(string(ResultOutput),
      plqc_write(verbose, [{very_verbose, very_verbose_term}, {debug, debug_term}])
  ).
%% --- info run and each  NOT OK call level ---
test(plqc_write2_from_info, true(ResultOutput =@= "")) :-
  with_output_to(string(ResultOutput),
      plqc_write(info, [{verbose, verbose_term}, {very_verbose, very_verbose_term}, {debug, debug_term}])
  ).
%% --- quiet run and all other call levels ---
test(plqc_write2_from_quiet, true(ResultOutput =@= "")) :-
  with_output_to(string(ResultOutput),
      plqc_write(quiet, [{info, info_term}, {verbose, verbose_term}, {very_verbose, very_verbose_term}, {debug, debug_term}])
  ).
%% --- each run level with reflexive call levels ---
%% --- debug run and debug call level ---
test(plqc_write2_from_debug, true(ResultOutput =@= "debug_term\n")) :-
  with_output_to(string(ResultOutput),
      plqc_write(debug, [{debug, debug_term}])
  ).
%% --- very_verbose run and very_verbose call level ---
test(plqc_write2_from_very_verbose, true(ResultOutput =@= "very_verbose_term\n")) :-
  with_output_to(string(ResultOutput),
      plqc_write(very_verbose, [{very_verbose, very_verbose_term}])
  ).
%% --- verbose run and verbose call level ---
test(plqc_write2_from_verbose, true(ResultOutput =@= "verbose_term\n")) :-
  with_output_to(string(ResultOutput),
      plqc_write(verbose, [{verbose, verbose_term}])
  ).
%% --- info run and info call level ---
test(plqc_write2_from_info, true(ResultOutput =@= "info_term\n")) :-
  with_output_to(string(ResultOutput),
      plqc_write(info, [{info, info_term}])
  ).
%% --- quiet run and quiet call level ---
test(plqc_write2_from_quiet, true(ResultOutput =@= "quiet_term\n")) :-
  with_output_to(string(ResultOutput),
      plqc_write(quiet, [{quiet, quiet_term}])
  ).
%% --- each run level with immediate higher level call  ---
%% --- debug run and very_verbose call level ---
test(plqc_write2_from_debug, true(ResultOutput =@= "very_verbose_term\n")) :-
  with_output_to(string(ResultOutput),
      plqc_write(debug, [{very_verbose, very_verbose_term}])
  ).
%% --- very_verbose run and verbose call level ---
test(plqc_write2_from_very_verbose, true(ResultOutput =@= "verbose_term\n")) :-
  with_output_to(string(ResultOutput),
      plqc_write(very_verbose, [{verbose, verbose_term}])
  ).
%% --- verbose run and info call level ---
test(plqc_write2_from_verbose, true(ResultOutput =@= "info_term\n")) :-
  with_output_to(string(ResultOutput),
      plqc_write(verbose, [{info, info_term}])
  ).
%% --- info run and quiet call level ---
test(plqc_write2_from_info, true(ResultOutput =@= "quiet_term\n")) :-
  with_output_to(string(ResultOutput),
      plqc_write(info, [{quiet, quiet_term}])
  ).
%% --- quiet run and no call ---
test(plqc_write2_from_quiet, true(ResultOutput =@= "")) :-
  with_output_to(string(ResultOutput),
      plqc_write(quiet, [])
  ).
%% --- each run level with immediate lower level call ---
%% --- debug run and no call  ---
test(plqc_write2_from_debug, true(ResultOutput =@= "")) :-
  with_output_to(string(ResultOutput),
      plqc_write(debug, [])
  ).
%% --- very_verbose run and debug call level ---
test(plqc_write2_from_very_verbose, true(ResultOutput =@= "")) :-
  with_output_to(string(ResultOutput),
      plqc_write(very_verbose, [{debug, debug_term}])
  ).
%% --- verbose run and very_verbose call level ---
test(plqc_write2_from_verbose, true(ResultOutput =@= "")) :-
  with_output_to(string(ResultOutput),
      plqc_write(verbose, [{very_verbose, very_verbose_term}])
  ).
%% --- info run and verbose call level ---
test(plqc_write2_from_info, true(ResultOutput =@= "")) :-
  with_output_to(string(ResultOutput),
      plqc_write(info, [{verbose, verbose_term}])
  ).
%% --- quiet run and info call level ---
test(plqc_write2_from_quiet, true(ResultOutput =@= "")) :-
  with_output_to(string(ResultOutput),
      plqc_write(quiet, [{info, info_term}])
  ).

:- end_tests(plqc_common_plqc_write2).
