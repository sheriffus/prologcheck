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
* File:     plqc_common.yap
* Last rev: 2019/06/06
* mods:
* comments: PrologCheck shared helper and auxiliary predicates module
*
*************************************************************************/


/** @defgroup PrologCheckCommon PrologCheck shared predicates
@ingroup PrologCheck
@{

PrologCheck internal module that implements miscelaneous helper predicates,
such as printing routines.

*/

/**
 * @file   plqc_common.yap
 * @author Claudio Amaral <coa@dcc.fc.up.pt>
 * @date   Mon July 17 14:55 2019
 *
 * @brief  PrologCheck shared helper and auxiliary predicates module.
 *
*/
:- module(plqc_common,
          [ member/2
          , verbosity_check/2
          , plqc_write/2
          , plqc_write/1 %% same as /2 with configured run verbosity
          , default_verbosity_level/1
          , configured_verbosity_level/1
          , update_verbosity_level/1
          , check_variable_W/2
          ]).

%% TODO - documentation

default_verbosity_level(info).
%default_verbosity_level(very_verbose).
%default_verbosity_level(debug).

:- dynamic configured_verbosity_level/1.

configured_verbosity_level(L) :- default_verbosity_level(L).

update_verbosity_level(L) :-
  verbosity_levels(VLS),
  member(L,VLS), !,
  retractall(configured_verbosity_level(_)),
  assert(configured_verbosity_level(L)).
update_verbosity_level(L) :-
  type_error(verbosity, L).


verbosity_levels([quiet,info,verbose,very_verbose,debug]).

member(X, [Y | XS]) :- X \== Y, !, member(X,XS).
member(X, [X | _XS]).

% % should not happen since both run and call levels are checked to be levels by verbosity_check/2
% verbosity_check(_RunLevel,_CallLevel,[]) :- fail.
verbosity_check(_RunLevel,CallLevel,[CallLevel | _]) :- !.
verbosity_check(RunLevel,_CallLevel,[RunLevel | _]) :- !, fail.
verbosity_check(RunLevel,CallLevel,[_ | Lvls]) :- !, verbosity_check(RunLevel,CallLevel,Lvls).

verbosity_check(RunLevel,CallLevel) :-
  verbosity_levels(VerbosityLevels),
  (( member(RunLevel, VerbosityLevels),
     member(CallLevel, VerbosityLevels),
     ! )
  ;
   ( type_error(verbosity_pair, {RunLevel, CallLevel})
   )
  ),
  verbosity_check(RunLevel,CallLevel,VerbosityLevels)
.


%modify_write_term([], Term, {Term, []}).
modify_write_term([], Term, {'~p~n', [Term]}).
modify_write_term([ indent | TModifiers ], Term, {TermFormat, Args}) :- !,
  modify_write_term(TModifiers, Term, {TermFormatAcc, Args}),
  atom_concat('  ', TermFormatAcc, TermFormat)
.

parse_write_term({Verbosity, Modifiers, Term}, Verbosity, ModifiedTerm) :- !,
  modify_write_term(Modifiers, Term, ModifiedTerm).
parse_write_term({Verbosity, Term}, Verbosity, ModifiedTerm) :-
  modify_write_term([], Term, ModifiedTerm)
.


plqc_write_term(VerbosityLevel, Verbosity, {Term, Args}) :-
  (verbosity_check(VerbosityLevel, Verbosity), !, format(Term, Args)
  ;
  true).
% %% clause not needed; verbosity_check/2 does not fail, it throws an error
% plqc_write_term(_VerbosityLevel, _Verbosity, Term) :-
%   type_error(pair, Term).


plqc_write(_VerbosityLevel, []) :-  !.
plqc_write(VerbosityLevel, [ HeadTerm | Terms ]) :-
  parse_write_term(HeadTerm, Verbosity, ParsedTerm),
  plqc_write_term(VerbosityLevel, Verbosity, ParsedTerm),
  plqc_write(VerbosityLevel, Terms)
.

plqc_write(Terms) :-
  configured_verbosity_level(VerbosityLevel),
  plqc_write(VerbosityLevel,Terms)
.

check_variable_W(Term, Details) :- %% Details is optional, not considered if it is a variable
  (nonvar(Term), !,
   (nonvar(Details), !, DeTail = [{info, {details, Details}}] ; DeTail = []),
   plqc_write([{info, 'WARNING: instantiated term found where the use of a variable is intended. '}
             , {info, {term, Term}}
             | DeTail ]))
  ; true
.

/** @} */
