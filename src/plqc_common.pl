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
          ]).

%% TODO - documentation

default_verbosity_level(info).

:- dynamic configured_verbosity_level/1.

configured_verbosity_level(L) :- default_verbosity_level(L).

member(X, [Y | XS]) :- X \== Y, !, member(X,XS).
member(X, [X | _XS]).

verbosity_levels([quiet,info,verbose,very_verbose,debug]).

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
   ( type_error({run_level:verbosity, call_level:verbosity}, {RunLevel, CallLevel})
   )
  ),
  verbosity_check(RunLevel,CallLevel,VerbosityLevels)
.


plqc_write_term(VerbosityLevel, Verbosity, Term) :-
  (verbosity_check(VerbosityLevel, Verbosity), !, writeln(Term))
  ;
  true.

plqc_write(_VerbosityLevel, []) :-  !.
plqc_write(VerbosityLevel, [ {Verbosity, Term} | Terms ]) :-
  plqc_write_term(VerbosityLevel, Verbosity, Term),
  plqc_write(VerbosityLevel, Terms)
.

plqc_write(Terms) :-
  configured_verbosity_level(VerbosityLevel),
  plqc_write(VerbosityLevel,Terms)
.

/** @} */
