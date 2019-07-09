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
 * @date   Mon June 06 14:55 2019
 *
 * @brief  PrologCheck shared helper and auxiliary predicates module.
 *
*/
:- module(plqc_common, [member/2]).


%% TODO - tests and documentation

member(X, [Y | XS]) :- X \== Y, !, member(X,XS).
member(X, [X | _XS]).

verbosity_levels([quiet,info,verbose,very_verbose,debug]).

verbosity_check(RunLevel,CallLevel,[]) :-
  writeln({'INVALID_VERBOSITY', run_lvl, RunLevel, call_lvl, CallLevel}), % TODO make error
  fail
.
verbosity_check(_RunLevel,CallLevel,[CallLevel | _]) :- !.
verbosity_check(RunLevel,_CallLevel,[RunLevel | _]) :- !, fail.
verbosity_check(RunLevel,CallLevel,[_ | Lvls]) :- !, verbosity_check(RunLevel,CallLevel,Lvls).

verbosity_check(RunLevel,CallLevel) :-
  verbosity_levels(VerbosityLevels),
  verbosity_check(RunLevel,CallLevel,VerbosityLevels)
.


plqc_write_term(VerbosityLevel, Verbosity, Term) :-
  verbosity_check(VerbosityLevel, Verbosity), !,
  writeln(Term).
plqc_write_term(_VerbosityLevel, _Verbosity, _Term).

plqc_write(_VerbosityLevel, []).
plqc_write(VerbosityLevel, [ {Verbosity, Term} | Terms ]) :-
  plqc_write_term(VerbosityLevel, Verbosity, Term),
  plqc_write(VerbosityLevel, Terms)
.

/** @} */
