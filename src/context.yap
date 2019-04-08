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
* File:     context.yap
* Last rev: 2019/04/04
* mods:
* comments: PrologCheck runtime state of executing properties
*
*************************************************************************/


/** @defgroup Context Property execution runtime state
@{

PrologCheck internal module that implements and manages auxiliary records
containing the state needed for executing a test of a property.

~~~~~~~~~~~~~~~~~~~~~{.yap}
{
  ctx
}
~~~~~~~~~~~~~~~~~~~~~
\internal
TODO, mode         :: 'new' | 'try_shrunk' | 'try_cexm'
TODO, bound        :: imm_testcase  () | counterexample ()
TODO, module       :: module_name ()
TODO, actions      :: fail_actions()                    % not used
TODO, samples      :: [sample()]                        % not used
TODO, printers     :: [stats_printer()]                 % not used
\endinternal

*/

:- module(ctx).


default({ctx
%        , [{mode, new}]
%        , {[bindings, []]}
%        , {[plqc]}
%        , {[actions, []]}
%        , {[samples, []]}
%        , {[printers, []]}
        }).


%% * @}
