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
* Last rev: 2019/04/08
* mods:
* comments: PrologCheck runtime state of executing properties
*
*************************************************************************/


/** @defgroup Context Property execution runtime state
@ingroup PrologCheck
@{

PrologCheck internal module that implements and manages auxiliary records
containing the state needed for executing a test of a property.

~~~~~~~~~~~~~~~~~~~~~{.prolog}
{
  record ctx
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

/**
 * @file   context.yap
 * @author Claudio Amaral <coa@dcc.fc.up.pt>
 * @date   Mon Apr 8 15:55 2019
 *
 * @brief  PrologCheck runtime state of executing properties.
 *
*/
:- module(ctx).


/** @pred default(? _Context_)
Succeeds with _Context_ unified to a context record of the default values
defined for the runtime property context.
*/
default({ctx}).
%        , [{mode, new}]
%        , {[bindings, []]}
%        , {[plqc]}
%        , {[actions, []]}
%        , {[samples, []]}
%        , {[printers, []]}

/** @pred default_(? _Context_)
Succeeds with _Context_ unified to a context record of the default values
defined for the runtime property context.
*/

/** @} */
