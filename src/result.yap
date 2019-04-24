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
* File:     result.yap
* Last rev: 2019/04/23
* mods:
* comments: PrologCheck result handling module
*
*************************************************************************/


/** @defgroup Result Property execution result handling
@ingroup PrologCheck
@{

PrologCheck internal module that implements and manages auxiliary records
of property execution results and their manipulation.

A result record is a record of the format
~~~~~~~~~~~~~~~~~~~~~{.prolog}
{
  result,
  Result :: 'pass' | 'fail',
  [
    {reason, Reason :: result_reason()}
  ]
}
~~~~~~~~~~~~~~~~~~~~~
where
- _Result_ states the gross test result of one execution of a property;
- _Reason_ states the reason for the gross result obtained.
and where
~~~~~~~~~~~~~~~~~~~~~{.bnf}
result_reason() :=
 | 'true_prop'
 | 'false_prop'
~~~~~~~~~~~~~~~~~~~~~

*/

/**
 * @file   result.yap
 * @author Claudio Amaral <coa@dcc.fc.up.pt>
 * @date   Mon Apr 23 17:55 2019
 *
 * @brief  PrologCheck result handling module.
 *
*/
:- module(result).

:- reconsult(generic_records).




/** @} */
