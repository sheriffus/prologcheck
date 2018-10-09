% -*- mode: prolog -*-

/*************************************************************************
*
*   PrologCheck
*
*   PrologCheck was developed at NCCUP - Universidade do Porto
*
*   Copyright
*   C. Amaral, A. M. Florido, V.S.Costa, P. B. Vasconcelos
*   and Universidade do Porto 2018
*
**************************************************************************
*
* File:     generator.yap
* Last rev: 2018/10/04
* mods:
* comments: PrologCheck built-in definitions of generators and
*           matching shrinkers
*           generator combinators for PrologCheck
*
*************************************************************************/


/**
@defgroup PrologCheck Tests Generators Property-based_Testing
@ingroup library
@{

PrologCheck generators are predicates of the form
    _GenName_ ( [ + ARGS ], ? _SAMPLE_, + _SIZE_ )
where
    _ARGS_ are parameters of the generator
and 
    _SAMPLE_ unifies with a sample term of the form
        _sample_ ( - _VALUE_, ? _SHRINKER_ )
    where 
        _Value_ is instantiated with a pseudo-randomly/automatically generated
        value of the intended domain
    and
        _SHRINKER_ unifies with the shrinking strategy, name of the predicate
        that can be used to shrink the generated value inside the generator domain;
        generators implement a default shrink strategy and can be made to accept
        other strategies
and
    _SIZE_ is a non-negative number that can be used to generate values based on
    a notion of size.

*/

:- module(generate).

:- use_module(library(random)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/** @pred choose(+ _MIN_, + _MAX_, ? _SAMPLE_ = sample( - _VALUE_, +- _SHRINKER_ ), ? _\_SIZE_)

 Generates a random element in the given inclusive range.
 Default shrinking strategy towards lower bound.

Generated value in the range `[MIN...MAX]`. Property inherited from
pred random/3 that "if both  _MIN_ and  _MAX_ are integers then  _VALUE_
will also be an integer, otherwise _VALUE_ will be a floating-point number.

_SIZE_ parameter is discarded/ignored in this generator.
*/
choose(Min,Max, sample(A,ShrinkStrategy), _Size) :-
    % set default strategy, but accept other if given
    (ShrinkStrategy = shrinkLow(Min) ; true),
    Cap is Max+1,      % the cap for including Max in the domain
    random(Min,Cap,A). % a value between Min and Cap-1

/** @pred shrinkLow(+ _LOW_, + _VALUE_, ? _RESULT_)

 Shrink an integer towards a lower value.

Unify _RESULT_ with _LOW_ or, recursively, to the _RESULT_ of the shrink
of _VALUE_ to the middle point of the interval. _RESULT_ is in range.
*/
shrinkLow(Low,  X0, Low) :-
    Low < X0.
shrinkLow(Low, X0, X) :-
    Mid is (Low+X0)//2,
    Mid > Low, Mid < X0,
    shrinkLow(Mid, X0, X).

/** @pred shrinkHigh(+ _HIGH_, + _VALUE_, - _RESULT_)
  
 Shrink an integer towards a higher value.

Unify _RESULT_ with _HIGH_ or, recursively, to the _RESULT_ of the shrink
of _VALUE_ to the middle point of the interval. _RESULT_ is in range.
*/
shrinkHigh(High, X0, High) :-
    X0 < High.
shrinkHigh(High, X0, X) :-
    Mid is (X0+High)//2,
    Mid < High, Mid > X0,
    shrinkHigh(Mid, X0, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/** @pred int(? _SAMPLE_ = sample( - _VALUE_, +- _SHRINKER_ ) , + _SIZE_)

 Generate an integer between -Size and Size inclusive range.
 Default shrinking strategy towards 0.

*/
int(Size, sample(X,shrinkInt)) :-
    Min is -Size,
    Max is Size,
    choose(Min, Max, _, sample(X,_)).

/** @pred shrinkInt(+ _VALUE_, - _RESULT_ )

 Shrink an integer absolute value towards 0.

*/
shrinkInt(X0, X) :-
    X0 > 0, !, shrinkLow(0, X0, X).
shrinkInt(X0, NX0) :-
    X0 < 0, NX0 is -X0.
shrinkInt(X0, X) :-
    X0 < 0, shrinkHigh(0, X0, X).

