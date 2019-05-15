:- module(testgens).

%:- use_module(library(random)).

choose(Low, High, sample(X,shrinkRange)) :-
    Low =< High,
    random(Low, High, X).


int3(X, TestDatum, Size) :-
  X = 3,
  TestDatum = {test_datum, X, Size, int3, intnot3}
.
intnot3(X, TestDatum, Size) :-
  X = 4,
  TestDatum = {test_datum, X, Size, intnot3, int3}
.
