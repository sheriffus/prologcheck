:- module(testgens).

%:- use_module(library(random)).


int3(X, TestDatum, Size) :-
  X = 3,
  TestDatum = {test_datum, [{value, X}, {size, Size}, {generator, int3}, {shrinker, intnot3}]}
  % TestDatum = {test_datum, X, Size, int3, intnot3}
.
intnot3(X, TestDatum, Size) :-
  X = 4,
  TestDatum = {test_datum, [{value, X}, {size, Size}, {generator, intnot3}, {shrinker, int3}]}
  % TestDatum = {test_datum, X, Size, intnot3, int3}
.


choose(Low, High, TestDatum, Size) :-
  Low =< High,
  TestDatum = {test_datum, [{value, X}, {size, Size}, {generator, choose}, {shrinker, shrinkRange}]},
  random(Low, High, X).


shrink_range(Low, X0, Low) :-
  Low < X0.
shrink_range(Low,  X0, X) :-
  Mid is (Low+X0)//2,
  Mid < X0, Mid > Low,
  shrink_range(Mid, X0, X)
.

int(TestDatum, Size) :-
  MinusSize is Size * -1,
  choose(MinusSize, Size, TestDatum1, Size),
  generic_records:replace(test_datum, shrinker, TestDatum1, shrink_int, TestDatum)
.

uint(TestDatum, Size) :-
  choose(0, Size, TestDatum1, Size),
  generic_records:replace(test_datum, shrinker, TestDatum1, shrink_pos, TestDatum)
.

shrink_int(TestDatum0, TestDatum) :-
  generic_records:get(test_datum, value, TestDatum0, Value0),
  shrink_int_(Value0, Value),
  generic_records:replace(test_datum, value, TestDatum0, Value, TestDatum)
.

shrink_pos(TestDatum0, TestDatum) :-
  generic_records:get(test_datum, value, TestDatum0, Value0),
  shrink_pos_(Value0, Value),
  generic_records:replace(test_datum, value, TestDatum0, Value, TestDatum)
.

shrink_int_(X0, X) :-
  X0 > 0, !, shrink_pos(0, X0, X).
shrink_int_(X0, X) :-
  X0 < 0, !, NX0 is -X0, shrink_pos(0, NX0, NX), X is -NX.

shrink_pos_(Low, X0, Low) :-
  Low < X0.
shrink_pos_(Low, X0, X) :-
  Low < X0,
  Mid is (Low+X0)//2,
  Mid > Low,
  shrink_pos_(Mid, X0, X)
.
