:- module(testprops).

% prop(l2dl2l) :-
%         plqc:for_all( listOf(int), L, (listsTest:l2dl(L,DL-T), !,
% %% pcprop(l2dl2l) :-
% %%         plqc:pcforall( listOf(int), L, (listsTest:l2dl(L,DL-T), !,
%                                         listsTest:dl2l(DL-T,L1), !,
%                                         L == L1), 50).

successfull_goal :-
  writeln(testprop_tgoal),
  X is 1+2, X == 3.

failing_goal :-
  writeln(testprop_fgoal),
  X is 1+2, X == 4.

prop(successfull_prop) :-
  writeln(testprop_tprop),
  X is 1+2, X == 3.

prop(failing_prop) :-
  writeln(testprop_fprop),
  X is 1+2, X == 4.

prop(succ_univ_quant) :-
  for_all(testgens:int3, X,
    (Y is 1+2, Y == X)
  )
.

prop(fail_univ_quant) :-
  for_all(testgens:intnot3, X,
    (Y is 1+2, Y == X)
  )
.

prop(succ_univ_quant_sized) :-
  for_all(testgens:uint, X,
    (X >= 0)
  )
.
prop(fail_univ_quant_sized) :-
  for_all(testgens:int, X,
    for_all(testgens:int, Y,
      for_all(testgens:int, Z,
        (X =\= Y ; X == Z)
  )))
.
