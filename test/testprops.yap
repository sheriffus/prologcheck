:- module(testprops).

% prop(l2dl2l) :-
%         plqc:for_all( listOf(int), L, (listsTest:l2dl(L,DL-T), !,
% %% pcprop(l2dl2l) :-
% %%         plqc:pcforall( listOf(int), L, (listsTest:l2dl(L,DL-T), !,
%                                         listsTest:dl2l(DL-T,L1), !,
%                                         L == L1), 50).

successfull_goal :-
  X is 1+2, X == 3.

failing_goal :-
  X is 1+2, X == 4.
