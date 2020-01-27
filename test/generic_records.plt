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
* File:     generic_records.plt
* Last rev: 2019/11/28
* mods:
* comments: PrologCheck generic record handling shared module
*     test file
*
*************************************************************************/

:- use_module('../src/generic_records.pl').

%% -----------------------------------------------------------------------
%% -----------------------------------------------------------------------

:- begin_tests(generic_records_get).

test(get_empty, [fail]) :-
  get(rec_name, key, {rec_name, []}, _Value).
test(get_singleton_ok, true(Value =@= value)) :-
  get(rec_name, key, {rec_name, [{key, value}|[]]}, Value).
test(get_singleton_ok2) :-
  get(rec_name, key, {rec_name, [{key, value}|[]]}, value).
test(get_singleton_nok, [fail]) :-
  get(rec_name, key, {rec_name, [{other_key, value}|[]]}, _Value).
test(get_bad_record, [fail]) :-
  get(rec_name, key, {other_rec_name, [{key, value}|[]]}, _Value).
test(get_head, true(Value =@= value1)) :-
  get(rec_name, key1, {rec_name, [{key1, value1}, {key2, value2}, {key3, value3}]}, Value).
test(get_middle, true(Value =@= value2)) :-
  get(rec_name, key2, {rec_name, [{key1, value1}, {key2, value2}, {key3, value3}]}, Value).
test(get_last, true(Value =@= value3)) :-
  get(rec_name, key3, {rec_name, [{key1, value1}, {key2, value2}, {key3, value3}]}, Value).
test(get_two_elems, [fail]) :-
  get(rec_name, key, {rec_name, [{key, value1}, {key2, value2}, {key, value3}]}, _Value).
test(get_two_elems2, [fail]) :-
  get(rec_name, key, {rec_name, [{key, value1}, {key2, value2}, {key, value3}]}, value1).
test(get_two_elems3, [fail]) :-
  get(rec_name, key, {rec_name, [{key, value1}, {key2, value2}, {key, value3}]}, value3).

:- end_tests(generic_records_get).

%% -----------------------------------------------------------------------

:- begin_tests(generic_records_replace).

test(replace_empty, [fail]) :-
  replace(rec_name, key, {rec_name, []}, new_value, _NewRecord).
test(replace_singleton_ok, true(NewRecord =@= {rec_name, [{key, new_value}|[]]})) :-
  replace(rec_name, key, {rec_name, [{key, old_value}|[]]}, new_value, NewRecord).
test(replace_singleton_ok2) :-
  replace(rec_name, key, {rec_name, [{key, old_value}|[]]}, new_value, {rec_name, [{key, new_value}|[]]}).
test(replace_singleton_nok, [fail]) :-
  replace(rec_name, key, {rec_name, [{other_key, value}|[]]}, new_value, _NewRecord).
test(replace_bad_record, [fail]) :-
  replace(rec_name, key, {other_rec_name, [{key, value}|[]]}, new_value, _NewRecord).
test(replace_head, true(NewRecord =@= {rec_name, [{key1, new_value}, {key2, value2}, {key3, value3}]})) :-
  replace(rec_name, key1, {rec_name, [{key1, value1}, {key2, value2}, {key3, value3}]}, new_value, NewRecord).
test(replace_middle, true(NewRecord =@= {rec_name, [{key1, value1}, {key2, new_value}, {key3, value3}]})) :-
  replace(rec_name, key2, {rec_name, [{key1, value1}, {key2, value2}, {key3, value3}]}, new_value, NewRecord).
test(replace_last, true(NewRecord =@= {rec_name, [{key1, value1}, {key2, value2}, {key3, new_value}]})) :-
  replace(rec_name, key3, {rec_name, [{key1, value1}, {key2, value2}, {key3, value3}]}, new_value, NewRecord).
test(replace_two_elems, [fail]) :-
  replace(rec_name, key, {rec_name, [{key, value1}, {key2, value2}, {key, value3}]}, new_value, _NewRecord).
test(replace_two_elems2, [fail]) :-
  replace(rec_name, key, {rec_name, [{key, value1}, {key2, value2}, {key, value3}]}, new_value, {rec_name, [{key, new_value}, {key2, value2}, {key, value3}]}).
test(replace_two_elems3, [fail]) :-
  replace(rec_name, key, {rec_name, [{key, value1}, {key2, value2}, {key, value3}]}, new_value, {rec_name, [{key, value1}, {key2, value2}, {key, new_value}]}).

:- end_tests(generic_records_replace).

%% -----------------------------------------------------------------------

:- begin_tests(generic_records_add).

test(add_empty_ok, true(NewRecord =@= {rec_name, [{new_key, new_value}|[]]})) :-
  add(rec_name, new_key, {rec_name, []}, new_value, NewRecord).
test(add_singleton_ok, true(NewRecord =@= {rec_name, [{new_key, new_value}|[{key, value}]]})) :-
  add(rec_name, new_key, {rec_name, [{key, value}|[]]}, new_value, NewRecord).
test(add_singleton_ok2) :-
  add(rec_name, new_key, {rec_name, [{key, value}|[]]}, new_value, {rec_name, [{new_key, new_value}, {key, value}]}).
test(add_singleton_nok, [fail]) :-
  add(rec_name, existing_key, {rec_name, [{existing_key, value}|[]]}, new_value, _NewRecord).
test(add_bad_record, [fail]) :-
  add(rec_name, new_key, {other_rec_name, [{key, value}|[]]}, new_value, _NewRecord).
test(add_to_list, true(NewRecord =@= {rec_name, [{new_key, new_value}, {key2, value2}, {key3, value3}]})) :-
  add(rec_name, new_key, {rec_name, [{key2, value2}, {key3, value3}]}, new_value, NewRecord).
test(add_head_nok, [fail]) :-
  add(rec_name, key1, {rec_name, [{key1, value1}, {key2, value2}, {key3, value3}]}, new_value, _NewRecord).
test(add_middle_nok, [fail]) :-
  add(rec_name, key2, {rec_name, [{key1, value1}, {key2, value2}, {key3, value3}]}, new_value, _NewRecord).
test(add_last_nok, [fail]) :-
  add(rec_name, key3, {rec_name, [{key1, value1}, {key2, value2}, {key3, value3}]}, new_value, _NewRecord).
test(add_third, [fail]) :-
  add(rec_name, key, {rec_name, [{key, value1}, {key2, value2}, {key, value3}]}, new_value, _NewRecord).
test(add_third2, [fail]) :-
  add(rec_name, key, {rec_name, [{key1, value1}, {key, value2}, {key, value3}]}, new_value, _NewRecord).
test(add_third3, [fail]) :-
  add(rec_name, key, {rec_name, [{key, value1}, {key, value2}, {key3, value3}]}, new_value, _NewRecord).

:- end_tests(generic_records_add).

%% -----------------------------------------------------------------------

:- begin_tests(generic_records_pop).

test(pop_empty, [fail]) :-
  pop(rec_name, key, {rec_name, []}, _Value, _NewRecord).
test(pop_singleton_ok, true(NewRecord =@= {rec_name, []})) :-
  pop(rec_name, key, {rec_name, [{key, value}|[]]}, value, NewRecord).
test(pop_singleton_ok2) :-
  pop(rec_name, key, {rec_name, [{key, value}]}, value, {rec_name, []}).
test(pop_bad_record, [fail]) :-
  pop(rec_name, key, {other_rec_name, [{key, value}|[]]}, _Value, _NewRecord).
test(pop_head, true(NewRecord =@= {rec_name, [{key2, value2}, {key3, value3}]})) :-
  pop(rec_name, key1, {rec_name, [{key1, value1}, {key2, value2}, {key3, value3}]}, value1, NewRecord).
test(pop_middle, true(NewRecord =@= {rec_name, [{key1, value1}, {key3, value3}]})) :-
  pop(rec_name, key2, {rec_name, [{key1, value1}, {key2, value2}, {key3, value3}]}, value2, NewRecord).
test(pop_last, true(NewRecord =@= {rec_name, [{key1, value1}, {key2, value2}]})) :-
  pop(rec_name, key3, {rec_name, [{key1, value1}, {key2, value2}, {key3, value3}]}, value3, NewRecord).
test(pop_two_elems, [fail]) :-
  pop(rec_name, key, {rec_name, [{key, value1}, {key2, value2}, {key, value3}]}, _Value, _NewRecord).
test(pop_two_elems2a, [fail]) :-
  pop(rec_name, key, {rec_name, [{key, value1}, {key2, value2}, {key, value3}]}, _Value, {rec_name, [{key2, value2}, {key, value3}]}).
test(pop_two_elems2b, [fail]) :-
  pop(rec_name, key, {rec_name, [{key, value1}, {key2, value2}, {key, value3}]}, _Value, {rec_name, [{key, value1}, {key2, value2}]}).
test(pop_two_elems2c, [fail]) :-
  pop(rec_name, key, {rec_name, [{key, value1}, {key2, value2}, {key, value3}]}, _Value, {rec_name, [{key2, value2}]}).
test(pop_two_elems3a, [fail]) :-
  pop(rec_name, key, {rec_name, [{key, value1}, {key2, value2}, {key, value3}]}, value1, _NewRecord).
test(pop_two_elems3b, [fail]) :-
  pop(rec_name, key, {rec_name, [{key, value1}, {key2, value2}, {key, value3}]}, value3, _NewRecord).
test(pop_two_elems4a, [fail]) :-
  pop(rec_name, key, {rec_name, [{key, value1}, {key2, value2}, {key, value3}]}, value1, {rec_name, [{key2, value2}, {key, value3}]}).
test(pop_two_elems4b, [fail]) :-
  pop(rec_name, key, {rec_name, [{key, value1}, {key2, value2}, {key, value3}]}, value3, {rec_name, [{key, value1}, {key2, value2}]}).
test(pop_two_elems5a, [fail]) :-
  pop(rec_name, key, {rec_name, [{key, value1}, {key2, value2}, {key, value3}]}, value1, {rec_name, [{key2, value2}]}).
test(pop_two_elems5b, [fail]) :-
  pop(rec_name, key, {rec_name, [{key, value1}, {key2, value2}, {key, value3}]}, value3, {rec_name, [{key2, value2}]}).

:- end_tests(generic_records_pop).
