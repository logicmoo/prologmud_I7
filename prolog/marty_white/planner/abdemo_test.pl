
%:- use_module(library(logicmoo_common)).

%:- consult(eventCalculusPlanner).
%:- consult(planner19a).
%:- consult(planner115).
:- consult(eventCalculusDMiles).

:- discontiguous reified_sort/1. 
:- discontiguous sort/1. 
:- discontiguous axiom/2.
:- discontiguous do_test/1.
:- multifile do_test/1.

testing_msg(_).

test_body(N,Body):- nl,
   write(' >>>>>START '), write(N ),
   ticks(Z1),
   (call(Body)-> TF = pass ; TF = fail),
   ticks(Z2), Z is (Z2-Z1)/60, nl, write(' +++++ '),write(N), write(' '), write(Z), write(' '), write(TF), nl.

run_tests:- 
  clause(do_test(N),Body),
  %\+ sub_term(slow, N),
  once(test_body(N,Body)),
  fail.
run_tests:- halt.

:- style_check(-singleton).
:- dynamic(do_test/1).
