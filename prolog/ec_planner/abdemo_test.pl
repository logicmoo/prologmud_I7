
%:- consult(eventCalculusPlanner).
%:- consult(planner19a).
%:- consult(planner115).
:- consult(eventCalculusDMiles).

:- discontiguous do_test/1.
:- multifile do_test/1.
                          
:- style_check(-singleton).
:- dynamic(do_test/1).


