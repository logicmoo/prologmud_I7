
:- use_module(library(debug)).
:- use_module(library(listing)).
:- use_module(library(prolog_clause)).
:- use_module(library(logicmoo_common)).

%:- consult(eventCalculusPlanner).
%:- consult(planner19a).
%:- consult(planner115).
:- consult(eventCalculusDMiles).

:- discontiguous reified_sort/1. 
:- discontiguous sort/1. 
:- discontiguous axiom/2.
:- discontiguous do_test/1.
:- multifile do_test/1.
                          
:- style_check(-singleton).
:- dynamic(do_test/1).
