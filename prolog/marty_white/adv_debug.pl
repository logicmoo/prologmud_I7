/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Bits and pieces:
%
% LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
% 
% Copyright (C) 2004 Marty White under the GNU GPL 
% Sept 20,1999 - Douglas Miles
% July 10,1996 - John Eikenberry 
%
% Logicmoo Project changes:
%
% Main file.
%
*/

:- user:ensure_loaded(library(poor_bugger)).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_debug')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 :- meta_predicate dshow_true(0).
 :- meta_predicate dshow_call(0).
 :- meta_predicate dshow_fail(0).

% '$hide'(Pred) :- '$set_predicate_attribute'(Pred, trace, false).
never_trace(Spec):- '$hide'(Spec),'$iso'(Spec),trace(Spec, -all).
:- use_module(library(lists)).
/*
:- never_trace(lists:append(_,_,_)).
:- never_trace(lists:list_to_set/2).
:- never_trace(lists:member_(_,_,_)).
:- never_trace(prolog_debug:assertion(_)).
*/

%:- never_trace(lists:member(_,_)).
%:- never_trace(lists:append(_,_,_)).
dshow_call(G):- simplify_dbug(G,GG), (call(G)*-> bugout(success_dshow_call(GG)) ; (bugout(failed_dshow_call(GG)),!,fail)).
dshow_fail((G1,G2)):- !,dshow_fail(G1),dshow_fail(G2).
dshow_fail(\+(G1)):- !, \+ dshow_true(G1).
dshow_fail(G):- simplify_dbug(G,GG), (call(G)*-> true ; (bugout(failed_dshow_call(GG)),!,fail)).
dshow_true(G):- simplify_dbug(G,GG), (call(G)*-> bugout(success_dshow_call(GG)) ; (!,fail)).
found_bug(S0,duplicated_object(X,R,L)) :-
 append(Left,[prop(X,R)|_],S0),
 member(prop(X,L),Left).


check4bugs(S0) :- found_bug(S0,Bug),throw(check4bugs_failed(Bug)).
check4bugs(_S0) :-
 !, true.
check4bugs(S0) :-
 % TODO: emergency save of S0, either here or better yet, in a catch().
 throw(check4bugs_failed(S0)).

  
:- multifile(user:portray/1).
:- dynamic(user:portray/1).
:- discontiguous(user:portray/1).
% user:portray


