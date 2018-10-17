/*
%  NomicMUD: A MUD server written in Prolog
%  Maintainer: Douglas Miles
%  Dec 13, 2035
%
%  Bits and pieces:
%
%    LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
% 
%  Copyright (C) 2004 Marty White under the GNU GPL 
%  Sept 20,1999 - Douglas Miles
%  July 10,1996 - John Eikenberry 
%
%  Logicmoo Project changes:
%
% Main file.
%
*/

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CODE FILE SECTION
:- nop(ensure_loaded('adv_debug')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% '$hide'(Pred) :- '$set_predicate_attribute'(Pred, trace, false).
never_trace(Spec):- '$hide'(Spec),'$iso'(Spec),trace(Spec, -all).
:- never_trace(list_to_set/2).
:- use_module(library(lists)).
%:- never_trace(lists:append(_,_,_)).
:- never_trace(lists:member_(_,_,_)).
:- never_trace(prolog_debug:assertion(_)).

%:- never_trace(lists:member(_,_)).
%:- never_trace(lists:append(_,_,_)).

check4bugs(_S0) :-
  !, true.
check4bugs(S0) :-
  % TODO: emergency save of S0, either here or better yet, in a catch().
  throw(check4bugs_failed(S0)).



