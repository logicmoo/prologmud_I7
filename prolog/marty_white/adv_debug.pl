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
dshow_call(G):- hide_state_lists(G,GG), (call(G)*-> dmsg(success_dshow_call(GG)) ; (dmsg(failed_dshow_call(GG)),!,fail)).
found_bug(S0,duplicated_object(X,R,L)) :-
  append(Left,[prop(X,R)|_],S0),
  member(prop(X,L),Left).

hide_state_lists(G,GG):- \+ compound(G),!,GG=G.
hide_state_lists([G1|G2],GG):- is_state_list([G1|G2],GG),!.
hide_state_lists([G1|G2],[GG1|GG2]):- !,hide_state_lists(G1,GG1),hide_state_lists(G2,GG2).
hide_state_lists(G,GG):- compound_name_arguments(G,F,GL),maplist(hide_state_lists,GL,GGL),!,compound_name_arguments(GG,F,GGL).

is_state_list(G,_):- \+ compound(G),!,fail.
is_state_list([G1|G2],{GG,'...'}):- compound(G1),G1=structure_label(GG),!.
is_state_list([_|G],GG):-is_state_list(G,GG).

check4bugs(S0) :- found_bug(S0,Bug),throw(check4bugs_failed(Bug)).
check4bugs(_S0) :-
  !, true.
check4bugs(S0) :-
  % TODO: emergency save of S0, either here or better yet, in a catch().
  throw(check4bugs_failed(S0)).



