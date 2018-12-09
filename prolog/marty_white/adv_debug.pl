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

found_bug(S0,open_list(Open)) :- \+is_list(S0),
  get_open_segement(S0,Open).
found_bug(S0,duplicated_object(X,R,L)) :-
 append(Left,[prop(X,R)|_],S0),
 member(prop(X,L),Left).

get_open_segement(S0,Open):- append(Left,_,S0),is_list(Left),length(Left,N),N>2,!,append([_,_],S1,S0),get_open_segement(S1,Open).
get_open_segement(S0,S0).


check4bugs(Why, S0):- found_bug(S0,Bug), pprint(S0, always), pprint(check4bugs_found_bug(Why,Bug),always),throw(check4bugs_failed(Bug)).
 % TODO: emergency save of S0, either here or better yet, in a catch().
check4bugs(_, _).


