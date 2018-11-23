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


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_relation')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- defn_state_none(never_equal(sense,inst,agent)).
never_equal(Sense,Thing,Agent):- nop(never_equal(Sense,Thing,Agent)),!.
never_equal(Sense,Thing,Agent):-
  never_equal(Sense,Thing),never_equal(Sense,Agent).
never_equal(Sense,Thing):-
 notrace((freeze(Thing, (dmust(Thing\==Sense))), freeze(Sense, (dmust(Thing\==Sense))))).



% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_relation')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- defn_state_getter(related_with_prop(domrel, inst, place, prop)).
related_with_prop(At, Object, Place, Prop, S0) :-
  h(At, Object, Place, S0),
  getprop(Object, Prop, S0).

:- defn_state_getter(in_state(domrel, inst)).
in_state(~(Open), Object, State) :- ground(Open),!,
 getprop(Object, state(Open, f), State).
in_state(Open, Object, State) :-
 getprop(Object, state(Open, t), State).
% getprop(Object, can_be(open, State),
% \+ getprop(Object, state(open, t), State).

:- defn_state_getter(in_scope(agent,thing)).
in_scope(_Agent, Star, _State) :- is_star(Star), !.
in_scope(Agent, Thing, S0) :-
  from_loc(Agent, Here, S0),
  (Thing=Here;  open_traverse(Thing, Here, S0)),!.
in_scope(Agent, Thing, State) :-
 % get_open_traverse(_Open, _See, _Traverse, Sense),
 h(Sense, Agent, Here, State),
 (Thing=Here; h(Sense, Thing, Here, State)).
in_scope(Agent, Thing, _State):- bugout1(pretending_in_scope(Agent, Thing)).



% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_relation')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% -----------------------------------------------------------------------------

subrelation(in, child).
subrelation(on, child).
subrelation(worn_by, child).
subrelation(held_by, child).
subrelation(Sub, child_not_in):- dif(Sub,in), subrelation(Sub, child).
%subrelation(under, in).
%subrelation(reverse(on), child).

same_rel(Rel,Prep):- Rel==Prep.
same_rel(Rel,Prep):- \+ ground((Rel,Prep)),!, fail.
same_rel(Rel,Prep):- compound(Prep),!,arg(_,Prep,E),same_rel(Rel,E).
same_rel(Rel,Prep):- compound(Rel),!,arg(_,Rel,E),same_rel(E,Prep).
same_rel(Rel,Prep):- subrelation(Rel,Prep).

:- defn_state_getter(prep_to_rel(target,preprel,-domrel)).
prep_to_rel(Target, Prep, Rel, S0):- has_rel(Rel, Target, S0), same_rel(Rel,Prep),!.
prep_to_rel(Target, Prep, Rel, S0):- in_model(h(Rel, Target, _), S0), same_rel(Rel,Prep), !. 
prep_to_rel(Target, Prep, Rel, S0):- atom(Prep), prep_to_rel(Target, exit(Prep), Rel, S0),!.
prep_to_rel(Target, _Prep, Rel, S0):- default_rel(Rel, Target, S0).

:- defn_state_getter(has_rel(domrel,inst)).
has_rel(At, X, S0) :- default_rel(At, X, S0).

:- defn_state_getter(default_rel(domrel,inst)).
default_rel(At, X, S0) :-
  getprop(X, default_rel(At), S0).
default_rel(At, X, S0) :-
  getprop(X, has_rel(At, TF), S0), TF \== f.
default_rel(in, _, _S0) :- !.

default_rel(At, X, S0) :-
  getprop(X, default_rel(Specific), S0),
  subrelation(Specific, At).
default_rel(At, X, S0) :-
  getprop(X, has_rel(Specific, TF), S0), TF \== f,
  subrelation(Specific, At).


:- defn_state_getter(h(domrel,source,target)).

h(At, X, Y, S0) :- in_model(h(At, X, Y), S0).

h(child, X, Y, S0) :- subrelation(At, child), h(At, X, Y, S0).

h(descended, X, Z, S0) :-
  h(child, X, Z, S0).
h(descended, X, Z, S0) :- 
  h(child, Y, Z, S0),
  h(descended, X, Y, S0).

h(open_traverse, X, Z, S0):-
  h(descended, X, Z, S0),
  \+ (is_closed(Z, S0), h(inside, X, Z, S0)).


h(inside, X, Z, S0) :- h(in, X, Z, S0).
h(inside, X, Z, S0) :- h(in, Y, Z, S0),
          h(descended, X, Y, S0).

h(exit(Out), Inner, Outer, S0) :- in_out(In,Out),
  h(child, Inner, Outer, S0),
  has_rel(In, Inner, S0),
  has_rel(child, Outer, S0),
  \+ is_closed(Inner, S0).
h(exit(Off), Inner, Outer, S0) :- on_off(On,Off),
  h(child, Inner, Outer, S0),
  has_rel(On, Inner, S0),
  has_rel(child, Outer, S0).
h(exit(Escape), Inner, Outer, S0) :- escape_rel(Escape),
  h(child, Inner, Outer, S0),
  has_rel(child, Inner, S0),
  has_rel(child, Outer, S0).


in_out(in,out).
on_off(on,off).
escape_rel(escape).

:- defn_state_getter(is_closed(inst)).

is_closed(Object, S0) :-
  getprop(Object, state(opened,f), S0).
%  getprop(Object, openable, S0),
%  \+ getprop(Object, open, S0).

  
:- defn_state_getter(from_loc(inst, place)).

from_loc(Thing, Here, S0):- 
   h(child, Thing, Here, S0), !.
from_loc(Thing, Here, S0):- 
   h(open_traverse, Thing, Here, S0), !.
from_loc(Thing, Here, S0):- 
   h(_, Thing, Here, S0), !.

:- defn_state_getter(open_traverse(inst,here)).

open_traverse(Thing, Here, S0):- 
   h(open_traverse, Thing, Here, S0).
open_traverse(Thing, Here, S0):- 
   h(open_traverse, Here, Thing, S0).



:- defn_state_getter(touchable(agent,thing)).
touchable(Agent, Thing, S0):- notrace(touchable0(Agent, Thing, S0)).
touchable0(_Agent, Star, _State) :- is_star(Star), !.
touchable0(Agent, Thing, S0) :-
  h(child, Agent, Here, S0), % can't reach out of boxes, etc.
  (Thing=Here;  open_traverse(Thing, Here, S0)).



% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_action')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- defn_state_getter(applied_direction(start,source,prep,domrel,target)).
applied_direction(Start, Here, Dir, Relation, End, S0):- 
 h(_Relation, Start, Here, S0),
 h(exit(Dir), Here, End, S0),
 has_rel(Relation, End, S0).



