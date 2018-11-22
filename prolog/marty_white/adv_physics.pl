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

:- defn_state_0(never_equal).
never_equal(Sense,Thing,Agent):- nop(never_equal(Sense,Thing,Agent)),!.
never_equal(Sense,Thing,Agent):-
  never_equal(Sense,Thing),never_equal(Sense,Agent).
never_equal(Sense,Thing):-
 notrace((freeze(Thing, (dmust(Thing\==Sense))), freeze(Sense, (dmust(Thing\==Sense))))).



% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_relation')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- defn_state_getter(related_with_prop).
related_with_prop(At, Object, Place, Prop, S0) :-
  h(At, Object, Place, S0),
  getprop(Object, Prop, S0).

in_state(~(Open), Object, State) :- ground(Open),!,
 getprop(Object, state(Open, f), State).
in_state(Open, Object, State) :-
 getprop(Object, state(Open, t), State).
% getprop(Object, can_be(open, State),
% \+ getprop(Object, state(open, t), State).

:- defn_state_getter(in_scope(agent,domain,thing)).
in_scope(_Agent, Star, _State) :- is_star(Star), !.
in_scope(Agent, Thing, S0) :-
  from_loc(Agent, Here, S0),
  (Thing=Here;  open_traverse(Thing, Here, S0)),!.
in_scope(Agent, Thing, State) :-
 % get_open_traverse(_Open, _See, _Traverse, Sense),
 h(Sense, Agent, Here, State),
 (Thing=Here; h(Sense, Thing, Here, State)).
in_scope(Agent, Thing, _State):- bugout(pretending_in_scope(Agent, Thing)).



% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_relation')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- defn_state_getter(has_rel(domrel,inst)).
:- defn_state_getter(h(domrel,source,target)).

% -----------------------------------------------------------------------------
                 
subrelation(in, child).
subrelation(on, child).
subrelation(worn_by, child).
subrelation(held_by, child).
%subrelation(under, in).
%subrelation(reverse(on), child).

has_rel(At, X, S0) :- default_rel(At, X, S0).

default_rel(At, X, S0) :-
  getprop(X, default_rel(At), S0).
default_rel(At, X, S0) :-
  getprop(X, default_rel(Specific), S0),
  subrelation(Specific, At).
default_rel(At, X, S0) :-
  getprop(X, has_rel(At, TF), S0), TF \== f.
default_rel(At, X, S0) :-
  getprop(X, has_rel(Specific, TF), S0), TF \== f,
  subrelation(Specific, At).
default_rel(in, _, _S0) :- !.


%h(At, X, Y, Z, S0):- break, throw(h(At, X, Y, Z, S0)).
h(At, X, Y, S0, S2):- h(At, X, Y, S0),S2=S0.

h(At, X, Y, S0) :- in_model(h(At, X, Y), S0).

h(child, X, Y, S0) :- subrelation(At, child), h(At, X, Y, S0).

h(descended, X, Z, S0) :-
  h(child, X, Z, S0).
h(descended, X, Z, S0) :- 
  h(child, Y, Z, S0),
  h(descended, X, Y, S0).

h(open_traverse, X, Z, S0) :-
  h(child, X, Z, S0).
h(open_traverse, X, Z, S0) :- 
  h(child, Y, Z, S0),
  \+ is_closed(Y, S0),
  h(open_traverse, X, Y, S0).

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


is_closed(Object, S0) :-
  getprop(Object, state(open,f), S0).
%  getprop(Object, openable, S0),
%  \+ getprop(Object, open, S0).

from_loc(Thing, Here, S0):- 
   h(child, Thing, Here, S0), !.
from_loc(Thing, Here, S0):- 
   h(open_traverse, Thing, Here, S0), !.
from_loc(Thing, Here, S0):- 
   h(_, Thing, Here, S0), !.

open_traverse(Thing, Here, S0):- 
   h(open_traverse, Thing, Here, S0).
open_traverse(Thing, Here, S0):- 
   h(open_traverse, Here, Thing, S0).



:- defn_state_getter(touchable(agent,thing)).
touchable(_Agent, Star, _State) :- is_star(Star), !.
touchable(Agent, Thing, S0) :-
  h(child, Agent, Here, S0), % can't reach out of boxes, etc.
  (Thing=Here;  open_traverse(Thing, Here, S0)).



% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_action')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- defn_state_getter(applied_direction(agent,source,domrel,target)).
applied_direction(Start, Here, Dir, Relation, End, S0):- 
 h(_Relation, Start, Here, S0),
 h(exit(Dir), Here, End, S0),
 has_rel(Relation, End, S0).














action_doer(Action,Agent):- \+ compound(Action),!, dmust(current_player(Agent)),!.
action_doer(Action,Agent):- functor(Action,Verb,_),verbatum_anon(Verb),current_player(Agent),!.
action_doer(Action,Agent):- arg(1,Action,Agent), nonvar(Agent), \+ preposition(_,Agent),!.
action_doer(Action,Agent):- throw(missing(action_doer(Action,Agent))).
  %dmust(agent_act_verb_thing_sense(Agent, Action, _Verb, _Thing, _Sense)).
/*
agent_act_verb_thing_sense(Agent, Action, Verb, Thing, Sense):- 
 never_equal(Sense,Thing, Agent),
 notrace(agent_act_verb_thing_sense0(Agent, Action, Verb, Thing, Sense)), !.

agent_act_verb_thing_sense0(_Agent, Atom, Atom, _Target, Sense):- \+ compound(Atom), !, is_sense(Sense),!.
%agent_act_verb_thing_sense0(_Agent, Action, _Look, _Star, _See):- assertion(ground(Action)),fail.

agent_act_verb_thing_sense0(Agent, goto(Agent, _Walk, _TO, Thing), goto, Thing, see):-!.
agent_act_verb_thing_sense0(Agent, look(Agent), look, *, see):-!.
agent_act_verb_thing_sense0(Agent, examine(Agent,Sense), examine, *, Sense).
agent_act_verb_thing_sense0(Agent, examine(Agent,Sense, Object), examine, Object, Sense).
agent_act_verb_thing_sense0(Agent, touch(Agent,Target), touch, Target, Sense):- is_sense(Sense), !.

agent_act_verb_thing_sense0(Agent, Action, Verb, Thing, Sense):-
  Action=..[Verb,Agent, Sense|Rest],
  is_sense(Sense), !,
  Action2=..[Verb,Agent|Rest],
  agent_act_verb_thing_sense0(Agent, Action2, Verb, Thing, _Sense).
agent_act_verb_thing_sense0(Agent, Action, Verb, Thing, Sense):-
  Action=..[Verb,Agent, W1|Rest],
  atom(W1), atom_concat(W2, 'ly', W1), !,
  Action2=..[Verb,Agent, W2|Rest],
  agent_act_verb_thing_sense0(Agent, Action2, Verb, Thing, Sense).
agent_act_verb_thing_sense0(Agent, Action, Verb, Thing, Sense):-
  Action=..[Verb,Agent, Prep|Rest],
  preposition(Prep), !,
  Action2=..[Verb,Agent|Rest],
  agent_act_verb_thing_sense0(Agent, Action2, Verb, Thing, Sense).
agent_act_verb_thing_sense0(Agent, Action, Verb, Thing, Sense):-
  Action=..[Verb,Agent, Thing|_], !,
  agent_act_verb_thing_sense0(Agent, Verb, _UVerb, _UThing, Sense).
agent_act_verb_thing_sense0(Agent, Action, Verb, '*', Sense):-
 Action=..[Verb,Agent], dmust((action_sensory(Verb, Sense))), !.

*/
