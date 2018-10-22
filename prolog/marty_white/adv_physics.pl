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


in_model(E, L):- member(E, L).
thought_model(Spatial, E, L):- in_model(model(Spatial, E), L).


get_open_traverse(Open, Sense, Traverse, Spatial, OpenTraverse):- get_open_traverse(Traverse, Spatial, OpenTraverse),
  nop((ignore(Open=open), ignore(Sense=see))).

get_open_traverse(_Need, Spatial, OpenTraverse):- ignore(OpenTraverse = open_traverse(_How, Spatial)).

%% equals_efffectly(Type, Model, Value).
equals_efffectly(sense, see, _).
equals_efffectly(model, spatial, _).
equals_efffectly(_, Value, Value).



% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CODE FILE SECTION
:- nop(ensure_loaded('adv_relation')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cant_be(Sense,Thing):-
  notrace((freeze(Thing, (dmust(Thing\==Sense))), freeze(Sense, (dmust(Thing\==Sense))))).

cant(Agent, Action, cant(sense(Spatial, Sense, Thing, Why)), State) :-
  cant_be(Sense,Thing),
  act_verb_thing_model_sense(Action, Verb, Thing, Spatial, Sense),
  psubsetof(Verb, _),
  \+ in_scope(Spatial, Thing, Agent, State),
  (Why = (\+ in_scope(Spatial, Thing, Agent))).


cant(Agent, Action, cant(sense(Spatial, Sense, Thing, Why)), State) :-
  cant_be(Sense,Thing),
  % sensory_model(Sense, Spatial),
  act_verb_thing_model_sense(Action, Verb, Thing, Spatial, Sense),
  psubsetof(Verb, examine(Sense)),
  \+ can_sense(Spatial, Sense, Thing, Agent, State),
  (Why = ( \+ can_sense(Spatial, Sense, Thing, Agent))).

/*
cant(Agent, Action, cant(reach(Spatial, Thing)), State) :-
  act_verb_thing_model_sense(Action, Verb, Thing, Spatial, _Sense),
  psubsetof(Verb, touch),
  \+ reachable(Spatial, Thing, Agent, State).
*/

cant(_Agent, Action, cant(move(Spatial, Thing)), State) :-
  act_verb_thing_model_sense(Action, Verb, Thing, Spatial, _Sense),
  psubsetof(Verb, move),
  getprop(Thing, can_be(Spatial, move, f), State).

cant(Agent, Action, musthave( Thing), State) :-
  act_verb_thing_model_sense(Action, Verb, Thing, Spatial, _Sense),
  get_open_traverse(Verb, Spatial, OpenTraverse),
  psubsetof(Verb, drop),
  \+ related(Spatial, OpenTraverse, Thing, Agent, State).

cant(Agent, Action, cant(manipulate(Spatial, self)), _) :-
  Action =.. [Verb, Agent |_],
  psubsetof(Verb, touch(Spatial)).
cant(Agent, take( Thing), alreadyhave(Thing), State) :-
  related(_Spatial, descended, Thing, Agent, State).
cant(Agent, take( Thing), mustgetout(Thing), State) :-
  related(_Spatial, descended, Agent, Thing, State).
cant(_Agent, put(_Spatial, Thing1, _How, Thing1), self_relation( Thing1), _S0).
cant(_Agent, put(Spatial, Thing1, _How, Thing2), moibeus_relation( Thing1, Thing2), S0) :-
  related(Spatial, descended, Thing2, Thing1, S0).
cant(_Agent, throw( Thing1, _How, Thing1), self_relation( Thing1), _S0).
cant(_Agent, throw( Thing1, _How, Thing2), moibeus_relation( Thing1, Thing2), S0) :-
  related(_Spatial, descended, Thing2, Thing1, S0).



cant(Agent, look(Spatial), TooDark, State) :-
  sensory_model_problem_solution(Sense, _Spatial, TooDark, _EmittingLight),
  % Perhaps this should return a logical description along the lines of
  %   failure(look(Spatial), requisite(look(Spatial), getprop(SomethingNearby, EmittingLight)))
  \+ has_sensory(Spatial, Sense, Agent, State).

% Can always know inventory
%cant(Agent, inventory, TooDark, State) :- equals_efffectly(sense, Sense, look),
%  sensory_model_problem_solution(Sense, Spatial, TooDark, _EmittingLight),
%  \+ has_sensory(Spatial, Sense, Agent, State).

cant(Agent, examine(Sense, Thing), cant(sense(Spatial, Sense, Thing, TooDark)), State) :- equals_efffectly(sense, Sense, see),
  cant_be(Sense,Thing),
  sensory_model_problem_solution(Sense, Spatial, TooDark, _EmittingLight),
  \+ has_sensory(Spatial, Sense, Agent, State).

cant(Agent, examine(Sense, Thing), cant(sense(Spatial, Sense, Thing, Why)), State) :-
  cant_be(Sense,Thing),
  \+ can_sense(Spatial, Sense, Thing, Agent, State),
  (Why = ( \+ can_sense(Spatial, Sense, Thing, Agent, State))).


cant(Agent, goto(_Relation, Object), mustdrop(Object), State) :-
  related(_Spatial, descended, Object, Agent, State).

cant(Agent, EatCmd, cantdothat(EatCmd), State) :-
  action_model(EatCmd, Spatial),
  getprop(Agent, can_do(Spatial, EatCmd, f), State).

cant(Agent, EatCmd, cantdothat_verb(EatVerb, EatCmd), State) :-
  act_verb_thing_model_sense(EatCmd, EatVerb, _Thing, Spatial, _Sense),
  getprop(Agent, can_do(Spatial, EatVerb, f), State).




% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CODE FILE SECTION
:- nop(ensure_loaded('adv_relation')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

related_with_prop(Spatial, How, Object, Place, Prop, State) :-
  related(Spatial, How, Object, Place, State),
  getprop(Object, Prop, State).

is_state(Spatial, ~(Open), Object, State) :- ground(Open),!,
  getprop(Object, state(Spatial, Open, f), State).
is_state(Spatial, Open, Object, State) :-
  getprop(Object, state(Spatial, Open, t), State).
%  getprop(Object, can_be(Spatial, open, State),
%  \+ getprop(Object, state(Spatial, open, t), State).

in_scope(_Spatial, Thing, _Agent, _State) :- Thing == '*', !.
in_scope(Spatial, Thing, Agent, State) :-
   get_open_traverse(_Open, _See, _Traverse, Spatial, OpenTraverse),
  related(Spatial, OpenTraverse, Agent, Here, State),
  (Thing=Here; related(Spatial, OpenTraverse, Thing, Here, State)).
in_scope(Spatial, Thing, Agent, _State):- dbug(pretending_in_scope(Spatial, Thing, Agent)).

reachable(_Spatial, Star, _Agent, _State) :- Star == '*', ! .
reachable(Spatial, Thing, Agent, State) :-
  get_open_traverse(touch, Spatial, OpenTraverse),
  related(Spatial, child, Agent, Here, State), % can't reach out of boxes, etc.
  (Thing=Here; related(Spatial, OpenTraverse, Thing, Here, State)).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CODE FILE SECTION
:- nop(ensure_loaded('adv_relation')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


subrelation(in, child).
subrelation(on, child).
%subrelation(under, in).
%subrelation(under, child).
subrelation(worn_by, child).
subrelation(held_by, child).

has_rel(Spatial, How, X, State) :-
  getprop(X, has_rel(Spatial, How), State).
has_rel(Spatial, How, X, State) :-
  getprop(X, has_rel(Spatial, Specific), State),
  subrelation(Specific, How).

%related(_Spatial, How, _X, _Y, _State) :- assertion(nonvar(How)), fail.
related(_Spatial, _How, _X, _Y, []) :- !, fail.
related(Spatial, How, X, Y, State):-  quietly(related_hl(Spatial, How, X, Y, State)).


related_hl(Spatial, How, X, Y, State) :- declared(h(Spatial, How, X, Y), State).
related_hl(_Spatial, How, _X, _Y, _State) :- var(How), !, fail.
related_hl(Spatial, child, X, Y, State) :- subrelation(How, child), related_hl(Spatial, How, X, Y, State).
related_hl(Spatial, descended, X, Z, State) :-
  related_hl(Spatial, child, X, Z, State).
related_hl(Spatial, descended, X, Z, State) :-
  related_hl(Spatial, child, Y, Z, State),
  related_hl(Spatial, descended, X, Y, State).
related_hl(Spatial, open_traverse(Traverse, Spatial), X, Z, State) :-
 get_open_traverse(_Traverse, Spatial, open_traverse(Traverse, Spatial)),
  related_hl(Spatial, child, X, Z, State).
related_hl(Spatial, open_traverse(Traverse, Spatial), X, Z, State) :-
 get_open_traverse(Open, _See, _Traverse, Spatial, open_traverse(Traverse, Spatial)),
  related_hl(Spatial, child, Y, Z, State),
  \+ is_state(Spatial, ~(Open), Y, State),
  related_hl(Spatial, open_traverse(Traverse, Spatial), X, Y, State).
related_hl(Spatial, inside, X, Z, State) :- related_hl(Spatial, in, X, Z, State).
related_hl(Spatial, inside, X, Z, State) :- related_hl(Spatial, in, Y, Z, State),
                                related_hl(Spatial, descended, X, Y, State).
related_hl(Spatial, exit(out), Inner, Outer, State) :-
  related_hl(Spatial, child, Inner, Outer, State),
  has_rel(Spatial, in, Inner, State),
  has_rel(Spatial, child, Outer, State),
  get_open_traverse(Open, _See, _Traverse, Spatial, _OpenTraverse),
  \+ is_state(Spatial, ~(Open), Inner, State).
related_hl(Spatial, exit(off), Inner, Outer, State) :-
  related_hl(Spatial, child, Inner, Outer, State),
  has_rel(Spatial, on, Inner, State),
  has_rel(Spatial, child, Outer, State).
related_hl(Spatial, exit(escape), Inner, Outer, State) :-
  related_hl(Spatial, child, Inner, Outer, State),
  has_rel(Spatial, child, Inner, State),
  has_rel(Spatial, child, Outer, State).




% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CODE FILE SECTION
:- nop(ensure_loaded('adv_action')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

moveto(Spatial, Object, How, Dest, Vicinity, Msg, State, S9) :-
  undeclare(h(Spatial, _, Object, Here), State, VoidState),
  declare(h(Spatial, How, Object, Dest), VoidState, S2),
  queue_local_event(Spatial, [moved(Spatial, Object, Here, How, Dest), Msg], Vicinity, S2, S9).

moveallto(_Spatial, [], _R, _D, _V, _M, S, S).
moveallto(Spatial, [Object|Tail], Relation, Destination, Vicinity, Msg, S0, S2) :-
  moveto(Spatial, Object, Relation, Destination, Vicinity, Msg, S0, S1),
  moveallto(Spatial, Tail, Relation, Destination, Vicinity, Msg, S1, S2).

disgorge(Spatial, Container, How, Here, Vicinity, Msg, S0, S9) :-
  findall(Inner, related(Spatial, child, Inner, Container, S0), Contents),
  bugout('~p contained ~p~n', [Container, Contents], general),
  moveallto(Spatial, Contents, How, Here, Vicinity, Msg, S0, S9).
disgorge(_Spatial, _Container, _How, _Here, _Vicinity, _Msg, S0, S0).

thrown( Thing, _Target, How, Here, Vicinity, S0, S9) :-
  getprop(Thing, fragile(Broken), S0),
  bugout('object ~p is fragile~n', [Thing], general),
  undeclare(h(Spatial, _, Thing, _), S0, S1),
  declare(h(Spatial, How, Broken, Here), S1, S2),
  queue_local_event(Spatial, [transformed(Thing, Broken)], Vicinity, S2, S3),
  disgorge(Spatial, Thing, How, Here, Vicinity, 'Something falls out.', S3, S9).
thrown( Thing, _Target, How, Here, Vicinity, S0, S9) :-
  moveto(spatial, Thing, How, Here, Vicinity, 'Thrown.', S0, S9).

hit( Target, _Thing, Vicinity, S0, S9) :-
  Spatial = spatial,
  getprop(Target, fragile(Broken), S0),
  bugout('target ~p is fragile~n', [Target], general),
  undeclare(h(Spatial, How, Target, Here), S0, S1),
  queue_local_event(Spatial, [transformed(Target, Broken)], Vicinity, S1, S2),
  declare(h(Spatial, How, Broken, Here), S2, S3),
  disgorge(Spatial, Target, How, Here, Vicinity, 'Something falls out.', S3, S9).
hit(_Spatial, _Target, _Thing, _Vicinity, S0, S0).




act_verb_thing_model_sense(Action, Verb, Thing, Spatial, Sense):-
    cant_be(Sense,Thing),
    notrace(act_verb_thing_model_sense0(Action, Verb, Thing, Spatial, Sense)), !.

act_verb_thing_model_sense0(goto(*, Thing), goto, Thing, spatial, see):-!.
act_verb_thing_model_sense0(look(spatial), look, *, spatial, see):-!.
act_verb_thing_model_sense0(look(spatial, spatial), look, *, spatial, see):-!.
act_verb_thing_model_sense0(look, look, *, spatial, see):-!.
act_verb_thing_model_sense0(look, look, *, spatial, Sense):- is_sense(Sense), !.

act_verb_thing_model_sense0(Action, Verb, Thing, W1, Sense):-
     Action=..[Verb, W1|Rest],
     W1 == spatial, !,
     Action2=..[Verb|Rest],
     act_verb_thing_model_sense(Action2, Verb, Thing, _Spatial, Sense).
act_verb_thing_model_sense0(Action, Verb, Thing, Spatial, Sense):-
     Action=..[Verb, Sense|Rest],
     is_sense(Sense), !,
     Action2=..[Verb|Rest],
     act_verb_thing_model_sense0(Action2, Verb, Thing, Spatial, _Sense).
act_verb_thing_model_sense0(Action, Verb, Thing, Spatial, Sense):-
     Action=..[Verb, W1|Rest],
     atom(W1), atom_concat(W2, 'ly', W1), !,
     Action2=..[Verb, W2|Rest],
     act_verb_thing_model_sense0(Action2, Verb, Thing, Spatial, Sense).
act_verb_thing_model_sense0(Action, Verb, Thing, Spatial, Sense):-
     Action=..[Verb, Prep|Rest],
     preposition(Spatial, Prep), !,
     Action2=..[Verb|Rest],
     act_verb_thing_model_sense0(Action2, Verb, Thing, _Spatial, Sense).
act_verb_thing_model_sense0(Action, Verb, Thing, Spatial, Sense):-
     Action=..[Verb, Thing|_], !,
     act_verb_thing_model_sense0(Verb, _UVerb, _UThing, Spatial, Sense).
act_verb_thing_model_sense0(Action, Verb, '*', Spatial, Sense):-
  Action=..[Verb], dmust((action_sensory(Verb, Sense), action_model(Verb, Spatial))), !.


