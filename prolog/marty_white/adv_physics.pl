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

:- defn_state_0(get_open_traverse).
get_open_traverse(Open, Sense, Traverse, Spatial, OpenTraverse):- get_open_traverse(Traverse, Spatial, OpenTraverse),
 ((ignore(Open=open), ignore(Sense=see))).

get_open_traverse(_Need, Spatial, OpenTraverse):- ignore(OpenTraverse = open_traverse(_Prep, Spatial)).

:- defn_state_0(equals_efffectly).
%% equals_efffectly(Type, Model, Value).
equals_efffectly(sense, see, _).
equals_efffectly(model, spatial, _).
equals_efffectly(_, Value, Value).



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


:- defn_state_getter(cant//2).
cant( Action, Why, State) :-
 never_equal(Sense,Thing, Agent),
 act_verb_thing_model_sense(Agent, Action, Verb, Thing, Spatial, Sense),
 psubsetof(Verb, _),
 \+ in_scope(Agent, Spatial, Thing, State),
 (Why = (\+ in_scope(Agent, Spatial, Thing, Agent))).


cant( Action,  Why, State) :-
 never_equal(Sense,Thing, Agent),
 % sensory_model(Sense, Spatial),
 act_verb_thing_model_sense(Agent, Action, Verb, Thing, _Spatial, Sense),
 psubsetof(Verb, examine(Agent, Sense)),
 \+ can_sense(Agent, Sense, Thing, State),
 (Why = ( reason( \+ can_sense(Agent, Sense, Thing)))).

/*
cant( Agent, Action, cant( reach(Agent, Spatial, Thing)), State) :-
 act_verb_thing_model_sense(Agent, Action, Verb, Thing, Spatial, _Sense),
 psubsetof(Verb, touch),
 \+ is_reachable(Agent, Spatial, Thing, State).
*/

cant( Action, getprop(Thing, can_be(Move, f)), State) :-
 act_verb_thing_model_sense(_Agent, Action, Verb, Thing, _Spatial, _Sense),
 psubsetof(Verb, Move),
 getprop(Thing, can_be(Move, f), State).

cant( Action, musthave( Thing), State) :-
 act_verb_thing_model_sense(Agent, Action, Verb, Thing, Spatial, _Sense),
 get_open_traverse(Verb, Spatial, OpenTraverse),
 psubsetof(Verb, drop),
 \+ related(Spatial, OpenTraverse, Thing, Agent, State).

cant( Action, manipulate(Spatial, self), _) :-
 Action =.. [Verb, Agent, Agent |_],
 psubsetof(Verb, touch(Spatial)).
cant( take(Agent, Thing), alreadyhave(Thing), State) :-
 related(_Spatial, descended, Thing, Agent, State).
cant( take(Agent, Thing), mustgetout(Thing), State) :-
 related(_Spatial, descended, Agent, Thing, State).

cant( put(_Agent, _Spatial, Thing1, Dest), self_relation(Thing1), _S0):- 
  dest_target(Dest,Object),Object==Thing1.
cant( put(_Agent, Spatial, Thing1, Dest), moibeus_relation( Thing1, Target), S0) :-
 dest_target(Dest,Target),
 related(Spatial, descended, Target, Thing1, S0).

cant( throw(_Agent, Thing1, _Prep, Thing1), self_relation( Thing1), _S0).
cant( throw(_Agent, Thing1, _Prep, Target), moibeus_relation( Thing1, Target), S0) :-
 related(_Spatial, descended, Target, Thing1, S0).


cant( look(Agent, Spatial), TooDark, State) :-
 sensory_model_problem_solution(Sense, _Spatial, TooDark, _EmittingLight),
 % Perhaps this should return a logical description along the lines of
 % failure(look(Agent, Spatial), requisite(look(Agent, Spatial), getprop(SomethingNearby, EmittingLight)))
 \+ can_sense_here(Agent, Spatial, Sense, State).

% Can always know inventory
%cant( Agent, inventory, TooDark, State) :- equals_efffectly(sense, Sense, look),
% sensory_model_problem_solution(Sense, Spatial, TooDark, _EmittingLight),
% \+ can_sense_here(Agent, Spatial, Sense, State).

cant( examine(Agent, Sense, Thing), TooDark, State) :- 
 equals_efffectly(sense, Sense, see),
 never_equal(Sense,Thing, Agent),
 sensory_model_problem_solution(Sense, Spatial, TooDark, _EmittingLight),
 \+ can_sense_here(Agent, Spatial, Sense, State).

cant( examine(Agent, Sense, Thing), Why, State) :-
 never_equal(Sense,Thing, Agent),
 \+ can_sense(Agent, Sense, Thing, State),
 (Why = ( reason(  \+ can_sense(Agent, Sense, Thing)))).


cant( goto(Agent, _Walk, Dest), mustdrop(Target), State) :- 
 dest_target(Dest,Target),
 nonvar(Target),
 related(_Spatial, descended, Target, Agent, State).

cant( EatCmd, cantdothat(Verb), State) :-
 act_verb_thing_model_sense(Agent, EatCmd, Verb, _Thing, _Spatial, _Sense),
 getprop(Agent, knows_verbs(Verb, f), State).



% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_relation')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- defn_state_getter(related_with_prop).
related_with_prop(Spatial, Prep, Object, Place, Prop, State) :-
 related(Spatial, Prep, Object, Place, State),
 getprop(Object, Prop, State).

in_state(~(Open), Object, State) :- ground(Open),!,
 getprop(Object, status(Open, f), State).
in_state(Open, Object, State) :-
 getprop(Object, status(Open, t), State).
% getprop(Object, can_be(open, State),
% \+ getprop(Object, status(open, t), State).

:- defn_state_getter(in_scope(agent,domain,thing)).
in_scope(_Agent, _Spatial, Star, _State) :- is_star(Star), !.
in_scope(Agent, Spatial, Thing, State) :-
 get_open_traverse(_Open, _See, _Traverse, Spatial, OpenTraverse),
 related(Spatial, OpenTraverse, Agent, Here, State),
 (Thing=Here; related(Spatial, OpenTraverse, Thing, Here, State)).
in_scope(Agent, Spatial, Thing, _State):- bugout(pretending_in_scope(Agent, Spatial, Thing)).

:- defn_state_getter(is_reachable(agent,domain,thing)).
is_reachable(_Agent, _Spatial, Star, _State) :- is_star(Star), !.
is_reachable(Agent, Spatial, Thing, State) :-
 get_open_traverse(touch, Spatial, OpenTraverse),
 related(Spatial, child, Agent, Here,State), % can't reach out of boxes, etc.
 (Thing=Here; related(Spatial, OpenTraverse, Thing, Here, State)).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_relation')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


subrelation(in, child).
subrelation(on, child).
%subrelation(under, in).
subrelation(reverse(on), child).
subrelation(worn_by, child).
subrelation(held_by, child).

:- defn_state_getter(has_rel(domain,domrel,inst)).
has_rel(Spatial, How, X, State) :-
 getprop(X, has_rel(Spatial, How, t), State).
has_rel(Spatial, How, X, State) :-
 getprop(X, has_rel(Spatial, Specific, t), State),
 subrelation(Specific, How).

%related(_Spatial, How, _X, _Y, _State) :- assertion(nonvar(How)), fail.
:- defn_state_getter(related(domain,domrel,source,target)).
related(_Spatial, _How, _X, _Y, []) :- !, fail.
related(Spatial, How, X, Y, State):- related_hl(Spatial, How, X, Y, State).

:- defn_state_getter(related_hl(domain,domrel,source,target)).
related_hl(Spatial, How, X, Y, State) :- declared(h(Spatial, How, X, Y), State).
related_hl(_Spatial, How, _X, _Y, _State) :- var(How), !, fail.
related_hl(Spatial, child, X, Y, State) :- subrelation(How, child), related_hl(Spatial, How, X, Y, State).

related_hl(Spatial, descended, X, Z, State) :-
 related_hl(Spatial, child, X, Z, State).
related_hl(Spatial, descended, X, Z, State) :-
 related_hl(Spatial, child, Y, Z, State),
 related_hl(Spatial, descended, X, Y, State).

related_hl(Spatial, open_traverse(Traverse, Spatial), X, Z, State) :- (nonvar(X);nonvar(Z)),
 get_open_traverse(_Traverse, Spatial, open_traverse(Traverse, Spatial)),
 related_hl(Spatial, child, X, Z, State).
related_hl(Spatial, open_traverse(Traverse, Spatial), X, Z, State) :- !,
 (nonvar(X);nonvar(Z)),
 get_open_traverse(Open, _See, _Traverse, Spatial, open_traverse(Traverse, Spatial)),
 related_hl(Spatial, child, Y, Z, State),
 \+ in_state(~(Open), Y, State),
 related_hl(Spatial, open_traverse(Traverse, Spatial), X, Y, State).

related_hl(Spatial, inside, X, Z, State) :- related_hl(Spatial, in, X, Z, State).
related_hl(Spatial, inside, X, Z, State) :- related_hl(Spatial, in, Y, Z, State),
        related_hl(Spatial, descended, X, Y, State).
related_hl(Spatial, exit(out), Inner, Outer, State) :-
 related_hl(Spatial, child, Inner, Outer, State),
 has_rel(Spatial, in, Inner, State),
 has_rel(Spatial, child, Outer, State),
 get_open_traverse(Open, _See, _Traverse, Spatial, _OpenTraverse),
 \+ in_state(~(Open), Inner, State).
related_hl(Spatial, exit(off), Inner, Outer, State) :-
 related_hl(Spatial, child, Inner, Outer, State),
 has_rel(Spatial, on, Inner, State),
 has_rel(Spatial, child, Outer, State).
related_hl(Spatial, exit(escape), Inner, Outer, State) :-
 related_hl(Spatial, child, Inner, Outer, State),
 has_rel(Spatial, child, Inner, State),
 has_rel(Spatial, child, Outer, State).




% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_action')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- defn_state_getter(applied_direction(agent,source,domrel,target)).
applied_direction(Start, Here, Dir, Relation, End, S0):- 
 related(Spatial, _Relation, Start, Here, S0),
 related(Spatial, exit(Dir), Here, End, S0),
 has_rel(Spatial, Relation, End, S0).


:- defn_state_getter(relative_dest(source,domrel,dest,source,target)).
relative_dest(Here, in, Dest, Src, Target, _State):- 
  Src = Here,
  ignore(Target = Dest).
relative_dest(Here, Exit, Dest, Src, Target, State):-  
  related(_Spatial, exit(Exit), Here, Target, State),
   Src = Here,
   ignore(Target = Dest).
relative_dest(Agent, Prep, Dest, Src, Target, State):-
  related(_Spatial, child, Agent, Here, State),
  relative_dest(Here, Prep, Dest, Src, Target, State).


:- defn_state_setter(moveto(domain,inst,domrel,dest,list_of(places),msg)).
moveto(Spatial, Object, Prep, Dest, Vicinity, Msg) -->
 undeclare(h(Spatial, _, Object, Here)),
 declare(h(Spatial, Prep, Object, Dest)),
 queue_local_event(Spatial, [moved( Object, Here, Prep, Dest), msg(Msg)], Vicinity).

/*moveallto(_Spatial, [], _R, _D, _V, _M, S, S).
moveallto(Spatial, [Object|Tail], Relation, Destination, Vicinity, Msg, S0, S2) :-
 moveto(Spatial, Object, Relation, Destination, Vicinity, Msg, S0, S1),
 moveallto(Spatial, Tail, Relation, Destination, Vicinity, Msg, S1, S2).
*/
moveallto(_Spatial, [], _R, _D, _V, _M, S, S).
moveallto(Spatial, List, Relation, Destination, Vicinity, Msg, S0, S2) :-
 apply_map_state(moveto(Spatial),List,rest(Relation, Destination, Vicinity, Msg), S0,S2).

disgorge(Spatial, Container, Prep, Here, Vicinity, Msg, S0, S9) :-
 rapply_state(S0,S9,
  (findall(Inner, related(Spatial, child, Inner, Container), Contents),
   bugout('~p contained ~p~n', [Container, Contents], general),
   map_each_state(moveto(Spatial), Contents, Prep, Here, Vicinity, Msg))).

event_props(thrown( Thing, _Target, Prep, Here, Vicinity),
 [getprop(Thing, breaks_into(Broken)),
 bugout('object ~p is breaks_into~n', [Thing], general),
 undeclare(h(Spatial, _, Thing, _)),
 declare(h(Spatial, Prep, Broken, Here)),
 queue_local_event(Spatial, [transformed(Thing, Broken)], Vicinity),
 disgorge(Spatial, Thing, Prep, Here, Vicinity, 'Something falls out.')]).

thrown( Thing, _Target, Prep, Here, Vicinity) --> moveto(spatial, Thing, Prep, Here, Vicinity, 'Thrown.').

hit(Spatial, Target, _Thing, Vicinity) -->
 ignoreable((Spatial = spatial,
  getprop(Target, breaks_into(Broken)),
  bugout('target ~p is breaks_into~n', [Target], general),
  undeclare(h(Spatial, Prep, Target, Here)),
  queue_local_event(Spatial, [transformed(Target, Broken)], Vicinity),
  declare(h(Spatial, Prep, Broken, Here)),
  disgorge(Spatial, Target, Prep, Here, Vicinity, 'Something falls out.'))).


act_verb_thing_model_sense(Agent, Action, Verb, Thing, Spatial, Sense):- 
 never_equal(Sense,Thing, Agent),
 notrace(act_verb_thing_model_sense0(Agent, Action, Verb, Thing, Spatial, Sense)), !.

act_verb_thing_model_sense0(_Agent, Atom, Atom, _Target, spatial, Sense):- \+ compound(Atom), !, is_sense(Sense),!.
%act_verb_thing_model_sense0(_Agent, Action, _Look, _Star, _Spatial, _See):- assertion(ground(Action)),fail.

act_verb_thing_model_sense0(Agent, goto(Agent, _Walk, loc(Agent, _Dir, _Rel, Thing)), goto, Thing, spatial, see):-!.
act_verb_thing_model_sense0(Agent, look(Agent, spatial), look, *, spatial, see):-!.
act_verb_thing_model_sense0(Agent, look(Agent, spatial, spatial), look, *, spatial, see):-!.
act_verb_thing_model_sense0(Agent, look(Agent), look, *, spatial, see):-!.
act_verb_thing_model_sense0(Agent, look(Agent), look, *, spatial, Sense):- is_sense(Sense), !.
act_verb_thing_model_sense0(Agent, touch(Agent,Target), touch, Target, spatial, Sense):- is_sense(Sense), !.

act_verb_thing_model_sense0(Agent, Action, Verb, Thing, W1, Sense):-
  Action=..[Verb,Agent, W1|Rest],
  W1 == spatial, !,
  Action2=..[Verb,Agent|Rest],
  act_verb_thing_model_sense(Agent, Action2, Verb, Thing, _Spatial, Sense).
act_verb_thing_model_sense0(Agent, Action, Verb, Thing, Spatial, Sense):-
  Action=..[Verb,Agent, Sense|Rest],
  is_sense(Sense), !,
  Action2=..[Verb,Agent|Rest],
  act_verb_thing_model_sense0(Agent, Action2, Verb, Thing, Spatial, _Sense).
act_verb_thing_model_sense0(Agent, Action, Verb, Thing, Spatial, Sense):-
  Action=..[Verb,Agent, W1|Rest],
  atom(W1), atom_concat(W2, 'ly', W1), !,
  Action2=..[Verb,Agent, W2|Rest],
  act_verb_thing_model_sense0(Agent, Action2, Verb, Thing, Spatial, Sense).
act_verb_thing_model_sense0(Agent, Action, Verb, Thing, Spatial, Sense):-
  Action=..[Verb,Agent, Prep|Rest],
  preposition(Spatial, Prep), !,
  Action2=..[Verb,Agent|Rest],
  act_verb_thing_model_sense0(Agent, Action2, Verb, Thing, _Spatial, Sense).
act_verb_thing_model_sense0(Agent, Action, Verb, Thing, Spatial, Sense):-
  Action=..[Verb,Agent, Thing|_], !,
  act_verb_thing_model_sense0(Agent, Verb, _UVerb, _UThing, Spatial, Sense).
act_verb_thing_model_sense0(Agent, Action, Verb, '*', Spatial, Sense):-
 Action=..[Verb,Agent], dmust((action_sensory(Verb, Sense), action_model(Verb, Spatial))), !.


