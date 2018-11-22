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

%:- nop(ensure_loaded('adv_chat80')).
%:- ensure_loaded(adv_main).
%:- endif.

%:- user:listing(adventure).


:- defn_state_getter(get_sensing_objects//1).
get_sensing_objects(Objects, S0):-
 setof(O,member(perceptq(O,_),S0),Objects).

get_sensing_objects(Sense, Agents, S0):-
 get_objects((has_sense(Sense);inherits(memorize)), Agents, S0).

:- defn_state_getter(get_live_agents(-listof(agnt))).
get_live_agents(LiveAgents, S0):-
 get_some_agents( \+ state(powered, f), LiveAgents, S0).

:- defn_state_getter(get_some_agents(conds,-listof(agnt))).
get_some_agents(Precond, LiveAgents, S0):-
 dmust((
  get_objects(  
  (inherits(character),Precond), LiveAgents, S0),
  LiveAgents \== [])).                



is_prop_public(_,P) :-
  member(P, [has_rel(_),
     emits_light, can_be(eat,t), name(_), desc(_), breaks_into(_),
             can_be(move, f), openable, open, closed(_), lockable, locked, locked(_),
             shiny]).
is_prop_public(_,Prop):- is_prop_nonpublic(Prop),!,fail.
is_prop_public(_,P) :-
 \+ \+ 
 member(P, [                            
    name(_),
    desc(_),
    breaks_into(_),emitting(_,_Light), 
    %has_rel(_), 
    
    can_be(eat, _), 
    can_be(move, _), 
    can_be(open, _), state(open, _), 
    can_be(lock, t), state(locked, _),
    inherit(shiny,t)]).            

is_prop_public(_,_):-!.

is_prop_nonpublic(P):- \+ callable(P),!,fail.
is_prop_nonpublic(inherit(_,f)):- !,fail.
is_prop_nonpublic(P):- compound(P),functor(P,F,_),!,is_prop_nonpublic(F).
is_prop_nonpublic(has_sense).
is_prop_nonpublic(has_rel).                            
is_prop_nonpublic(effect).
is_prop_nonpublic(oper).
is_prop_nonpublic(co).
is_prop_nonpublic(class_desc).
is_prop_nonpublic(inherits).
is_prop_nonpublic(knows_verbs).
is_prop_nonpublic(can_be).
is_prop_nonpublic(breaks_into).
is_prop_nonpublic(before).
is_prop_nonpublic(after).


sense_here(_Sense, _Here, _S0):-!.
sense_here(Sense, Here, S0):- 
 getprop(Here, TooDark, S0),
 (sensory_problem_solution(Sense, TooDark, EmittingLight) -> 
   related_with_prop(Sense, _Obj, Here, EmittingLight, S0) ; true).

can_sense_here(Agent, Sense, S0) :-
 from_loc(Agent, Here, S0),
 sense_here(Sense, Here, S0), !.
can_sense_here(_Agent, _Sense, _State) .

is_star(Star):- Star == '*'.
is_star('*'(Star)):- nonvar(Star).

can_sense(Agent, Sense, Thing, S0, S9):- can_sense(Agent, Sense, Thing, S0),S9=S0.
:- defn_state_getter(can_sense(agent,sense,thing)).
can_sense(_Agent, _See, Star, _State) :- is_star(Star), !.
can_sense(Agent, Sense, Thing, S0) :- Agent == Thing, !, can_sense_here(Agent, Sense, S0).
can_sense(_Agent, Sense, Here, S0) :- 
  getprop(Here, has_rel(exit(_),t), S0), 
  sense_here(Sense, Here, S0),!.

can_sense(Agent, Sense, Thing, S0) :-
  can_sense_here(Agent, Sense, S0),
  from_loc(Agent, Here, S0),
  (Thing=Here;  open_traverse(Thing, Here, S0)), !.
/*can_sense(Agent, Sense, Thing, S0) :-
 % get_open_traverse(_Open, Sense, _Traverse, Sense),
 can_sense_here(Agent, Sense, S0),
 h(Sense, Agent, Here, S0),
 (Thing=Here; h(Sense, Thing, Here, S0)).
*/
can_sense(Agent, Sense, Thing, _State):- 
 bugout(pretending_can_sense(Agent, Sense, Thing, Agent)),!.




% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_events')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- defn_state_setter(queue_agent_percept//2).
% Manipulate one agents percepts
queue_agent_percept(Agent, Event, S0, S2) :- 
 \+ is_list(Event),!, 
 queue_agent_percept(Agent, [Event], S0, S2).
queue_agent_percept(Agent, Events, S0, S2) :-
 dmust((select(perceptq(Agent, Queue), S0, S1),
 append(Queue, Events, NewQueue),
 append([perceptq(Agent, NewQueue)], S1, S2))).


:- defn_state_setter(queue_event//1).
queue_event(Event, S0, S2) :-
 each_sensing_agent(_All, queue_agent_percept(Event), S0, S2).


:- defn_state_setter(queue_local_agent_percept//4).
% Room-level simulation percepts
queue_local_agent_percept(Agent, Event, Places, S0, S1) :-
 member(Where, Places),can_sense(Agent,_,Where,S0),!,
 queue_agent_percept(Agent, Event, S0, S1),!.
queue_local_agent_percept(_Agent, _Event, _Places, S0, S0).


queue_local_event(Event, Places, S0, S2) :- 
 each_sensing_agent(_All, queue_local_agent_percept(Event, Places), S0, S2).




is_sense(X):- sensory_verb(X, _).

sensory_verb(see, look).
sensory_verb(hear, listen).
sensory_verb(taste, taste).
sensory_verb(smell, smell).
sensory_verb(touch, feel).


action_sensory(Action, Sense):-
 compound(Action),
 Action=..[_Verb, Sensory|_],
 is_sense(Sensory), !,
 Sense=Sensory.
action_sensory(Action, Sense):-
 compound(Action),
 Action=..[Verb|_],
 verb_sensory(Verb, Sense).
action_sensory(Action, Sense):- 
 verb_sensory(Action, Sense) *-> true; Sense=see.


% listen->hear
verb_sensory(goto, Sense):- is_sense(Sense).
verb_sensory(examine, Sense):- is_sense(Sense).
verb_sensory(wait, Sense):- is_sense(Sense).
verb_sensory(print_, Sense):- is_sense(Sense).
verb_sensory(Verb, Sense):- sensory_verb(Sense, Verb).
verb_sensory(look, see).
verb_sensory(say, hear).
verb_sensory(eat, taste).
verb_sensory(feel, touch).
verb_sensory(goto, see).
verb_sensory(Verb, Sense):- nonvar(Verb), is_sense(Verb), Sense=Verb.
verb_sensory(Verb, Sense):- subsetof(Verb, Verb2), Verb\=Verb2,
 verb_sensory(Verb2, Sense), \+ is_sense(Verb).
verb_sensory(Verb, Sense):- verb_alias(Verb, Verb2), Verb\=Verb2,
 verb_sensory(Verb2, Sense), \+ is_sense(Verb).



% sensory_model(Visual, TooDark, EmittingLight))
sensory_problem_solution(Sense, state(Dark, t), emitting(Sense, Light)):-
 problem_solution(Dark, Sense, Light).

problem_solution(dark, see, light).
problem_solution(stinky, smell, pure).
problem_solution(noisy, hear, quiet).


%percept_todo(Actions, Mem0, Mem2):- apply_all(Actions,add_goal(),Mem0, Mem2).
:- defn_state_setter(percept_todo//1).
percept_todo(Actions, Mem0, Mem2):- add_todo_all(Actions, Mem0, Mem2),!.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_agent_percepts')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Autonomous logical percept processing.
%process_percept_auto(Agent, with_msg(Percept, _Msg), Timestamp, M0, M2) :- !, 
% process_percept_auto(Agent, Percept, Timestamp, M0, M2).

:- defn_state_setter(process_percept_auto//3).
process_percept_auto(_Agent, msg(_), _Stamp, M0, M0) :- !.
process_percept_auto(_Agent, [], _Stamp, M0, M0) :- !.
process_percept_auto(Agent, [Percept|Tail], Stamp, M0, M9) :-
 process_percept_auto(Agent, Percept, Stamp, M0, M1),
 process_percept_auto(Agent, Tail, Stamp, M1, M9).

process_percept_auto(Agent, Percept, _Stamp, M0, M0) :- was_own_self(Agent, Percept),!.

% Auto examine room items
process_percept_auto(Agent, notice_children(Agent, Sense, _Here, _Prep, Depth, Objects), _Stamp, Mem0, Mem2) :- 
 agent_thought_model(Agent, ModelData, Mem0),
 Depth = depth(DepthN),
 DepthN > 1, DepthLess is DepthN - 1,
 findall( examine(Agent, Sense, Obj, depth(DepthLess)),
   ( member(Obj, Objects),    
   \+ member(holds_at(props(Obj, _), _), ModelData)),
   Actions),
 percept_todo(Actions, Mem0, Mem2).

process_percept_auto(_Agent, _Percept, _Timestamp, M0, M0):-  \+ declared(inherits(autonomous), M0),!.

% Auto Answer
process_percept_auto(Agent, emoted(Speaker,  EmoteType, Agent, Words), _Stamp, Mem0, Mem1) :-
 trace, consider_text(Speaker,EmoteType, Agent, Words, Mem0, Mem1).
process_percept_auto(Agent, emoted(Speaker,  EmoteType, Star, WordsIn), _Stamp, Mem0, Mem1) :- is_star(Star),
 addressing_whom(WordsIn, Whom, Words),
 Whom == Agent,
 consider_text(Speaker,EmoteType, Agent, Words, Mem0, Mem1).

% Auto take
process_percept_auto(Agent, sense_props(Agent, Sense, Object, Depth, PropList), _Stamp, Mem0, Mem2) :-
 Depth = depth(DepthN),
 DepthN > 1, 
 (member(inherits(shiny), PropList)),
 Object \== Agent,
 bugout('~w: ~p~n', [Agent, sense_props(Agent, Sense, Object, Depth, PropList)], autonomous),
 agent_thought_model(Agent,ModelData, Mem0),
 \+ h(descended, Object, Agent, ModelData), % Not holding it? 
 add_todo_all([take(Agent, Object), print_(Agent, 'My shiny precious!')], Mem0, Mem2).


process_percept_auto(_Agent, _Percept, _Stamp, M0, M0).

addressing_whom(List, Agent, Words):- Words = [_|_], append(Words,[Agent],List).
addressing_whom(List, Agent, Words):- Words = [_|_], append(_,[Agent|Words],List).


%was_own_self(Agent, say(Agent, _)).
was_own_self(Agent, emote(Agent, _, _Targ, _)).
was_own_self(Agent, emoted(Agent, _, _Targ, _)).
% was_own_self(Agent, Action):- action_doer(Action, Was), Was == Agent.

:- defn_state_setter(process_percept_player//3).
% Ignore own speech.
process_percept_player(Agent, _Percept, _Stamp, Mem0, Mem0) :- \+ is_player(Agent),!.
process_percept_player(_, [], _Stamp, Mem0, Mem0) :- !.
process_percept_player(Agent, [Percept|Tail], Stamp, Mem0, Mem4) :- !,
 process_percept_player(Agent, Percept, Stamp, Mem0, Mem1),
 process_percept_player(Agent, Tail, Stamp, Mem1, Mem4).
process_percept_player(Agent,Percept, _Stamp, Mem0, Mem0) :- was_own_self(Agent, Percept),!.
process_percept_player(Agent, Percept, _Stamp, Mem0, Mem0) :-
 percept2txt(Agent, Percept, Text),!, player_format('~N~w~n', [Text]),!.

process_percept_player(Agent, Percept, _Stamp, M0, M0) :-
 player_format(Agent, '~N~q~n', [Agent:Percept]).

is_player(Agent):- \+ is_non_player(Agent).
is_non_player(Agent):- Agent == 'floyd~1'.


:- defn_state_setter(process_percept_main//3).
% process_percept_main(Agent, PerceptsList, Stamp, OldModel, NewModel)
process_percept_main(_Agent, [], _Stamp, Mem0, Mem0) :- !.
process_percept_main(Agent, Percept, Stamp, Mem0, Mem2) :-
 quietly(process_percept_player(Agent, Percept, Stamp, Mem0, Mem1)),
 process_percept_auto(Agent, Percept, Stamp, Mem1, Mem2).
process_percept_main(Agent, Percept, Stamp, Mem0, Mem0):- 
 bugout('~q FAILED!~n', [bprocess_percept(Agent, Percept, Stamp)], perceptq), !.


:- defn_state_setter(process_percept_list//3).
% caller memorizes PerceptList
process_percept_list(_Agent, _, _Stamp, Mem, Mem) :-
 declared(inherits(no_perceptq), Mem),
 !.
process_percept_list(Agent, Percept, Stamp, Mem0, Mem3) :-
 dmust((
 forget(model(Model0), Mem0, Mem1),
 update_model(Agent, Percept, Stamp, Mem1, Model0, Model1),
 memorize(model(Model1), Mem1, Mem2),
 process_percept_main(Agent, Percept, Stamp, Mem2, Mem3))),!.
process_percept_list(_Agent, Percept, _Stamp, Mem0, Mem0) :-
 bugout('process_percept_list(~w) FAILED!~n', [Percept], todo), !.




