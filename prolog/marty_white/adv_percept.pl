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

get_sensing_objects(Objects, S0):-
 setof(O,member(perceptq(O,_),S0),Objects).

get_sensing_objects(Sense, Agents, S0):-
 get_objects((has_sense(Sense);inherited(memorize)), Agents, S0).

get_live_agents(LiveAgents, S0):-
 get_some_agents( \+ state(_Spatial, powered, f), LiveAgents, S0).

get_some_agents(Precond, LiveAgents, S0):-
 dmust((
  get_objects(  
  (inherited(character),Precond), LiveAgents, S0),
 LiveAgents = [_|_])).



is_prop_public(_,P) :-
 \+ \+ 
 member(P, [
    name(_),
    desc(_),
    fragile(_),emitting(_,_Light), 
    %has_rel(_Spatial, _), 
    
    can_be(eat, _), 
    can_be(move, _), 
    can_be(open, _), state(open, _), 
    can_be(lock, t), state(locked, _),
    inherit(shiny,t)]).
is_prop_public(_,Prop):- is_prop_nonpublic(Prop),!,fail.

is_prop_public(_,_):-!.


is_prop_nonpublic(has_sense(_)).
is_prop_nonpublic(_):- !, fail.

has_sensory(Spatial, Sense, Agent, State) :-
 sensory_model_problem_solution(Sense, Spatial, TooDark, EmittingLight),
 get_open_traverse(_Open, Sense, _Traverse, Spatial, OpenTraverse),
 related(Spatial, OpenTraverse, Agent, Here, State),
 getprop(Here, TooDark, State) , 
 \+ related_with_prop(Spatial, OpenTraverse, _Obj, Here, EmittingLight, State), !, fail.
has_sensory(_Spatial, _Sense, _Agent, _State) .


can_sense( _See, Star, _Agent, _State) :- Star == '*', !.
can_sense( Sense, Thing, Agent, State) :-
 get_open_traverse(_Open, Sense, _Traverse, Spatial, OpenTraverse),
 has_sensory(Spatial, Sense, Agent, State),
 related(Spatial, OpenTraverse, Agent, Here, State),
 (Thing=Here; related(Spatial, OpenTraverse, Thing, Here, State)).
can_sense( Sense, Thing, Agent, _State):- 
 bugout(pretending_can_sense( Sense, Thing, Agent)),!.


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_events')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Manipulate one agents percepts
queue_agent_percept(Agent, Event, S0, S2) :- 
 \+ is_list(Event),!, 
 queue_agent_percept(Agent, [Event], S0, S2).
queue_agent_percept(Agent, Events, S0, S2) :-
 dmust((select(perceptq(Agent, Queue), S0, S1),
 append(Queue, Events, NewQueue),
 append([perceptq(Agent, NewQueue)], S1, S2))).

queue_event(Event, S0, S2) :-
 each_sensing_agent(_All, queue_agent_percept(Event), S0, S2).


% Room-level simulation percepts
queue_local_percept(Agent, Spatial, Event, Places, S0, S1) :-
 ignore(current_spatial(Spatial)),
 member(Where, Places),
 ((get_open_traverse(look, Spatial, OpenTraverse), related(Spatial, OpenTraverse, Agent, Where, S0));Where=Agent),
 queue_agent_percept(Agent, Event, S0, S1),!.
queue_local_percept(_Agent, _Spatial, _Event, _Places, S0, S0).


queue_local_event(Spatial, Event, Places, S0, S2) :- 
 each_sensing_agent(_All, queue_local_percept(Spatial, Event, Places), S0, S2).



/*

sensory_model(olfactory, spatial).
sensory_model(taste, spatial).
sensory_model(tactile, spatial).

sensory_model(sixth, _).
*/


current_spatial(spatial).


is_sense(X):- sensory_model(X, _).

sensory_model(see, spatial).
sensory_model(hear, spatial).
sensory_model(taste, spatial).
sensory_model(smell, spatial).
sensory_model(feel, spatial).

action_model(_, spatial).

sensory_verb(see, look).
sensory_verb(hear, listen).
sensory_verb(taste, taste).
sensory_verb(smell, smell).
sensory_verb(feel, touch).


action_sensory(Action, Sense):-
 compound(Action),
 Action=..[_Verb, Sensory|_],
 is_sense(Sensory), !,
 Sense=Sensory.
action_sensory(Action, Sense):-
 compound(Action),
 Action=..[Verb|_],
 verb_sensory(Verb, Sense).
action_sensory(Action, Sense):- verb_sensory(Action, Sense) *-> true; Sense=see.


 % sensory_model(Spatial1, Spatial2):- Spatial1 == Spatial2, !.

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



% sensory_model(Visual, Spatial, TooDark, EmittingLight))
sensory_model_problem_solution(Sense, Spatial, state(Dark, t), emitting(Sense, Light)):-
 problem_solution(Dark, Sense, Light), sensory_model(Sense, Spatial).

problem_solution(dark, see, light).
problem_solution(stinky, smell, pure).
problem_solution(noisy, hear, quiet).



% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_agent_percepts')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Autonomous logical percept processing.
%process_percept_auto(Agent, with_msg(Percept, _Msg), Timestamp, M0, M2) :- !, 
% process_percept_auto(Agent, Percept, Timestamp, M0, M2).

process_percept_auto(_Agent, msg(_), _Stamp, Mem0, Mem0) :- !.
process_percept_auto(_Agent, [], _Stamp, Mem0, Mem0) :- !.
process_percept_auto(Agent, [Percept|Tail], Stamp, Mem0, Mem4) :-
 process_percept_auto(Agent, Percept, Stamp, Mem0, Mem1),
 process_percept_auto(Agent, Tail, Stamp, Mem1, Mem4).

process_percept_auto(Agent, Percept, _Stamp, Mem0, Mem0) :- was_own_self(Agent, Percept),!.

process_percept_auto(_Agent2, sense_each(Agent,_See, List), Timestamp, M0, M2) :- !, 
 process_percept_auto(Agent, List, Timestamp, M0, M2).

% Auto examine room items
process_percept_auto(Agent, notice_children(Agent, Sense, _Here, _Prep, Objects), _Stamp, Mem0, Mem2) :- 
 thought_model(ModelData, Mem0),
 findall( examine(Agent, Sense, Obj),
   ( member(Obj, Objects),
   \+ member(holds_at(props(Obj, _), _), ModelData)),
   ExamineNewObjects),
 add_todo_all(ExamineNewObjects, Mem0, Mem2).

process_percept_auto(_Agent, _Percept, _Timestamp, M0, M0):-  \+ declared(inherited(autonomous), M0),!.

% Auto Answer
process_percept_auto(Agent, emoted(Speaker,  _Say, Agent, Words), _Stamp, Mem0, Mem1) :-
 trace, consider_text(Speaker, Agent, Words, Mem0, Mem1).
process_percept_auto(Agent, emoted(Speaker,  _Say, (*), WordsIn), _Stamp, Mem0, Mem1) :-
 addressing_whom(WordsIn, Whom, Words),
 Whom == Agent,
 consider_text(Speaker, Agent, Words, Mem0, Mem1).

% Auto take
process_percept_auto(Agent, sense_props(Agent, Sense, Object, PropList), _Stamp, Mem0, Mem2) :-
 bugout('~w: ~p~n', [Agent, sense_props(Agent, Sense, Object, PropList)], autonomous),
 (member(inherited(shiny), PropList)),
 Object \== Agent,
 thought_model(ModelData, Mem0),
 \+ related(_Spatial, descended, Object, Agent, ModelData), % Not holding it? 
 add_todo_all([take(Agent, Object), print_(Agent, 'My shiny precious!')], Mem0, Mem2).


process_percept_auto(_Agent, _Percept, _Stamp, Mem0, Mem0).


%was_own_self(Agent, say(Agent, _)).
was_own_self(Agent, emote(Agent, _, _Targ, _)).
was_own_self(Agent, emoted(Agent, _, _Targ, _)).

% Ignore own speech.
process_percept_player(Agent,Percept, _Stamp, Mem0, Mem0) :- was_own_self(Agent, Percept),!.

process_percept_player(Agent, _Percept, _Stamp, Mem0, Mem0) :- \+ is_player(Agent),!.
process_percept_player(Agent, Percept, _Stamp, Mem0, Mem0) :-
 percept2txt(Agent, Percept, Text),
 player_format('~N~w~n', [Text]),!,
 redraw_prompt(Agent).
process_percept_player(Agent, Percept, _Stamp, Mem0, Mem0) :-
 player_format('~N~q~n', [Agent:Percept]),
 dmust(redraw_prompt(Agent)),!.

% once(( notrace((thought(inherited(console), Mem0);thought(inherit(player,t), Mem0);thought(inherit(telnet,t), Mem0))).
is_player(Agent):- \+ is_non_player(Agent).
is_non_player(Agent):- Agent == 'floyd~1'.


% process_percept_main(Agent, PerceptsList, Stamp, OldModel, NewModel)
process_percept_main(_Agent, [], _Stamp, Mem0, Mem0) :- !.
process_percept_main(Agent, [Percept|Tail], Stamp, Mem0, Mem4) :-
 process_percept_main(Agent, Percept, Stamp, Mem0, Mem1),
 process_percept_main(Agent, Tail, Stamp, Mem1, Mem4).
process_percept_main(Agent, Percept, Stamp, Mem0, Mem2) :-
 quietly(process_percept_player(Agent, Percept, Stamp, Mem0, Mem1)),
 process_percept_auto(Agent, Percept, Stamp, Mem1, Mem2).
process_percept_main(Agent, Percept, Stamp, Mem0, Mem0):- 
 bugout('~q FAILED!~n', [bprocess_percept(Agent, Percept, Stamp)], todo), !.


% caller memorizes PerceptList
process_percept_list(_Agent, _, _Stamp, Mem, Mem) :-
 declared(inherited(no_perceptq), Mem),
 !.
process_percept_list(Agent, Percept, Stamp, Mem0, Mem3) :-
 dmust((
 forget(model(Model0), Mem0, Mem1),
 update_model(Agent, Percept, Stamp, Mem1, Model0, Model1),
 memorize(model(Model1), Mem1, Mem2),
 process_percept_main(Agent, Percept, Stamp, Mem2, Mem3))),!.
process_percept_list(_Agent, Percept, _Stamp, Mem0, Mem0) :-
 bugout('process_percept_list(~w) FAILED!~n', [Percept], todo), !.




