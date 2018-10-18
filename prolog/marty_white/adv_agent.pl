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


each_live_agent(NewGoal, S0, S2) :-
 get_live_agents(List, S0),
 apply_all(List, NewGoal, S0, S2).

each_sensing_agent(Sense, NewGoal, S0, S2) :-
 dmust((get_sensing_objects(Sense, List, S0),
      List\==[],
      %dmsg(each_sensing_agent(Sense)=(List=NewGoal)),
 apply_all(List, NewGoal, S0, S2))).

each_agent(Precond, NewGoal, S0, S2) :-
 get_some_agents(Precond, List, S0),
 apply_all(List, NewGoal, S0, S2).



% -----------------------------------------------------------------------------
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CODE FILE SECTION
:- nop(ensure_loaded('adv_agent_model')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Protocol:
%   Agent: request(Action, Action_Id)
%   Simulation: respond(Action_Id, LogicalResponse/Percept, EnglishResponse)
%   Action(Verb, ...)
%   failure(Reason)
%   moved(Spatial, obj, from, how, to)

% -----------------------------------------------------------------------------
% The state of an Agent is stored in its memory.
% Agent memory is stored as a list in reverse chronological order, implicitly
%   ordering and timestamping everything.
% Types of memories:
%   inst(A)        - identity of agent (?)
%   timestamp(T)    - agent may add a new timestamp whenever a sequence point
%                     is desired.
%   [percept]       - received perceptions.
%   model(Spatial, [...])    - Agent's internal model of the Spatial world.
%                     Model is a collection of timestampped relations.
%   goals([...])    - states the agent would like to achieve, or
%                     acts the agent would like to be able to do.
%   plan(S, O, B, L)   - plans for achieving goals.
%   affect(...)     - Agent's current affect.
% Multiple plans, goals, models, affects, etc. may be stored, for introspection
%   about previous internal states.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CODE FILE SECTION
:- nop(ensure_loaded('adv_agent_goal')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_goal(Goal, Mem0, Mem2) :-
  bugout('adding goal ~w~n', [Goal], planner),
  forget(goals(OldGoals), Mem0, Mem1),
  append([Goal], OldGoals, NewGoals),
  memorize(goals(NewGoals), Mem1, Mem2).

add_goals(Goals, Mem0, Mem2) :-
  forget(goals(OldGoals), Mem0, Mem1),
  append(Goals, OldGoals, NewGoals),
  memorize(goals(NewGoals), Mem1, Mem2).

add_todo(Auto, Mem0, Mem3) :- Auto = a,
  member(inst(Agent), Mem0),
  autonomous_decide_goal_action(Agent, Mem0, Mem3),!.
add_todo(Action, Mem0, Mem2) :-
  forget(todo(OldToDo), Mem0, Mem1),
  append(OldToDo, [Action], NewToDo),
  memorize(todo(NewToDo), Mem1, Mem2).

add_todo_all([], Mem0, Mem0).
add_todo_all([Action|Rest], Mem0, Mem2) :-
  add_todo(Action, Mem0, Mem1),
  add_todo_all(Rest, Mem1, Mem2).



% -----------------------------------------------------------------------------

% do_introspect(Query, Answer, Memory)
do_introspect(path(Spatial, There), Answer, Memory) :-
  thought(inst(Agent), Memory),
  thought_model(Spatial,ModelData, Memory),
  in_model(h(Spatial, _How, Agent, Here, _T), ModelData),
  find_path(Spatial, Here, There, Route, ModelData),
  Answer = ['Model is', ModelData, '\nShortest path is', Route].

do_introspect(whereis(Spatial, X), Answer, Memory) :-
  remember_whereis(Spatial, X, Answer, Memory).
do_introspect(whereis(Spatial, X), Answer, Memory) :-
  thought(inst(Agent), Memory),
  sensory_model(Sense, Spatial),
  Answer = [subj(Agent), person('don\'t', 'doesn\'t'),
            'recall ever ', ing(Sense), ' a "', X, '".'].

do_introspect(whois(Spatial, X), Answer, Memory) :-
  remember_whereis(Spatial, X, Answer, Memory).
do_introspect(whois(_Spatial, X), [X, is, X, .], _Memory).

do_introspect(whatis(Spatial, X), Answer, Memory) :-
  remember_whereis(Spatial, X, Answer, Memory).
do_introspect(whatis(_Spatial, X), [X, is, X, .], _Memory).



remember_whereis(Spatial, Thing, Answer, Memory) :-
  thought(inst(Agent), Memory),
  thought_model(Spatial,ModelData, Memory),
  in_model(h(Spatial, How, Thing, Where, T), ModelData),
  How \= exit(_),
  Answer = ['At time', T, subj(Agent), 'saw the', Thing, How, the, Where, .].
remember_whereis(Spatial, Here, Answer, Memory) :-
  thought(inst(Agent), Memory),
  thought_model(Spatial,ModelData, Memory),
  in_model(h(Spatial, _How, Agent, Here, _T), ModelData),
  Answer = 'Right here.'.
remember_whereis(Spatial, There, Answer, Memory) :-
  thought(inst(Agent), Memory),
  thought_model(Spatial,ModelData, Memory),
  in_model(h(Spatial, _How, Agent, Here, _T), ModelData),
  find_path(Spatial, Here, There, Route, ModelData),
  Answer = ['To get to the', There, ', ', Route].
remember_whereis(Spatial, There, Answer, Memory) :-
  thought_model(Spatial,ModelData, Memory),
  ( in_model(h(Spatial, exit(_), _, There, _T), ModelData);
    in_model(h(Spatial, exit(_), There, _, _T), ModelData)),
  Answer = 'Can''t get there from here.'.



% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CODE FILE SECTION
:- dbug(ensure_loaded('adv_agents')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Telnet client
decide_action(Agent, Mem0, Mem1) :-
  notrace(thought(inherit(telnet,t), Mem0)),!,
  dmust(telnet_decide_action(Agent, Mem0, Mem1)).


% Stdin Client
decide_action(Agent, Mem0, Mem1) :-
  notrace(thought(inherit(console,t), Mem0)),!,
  current_input(In), % agent_to_input(Agent,In),
  (tracing->catch(sleep(3),_,(nortrace,notrace,break));true),
  wait_for_input([In,user_input],Found,0.5),
  
  % read_pending_codes(In,Codes,Missing), 
  (Found==[] -> (Mem0=Mem1) ; 
  ((  
  thought(timestamp(T0), Mem0),
  %dmsg(read_pending_codes(In,Codes,Found,Missing)),
  repeat,
   notrace((ttyflush,
    player_format('[~p: ~p] ==> ', [T0, Agent]), ttyflush,
    agent_to_input(Agent,In),
    dmust(is_stream(In)),
    readtokens(In,[], Words0),
    read_pending_input(In,_,[]),
    (Words0==[]->Words=[wait];Words=Words0))),
    parse(Words, Action, Mem0),
    !,
  (Action =.. Words; player_format('~w~n', [Action])),
  add_todo(Action, Mem0, Mem1), ttyflush))), !.

% Autonomous
decide_action(Agent, Mem0, Mem3) :-
  thought(inherit(autonomous,t), Mem0),
  maybe_autonomous_decide_goal_action(Agent, Mem0, Mem3).

decide_action(_Agent, Mem, Mem) :-
  thought(inherit(memorize,t), Mem), !.  % recorders don't decide much.
decide_action(Agent, Mem0, Mem0) :-
  bugout('decide_action(~w) FAILED!~n', [Agent], general).


run_agent_pass_1(Agent, S0, S) :-
  with_agent_console(Agent,run_agent_pass_1_0(Agent, S0, S)).
run_agent_pass_1(Agent, S0, S0) :-
  bugout('run_agent_pass_1(~w) FAILED!~n', [Agent], general).

run_agent_pass_2(Agent, S0, S) :-
  with_agent_console(Agent,run_agent_pass_2_0(Agent, S0, S)).
run_agent_pass_2(Agent, S0, S0) :-
  bugout('run_agent_pass_2(~w) FAILED!~n', [Agent], general).



/*
with_agent_console(Agent,Goal):- 
   adv:console_info(Id,Alias,InStream,OutStream, Host, Peer, Agent),
   nop(adv:console_info(Id,Alias,InStream,OutStream, Host, Peer, Agent)),
   current_input(WasIn),
   InStream\==WasIn,!,
   setup_call_cleanup(set_input(InStream),with_agent_console(Agent,Goal),set_input(WasIn)).
*/
with_agent_console(Agent,Goal):- 
  setup_call_cleanup(
     asserta(adv:current_agent(Agent),E),
     Goal,erase(E)),!.

run_agent_pass_1_0(Agent, S0, S) :-
  must_input_state(S0),
  %dmust((
  undeclare(memories(Agent, Mem0), S0, S1),
  undeclare(perceptq(Agent, PerceptQ), S1, S2),
  thought(timestamp(T0), Mem0),  
  (PerceptQ==[] -> (T1 is T0 + 0, Mem0 = Mem1) ;  (T1 is T0 + 1, memorize(timestamp(T1), Mem0, Mem1))), 
  process_percept_list(Agent, PerceptQ, T1, Mem1, Mem2),
  memorize_list(PerceptQ, Mem2, Mem3),
  decide_action(Agent, Mem3, Mem4),
  declare(memories(Agent, Mem4), S2, S3),
  declare(perceptq(Agent, []), S3, S4),
  % dmsg(timestamp(Agent, T1)),
  apply_first_arg_state(Agent, do_todo(), S4, S),
  % pprint(S, general),
  
  notrace(must_output_state(S)),!.
  
run_agent_pass_2_0(_Agent, S0, S0):-!.
run_agent_pass_2_0(Agent, S0, S) :-
  must_input_state(S0),
  apply_first_arg_state(Agent, do_todo(), S0, S),
  notrace(must_output_state(S)),!.
  
% --------

