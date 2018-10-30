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
      %dbug(each_sensing_agent(Sense)=(List=NewGoal)),
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
%   moved( obj, from, how, to)

% -----------------------------------------------------------------------------
% The state of an Agent is stored in its memory.
% Agent memory is stored as a list in reverse chronological order, implicitly
%   ordering and timestamping everything.
% Types of memories:
%   inst(A)        - identity of agent (?)
%   timestamp(T)    - agent may add a new timestamp whenever a sequence point
%                     is desired.
%   [percept]       - received perceptions.
%   model([...])    - Agent's internal model of the Spatial world.
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

add_todo(Auto, Mem0, Mem3) :- Auto = auto,
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
do_introspect(path(There), Answer, Memory) :-
  thought(inst(Agent), Memory),
  thought_model(ModelData, Memory),
  in_model(h(Spatial, _Prep, Agent, Here, _T), ModelData),
  find_path(Spatial, Here, There, Route, ModelData),
  Answer = ['Model is', ModelData, '\nShortest path is', Route].


do_introspect(whereis(X), Answer, Memory) :-
  remember_whereis(X, Answer, Memory).
do_introspect(whereis(X), Answer, Memory) :- !,
  thought(inst(Agent), Memory),
  sensory_model(Sense, spatial),
  Answer = [subj(Agent), person('don\'t', 'doesn\'t'),
            'recall ever ', ing(Sense), ' a "', X, '".'].

do_introspect(whois(X), Answer, Memory) :-
  remember_whereis(X, Answer, Memory).
do_introspect(whois( X), [X, is, X, .], _Memory).

do_introspect(whatis( X), Answer, Memory) :-
  remember_whereis(X, Answer, Memory).
do_introspect(whatis( X), [X, is, X, .], _Memory).



remember_whereis(Thing, Answer, Memory) :-
  thought(inst(Agent), Memory),
  thought_model(ModelData, Memory),
  in_model(h(_Spatial, Prep, Thing, Where, T), ModelData),
  Prep \= exit(_),
  Answer = ['At time', T, subj(Agent), 'saw the', Thing, Prep, the, Where, .].
remember_whereis(Here, Answer, Memory) :-
  thought(inst(Agent), Memory),
  thought_model(ModelData, Memory),
  in_model(h(_Spatial, _Prep, Agent, Here, _T), ModelData),
  Answer = 'Right here.'.
remember_whereis(There, Answer, Memory) :-
  thought(inst(Agent), Memory),
  thought_model(ModelData, Memory),
  in_model(h(Spatial, _Prep, Agent, Here, _T), ModelData),
  find_path(Spatial, Here, There, Route, ModelData),
  Answer = ['To get to the', There, ', ', Route].
remember_whereis(There, Answer, Memory) :-
  thought_model(ModelData, Memory),
  (in_model(h(_Spatial, exit(_), _, There, _T), ModelData);
    in_model(h(_Spatial, exit(_), There, _, _T), ModelData)),
  Answer = 'Can''t get there from here.'.



console_decide_action(Agent, Mem0, Mem1):- 
  %thought(timestamp(T0), Mem0),
  %dbug(read_pending_codes(In,Codes,Found,Missing)),
  repeat,
   notrace((ttyflush,
    %player_format('[~p: ~p] ==> ', [T0, Agent]), ttyflush,
    agent_to_input(Agent,In),
    dmust(is_stream(In)),
    setup_console,
    read_line_to_tokens(Agent, In,[], Words0),    
    (Words0==[]->(Words=[wait],makep);Words=Words0))),
    parse(Words, Action, Mem0),                       
    !,
  (Action =.. Words; player_format('~w~n', [Action])),
  add_todo(Action, Mem0, Mem1), ttyflush, !.

makep:- 
 locally(set_prolog_flag(verbose_load,true),
   with_no_dmsg(make:((
        
        '$update_library_index',
    findall(File, make:modified_file(File), Reload0),
    list_to_set(Reload0, Reload),
    (   prolog:make_hook(before, Reload)
    ->  true
    ;   true
    ),
    print_message(silent, make(reload(Reload))),
    maplist(reload_file, Reload),
    print_message(silent, make(done(Reload))),
    (   prolog:make_hook(after, Reload)
    ->  true
    ;   nop(list_undefined),
        nop(list_void_declarations)
    ))))).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CODE FILE SECTION
:- dbug(ensure_loaded('adv_agents')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Telnet client
decide_action(Agent, Mem0, Mem1) :-
  notrace(declared(inherited(telnet), Mem0)),!,
  dmust(telnet_decide_action(Agent, Mem0, Mem1)).

% Stdin Client
decide_action(Agent, Mem0, Mem1) :-
  notrace(declared(inherited(console), Mem0)),!,
 % agent_to_input(Agent,In),
   (tracing->catch(sleep(3),_,(nortrace,notrace,break));true),

   (current_input(In), wait_for_input([In,user_input],Found,0.1)),
                  %   Found = [some],
  % read_pending_codes(In,Codes,Missing), 
  (Found==[] -> (Mem0=Mem1) ; 
    (((console_decide_action(Agent, Mem0, Mem1))))).

% Autonomous
decide_action(Agent, Mem0, Mem3) :-
  declared(inherited(autonomous), Mem0),
  maybe_autonomous_decide_goal_action(Agent, Mem0, Mem3).

decide_action(_Agent, Mem, Mem) :-
  declared(inherited(memorize), Mem), !.  % recorders don't decide much.
decide_action(Agent, Mem0, Mem0) :-
  set_last_action(Agent,[auto]),
  nop(bugout('decide_action(~w) FAILED!~n', [Agent], general)).


:- meta_predicate with_agent_console(*,0).
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


run_agent_pass_1(Agent, S0, S) :-
  with_agent_console(Agent,run_agent_pass_1_0(Agent, S0, S)).
run_agent_pass_1(Agent, S0, S0) :-
  bugout('run_agent_pass_1(~w) FAILED!~n', [Agent], general).

run_agent_pass_2(Agent, S0, S) :-
  with_agent_console(Agent,run_agent_pass_2_0(Agent, S0, S)).
run_agent_pass_2(Agent, S0, S0) :-
  bugout('run_agent_pass_2(~w) FAILED!~n', [Agent], general).




run_agent_pass_1_0(Agent, S0, S) :-
  clock_time(Now),
  must_input_state(S0),
  %dmust((
  undeclare(memories(Agent, Mem0), S0, S1),
  undeclare(perceptq(Agent, PerceptQ), S1, S2),
  nb_setval(advstate,S2), % backtrackable leaks :(
  % b_setval(advstate,S2),
  thought(timestamp(T0,_OldNow), Mem0),  
  (PerceptQ==[] -> (T1 is T0 + 0, Mem0 = Mem1) ;  (T1 is T0 + 1, memorize(timestamp(T1,Now), Mem0, Mem1))), 
  process_percept_list(Agent, PerceptQ, T1, Mem1, Mem2),
  memorize_list(PerceptQ, Mem2, Mem3),
  decide_action(Agent, Mem3, Mem4),
  declare(memories(Agent, Mem4), S2, S3),
  declare(perceptq(Agent, []), S3, S4),
  % dbug(timestamp(Agent, T1)),
  apply_first_arg_state(Agent, do_todo(), S4, S),
  % pprint(S, general),
  
  notrace(must_output_state(S)),!.
  
run_agent_pass_2_0(_Agent, S0, S0):-!.
run_agent_pass_2_0(Agent, S0, S) :-
  must_input_state(S0),
  apply_first_arg_state(Agent, do_todo(), S0, S),
  notrace(must_output_state(S)),!.
  
% --------

