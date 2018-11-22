/*
% NomicMUD: A MUD server written in Prolog
%
% Some parts used Inform7, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
% 
% July 10,1996 - John Eikenberry 
% Copyright (C) 2004 Marty White under the GNU GPL
% 
% Dec 13, 2035 - Douglas Miles
%
%
% Logicmoo Project changes:
%
% Main file.
%
*/

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- bugout(ensure_loaded('adv_robot_floyd')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*extra_look_around(Agent, S0, S9) :-
 undeclare(memories(Agent, Mem0), S0, S1),
 memorize_list([did(look(Agent)), did(inventory)], Mem0, Mem1),
 declare(memories(Agent, Mem1), S1, S2),
 must_act(look(Agent), S2, S3),
 must_act(inventory(Agent), S3, S9).
*/

random_noise(Agent, [cap(subj(Agent)), Msg]) :- fail, 
 random_member(Msg, [
 'hums quietly to themself.',
 'inspects their inspection cover.',
 'buffs their chestplate.',
 'fidgets uncomfortably.'
 ]).

:- dynamic(adv:agent_last_action/3).
 

do_autonomous_cycle(Agent):- time_since_last_action(Agent,When), When > 10, !.
do_autonomous_cycle(Agent):- 
 time_since_last_action(Other,When),
 Other \== Agent, When < 1, !, 
 retractall(adv:agent_last_action(Other,_,_)),
 nop(bugout(time_since_last_action_for(Other,When,Agent))).


% Is powered down
maybe_autonomous_decide_goal_action(Agent, Mem0, Mem0) :- 
 getprop(Agent, state(powered, f), advstate),!.

maybe_autonomous_decide_goal_action(Agent, Mem0, Mem1) :- fail, notrace((do_autonomous_cycle(Agent),
 set_last_action(Agent,[auto(Agent)]))),
 autonomous_decide_goal_action(Agent, Mem0, Mem1),!.
maybe_autonomous_decide_goal_action(_Agent, Mem0, Mem0).


% ......
autonomous_decide_goal_action(Agent, Mem0, Mem3) :-
 dmust((
    forget(goals(Goals), Mem0, Mem1),
    agent_thought_model(Agent,ModelData, Mem1),
    select_unsatisfied_conditions(Goals, Unsatisfied, ModelData),
    subtract(Goals,Unsatisfied,Satisfied),
    memorize(goals(Unsatisfied), Mem1, Mem1a),
    (Satisfied==[] -> Mem1a=Mem2 ; memorize(satisfied(Satisfied), Mem1a, Mem2)),
    autonomous_decide_action(Agent, Mem2, Mem3))).

% If actions are queued, no further thinking required. 
autonomous_decide_action(Agent, Mem0, Mem0) :- 
 thought(todo([Action|_]), Mem0),
 (declared(h(in, Agent, Here), advstate)->true;Here=somewhere),
 (trival_act(Action)->true;bugout('~w @ ~w: already about todo: ~w~n', [Agent, Here, Action], autonomous)).

% notices bugs
autonomous_decide_action(Agent, Mem0, _) :-
 once((agent_thought_model(Agent,ModelData, Mem0),
 (\+ in_model(Agent, h(_, Agent, _), ModelData) -> (pprint(Mem0, always),pprint(ModelData, always)) ; true),
 dmust(in_model(Agent,h(_Prep, Agent, Here), ModelData)),
 nonvar(Here))), 
 fail.


% If goals exist, try to solve them.
autonomous_decide_action(Agent, Mem0, Mem1) :-
 Knower = Agent,
 thought(goals([_|_]), Mem0),
 bugout('~w: goals exist: generating a plan...~n', [Agent], autonomous),
 generate_plan(Knower, Agent, NewPlan, Mem0), !,
 serialize_plan(Knower, Agent, NewPlan, Actions), !,
 bugout('Planned actions are ~w~n', [Actions], autonomous),
 Actions = [Action|_],
 add_todo(Action, Mem0, Mem1).
% If goals exist, forget them
autonomous_decide_action(Agent, Mem0, Mem9) :-
 forget(goals([G0|GS]), Mem0, Mem1),
 memorize(goals([]), Mem1, Mem2),
 memorize(old_goals([G0|GS]), Mem2, Mem9),
 bugout('~w: Can\'t solve goals ~p. Forgetting them.~n', [Agent,[G0|GS]], autonomous).

% If no actions or goals, but there's an unexplored exit here, go that way.
autonomous_decide_action(Agent, Mem0, Mem1) :-
 agent_thought_model(Agent,ModelData, Mem0),
 in_model(Agent,h(_Prep, Agent, Here), ModelData),
 in_model(Agent,h(exit(Dir), Here, '<unexplored>'), ModelData),
 add_todo( goto_dir(Agent, walk, Dir), Mem0, Mem1).

% Follow Player to adjacent rooms.
autonomous_decide_action(Agent, Mem0, Mem1) :- % 1 is random(2),
 dmust((
 agent_thought_model(Agent,ModelData, Mem0),
 in_model(Agent,h(_, Agent, Here), ModelData))),
 dif(Agent, Player), current_player(Player),
 in_model(Agent,h(_, Player, There), ModelData),
 in_model(Agent,h(exit(Dir), Here, There), ModelData),
 add_todo(goto_dir(Agent, walk, Dir), Mem0, Mem1).

autonomous_decide_action(Agent, Mem0, Mem1) :-
 0 is random(5),
 random_noise(Agent, Msg),
 add_todo(emote(Agent, act, *, Msg), Mem0, Mem1).
autonomous_decide_action(Agent, Mem0, Mem0) :-
 (declared(h(in, Agent, Here), advstate)->true;Here=somewhere),
 nop(bugout('~w: Can\'t think of anything to do.~n', [Agent-Here], autonomous+verbose)).% trace.



% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_agent_listen')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

consider_text(Speaker, _EmoteType, Agent, Words, Mem0, Mem1):-
 parse_command(Agent, Words, Action, Mem0),
 trace, consider_request(Speaker, Agent, Action, Mem0, Mem1).

% For now, agents will attempt to satisfy all commands.
consider_request(Requester, Agent, Action, _M0, _M1) :-
 bugout('~w: considering request from: ~w.~n', [Requester, Agent, Action], autonomous),
 fail.

consider_request(Requester, Agent, Query, M0, M1) :-
 do_introspect(Agent,Query, Answer, M0),
 %add_todo(print_(Answer), M0, M1).
 add_todo(emote(Agent, say, Requester, Answer), M0, M1).

consider_request(_Speaker, Agent, forget(goals), M0, M2) :-
 bugout('~w: forgetting goals.~n', [Agent], autonomous),
 forget_always(goals(_), M0, M1),
 memorize(goals([]), M1, M2).
% Bring object back to Speaker.
consider_request(Speaker, _Agent, fetch(Object), M0, M1) :- 
 add_goal(h(held_by, Object, Speaker), M0, M1).
consider_request(_Speaker, Agent, put(Agent, Thing, Relation, Where), M0, M) :-
 add_goal(h(Relation, Thing, Where), M0, M).
consider_request(_Speaker, Agent, take(Agent, Thing), M0, M) :-
 add_goal(h(held_by, Thing, Agent), M0, M).
consider_request(_Speaker, Agent, drop(Agent, Object), M0, M1) :-
 add_goal(~(h(held_by, Object, Agent)), M0, M1).
consider_request(_Speaker, Agent, goto(Agent, How, Prep, OfWhat), M0, M1) :-  
 Action = goto(Agent, How, Prep, OfWhat), 
 bugout('Queueing action ~w~n', Action, autonomous),
 add_todo(Action, M0, M1).

consider_request(_Speaker, Agent, Action, M0, M1) :-
 bugout('Finding goals for action: ~w~n', [Action], autonomous),
 initial_operators(Agent, Operators),
 findall(Effects,
   member(oper(Agent, Action, _Conds, Effects), Operators),
   [UnambiguousGoals]),
 bugout('Request: ~w --> goals ~w.~n', [Action, UnambiguousGoals], autonomous),
 add_goals(UnambiguousGoals, M0, M1).

consider_request(_Speaker, _Agent, Action, M0, M1) :-
 bugout('Queueing action: ~w~n', [Action], autonomous),
 add_todo(Action, M0, M1).
consider_request(_Speaker, Agent, Action, M0, M0) :-
 bugout('~w: did not understand request: ~w~n', [Agent, Action], autonomous).


