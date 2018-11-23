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

%:- ensure_loaded(adv_main).

% Manipulate memories (M stands for Memories)
memorize(Figment, M0, M1) :- assertion(\+ is_list(Figment)), notrace(append([Figment], M0, M1)).
% memorize(Figment, M0, M1) :- notrace(append([Figment], M0, M1)).
memorize_list([],M0,M0):-!.
memorize_list([E|List],M0,M2):-!,
  memorize_list(List,M0,M1),
  memorize_list(E,M1,M2).
memorize_list(Figment, M0, M1):-
  notrace(append([Figment], M0, M1)).
%memorize_list(FigmentList, M0, M1) :- notrace((must_be(list,FigmentList),dmust(append(FigmentList, M0, M1)))).
%memorize_list(FigmentList, M0, M1) :- notrace((must_be(list,FigmentList),dmust(append(FigmentList, M0, M1)))).
forget(Figment, M0, M1) :- select(Figment, M0, M1).
forget_always(Figment, M0, M1) :- select_always(Figment, M0, M1).
%forget_default(Figment, Default, M0, M1) :-
% select_default(Figment, Default, M0, M1).
thought(Figment, M) :- member(Figment, M).


in_agent_model(_Knower, E, L):- in_model(E, L).
% in_agent_model(Knower, E, L):- in_model(E, Knower).

in_model(E, L):- member(E, L).
in_model(E, L):- member(holds_at(E,_), L).
in_model(E, L):- member(model(M), L), is_list(M), in_model(E,M).

:- defn_state_getter(agent_thought_model(agent,fact)).
agent_thought_model(_Agent,E, L):- in_model(model(E), L), nonvar(E).
% agent_thought_model(Agent,Model,List):- dmust((nop(memberchk(agent(Agent),List)), member(model(Model),List))).



% TODO: change agent storage into a term:
% mind(AgentName, AgentType, History, ModelData, Goals /*, ToDo*/)

% -------- Model updating predicates (here M stands for ModelData)

% Fundamental predicate that actually modifies the list:
update_relation( NewHow, Item, NewParent, Timestamp, M0, M2) :-
 select_always(holds_at(h(_OldHow, Item, _OldWhere), _T), M0, M1a),
 select_always(h(_OldHow2, Item, _OldWhere2), M1a, M1),
 append([holds_at(h(NewHow, Item, NewParent), Timestamp)], M1, M2).

% Batch-update relations.
update_relations(_NewHow, [], _NewParent, _Timestamp, M, M).
update_relations( NewHow, [Item|Tail], NewParent, Timestamp, M0, M2) :-
 update_relation( NewHow, Item, NewParent, Timestamp, M0, M1),
 update_relations( NewHow, Tail, NewParent, Timestamp, M1, M2).

% If dynamic topology needs remembering, use
%  h(exit(E), Here, [There1|ThereTail], Timestamp)
update_model_exit(How, From, Timestamp, M0, M2) :-
 select(holds_at(h(How, From, To), _T), M0, M1),
 append([holds_at(h(How, From, To), Timestamp)], M1, M2).

update_model_exit(How, From, Timestamp, M0, M1) :-
 append([holds_at(h(How, From, '<unexplored>'), Timestamp)], M0, M1).

update_model_exit(How, From, To, Timestamp, M0, M2) :-
 select_always(holds_at(h(How, From, _To), _T), M0, M1),
 append([holds_at(h(How, From, To), Timestamp)], M1, M2).


update_model_exits([], _From, _T, M, M).
update_model_exits([Exit|Tail], From, Timestamp, M0, M2) :-
 update_model_exit(Exit, From, Timestamp, M0, M1),
 update_model_exits(Tail, From, Timestamp, M1, M2).


% Match only the most recent Figment in Memory.
%last_thought(Figment, Memory) :- % or member1(F, M), or memberchk(Term, List)
% copy_term(Figment, FreshFigment),
% append(RecentMemory, [Figment|_Tail], Memory),
% \+ member(FreshFigment, RecentMemory).

update_model(Knower, moved( Agent, There, How, Here), Timestamp, Mem, M0, M2) :-
 Knower = Agent,
 dmust((
 % According to model, where was I?
 in_agent_model(Knower,  h(_, Agent, There), M0),
 % TODO: Handle goto(Agent, on, table)
 % How did I get Here?
 append(RecentMem, [did( goto(_, _HowGo, A,B))|OlderMem], Mem), % find figment
 member(ExitName,[A,B]),atom(ExitName),
 \+ member(did( goto(_, _, _, _)), RecentMem),   % guarrantee recentness
 memberchk(timestamp(_T1,_WhenNow), OlderMem),   % get associated stamp

 %player_format('~p moved: goto(Agent, ~p, ~p) from ~p leads to ~p~n',
 %  [Agent, HowGo, Dest, There, Here]),
 update_model_exit(exit(ExitName), There, Here, Timestamp, M0, M1), % Model the path.
 update_relation(How, Agent, Here, Timestamp, M1, M2))). % And update location.

update_model(_Agent, moved( Object, _From, How, To), Timestamp, _Mem, M0, M1) :-
 update_relation( How, Object, To, Timestamp, M0, M1).


update_model(Agent, carrying(Agent, Objects), Timestamp, _Memory, M0, M1) :-
 update_relations( held_by, Objects, Agent, Timestamp, M0, M1).
update_model(Agent, wearing(Agent, Objects), Timestamp, _Memory, M0, M1) :-
 update_relations( worn_by, Objects, Agent, Timestamp, M0, M1).
update_model(Agent, notice_children(Agent, _Sense, Object, How, _Depth, Children), Timestamp, _Mem, M0, M1) :-
 update_relations( How, Children, Object, Timestamp, M0, M1).
update_model(Agent, sense_props(Agent, _Sense, Object, _Depth, PropList), Stamp, _Mem, M0, M2) :-
 select_always(holds_at(props(Object, _), _), M0, M1),
 append([holds_at(props(Object, PropList), Stamp)], M1, M2).



%update_model(Agent, you_are(Agent, _How, _He\re), _Timestamp, _Mem, M0, M0):- !.

% Model exits from Here.
update_model(Agent, exits_are(Agent2, Relation, Here, Exits), Timestamp, _Mem, M0, M4):- Agent==Agent2, !,
  findall(exit(E), member(E, Exits), ExitRelations),
    % Don't update map here? it's better done in the moved( ) clause?
    update_relations(Relation, [Agent], Here, Timestamp, M0, M3),
  update_model_exits( ExitRelations, Here, Timestamp, M3, M4).
update_model(_Agent, exits_are(S,_,_,_), _Timestamp, _Mem, M0, M0):- S == '$fake',!.

% Model objects seen Here
update_model(Agent, notice_children(Agent, _Sense, Here, Prep, _Depth, Objects), Timestamp, _Mem, M0, M3):- !,
   update_relations(Prep, Objects, Here, Timestamp, M0, M3). 

update_model(_Agent, [], _Timestamp, _Memory, M, M).
update_model(Agent, [Percept|Tail], Timestamp, Memory, M0, M2) :-
 update_model(Agent, Percept, Timestamp, Memory, M0, M1),
 update_model_all( Agent, Tail, Timestamp, Memory, M1, M2),!.
update_model(_Agent, failure(_,_), _Timestamp, _Mem, M0, M0) :- !.
update_model(_Agent, success(_,_), _Timestamp, _Mem, M0, M0) :- !.
update_model(_Agent, failure(_), _Timestamp, _Mem, M0, M0) :- !.
update_model(_Agent, emoted(_,_,_,_), _Timestamp, _Mem, M0, M0) :- !.
update_model(_Agent, emote(_,_,_,_), _Timestamp, _Mem, M0, M0) :- !.
update_model(_Agent, msg(_), _Timestamp, _Mem, M0, M0) :- !.

update_model(Agent, time_passes(Target), Timestamp, _Memory, M, M):-
 nop(bugout1(unused_update_model(Agent, time_passes(Target), Timestamp, M))).



update_model(Agent, Percept, Timestamp, _Memory, M, M):-
 nop(bugout1(failed_update_model(Agent, Percept, Timestamp), model)).

% update_model_all(Agent, PerceptsList, Stamp, ROMemory, OldModel, NewModel)
update_model_all(_Agent, [], _Timestamp, _Memory, M, M).
update_model_all( Agent, [Percept|Tail], Timestamp, Memory, M0, M2) :-
 update_model(Agent, Percept, Timestamp, Memory, M0, M1),
 update_model_all( Agent, Tail, Timestamp, Memory, M1, M2).


