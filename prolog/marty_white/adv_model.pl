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

%:- ensure_loaded(adv_main).

% Manipulate memories (M stands for Memories)
memorize(Figment, M0, M1) :- notrace(append([Figment], M0, M1)).
memorize_list(FigmentList, M0, M1) :- notrace((must_be(list,FigmentList),dmust(append(FigmentList, M0, M1)))).
forget(Figment, M0, M1) :- select(Figment, M0, M1).
forget_always(Figment, M0, M1) :- select_always(Figment, M0, M1).
%forget_default(Figment, Default, M0, M1) :-
%  select_default(Figment, Default, M0, M1).
thought(Figment, M) :- member(Figment, M).


% TODO: change agent storage into a term:
%   mind(AgentName, AgentType, History, ModelData, Goals /*, ToDo*/)

% -------- Model updating predicates (here M stands for ModelData)

% Fundamental predicate that actually modifies the list:
update_relation( NewHow, Item, NewParent, Timestamp, M0, M2) :-
  select_always(h(Spatial, _How, Item, _Where, _T), M0, M1),
  ignore(Spatial=spatial),
  append([h(Spatial, NewHow, Item, NewParent, Timestamp)], M1, M2).

% Batch-update relations.
update_relations(_NewHow, [], _NewParent, _Timestamp, M, M).
update_relations( NewHow, [Item|Tail], NewParent, Timestamp, M0, M2) :-
  update_relation( NewHow, Item, NewParent, Timestamp, M0, M1),
  update_relations( NewHow, Tail, NewParent, Timestamp, M1, M2).

% If dynamic topology needs remembering, use
%      related(Spatial, exit(E), Here, [There1|ThereTail], Timestamp)
update_model_exit(Spatial, How, From, Timestamp, M0, M2) :-
  select(h(Spatial, How, From, To, _T), M0, M1),
  append([h(Spatial, How, From, To, Timestamp)], M1, M2).
update_model_exit(Spatial, How, From, Timestamp, M0, M1) :-
  append([h(Spatial, How, From, '<unexplored>', Timestamp)], M0, M1).
update_model_exit(Spatial, How, From, To, Timestamp, M0, M2) :-
  select_always(h(Spatial, How, From, _To, _T), M0, M1),
  append([h(Spatial, How, From, To, Timestamp)], M1, M2).


update_model_exits(_Spatial, [], _From, _T, M, M).
update_model_exits(Spatial, [Exit|Tail], From, Timestamp, M0, M2) :-
  update_model_exit(Spatial, Exit, From, Timestamp, M0, M1),
  update_model_exits(Spatial, Tail, From, Timestamp, M1, M2).

%butlast(List, ListButLast) :-
%  %last(List, Item),
%  append(ListButLast, [_Item], List).

% Match only the most recent Figment in Memory.
%last_thought(Figment, Memory) :-  % or member1(F, M), or memberchk(Term, List)
%  copy_term(Figment, FreshFigment),
%  append(RecentMemory, [Figment|_Tail], Memory),
%  \+ member(FreshFigment, RecentMemory).

update_model(Agent, carrying(_Spatial, Objects), Timestamp, _Memory, M0, M1) :-
  update_relations( held_by, Objects, Agent, Timestamp, M0, M1).
update_model(_Agent, notice_children(_Sense, Object, How, Children), Timestamp, _Mem, M0, M1) :-
  update_relations( How, Children, Object, Timestamp, M0, M1).
update_model(_Agent, sense_props(_Sense, Object, PropList), Stamp, _Mem, M0, M2) :-
  select_always(props_at(Object, _, _), M0, M1),
  append([props_at(Object, PropList, Stamp)], M1, M2).

update_model(_Agent,
             sense(Sense, [you_are(Spatial, How, Here), exits_are(Exits), here_are(Objects)]),
             Timestamp, _Mem, M0, M4) :-
  sensory_model(Sense, Spatial),
  % Don't update map here, it's better done in the moved( ) clause.
  update_relations( How, Objects, Here, Timestamp, M0, M3), % Model objects seen Here
  findall(exit(E), member(E, Exits), ExitRelations),
  update_model_exits(Spatial, ExitRelations, Here, Timestamp, M3, M4).% Model exits from Here.


update_model(Agent, moved( Agent, There, How, Here), Timestamp, Mem, M0, M2) :-
  % According to model, where was I?
  in_model(h(Spatial, _, Agent, There, _T0), M0),
  % TODO: Handle goto(on, table)
  % How did I get Here?
  append(RecentMem, [did(goto(_HowGo,A,C,B))|OlderMem], Mem), % find figment
  member(ExitName,[A,B,C]),atom(ExitName),
  \+ member(did(goto(_, _, _, _)), RecentMem),          % guarrantee recentness
  memberchk(timestamp(_T1,_WhenNow), OlderMem),          % get associated stamp

  %player_format('~p moved: goto(~p, ~p) from ~p leads to ~p~n',
  %       [Agent, HowGo, Dest, There, Here]),
  update_model_exit(Spatial, exit(ExitName), There, Here, Timestamp, M0, M1), % Model the path.
  update_relation(How, Agent, Here, Timestamp, M1, M2). % And update location.

update_model(_Agent, moved( Object, _From, How, To), Timestamp, _Mem, M0, M1) :-
  update_relation( How, Object, To, Timestamp, M0, M1).

update_model(_Agent, failure(_), _Timestamp, _Mem, M0, M0) :- !.

update_model(Agent, time_passes, Timestamp, _Memory, M, M):-
  nop(dbug(unused_update_model(Agent, time_passes, Timestamp, M))).

update_model(_Agent, [], _Timestamp, _Memory, M, M).
update_model(Agent, [Percept|Tail], Timestamp, Memory, M0, M2) :-
  update_model(Agent, Percept, Timestamp, Memory, M0, M1),
  update_model_all( Agent, Tail, Timestamp, Memory, M1, M2).


update_model(Agent, Percept, Timestamp, _Memory, M, M):-
  dbug(failed_update_model(Agent, Percept, Timestamp)).

% update_model_all(Spatial, Agent, PerceptsList, Stamp, ROMemory, OldModel, NewModel)
update_model_all(_Agent, [], _Timestamp, _Memory, M, M).
update_model_all( Agent, [Percept|Tail], Timestamp, Memory, M0, M2) :-
  update_model(Agent, Percept, Timestamp, Memory, M0, M1),
  update_model_all( Agent, Tail, Timestamp, Memory, M1, M2).


