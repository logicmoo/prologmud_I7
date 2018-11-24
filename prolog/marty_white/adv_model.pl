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

memorize_edit(Pred3, Figment, M0, M2) :- assertion(\+ is_list(Figment)),
   Figment =.. [Name,Value], OldFigment =.. [Name,OldValue],
   (forget(OldFigment, M0, M1) 
     -> ( call(Pred3, OldValue,Value,NewValue), NewFigment =.. [Name,NewValue])
     ; (NewFigment=Figment, M0=M1)),
   memorize(NewFigment, M1, M2).

memorize_appending(Figment, M0, M2) :-  memorize_edit(append,Figment, M0, M2).

% Manipulate memories (M stands for Memories)
memorize(Figment, M0, M1) :- assertion(\+ is_list(Figment)), notrace(append([Figment], M0, M1)).
% memorize(Figment, M0, M1) :- notrace(append([Figment], M0, M1)).
memorize_list([],M0,M0):-!.
memorize_list([E|List],M0,M2):-!,
  memorize_list(List,M0,M1),
  memorize_list(E,M1,M2).
memorize_list(Figment, M0, M1):-
  notrace(append([Figment], M0, M1)).
%memorize_list(FigmentList, M0, M1) :- notrace((must_be(list,FigmentList),dmust_det(append(FigmentList, M0, M1)))).
%memorize_list(FigmentList, M0, M1) :- notrace((must_be(list,FigmentList),dmust_det(append(FigmentList, M0, M1)))).
forget(Figment, M0, M1) :- select(Figment, M0, M1).
forget_always(Figment, M0, M1) :- select_always(Figment, M0, M1).
%forget_default(Figment, Default, M0, M1) :-
% select_default(Figment, Default, M0, M1).
thought(Figment, M) :- member(Figment, M).


in_agent_model(Agent, Fact, State):- in_model(Fact, State)*-> true ; (agent_thought_model(Agent, ModelData, State), in_model(Fact, ModelData)).

in_model(E, L):- quietly(in_model0(E, L)).
in_model0(E, L):- \+ is_list(L),declared_link(declared, E, L).
in_model0(E, L):- compound(E),E = holds_at(_,_),!, member(E, L).
in_model0(E, L):- member(EE, L), same_element(EE,E).
same_element(E, E) :- !.
same_element(holds_at(E,_), E).



:- defn_state_getter(agent_thought_model(agent,model)).
agent_thought_model(Agent, ModelData, M0):- var(M0), get_advstate(State),!, member(memories(Agent,M0),State), agent_thought_model(Agent, ModelData, M0).
agent_thought_model(Agent, ModelData, M0):- \+ is_list(M0), !, declared_link(agent_thought_model(Agent), ModelData, M0).
agent_thought_model(Agent, ModelData, M0) :- memberchk(inst(Agent),M0), ModelData = M0, !.
agent_thought_model(Agent, ModelData, M0):- declared(memories(Agent,M1),M0),!,
  agent_thought_model(Agent, ModelData, M1).



% TODO: change agent storage into a term:
% mind(AgentName, AgentType, History, ModelData, Goals /*, ToDo*/)

% -------- Model updating predicates (here M stands for ModelData)

% Fundamental predicate that actually modifies the list:
update_relation( NewHow, Item, NewParent, Timestamp, M0, M2) :-
 remove_old_info( NewHow, Item, NewParent, Timestamp, M0, M1),
 append([(h(NewHow, Item, NewParent))], M1, M2).

remove_old_info( _NewHow, '<mystery>'(_, _, _), _NewParent, _Timestamp, M0, M0) :- !.
remove_old_info( _NewHow, Item, _NewParent, _Timestamp, M0, M2) :- 
 select_always((h(_OldHow, Item, _OldWhere)), M0, M1),
 select_always(h(_OldHow2, Item, _OldWhere2), M1, M2).


remove_children(_At, '<mystery>'(_, _, _), _Object, _Timestamp, M0, M0):- !.
remove_children( At, _, Object, Timestamp, M0, M2):- 
  select((h(At, _, Object)), M0, M1), !,
  remove_children( At, _, Object, Timestamp, M1, M2).
remove_children( _At, _, _Object, _Timestamp, M0, M0).

% Batch-update relations.

update_relations(Prep, '<mystery>'(How,What,Object2), Object, Timestamp, M0, M1):-
  \+ in_model((h(What, _Child, Object2)), M0), 
  % \+ in_model((h(What, Object2, _Parent), _), M0),
  update_relation( Prep, '<mystery>'(How,What,Object2), Object, Timestamp, M0, M1).

update_relations(_NewHow, '<mystery>'(_,_,_), _NewParent, _Timestamp, M, M).
update_relations(_NewHow, [], _NewParent, _Timestamp, M, M).
update_relations( NewHow, [Item|Tail], NewParent, Timestamp, M0, M2) :-
 update_relation( NewHow, Item, NewParent, Timestamp, M0, M1),
 update_relations( NewHow, Tail, NewParent, Timestamp, M1, M2).


% If dynamic topology needs remembering, use
%  h(exit(E), Here, [There1|ThereTail], Timestamp)
update_model_exit(At, From, _Timestamp, M0, M2) :-
 forget((h(At, From, To)), M0, M1),
 append([(h(At, From, To))], M1, M2).
update_model_exit(At, From, _Timestamp, M0, M1) :-
 append([(h(At, From, '<mystery>'(exit, At, From)))], M0, M1).
update_model_exit(At, From, To, _Timestamp, M0, M2) :-
 select_always((h(At, From, _To)), M0, M1),
 append([(h(At, From, To))], M1, M2).


update_model_exits([], _From, _T, M, M).
update_model_exits([Exit|Tail], From, Timestamp, M0, M2) :-
 update_model_exit(Exit, From, Timestamp, M0, M1),
 update_model_exits(Tail, From, Timestamp, M1, M2).

% Match only the most recent Figment in Memory.
%last_thought(Figment, Memory) :- % or member1(F, M), or memberchk(Term, List)
% copy_term(Figment, FreshFigment),
% append(RecentMemory, [Figment|_Tail], Memory),
% \+ member(FreshFigment, RecentMemory).

update_model(Knower, arriving(Agent,Here,_,ExitNameReversed), Timestamp, Mem, M0, M2) :-  Knower = Agent,    
 dmust_det((  reverse_dir(ExitNameReversed, ExitName, advstate),
  At = in,
  % According to model, where was I?
  in_model(h(_, Agent, There), M0),
  % TODO: Handle goto(Agent, walk, on, table)
  % At did I get Here?
  append(RecentMem, [did(goto_dir(Agent, _, ExitName))|OlderMem], Mem), % find figment
  \+ member(did(goto_dir(Agent, _, _)), RecentMem),               % guarrantee recentness
  memberchk(timestamp(_T1,_OldNow), OlderMem),               % get associated stamp
  %player_format(Agent, '~p moved: goto(Agent, walk, ~p, ~p) from ~p leads to ~p~n',
  %       [Agent, AtGo, Dest, There, Here]),
  update_model_exit(exit(ExitName), There, Here, Timestamp, M0, M1), % Model the path.
  update_relation(At, Agent, Here, Timestamp, M1, M2))). % And update location.

update_model(_Agent, moved(_Doer, _How, Object,_From, At, To), Timestamp, _Mem, M0, M1) :-
  update_relation(At, Object, To, Timestamp, M0, M1).


update_model(Agent, carrying(Agent, Objects), Timestamp, _Memory, M0, M1) :-
 update_relations( held_by, Objects, Agent, Timestamp, M0, M1).
update_model(Agent, wearing(Agent, Objects), Timestamp, _Memory, M0, M1) :-
 update_relations( worn_by, Objects, Agent, Timestamp, M0, M1).

update_model(Agent, notice_children(Agent, _Sense, Object, At, _Depth, Children), Timestamp, _Mem, M0, M2) :-
 dmust_det((remove_children( At, Children, Object, Timestamp, M0, M1),
   update_relations( At, Children, Object, Timestamp, M1, M2))).
update_model(Agent, sense_props(Agent, _Sense, Object, _Depth, PropList), _Stamp, _Mem, M0, M2) :-
 select_always((props(Object, _)), M0, M1),
 append([(props(Object, PropList))], M1, M2).


update_model(_Agent, exits_are(_Whom, in, Here, Exits), Timestamp, _Mem, M0, M4) :-
  % Don't update map here, it's better done in the moved() clause.
  findall(exit(E), member(E, Exits), ExitRelations),
  update_model_exits(ExitRelations, Here, Timestamp, M0, M4).% Model exits from Here.

/*
% Model exits from Here.
update_model(Agent, exits_are(Agent2, Relation, Here, Exits), Timestamp, _Mem, M0, M4):- Agent=@=Agent2, !,
  findall(exit(E), member(E, Exits), ExitRelations),
    % Don't update map here? it's better done in the moved( ) clause?
    update_relations(Relation, [Agent], Here, Timestamp, M0, M3),
  update_model_exits( ExitRelations, Here, Timestamp, M3, M4).
*/

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


