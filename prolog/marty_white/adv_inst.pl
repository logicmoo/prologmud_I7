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

filter_spec( \+ Spec, PropList):- !,
  \+  filter_spec(Spec, PropList).
filter_spec((Spec1;Spec2), PropList):- !, filter_spec(Spec1, PropList);filter_spec(Spec2, PropList).
filter_spec((Spec1, Spec2), PropList):- !, filter_spec(Spec1, PropList), filter_spec(Spec2, PropList).
filter_spec(    Spec, PropList):- member(Spec, PropList).

create_new_unlocated(Type,Inst,S0,S2):- 
  atom_concat(Type,'~',TType),gensym(TType,Inst),
  declare(props(Inst,[inherit(Type),inherit(instance)]),S0,S2).
create_new_suffixed_unlocated(Suffix, Type,Inst,S0,S2):- 
  atom_concat(Type,Suffix,Inst),
  declare(props(Inst,[inherit(Type),inherit(instance)]),S0,S2).

% create_agent_conn(Agent,_Named, _Info, S0, S0) :- declared(agent(Agent,t), S0),!.
create_agent_conn(Agent,Named,Info,S0,S9):- 
   apply_state([create_new_unlocated('watch',Watch),
                create_new_unlocated('bag',Bag),
                create_new_unlocated('coins',Coins),
   declare(
     (props(Agent, [inherit(instance), name(['Telnet:',Named]), inherit(telnet), inherit(humanoid), inherit(player), info(Info)]),               
               h(Spatial, in, Agent, kitchen),
               h(Spatial, worn_by, Watch, Agent),
               h(Spatial, in, Bag, Coins),
               h(Spatial, held_by, Bag, Agent)))],S0,S1),
   init_objects(S1,S9).


init_objects(S0, S2) :-
  must_input_state(S0),
  create_missing_instances(S0,S1),
  dmust(call((get_objects(inherit('instance'), ObjectList, S1), ObjectList\==[]))),
  dbug(iObjectList  = ObjectList),
  apply_all(ObjectList, create_object(), S1, S2),
  must_output_state(S2), !.


%create_object(Agent, S0, S2) :- declared(perceptq(Agent, []), S0), !,
%  dbug(existingAgent=Agent),
%  S2=S0.
                   
create_object(Object, S0, S0) :- declared(object(Object,t), S0),!.
create_object(Object, S0, S3) :- fail,
   declare(object(Object,t), S0, S1),
   (select(props(Object, PropList),S1,S2);(PropList=[],S1=S2)),!,
   visit_existing(Object, PropList, S2, S3).
create_object(Object, S0, S2) :- 
   declare(object(Object,t), S0, S1),
   (declared(props(Object, PropList), S0);PropList=[]),!,
   visit_existing(Object, PropList, S1, S2).

visit_existing(_Object, [], S0, S0) :-!.
visit_existing(Object, [Prop|List], S0, S2):- !,  
   visit_existing(Object, List, S0, S1),
   visit_existing(Object, Prop, S1, S2).

%visit_existing(Object, Prop, S1, S2):- dmust(create_objprop(Object, Prop, S1, S2)).

visit_existing(Object, Prop, S1, S2):- Prop=inherit(_),!,dmust(create_objprop(Object, Prop, S1, S2)).
visit_existing(Object, Prop, S0, S2):- dmust(updateprop(Object,Prop,S0, S2)).
  

create_objprop(_Object, [], S0, S0).
create_objprop(Object, [Prop|List], S0, S2):- !,
   create_objprop(Object, List, S0, S1),
   create_objprop(Object, Prop, S1, S2).

create_objprop(Object, inherit(Other), S0, S0):- getprop(Object,inherited(Other),S0),!.
create_objprop(Object, inherit(Other), S0, S0):- getprop(Object,isnt(Other),S0),!.
create_objprop(Object, inherit(Other), S0, S0):- Other==Object,!.

  % As events happen, percepts are entered in the percept queue of each agent.
  % Each agent empties their percept queue as they see fit.
create_objprop(Object, inherit(perceptq), S0, S0):- declared(perceptq(Object,_),S0),!.
create_objprop(Object, inherit(perceptq), S0, S1):- !,
   declare(perceptq(Object, []), S0, S1).


% Most agents store memories of percepts, world model, goals, etc.
create_objprop(Object, inherit(memorize), S0, S0):- declared(memories(Object,_),S0),!.
create_objprop(Object, inherit(memorize), S0, S2):- !,
  (declared(props(Object, PropList), S0);declared(class_props(Object, PropList), S0)),
  copy_term(PropList,PropListC),!,
  % =(PropList,PropListC),!,
  declare(memories(Object, [
    structure_label(initial_memories),
    timestamp(0),
    model([]),
    goals([]),
    todo([look]),
    inst(Object)|PropListC]), S0, S2).


create_objprop(Object, inherit(Other), S0, S9):- 
   (declared(props(Other, PropList), S0);declared(class_props(Other, PropList), S0); PropList=[]),
   copy_term(PropList,PropListC),!,
   dmust(setprop(Object, inherited(Other), S0, S1)), !,
   dmust(create_objprop(Object, PropListC, S1, S2)),
   dmust(setprop(Object, inherit(Other), S2, S9)), !,
   !.
   
create_objprop(Object, Prop, S0, S2):- dmust(updateprop(Object,Prop,S0, S2)).



create_missing_instances(S0,S2):- 
  create_instances('~1',S0,S0,S2).

may_contain_insts(h).

create_instances(Suffix,Info,[Prop|S0],[NewProp|S2]):-
 Prop =.. [F, Spatial, Pred | Objs], 
 may_contain_insts(F),member(Obj,Objs),compound(Obj),!,
 dmust((create_objs(Objs,NewObjs,Suffix,Info,S0,S1),
 NewProp =.. [F, Spatial, Pred | NewObjs],
 create_instances(Suffix,Info,S1,S2))).
create_instances(Suffix,Info,[Prop|S0],[Prop|S2]):-
   create_instances(Suffix,Info,S0,S2).
create_instances(_Suffix,_Info,[],[]).

create_objs([Obj|Objs],[NewObj|NewObjs],Suffix,Info,S0,S2):-
  dmust(create_1obj(Suffix,Info,Obj,NewObj,S0,S1)),
  create_objs(Objs,NewObjs,Suffix,Info,S1,S2).
create_objs([],[],_Suffix,_Info,S0,S0).


create_1obj(Suffix,_Info,a(Type),Inst,S0,S2):- !, 
  create_new_suffixed_unlocated(Suffix,Type,Inst,S0,S2).

create_1obj(Suffix,_Info,the(Type),Inst,S0,S2):- !, dmust(find_recent(Suffix,Type,Inst,S0,S2)).
create_1obj(_Suffix,_Info,I,I, S0,S0):- atom_contains(I,'~').
create_1obj(_Suffix,_Info,I,I, S0,S0):- assertion(atom(I)),!.

find_recent(_Suffix,Type,Inst,S0,S0):- member(props(Inst,PropList),S0),member(instance(Type),PropList).

/*
inst_of(I,C,N):- compound(I),!,I=..[C,N|_].
inst_of(I,C,N):- atom(I),!, atomic_list_concat([C,NN],'~',I),atom_number(NN,N).
inst_of(I,C,N):- atom(C),atomic_list_concat([C,NN],'~',I),atom_number(NN,N).
*/








