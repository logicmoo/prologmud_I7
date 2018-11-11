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


act_examine(Agent, Sense, Here, State, NewState) :- 
 \+ \+ related(_, exit(_), Here, _, State), 
 sensory_model_problem_solution(Sense, Spatial, _TooDark, _EmittingLight),
 related(Spatial, Relation, Agent, Here, State), !,
 findall(What,
   (related(Spatial, Relation, What, Here, State),
    (related(Spatial, descended, What, Here, State),
     \+ (related(Spatial, inside, What, Container, State),
     related(Spatial, descended, Container, Here, State)))),
   Nearby),
 findall(Direction, related(Spatial, exit(Direction), Here, _, State), Exits),
 !,
 queue_agent_percept(Agent,
    [sense_each(Agent, Sense, [
             you_are(Agent, Relation, Here), 
             exits_are(Agent,Here,Exits), 
             notice_children(Agent, Sense, Here, Relation, Nearby)]) ],
    State, NewState).

act_examine(Agent, Sense, Object, S0, S2):- 
 findall(P, (getprop(Object, P, S0), is_prop_public(Sense,P)), PropListL),
 list_to_set(PropListL,PropList),
 queue_agent_percept(Agent, sense_props(Agent, Sense, Object, PropList), S0, S1),
 add_child_precepts(Sense,Agent,Object,S1,S2),!.

add_child_precepts(Sense,Agent,Object,S1,S2):- 
 findall(Relation, 
     (getprop(Object,has_rel(_Domain1,Relation,t),S1);      
      declared(h(_Domain2, Relation, _, Object),S1)), RelationList),
 list_to_set(RelationList,RelationSet),
 %dmsg(list_to_set(RelationList,RelationSet)),
 add_child_precepts_rel_list(Sense, Agent,Object,RelationSet,S1,S2).


add_child_precepts_rel_list(Sense, Agent,Here,[Prep|More],S0,S2):-  
  (exclude(=(exit(_)),[Prep|More],RelationSet)-> RelationSet \== [Prep|More]),!, 
  findall(Direction, related(_Spatial, exit(Direction), Here, _, S0), Exits),  
  queue_agent_percept(Agent,[exits_are(Agent,Here,Exits)],S0,S1),
  add_child_precepts_rel_list(Sense, Agent,Here,RelationSet,S1,S2),!.

  
add_child_precepts_rel_list(_Sense, _Agent,_Object,[],S1,S1).
add_child_precepts_rel_list(Sense, Agent,Object,[Prep|More],S0,S2):- !, 
  add_child_precepts_rel_list(Sense, Agent,Object,More,S0,S1),
  add_child_precepts_rel_list(Sense, Agent,Object,Prep,S1,S2).

add_child_precepts_rel_list(_Sense, _Agent,_Object,exit(_),S1,S1).

add_child_precepts_rel_list(Sense, Agent,Object,Relation,S1,S2):- 
 findall(What,
   (related(_Spatial, Relation, What, Object, S1),
    nop(once(can_sense(_VSense, What, Agent, S1)))),
   ChildrenL),
 list_to_set(ChildrenL,Children),
 queue_agent_percept(Agent, notice_children(Agent, Sense, Object, Relation, Children), S1, S2).
                                 

