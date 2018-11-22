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
findall_set(E,G,S):- findall(E,G,L),list_to_set(L,S).

nearby_objs(Agent, Here, Nearby, S0):- 
 ignore(h(Relation, Agent, Here, S0)),
 findall_set(What,  
   (h(Relation, What, Here, S0),
    sub_objs(descended, Here, What, S0)),
   Nearby).

sub_objs(Relation, Here, What, S0):- 
  h(Relation, What, Here, S0),
 \+ ((h(inside, What, Container, S0), 
   Container\==Here, h(descended, Container, Here, S0))).

exits_of(in, Object, Exits, S0) :- 
  findall(Direction, h(exit(Direction), Object, _, S0), Exits), Exits\==[], !.
exits_of(in, _Object, [escape], _S0) :- !.
exits_of(on, _Object, [escape], _S0) :- !.
exits_of(under, _Object, [escape], _S0) :- !.
exits_of(at, _Object, [escape], _S0) :- !.
exits_of(Other, _Object, [reverse(Other)], _S0).

object_props(Object, Sense, PropList, S0):- 
 findall(P, (getprop(Object, P, S0), is_prop_public(Sense,P)), PropListL),
 list_to_set(PropListL,PropList).

                                       
act_examine(Agent, Sense, PrepIn, Here, Depth, S0, S9) :- % next_depth(Depth2 is Depth -1),
 \+ \+ h(exit(_), Here, _, S0),
 Depth = depth(3), 
 % h(PrepIn, Agent, Here, S0), !,

 nearby_objs(Agent, Here, Nearby, S0),
 object_props(Here, Sense, PropList, S0),
 exits_of(PrepIn, Here, Exits, S0),
 queue_agent_percept(Agent,
    [       %you_are(Agent, Relation, Here),
             notice_children(Agent, Sense, Here, PrepIn, Depth, Nearby), 
             sense_props(Agent, Sense, Here, depth(2), PropList),
             exits_are(Agent, PrepIn, Here, Exits) ],
    S0, S9).

act_examine(Agent, Sense, PrepIn, Object, Depth, S0, S2):- Depth = depth(DepthN),
 object_props(Object, Sense, PropList, S0),
 (DepthN>2 -> queue_agent_percept(Agent, sense_props(Agent, Sense, Object, Depth, PropList), S0, S1) ; S0=S1), 
 (DepthN>0 -> add_child_precepts(Depth,Sense,Agent,PrepIn, Object,S1,S2) ; S1=S2),!.

get_relation_list(Object, RelationSet, S1) :- 
  findall_set(Relation, 
     ((getprop(Object,has_rel(Relation,t),S1);      
      (declared(h(Relation, _, Object),S1))),
     Relation\=exit(_)), RelationSet).

add_child_precepts(Depth, Sense, Agent, PrepIn, Object, S1, S2):- 
 get_relation_list(Object, RelationSet, S1),
 (member(PrepIn,RelationSet) -> UseRelationSet = [PrepIn] ; UseRelationSet= RelationSet),
 % dmsg(get_relation_list(Object, RelationSet)),
 findall(notice_children(Agent, Sense, Object, Relation, Depth, Children),
     ((member(Relation,UseRelationSet),
       child_precepts(Agent, Sense, Object, Relation, Depth, Children, S1))), PreceptS),
 queue_agent_percept(Agent,PreceptS, S1, S2).

child_precepts(_Agent, see, Object, in, _Depth, '<unknown closed>', S1):- is_closed(Object,S1),!.
child_precepts(Agent, Sense, Object, Relation, _Depth, Children, S1):- 
 findall_set(What,  
  (h(Relation, What, Object, S1), 
   nop(once(can_sense(Agent, Sense, What, S1)))), 
   Children). 

