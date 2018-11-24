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

nearby_objs(Agent, Here, Nearby, S0):- 
 ignore(h(At, Agent, Here, S0)),
 findall_set(What,  
   (h(At, What, Here, S0),
    sub_objs(descended, Here, What, S0)),
   Nearby).

sub_objs(At, Here, What, S0):- 
  h(At, What, Here, S0),
 \+ ((h(inside, What, Container, S0), 
   Container\==Here, h(descended, Container, Here, S0))).

prep_object_exitnames(in, Object, Exits, S0) :- 
  findall(Direction, h(exit(Direction), Object, _, S0), Exits), Exits\==[], !.
prep_object_exitnames(in, _Object, [escape], _S0) :- !.
prep_object_exitnames(on, _Object, [escape], _S0) :- !.
prep_object_exitnames(under, _Object, [escape], _S0) :- !.
prep_object_exitnames(at, _Object, [escape], _S0) :- !.
prep_object_exitnames(Other, _Object, [reverse(Other)], _S0).

object_props(Object, Sense, PropList, S0):- 
 findall(P, (getprop(Object, P, S0), is_prop_public(Sense,P)), PropListL),
 list_to_set(PropListL,PropList).

/*
act_examine(Agent, Sense, PrepIn, Here, Depth, S0, S9) :- 
 \+ \+ h(exit(_), Here, _, S0),
 Depth = depth(3), 
 % h(PrepIn, Agent, Here, S0), !,
 nearby_objs(Agent, Here, Nearby, S0),
 object_props(Here, Sense, PropList, S0),
 prep_object_exitnames(PrepIn, Here, Exits, S0),
 queue_agent_percept(Agent,
    [       %you_are(Agent, At, Here),
             percept_children(Agent, Sense, Here, PrepIn, Depth, Nearby), 
             percept_props(Agent, Sense, Here, depth(2), PropList),
             percept_exits(Agent, PrepIn, Here, Exits) ],
    S0, S9).
*/

act_examine(Agent, Sense, PrepIn, Object, Depth, S0, S3):- Depth = depth(DepthN),
 (DepthN>2 -> (object_props(Object, Sense, PropList, S0), queue_agent_percept(Agent, percept_props(Agent, Sense, Object, Depth, PropList), S0, S1)) ; S0 = S1), 
 (DepthN>0 -> (add_child_precepts(Depth,Sense,Agent,PrepIn, Object, S1, S2)) ; S1 = S2),
 (DepthN>2 -> (prep_object_exitnames(PrepIn, Object, Exits, S0), queue_agent_percept(Agent, percept_exits(Agent, PrepIn, Object, Exits), S2, S3)) ; S2 = S3),!.


get_relation_list(Object, RelationSet, S1) :- 
  findall_set(At, 
     ((getprop(Object,has_rel(At,t),S1);      
      (declared(h(At, _, Object),S1))),
     At\=exit(_)), RelationSet).

add_child_precepts(Depth, Sense, Agent, PrepIn, Object, S1, S2):- 
 get_relation_list(Object, RelationSet, S1),
 (member(PrepIn,RelationSet) -> UseRelationSet = [PrepIn] ; UseRelationSet= RelationSet),
 % dmsg(get_relation_list(Object, RelationSet)),
 findall(percept_children(Agent, Sense, Object, At, Depth, Children),
     ((member(At,UseRelationSet),
       child_precepts(Agent, Sense, Object, At, Depth, Children, S1))), PreceptS),
 queue_agent_percept(Agent,PreceptS, S1, S2).

child_precepts(Agent, Sense, Object, At, Depth, Children, S0):-  At == at,
 getprop(Object, default_rel = Default, S0), Default\==At, !,
 child_precepts(Agent, Sense, Object, Default, Depth, Children, S0).
child_precepts(_Agent, _All, Object, At, _Depth, '<mystery>'(closed,At,Object), S1):- is_closed(At, Object, S1),!.
/*act_examine(Agent, Sense, At, Here, Depth, S0, S9):-  At == at,
 getprop(Object, default_rel = Default, S0), Default\==At, !,
 act_examine(Agent, Sense, Default, Here, Depth, S0, S9).
*/
child_precepts(Agent, Sense, Object, At, _Depth, Children, S1):- 
 findall_set(What,  
  (h(At, What, Object, S1), 
   nop(once(can_sense(Agent, Sense, What, S1)))), 
   Children). 

