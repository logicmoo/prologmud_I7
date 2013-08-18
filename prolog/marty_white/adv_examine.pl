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

/*
nearby_objs(Agent, Here, Nearby, S0):- 
 ignore(h(At, Agent, Here, S0)),
 findall_set(What,  
   (h(At, What, Here, S0),
    sub_objs(descended, Here, What, S0)),
   Nearby).
*/

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


is_prop_public(_,N,_):- N == 5, !.
is_prop_public(_,N,_):- N == 4, admin, !.
is_prop_public(Sense, N, Prop):- is_prop_public_at(Sense,NL, Prop), !, N >= NL.
% is_prop_public(_,1,_):- !.

is_prop_public_at(_,_, P):- \+ callable(P),!,fail.

is_prop_public_at(see,1, desc).
is_prop_public_at(see,1, emitting).
is_prop_public_at(see,1, opened).
is_prop_public_at(see,1, shiny).
is_prop_public_at(touch,1, locked).
is_prop_public_at(see,1, shape).
is_prop_public_at(see,1, worn_on).

is_prop_public_at(see,1, in). % has_rel
is_prop_public_at(see,1, on). % has_rel

is_prop_public_at(knows,2, name).
is_prop_public_at(knows,2, eat).
is_prop_public_at(knows,2, has_rel).
is_prop_public_at(knows,2, default_rel).
is_prop_public_at(knows,1, adjs).
is_prop_public_at(knows,1, nouns).

is_prop_public_at(action,3, move).

is_prop_public_at(knows,3, inherit).
is_prop_public_at(knows,3, isnt).
is_prop_public_at(knows,3, inheriting).

is_prop_public_at(knows,4, inherited).
is_prop_public_at(knows,4, held_by).


is_prop_public_at(action,5, effect).
is_prop_public_at(action,5, after).
is_prop_public_at(action,5, before).
is_prop_public_at(action,5, breaks_into).
is_prop_public_at(action,5, oper).
is_prop_public_at(S,N, P):- var(N), compound(P), functor(P,F,_), is_prop_public_at(S, 5, F), !, N = 5.

is_prop_public_at(knows,4, can_be).
is_prop_public_at(knows,4, class_desc).
is_prop_public_at(_,4, co(_)).
is_prop_public_at(_,4, effect).
is_prop_public_at(_,4, has_rel).                            
is_prop_public_at(knows,4, has_sense).
is_prop_public_at(knows,4, knows_verbs).
is_prop_public_at(action,4, cant_go).

is_prop_public_at(_,_, P):- \+ compound(P), !, fail.
is_prop_public_at(S,N, F = _):- !, is_prop_public_at(S, N, F).
is_prop_public_at(S,N, P) :- arg(1, P, F), is_prop_public_at(S, N, F).
is_prop_public_at(S,N, P):- functor(P,F,_), is_prop_public_at(S, N, F).

object_props(Object, Sense, PropDepth, PropList, S0):- 
 findall(P, (getprop(Object, P, S0), is_prop_public(Sense, PropDepth, P)), PropListL),
 list_to_set(PropListL,PropList), !.
                                   

act_examine(Agent, Sense, PrepIn, Object, Depth, SA, S3):- Depth = depth(DepthN),
   (DepthN = 1 -> KnowsD = 3 ;  (DepthN = 2 -> KnowsD = 2 ; KnowsD = 1)),
   object_props(Object, knows, KnowsD, KPropList, S0), !, 
     %(PropList\==[] -> (queue_agent_percept(Agent, props(Object, KPropList), SA, S0)) ; SA = S0),
     %(PropList\==[] -> (queue_agent_percept(Agent, percept_props(Agent, knows, Object, KnowsD, KPropList), SA, S0)) ; SA = S0),
     (PropList\==[] -> (queue_agent_percept(Agent, percept(Agent, knows, KnowsD, props(Object, KPropList)), SA, S0)) ; SA = S0),
   object_props(Object, Sense, 3, PropList, S0), !, 
   %(PropList\==[] -> (queue_agent_percept(Agent, percept_props(Agent, Sense, Object, Depth, PropList), S0, S1)) ; S0 = S1), 
   (PropList\==[] -> (queue_agent_percept(Agent, percept(Agent, Sense, Depth, props(Object, PropList)), S0, S1)) ; S0 = S1), 
 add_child_precepts(Sense,Agent,PrepIn, DepthN, Object, S1, S2),
 (DepthN=1 -> (prep_object_exitnames(PrepIn, Object, Exits, S0), queue_agent_percept(Agent, percept(Agent, Sense, Depth, exits(PrepIn, Object, Exits)), S2, S3)) ; S2 = S3),!.


get_relation_list(Object, RelationSet, S1) :- 
  findall_set(At, 
     ((getprop(Object,has_rel(At,t),S1);      
      (declared(h(At, _, Object),S1))), 
     At\=exit(_)), RelationSet).

% add_child_precepts(_Sense, _Agent, _PrepIn, Depth, _Object, S1, S1):- Depth > 2, !.
add_child_precepts(Sense, Agent, PrepIn, Depth, Object, S1, S2):- 
 get_relation_list(Object, RelationSet, S1),
 (member(PrepIn,RelationSet) -> UseRelationSet = [PrepIn] ; UseRelationSet= RelationSet),
 % dmsg(get_relation_list(Object, RelationSet)),
 findall(percept(Agent, Sense, depth(Depth), child_list(Object, At, Children)),
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

