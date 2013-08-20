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

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_plan_opers')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- op(900, fy, '~').

precond_matches_effect(Cond, Cond).


precond_matches_effects(path(Spatial, Here, There), StartEffects) :-
 find_path(Spatial, Here, There, _Route, StartEffects).
precond_matches_effects(exists(Spatial, Object), StartEffects) :-
 in_model(h_at(Spatial, _, Object, _, _), StartEffects)
 ;
 in_model(h_at(Spatial, _, _, Object, _), StartEffects).
precond_matches_effects(Cond, Effects) :-
 member(E, Effects),
 precond_matches_effect(Cond, E).

 
% oper(Self, Action, Desc, Preconds, Effects)

% go north
oper(Self, goto(Self, Walk, loc(Self, Dir, Rel, There)),
  [ %Desc: cap(subj(Agent)), person(go, goes)
  cap(subj(actor(Self))), does(Walk), from(place(Here)), via(exit(Dir)) , Rel, to(place(There)) ],
  [ %Preconds:
  Here \= Self, There \= Self,
  \+ props(Self, can_do(goto, f)),
  h(Spatial, WasRel, Self, Here),
  props(Here, inherit(place, t)),
  props(There, inherit(place, t)),
  \+ is_state(~(open), There),
  \+ is_state(~(open), Here),
  \+ is_state(~(open), Dir),
  h(Spatial, exit(Dir), Here, There)], % path(Spatial, Here, There)
  [ %Postconds:
  h_at(Spatial, Rel, Self, There, _),
  ~ h_at(Spatial, WasRel, Self, Here, _)]).



% oper(Self, Action, Preconds, Effects)
oper(Self, Action, Preconds, Effects):- % Hooks to better KR above
 oper(Self, Action, _Desc, Preconds, Effects).


oper(Self, goto(Self, _Walk, loc(Self, Dir, Rel, There)),
  [ Here \= Self, There \= Self,
  h_at(Spatial, WasRel, Self, Here, _),
  h_at(Spatial, exit(Dir), Here, There, _)], % path(Spatial, Here, There)
  [ h_at(Spatial, Rel, Self, There, _),
  ~ h_at(Spatial, WasRel, Self, Here, _)]).

oper(Self, take(Self, Thing), % from same room
  [ Thing \= Self, exists(Spatial, Thing),
  There \= Self,
  h_at(Spatial, At, Thing, There, _T0),
  h_at(Spatial, At, Self, There, _T1)],
  [ h_at(Spatial, held_by, Thing, Self, _T2),
  ~ h_at(Spatial, At, Thing, There, _T3)]).
oper(Self, take(Self, Thing), % from something else
  [ Thing \= Self, exists(Spatial, Thing),
  h_at(Spatial, Prep, Thing, What, _),
  h_at(Spatial, At, What, There, _),
  h_at(Spatial, At, Self, There, _) ],
  [ h_at(Spatial, held_by, Thing, Self, _),
  ~ h_at(Spatial, Prep, Thing, There, _)]):- extra.
oper(Self, drop(Self, Thing),
  [ Thing \= Self, exists(Spatial, Thing),
  h_at(Spatial, held_by, Thing, Self, _)],
  [ ~ h_at(Spatial, held_by, Thing, Self, _)] ).
oper(Self, emote(Self, say, Player, [please, give, Self, the(Thing)]),
  [ Thing \= Self, exists(Spatial, Thing),
  h_at(Spatial, held_by, Thing, Player, _),
  h_at(Spatial, Prep, Player, Where, _),
  h_at(Spatial, Prep, Self, Where, _) ],
  [ h_at(Spatial, held_by, Thing, Self, _),
  ~ h_at(Spatial, held_by, Thing, Player, _)] ):- extra.
oper(Self, give(Self, Thing, Recipient),
  [ Thing \= Self, Recipient \= Self,
  exists(Spatial, Thing), exists(Spatial, Recipient),
  Where \= Self,
  h_at(Spatial, held_by, Thing, Self, _),
  h_at(Spatial, in, Recipient, Where, _), exists(Spatial, Where),
  h_at(Spatial, in, Self, Where, _)],
  [ h_at(Spatial, held_by, Thing, Recipient, _),
  ~ h_at(Spatial, held_by, Thing, Self, _)
  ] ).
oper(Self, put(Self, Spatial, Thing, Relation, What), % in something else
  [ Thing \= Self, What \= Self, Where \= Self,
  Thing \= What, What \= Where, Thing \= Where,
  h_at(Spatial, held_by, Thing, Self, _), exists(Spatial, Thing),
  h_at(Spatial, in, What, Where, _), exists(Spatial, What), exists(Spatial, Where),
  h_at(Spatial, in, Self, Where, _)],
  [ h_at(Spatial, Relation, Thing, What, _),
  ~ h_at(Spatial, held_by, Thing, Self, _)] ).
oper(Self, put(Self, Spatial, Thing, Relation, Where), % in room
  [ Thing \= Self, exists(Spatial, Thing),
  h_at(Spatial, held_by, Thing, Self, _),
  h_at(Spatial, Relation, Self, Where, _)],
  [ h_at(Spatial, Relation, Thing, Where, _),
  ~ h_at(Spatial, held_by, Thing, Self, _)] ) :- extra.

% Return an operator after substituting Agent for Self.
operagent(Agent, Action, Conds, Effects) :- oper(Agent, Action, Conds, Effects).

% Return the initial list of operators.
initial_operators(Agent, Operators) :-
 findall(oper(Agent, Action, Conds, Effects),
   operagent(Agent, Action, Conds, Effects),
   Operators).

precondition_matches_effect(Cond, Effect) :-
 % player_format('  Comparing cond ~w with effect ~w: ', [Cond, Effect]),
 Cond = Effect. %, player_format('match~n', []).
%precondition_matches_effect(~ ~ Cond, Effect) :-
% precondition_matches_effect(Cond, Effect).
%precondition_matches_effect(Cond, ~ ~ Effect) :-
% precondition_matches_effect(Cond, Effect).
precondition_matches_effects(Cond, Effects) :-
 member(E, Effects),
 precondition_matches_effect(Cond, E).
preconditions_match_effects([Cond|Tail], Effects) :-
 precondition_matches_effects(Cond, Effects),
 preconditions_match_effects(Tail, Effects).

% plan(steps, orderings, bindings, links)
% step(id, operation)
new_plan(_Agent, CurrentState, GoalState, Plan) :-
 Plan = plan([step(start , oper(Self, true, [], CurrentState)),
    step(finish, oper(Self, true, GoalState, []))],
    [before(start, finish)],
    [],
    []).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_util_ordering')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isbefore(I, J, Orderings) :-
 member(before(I, J), Orderings).
%isbefore(I, K, Orderings) :-
% select(before(I, J), Orderings, Remaining),
% isbefore(J, K, Remaining).

% These will fail to create inconsistent orderings.
%add_ordering(B, Orderings, Orderings) :-
% member(B, Orderings), !.
%add_ordering(before(I, K), Orderings, [before(I, K)|Orderings]) :-
% I \= K,
% \+ isbefore(K, I, Orderings),
% bugout(' ADDED ~w to orderings.~n', [before(I, K)], planner).
%add_ordering(B, O, O) :-
% bugout(' FAILED to add ~w to orderings.~n', [B], planner),
% fail.

add_ordering(B, Orderings, Orderings) :-
 member(B, Orderings), !.
add_ordering(before(I, J), Order0, Order1) :-
 I \= J,
 \+ isbefore(J, I, Order0),
 add_ordering3(before(I, J), Order0, Order0, Order1).
add_ordering(B, Order0, Order0) :-
 once(pick_ordering(Order0, List)),
 bugout(' FAILED add_ordering ~w to ~w~n', [B, List], planner),
 fail.

% add_ordering3(NewOrder, ToCheck, OldOrderings, NewOrderings)
add_ordering3(before(I, J), [], OldOrderings, NewOrderings) :-
 union([before(I, J)], OldOrderings, NewOrderings).
add_ordering3(before(I, J), [before(J, K)|Rest], OldOrderings, NewOrderings) :-
 I \= K,
 union([before(J, K)], OldOrderings, Orderings1),
 add_ordering3(before(I, J), Rest, Orderings1, NewOrderings).
add_ordering3(before(I, J), [before(H, I)|Rest], OldOrderings, NewOrderings) :-
 H \= J,
 union([before(H, J)], OldOrderings, Orderings1),
 add_ordering3(before(I, J), Rest, Orderings1, NewOrderings).
add_ordering3(before(I, J), [before(H, K)|Rest], OldOrderings, NewOrderings) :-
 I \= K,
 H \= J,
 add_ordering3(before(I, J), Rest, OldOrderings, NewOrderings).

% insert(E, L, L1) inserts E into L producing L1
% E is not added it is already there.
insert(X, [], [X]).
insert(A, [A|R], [A|R]).
insert(A, [B|R], [B|R1]) :-
 A \== B,
 insert(A, R, R1).

add_orderings([], Orderings, Orderings).
add_orderings([B|Tail], Orderings, NewOrderings) :-
 add_ordering(B, Orderings, Orderings2),
 add_orderings(Tail, Orderings2, NewOrderings).

del_ordering_node(I, [before(I, _)|Tail], Orderings) :-
 del_ordering_node(I, Tail, Orderings).
del_ordering_node(I, [before(_, I)|Tail], Orderings) :-
 del_ordering_node(I, Tail, Orderings).
del_ordering_node(I, [before(X, Y)|Tail], [before(X, Y)|Orderings]) :-
 X \= I,
 Y \= I,
 del_ordering_node(I, Tail, Orderings).
del_ordering_node(_I, [], []).

ordering_nodes(Orderings, Nodes) :-
 setof(Node,
  Other^(isbefore(Node, Other, Orderings);isbefore(Other, Node, Orderings)),
  Nodes).

pick_ordering(Orderings, List) :-
 ordering_nodes(Orderings, Nodes),
 pick_ordering(Orderings, Nodes, List).

pick_ordering(Orderings, Nodes, [I|After]) :-
 select(I, Nodes, RemainingNodes),
 forall(member(J, RemainingNodes), \+ isbefore(J, I, Orderings) ),
 pick_ordering(Orderings, RemainingNodes, After).
pick_ordering(_Orderings, [], []).

test_ordering :-
 bugout('ORDERING TEST:~n', planner),
 Unordered =
 [ before(start, finish),
  before(start, x),
  before(start, y), before(y, finish),
  before(x, z),
  before(z, finish)
 ],
 once(add_orderings(
 Unordered,
 [],
 Orderings)),
 bugout(' unordered was ~w~n', [Unordered], planner),
 bugout(' ordering is ~w~n', [Orderings], planner),
 pick_ordering(Orderings, List),
 bugout(' picked ~w~n', [List], planner),
 fail.
test_ordering :- bugout(' END ORDERING TEST~n', planner).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_planner_conds')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cond_is_achieved(step(J, _Oper), C, plan(Steps, Orderings, _, _)) :-
 member(step(I, oper(_Self, _, _, Effects)), Steps),
 precondition_matches_effects(C, Effects),
 isbefore(I, J, Orderings),
 bugout('  Cond ~w of step ~w is achieved!~n', [C, J], planner).
cond_is_achieved(step(J, _Oper), C, plan(_Steps, _Orderings, _, _)) :-
 bugout('  Cond ~w of step ~w is NOT achieved.~n', [C, J], planner),
 !, fail.

% Are the preconditions of a given step achieved by the effects of other
% steps, or are already true?
step_is_achieved(step(_J, oper(_Self, _, [], _)), _Plan). % No conditions, OK.
step_is_achieved(step(J, oper(Self, _, [C|Tail], _)), plan(Steps, Orderings, _, _)) :-
 cond_is_achieved(step(J, _), C, plan(Steps, Orderings, _, _)),
 step_is_achieved(step(J, oper(Self, _, Tail, _)), plan(Steps, Orderings, _, _)).

all_steps_are_achieved([Step|Tail], Plan) :-
 step_is_achieved(Step, Plan),
 all_steps_are_achieved(Tail, Plan).
all_steps_are_achieved([], _Plan).

is_solution(plan(Steps, O, B, L)) :-
 all_steps_are_achieved(Steps, plan(Steps, O, B, L)).

% Create a new step given an operator.
operator_as_step(oper(Self, Act, Cond, Effect), step(Id, oper(Self, Act, Cond, Effect))) :-
 Act =.. [Functor|_],
 atom_concat(Functor, '_step_', Prefix),
 gensym(Prefix, Id).

% Create a list of new steps given a list of operators.
operators_as_steps([], []).
operators_as_steps([Oper | OpTail], [Step | StepTail]) :-
 copy_term(Oper, FreshOper), % Avoid instantiating operator database.
 operator_as_step(FreshOper, Step),
 operators_as_steps(OpTail, StepTail).

cond_as_goal(ID, Cond, goal(ID, Cond)).
conds_as_goals(_, [], []).
conds_as_goals(ID, [C|R], [G|T]) :-
 cond_as_goal(ID, C, G),
 conds_as_goals(ID, R, T).

cond_equates(Cond0, Cond1) :- Cond0 = Cond1.
cond_equates(h_at(Spatial, X, Y, Z, _), h_at(Spatial, X, Y, Z, _)).
cond_equates(~ ~ Cond0, Cond1) :- cond_equates(Cond0, Cond1).
cond_equates(Cond0, ~ ~ Cond1) :- cond_equates(Cond0, Cond1).

cond_negates(~ Cond0, Cond1) :- cond_equates(Cond0, Cond1).
cond_negates(Cond0, ~ Cond1) :- cond_equates(Cond0, Cond1).

% Protect 1 link from 1 condition
% protect(link_to_protect, threatening_step, threatening_cond, ...)
protect(causes(StepI, _Cond0, _StepJ), StepI, _Cond1, Order0, Order0) :-
 !. % Step does not threaten itself.
protect(causes(_StepI, _Cond0, StepJ), StepJ, _Cond1, Order0, Order0) :-
 !. % Step does not threaten itself.
%protect(causes(_StepI, Cond, _StepJ), _StepK, Cond, Order0, Order0) :-
% !. % Cond does not threaten itself.
protect(causes(_StepI, Cond0, _StepJ), _StepK, Cond1, Order0, Order0) :-
 \+ cond_negates(Cond0, Cond1),
 !.
protect(causes(StepI, Cond0, StepJ), StepK, _Cond1, Order0, Order0) :-
 bugout(' THREAT: ~w <> causes(~w, ~w, ~w)~n',
   [StepK, StepI, Cond0, StepJ], planner),
 fail.
protect(causes(StepI, _Cond0, StepJ), StepK, _Cond1, Order0, Order1) :-
 % Protect by moving threatening step before or after this link.
 add_ordering(before(StepK, StepI), Order0, Order1),
 bugout(' RESOLVED with ~w~n', [before(StepK, StepI)], planner)
 ;
 add_ordering(before(StepJ, StepK), Order0, Order1),
 bugout(' RESOLVED with ~w~n', [before(StepJ, StepK)], planner).
protect(causes(StepI, Cond0, StepJ), StepK, _Cond1, Order0, Order0) :-
 bugout(' FAILED to resolve THREAT ~w <> causes(~w, ~w, ~w)~n',
   [StepK, StepI, Cond0, StepJ], planner),
 once(pick_ordering(Order0, Serial)),
 bugout(' ORDERING is ~w~n', [Serial], planner),
 fail.

% Protect 1 link from 1 step's multiple effects
protect_link(_Link, _StepID, [], Order0, Order0).
protect_link(Link, StepID, [Cond|Effects], Order0, Order2):-
 protect(Link, StepID, Cond, Order0, Order1),
 protect_link(Link, StepID, Effects, Order1, Order2).

% Protect all links from 1 step's multiple effects
% protect_links(links_to_protect, threatening_step, threatening_cond, ...)
protect_links([], _StepID, _Effects, Order0, Order0).
protect_links([Link|Tail], StepID, Effects, Order0, Order2) :-
 protect_link(Link, StepID, Effects, Order0, Order1),
 protect_links(Tail, StepID, Effects, Order1, Order2).

% Protect 1 link from all steps' multiple effects
protect_link_all(_Link, [], Order0, Order0).
protect_link_all(Link, [step(StepID, oper(_Self, _, _, Effects))|Steps], Order0, Order2) :-
 protect_link(Link, StepID, Effects, Order0, Order1),
 protect_link_all(Link, Steps, Order1, Order2).

%add_binding((X\=Y), Bindings0, Bindings) :-
% X \= Y, % if they can't bind, don't bother to add them.
add_binding((X\=Y), Bindings, [(X\=Y)|Bindings]) :-
 X \== Y, % if they're distinct,
 % \+ \+ X=Y, % but could bind
 bindings_valid(Bindings).

bindings_valid([]).
bindings_valid([(X\=Y)|Bindings]) :-
 X \== Y,
 bindings_valid(Bindings).
%bindings_valid(B) :-
% bugout(' BINDINGS are *INVALID*: ~w~n', [B], planner),
% fail.

bindings_safe([]) :- bugout(' BINDINGS are SAFE~n', planner).
bindings_safe([(X\=Y)|Bindings]) :-
 X \= Y,
 bindings_safe(Bindings).
%bindings_safe(B) :-
% bugout(' BINDINGS are *UNSAFE*: ~w~n', [B], planner),
% fail.


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_planner_main')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


choose_operator([goal(GoalID, GoalCond)|Goals0], Goals0,
     _Operators,
     plan(Steps, Order0, Bindings, OldLinks),
     plan(Steps, Order9, Bindings, NewLinks),
     Depth, Depth ) :-
 % Achieved by existing step?
 member(step(StepID, oper(_Self, _Action, _Preconds, Effects)), Steps),
 precondition_matches_effects(GoalCond, Effects),
 add_ordering(before(StepID, GoalID), Order0, Order1),
 % Need to protect new link from all existing steps
 protect_link_all(causes(StepID, GoalCond, GoalID), Steps, Order1, Order9),
 union([causes(StepID, GoalCond, GoalID)], OldLinks, NewLinks),
 bindings_valid(Bindings),
 bugout(' EXISTING step ~w satisfies ~w~n', [StepID, GoalCond], planner).
choose_operator([goal(_GoalID, X \= Y)|Goals0], Goals0,
     _Operators,
     plan(Steps, Order, Bindings, Links),
     plan(Steps, Order, NewBindings, Links),
     Depth, Depth ) :-
 add_binding((X\=Y), Bindings, NewBindings),
 bugout(' BINDING ADDED: ~w~n', [X\=Y], planner).
choose_operator([goal(GoalID, ~ GoalCond)|Goals0], Goals0,
     _Operators,
     plan(Steps, Order0, Bindings, OldLinks),
     plan(Steps, Order9, Bindings, NewLinks),
     Depth, Depth ) :-
 % Negative condition achieved by start step?
 memberchk(step(start, oper(_Self, _Action, _Preconds, Effects)), Steps),
 \+ precondition_matches_effects(GoalCond, Effects),
 add_ordering(before(start, GoalID), Order0, Order1),
 % Need to protect new link from all existing steps
 protect_link_all(causes(start, GoalCond, GoalID), Steps, Order1, Order9),
 union([causes(start, ~ GoalCond, GoalID)], OldLinks, NewLinks),
 bindings_valid(Bindings),
 bugout(' START SATISFIES NOT ~w~n', [GoalCond], planner).
choose_operator([goal(GoalID, exists(Spatial, GoalCond))|Goals0], Goals0,
     _Operators,
     plan(Steps, Order0, Bindings, OldLinks),
     plan(Steps, Order9, Bindings, NewLinks),
     Depth, Depth ) :-
 memberchk(step(start, oper(_Self, _Action, _Preconds, Effects)), Steps),
 ( in_model(h_at(Spatial, _Prep, GoalCond, _Where, _), Effects);
 in_model(h_at(Spatial, _Prep, _What, GoalCond, _), Effects)),
 add_ordering(before(start, GoalID), Order0, Order1),
 % Need to protect new link from all existing steps
 protect_link_all(causes(start, GoalCond, GoalID), Steps, Order1, Order9),
 union([causes(start, exists(Spatial, GoalCond), GoalID)], OldLinks, NewLinks),
 bindings_valid(Bindings),
 bugout(' START SATISFIES exists(Spatial, ~w)~n', [GoalCond], planner).
choose_operator([goal(GoalID, GoalCond)|Goals0], Goals2,
     Operators,
     plan(OldSteps, Order0, Bindings, OldLinks),
     plan(NewSteps, Order9, Bindings, NewLinks),
     Depth0, Depth ) :-
 % Condition achieved by new step?
 Depth0 > 0,
 Depth is Depth0 - 1,
 %operators_as_steps(Operators, FreshSteps),
 copy_term(Operators, FreshOperators),
 % Find a new operator.
 %member(step(StepID, oper(Self, Action, Preconds, Effects)), FreshSteps),
 member(oper(Self, Action, Preconds, Effects), FreshOperators),
 precondition_matches_effects(GoalCond, Effects),
 operator_as_step(oper(Self, Action, Preconds, Effects),
     step(StepID, oper(Self, Action, Preconds, Effects)) ),
 % Add ordering constraints.
 add_orderings([before(start, StepID),
     before(StepID, GoalID),
     before(StepID, finish)],
    Order0, Order1),
 % Need to protect existing links from new step.
 protect_links(OldLinks, StepID, Effects, Order1, Order2),
 % Need to protect new link from all existing steps
 protect_link_all(causes(StepID, GoalCond, GoalID), OldSteps, Order2, Order9),
 % Add the step.
 append(OldSteps, [step(StepID, oper(Self, Action, Preconds, Effects))], NewSteps),
 % Add causal constraint.
 union([causes(StepID, GoalCond, GoalID)], OldLinks, NewLinks),
 % Add consequent goals.
 conds_as_goals(StepID, Preconds, NewGoals),
 append(Goals0, NewGoals, Goals2),
 bindings_valid(Bindings),
 bugout(' ~w CREATED ~w to satisfy ~w~n',
   [Depth, StepID, GoalCond], autonomous),
 pprint(oper(_Self, Action, Preconds, Effects), planner),
 once(pick_ordering(Order9, List)),
 bugout(' Orderings are ~w~n', [List], planner).
choose_operator([goal(GoalID, GoalCond)|_G0], _G2, _Op, _P0, _P2, D, D) :-
 bugout(' CHOOSE_OPERATOR FAILED on goal:~n goal(~w, ~w)~n',
   [GoalID, GoalCond], planner),
 !, fail.
choose_operator(G0, _G2, _Op, _P0, _P2, D, D) :-
 bugout(' !!! CHOOSE_OPERATOR FAILED: G0 = ~w~n', [G0], planner), !, fail.

planning_loop([], _Operators, plan(S, O, B, L), plan(S, O, B, L), _Depth, _TO ) :-
 bugout('FOUND SOLUTION?~n', planner),
 bindings_safe(B).
planning_loop(Goals0, Operators, Plan0, Plan2, Depth0, Timeout) :-
 %Limit > 0,
 get_time(Now),
 (Now > Timeout -> throw(timeout(planner)); true),
 bugout('GOALS ARE: ~w~n', [Goals0], planner),
 choose_operator(Goals0, Goals1, Operators, Plan0, Plan1, Depth0, Depth),
 %Limit2 is Limit - 1,
 planning_loop(Goals1, Operators, Plan1, Plan2, Depth, Timeout).
%planning_loop(_Goals0, _Operators, Plan0, Plan0, _Limit) :-
% Limit < 1,
% bugout('Search limit reached!~n', planner),
% fail.

serialize_plan(plan([], _Orderings, _B, _L), []) :- !.

serialize_plan(plan(Steps, Orderings, B, L), Tail) :-
 select(step(_, oper(_Self, true, _, _)), Steps, RemainingSteps),
 !,
 serialize_plan(plan(RemainingSteps, Orderings, B, L), Tail).

serialize_plan(plan(Steps, Orderings, B, L), [Action|Tail]) :-
 select(step(StepI, oper(_Self, Action, _, _)), Steps, RemainingSteps),
 \+ (member(step(StepJ, _Oper), RemainingSteps),
  isbefore(StepJ, StepI, Orderings)),
 serialize_plan(plan(RemainingSteps, Orderings, B, L), Tail).

serialize_plan(plan(_Steps, Orderings, _B, _L), _) :-
 bugout('serialize_plan FAILED!~n', planner),
 pick_ordering(Orderings, List),
 bugout(' Orderings are ~w~n', [List], planner),
 fail.

select_unsatisfied_conditions([], [], _Model) :- !.
select_unsatisfied_conditions([Cond|Tail], Unsatisfied, ModelData) :-
 precondition_matches_effects(Cond, ModelData),
 !,
 select_unsatisfied_conditions(Tail, Unsatisfied, ModelData).
select_unsatisfied_conditions([~ Cond|Tail], Unsatisfied, ModelData) :-
 \+ precondition_matches_effects(Cond, ModelData),
 !,
 select_unsatisfied_conditions(Tail, Unsatisfied, ModelData).
select_unsatisfied_conditions([Cond|Tail], [Cond|Unsatisfied], ModelData) :-
 !,
 select_unsatisfied_conditions(Tail, Unsatisfied, ModelData).

depth_planning_loop(PlannerGoals, Operators, SeedPlan, FullPlan,
     Depth, Timeout) :-
 bugout('PLANNING DEPTH is ~w~n', [Depth], autonomous),
 planning_loop(PlannerGoals, Operators, SeedPlan, FullPlan, Depth, Timeout),
 !.
depth_planning_loop(PlannerGoals, Operators, SeedPlan, FullPlan,
     Depth0, Timeout) :-
 Depth0 =< 7,
 Depth is Depth0 + 1,
 depth_planning_loop(PlannerGoals, Operators, SeedPlan, FullPlan,
      Depth, Timeout).

generate_plan(FullPlan, Mem0) :-
 thought(inst(Agent), Mem0),
 initial_operators(Agent, Operators),
 bugout('OPERATORS are:~n', planner), pprint(Operators, planner),
 thought_model(ModelData, Mem0),
 %bugout('CURRENT STATE is ~w~n', [Model0], planner),
 thought(goals(Goals), Mem0),
 new_plan(Agent, ModelData, Goals, SeedPlan),
 bugout('SEED PLAN is:~n', planner), pprint(SeedPlan, planner),
 !,
 %planning_loop(Operators, SeedPlan, FullPlan),
 conds_as_goals(finish, Goals, PlannerGoals),
 get_time(Now),
 Timeout is Now + 60, % seconds
 catch(
 depth_planning_loop(PlannerGoals, Operators, SeedPlan, FullPlan,
      1, Timeout),
 timeout(planner),
 (bugout('PLANNER TIMEOUT~n', autonomous), fail)
 ),
 bugout('FULL PLAN is:~n', planner), pprint(FullPlan, planner).

% ----


path2directions(Spatial, [Here, There], [ goto(Self, _Walk, loc(Self, Dir, _To, There))], ModelData) :-
 in_model(h_at(Spatial, exit(Dir), Here, There, _), ModelData).

path2directions(Spatial, [Here, There], [ goto(Self, _Walk, loc(Self, _Dir, in, There))], ModelData) :-
 in_model(h_at(Spatial, descended, Here, There, _), ModelData).

path2directions(Spatial, [Here, Next|Trail], [goto(Self, _Walk, loc(Self, Dir, _To, _There))|Tail], ModelData) :-
 in_model(h_at(Spatial, exit(Dir), Here, Next, _), ModelData),
 path2directions(Spatial, [Next|Trail], Tail, ModelData).

path2directions(Spatial, [Here, Next|Trail], [goto(Self, _Walk, loc(Self, _Dir, in, Next))|Tail], ModelData) :-
 in_model(h_at(Spatial, descended, Here, Next, _), ModelData),
 path2directions(Spatial, [Next|Trail], Tail, ModelData).

find_path1(_Spatial, [First|_Rest], Dest, First, _ModelData) :-
 First = [Dest|_].
find_path1(Spatial, [[Last|Trail]|Others], Dest, Route, ModelData) :-
 findall([Z, Last|Trail],
   (in_model(h_at(Spatial, _Prep, Last, Z, _), ModelData), \+ member(Z, Trail)),
   List),
 append(Others, List, NewRoutes),
 find_path1(Spatial, NewRoutes, Dest, Route, ModelData).
find_path(Spatial, Start, Dest, Route, ModelData) :-
 find_path1(Spatial, [[Start]], Dest, R, ModelData),
 reverse(R, RR),
 path2directions(Spatial, RR, Route, ModelData).



