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
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CODE FILE SECTION
:- nop(ensure_loaded('adv_main_states')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- dynamic(undo/2).
%undo([u, u, u, u, u, u, u, u]).
:- dynamic(advstate/1).
advstate([]).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CODE FILE SECTION
:- nop(ensure_loaded('adv_state')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -----------------------------------------------------------------------------
% State may be implemented differently in the future (as a binary tree or
% hash table, etc.), but for now is a List.  These (backtrackable) predicates
% hide the implementation:
% assert/record/declare/memorize/think/associate/know/retain/affirm/avow/
%   insist/maintain/swear/posit/postulate/allege/assure/claim/proclaim
% retract/erase/forget/un-declare/unthink/repress/supress
% retrieve/remember/recall/ask/thought/think-of/reminisc/recognize/review/
%   recollect/remind/look(Spatial)-up/research/establish/testify/sustain/attest/certify/
%   verify/prove
% simulation: declare/undeclare/declared
% perception:
% memory: memorize/forget/thought

% Like select, but always succeeds, for use in deleting.
select_always(Item, List, ListWithoutItem) :-
  select(Item, List, ListWithoutItem),
  !.
select_always(_Item, ListWithoutItem, ListWithoutItem).

% Like select, but with a default value if not found in List..
%select_default(Item, _DefaultItem, List, ListWithoutItem) :-
%  select(Item, List, ListWithoutItem).
%select_default(DefaultItem, DefaultItem, ListWithoutItem, ListWithoutItem).

% Manipulate simulation state
%declare(Fact, State):- player_local(Fact, Player), !, declare(wishes(Player, Fact), State).
declare((Fact1,Fact2), State, NewState) :- !,declare(Fact1, State, MidState),declare(Fact2, MidState, NewState).
declare(props(Object,Props), State, NewState) :- select(props(Object,OldProps), State, MidState),!,
  dmust((append(Props,OldProps,NewProps),!,declare(props(Object,NewProps), MidState, NewState))),!.
declare(Fact, State, NewState) :- notrace(((assertion(var(NewState)),dmust(append([Fact], State, NewState))))).

%undeclare(Fact, State):- player_local(Fact, Player), !, undeclare(wishes(Player, Fact), State).
undeclare(Fact, State, NewState):- notrace(undeclare_(Fact, State, NewState)).
undeclare_(Fact, State, NewState) :- copy_term(State, Copy), select(Fact, State, NewState),
    assertion( \+ member(Copy , NewState)).

%undeclare_always(Fact, State):- player_local(Fact, Player), !, undeclare_always(wishes(Player, Fact), State).
undeclare_always(Fact, State, NewState) :- select_always(Fact, State, NewState).

%declared(Fact, State) :- player_local(Fact, Player), !, declared(wishes(Player, Fact), State).
declared(Fact, State) :- member(Fact, State).

player_local(Fact, Player):- atom(Fact), is_declared_thread_player(Fact), !, current_player(Player).

is_declared_thread_player(quit).
is_declared_thread_player(undo).



% Entire state of simulation & agents is held in one list, so it can be easy
% to roll back.  The state of the simulation consists of:
%   object properties
%   object relations
%   percept queues for agents
%   memories for agents (actually logically distinct from the simulation)
% Note that the simulation does not maintain any history.
% TODO: change state into a term:
%   ss(Objects, Relationships, PerceptQueues, AgentMinds)
% TODO:
%   store initial state as clauses which are collected up and put into a list,
%     like the operators are, to provide proper prolog variable management.
must_input_state(S0):- notrace(assertion(is_list(S0);must_state(S0))).
must_output_state(S0):- notrace(assertion(must_state(S0);is_list(S0))),notrace(check4bugs(S0)).
must_state(S0):- is_list(S0), nb_setval(advstate,S0).

get_objects(Spec, Set, State):- must_input_state(State), get_objects_(Spec, List, State, im(State)), !, list_to_set(List,Set).
%get_objects(_Spec, [player1, floyd], _State):-!.

get_objects_(_Spec, [], [], im(_)) :- !.
get_objects_(Spec, OutList, [Store|StateList], im(S0)):-     
  (( stores_props(Store, Object, PropList) -> filter_spec(Spec, PropList))
    ->  OutList = [Object|MidList]
    ; OutList = MidList), !,
   get_objects_(Spec, MidList, StateList, im(S0)).

stores_props(perceptq(Agent, PropList), Agent, PropList).
%stores_props(class_props(Agent, PropList), Agent, PropList).
stores_props(memories(Agent, PropList), Agent, PropList).
stores_props(props(Object, PropList), Object, PropList).

% Retrieve Prop.
% NOPE getprop(Object, state(Spatial, Prop, Value), State):- atom(Prop), !, getprop1(Object, state(Spatial, Prop, Value), State).
% NOPE getprop(Object, Prop, State):- getprop1(Object, Prop, state(Spatial, State, f)), !, fail.
% MAYBE getprop(Object, Prop, State):- atom(Prop), getprop1(Object, state(Spatial, Prop, t), State).
% MAYBE getprop(Object, Prop, State):- atom(Prop), getprop1(Object, state(Spatial, Prop, f), State), !, fail.


get_all_props(Object, AllProps, S0):- findall(Prop,getprop(Object, Prop, S0),AllProps).

getprop(Object, Prop, S0):-
  quietly((assertion(\+ atom(Prop)), getprop1(Object, Prop, S0)))
    *-> true; getprop2(Object, Prop, S0).

getprop2(Object, Prop, Memory):- notrace(member(state(S0), Memory)), !,
  getprop1(Object, Prop, S0).


getiprop(Object, Prop, S0) :-
  current_props(Object, PropList, S0),
  member(Prop, PropList).

getprop1(Object, Prop, S0) :- getiprop(Object, Prop, S0).
getprop1(Object, Prop, S0) :- \+ member(object(Object,t), S0),
  current_props(Object, PropList, S0),  
  member(inherit(Delegate,t), PropList),
  \+ member(inherited(Delegate), PropList),
  getprop1(Delegate, Prop, S0).

getprop_from_state(Object, Prop, Memory):-
  member(state(S0), Memory), !,
  getprop1(Object, Prop, S0).

% current_props(Object, PropList, S0):- atom(Object),atom_
% current_props(Object, PropList, S0):- atom(Object),atom_
%current_props(Object, PropList, S0):- declared(props(Object, PropList), S0).
current_props(Object, PropList, S0):- 
  declared(props(Object, PropList), S0) 
    *-> true 
      ; declared(class_props(Object, PropList), S0).

current_props_or(Object,PropList, Default, S0) :-
  declared(props(Object,PropList),S0)*->true; PropList=Default.

% Replace or create Prop.
setprop(Object, Prop, S0, S2) :- notrace((setprop_(Object, Prop, S0, S2))).


setprop_(Object, Prop, S0, S2) :- 
  assertion(compound(Prop)),

  current_props_or(Object, PropList, [], S0),
  undeclare_always(props(Object, _), S0, S1),

  functor(Prop,F,A),
  duplicate_term(Prop,Old),
  nb_setarg(A,Old,_),

  (select(Old, PropList, PropList2) ->
      (upmerge_prop(F,A,Old,Prop,Merged) ->
         ((Old==Merged,fail) -> S2=S0 ; 
           (append([Merged], PropList2, PropList3),declare(props(Object, PropList3), S1, S2)));
        append([Prop], PropList, PropList3),declare(props(Object, PropList3), S1, S2));
   (append([Prop], PropList, PropList3),declare(props(Object, PropList3), S1, S2))).


%     delprop_always(Object, Prop, S0U, S0a),

/*setprop(Object, Prop, S0, S2) :-
  %dmust((
  %assertion(\+ atom(Prop)),
  undeclare(props(Object, PropList), S0, S1),
  select_always(Prop, PropList, PropList2),
  append([Prop], PropList2, PropList3),
  declare(props(Object, PropList3), S1, S2))
    ->true;
  declare(props(Object, [Prop]), S0, S2)).
*/

upmerge_prop(_,_,Before,After,Result):- Before==After,!, Result=Before.
upmerge_prop(F,N,Before,After,Result):- arg(N,Before,B),arg(N,After,A),!,
  merge_value(F,N,B,A,R),duplicate_term(After,Result),nb_setarg(N,Result,R).

merge_value(_,_,_,t,R):- !, R = t.
merge_value(_,_,_,f,R):- !, R = f.
merge_value(_,_,_,[],R):- !, R = [].
merge_value(_,_,_,A,R):- number(A),!,A=R.

merge_value(_F,1,B,A,R):- B == A, !, R = A.

merge_value(_F,1,B,A,R):- (is_list(B);is_list(A)),flatten([B,A],R).

merge_value(_, 1,_,A,R):- number(A),!,A=R.
merge_value(_,1,_,_,_):- !,fail.
merge_value(_F,_,_B,A,R):- R = A.

% Replace or create Prop.
updateprop(Object, Prop, S00, S2) :- notrace((updateprop_(Object, Prop, S00, S2))).

updateprop_(Object, Prop, S0, S2) :-
  assertion(compound(Prop)),

  current_props_or(Object, PropList, [], S0),
  undeclare_always(props(Object, _), S0, S1),

  functor(Prop,F,A),
  duplicate_term(Prop,Old),
  nb_setarg(A,Old,_),

  (select(Old, PropList, PropList2) ->
      (upmerge_prop(F,A,Old,Prop,Merged) ->
         ((Old==Merged,fail) -> S2=S0 ; % no update
           (append([Merged], PropList2, PropList3),declare(props(Object, PropList3), S1, S2)));
        append([Prop], PropList, PropList3),declare(props(Object, PropList3), S1, S2));
   (append([Prop], PropList, PropList3),declare(props(Object, PropList3), S1, S2))).


% Remove Prop.
delprop(Object, Prop, S0, S2) :-
  dmust((
  assertion(\+ atom(Prop)),
  undeclare(props(Object, PropList), S0, S1),
  select(Prop, PropList, NewPropList),
  declare(props(Object, NewPropList), S1, S2))).


delprop_always(Object, Prop, S0, S2) :-
  assertion(\+ atom(Prop)),
  undeclare(props(Object, PropList), S0, S1),
  select(Prop, PropList, NewPropList),
  declare(props(Object, NewPropList), S1, S2).
delprop_always(_Object, _Prop, S0, S0).

