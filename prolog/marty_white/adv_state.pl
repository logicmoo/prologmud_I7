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



get_objects(Spec, Set, State):- get_objects(Spec, List, State, State), !, list_to_set(List,Set).
%get_objects(_Spec, [player1, floyd], _State):-!.

get_objects(Spec, OutList, [Store|StateList], S0):- !,
  (( stores_props(Store, Object, PropList) -> filter_spec(Spec, PropList))
    ->  OutList = [Object|MidList]
    ; OutList = MidList), !,
   get_objects(Spec, MidList, StateList, S0).
get_objects(_Spec, [], [], _) :- !.




check4bugs(_S0) :-
  !, true.
check4bugs(S0) :-
  % TODO: emergency save of S0, either here or better yet, in a catch().
  throw(check4bugs_failed(S0)).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CODE FILE SECTION
:- nop(ensure_loaded('adv_state')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Some Inform properties:
%   light - rooms that have light in them
%   can_be(Spatial, eat, t) - can be eaten
%   static - can't be taken or moved
%   scenery - assumed to be in the room description (implies static)
%   concealed - obscured, not listed, not part of 'all', but there
%   found_in - lists places where scenery objects are seen
%   absent - hides object entirely
%   clothing - can be worn
%   worn - is being worn
%   container
%   state(Spatial, open, t) - container is state(Spatial, open, t) (must be state(Spatial, open, t) to be used. there is no "closed").
%   can_be(Spatial, open, t) - can be opened and closed
%   capacity(N) - number of objects a container or supporter can hold
%   state(Spatial, locked, t) - cannot be opened
%   can_be(Spatial, lock, t), with_key
%   enterable
%   supporter
%   article - specifies indefinite article ('a', 'le')
%   cant_go
%   daemon - called each turn, if it is enabled for this object
%   description
%   inside_description
%   invent - code for inventory listing of that object
%   list_together - way to handle "5 fish"
%   plural - pluralized-name if different from singular
%   when_closed - description when closed
%   when_open - description when state(Spatial, open, t)
%   when_on, when_off - like when_closed, etc.
% Some TADS properties:
%   thedesc
%   pluraldesc
%   is_indistinguishable
%   is_visible(vantage)
%   is_reachable(actor)
%   valid(verb) - is object seeable, reachable, etc.
%   verification(verb) - is verb logical for this object
% Parser disambiguation:
%   eliminate objs not see, reachable, etc.
%   check preconditions for acting on a candidate object

% TODO: change agent storage into a term:
%   mind(AgentName, AgentType, History, ModelData, Goals /*, ToDo*/)


%create_object(Agent, S0, S2) :- declared(perceptq(Agent, []), S0), !,
%  dbug(existingAgent=Agent),
%  S2=S0.
                   
create_object(Object, S0, S0) :- declared(object(Object,t), S0),!.
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

  % As events happen, percepts are entered in the percept queue of each agent.
  % Each agent empties their percept queue as they see fit.
create_objprop(Object, inherit(perceptq), S0, S0):- declared(perceptq(Object,_),S0),!.
create_objprop(Object, inherit(perceptq), S0, S1):- !,
   declare(perceptq(Object, []), S0, S1).

  % Most agents store memories of percepts, world model, goals, etc.
create_objprop(Object, inherit(memorize), S0, S0):- declared(memories(Object,_),S0),!.
create_objprop(Object, inherit(memorize), S0, S2):- !,
  declared(props(Object, PropList), S0),
  copy_term(PropList,PropListC),!,
  % =(PropList,PropListC),!,
  declare(memories(Object, [
    timestamp(0),
    model(spatial, []),
    goals([]),
    todo([look]),
    inst(Object)|PropListC]), S0, S2).


create_objprop(Object, inherit(Other), S0, S9):- 
   (declared(props(Other, PropList), S0); PropList=[]),
   copy_term(PropList,PropListC),!,
   dmust(setprop(Object, inherited(Other), S0, S1)), !,
   dmust(create_objprop(Object, PropListC, S1, S2)),
   dmust(setprop(Object, inherit(Other), S2, S9)), !,
   !.
   
create_objprop(Object, Prop, S0, S2):- dmust(updateprop(Object,Prop,S0, S2)).

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
declare(Fact, State, NewState) :- assertion(var(NewState)),dmust(append([Fact], State, NewState)).

%undeclare(Fact, State):- player_local(Fact, Player), !, undeclare(wishes(Player, Fact), State).
undeclare(Fact, State, NewState) :- copy_term(State, Copy), select(Fact, State, NewState),
    assertion( \+ member(Copy , NewState)).

%undeclare_always(Fact, State):- player_local(Fact, Player), !, undeclare_always(wishes(Player, Fact), State).
undeclare_always(Fact, State, NewState) :- select_always(Fact, State, NewState).

%declared(Fact, State) :- player_local(Fact, Player), !, declared(wishes(Player, Fact), State).
declared(Fact, State) :- member(Fact, State).

player_local(Fact, Player):- atom(Fact), is_declared_thread_player(Fact), !, current_player(Player).

is_declared_thread_player(quit).
is_declared_thread_player(undo).

must_input_state(S0):- assertion(is_list(S0);must_state(S0)).
must_output_state(S0):- notrace(assertion(must_state(S0);is_list(S0))),check4bugs(S0).
must_state(S0):- is_list(S0), nb_setval(advstate,S0).


% Retrieve Prop.
% NOPE getprop(Object, state(Spatial, Prop, Value), State):- atom(Prop), !, getprop1(Object, state(Spatial, Prop, Value), State).
% NOPE getprop(Object, Prop, State):- getprop1(Object, Prop, state(Spatial, State, f)), !, fail.
% MAYBE getprop(Object, Prop, State):- atom(Prop), getprop1(Object, state(Spatial, Prop, t), State).
% MAYBE getprop(Object, Prop, State):- atom(Prop), getprop1(Object, state(Spatial, Prop, f), State), !, fail.

getprop(Object, Prop, State):-
  quietly((assertion(\+ atom(Prop)), getprop1(Object, Prop, State)))
    *-> true; getprop2(Object, Prop, State).

getprop2(Object, Prop, Memory):- member(state(_Spatial, State), Memory), !,
  getprop1(Object, Prop, State).

% current_props(Object, PropList, State):- atom(Object),atom_
current_props(Object, PropList, State):- declared(props(Object, PropList), State).

getiprop(Object, Prop, State) :-
  current_props(Object, PropList, State),
  member(Prop, PropList).

getprop1(Object, Prop, State) :- getiprop(Object, Prop, State).
getprop1(Object, Prop, State) :- 
  current_props(Object, PropList, State),  
  \+ member(inherit(instance), PropList),  
  member(inherit(Delegate), PropList),
  \+ member(inherited(Delegate), PropList),
  getprop1(Delegate, Prop, State).

% Replace or create Prop.
setprop(Object, Prop, S0, S2) :-
  assertion(compound(Prop)),
  functor(Prop,F,A),
  duplicate_term(Prop,Old),
  nb_setarg(A,Old,_),
  undeclare(props(Object, PropList), S0, S1),
  (select(Old, PropList, PropList2) ->
      (upmerge_prop(F,A,Old,Prop,Merged) ->
         (Old==Merged -> S2=S0 ; 
           (append([Merged], PropList2, PropList3),declare(props(Object, PropList3), S1, S2)));
        append([Prop], PropList, PropList3),declare(props(Object, PropList3), S1, S2));
   (append([Prop], PropList, PropList3),declare(props(Object, PropList3), S1, S2))).

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
updateprop(Object, Prop, S0, S2) :-
  assertion(compound(Prop)),
  functor(Prop,F,A),
  duplicate_term(Prop,Old),
  nb_setarg(A,Old,_),
  undeclare(props(Object, PropList), S0, S1),
  (select(Old, PropList, PropList2) ->
      (upmerge_prop(F,A,Old,Prop,Merged) ->
         (Old==Merged -> S2=S0 ; 
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


