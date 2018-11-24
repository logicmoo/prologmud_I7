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
:- nop(ensure_loaded('adv_main_states')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- multifile(extra_decl/2).
:- dynamic(extra_decl/2).
:- dynamic(undo/2).
%undo([u, u, u, u, u, u, u, u]).
:- dynamic(advstate_db/1).
advstate_db([]).


get_advstate_varname(Varname):- nb_current(advstate_var,Varname),Varname\==[],!.
get_advstate_varname(advstate).
get_advstate(State):- get_advstate_varname(Var),nb_current(Var,State).
set_advstate(State):- get_advstate_varname(Var),nb_setval(Var,State).
declared_advstate(Fact):- get_advstate(State),declared(Fact,State).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_state')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -----------------------------------------------------------------------------
% State may be implemented differently in the future (as a binary tree or
% hash table, etc.), but for now is a List. These (backtrackable) predicates
% hide the implementation:
% assert/record/declare/memorize/think/associate/know/retain/affirm/avow/
% insist/maintain/swear/posit/postulate/allege/assure/claim/proclaim
% retract/erase/forget/un-declare/unthink/repress/supress
% retrieve/remember/recall/ask/thought/think-of/reminisc/recognize/review/
% recollect/remind/look-up/research/establish/testify/sustain/attest/certify/
% verify/prove
% simulation: declare/undeclare/declared
% perception:
% memory: memorize/forget/thought

% Like select, but always succeeds, for use in deleting.
select_always(Item, List, ListWithoutItem) :- select(Item, List, ListWithoutItem) -> true; ListWithoutItem=List.
 
% Like select, but with a default value if not found in List..
%select_default(Item, _DefaultItem, List, ListWithoutItem) :-
% select(Item, List, ListWithoutItem).
%select_default(DefaultItem, DefaultItem, ListWithoutItem, ListWithoutItem).

% Manipulate simulation state
%declare(Fact, State):- player_local(Fact, Player), !, declare(wishes(Player, Fact), State).
:- export(declare/3).
:- defn_state_setter(declare(fact)).
declare(Fact, State, NewState) :- notrace((assertion(var(NewState)),is_list(State))),!,notrace(declare_list(Fact,State,NewState)).
declare(Fact, inst(Object), inst(Object)):- !,
   get_advstate(State), 
   (declared(props(Object, PropList),State);PropList=[]),!, 
   declare_list(Fact,PropList,NewPropList),
   select_always(props(Object,_),State,MidState),
   append([props(Object,NewPropList)], MidState, NewState),
   set_advstate(NewState).
declare(Fact, type(Type), type(Type)):- !,
   get_advstate(State), 
   (declared(type_props(Type, PropList),State);PropList=[]),!, 
   declare_list(Fact,PropList,NewPropList),
   select_always(type_props(Type,_),State,MidState),
   append([type_props(Type,NewPropList)], MidState, NewState),
   set_advstate(NewState).
declare(Fact, Pred1Name, Pred1Name):- is_pred1_state(Pred1Name),DBPred=..[Pred1Name,State], (retract(DBPred);State=[]),!, declare_list(Fact, State, NewState),DBPredNewState=..[Pred1Name,NewState], asserta(DBPredNewState).
declare(Fact, VarName, VarName):- atom(VarName),nb_current(VarName,PropList), declare_list(Fact,PropList,NewPropList),b_setval(VarName,NewPropList).
declare(Fact, Object, Object):- callable(Fact),!, Fact=..[F|List], 
  Call=..[F, NewArg|List], 
  current_predicate(_,Call),!, 
  ignore( \+ \+ retract(Call)),
  NewArg=Object,
  asserta(Call).

is_pred1_state(istate).
is_pred1_state(statest).
is_pred1_state(advstate_db).

declare_list(Fact, State, NewState) :- assertion(compound(Fact)),assertion(var(NewState)), Fact==[], !, NewState = State.
declare_list((Fact1,Fact2), State, NewState) :- !,declare_list(Fact1, State, MidState),declare_list(Fact2, MidState, NewState).
declare_list([Fact1|Fact2], State, NewState) :- !,declare_list(Fact1, State, MidState),declare_list(Fact2, MidState, NewState).
declare_list(HasList, State, [NewFront|NewState]) :- 
  functor(HasList,F,A), arg(A,HasList,PropList),is_list(PropList),
  functor(Functor,F,A), \+ \+ type_functor(state,Functor),
  arg(1,HasList,Object), arg(1,Functor,Object),
  select(Functor,State,NewState),!,
  arg(A,Functor,OldPropList),assertion(is_list(OldPropList)),
  append(PropList,OldPropList,NewPropList),
  assertion(A=2), NewFront=..[F,Object,NewPropList]. 
declare_list(Fact, State, NewState) :- append([Fact],State,NewState).





%undeclare(Fact, State):- player_local(Fact, Player), !, undeclare(wishes(Player, Fact), State).
undeclare(Fact, State, NewState):- notrace(undeclare_(Fact, State, NewState)).
undeclare_(Fact, State, NewState) :- copy_term(State, Copy), select(Fact, State, NewState),
 assertion( \+ member(Copy , NewState)).

%undeclare_always(Fact, State):- player_local(Fact, Player), !, undeclare_always(wishes(Player, Fact), State).
undeclare_always(Fact, State, NewState) :- select_always(Fact, State, NewState).

%declared(Fact, State) :- player_local(Fact, Player), !, declared(wishes(Player, Fact), State).

:- export(declared/2).
:- defn_state_getter(declared(fact)).
declared(Fact, State) :-
  quietly(( is_list(State)->declared_list(Fact, State);declared_link(declared,Fact, State))).

declared_list(Fact, State) :- member(Fact, State).
declared_list(Fact, State) :- member(link(VarName), State), declared_link(declared, Fact, VarName).
declared_list(Fact, State) :- member(inst(Object), State), declared_link(declared, Fact, Object).

:- meta_predicate(declared_link(2,?,*)).
declared_link(Pred2, Fact, VarName):- strip_module(Pred2,_,Var), var(Var), !, declared_link(declared, Fact, VarName).
declared_link(Pred2, Fact, VarName):- atom(VarName), nb_current(VarName,PropList), call(Pred2, Fact, PropList).
declared_link(Pred2, Fact, inst(Type)):- declared_advstate(props(Type,PropList)), call(Pred2, Fact, PropList).
declared_link(Pred2, Fact, type(Type)):- declared_advstate(type_props(Type,PropList)), call(Pred2, Fact, PropList).
declared_link(Pred2, Fact, Object):- nonvar(Object), extra_decl(Object, PropList), call(Pred2, Fact, PropList).
declared_link(Pred2, Fact, Object):- get_advstate(State), direct_props(Object,PropList,State), call(Pred2, Fact, PropList).
declared_link(declared, Fact, Object):- callable(Fact), Fact=..[F|List], Call=..[F, Object|List], current_predicate(_,Call),!,call(Call).
declared_link(Pred2, Fact, Object):- var(Object), get_advstate(State),member(Prop, State),arg(1, Prop, Object), arg(2,Prop,PropList),
  call(Pred2, Fact, PropList).
  


% extra_decl(Object, PropList):- get_advstate(State), direct_props(Object,PropList,State).

% Entire state of simulation & agents is held in one list, so it can be easy
% to roll back. The state of the simulation consists of:
% object properties
% object relations
% percept queues for agents
% memories for agents (actually logically distinct from the simulation)
% Note that the simulation does not maintain any history.
% TODO: change state into a term:
% ss(Objects, Relationships, PerceptQueues, AgentMinds)
% TODO:
% store initial state as clauses which are collected up and put into a list,
% like the operators are, to provide proper prolog variable management.

get_objects(Spec, Set, State):- 
 quietly((must_input_state(State), 
  get_objects_(Spec, List, State, im(State)), !, 
  list_to_set(List,Set))).
%get_objects(_Spec, [player1, floyd], _State):-!.

get_objects_(_Spec, [], [], im(_)) :- !.
get_objects_(Spec, OutList, [Store|StateList], im(S0)):- 
 (( stores_props(Store, Object, PropList) -> filter_spec(Spec, PropList))
 -> OutList = [Object|MidList]
 ; OutList = MidList), !,
 get_objects_(Spec, MidList, StateList, im(S0)).

stores_props(perceptq(Agent, PropList), Agent, PropList).
%stores_props(type_props(Agent, PropList), Agent, PropList).
stores_props(memories(Agent, PropList), Agent, PropList).
stores_props(props(Object, PropList), Object, PropList).





% get_all_props(Object, AllProps, S0):- findall(Prop,getprop(Object, Prop, S0),AllProps).
:- defn_state_getter(getprop(thing, nv)).
getprop(Object, Prop, S0):- 
 quietly(( compound(Prop), 
   getprop0(Object, Prop, S0))) 
 *-> true ; 
 quietly(( assertion(\+ atom(Prop)), 
   getprop1(Object, [], Object, Prop, S0)))
 *-> true ;
 fail.

getprop0(Object, Prop, S0):- Prop=..[Name,Value], Element =..[Name, Object, Value], member(Element,S0).

getprop1(Orig, AlreadyUsed, Object, Prop, S0) :- 
 direct_props(Object, PropList, S0),
 ( declared(Prop,PropList)*-> true ; 
 inherited_prop1(Orig, AlreadyUsed, Object, Prop, PropList, S0)).

inherited_prop1(Orig, AlreadyUsed, _Object, Prop, PropList, S0):- 
 member(inherit(Delegate,t), PropList),
 \+ member(inherit(Delegate,t), AlreadyUsed),
 \+ member(inherits(Delegate), PropList),
 \+ member(isnt(Delegate), PropList),
 \+ member(inherited(Delegate), AlreadyUsed),
 \+ member(isnt(Delegate), AlreadyUsed),
 append(AlreadyUsed,PropList,AllPropList),
 getprop1(Orig, AllPropList, Delegate, Prop, S0).

inherited_prop1(_Orig, AlreadyUsed, _Object, Prop, PropList, _S0):- 
 member(link(Delegate), PropList),
 \+ member(link(Delegate), AlreadyUsed),
 nb_current(Delegate,NewProps),
 member(Prop,NewProps).



direct_props(Object, PropList, State):- 
 (var(State)->get_advstate(State); true),
 (declared(props(Object, PropList), State) 
 *-> true 
 ; ( declared(type_props(Object, PropList), State) 
 *-> true 
  ; extra_decl(Object, PropList))).

direct_props_or(Object,PropList, Default, S0) :-
 direct_props(Object, PropList, S0)*->true; PropList=Default.

object_props_or(Object,PropList, Default, S0) :-
 declared(props(Object,PropList),S0)*->true; PropList=Default.

 :- meta_predicate each_prop(3,?,?,?).
each_prop(_, [], S0, S0) :-!.
each_prop(Pred, [Prop|List], S0, S2) :- !,
  each_prop(Pred, Prop, S0, S1),
  each_prop(Pred, List, S1, S2).
each_prop(Pred, Prop, S0, S1):- assertion(compound(Prop)), call(Pred, Prop, S0, S1),!.


% Remove Prop.
:- defn_state_setter(delprop(thing, nv)).
delprop(Object, Prop, S0, S2) :- notrace(dmust_det((correct_props(Object,Prop,PropList), each_prop(delprop_(Object), PropList, S0, S2)))).
delprop_(Object, Prop, S0, S2) :- 
 undeclare(props(Object, PropList), S0, S1),
 select(Prop, PropList, NewPropList),
 declare(props(Object, NewPropList), S1, S2).

% Remove Prop Always.
:- defn_state_setter(delprop_always(thing, nv)).
delprop_always(Object, Prop, S0, S2) :- notrace(dmust_det((correct_props(Object,Prop,PropList), each_prop(delprop_always_(Object), PropList, S0, S2)))).
delprop_always_(Object, Prop, S0, S2) :-  delprop_(Object, Prop, S0, S2), !.
delprop_always_(_Object, _Prop, S0, S0).

% Replace or create Prop.
:- defn_state_setter(setprop(thing, nv)).
setprop(Object, Prop, S0, S2) :- notrace((correct_props(Object,Prop,PropList), each_prop(setprop_(Object), PropList, S0, S2))).

setprop_(Object, Prop, S0, S2) :-  
 direct_props_or(Object, PropList, [], S0),
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

% Update or create Prop.
:- defn_state_setter(updateprop(thing, nv)).
updateprop(Object, Prop, S0, S2) :- notrace((correct_props(Object,Prop,PropList), each_prop(updateprop_(Object), PropList, S0, S2))).

updateprop_(Object, Prop, S0, S2) :- 
 assertion(compound(Prop)),
 direct_props_or(Object, PropList, [], S0),
 (member(Prop,PropList)
 -> S0=S2;
 (undeclare_always(props(Object, _), S0, S1),
 updateprop_1(Object, Prop, PropList, S1, S2))).

updateprop_1(Object, Prop, PropList, S0, S2) :-
 functor(Prop,F,A),
 duplicate_term(Prop,Old),
 nb_setarg(A,Old,_),

 (select(Old, PropList, PropList2) ->
 (upmerge_prop(F,A,Old,Prop,Merged) ->
  ((Old==Merged,fail) -> declare(props(Object, PropList), S0, S2) ; % no update
  (append([Merged], PropList2, PropList3),declare(props(Object, PropList3), S0, S2)));
 append([Prop], PropList, PropList3),declare(props(Object, PropList3), S0, S2));
 (append([Prop], PropList, PropList3),declare(props(Object, PropList3), S0, S2))).

      
/*

setprop(Object, Prop, S0, S2) :-
 %dmust_det((
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

merge_value(F,N,B,A,RO):- text_prop(F), \+ is_list(B),!,merge_value(F,N,[B],A,RO).
merge_value(F,N,B,A,RO):- text_prop(F), \+ is_list(A),!,merge_value(F,N,B,[A],RO).
merge_value(F,_,_,A,R):- single_valued_prop(F),!,A=R.

merge_value(=,2,_,V,R):- !, R = V.

merge_value(_,_,_,t,R):- !, R = t.
merge_value(_,_,_,f,R):- !, R = f.
merge_value(_,_,_,[],R):- !, R = [].
merge_value(_,_,_,A,R):- number(A),!,A=R.

merge_value(_F,1,B,A,R):- B == A, !, R = A.

merge_value(_F,1,B,A,RO):- (is_list(B);is_list(A)),flatten([A,B],R),!,list_to_set(R,RO).

merge_value(_, 1,_,A,R):- number(A),!,A=R.
merge_value(_,1,_,_,_):- !,fail.
merge_value(_F,_,_B,A,R):- R = A.

text_prop(nouns).
text_prop(adjs).
text_prop(desc).
single_valued_prop(name).
single_valued_prop(desc).
single_valued_prop(mass).
single_valued_prop(volume).



