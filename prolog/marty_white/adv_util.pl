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

% Miscellaneous generic utility predicates.

:- meta_predicate findall_set(?,0,*).
findall_set(E,G,S):- findall(E,G,L),list_to_set(L,S).

% was_dcg(M,Kept,S0,S2):- !, M:apply_state(Kept,S0,S2).
was_dcg(M,Kept,S0,S2):- call(M:phrase(Kept,S0,S2)).
%:- trace.
term_expansion_was_dcg('-->'(DCG , Keeper), '-->'(DCG , was_dcg(M,Keeper))):- Keeper \= was_dcg(_,_), prolog_load_context(module,M).

:- meta_predicate(sg(1,?,?)).
sg(G,S0,S9) :- call(G,S0),S0=S9.

%mu:term_expansion(I,P,O,PO):- notrace((compound(I),nonvar(P))),term_expansion_was_dcg(I,O),P=PO.
%foo --> bar ,!.
%foo --> bar,baz.
%:- break.

clock_time(T):- statistics(walltime,[X,_]),T is ('//'(X , 100))/10.


:- dynamic(is_state_pred/2).
is_state_pred(F,N):- atom(F),!,is_state_pred(P,N),functor(P,F,_).

defn_state_pred(P,N):- is_state_pred(P,N),!.
defn_state_pred(P,N):- asserta(is_state_pred(P,N)),
  strip_module(P,M,PP),
  assertion(compound(PP)),functor(PP,F,A),            
  ignore(defn_state_pred_wrapper(M,F,A,PP,N)).

defn_state_pred_wrapper(M,F,A,_,0):- 
  assertion(F\==('/')),assertion(F\==('//')),
  functor(PP,F,A),PP=..[F|Args],
  append(Args,[S0,S9],NewArgs),
  PPS09=..[F|NewArgs],
  M:asserta((PPS09:- M:PP, S0 = S9)).

defn_state_pred_wrapper(M,F,A,_,1):- 
  assertion(F\==('/')),assertion(F\==('//')),
  functor(PP,F,A),PP=..[F|Args],
  append(Args,[S0],NewArgs0),
  PPS0 =..[F|NewArgs0],
  append(Args,[S0,S9],NewArgs09),
  PPS09 =..[F|NewArgs09],
  
  M:asserta((PPS09:- M:PPS0, S0 = S9)).
 


defn_state_none(P):- defn_state_pred(P,0).
defn_state_getter(P):- defn_state_pred(P,1).
defn_state_setter(P):- defn_state_pred(P,2).

:- defn_state_none(user:bugout1(term)).
:- defn_state_none(adv_io:bugout3(string,list(term),term)).
:- defn_state_none(adv_io:bugout3(string,term)).
:- defn_state_none(==(term,term)).
:- defn_state_none(\==(term,term)).
:- defn_state_none(=(term,term)).
:- defn_state_none(\=(term,term)).
:- defn_state_none(dif(term,term)).
:- defn_state_none(nop(term)).


mk_complex(R, I, '@'(R, I)).
get_complex('@'(R, I), R, I).

complex(C, R, I):- ground(C), get_complex(C, R0, I0), !, R=R0, I=I0.
complex(C, R, I):- ground((R, I)), mk_complex(R, I, C0), !, C=C0.
complex(C, R, I):- freeze(C, complex(C, R, I)), freeze(R, complex(C, R, I)), freeze(I, complex(C, R, I)).


nonvar_subterm(Var, Data):- var(Var), !, sub_term(Var, Data),nonvar(Var).
nonvar_subterm(Bound, Data):- sub_term(E, Data),nonvar(E),'=@='(E,Bound).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
% :- nop(ensure_loaded('adv_util_subst')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


apply_all([], _Goal, S0, S0) :- !.
apply_all([Arg], Goal, S0, S2) :- !, apply_first_arg_state(Arg, Goal, S0, S2).

apply_all(List, Goal, S0, S2) :- notrace((list_to_set(List,Set), 
 List\==Set)), !,
 apply_all(Set, Goal, S0, S2).

apply_all([Arg|ArgTail], Goal, S0, S2) :-
 runnable_goal(Goal, Runnable),
 apply_first_arg_state(Arg, Runnable, S0, S1),
 !, % Don't allow future failure to redo successful agents.
 apply_all(ArgTail, Goal, S1, S2).


apply_map_state(_Front, [], _Rest, S, S).
apply_map_state(Front, [E|List], Rest, S0, S2) :-
 copy_term(Front+Rest,FrontC+RestC),
 apply_state_rest(Front, E, Rest, S0, S1),
 apply_map_state(FrontC, List, RestC, S1, S2).

as_rest_list(Rest,RestL):- is_list(Rest)->Rest=RestL;Rest=..[_|RestL].

apply_state_rest(Front, E, Rest, S0, S1):- as_rest_list(Rest,RestL),
   append(E,RestL,ERestL),append(ERestL,[S0,S1],APPLYARGS),
   apply(Front,APPLYARGS).
   
 


runnable_goal(Goal, Goal) :- ground(Goal), !.
%runnable_goal(Goal, Goal_Copy):- copy_term(Goal, Goal_Copy).
runnable_goal(Goal, Goal).


:- module_transparent(apply_forall_frames//3).
:- meta_predicate(apply_forall_frames(+,+,2,+,-)).
apply_forall_frames([],_Forall,_Apply,S0,S0).
apply_forall_frames([Frame|Frames],Forall,Apply,S0,S2):-
 Frame=Forall,apply_state(Apply,S0,S1),
 apply_forall_frames(Frames,Forall,Apply,S1,S2).

:- module_transparent(apply_forall//2).
:- meta_predicate(apply_forall(0,2,+,-)).
apply_forall(Forall,Apply,S0,S1):-
 findall(Forall,Forall,Frames),
  apply_forall_frames(Frames,Forall,Apply,S0,S1).

findall(E,Goal,L, S0, S2):- apply_state(findall(E,Goal,L), S0, S2).

%unless(G,Else,S0,S2):- apply_state(unless(G,Else), S0, S2).
dmust(Goal,S0,S2):- apply_state(dmust(Goal), S0, S2).
ignore(Goal,S0,S2):- apply_state(ignore(Goal), S0, S2).

:- meta_predicate with_state(*,0,*,*).
with_state(S,Goal,S0,S2):- S0=S,call(Goal),S0=S2.

is_state_getter(P):- compound(P),functor(P,F,Am1),A is Am1+1, current_predicate(F/A),!.
is_state_getter(P):- \+ atom(P),!,compound(P),functor(P,F,_),!,is_state_getter(F).
is_state_getter(F):- is_state_pred(F,1).

is_state_setter(P):- \+ atom(P),!,compound(P),functor(P,F,_),!,is_state_setter(F).
is_state_setter(F):- is_state_pred(F,2).

is_state_meta(P,N):- \+ atom(P),!,compound(P),functor(P,F,_),!,is_state_meta(F,N).
is_state_meta(rtrace,0).
is_state_meta(findall,1).

is_state_ignorer(P):- \+ atom(P),!,compound(P),functor(P,F,_),!,is_state_ignorer(F).
is_state_ignorer(F):- is_state_pred(F,1).
%is_state_ignorer('{}'(term)).

must_input_state(S0):- quietly(dmust((is_list(S0);must_state(S0)))).
must_output_state(S0):- quietly(dmust((must_state(S0);is_list(S0)))),quietly(check4bugs(S0)).
must_state(S0):- is_list(S0),dmust(set_advstate(S0)),!.
must_state(S0):- pprint(must_state(S0),always),trace, check4bugs(S0).

:- module_transparent(apply_state//3).
:- meta_predicate(apply_state(*,+,-)).
:- meta_predicate(apply_state(*,+,-,+,-)).

apply_state(Goal,S0,S2,DCG0,DCG2):-
  DCG0=S0,
  apply_state(Goal, S0, S2),
  DCG2=S2.


rapply_state(S0,S2,Goal):- apply_state(Goal, S0, S2).

:- module_transparent(apply_state//1).
%:- meta_predicate(apply_state(//,+,-)).

apply_state(NonGoal, S0, S2) :- \+ callable(NonGoal),!,trace, S0=S2.
apply_state(M:Goal, S0, S2) :- !, assertion(atom(M)),
 M:apply_state(Goal, S0, S2).
apply_state({Goal}, S0, S0) :- !, call(Goal).
apply_state(M:{Goal}, S0, S0) :- !, call(M:Goal).
apply_state(Goal,S0,S0):- Goal==[],!.

apply_state(List, S0, S2) :- is_list(List),!,append(S0,List,S2),!.
apply_state(G12, S0, S2) :- G12 = [_|_],!, 
 append(GL,G2,G12), (((is_list(GL),append(S0,GL,S1))-> apply_state(G2, S1, S2))).

%apply_state((unless(Unless,Error),More), S0, S2) :- !, (apply_state(Unless, S0, S1)->apply_state(More, S1, S2);apply_state(Error, S0, S2)).
%apply_state(unless(Unless,Error), S0, S2) :- !, (apply_state(Unless, S0, S2)->true;apply_state(Error, S0, S2)).
apply_state(ignore(Goal), S0, S2) :- !, apply_state(Goal, S0, S2)->true;S0=S2.
apply_state(findall(E,Goal,L), S0, S2) :- !, findall(E,apply_state(Goal, S0, _),L),S0=S2.
apply_state(i_o(S0,S2), S0, S2) :- !.

apply_state(Goal, S0, S2) :- is_state_getter(Goal),call(Goal,S0),!, S0=S2.
apply_state(rtrace(Goal), S0, S2) :- !, rtrace(apply_state(Goal, S0, S2)). 
apply_state(current_state(S0), S0, S2) :- !, S0=S2.
apply_state(dmust((G1,G2)), S0, S2) :- !, apply_state(dmust(G1), S0, S1),apply_state(dmust(G2), S1, S2).
apply_state(must(Goal), S0, S2) :- !, dmust(apply_state(Goal, S0, S2)). 
apply_state(nop(_), S0, S2) :- !, S0=S2.
apply_state(dmust(Goal), S0, S2) :- !, dmust(apply_state(Goal, S0, S2)).
apply_state(Meta, S0, S2) :- is_state_meta(Meta,N), length(Left,N),Meta=..[F|MetaL],
   append(Left,[Goal|MetaR],MetaL),
   append(Left,[apply_state(Goal, S0, S2)|MetaR],MetaC),
   apply(call(F),MetaC).
apply_state(Goal, S0, S2) :- is_state_ignorer(Goal),call(Goal),!, S0=S2.
apply_state((('->'(G1,G2));G3), S0, S2) :- !,
 apply_state(G1, S0, If) -> 
 apply_state(G2, If, S2);
 apply_state(G3, S0, S2). 
apply_state((G1*->G2;G3), S0, S2) :- !,
 apply_state(G1, S0, If) *-> 
 apply_state(G2, If, S2);
 apply_state(G3, S0, S2). 
apply_state((G1,G2), S0, S2) :- !,
 apply_state(G1, S0, S1),
 apply_state(G2, S1, S2).
apply_state((G1;G2), S0, S2) :- !,
 apply_state(G1, S0, S2);
 apply_state(G2, S0, S2).


apply_state(sg(Goal), S0, S2) :- !,
 notrace((compound_name_arguments(Goal, F, GoalL),
 append(GoalL, [S0], NewGoalL),
 must_input_state(S0),
 Call=..[F|NewGoalL])),
 dmust(Call),
 S0 = S2,
 must_output_state(S2).

apply_state(Goal, S0, S2) :- 
 notrace(is_state_setter(Goal)), !,
 notrace((compound_name_arguments(Goal, F, GoalL),
 append(GoalL, [S0, S2], NewGoalL),
 must_input_state(S0),
 Call=..[F|NewGoalL])),
 dmust(Call),
 must_output_state(S2).

apply_state(Goal, S0, S2) :-
 notrace((compound_name_arguments(Goal, F, GoalL),
 append(GoalL, [S0, S2], NewGoalL),
 must_input_state(S0),
 Call=..[F|NewGoalL])), !,
 dmust(Call),
 notrace(must_output_state(S2)).

 %apply_state(Goal, S0, S2):- phrase(Goal,S0,S2).



%:- meta_predicate(apply_first_arg_state(+,3,+,-)).
apply_first_arg_state(Arg, Goal, S0, S2) :-
 notrace((compound_name_arguments(Goal, F, GoalL),
 append(GoalL, [S0, S2], NewGoalL),
 must_input_state(S0),
 Call=..[F, Arg|NewGoalL])),
 dmust(Call),
 notrace(must_output_state(S2)).

%:- meta_predicate(apply_first(+,3,+,-)).
apply_first_arg(Arg, Goal, S0, S2):- 
 apply_first_arg_state(Arg, Goal, S0, S2).

% --------

% TODO: rewrite/debug findterm.

findterm(Term, Term).
findterm(Term, [Head|_]) :- nonvar(Head),
 findterm(Term, Head).
findterm(Term, [_|Tail]) :- nonvar(Tail),
 findterm(Term, Tail).
findterm(Term, T) :-
 compound(T),
 \+ is_list(T),
 T =.. List,
 findterm(Term, List).

subst(Prop,Find,Replace,NewProp):- subst(equivalent,Find,Replace,Prop,NewProp).

% Substitute 'Replace' for 'Find' in T0, yielding T.
% TODO: add ^ handling like with bagof/setof.
% bagof(Template, X^Goal, List) means to never instantiate X
% Current behavior:
% subst(copy_term, macro(Code), expanded(Code, X), macro(foo), expanded(foo, Y))
%  leaving X unbound. Suppose I wanted X left bound?
% subst(equivalent, macro(Code), expanded(Code, X), macro(foo), macro(foo))
%  This won't match Code.
% subst(unify, macro(Code), expanded(Code, X), macro(foo), expanded(foo, X))
%  This only matches all occurrences of the same first Code!
subst(unify, Find1, Replace, Find2, Replace) :- Find1 = Find2,
 % The first unification of Find sticks! Doesn't seem too useful to me.
 % TODO: consider somehow allowing a solution for each match.
 % ground(Find) -> T0=Find, ! ; T0=Find. sort of does it
 !.
subst(equivalent, Find, Replace, T0, Replace) :-
 % Don't unify any variables. Safe and simple.
 T0 == Find,
 !.
subst(copy_term, Find, Replace, FindCopy, ReplaceCopy) :-
 % Unify with new instantiations at each replacement.
 % Allows sensible behavior like:
 % subst(my_macro(Code),
 %   expanded(Code),
 %   (this, my_macro(that), other, my_macro(another)),
 %   (this, expanded(that), other, expanded(another)) )
 % ...but unfortunately will break any free-variable associations.
 % TODO: think about how bagof works; apply here.
 copy_term(Find-Replace, FindCopy-ReplaceCopy),
 !.
subst(BindType, Find, Replace, List, [T|Rest]) :-
 is_list(List),
 List = [T0|Rest0], % fails when List = []
 !,
 subst(BindType, Find, Replace, T0, T),
 subst(BindType, Find, Replace, Rest0, Rest).
subst(BindType, Find, Replace, T0, T) :-
 compound(T0),
 % \+ is_list(T0),
 !,
 T0 =.. [Functor0|Args0],
 subst(BindType, Find, Replace, Functor0, Functor1),
 subst(BindType, Find, Replace, Args0, Args1),
 % If Replacement would cause invalid functor, don't subst.
 ( atom(Functor1) -> T =.. [Functor1|Args1] ; T =.. [Functor0|Args1]).
subst(_BindType, _Find, _Replace, T, T).

% Call subst on T for each Find-Replace pair in the given list.
% Order of substitution may matter to you!
subst_dict(_BindType, [], T, T).
subst_dict(BindType, [Find-Replace|Rest], T0, T) :-
 subst(BindType, Find, Replace, T0, T1),
 subst_dict(BindType, Rest, T1, T).



writel([]).
writel([nl]) :- !, nl. % special case if 'nl' is at end of list.
writel([H|T]) :- write(H), writel(T).
%writeln(L) :- writel(L), nl.

% Is Term uninstantiated in any of its parts?
uninstantiated([]) :- !, fail.
uninstantiated(Term) :- var(Term).
uninstantiated([Head|_]) :- uninstantiated(Head).
uninstantiated([_|List]) :- !, uninstantiated(List).
uninstantiated(Term) :-
 compound(Term),
 Term =.. [Head | Tail],
 (uninstantiated(Head); uninstantiated(Tail)).

% ground(Term) :- \+ uninstantiated(Term)

% A safer "not" forbids uninstantiated arguments.
%:- op(900, fy, not).
%not(P) :- uninstantiated(P), throw(not(uninstantiated_var)).
%not(P) :- call(P), !, fail. % standard prolog not(P) predicate
%not(_).

%nth0(N0,List,Member) :-
% N1 is N0 +1,
% nth(N1,List,Member). % gprolog only has 1-based indexing


/*
SWI

random_adv(Base, Max, Number) :-
 Number is Base + random(Max - Base).

my_random_member(List, Member) :-
 length(List, Count),
 random_adv(0, Count, R), % fails if Count is 0
 nth0(R, List, Member).
*/

%gensym(Base, NewSymbol) :- new_atom(Base, NewSymbol).
%gensym(NewSymbol) :- new_atom(gensym_, NewSymbol).

%subset([Element|Tail], Set) :- member(Element, Set), subset(Tail, Set).
%subset([], _Set).

%union([],Set,Set).
%union([Item|Tail],Set1,Set2) :-
% member(Item,Set1),
% !,
% union(Tail, Set1, Set2).
%%union([Item|Tail],Set1,Set2) :-
%% member(Item,Tail),
%% !,
%% union(Tail, Set1, Set2).
%union([Item|Tail],Set1,[Item|Tail2]) :-
% union(Tail,Set1,Tail2).

%intersection([],_Set,[]).
%intersection([Item|Tail],Set1,[Item|Tail2]) :-
% member(Item, Set1),
% !,
% intersection(Tail,Set1,Tail2).
%intersection([_Item|Tail],Set1,Set2) :-
% intersection(Tail,Set1,Set2).

