/*
% NomicMUD: A MUD server written in Prolog
%
% Some parts used Inform7, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
% 
% July 10, 1996 - John Eikenberry 
% Copyright (C) 2004 Marty White under the GNU GPL
% 
% Dec 13, 2035 - Douglas Miles
%
%
% Logicmoo Project changes:
%
% Main file.
%
*/
% =========================================
% Goal/Plan translating
% =========================================
:- module(ec_loader,[load_e/1, needs_proccess/2,process_ec/2]).

:- reexport(library('ec_planner/ec_planner_dmiles')).
:- reexport(library('ec_planner/ec_reader')).


:- export(e_reader_teste/0).
e_reader_teste:- with_e_sample_tests(load_e),e_reader_teste2.

e_reader_teste2:- 
     convert_to_axiom(fff,
  ((holds_at(beWaiter3(waiterOf(Restaurant)), Time),
    exists([Agent], holds_at(knowOrder(waiterOf(Restaurant), Agent, Food), Time))) ->
       ( happens(order(waiterOf(Restaurant),
                      cookOf(Restaurant),
                      Food),
                Time))),O),
     pprint_ecp(e,O).

:- export(e_reader_testec/0).
e_reader_testec:- with_e_sample_tests(load_e_pl).

:- export(load_e/1).


load_e(F):- is_filename(F), \+ atom_concat(_,'.e',F), !.
load_e(F):- needs_resolve_local_files(F, L), !, maplist(load_e, L).  
load_e('foundations/EC.e').
load_e('foundations/Root.e').
load_e(F):- Req = [ec,pl],
   \+ nb_current('$output_lang',Req), !,
   locally(b_setval('$output_lang',Req), load_e(F)).
load_e(F):-
  is_filename(F), 
  \+ etmp:ec_option(load(F), _),
  set_ec_option(load(F), loaded),
  must_det_l((echo_format('~N% '), pprint_ec(green, loading(F)),
  e_to_pl(on_load_ele, current_output, F))), !.
load_e(_).

load_e_pl(F):- needs_resolve_local_files(F, L), !, maplist(load_e_pl, L).  
load_e_pl(F):-
  calc_where_to(F, outdir('.'), OutputName),
  open(OutputName, write, Outs),
  format(Outs,'~n~q.~n',[:-(include(library('ec_planner/ec_test_incl')))]), 
  with_output_to(Outs, load_e(F)), !,
  close(Outs),
  %trace,
  %consult(OutputName),
  !.

on_load_ele(translate(Event, Outfile)):- !, mention_s_l, echo_format('~N% translate: ~w  File: ~w ~n',[Event, Outfile]).
on_load_ele(load(S0)):- resolve_local_files(S0,SS), !, maplist(load_e, SS), !.
on_load_ele(include(S0)):- resolve_local_files(S0,SS), !, maplist(load_e, SS), !.
on_load_ele(HB):- 
  echo_format('~N'), pprint_ecp(e, HB),
  must( get_linfo(lsvm(L,F,Vs,M))), 
  must(convert_to_axiom(lsvm(L,F,Vs,M),HB,NEWHB)),
  do_process_ec(assert_ele,M, NEWHB),
  nl.


/*
 
 state constraints:  

      holds_at(Fluent)
          implies/equivalent_to
      holds_at(Fluent)


 effect axioms:  

      happens(Event)
           terminates/initiates/releases
      holds_at(Fluent)
       

 trigger axioms: 
  
      holds_at(Fluent)/happens(Event)
           triggers
      happens(Event)


 precondition axioms:  
  
      happens(Event)
           implies
      holds_at(Fluent)

 

happens(doorLock(Agent,Door),Time) -> 
  holds_at(awake(Agent),Time) &
  holds_at(doorUnlocked(Door),Time) &
  holds_at(nearPortal(Agent,Door),Time)




   
axiom(
 terminates(doorLock(Agent,Door),
            doorUnlocked(Door),Time), [])

becomes 
...

axiom(
 terminates(doorLock(Agent,Door),
            doorUnlocked(Door),Time),
   [ holds_at(awake(Agent),Time) &
     holds_at(doorUnlocked(Door),Time) &
     holds_at(nearPortal(Agent,Door),Time)]).
 
*/
functors_are(F,E):- \+ is_list(E), conjuncts_to_list(E, L), !, functors_are(F, L).
functors_are(\+ F,L):-  nonvar(F), !, forall(member(E,L), \+ functor_is(F, E)).
functors_are((F1,F2),L):- !,  
  partition(functors_are(F1),L,[_|_],RestOf),
  functors_are(F2,RestOf).
%functors_are((F1;F2),L):-  !, nonvar(F1), (functors_are(F1,L);functors_are(F2,L)).
functors_are(F,L):- maplist(functor_is(F),L).


functor_is(F, not(E)):- !, compound(E), functor_is(F, E).
functor_is(\+ F,P):- !, nonvar(F), !,  \+ functor_is(F,P).
functor_is((F1;F2),P):- !, nonvar(F1), (functor_is(F1,P);functor_is(F2,P)).
functor_is(F,P):- compound_name_arity(P,F,_).






:- export(assert_ele/1).
assert_ele(SS):- is_list(SS),!,maplist(assert_ele,SS).
%assert_ele(SS):- syntx_term_check(SS),!.
assert_ele(ec_current_domain_db(P,_TMI_)):- !, assert_ele(P).
assert_ele(ec_axiom(H,B,_TMI_)):- !,  assert_axiom(H,B).

assert_ele((H,B)):-  !,  assert_m_axiom((H,B),[]).
assert_ele((H;B)):-  !,  assert_m_axiom((H;B),[]).
assert_ele(not(H)):-  !,  assert_m_axiom(not(H),[]).
assert_ele(happens(H,T)):-  !,  assert_m_axiom(happens(H,T),[]).
assert_ele(EffectAx):- EffectAx=..[Effect|_],
  member(Effect,[initiates,terminates,releases]),
  assert_m_axiom(EffectAx,[]).


assert_ele(axiom(H,B)):- echo_format('~N'), !,
  pprint_ecp(pl, axiom(H,B)).
assert_ele(SS):- echo_format('~N'), 
  pprint_ecp(ec, SS).



assert_m_axiom(X,Y):- clausify(X,X0), trace, assert_axiom(X0,Y).



assert_axiom(Conds, [happens(A,T)]):-
   conjuncts_to_list(Conds, B ), 
   functors_are(\+ happens, B), !,
   assert_axiom(requires(A,T),B).

assert_axiom(happens(A,T), []):- !,
   assert_axiom(happens(A,T), [is_time(T)]).  

assert_axiom(EffectAx, B):- 
  EffectAx=..[Effect,Event,Fluent,T],
  member(Effect,[initiates,terminates,releases]),
  assert_effect(Effect,Event,Fluent,T,B).
/*
assert_axiom((A1,A2), B):- 
  assert_axiom(A1, [possible(A2)|B]),
  assert_axiom(A2, [possible(A1)|B]).
assert_axiom(A1;A2, B):- 
  assert_axiom(A1, [not(A2)|B]),
  assert_axiom(A2, [not(A1)|B]).
*/
assert_axiom(H,B):- 
  assert_ele(axiom(H,B)).

% Normals
assert_effect(Effect,(A1,A2),Fluent,T,B):- !,
   assert_effect(Effect,A1,Fluent,T,[possible(A2)|B]),
   assert_effect(Effect,A2,Fluent,T,[possible(A1)|B]).
assert_effect(Effect,(A1;A2),Fluent,T,B):- !,
   assert_effect(Effect,A1,Fluent,T,[possible(not(A2))|B]),
   assert_effect(Effect,A2,Fluent,T,[possible(not(A2))|B]).
assert_effect(Effect,Event,Fluent,T,B):-
  EffectAx=..[Effect,Event,Fluent,T],
  assert_ele(axiom(EffectAx,B)).



:- export(ect/0).
ect:- load_e('examples/FrankEtAl2003/Story1.e').

/* In addition:

   all(X,P)     denotes "for all X, P holds"
   exists(X,P)  denotes "there is an X such that P holds"

   For both of these X must be a PROLOG VARIABLE
   (note that this diverges from what Clocksin & Mellishs program requires)
*/

/* The main procedure */

clausify(X,Y) :-
   implout(X,X1),
   negin(X1,X2),
   skolem(X2,X3,[]),
   univout(X3,X4),
   conjn(X4,X5),
   clauses(X5,Y,0,_),
   %toclauses(X5,_Y,[]),
   !.

/* Removing Implications */

implout((P <-> Q),((P11 , Q11) ; (not(P1) , not(Q1)))) :- !,
   implout(P,P11), rename_vars(P11,P1,[]), implout(Q,Q11),
   rename_vars(Q11,Q1,[]).
implout((P -> Q),(not(P1) , Q1)) :- !,
   implout(P,P1), implout(Q,Q1).
implout(all(X,P),all(X,P1)) :- !,
   implout(P,P1).
implout(exists(X,P),exists(X,P1)) :- !,
   implout(P,P1).
implout((P , Q),(P1 , Q1)) :- !,
   implout(P,P1), implout(Q,Q1).
implout((P ; Q),(P1 ; Q1)) :- !,
   implout(P,P1), implout(Q,Q1).
implout(not(P),not(P1)) :- !,
   implout(P,P1).
implout(P,P).

/* Moving negation inwards */

negin(not(P),P1) :- !, neg(P,P1).
negin(all(X,P),all(X,P1)) :- !, negin(P,P1).
negin(exists(X,P), exists(X,P1)) :- !, negin(P,P1).
negin((P , Q),(P1 , Q1)) :- !, negin(P,P1), negin(Q,Q1).
negin((P ; Q),(P1 ; Q1)) :- !, negin(P,P1), negin(Q,Q1).
negin(P,P).

neg(not(P),P1) :- !, negin(P,P1).
neg(all(X,P),exists(X,P1)) :- !, neg(P,P1).
neg(exists(X,P),all(X,P1)) :- !, neg(P,P1).
neg((P , Q),(P1 ; Q1)) :- !, neg(P,P1), neg(Q,Q1).
neg((P ; Q),(P1 , Q1)) :- !, neg(P,P1), neg(Q,Q1).
neg(P,not(P)).

/* Skolemising */

skolem(all(X,P),all(X,P1),Vars) :- !,
   var(X),
   skolem(P,P1,[X|Vars]).
skolem(exists(X,P),P1,Vars) :- !,
   var(X),
   gensym(f,F),
   X =.. [F|Vars],
   skolem(P,P1,Vars).
skolem((P , Q),(P1 , Q1),Vars) :- !,
   skolem(P,P1,Vars), skolem(Q,Q1,Vars).
skolem((P ; Q),(P1 ; Q1),Vars) :- !,
   skolem(P,P1,Vars), skolem(Q,Q1,Vars).
skolem(P,P,_).

/* Mo;ing uni;ersal quantifiers outwards */

univout(all(_X,P),P1) :- !, univout(P,P1).
univout((P , Q),(P1 , Q1)) :- !,
   univout(P,P1), univout(Q,Q1).
univout((P ; Q),(P1 ; Q1)) :- !,
   univout(P,P1), univout(Q,Q1).
univout(P,P).

/* Distributing and over or */

conjn((P ; Q),R) :- !,
   conjn(P,P1), conjn(Q,Q1),
   conjn1((P1 ; Q1),R).
conjn((P , Q),(P1 , Q1)) :- !,
   conjn(P,P1), conjn(Q,Q1).
conjn(P,P).

conjn1(((P , Q) ; R),(P1 , Q1)) :- !,
   conjn((P ; R),P1), conjn((Q ; R),Q1).
conjn1((P ; (Q , R)),(P1 , Q1)) :- !,
/*   conjn((P ; Q),P1), conjn((P1 ; R),Q1). */
   conjn((P ; Q),P1), conjn((P ; R),Q1).
conjn1(P,P).

/* Putting into clauses */

toclauses((P , Q),C1,C2) :- !,
   toclauses(P,C1,C3), toclauses(Q,C3,C2).
toclauses(P,[fact(C)|Cs],Cs) :- inclause(P,C,[]), !.
toclauses(_P,C,C).

inclause((P ; Q),A,A1) :- !,
   inclause(P,A2,A1), inclause(Q,A,A2).
inclause(not(P),A1,A) :- !,
   notin(P,A), putin(not(P),A,A1).
inclause(P,A1,A) :- !,
   notin(not(P),A), putin(P,A,A1).


list_union([X|L1],L2,L3) :-
	identical_member(X,L2),
	!,
	list_union(L1,L2,L3).
list_union([X|L1],L2,[X|L3]) :-
	list_union(L1,L2,L3).
list_union([],L,L).


clauses((A , B),L,WffNum1,WffNum2) :-
	!,
	clauses(A,L1,WffNum1,W),
	clauses(B,L2,W,WffNum2),
	conjoin(L1,L2,L).

clauses(NNF,L,WffNum1,WffNum2):- 
   %save_wid(WffNum1,pttp_in,PNF),
   %once(pttp_nnf(PNF,OUT)),
   %save_wid(WffNum1,pttp_nnf,OUT),
   clauses1(NNF,L,WffNum1,WffNum2).


clauses1(A,L,WffNum1,WffNum2) :-
	write_clause_with_number(A,WffNum1),
	head_literals(A,Lits),
	clauses2(A,Lits,L,WffNum1),
	WffNum2 is  WffNum1 + 1.

clauses2(A,[Lit|Lits],L,WffNum) :-
	body_for_head_literal(Lit,A,Body1),
	(Body1 == false ->
		L = true;
	%true ->
		conjoin(infer_by(WffNum),Body1,Body),
		clauses2(A,Lits,L1,WffNum),
		conjoin((Lit :- Body),L1,L)).
clauses2(_,[],true,_).

head_literals(Wff,L) :-
	Wff = (A :- _B) ->	% contrapositives not made for A :- ... inputs
		head_literals(A,L);
	Wff = (A , B) ->
		(head_literals(A,L1),
		 head_literals(B,L2),
		 list_union(L1,L2,L));
	Wff = (A ; B) ->
		(head_literals(A,L1),
		 head_literals(B,L2),
		 list_union(L1,L2,L));
	%true ->
		L = [Wff].

body_for_head_literal(Head,Wff,Body) :-
	Wff = (A :- B) ->
		(body_for_head_literal(Head,A,A1),
		 conjoin(A1,B,Body));
	Wff = (A , B) ->
		(body_for_head_literal(Head,A,A1),
		 body_for_head_literal(Head,B,B1),
		 pttp_disjoin(A1,B1,Body));
	Wff = (A ; B) ->
		(body_for_head_literal(Head,A,A1),
		 body_for_head_literal(Head,B,B1),
		 conjoin(A1,B1,Body));
	Wff == Head ->
		Body = true;
	(once(negated_literal(Wff,Was)),Head=@=Was) ->
		Body = false;
	%true ->
		negated_literal(Wff,Body).

negated_literal(Lit,not(Lit)).

pttp_disjoin(A,B,C) :-
	A == true ->
		C = true;
	B == true ->
		C = true;
	A == false ->
		C = B;
	B == false ->
		C = A;
	%true ->
		C = (A ; B).

write_clause_with_number(A,WffNum) :-
	nl,
	write_indent_for_number(WffNum),
	write(WffNum),
	write('  '),
        copy_term(A,AA),
        numbervars(AA,0,_,[attvar(bind),singletons(true)]),
	write(AA),
	write(.).

write_indent_for_number(N) :-
	((number(N) , N <  100) -> write(' ') ; true),
	((number(N) , N <   10) -> write(' ') ; true).


identical_member(E,L):- member(EE,L),EE==E.

notin(X,[Y|_]) :- X == Y, !, fail.
notin(X,[_|L]) :- !, notin(X,L).
notin(_,[]).

putin(X,[],[X]) :- !.
putin(X,[Y|L],L) :- X == Y, !.
putin(X,[Y|L],[Y|L1]) :- putin(X,L,L1).

%;;; Renaming variables, when a quantified proposition is split into
%;;; two. This avoids nasty problems where the same variable is both
%;;; existentially and uni;ersally quantified over.

rename_vars(X,Y,Assocs) :- var(X), !, assoc(X,Y,Assocs).
rename_vars(X,X,_) :- atomic(X), !.
rename_vars(all(X,Y),all(X1,Y1),Ass) :- !,
   rename_vars(Y,Y1,[[X|X1]|Ass]).
rename_vars(exists(X,Y),exists(X1,Y1),Ass) :- !,
   rename_vars(Y,Y1,[[X|X1]|Ass]).
rename_vars(X,Y,Ass) :-
   functor(X,F,N), functor(Y,F,N),
   rename_vars_args(N,X,Y,Ass).

rename_vars_args(0,_,_,_) :- !.
rename_vars_args(N,X,Y,Ass) :-
   arg(N,X,X1), arg(N,Y,Y1),
   rename_vars(X1,Y1,Ass),
   N1 is N - 1, rename_vars_args(N1,X,Y,Ass).

assoc(X,Y,[]) :- !, X=Y.
assoc(X,Y,[[X1|Y1]|_]) :- X == X1, !, Y=Y1.
assoc(X,Y,[_|L]) :- assoc(X,Y,L).




fix_goal_add_on_arg(T, G, G0, [b(T,T2),b(T2,end)]):- G =.. [F,A], already_good(F,2), G0 =.. [F,A,T].%, next_t(T,T2).
fix_goal_add_on_arg(T, G, G0, [b(T,T2),b(T2,end)]):- G =.. [F,A,B], already_good(F,3), G0 =.. [F,A,B,T]. %, next_t(T,T2).


:- export(fix_goal/3).


to_axiom_head(T,G,GG) :-  notrace(fix_axiom_head(T,G,GG)),!.
to_axiom_head(T,G,GG) :- fix_axiom_head(T,G,GG),!.


fix_goal(_, Nil,[]):- Nil==[],!.
fix_goal(T,[G|Gs],GGs):- !, fix_goal(T,G,G0),fix_goal(T,Gs,Gs0),append(G0,Gs0,GGs),!.
fix_goal(T,(G,Gs),GGs):- !, fix_goal(T,G,G0),fix_goal(T,Gs,Gs0),append(G0,Gs0,GGs),!.
fix_goal(T,{Gs},GGs):- !, fix_goal(T,Gs,GGs).
fix_goal(T, G, GGs):- fix_axiom_head(T,G,GG),!, listify(GG,GGs).
fix_goal(T, G, [Gs| TExtra]):- fix_goal_add_on_arg( T, G, Gs, TExtra),!.
fix_goal(T, G, [GGs]):- to_axiom_head(T,G,GGs),!.
fix_goal(T, G, [holds_at(G, T)]).


ec_to_ax(_, X,Y):-  (\+ callable(X) ; \+ compound(X)), !, X=Y.
ec_to_ax(T, (Pre -> '<->'(HB,BH)), HBO):- ec_to_ax(T, ('<->'((Pre,HB),(Pre,BH))), HBO).
ec_to_ax(T, (Pre ; '->'(HB,BH)), HBO):- ec_to_ax(T, '->'((Pre ; HB),BH), HBO).
ec_to_ax(T, (H<-B),O):- !, into_axiom(T,H,B,O).
ec_to_ax(T, (B->H),O):- !, into_axiom(T,H,B,O).
ec_to_ax(T, (HB1<->HB2),[A,B]):- !, ec_to_ax(T, (HB1->HB2),A),ec_to_ax(T, (HB1<-HB2),B).
ec_to_ax(T, axiom(H,B),O):- into_axiom(T,H,B,O), !.
ec_to_ax(_, axiom(H,B), axiom(H,B)):- !.
ec_to_ax(T, X,Y):- cvt1(T, X,Y),X=Y,!.
ec_to_ax(T, X,Y):- cvt1(T, X,XY),ec_to_ax(T, XY,Y).
ec_to_ax(_, X,X).

to_axiom_body(T,G,GGs) :-  fix_goal(T,G,GGs).

%into_axiom(T,H,happens(E,T),OUT):- into_axiom(T,not(happens(E,T)),not(H),OUT).
into_axiom(T,H,B,axiom(AH,AB)):- to_axiom_head(T1,H,AH),to_axiom_body(T2,B,AB),!,ignore(T=T1),ignore(T2=T1).

:- export(fix_axiom_head/3).

cvt1(_, X,Y):-  (\+ callable(X);\+ compound(X)), !, X=Y.
cvt1(T, (Pre -> '<->'(HB,BH)), HBO):- cvt1(T, ('<->'((Pre,HB),(Pre,BH))), HBO).
cvt1(T,'&'(A,B),(AABB)):- !, fix_axiom_head(T,(A,B),(AABB)).
cvt1(T,(A,B),(AA,BB)):- !, fix_axiom_head(T,A,AA), fix_axiom_head(T,B,BB).
cvt1(T,(A;B),(AA;BB)):- !, fix_axiom_head(T,A,AA), fix_axiom_head(T,B,BB).
cvt1(_, X\=Y, diff(X,Y)).
cvt1(T, X=Y, Term):- as_equals(X,Y,Equals), !, fix_axiom_head(T, Equals, Term),!.

cvt1(T, HT, HTTermO):-  
 compound_name_arguments(HT, F, L),
 upcase_atom(F,U),downcase_atom(F,U),
 maplist(fix_axiom_head(T),L,LL), LL\==L, !,
 compound_name_arguments(HTTerm, F, LL),
 fix_axiom_head(T, HTTerm, HTTermO).

cvt1(_T, ec_option(X,Y),option(X,Y)):-!.
cvt1(_T, option(X,Y),option(X,Y)):-!.
cvt1(T, neg(exists(_Vars, holds_at(P, Time))),O):- cvt1(T, holds_at(neg(P), Time), O).


cvt1(T, isa(E, C), List):- !, fix_axiom_head(T, t(C, E), List).
cvt1(T, not(holds_at(N,AT)),O):- AT==0, fix_axiom_head(T, initially(neg(N)),O).
cvt1(T, holds_at(N,AT),O):- AT==0, fix_axiom_head(T, initially(N),O).
cvt1(T, not(holds_at(N,AT)),O):- !, fix_axiom_head(T, holds_at(neg(N),AT),O).
cvt1(T, neg(holds_at(N)),O):- !, fix_axiom_head(T, holds_at(neg(N),T),O).
cvt1(T, not(happens(G,T2)),not(O)):- !, fix_axiom_head(T,happens(G,T2),O).
cvt1(T, not(I),O):- !, fix_axiom_head(T, neg(I),O).
cvt1(T, not(exists(_Vars,I)),O):- !, fix_axiom_head(T, not(I),O).
cvt1(_, exists(Vars,Truth), NewTruth):- conjoin(Truth,some(Vars),NewTruth).
cvt1(T, happens(F, T1, T2), O):- T1==T2,!, fix_axiom_head(T, happens(F, T1), O).
cvt1(_, equals(X,Y),equals(X,Y)).

cvt1(_, G, G):- functor_skel(G,P), syntx_term_check(predicate(P)).
cvt1(T, G, happens(G,T)):- functor_skel(G,P), (syntx_term_check(event(P));executable(P)).
cvt1(T, G, holds_at(G,T)):- functor_skel(G,P), syntx_term_check(fluent(P)).
cvt1(_, G, G):- functor(G,F,A), already_good(F,A),!.


fix_axiom_head(T,X,Y):- ec_to_ax(T, X,Y).

fix_axiom_head_2(T, G, Gs):- fix_goal_add_on_arg( T, G, Gs, _TExtra),!.
fix_axiom_head_2(T, P,PP):- P =..[F|Args],functor(P,F,A), arg_info(AxH,F,Arity),!,
   functor(Arity,_,N), 
   must(correct_ax_args(T,F,A,Args,AxH,Arity,N,PP)).
%fix_axiom_head(T, G, P):- ec_to_ax(T, G,P),!.

as_equals(X,Y,equals(X,Y)).
as_equals(X,Y,Equals):- compound(X),append_term(X,Y, Equals).
as_equals(X,Y,equals(X,Y)).


syntx_term_check(G):- clause(G,_).
syntx_term_check(G):- clause(ec_current_domain_db(G, _),_).

functor_skel(G,P):- compound(G), compound_name_arity(G,F,A), compound_name_arity(P,F,A).
functor_skel(G,P):- atom(G),P=G.

between_r(H,L,N):- nonvar(N),!,between(L,H,N).
between_r(H,L,N):- Hm1 is H - L, !, between(L,H,NN), N is NN + Hm1.

semi_legit_time(happens(_,T1),T1).
semi_legit_time(happens(_,_,T2),T2).
semi_legit_time(happens(_,T1,_),T1).
semi_legit_time(not(Holds),T):- !, semi_legit_time(Holds,T).
semi_legit_time(Holds1,T1):- 
   functor(Holds1,F,_),
   time_arg(F,N),
   arg(N,Holds1,T1).
semi_legit_time(Holds1,T1):- 
   functor(Holds1,_,A), 
   member(P1,[number,string,atom]),
   (arg(A,Holds1,T1);arg(_,Holds1,T1)), 
   T1\==[], call(P1,T1).

:- export(sort_on_times_arg/3).
sort_on_times_arg(Result,Holds1,Holds2):- 
   (((semi_legit_time(Holds1,T1),semi_legit_time(Holds2,T2),
      compare(Result,T1,T2), Result\== (=))) 
     -> true;
        sort_on_times_arg(Result,Holds1,Holds2)).

time_arg(b, N):- between(1,2,N).
time_arg(beq, N):- between(1,2,N).
time_arg(holds_at, 2).
time_arg(happens, N):- between_r(3,2,N), N\=1.
time_arg(clipped, N):- between_r(3,1,N), N\=2.
time_arg(declipped, N):- between_r(3,1,N), N\=2.

:- export(fix_time_args/3).
fix_time_args(T,[G|Gs],Gss):- 
  semi_legit_time(G,ST),
  fix_time_args1(ST,[G|Gs],Gs0),
  fix_time_args2(T,Gs0,Gss).

fix_time_args2(_,Gs,Gss):-
  Gss = [b(start,now),b(now,aft),b(aft,end)|Gs].

visit_time_args(_,In,[],[],In).
visit_time_args(Stem,In,[G|Gs],[GO|GsO],Out):- !, 
    visit_time_args(Stem,In,G,GO,Mid),
    visit_time_args(Stem,Mid,Gs,GsO,Out).
visit_time_args(Stem,In,holds_at(A,T1),holds_at(A,T1R),Out):- 
   correct_time_arg(Stem,In,T1,T1R,Out).
visit_time_args(Stem,In,happens(A,T1,T2),happens(A,T1R,T2R),Out):- 
   correct_time_arg(Stem,In,T1,T1R,B0),
   correct_time_arg(Stem,B0,T2,T2R,Out).
visit_time_args(Stem,In,happens(A,T1),happens(A,T1R),Out):- 
   correct_time_arg(Stem,In,T1,T1R,Out).
visit_time_args(Stem,In,b(T1,T2),b(T1R,T2R),Out):- 
   correct_time_arg(Stem,In,T1,T1R,B0),
   correct_time_arg(Stem,B0,T2,T2R,Out).
visit_time_args(Stem,In,not(G),not(GG),Out):- !, visit_time_args(Stem,In,G,GG,Out).
visit_time_args(Stem,In,beq(T1,T2),beq(T1R,T2R),Out):- 
   correct_time_arg(Stem,In,T1,T1R,B0),
   correct_time_arg(Stem,B0,T2,T2R,Out).
visit_time_args(Stem,In,clipped(T1,A,T2),clipped(T1R,A,T2R),Out):- 
   correct_time_arg(Stem,In,T1,T1R,B0),
   correct_time_arg(Stem,B0,T2,T2R,Out).
visit_time_args(Stem,In,declipped(T1,A,T2),declipped(T1R,A,T2R),Out):- 
   correct_time_arg(Stem,In,T1,T1R,B0),
   correct_time_arg(Stem,B0,T2,T2R,Out).
visit_time_args(_,In,G,G,In).

correct_time_arg(_Stem,In, TN, TN, In):- var(TN), !.
correct_time_arg(_Stem,In, TN, TN, In):- atom(TN), !.
correct_time_arg(_Stem,In, v, _, In):- !.
correct_time_arg(_Stem,In, TN, TpN, In):- lookup_time_val(TN,TpN,In),!.
correct_time_arg(Stem,In, TN, TpN, [ignore(TN=TpN)|Out]):-  number(TN), !, correct_time_arg(Stem,In, Stem+TN, TpN, Out).
correct_time_arg(Stem,In, T-N, TpN, Out):- number(N), N<0, NN is abs(N),!,correct_time_arg(Stem,In, T+NN, TpN, Out).
correct_time_arg(Stem,In, T+N, TpN, Out):- number(N), N<0, NN is abs(N),!,correct_time_arg(Stem,In, T-NN, TpN, Out).
correct_time_arg(Stem,In, Now+N, T, [ignore(Now+N=T)|Out]):- number(N), N>1, NN is N-1, correct_time_arg(_Stem,In, Now+1, Tm2, Mid),
  correct_time_arg(Stem, Mid, Tm2+NN, T, Out).
correct_time_arg(Stem,In, Now-N, T, [ignore(Now-N=T)|Out]):- number(N), N<1, NN is N+1, correct_time_arg(_Stem,In, Now-1, Tm2, Mid),
  correct_time_arg(Stem, Mid, Tm2-NN, T, Out).
correct_time_arg(_Stem,In, T-1, TN, Out):- !, t_plus_or_minus_1(In, T-1, TN, Out).
correct_time_arg(_Stem,In, T+1, TN, Out):- !, t_plus_or_minus_1(In, T+1, TN, Out).
correct_time_arg(_Stem,In, T+0, T, In):-!.
correct_time_arg(_Stem,In, T-0, T, In):-!.
correct_time_arg(_Stem,In, TN, TN, In).

lookup_time_val(TN,TpN,In):- copy_term(TN,TNS),member(ignore(TNS=TpN),In),TNS=@=TN,!.

t_plus_or_minus_1(In, TN, TpN, In):- lookup_time_val(TN,TpN,In).
t_plus_or_minus_1(In, T+1, TN, In):- memberchk(b(T,TN),In),!.
t_plus_or_minus_1(In, T-1, TN, In):- memberchk(b(TN,T),In),!.
t_plus_or_minus_1(In, T-1, TN, [b(TN,T),ignore(T-1=TN)|In]):- next_t(TN,T),!.
t_plus_or_minus_1(In, T+1, TN, [b(T,TN),ignore(T+1=TN)|In]):- next_t(T,TN),!.
t_plus_or_minus_1(In, T+N, TN, [b(T,TN),ignore(T+N=TN)|In]):- atom_concat(T,N,TN).
t_plus_or_minus_1(In, T-N, TN, [b(TN,T),ignore(T-N=TN)|In]):- atomic_list_concat([T,N],minus,TN).


next_t(t,start).
next_t(start,now).
next_t(now,aft).
next_t(aft,Aft_1):- var(Aft_1),!,gensym(aft_,Aft_1).


fix_time_args1(T,G,Gs):- 
  visit_time_args(T,[],G,Gs,_Mid).



already_good(happens, 2).
already_good(happens, 3).
already_good(holds_at, 2).
already_good(b, 2).
already_good(is, 2).
already_good(diff, 2).
already_good(dif, 2).
already_good(terms_or_rels,3).
already_good(F,A):- functor(P,F,A),syntx_term_check(abducible(PP)),compound(PP),PP=P.
already_good(F,A):- functor(P,F,A),syntx_term_check(predicate(PP)),compound(PP),PP=P.



%predicate, option range load fluent event noninertial xor completion


%special_directive('!').
%special_directive('neg').

% builtin_pred(releasedAt).

%xfr_body(true,[]).








:- export(get_linfo/1).
get_linfo(lsvm(L,F,Vs,M)):- 
  quietly((must(s_l(F,L)),!,
  '$current_source_module'(M),
  nb_current('$variable_names',Vs))).


:- export(process_ec/1).
:- module_transparent(process_ec/1).
process_ec( HB ):- notrace(must(get_linfo(T))), process_ec( T, HB ).
%:- export(process_ec/2).
%process_ec( _, HB , T):- !,process_ec( T, HB ).

:- export(process_ec/2).
:- module_transparent(process_ec/2).
process_ec( lsvm(L,S,Vs,M), HB ):-  
  must(convert_to_axiom(lsvm(L,S,Vs,M),HB,NEWHB)),
  do_process_ec(assertz,M, NEWHB).

merge_into_body(X,_Y,Z):- Z = X.

:- export(do_process_ec/3).
:- module_transparent(do_process_ec/3).
do_process_ec(_Why, M, NonCallable) :- assertion((current_module(M),callable(NonCallable))), fail.
do_process_ec(Why, M, NEWHB):- is_list(NEWHB), !, maplist(do_process_ec(Why, M), NEWHB).
do_process_ec(_Why, M, (:- GOAL)):- !, must(M:GOAL).
do_process_ec(_Why, M, (?- GOAL)):- !, M:forall(GOAL, true).
% How to? M:assertz('$source_location'(S, L):NEWHB),
%do_process_ec(Why, M, NEWHB):- wdmsg(do_process_ec(Why, M, NEWHB)),fail.
do_process_ec(Why, M, NEWHB):- M:call(Why, NEWHB).

:- export(convert_to_axiom/3).
convert_to_axiom(_, EOF, EOF) :- EOF == end_of_file,!.
convert_to_axiom(_, I, O):- \+ callable(I),!, I = O.
convert_to_axiom(_, subsort(F, W), List):- !, to_fact_head([subsort(F, W),sort(F),sort(W)],List).
convert_to_axiom(_, option(X,Y), [(:- set_ec_option(X,Y))]).
convert_to_axiom(T, M:H, [M:HH]):- !, convert_to_axiom(T, H, HH).
convert_to_axiom(T, (H:-B),[(HH:- B)]):- !, convert_to_axiom(T, H,HH).
convert_to_axiom(_, abducible(H), abducible(H)):- !.
convert_to_axiom(_, t(C, E), List):- !, to_fact_head([sort(C),t(C, E)],List).

convert_to_axiom(L,[H|T],ABC):- trace, 
   once((convert_to_axiom(L,H,A), convert_to_axiom(L,T,B),append(A,B,AB))), 
   AB\=@= [H|T],
   convert_to_axiom(L,AB,ABC).

convert_to_axiom(T, (Pre -> '<->'(HB,BH)), HBO):-
  convert_to_axiom(T, ('<->'((Pre,HB),(Pre,BH))), HBO),!.

convert_to_axiom(T, '<->'(HB,BH), HBOO):-
  convert_to_axiom(T, '->'(HB,BH), HBO1),
   convert_to_axiom(T, '<-'(HB,BH), HBO2),
  flatten([HBO1,HBO2],HBO),
  convert_to_axiom1(T,HBO,HBOO),!.

convert_to_axiom(T, exists(Vars,B->H), HBO):- conjoin(H,some(Vars),Conj), !, convert_to_axiom(T, B -> Conj , HBO).
convert_to_axiom(T, exists(Vars,Info), HBO):- !, convert_to_axiom(T, Info, HB), 
  merge_into_body(HB,some(Vars),HBO).

%  compound_name_arity(P,F,A),compound_name_arity(PP,F,A).


convert_to_axiom(T, X, O):- nop(debug_var('AxTime',Time)), ec_to_ax(Time, X,Y), 
  (is_list(Y)->convert_to_axiom1(T, Y, O); (X\=Y -> convert_to_axiom(T, Y, O);convert_to_axiom1(T, Y, O))), !.
convert_to_axiom(T, Y, O):- convert_to_axiom1(T, Y, O).

% convert_to_axiom1(T, '->'(E,B), HBO2):- convert_to_axiom(T, precond(B,E), HBO2).
to_fact_head(H,List):- H=List.


convert_to_axiom1(_, EOF, []) :- EOF = end_of_file,!.
convert_to_axiom1(T, P, O):- is_axiom_head(P),!, convert_to_axiom1(T, axiom(P), O).
convert_to_axiom1(T, axiom(P), O):- convert_to_axiom1(T, axiom(P ,[]), O).
convert_to_axiom1(LSV, axiom(X,Y), [ec_axiom(X,Y,LSV)]).
convert_to_axiom1(LSV, Pred, [ec_current_domain_db(Pred,LSV)]).


is_axiom_head(P):- compound_name_arity(P,F,_), arg_info(axiom_head,F,_),!.
is_axiom_head(P):- functor_skel(P, G), syntx_term_check(predicate(G)),!.


arg_info(domain,fluent,arginfo).
arg_info(domain,noninertial,arginfo).
arg_info(domain,predicate,arginfo).
arg_info(domain,function,arginfo).
arg_info(domain,event,arginfo).
arg_info(domain,sort,arginfo).

arg_info(domain,reified_sort,arginfo).
arg_info(abducible,subsort,v(sort,sort)).
arg_info(abducible,t,v(sort,term)).
%arg_info(domain,axiom,v(axiom,list)).

arg_info(axiom_head,happens,v(event,time)).
arg_info(axiom_head,holds_at,v(fluent,time)).
arg_info(axiom_head,initially,v(fluent)).
arg_info(axiom_head,initiates,v(event,fluent,time)).
arg_info(axiom_head,terminates,v(event,fluent,time)).
arg_info(axiom_head,releases,v(event,fluent,time)).
arg_info(axiom_head,trajectory,v(fluent,time,fluent,offset)).
%arg_info(axiom_head,releasesAt,v(fluent,time)).

correct_ax_args(T,F,A,Args,axiom_head,_Arity,N, PP):-  N is A +1 ,!, append(Args,[T],NewArgs), PP =.. [F|NewArgs].
correct_ax_args(_T,F,A,Args,axiom_head,_Arity,N,PP):- A=N, PP =.. [F|Args].
correct_ax_args(_T,F,1,Args,domain,arginfo,0,PP):- PP =.. [F|Args].
correct_ax_args(_T,F,2,[P,R],domain,arginfo,0,PP):- append_term(P,R,AB),PP =.. [F,AB].
%orrect_ax_args(_4456,function,2,[side1(portal),location],domain,arginfo,0,_4470))

%correct_ax_args(_T,initiates,2,[go(_1694),at(_1694)],axiom_head,3,v(event,fluent,time),_1684)
%failed_must(correct_ax_args(_T,initiates,2,[go(_1694),at(_1694)],axiom_head,3,v(event,fluent,time),_1684))








needs_process_axiom(C):- \+ compound(C), !, fail.
needs_process_axiom(axiom(_,_)).
needs_process_axiom(axiom(_)).
needs_process_axiom(abducible(_)).
needs_process_axiom(executable(_)).
needs_process_axiom(P):- compound_name_arity(P,F,A),needs_process_axiom_fa(F,A).

needs_process_axiom_fa(F,_):- arg_info(_,F,_).
needs_process_axiom_fa('<->',2).
needs_process_axiom_fa('<-',2).
needs_process_axiom_fa('->',2).


:- export(needs_proccess/2).
needs_proccess(PA,_):- \+ compound(PA),!,fail.
needs_proccess(PA, process_ec):- needs_process_axiom(PA),!.
needs_proccess((H :- _B),How):- nonvar(H),!,needs_proccess(H,How).
needs_proccess( _M:H, How):- nonvar(H),!,needs_proccess(H,How).



:- module_transparent(hook_ec_axioms/2).
:- export(hook_ec_axioms/2).
:- prolog:import(hook_ec_axioms/2).
hook_ec_axioms(What, File):- var(File), !, current_input(Input), hook_ec_axioms(What, Input).
hook_ec_axioms(What, file(_File,AbsFile)):- !, hook_ec_axioms(What, file(AbsFile)).
hook_ec_axioms(What, file(AbsFile)):- !, hook_ec_axioms(What, AbsFile).
hook_ec_axioms(What, File):- fail, 
    prolog_load_context(module, M),
    dmsg(hook_ec_axioms(M, What, File)),fail.
hook_ec_axioms(_What, File):- atom(File), exists_file(File),
    forall((clause(ec_axiom(_, _, lsvm(_L,S,_Vs,_M)), _Body, Ref), File==S),
       erase(Ref)),!.

:- multifile(user:message_hook/3).
:- dynamic(user:message_hook/3).
:- module_transparent(user:message_hook/3).
user:message_hook(load_file(start(_Level, File)),_,_):- hook_ec_axioms(load,File),fail.
user:message_hook(include_file(start(_Level, File)),_,_):- hook_ec_axioms(include,File),fail.
:- multifile(prolog:make_hook/2).
:- module_transparent(prolog:make_hook/2).
prolog:make_hook(before, Files):-  maplist(hook_ec_axioms(make(before)),Files), fail.

:- multifile prolog:message//1.
prolog:message(welcome) -->  {hook_ec_axioms(welcome, welcome),fail}.





:- multifile(user:term_expansion/4).
:- dynamic(user:term_expansion/4).
:- module_transparent(user:term_expansion/4).
:- user:import(ec_loader:needs_proccess/2).
:- user:import(ec_loader:process_ec/2).
user:term_expansion(In,P,Out,PO):- 
  notrace((nonvar(P),compound(In), In\=(:- _), 
      needs_proccess(In, Type),PO=P)),
  Out = ( :- call(Type, In) ).



