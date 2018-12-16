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
:- module(ec_loader,[ec_load/1, op(1200,xfx,'<-'),op(1200,xfx,'<->')]).

:- reexport('./ec_reader').

:- export(ec_load/1).
ec_load(F):-
  is_filename(F),  
  \+ etmp:ec_option(load(F), _),
  asserta(etmp:ec_option(load(F), ec_load)),
  e_to_pl(process_ec, current_output, F).


:- export(ect/0).
ect:- ec_load('examples/FrankEtAl2003/Story1.e').



fix_goal_add_on_arg(T, G, G0, [b(T,T2),b(T2,end)]):- G =.. [F,A], already_good(F,2), G0 =.. [F,A,T], next_t(T,T2).
fix_goal_add_on_arg(T, G, G0, [b(T,T2),b(T2,end)]):- G =.. [F,A,B], already_good(F,3), G0 =.. [F,A,B,T], next_t(T,T2).


:- export(fix_goal/3).


to_axiom_head(T,G,GG) :-  notrace(fix_axiom_head(T,G,GG)),!.
to_axiom_head(_,G,G) :- fix_axiom_head(T,G,GG),!.

to_axiom_body(T,G,GGs) :-  fix_goal(T,G,GGs).

fix_goal(_, Nil,[]):- Nil==[],!.
fix_goal(T,[G|Gs],GGs):- !, fix_goal(T,G,G0),fix_goal(T,Gs,Gs0),append(G0,Gs0,GGs),!.
fix_goal(T,(G,Gs),GGs):- !, fix_goal(T,G,G0),fix_goal(T,Gs,Gs0),append(G0,Gs0,GGs),!.
fix_goal(T,{Gs},GGs):- !, fix_goal(T,Gs,GGs).
fix_goal(T, G, [Gs| TExtra]):- fix_goal_add_on_arg( T, G, Gs, TExtra),!.
fix_goal(T, G, [GGs]):- fix_axiom_head(T,G,GGs),!.
fix_goal(T, G, [holds_at(G,T)]).

:- export(fix_axiom_head/3).
fix_axiom_head(_, G, G):- \+ callable(G),!.
fix_axiom_head(T, neg(holds_at(N,AT)),O):- !, fix_axiom_head(T, holds_at(neg(N,AT)),O).
fix_axiom_head(T, holds_at(N,AT),O):- AT==0, fix_axiom_head(T, initially(N),O).
fix_axiom_head(T, happens(F, T1, T2), O):- T1==T2,!, fix_axiom_head(T, happens(F, T1), O).
fix_axiom_head(_, X\=Y, diff(X,Y)).
fix_axiom_head(T, P,PP):- P =..[F|Args],functor(P,F,A), arg_info(AxH,F,Arity),!,
   functor(Arity,_,N),  must(correct_ax_args(T,F,A,Args,AxH,Arity,N,PP)).
fix_axiom_head(T, G, happens(G,T)):- functor_skel(G,P), executable(P),!.
fix_axiom_head(T, G, happens(G,T)):- functor_skel(G,P), ec_current_domain(event(P)).
fix_axiom_head(T, G, holds_at(G,T)):- functor_skel(G,P), ec_current_domain(fluent(P)).
fix_axiom_head(T, G, G):- functor_skel(G,P), ec_current_domain(predicate(P)).
fix_axiom_head(T, G, Gs):- fix_goal_add_on_arg( T, G, Gs, _TExtra),!.
fix_axiom_head(_, G, G):- functor(G,F,A), already_good(F,A),!.
%fix_axiom_head(T, G, P):- ec_to_ax(T, G,P),!.

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

t_plus_or_minus_1(In, T+1, TN, In):- lookup_time_val(TN,TpN,In).
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


:- export(make_falling_edges_v2/5).
make_falling_edges_v2(_Stem,LastTime,[],[],LastTime):-!.
make_falling_edges_v2(Stem,LastTime,
              [happens(Event,When1)|HapsList],
              [before(LastTime,ThisTime),happens(Event,ThisTime,NextTime)|Befores],Out):- 
   atom_concat_gs(Stem, When1, ThisTime),
   atom_concat_gs(Stem, When1, NextTime),
  make_falling_edges_v2(Stem, NextTime, HapsList,Befores,Out).
make_falling_edges_v2(Stem,LastTime,
              [happens(Event,When1,When2)|HapsList],
              [before(LastTime,ThisTime),happens(Event,ThisTime,NextTime)|Befores],Out):- 
   atom_concat_gs(Stem, When1, ThisTime),
   atom_concat_gs(Stem, When2, NextTime),
  make_falling_edges_v2(Stem, NextTime, HapsList,Befores,Out).

atom_concat_gs(Stem, When1, ThisTime):- atom_concat(Stem, When1, Lose1),gensym(Lose1,ThisTime).

make_falling_edges(_Stem,LastTime,[],[],LastTime):-!.
make_falling_edges(Stem, LastTime,
              [happens(Event, When1)|HapsList],
              [before(LastTime, ThisTime),holds_at(has_occured(Event),ThisTime)|Befores],Out):- 
   atom_concat_gs(Stem, When1, ThisTime),
  make_falling_edges(Stem,ThisTime, HapsList,Befores,Out).
make_falling_edges(Stem,LastTime,
              [happens(Event,When1,When2)|HapsList],
              [before(LastTime,ThisTime),holds_at(has_occured(Event),ThisTime)|Befores],Out):- 
   atom_concat_gs(Stem, When1, ThisTime),
   atom_concat_gs(Stem, When2, NextTime),
  make_falling_edges(Stem, NextTime, HapsList,Befores,Out).


already_good(happens, 2).
already_good(happens, 3).
already_good(holds_at, 2).
already_good(b, 2).
already_good(is, 2).
already_good(diff, 2).
already_good(dif, 2).
already_good(terms_or_rels,3).
already_good(F,A):- functor(P,F,A),ec_current_domain(abducible(PP)),compound(PP),PP=P.
already_good(F,A):- functor(P,F,A),ec_current_domain(predicate(PP)),compound(PP),PP=P.



%predicate, option range load fluent event noninertial xor completion


%special_directive('!').
%special_directive('neg').

% builtin_pred(releasedAt).

%xfr_body(true,[]).

ec_to_ax(T, X,Y):-  \+ callable(X), !, X=Y.
ec_to_ax(_T, ec_option(X,Y),option(X,Y)):-!.
ec_to_ax(_T, option(X,Y),option(X,Y)):-!.

ec_to_ax(T, Compound=Value, Term):- compound(Compound), append_term(Compound, Value, Term0), ec_to_ax(T, Term0, Term),!.
ec_to_ax(T, (H<-B),O):- !, into_axiom(T,H,B,O).
ec_to_ax(T, (B->H),O):- !,  into_axiom(T,H,B,O).
ec_to_ax(T, (HB1<->HB2),[A,B]):- !, ec_to_ax(T, (HB1->HB2),A),ec_to_ax(T, (HB1<-HB2),B).
ec_to_ax(T, isa(E, C), List):- !, ec_to_ax(T, t(C, E), List).
ec_to_ax(_T, subsort(F, W), List):- !, to_fact_head([subsort(F, W),sort(F),sort(W)],List).
ec_to_ax(T, abducible(H),[abducible(H)]):- !.
ec_to_ax(_T, t(C, E), List):- !, to_fact_head([t(C, E), sort(C)],List).
ec_to_ax(T, axiom(H,B), axiom(H,B)):- !.
ec_to_ax(T, H, AH):- fix_axiom_head(T,H,AH),!.
%ec_to_ax(_T, range(X,Y,Z),range(X,Y,Z)):-!.

to_fact_head(H,List):- H=List.

into_axiom(T,H,B,axiom(AH,AB)):- to_axiom_head(T1,H,AH),to_axiom_body(T2,B,AB),!,ignore(T=T1),ignore(T2=T1).






:- export(get_linfo/1).
get_linfo(lsvm(L,F,Vs,M)):- 
  quietly((must(s_l(F,L)),!,
  '$current_source_module'(M),
  nb_current('$variable_names',Vs))).


:- export(process_ec/1).
:- module_transparent(process_ec/1).
process_ec( HB ):- notrace(must(get_linfo(LSV))), process_ec( LSV, HB ).
%:- export(process_ec/2).
%process_ec( _, HB , LSV):- !,process_ec( LSV, HB ).

:- export(process_ec/2).
:- module_transparent(process_ec/2).
process_ec( lsvm(L,S,Vs,M), HB ):-  
  must(convert_to_axiom(lsvm(L,S,Vs,M),HB,NEWHB)),
  do_process_ec(M, NEWHB).

:- export(do_process_ec/2).
:- module_transparent(do_process_ec/2).
do_process_ec(M, NonCallable) :- assertion((current_module(M),callable(NonCallable))), fail.
do_process_ec(M, NEWHB):- is_list(NEWHB), !, maplist(do_process_ec(M), NEWHB).
do_process_ec(M, (:- GOAL)):- !, must(M:GOAL).
do_process_ec(M, (?- GOAL)):- !, M:forall(GOAL, true).
% How to? M:assertz('$source_location'(S, L):NEWHB),
%do_process_ec(M, NEWHB):- wdmsg(do_process_ec(M, NEWHB)),fail.
do_process_ec(M, NEWHB):- M:assertz(NEWHB).

:- export(convert_to_axiom/3).
convert_to_axiom(LSV,  I, O):- \+ callable(I),!, I = O.
convert_to_axiom(_LSV, option(X,Y), [(:- set_ec_option(X,Y))]).
convert_to_axiom(LSV, M:H, [M:HH]):- !, convert_to_axiom(LSV, H, HH).
convert_to_axiom(LSV, (H:-B),[(HH:- B)]):- !, convert_to_axiom(LSV, H,HH).
convert_to_axiom(LSV, event(P), [ec_current_domain_db(event(P),LSV),executable(PP)]):-
  compound_name_arity(P,F,A),compound_name_arity(PP,F,A).
convert_to_axiom(LSV, X, O):- nop(debug_var('AxTime',Time)), ec_to_ax(Time, X,Y),!, convert_to_axiom1(LSV, Y, O).
convert_to_axiom(LSV, Y, O):- convert_to_axiom1(LSV, Y, O).

convert_to_axiom1(LSV, P, O):- is_axiom_head(P),!, convert_to_axiom1(LSV, axiom(P), O).
convert_to_axiom1(LSV, axiom(P), O):- convert_to_axiom1(LSV, axiom(P ,[]), O).
convert_to_axiom1(LSV, axiom(X,Y), [ec_axiom(X,Y,LSV)]).
convert_to_axiom1(LSV, Pred, [ec_current_domain_db(Pred,LSV)]).


is_axiom_head(P):- compound_name_arity(P,F,_), arg_info(axiom_head,F,_),!.
is_axiom_head(P):- functor_skel(P, G), ec_current_domain(predicate(G)),!.


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

correct_ax_args(T,F,A,Args,axiom_head,Arity,N, PP):-  N is A +1 ,!, append(Args,[T],NewArgs), PP =.. [F|NewArgs].
correct_ax_args(_T,F,A,Args,axiom_head,Arity,N,PP):- A=N, PP =.. [F|Args].
correct_ax_args(_T,F,1,Args,domain,arginfo,0,PP):- PP =.. [F|Args].

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
needs_proccess((H :- B),How):- nonvar(H),!,needs_proccess(H,How).
needs_proccess( M:H, How):- nonvar(H),!,needs_proccess(H,How).



:- module_transparent(hook_ec_axioms/2).
:- export(hook_ec_axioms/2).
:- prolog:import(hook_ec_axioms/2).
hook_ec_axioms(What, File):- var(File), !, current_input(Input), hook_ec_axioms(What, Input).
hook_ec_axioms(What, file(_File,AbsFile)):- !, hook_ec_axioms(What, file(AbsFile)).
hook_ec_axioms(What, file(AbsFile)):- !, hook_ec_axioms(What, AbsFile).
hook_ec_axioms(What, File):- fail, 
    prolog_load_context(module, M),
    dmsg(hook_ec_axioms(M, What, File)),fail.
hook_ec_axioms(What, File):- atom(File), exists_file(File),
    forall((clause(ec_axiom(_, _, lsvm(_L,S,_Vs,_M)), _Body, Ref), File==S),
       erase(Ref)),!.

:- multifile(user:message_hook/3).
:- dynamic(user:message_hook/3).
:- module_transparent(user:message_hook/3).
user:message_hook(load_file(start(Level, File)),_,_):- hook_ec_axioms(load,File),fail.
user:message_hook(include_file(start(Level, File)),_,_):- hook_ec_axioms(include,File),fail.
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


