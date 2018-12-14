
:- use_module(library(debug)).
:- use_module(library(listing)).
:- use_module(library(prolog_clause)).
:- use_module(library(logicmoo_common)).

is_sicstus:- \+ current_prolog_flag(version_data,swi(_,_,_,_)).


% =========================================
% Debug Info
% =========================================

testing_msg(_).

:- use_module('./ec_reader').

set_ec_option(N,V):- retractall(etmp:ec_option(N,_)),asserta(etmp:ec_option(N,V)).

:- set_ec_option(verbose, all).
:- set_ec_option(extreme, false).
:- set_ec_option(debug, failure).

is_dbginfo(N):- var(N),!, fail.
is_dbginfo(N=V):- !, etmp:ec_option(N, V).
is_dbginfo(not(N)):- !, \+ is_dbginfo(N).
is_dbginfo(N):- is_list(N), !, maplist(is_dbginfo,N).
is_dbginfo(N):- etmp:ec_option(N, false),!,fail.
is_dbginfo(N):- etmp:ec_option(verbose, N),!.


maybe_nl:- notrace(format('~N',[])).

dbginfo(NV, G):- notrace(tracing), !,notrace,dbginfo(NV, G),notrace(trace).
dbginfo(NV, G):- \+ is_dbginfo(NV) -> true ; dbginfo(G). 
:- export(dbginfo/1).
dbginfo_else(NV,G,E):- is_dbginfo(NV) -> dbginfo(G); dbginfo(E).

:- meta_predicate catch_ignore(0).
catch_ignore(G):- ignore(catch(G,E,wdmsg(E))),!.

dbginfo(G):- notrace(tracing),!,notrace,dbginfo(G),notrace(trace).
dbginfo(Var):- var(Var),!, maybe_nl, format('ListVAR = ~p~n',[Var]).
dbginfo([]):- !, maybe_nl. 
dbginfo(call(G)):- !, catch_ignore(G).
dbginfo({G}):- !, maybe_nl, catch_ignore(G).
dbginfo([A|B]):- !, maybe_nl, dbginfo(A), maybe_nl, dbginfo(B),!.
dbginfo(N=V):- !, maybe_nl, catch_ignore(portray_clause(var(N):-V)).
dbginfo(nl):- !, maybe_nl, nl.
dbginfo(nl(N)):- !, maybe_nl, catch_ignore(forall(between(0,N,_),nl)).
dbginfo(fmt(F,A)):- !, catch_ignore(format(F,A)).
dbginfo(afmt(Ansi,F,A)):- !, catch_ignore(ansi_format(Ansi,F,A)).
dbginfo(NV):- catch_ignore(portray_clause(:- NV)), !.
:- export(dbginfo/1).
% =========================================
% Test Decls
% =========================================

:- meta_predicate test_body(*,0,*,*).
test_body(N,(Was=G,Body),Info,Vs):- Was==N,!, copy_term(G,NewN),!,Was=G, test_body(NewN,(Body),Info,Vs).
test_body(N,true,Info,Vs):- !, test_body(N,abdemo_solve(N,R),Info,['R'=R|Vs]).
test_body(N,Body,Info,Vs):-
   dbginfo(verbose, nl(2)),
   dbginfo(verbose, [Info,nl(1)]),
   %dbginfo(all,['body'=Body,nl(2)]),
   % copy_term(Body,BodyA),
   maybe_nl, write('START OUTPUT of '), write(N), write(' >>>>>'),
   maybe_nl, 
   ticks(Z1),   
   (call(Body)-> (Passed = true) ; Passed = '?!?!??!FAILED!?!?!?'),
   ticks(Z2), TotalTime is (Z2-Z1)/1000, 
   maybe_nl, write('<<<<< ENDOF OUTPUT of '), write(N),nl,
   dbginfo(verbose, nl(2)),
   % dbginfo(all,['bodyAgain'=BodyA, nl, nl, Vs]),
   dbginfo(all,Vs),
   maybe_nl, nl, 
   (Passed == true -> ansi_format(fg(cyan),'!!!PASSED!!! ~w time=~w',[N,TotalTime]) ;
     (ansi_format(hfg(red),'~p ~w time=~w',[Passed,N,TotalTime]),sleep(1.0))),
   nl,
   dbginfo(verbose, nl(2)).

run_tests:- 
  clause_w_names(do_test(N),Body,_Ref,File,Vs),
  once(test_body(N,Body,File,Vs)),
  fail.
run_tests:- current_prolog_flag(debug,false) -> halt(7) ; true.
:- export(run_tests/0).

% =========================================
% Plan Portrayal
% =========================================

write_plan_len(A,B):- length(A,AL), length(B,BL),write_plan(AL,BL).
write_plan(HA,BA):- write('Plan: '), write(HA), write('-'), write(BA), write('    ').
/* Emulates the writenl(1) function */

%writeNoln(A) :-  write(A),!.
writeNoln(_).

writenl(_).

writeYesln(_).

% =========================================
% Axiom Access
% =========================================

system:axiom(X):- throw(use(current_axiom/2,not(axiom(X)))).
:- lock_predicate(system:axiom/1).

system:axiom(X,Y):- throw(use(current_axiom/2,not(axiom(X)))).
:- lock_predicate(system:axiom/2).

system:abducible(A):- current_plan_domain(abducible(A)).
:- lock_predicate(system:abducible/1).

system:executable(A):- current_plan_domain(executable(A)).
:- lock_predicate(system:executable/1).

%:- dynamic(current_axiom/3).
%:- multifile(current_axiom/3).
%:- dynamic(current_plan_domain/2).
%:- multifile(current_plan_domain/2).

:- module_transparent(system:current_plan_domain/1).
system:current_plan_domain(A):- current_plan_domain((A), _LSV).
:- lock_predicate(system:current_plan_domain/1).
%:- system:import(current_plan_domain/2).

:- module_transparent(system:current_axiom/2).
system:current_axiom(G,Gs):- current_axiom(G,Gs,_Info).
:- lock_predicate(system:current_axiom/2).
%:- system:import(current_axiom/3).


get_linfo(lsvm(L,F,Vs,M)):- 
  current_stream(F,read,S),atom(F),
  stream_property(S,file_name(F)),
  ignore(stream_property(S,line_count(L));line_or_char_count(S,L);stream_property(S,line_or_char_count(L))),
  ignore(L=999),!, 
  '$current_source_module'(M),
  nb_current('$variable_names',Vs).


process_axiom( HB ):- get_linfo(LSV), process_axiom( LSV, HB ).
process_axiom( _, HB , LSV):- !,process_axiom( LSV, HB ).
process_axiom( lsvm(L,S,Vs,M), HB ):-  convert_to_axiom(lsvm(L,S,Vs,M),HB,NEWHB), 
  %M:assertz('$source_location'(S, L):NEWHB),
  assertz(M:NEWHB),
 % M:assertz_with_names('$source_location'(S, L):NEWHB, Vs),
  nop(dmsg(green(NEWHB))).

convert_to_axiom(LSV,  I, O):- \+ callable(I),!, I = O.
convert_to_axiom(LSV, M:H, M:HH):- !, convert_to_axiom(LSV, H,HH).
convert_to_axiom(LSV, (H:-B),(HH:- B)):- !, convert_to_axiom(LSV, H,HH).
convert_to_axiom(LSV, axiom(X),O):- !, convert_to_axiom(LSV, axiom(X,[]),O).
convert_to_axiom(LSV, axiom(X,Y), current_axiom(X,Y,LSV)).
convert_to_axiom(LSV, O, current_plan_domain(O,LSV)).


get_process_axiom(process_axiom(LSV)):- get_linfo(LSV). 

arg_info(domain,sort,arginfo).
arg_info(domain,subsort,arginfo).
arg_info(domain,isa,arginfo).
arg_info(domain,reified_sort,arginfo).
arg_info(domain,fluent,arginfo).
arg_info(domain,noninertial,arginfo).
arg_info(domain,predicate,arginfo).
arg_info(domain,function,arginfo).
arg_info(domain,event,arginfo).
%arg_info(domain,axiom,v(axiom,list)).
arg_info(axiom_head,happens,v(event,time)).
arg_info(axiom_head,holds_at,v(fluent,time)).
arg_info(axiom_head,initially,v(fluent)).
%arg_info(axiom_head,releasesAt,v(fluent,time)).
arg_info(axiom_head,initiates,v(event,fluent,time)).
arg_info(axiom_head,terminates,v(event,fluent,time)).
arg_info(axiom_head,releases,v(event,fluent,time)).
arg_info(axiom_head,trajectory,v(fluent,time,fluent,offset)).

needs_process_axiom(C):- \+ compound(C), !, fail.
needs_process_axiom(axiom(_,_)).
needs_process_axiom(axiom(_)).
needs_process_axiom(abducible(_)).
needs_process_axiom(executable(_)).
needs_process_axiom(P):- functor(P,F,_),arg_info(_,F,_).


:- module_transparent(hook_ec_axioms/2).
hook_ec_axioms(What, File):- var(File), !, current_input(Input), hook_ec_axioms(What, Input).
hook_ec_axioms(What, file(_File,AbsFile)):- !, hook_ec_axioms(What, file(AbsFile)).
hook_ec_axioms(What, file(AbsFile)):- !, hook_ec_axioms(What, AbsFile).
hook_ec_axioms(What, File):- fail, 
    prolog_load_context(module, M),
    dmsg(hook_ec_axioms(M, What, File)),fail.
hook_ec_axioms(What, File):- atom(File), exists_file(File),
    forall((clause(current_axiom(_, _, lsvm(_L,S,_Vs,_M)), _Body, Ref), File==S),
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

needs_proccess(PA, LSV):- needs_process_axiom(PA), get_process_axiom(LSV).
needs_proccess((H :- B),How):- nonvar(H),!,needs_proccess(H,How).
needs_proccess( M:H, How):- nonvar(H),!,needs_proccess(H,How).


% =========================================
% Goal/Plan translating
% =========================================

fail_solve_goal(G,R):- \+ abdemo_solve(G,R).

abdemo_solve(Gs,R):- abdemo_solve(Gs,R,1,4).
abdemo_solve(Gs,R,H,L):- 
  When = now,
  must(fix_goal(When,Gs,Gs0)), !,
  must(fix_time_args(When,Gs0,Gss)), !,  
  dbginfo(all, [nl,realGoal=Gss,nl]),
  abdemo_special(depth(H,L), Gss, R).

fix_goal(_, Nil,[]):- Nil==[],!.
fix_goal(T,[G|Gs],GGs):- !, fix_goal(T,G,G0),fix_goal(T,Gs,Gs0),append(G0,Gs0,GGs),!.
fix_goal(T,(G,Gs),GGs):- !, fix_goal(T,G,G0),fix_goal(T,Gs,Gs0),append(G0,Gs0,GGs),!.
fix_goal(T,{Gs},GGs):- !, fix_goal(T,Gs,GGs).
fix_goal(T, G, [Gs, b(T,T2),b(T2,end)]):- G =.. [F,A], already_good(F,2), G0 =.. [F,A,T], next_t(T,T2), fix_goal(T2,G0,Gs).
fix_goal(_, G, [G]):- functor(G,F,A), already_good(F,A),!.
fix_goal(T, G, [happens(G,T)]):- executable(G),!.
fix_goal(T, G, [holds_at(G,T)]).

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
already_good(F,A):- functor(P,F,A),clause(abducible(PP),_),compound(PP),PP=P.
already_good(F,A):- functor(P,F,A),clause(abducible(PP),_),compound(PP),PP=P.


:- multifile(user:term_expansion/4).
:- dynamic(user:term_expansion/4).
:- module_transparent(user:term_expansion/4).
:- user:import(ec:needs_proccess/2).
user:term_expansion(In,P,Out,PO):- 
  notrace((nonvar(P),compound(In), In\=(:- _), needs_proccess(In, Type),PO=P)),
  Out = ( :- call(Type, In) ).

?- is_sicstus -> prolog_flag(single_var_warnings,_,off) ; true.

?- is_sicstus -> ensure_loaded(abdemo_incl_sicstus) ; ensure_loaded(abdemo_incl_swi).

:- style_check(-singleton).
/* Emulates the writenl(1) function */

