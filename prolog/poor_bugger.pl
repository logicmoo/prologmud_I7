% ===================================================================
% File 'poor_bugger.pl'
% Purpose: a small number of debugging utils
% The original debugging package that I created had too many interdependencies
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'poor_bugger.pl' 1.0.0
% Revision:  $Revision: 1.1 $
% Revised At:   $Date: 2035/06/06 15:43:15 $
% ===================================================================

% :- set_prolog_flag(gc,false).
:- module(poor_bugger,[bugout1/1,debug_var/2]).

ludef:- list_undefined([module_class([user,system,library,test,development])]).

:- use_module(library(logicmoo_util_strings)).

%atom_contains(Atom,SubAtom):- atomic_list_concat([_,_|_],SubAtom,Atom).


:- if(exists_source(library(xlisting))).
:- use_module(library(xlisting)).
:- endif.

:- op(1050,xfy,('*->')).

update_deps :-
   pack_install(each_call_cleanup,[url('https://github.com/TeamSPoon/each_call_cleanup.git'),upgrade(true),interactive(false)]),
   pack_install(no_repeats,[url('https://github.com/TeamSPoon/no_repeats.git'),upgrade(true),interactive(false)]),
   pack_install(loop_check,[url('https://github.com/TeamSPoon/loop_check.git'),upgrade(true),interactive(false)]),
   % The whole point of me making Poor_bugger is to not need to install must_trace :)
   nop(pack_install(must_trace,[url('https://github.com/TeamSPoon/must_trace.git'),upgrade(true),interactive(true)])),
   % hoses developement 
   nop(pack_install(small_adventure_games,[url('https://github.com/TeamSPoon/small_adventure_games.git'),upgrade(true),interactive(true)])),
   !.



:- module_transparent(swi_soft_if_then/3).
:- meta_predicate(swi_soft_if_then(0,0,0)).
swi_soft_if_then(C,T,F):-  C *-> T ; F.


bugout1(P):- notrace(ansi_format([fg(cyan)],'~N% ~p.~n',[P])).

:- module_transparent(dmust/1).
:- meta_predicate dmust(:).
dmust(M:G):- dmust_m(M,G).
dmust_m(M,(A,!,B)):-!,dmust_m(M,A),!,dmust_m(M,B).
dmust_m(M,(A,B)):-!,dmust_m(M,A),dmust_m(M,B).
dmust_m(M,(A;B)):-!,(call(M:A);dmust_m(M,B)).
dmust_m(M,(A->B;C)):-!,(call(M:A)->dmust_m(M,B);dmust_m(M,C)).
dmust_m(M,(A*->B;C)):-!,(call(M:A)*->dmust_m(M,B);dmust_m(M,C)).
dmust_m(M,A):- call(M:A)*->true; (failed_dmust(M:A),!,fail).

:- meta_predicate dmust_det(:).
dmust_det(M:G):- dmust_det_m(M,G).
dmust_det_m(M,(A,B)):-!,dmust_det_m(M,A),!,dmust_det_m(M,B),!.
dmust_det_m(M,(A;B)):-!,(call(M:A)->true;dmust_det_m(M,B)),!.
dmust_det_m(M,(A->B;C)):-!,(call(M:A)->dmust_det_m(M,B);dmust_det_m(M,C)),!.
dmust_det_m(M,(A*->B;C)):-!,(call(M:A)*->dmust_det_m(M,B);dmust_det_m(M,C)),!.
dmust_det_m(M,A):- call(M:A)*->true; (failed_dmust(M:A),!,fail).


:- module_transparent(failed_dmust/1).
:- meta_predicate failed_dmust(*).
failed_dmust(once(A)):-!, failed_dmust(A),!.
failed_dmust((A,B)):- !,bugout1(dmust_start_mid(A)),confirm_rtrace,ignore(rtrace(A)),bugout1(dmust_mid(A)), failed_dmust(B).
failed_dmust(A):- simplify_dbug(A,AA), bugout1(failed_dmust_start(AA)),confirm_rtrace,ignore(rtrace(A)),bugout1(failed_dmust_end(AA)),
  nortrace,notrace.

confirm_rtrace:- nortrace,notrace, trace.

:- module_transparent(no_repeats_must/1).
:- meta_predicate(no_repeats_must(0)).
no_repeats_must(Call):-
 gripe_time(0.5,no_repeats(Call)) *-> true;
  (fail,(bugout1(warn(show_failure(Call))),!,fail)).

:- ensure_loaded(library(no_repeats)).
:- ensure_loaded(library(loop_check)).
:- use_module(library(must_sanity)).

:- if(\+ current_module(pfc)).
:- module_transparent(call_u/1).
call_u(Q):- notrace(current_predicate(_,Q)),call(call,Q).
%call_u(P) :- call(call,P).
:- endif.



%! nop( :Goal) is det.
%
%  Comments out code without losing syntax
%
:- if(\+ current_module(must_trace)).

nop(_).


/*
scce_orig(Setup,Goal,Cleanup):-
   \+ \+ '$sig_atomic'(Setup), 
   catch( 
     ((Goal, deterministic(DET)),
       '$sig_atomic'(Cleanup),
         (DET == true -> !
          ; (true;('$sig_atomic'(Setup),fail)))), 
      E, 
      ('$sig_atomic'(Cleanup),throw(E))). 

:- abolish(system:scce_orig,3).


[debug]  ?- scce_orig( (writeln(a),trace,start_rtrace,rtrace) , (writeln(b),member(X,[1,2,3]),writeln(c)), writeln(d)).
a
b
c
d
X = 1 ;
a
c
d
X = 2 ;
a
c
d
X = 3.


*/

scce_orig(Setup0,Goal,Cleanup0):-
  notrace((Cleanup = notrace('$sig_atomic'(Cleanup0)),Setup = notrace('$sig_atomic'(Setup0)))),
   \+ \+ Setup, !,
   (catch(Goal, E,(Cleanup,throw(E)))
      *-> (notrace(tracing)->(notrace,deterministic(DET));deterministic(DET)); (Cleanup,!,fail)),
     Cleanup,
     (DET == true -> ! ; (true;(Setup,fail))).



%% gripe_time( +TooLong, :Goal) is nondet.
%
% Gripe Time.
%

call_for_time(Goal,ElapseCPU,ElapseWALL,Success):- 
   statistics(cputime,StartCPU0),statistics(walltime,[StartWALL0,_]),
   My_Starts = start(StartCPU0,StartWALL0),  
   (Goal*->Success=true;Success=fail),
   statistics(cputime,EndCPU),statistics(walltime,[EndWALL,_]),
   arg(1,My_Starts,StartCPU), ElapseCPU is EndCPU-StartCPU,nb_setarg(1,My_Starts,EndCPU),
   arg(2,My_Starts,StartWALL), ElapseWALL is  (EndWALL-StartWALL)/1000,nb_setarg(2,My_Starts,EndWALL).

gripe_time(_TooLong,Goal):- current_prolog_flag(runtime_speed,0),!,Goal.
gripe_time(_TooLong,Goal):- current_prolog_flag(runtime_debug,0),!,Goal.
gripe_time(_TooLong,Goal):- current_prolog_flag(runtime_debug,1),!,Goal.
% gripe_time(_TooLong,Goal):- \+ current_prolog_flag(runtime_debug,3),\+ current_prolog_flag(runtime_debug,2),!,Goal.
gripe_time(TooLong,Goal):-
 call_for_time(Goal,ElapseCPU,ElapseWALL,Success),
 (ElapseCPU>TooLong -> bugout1(gripe_CPUTIME(Success,warn(ElapseCPU>TooLong),Goal)) ;
   (ElapseWALL>TooLong -> bugout1(gripe_WALLTIME(Success,warn(ElapseWALL>TooLong),Goal,cputime=ElapseCPU)) ;
     true)),
  Success.



:- module_transparent(loop_check_u/1).
loop_check_u(P):- loop_check(call_u(P)).

% :- fixup_exports.

%:- multifile(parser_sharing:term_expansion/4).
%:- rtrace.
/*
parser_sharing:term_expansion(G,I,GG,O):- nonvar(I),compound(G),importing_clause(G,GG) -> G \== GG, I=O.
:- export(parser_sharing:term_expansion/4).
*/
%:- nortrace.

:- module_transparent(nortrace/0).

:-thread_local(t_l:rtracing/0).
:-thread_local(t_l:tracer_reset/1).
:-thread_local(t_l:wasguitracer/1).
:-thread_local(t_l:wastracer/1).

:- meta_predicate(call_call(0)).
call_call(G):-call(G).


:- meta_predicate
   rtrace(0),
   restore_trace(0),
   on_x_debug(0),
   on_f_rtrace(0),   
   rtrace_break(0),
   quietly(0),
   ftrace(0).

%! on_f_rtrace( :Goal) is det.
%
% If :Goal fails trace it 
%


% on_f_rtrace(Goal):-  Goal *-> true; ((nortrace,notrace,debugCallWhy(failed(on_f_rtrace(Goal)),Goal)),fail).

on_f_rtrace(Goal):-  Goal *-> true; (rtrace(Goal),debugCallWhy(on_f_rtrace(Goal),Goal)).



debugCallWhy(Why, C):- bugout1(Why),catch(failed_dmust(C),E,bugout1(cont_X_debugCallWhy(E,Why, C))).

%! on_x_debug( :Goal) is det.
%
% If there If Is an exception in :Goal then rtrace.
%
on_x_debug(Goal):- 
 ((( tracing; t_l:rtracing),maybe_leash(+exception))) 
  -> Goal
   ;
   (catch(Goal,E,(ignore(debugCallWhy(on_x_debug(E,Goal),Goal)),throw(E)))).


:- meta_predicate('$with_unlocked_pred_local'(:,0)).
'$with_unlocked_pred_local'(MP,Goal):- strip_module(MP,M,P),Pred=M:P,
   (predicate_property(Pred,foreign)-> true ;
  (
 ('$get_predicate_attribute'(Pred, system, OnOff)->true;throw('$get_predicate_attribute'(Pred, system, OnOff))),
 (==(OnOff,0) -> Goal ;
 setup_call_cleanup('$set_predicate_attribute'(Pred, system, 0),
   catch(Goal,E,throw(E)),'$set_predicate_attribute'(Pred, system, 1))))).

:- meta_predicate(totally_hide(:)).
totally_hide(MP):- strip_module(MP,M,P),Pred=M:P,
   % (current_prolog_flag(runtime_debug,N), N>2) -> unhide(Pred) ; 
  '$with_unlocked_pred_local'(Pred,
   (('$set_predicate_attribute'(Pred, trace, false),'$set_predicate_attribute'(Pred, hide_childs, true)))).

%% with_unlocked_pred_b( ?Pred, :Goal) is semidet.
%
% Using Unlocked Predicate.
%
with_unlocked_pred_b(MP,Goal):- strip_module(MP,M,P),Pred=M:P,
   (predicate_property(Pred,foreign)-> true ;
  (
 ('$get_predicate_attribute'(Pred, system, 0) -> Goal ;
 setup_call_cleanup('$set_predicate_attribute'(Pred, system, 0),
   catch(Goal,E,throw(E)),'$set_predicate_attribute'(Pred, system, 1))))).

unhide(Pred):- '$set_predicate_attribute'(Pred, trace, 1),'$set_predicate_attribute'(Pred, hide_childs, 0).


/*
mpred_trace_childs(W) :- forall(match_predicates(W,M,Pred,_,_),(
   with_unlocked_pred_b(M:Pred,(
   '$set_predicate_attribute'(M:Pred, trace, 0),
   %'$set_predicate_attribute'(M:Pred, noprofile, 0),
   '$set_predicate_attribute'(M:Pred, hide_childs, 0))))).   
*/

%! maybe_leash( +Flags) is det.
%
% Only leashes interactive consoles
%
maybe_leash(Some):- notrace((maybe_leash->leash(Some);true)).
:- totally_hide(maybe_leash/1).

maybe_leash:- notrace((\+ current_prolog_flag(runtime_must,keep_going), \+ non_user_console)).

%non_user_console:- !,fail.
non_user_console:- \+ stream_property(current_input, tty(true)),!.
non_user_console:- \+ stream_property(current_input,close_on_abort(false)).

%! get_trace_reset( ?Reset) is det.
%
% Get Tracer.
%
get_trace_reset((notrace,set_prolog_flag(debug,WasDebug),CC3,'$visible'(_, OldV),'$leash'(_, OldL),RestoreTrace)):- 
     (notrace(tracing) -> (notrace,RestoreTrace = trace) ; RestoreTrace = notrace),
     '$leash'(OldL, OldL),'$visible'(OldV, OldV),
     (current_prolog_flag(debug,true)->WasDebug=true;WasDebug=false),     
     (current_prolog_flag(gui_tracer, GWas)->CC3=set_prolog_flag(gui_tracer, GWas);CC3=true),!,
     RestoreTrace.
:- totally_hide(get_trace_reset/1).


%! push_guitracer is nondet.
%
% Save Guitracer.
%
push_guitracer:-  notrace(ignore(((current_prolog_flag(gui_tracer, GWas);GWas=false),asserta(t_l:wasguitracer(GWas))))).
:- totally_hide(push_guitracer/0).


%! pop_guitracer is nondet.
%
% Restore Guitracer.
%
pop_guitracer:- notrace(ignore(((retract(t_l:wasguitracer(GWas)),set_prolog_flag(gui_tracer, GWas))))).
:- totally_hide(pop_guitracer/0).


%! push_tracer is det.
%
% Push Tracer.
%
push_tracer:- get_trace_reset(Reset)->asserta(t_l:tracer_reset(Reset)).
:- totally_hide(push_tracer/0).

%! pop_tracer is det.
%
% Pop Tracer.
%
pop_tracer:- notrace((retract(t_l:tracer_reset(Reset))->Reset;true)).
:- totally_hide(pop_tracer/0).

%! reset_tracer is det.
%
% Reset Tracer.
%
reset_tracer:- ignore((t_l:tracer_reset(Reset)->Reset;true)).
:- totally_hide(reset_tracer/0).


:- multifile(user:prolog_exception_hook/4).
:- dynamic(user:prolog_exception_hook/4).
:- module_transparent(user:prolog_exception_hook/4).

% Make sure interactive debugging is turned back on

user:prolog_exception_hook(error(_, _),_, _, _) :- leash(+all),fail.

user:prolog_exception_hook(error(_, _),_, _, _) :- fail, 
   notrace((  reset_tracer ->
     maybe_leash ->
     t_l:rtracing ->
     leash(+all),
     fail)).

%! quietly( :Goal) is nondet.
%
% Unlike notrace/1, it allows nondet tracing 
%
% But also may be break when excpetions are raised during Goal.
%

% Version 1
quietly(Goal):- \+ tracing,!,call(Goal).
quietly(Goal):- notrace,call_cleanup(Goal,trace).

% version 2 
quietly2(Goal):- \+ tracing -> Goal ; (notrace,call_cleanup(scce_orig(notrace,Goal,trace),trace)).

% version 3 
% quietly(Goal):- !, Goal.  % for overiding
quietly3(Goal):- \+ tracing -> Goal ; 
 (notrace,
  (((Goal,deterministic(YN))) *->
     (YN == yes -> trace ; (trace;(notrace,fail)));
  (trace,!,notrace(fail)))).



deterministically_must(G):- call(call,G),deterministic(YN),true,
  (YN==true -> true; 
     ((bugout1(failed_deterministically_must(G)),(!)))),!.


%:- totally_hide(quietly/1).


%! rtrace is det.
%
% Start RTracer.
%
rtrace:- start_rtrace,trace.

:- totally_hide(rtrace/0).

start_rtrace:-
      leash(-all),
      assert(t_l:rtracing),
      set_prolog_flag(access_level,system),
      push_guitracer,
      set_prolog_flag(gui_tracer,false),
      visible(+all),
      visible(+exception),
      maybe_leash(+exception).

:- totally_hide(start_rtrace/0).

%! srtrace is det.
%
% Start RTracer.
%
srtrace:- notrace, set_prolog_flag(access_level,system), rtrace.

:- totally_hide(srtrace/0).



%! nortrace is det.
%
% Stop Tracer.
%
stop_rtrace:- 
  notrace,
  maybe_leash(+all),
  visible(+all),
  maybe_leash(+exception),
  retractall(t_l:rtracing),
  !.

:- totally_hide(stop_rtrace/0).
:- system:import(stop_rtrace/0).

nortrace:- stop_rtrace,ignore(pop_tracer).

:- totally_hide(nortrace/0).


:- thread_local('$leash_visible'/2).

%! restore_trace( :Goal) is det.
%
% restore  Trace.
%
%! restore_trace( :Goal) is nondet.
%
% restore  Trace.
%
restore_trace(Goal):- 
  setup_call_cleanup(
   push_leash_visible,
   scce_orig(push_tracer,Goal,pop_tracer),
   restore_leash_visible).

restore_trace0(Goal):- 
  '$leash'(OldL, OldL),'$visible'(OldV, OldV),
   scce_orig(restore_leash_visible,
   ((Goal*-> (push_leash_visible, '$leash'(_, OldL),'$visible'(_, OldV)) ; fail)),
   ('$leash'(_, OldL),'$visible'(_, OldV))).

:- totally_hide(system:'$leash'/2).
:- totally_hide(system:'$visible'/2).

push_leash_visible:- notrace((('$leash'(OldL0, OldL0),'$visible'(OldV0, OldV0), asserta('$leash_visible'(OldL0,OldV0))))).
restore_leash_visible:- notrace((('$leash_visible'(OldL1,OldV1)->('$leash'(_, OldL1),'$visible'(_, OldV1));true))).

% restore_trace(Goal):- setup_call_cleanup(get_trace_reset(Reset),Goal,notrace(Reset)).
:- totally_hide(restore_trace/0).



%! rtrace( :Goal) is nondet.
%
% Trace a goal non-interactively until the first exception on
%  total failure
%
% ?- rtrace(member(X,[1,2,3])).
%    Call: (9) [lists] lists:member(_7172, [1, 2, 3])    
%    Unify: (9) [lists] lists:member(_7172, [1, 2, 3])   
%    Call: (10) [lists] lists:member_([2, 3], _7172, 1)  
%    Unify: (10) [lists] lists:member_([2, 3], 1, 1)     
%    Exit: (10) [lists] lists:member_([2, 3], 1, 1)      
%    Exit: (9) [lists] lists:member(1, [1, 2, 3])        
% X = 1 ;                                                
%    Redo: (10) [lists] lists:member_([2, 3], _7172, 1)  
%    Unify: (10) [lists] lists:member_([2, 3], _7172, 1) 
%    Call: (11) [lists] lists:member_([3], _7172, 2)     
%    Unify: (11) [lists] lists:member_([3], 2, 2)        
%    Exit: (11) [lists] lists:member_([3], 2, 2)         
%    Exit: (10) [lists] lists:member_([2, 3], 2, 1)      
%    Exit: (9) [lists] lists:member(2, [1, 2, 3])        
% X = 2 ;                                                
%    Redo: (11) [lists] lists:member_([3], _7172, 2)     
%    Unify: (11) [lists] lists:member_([3], _7172, 2)    
%    Call: (12) [lists] lists:member_([], _7172, 3)      
%    Unify: (12) [lists] lists:member_([], 3, 3)         
%    Exit: (12) [lists] lists:member_([], 3, 3)          
%    Exit: (11) [lists] lists:member_([3], 3, 2)         
%    Exit: (10) [lists] lists:member_([2, 3], 3, 1)      
%    Exit: (9) [lists] lists:member(3, [1, 2, 3])        
% X = 3.                                                 
%                                                        
%  ?- rtrace(fail).                                      
%    Call: (9) [system] fail                             
%    Fail: (9) [system] fail                             
% ^  Redo: (8) [rtrace] rtrace:rtrace(user:fail)
% false.

/*
  ?- rtrace((member(X,[writeln(1),throw(good),writen(failed)]),X)).
   Call: (10) [lists] lists:member(_13424, [writeln(1), throw(good), writen(failed)])
   Unify: (10) [lists] lists:member(_13424, [writeln(1), throw(good), writen(failed)])
   Call: (11) [lists] lists:member_([throw(good), writen(failed)], _13424, writeln(1))
   Unify: (11) [lists] lists:member_([throw(good), writen(failed)], writeln(1), writeln(1))
   Exit: (11) [lists] lists:member_([throw(good), writen(failed)], writeln(1), writeln(1))
   Exit: (10) [lists] lists:member(writeln(1), [writeln(1), throw(good), writen(failed)])
   Call: (10) [system] writeln(1)
1
   Exit: (10) [system] writeln(1)
X = writeln(1) ;
   Redo: (11) [lists] lists:member_([throw(good), writen(failed)], _13424, writeln(1))
   Unify: (11) [lists] lists:member_([throw(good), writen(failed)], _13424, writeln(1))
   Call: (12) [lists] lists:member_([writen(failed)], _13424, throw(good))
   Unify: (12) [lists] lists:member_([writen(failed)], throw(good), throw(good))
   Exit: (12) [lists] lists:member_([writen(failed)], throw(good), throw(good))
   Exit: (11) [lists] lists:member_([throw(good), writen(failed)], throw(good), writeln(1))
   Exit: (10) [lists] lists:member(throw(good), [writeln(1), throw(good), writen(failed)])
   Call: (10) [system] throw(good)
ERROR: Unhandled exception: good
*/

set_leash_vis(OldL,OldV):- '$leash'(_, OldL),'$visible'(_, OldV),!.
:- totally_hide(set_leash_vis/2).

next_rtrace:- (nortrace;(rtrace,trace,notrace(fail))).
:- totally_hide(next_rtrace/0).


rtrace(Goal):- notrace(tracing)-> rtrace0((trace,Goal)) ; 
  setup_call_cleanup(current_prolog_flag(debug,WasDebug),
   rtrace0((trace,Goal)),(set_prolog_flag(debug,WasDebug),notrace(stop_rtrace))).
rtrace0(Goal):-
 setup_call_cleanup(notrace((current_prolog_flag(debug,O),rtrace)),
   (trace,Goal,notrace,deterministic(YN),
     (YN == true->!;next_rtrace)),
     notrace(set_prolog_flag(debug,O))).

:- '$hide'(rtrace/1).
:- '$hide'(rtrace0/1).
:- '$set_predicate_attribute'(rtrace/1, hide_childs, true).
:- '$set_predicate_attribute'(rtrace0/1, hide_childs, false).


%! rtrace_break( :Goal) is nondet.
%
% Trace a goal non-interactively and break on first exception 
% or on total failure
%
rtrace_break(Goal):- \+ maybe_leash, !, rtrace(Goal).
rtrace_break(Goal):- stop_rtrace,trace,debugCallWhy(rtrace_break(Goal),Goal).
%:- totally_hide(rtrace_break/1).
:- '$set_predicate_attribute'(rtrace_break/1, hide_childs, false).




:- '$hide'(quietly/1).
%:- if_may_hide(totally_hide(notrace/1,  hide_childs, 1)).
%:- if_may_hide(totally_hide(notrace/1)).
:- totally_hide(system:tracing/0).
:- totally_hide(system:notrace/0).
:- totally_hide(system:notrace/1).
:- totally_hide(system:trace/0).

%! ftrace( :Goal) is nondet.
%
% Functor Trace.
%
ftrace(Goal):- restore_trace((
   visible(-all),visible(+unify),
   visible(+fail),visible(+exception),
   maybe_leash(-all),maybe_leash(+exception),trace,Goal)).


/*
:- ignore((source_location(S,_),prolog_load_context(module,M),module_property(M,class(library)),
 forall(source_file(M:H,S),
 ignore((functor(H,F,A),
  ignore(((\+ atom_concat('$',_,F),(export(F/A) , current_predicate(system:F/A)->true; system:import(M:F/A))))),
  ignore(((\+ predicate_property(M:H,transparent), module_transparent(M:F/A), \+ atom_concat('__aux',_,F),
    debug(modules,'~N:- module_transparent((~q)/~q).~n',[F,A]))))))))).
*/
%:- use_module(library(logicmoo_utils_all)).
%:- fixup_exports.
:- totally_hide('$toplevel':save_debug).
:- totally_hide('$toplevel':toplevel_call/1).
:- totally_hide('$toplevel':residue_vars(_,_)).
:- totally_hide('$toplevel':save_debug).
:- totally_hide('$toplevel':no_lco).


:- endif.


:- meta_predicate reset_prolog_flag(0,*,*,*).
:- meta_predicate reset_prolog_flag(0,*,*).
:- meta_predicate system_default_debug(0).

reset_prolog_flag(YN, Name, SystemValue):- 
  YN -> set_prolog_flag(Name, SystemValue) ; true.

reset_prolog_flag(YN, Name, SystemValue, OverrideValue):-
  YN -> set_prolog_flag(Name, SystemValue)
   ;  set_prolog_flag(Name, OverrideValue).

system_default_debug(YN):-
  reset_prolog_flag(YN, answer_format, '~p', '~q'), 
  reset_prolog_flag(YN, answer_write_options, [quoted(true), portray(true), max_depth(10), spacing(next_argument)], 
   [quoted(true), portray(true), max_depth(4), spacing(next_argument)]), 
  reset_prolog_flag(YN, debugger_write_options, [quoted(true), portray(true), max_depth(10), attributes(portray), spacing(next_argument)], 
   [quoted(true), portray(true), max_depth(4), attributes(portray), spacing(next_argument)]), 
  reset_prolog_flag(YN, print_write_options, [portray(true), quoted(true), numbervars(true)], 
   [portray(true), quoted(true), numbervars(true)]), 

  reset_prolog_flag(YN, backtrace, true), 
  reset_prolog_flag(YN, backtrace_depth, 20, 2000), 
  reset_prolog_flag(YN, backtrace_goal_depth, 3, 4), 
  reset_prolog_flag(YN, backtrace_show_lines, true), 
  reset_prolog_flag(YN, debug, false, true), 
  reset_prolog_flag(YN, debug_on_error, true), 
  reset_prolog_flag(YN, debugger_show_context, false, true), 

  reset_prolog_flag(YN, gc, true), 

  reset_prolog_flag(YN, last_call_optimisation, true, false), 
  reset_prolog_flag(YN, optimise, false), 
  reset_prolog_flag(YN, optimise_debug, default), 

  reset_prolog_flag(YN, prompt_alternatives_on, determinism), 
  reset_prolog_flag(YN, toplevel_goal, default), 
  reset_prolog_flag(YN, toplevel_mode, backtracking), 
  reset_prolog_flag(YN, toplevel_residue_vars, false, true), 
  reset_prolog_flag(YN, toplevel_print_anon, true), 
  reset_prolog_flag(YN, toplevel_print_factorized, false, true), 
  reset_prolog_flag(YN, write_attributes, ignore),

  reset_prolog_flag(YN, warn_override_implicit_import, true), 
  reset_prolog_flag(YN, access_level, user),
  reset_prolog_flag(YN, sandboxed_load, false), 
  reset_prolog_flag(YN, save_history, true), 
  !.

:- system_default_debug(false).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
% :- nop(ensure_loaded('adv_debug')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module_transparent(dshow_call/1).
:- module_transparent(dshow_true/1).
:- module_transparent(dshow_fail/1).
:- module_transparent(dmust_tracing/1).
:- module_transparent(if_tracing/1).

:- meta_predicate(dmust_tracing(*)).
dmust_tracing(G):- notrace((tracing,cls)),!,dmust_det(G).
dmust_tracing(G):- call(G).
:- meta_predicate(if_tracing(*)).
if_tracing(G):- tracing -> notrace(G) ; true.


% '$hide'(Pred) :- '$set_predicate_attribute'(Pred, trace, false).
never_trace(_Spec):- prolog_load_context(reloading,true),!.
never_trace(Spec):- '$hide'(Spec),'$iso'(Spec),trace(Spec, -all).
:- call(ensure_loaded,library(lists)).
:- never_trace(lists:append(_,_,_)).
:- never_trace(lists:list_to_set/2).
:- never_trace(lists:member_(_,_,_)).
/*
:- never_trace(prolog_debug:assertion(_)).
*/

:- export(simplify_dbug/2).
simplify_dbug(G,GG):- \+ compound(G),!,GG=G.
simplify_dbug({O},{O}):- !.
simplify_dbug(List,O):-
 ( is_list(List) -> clip_cons(List,'...'(_),O) ;
 ( List = [_|_], append(LeftSide,Open,List),
  ((var(Open);Open \= [_|_])), !, assertion(is_list(LeftSide)),
 clip_cons(LeftSide,'...'(Open),O))).
simplify_dbug(G,GG):- compound_name_arguments(G,F,GL), F\==percept_props, !,
 maplist(simplify_dbug,GL,GGL),!,compound_name_arguments(GG,F,GGL).
simplify_dbug(G,G).              

%:- system:import(simplify_dbug/2).
%:- listing(simplify_dbug/2).


is_state_list(G,_):- \+ compound(G),!,fail.
is_state_list([G1|_],{GG,'...'}):- compound(G1),G1=structure_label(GG),!.
is_state_list([_|G],GG):- is_state_list(G,GG).
clip_cons(G,GG):- is_state_list(G,GG),!.
clip_cons(List,ClipTail,{Len,LeftS,ClipTail}):- 
 length(List,Len),
 MaxLen = 5, Len>MaxLen,
 length(Left,MaxLen),
 append(Left,_,List),!,
 maplist(simplify_dbug,Left,LeftS).
clip_cons(Left,_,List):-maplist(simplify_dbug,Left,List).


%:- never_trace(lists:member(_,_)).
%:- never_trace(lists:append(_,_,_)).
:- meta_predicate(dshow_call(*)).
dshow_call((G1,G2)):- !,dshow_fail(G1),dshow_fail(G2).
dshow_call(G):- simplify_dbug(G,GG), swi_soft_if_then(G,bugout1(success_dshow_call(GG)),(bugout1(failed_dshow_call(GG)),!,fail)).

:- meta_predicate(dshow_fail(*)).
dshow_fail('\\+'(G1)):- !, \+ dshow_true(G1).
dshow_fail(G):- simplify_dbug(G,GG), swi_soft_if_then(G, true , (bugout1(failed_dshow_call(GG)),!,fail)).

:- meta_predicate(dshow_true(*)).
dshow_true('\\+'(G1)):- !, \+ dshow_fail(G1).
dshow_true(G):- simplify_dbug(G,GG),  swi_soft_if_then(G, bugout1(success_dshow_call(GG)) , (!,fail)).



:- multifile(user:portray/1).
:- dynamic(user:portray/1).
:- discontiguous(user:portray/1).
% user:portray





%:- set_prolog_flag(verbose_load,full).
:- set_prolog_flag(verbose,normal).
%:- set_prolog_flag(verbose_autoload,true).



% debug_var(_A,_Var):-!.
debug_var(X,Y):- notrace(catch(debug_var0(X,Y),_,fail)) -> true ; rtrace(debug_var0(X,Y)).

maybe_debug_var(X,Y):- notrace(maybe_debug_var0(X,Y)).
maybe_debug_var0(_,Y):- nonvar(Y),!.
maybe_debug_var0(X,_):- get_var_name(X,_),!.
maybe_debug_var0(X,Y):- (catch(debug_var0(X,Y),_,fail)) -> true ; rtrace(debug_var0(X,Y)).

debug_var(Sufix,X,Y):- notrace((flatten([X,Sufix],XS),debug_var(XS,Y))).

p_n_atom(Cmpd,UP):- sub_term(Atom,Cmpd),nonvar(Atom),\+ number(Atom), Atom\==[], catch(p_n_atom0(Atom,UP),_,fail),!.
p_n_atom(Cmpd,UP):- term_to_atom(Cmpd,Atom),p_n_atom0(Atom,UP),!.

filter_var_chars([58|X],[107, 119, 95|Y]):- filter_var_chars_trim_95(X,Y).
filter_var_chars([95|X],[95|Y]):- !, filter_var_chars_trim_95(X,Y).
filter_var_chars(X,Y):- filter_var_chars_trim_95(X,Y).



filter_var_chars_trim_95(X,Y):- filter_var_chars0(X,M),trim_95(M,Y),!.

trim_95([X],[X]).
trim_95([95|M],Y):-!, trim_95(M,Y).
trim_95([X|L],[100,X|Y]):- char_type(X,digit), trim_96(L,Y).
trim_95([X|L],[97,X|Y]):- \+ char_type(X,alpha), trim_96(L,Y).
trim_95(X,Y):- trim_96(X,Y).

trim_96([95],[]).
trim_96([],[]).
trim_96([95,95|M],Y):- trim_96([95|M],Y).
trim_96([X|M],[X|Y]):- trim_96(M,Y).



filter_var_chars0([],[]).


% WATN WHEN MAKING SYMBOLs...  `_` -> `__`

%  `-` -> `c45`
filter_var_chars0(`-`,`c45`):-!.
%  `*` -> `_xx_`
filter_var_chars0([42|T],[95,120,120,95|Rest]):-!,filter_var_chars0(T,Rest).
%  `%` -> `_pf_`
filter_var_chars0([37|T],[95,112, 102, 95| Rest]):-!,filter_var_chars0(T,Rest).
%  `-` -> `_`
filter_var_chars0([45|T],[95|Rest]):-!,filter_var_chars0(T,Rest).
%  `:` -> `_`
filter_var_chars0([42|T],[95,120,95|Rest]):-!,filter_var_chars0(T,Rest).
filter_var_chars0([H|T],[H|Rest]):-  code_type(H, prolog_identifier_continue),!,filter_var_chars0(T,Rest).
filter_var_chars0([H|T],Rest):- number_codes(H,Codes), filter_var_chars0(T,Mid),append([95, 99|Codes],[95|Mid],Rest).

atom_concat_some_left(L,R,LR):- atom_concat(L,R,LR),atom_length(R,Len),Len>0.
atom_concat_some_left(L,R,LR):- upcase_atom(L,L0),L\==L0,atom_concat(L0,R,LR),atom_length(R,Len),Len>0.
atom_concat_some_left(L,R,LR):- downcase_atom(L,L0),L\==L0,atom_concat(L0,R,LR),atom_length(R,Len),Len>0.

reduce_atomLR(L,R):- atom_concat_some_left('Cl_',LL,L),reduce_atomLR(LL,R).
reduce_atomLR(L,R):- atom_concat_some_left('U_',LL,L),reduce_atomLR(LL,R).
reduce_atomLR(L,R):- atom_concat_some_left('F_',LL,L),reduce_atomLR(LL,R).
reduce_atomLR(L,R):- atom_concat_some_left('Pf_',LL,L),reduce_atomLR(LL,R).
reduce_atomLR(L,R):- atom_concat_some_left('Kw_',LL,L),reduce_atomLR(LL,R).
reduce_atomLR(L,R):- atom_concat_some_left('Sys_',LL,L),reduce_atomLR(LL,R).
reduce_atomLR(L,L).

p_n_atom0(Atom,UP):- guess_textname(Atom,M),Atom\==M,!,p_n_atom0(M,UP).
p_n_atom0(Atom,UP):- atom(Atom),!,
  reduce_atomLR(Atom,AtomR),
  name(AtomR,[C|Was]),to_upper(C,U),filter_var_chars([U|Was],CS),name(UP,CS).
p_n_atom0(String,UP):- string(String),!,string_to_atom(String,Atom),!,p_n_atom0(Atom,UP).
p_n_atom0([C|S],UP):- !,notrace(catch(atom_codes(Atom,[C|S]),_,fail)),!,p_n_atom0(Atom,UP).

debug_var0(_,NonVar):-nonvar(NonVar),!.
debug_var0([C|S],Var):- notrace(catch(atom_codes(Atom,[C|S]),_,fail)),!,debug_var0(Atom,Var).
debug_var0([AtomI|Rest],Var):-!,maplist(p_n_atom,[AtomI|Rest],UPS),atomic_list_concat(UPS,NAME),debug_var0(NAME,Var),!.
debug_var0(Atom,Var):- p_n_atom(Atom,UP),  
  check_varname(UP),
  add_var_to_env_loco(UP,Var),!.


add_var_to_env_loco(UP,Var):- var(Var), get_var_name(Var,Prev),atomic(Prev),add_var_to_env_locovs_prev(UP,Prev,Var).
add_var_to_env_loco(UP,Var):-add_var_to_env(UP,Var).

add_var_to_env_locovs_prev(UP,Prev,_Var):- UP==Prev,!.
add_var_to_env_locovs_prev(UP,_Prev,_Var):- atom_concat_or_rtrace('_',_,UP),!.
add_var_to_env_locovs_prev(UP,_Prev,_Var):- atom_concat_or_rtrace(_,'_',UP),!.
add_var_to_env_locovs_prev(UP,_Prev,Var):-add_var_to_env(UP,Var).
add_var_to_env_locovs_prev(UP,Prev,Var):- atom_concat_or_rtrace('_',_,Prev),!,add_var_to_env(UP,Var).
add_var_to_env_locovs_prev(UP,Prev,Var):- atom_concat_or_rtrace(UP,Prev,New),add_var_to_env(New,Var).
add_var_to_env_locovs_prev(UP,_Prev,Var):- add_var_to_env(UP,Var).

check_varname(UP):- name(UP,[C|_]),(char_type(C,digit)->throw(check_varname(UP));true).
                        


resolve_char_codes('','_').
resolve_char_codes('pf','%').
%resolve_char_codes(C48,C):- notrace(catch((name(C48,[99|Codes]),number_codes(N,Codes),name(C,[N])),_,fail)),!,fail.
resolve_char_codes(C48,_):- notrace(catch((name(C48,[99|Codes]),number_codes(_,Codes)),_,fail)),!,fail.
resolve_char_codes(D1,N):- atom_concat('d',N,D1),notrace(catch(atom_number(N,_),_,fail)),!.
resolve_char_codes(C,CC):- atom_concat(C,'-',CC).

into_symbol_name(Atom,UPPER):- atomic(Atom),atomic_list_concat([Pkg|HC],'_',Atom),!,into_symbol_name([Pkg|HC],UPPER).
into_symbol_name(HC,UPPER):- maplist(resolve_char_codes,HC,RHC),atomics_to_string(RHC,'',STR),
   atom_trim_suffix(STR,'-',Trimed),string_upper(Trimed,UPPER),!.

% *PACKAGE* becomes xx_package_xx
% %MAKE-PACKAGE becomes pf_make_package

prologcase_name(I,O):-notrace(prologcase_name0(I,O)),assertion(O\=='').

prologcase_name0(String,Nonvar):-nonvar(Nonvar),!,prologcase_name(String,ProposedName),!,ProposedName==Nonvar.
prologcase_name0(String,ProposedName):- 
  string_lower(String,In),string_codes(In,Was),!,filter_var_chars(Was,CS),!,name(ProposedName,CS),!.


atom_concat_if_new(Prefix,Atom,NewAtom):-
  (atom_concat_or_rtrace(Prefix,_,Atom)-> NewAtom=Atom ; atom_concat_or_rtrace(Prefix,Atom,NewAtom)).


atom_trim_prefix(Root,Prefix,Result):- atom_concat(Prefix,Result,Root) -> true ; Result=Root.
atom_trim_suffix(Root,Suffix,Result):- atom_concat(Result,Suffix,Root) -> true ; Result=Root.

atom_concat_suffix('',Result,Result):-!.
atom_concat_suffix(Result,'',Result):-!.
atom_concat_suffix(Root,Suffix,Root):- atom_concat(_,Suffix,Root),!.
atom_concat_suffix(Root,Suffix,Result):- 
  atom_trim_prefix(Suffix,'_',Suffix2),
  atom_trim_suffix(Root,'_',Root2),
  atomic_list_concat([Root2,Suffix2],'_',Result),!.

shrink_lisp_strings(I,I).

make_pretty(I,O):- !,notrace((shrink_lisp_strings(I,O), make_pretty2(O))).
make_pretty2(O):- !,notrace((pretty1(O),pretty2(O),pretty3(O))).
%make_pretty(I,O):- is_user_output,!,shrink_lisp_strings(I,O), pretty1(O),pretty2(O),pretty3(O).
%make_pretty(I,O):- I=O, pretty1(O),pretty2(O),pretty3(O).

print_clause_plain(I):-
  current_prolog_flag(color_term, Was),
  make_pretty(I,O),
    setup_call_cleanup(set_prolog_flag(color_term, false),
     (nl,lcolormsg1((O))),
     set_prolog_flag(color_term, Was)).


%lcolormsg1(Msg):- mesg_color(Msg,Ctrl),!,ansicall_maybe(Ctrl,fmt9(Msg)).
lcolormsg1(Msg):- fmt9(Msg).

% print_clause_plain(C):- portray_clause_w_vars(O).


may_debug_var(_,_,V):- nonvar(V),!.
may_debug_var(_,_,V):- variable_name(V,_),!.
may_debug_var(L,_,_):- upcase_atom(L,L),!.
may_debug_var(L,R,V):- atom(L),atom_concat('f_',LL,L),may_debug_var(LL,R,V).
may_debug_var(L,R,V):- atomic_list_concat([_A1,A2,A3|AS],'_',L),atomic_list_concat([A2,A3|AS],'_',LL),may_debug_var(LL,R,V).
may_debug_var(L,R,V):- debug_var([L,R],V).

may_debug_var(_,V):- nonvar(V),!.
may_debug_var(_,V):- variable_name(V,_),!.
may_debug_var(R,V):- debug_var(R,V).

pretty1(H):- \+ compound(H),!.
pretty1(as_rest(Name, Rest, _)):- may_debug_var(Name,Rest).
pretty1(get_var(Env, Name, Val)):- may_debug_var('GEnv',Env),may_debug_var(Name,Val).
pretty1(deflexical(Env,_Op, Name, Val)):- may_debug_var('SEnv',Env),may_debug_var(Name,Val).
pretty1(set_var(Env,Name, Val)):- may_debug_var('SEnv',Env),may_debug_var(Name,Val).

pretty1(f_slot_value(_Env, Name, Val)):- may_debug_var(slot,Name,Val).
%pretty1(get_kw(ReplEnv, RestNKeys, test, test, f_eql, true, True)
pretty1(Env=[List|_]):- compound(List),var(Env),List=[H|_],compound(H),H=bv(_,_), may_debug_var('Env',Env),
  maplist(pretty1,List).
pretty1(Env=List):- compound(List),var(Env),List=[H|_],compound(H),H=bv(_,_), may_debug_var('Env',Env),
  maplist_not_tail(pretty1,List).
pretty1(P):- P=..[_,_|List],append(_,[Name, Val|_],List),atom(Name),var(Val),may_debug_var(Name,Val).
pretty1(debug_var(R,V)):- may_debug_var(R,V).
pretty1(bv(R,V)):- may_debug_var(R,V).
pretty1(H):-H=..[_|ARGS],must_maplist_det(pretty1,ARGS).


:- meta_predicate(maplist_not_tail(1,*)).
maplist_not_tail(_,ArgS):- var(ArgS),!.
maplist_not_tail(G,[X|ArgS]):-call(G,X),maplist_not_tail(G,ArgS).

pretty2(H):- \+ compound(H),!. % may_debug_var(F,'_Call',H).
%pretty2([H|T]):-!,maplist_not_tail(pretty2,[H|T]).
pretty2(H):-  
 dmust_det((functor(H,F,A),
   H=..[F,P1|ARGS],   
   (A>1 -> may_debug_var(F,'_Param',P1) ; true),
   must_maplist_det(pretty2,[P1|ARGS]))),!. 

pretty3(H):- \+ compound(H),!. % may_debug_var(F,'_Call',H).
pretty3(H):-pretty4(H),pretty5(H).

pretty4(H):- \+ compound(H),!. % may_debug_var(F,'_Call',H).
%pretty4([H|T]):-!,maplist_not_tail(pretty4,[H|T]).
pretty4(H):-  
 ignore(((functor(H,F,_), fail,
  nop((wl:init_args(N,F),integer(N),
   A is N + 1,   
   arg(A,H,R),may_debug_var('KeysNRest',R)))),
   H=..[F,P1|ARGS],  
   must_maplist_det(pretty4,[P1|ARGS]))),!. 

pretty5(H):- \+ compound(H),!. % may_debug_var(F,'_Call',H).
pretty5([H | B]):- pretty5(H),pretty5(B),may_debug_var('CAR',H),may_debug_var('CDR',B).
pretty5(H):-  
 dmust_det((functor(H,F,A),
   H=..[F,P1|ARGS],   
   arg(A,H,R),may_debug_var(F,'_Ret',R),   
   nop(may_debug_var(F,'_Param',P1)),
   must_maplist_det(pretty5,[P1|ARGS]))),!. 

atom_concat_or_rtrace(X,Y,Z):- tracing->atom_concat(X,Y,Z);catch(atom_concat(X,Y,Z),_,break).


:- export(i_name_lc/2).

%= 	 	 

%% i_name_lc( ?OType, ?IType) is semidet.
%
% Instance Name Not Loop Checked.
%
i_name_lc(OType,IType):-typename_to_iname0('',OType,IOType),!,string_equal_ci(IOType,IType).



%= 	 	 

%% to_iname( ?T, ?T) is semidet.
%
% Converted To Iname.
%
to_iname(T,TT):- var(T),!,freeze(T,to_iname(T,TT)).
to_iname(T,TT):- not(current_predicate(i_name/3)),!,T=TT.
%to_iname(T,TT):- (not_log_op(T),i_name(t,T,TT))->true;TT=T.



%= 	 	 

%% toUpperCamelcase( ?Type, ?TypeUC) is semidet.
%
% Converted To Upper Camelcase.
%
toUpperCamelcase(Type,TypeUC):-toCamelcase(Type,TypeUC). % ,toPropercase(TypeC,TypeUC),!.
:- export(i_name/2).


icn_tcn(I,IC):-atom(I),i_name('t',I,IC)->I\==IC.

%= 	 	 

%% i_name( ?OType, ?IType) is semidet.
%
% Instance Name.
%
i_name(OType,IType):-typename_to_iname0('',OType,IOType),!,IOType=IType.
:- export(i_name/3).

%= 	 	 

%% i_name( ?I, ?OType, ?IType) is semidet.
%
% Instance Name.
%
i_name(I,OType,IType):-typename_to_iname0(I,OType,IOType),!,IOType=IType.

:- export(typename_to_iname0/3).


%= 	 	 

%% typename_to_iname0( ?I, ?OType, ?IType) is semidet.
%
% Typename Converted To Iname Primary Helper.
%
typename_to_iname0(I, [], O):- trace_or_throw(bad_typename_to_iname0(I, [], O)).
%typename_to_iname0(I,OType,IType):- fail, (type_prefix(Prefix,_)),atom_concat(Prefix,Type,OType),capitalized(Type),!,typename_to_iname0(I,Type,IType).
typename_to_iname0(I,Type,IType):-nonvar(Type),toUpperCamelcase(Type,UType),atom_concat(I,UType,IType).

:- export(split_name_type/3).
:- '$hide'(split_name_type/3).

%= 	 	 

%% split_name_type( ?Suggest, ?InstName, ?Type) is semidet.
%
% Split Name Type.
%
split_name_type(Suggest,InstName,Type):- maybe_notrace(split_name_type_0(Suggest,NewInstName,NewType)),!,must((NewInstName=InstName,NewType=Type)),!.


%= 	 	 

%% split_name_type_0( ?S, ?P, ?C) is semidet.
%
% split name type  Primary Helper.
%
split_name_type_0(S,P,C):- string(S),!,atom_string(A,S),split_name_type_0(A,P,C),!.
%split_name_type_0(FT,FT,ttExpressionType):-a(ttExpressionType,FT),!,dmsg(trace_or_throw(ttExpressionType(FT))),fail.
split_name_type_0(T,T,C):- compound(T),functor(T,C,_),!.
split_name_type_0(T,T,C):- quietly((once(atomic_list_concat_safe([CO,'-'|_],T)),atom_string(C,CO))).
split_name_type_0(T,T,C):- quietly((atom(T),atom_codes(T,AC),last(AC,LC),is_digit(LC),append(Type,Digits,AC),
  catch(number_codes(_,Digits),_,fail),atom_codes(CC,Type),!,i_name(t,CC,C))).
split_name_type_0(C,P,C):- var(P),atom(C),i_name(i,C,I),gensym(I,P),!.





%= 	 	 

%% toCamelAtom0( :TermA, ?O) is semidet.
%
% Converted To Camel Atom Primary Helper.
%
toCamelAtom0([A],O):-nonvar(A),!,toPropercase(A,O),!.
toCamelAtom0([A|List],O):-!,toPropercase(A,AO),toCamelAtom0(List,LO),atom_concat(AO,LO,O).
toCamelAtom0(A,O):-toPropercase(A,O),!.



%= 	 	 

%% to_prefixed( ?Prefix, ?I, ?O) is semidet.
%
% Converted To Prefixed.
%
to_prefixed(Prefix,I,O):-to_atomic_name(I,i_name(Prefix),O).

:- meta_predicate to_atomic_name(?,2,?).

%= 	 	 

%% to_atomic_name( ?I, :PRED2Pred, ?O) is semidet.
%
% Converted To Atomic Name.
%
to_atomic_name(I,Pred,O):-is_list(I),toCamelAtom0(I,A),!,to_atomic_name(A,Pred,O).
to_atomic_name(I,Pred,O):-string(I),!,string_to_atom(I,A),!,to_atomic_name(A,Pred,O).
%to_atomic_name(Name,Pred,O):-atomic(Name),ereq(mudKeyword(W,KW)),string_equal_ci(Name,KW),!,to_atomic_name(W,Pred,O).
to_atomic_name(Name,Pred,_):- not(atom(Name)),!,trace_or_throw(todo(not_atom_to_atomic_name(Name,Pred))).
to_atomic_name(Name,Pred,O):- call(Pred,Name,O).


guess_textname(Name,Text):- guess_textname(Name,'',Text).
guess_textname(Name,Sep,Text):-atomic(Name),to_case_breaks(Name,ListN),to_case_breaks_trimed(Name,ListN,Sep,Text),!.

to_case_breaks_trimed(Name,[xti(TextL,ClassL),xti(TextR,ClassR)|ListN],Sep,Text):-  ClassL==ClassR,!,
    maplist(to_descriptive_name(Name),[xti(TextL,ClassL),xti(TextR,ClassR)|ListN],Desc),
    (string(Sep) -> atomics_to_string(Desc,Sep,Text) ; atomic_list_concat(Desc,Sep,Text)).

to_case_breaks_trimed(Name,[xti(_,lower),xti(TextR,ClassR)|ListN],Sep,Text):-
    maplist(to_descriptive_name(Name),[xti(TextR,ClassR)|ListN],Desc),
    (string(Sep) -> atomics_to_string(Desc,Sep,Text) ; atomic_list_concat(Desc,Sep,Text)).

to_case_breaks_trimed(Name,ListN,Sep,Text):- is_list(ListN),!,
    maplist(to_descriptive_name(Name),ListN,Desc),
    (string(Sep) -> atomics_to_string(Desc,Sep,Text) ; atomic_list_concat(Desc,Sep,Text)).



%to_descriptive_name(For,Desc,Atom):- type_descriptive_name(Type,Desc,Atom),isa(For,Type),!.
%to_descriptive_name(_For,Pefix,Desc):- (type_prefix(Pefix,TypeName)), guess_textname(TypeName,Desc).
%to_descriptive_name(For,xti(Pefix,lower),Desc):-!,to_descriptive_name(For,Pefix,Desc).
to_descriptive_name(For,xti(Pefix,_),Desc):-!,to_descriptive_name(For,Pefix,Desc).
to_descriptive_name(_For,X,X).

