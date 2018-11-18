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

% :- user:ensure_loaded(library(poor_bugger)).

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

dmust_tracing(G):- notrace((tracing,cls)),!,dmust(G).
dmust_tracing(G):- call(G).
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


%:- never_trace(lists:member(_,_)).
%:- never_trace(lists:append(_,_,_)).
dshow_call((G1,G2)):- !,dshow_fail(G1),dshow_fail(G2).
dshow_call(G):- simplify_dbug(G,GG), swi_soft_if_then(G,bugout(success_dshow_call(GG)),(bugout(failed_dshow_call(GG)),!,fail)).

dshow_fail('\\+'(G1)):- !, \+ dshow_true(G1).
dshow_fail(G):- simplify_dbug(G,GG), swi_soft_if_then(G, true , (bugout(failed_dshow_call(GG)),!,fail)).

dshow_true('\\+'(G1)):- !, \+ dshow_fail(G1).
dshow_true(G):- simplify_dbug(G,GG),  swi_soft_if_then(G, bugout(success_dshow_call(GG)) , (!,fail)).

found_bug(S0,open_list(Open)) :- \+is_list(S0),
  get_open_segement(S0,Open).
found_bug(S0,duplicated_object(X,R,L)) :-
 append(Left,[prop(X,R)|_],S0),
 member(prop(X,L),Left).

get_open_segement(S0,Open):- append(Left,_,S0),is_list(Left),length(Left,N),N>2,!,append([_,_],S1,S0),get_open_segement(S1,Open).
get_open_segement(S0,S0).


check4bugs(S0) :- found_bug(S0,Bug),throw(check4bugs_failed(Bug)).
check4bugs(_S0) :-
 !, true.
check4bugs(S0) :-
 % TODO: emergency save of S0, either here or better yet, in a catch().
 throw(check4bugs_failed(S0)).

  
:- multifile(user:portray/1).
:- dynamic(user:portray/1).
:- discontiguous(user:portray/1).
% user:portray


