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

:- dynamic(adv:cmd_help/2).

add_help(Cmd,HelpStr):-
  assert(adv:cmd_help(Cmd,HelpStr)).

add_help_cmd_borked(Cmd):-
  with_output_to(string(HelpStr),help(Cmd)),
  assert(adv:cmd_help(Cmd,HelpStr)).


add_help_cmd(Cmd):-
  redirect_error_to_string(help(Cmd),HelpStr),
  assert(adv:cmd_help(Cmd,HelpStr)).


%:- nop(ensure_loaded('adv_chat80')).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CODE FILE SECTION
:- nop(ensure_loaded('adv_main_commands')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%printable_state(L,S):- sort(L,S).
printable_state(S,S).


meta_pprint(D,K):- pprint(D,K).

% do_metacmd(Doer, Action, S0, S1)
:- add_help(quit,"Quits the game.").
do_metacmd(Doer, quit, S0, S1) :-
  declare(wishes(Doer, quit), S0, S1),
  player_format('Bye!~n', []).

do_metacmd(_Doer, help, S0, S0) :- !,
  listing(adv:cmd_help).

:- add_help(rtrace,"Debbuging: Start the non-interactive tracer.").
do_metacmd(Doer, rtrace, S0, S0) :- security_of(Doer,admin), rtrace.

:- add_help(nortrace,"Debbuging: Stop the non-interactive tracer.").
do_metacmd(Doer, nortrace, S0, S0) :- security_of(Doer,admin), nortrace.

:- add_help(trace,"Debbuging: Start the interactive tracer.").
do_metacmd(Doer, trace, S0, S0) :- security_of(Doer,admin), trace.

:- add_help(notrace,"Debbuging: Stop the interactive tracer.").
do_metacmd(Doer, notrace, S0, S0) :- security_of(Doer,admin), notrace.

:- add_help_cmd(spy).
do_metacmd(Doer, spy(Pred), S0, S0) :- security_of(Doer,admin), spy(Pred).

:- add_help_cmd(nospy).
do_metacmd(Doer, nospy(Pred), S0, S0) :- security_of(Doer,admin), nospy(Pred).

:- add_help(possess(agent),"Take possession of a character").
do_metacmd(Doer, possess(NewAgent), S0, S0) :-
  security_of(Doer,wizard),
  retract(current_player(_Agent)),
  asserta(current_player(NewAgent)).
do_metacmd(Doer, Echo, S0, S0) :-
  security_of(Doer,admin),
  Echo =.. [echo|Args],
  player_format('~w~n', [Args]).
do_metacmd(Doer, state, S0, S0) :-
  security_of(Doer,wizard),
  printable_state(S0,S),
  meta_pprint(S, general),
  maybe_pause(Doer).
do_metacmd(Doer, props, S0, S0) :-
  security_of(Doer,wizard),
  printable_state(S0,S),
  include(@=<(props(_,_)),S,SP),
  reverse(SP,SPR),
  meta_pprint(SPR, general),
  maybe_pause(Doer).
do_metacmd(Doer, mem, S0, S0) :-
  security_of(Doer,wizard),
  printable_state(S0,S),
  include(@>=(props(_,_)),S,SP),
  reverse(SP,SPR),
  meta_pprint(SPR, general),
  maybe_pause(Doer).
do_metacmd(Doer, make, S0, S0) :-
  security_of(Doer,wizard),
  thread_signal(main,make).
do_metacmd(Doer, prolog, S0, S0) :-
  security_of(Doer,wizard),
  '$current_typein_module'(Was),
  setup_call_cleanup('$set_typein_module'(mu),prolog,'$set_typein_module'(Was)).

do_metacmd(Doer, CLS, S0, S0) :- security_of(Doer,wizard), 
  current_predicate(_, CLS), 
  (is_main_console -> catch(CLS,E,(dbug(CLS:- throw(E)),fail)) ;
    (redirect_error_to_string(catch(CLS,E,(dbug(CLS:- throw(E)),fail)),Str),!, write(Str))).

do_metacmd(Doer, memory(Agent), S0, S0) :-
  security_of(Doer,wizard),
  forall(member(memories(Agent, Memory), S0),
  meta_pprint(Memory, general)),
  maybe_pause(Doer).

do_metacmd(Doer, model(Agent), S0, S0) :-
  security_of(Doer,wizard),
  forall(member(memories(Agent, Memory), S0),
  forall(thought(model(ModelData), Memory),
   meta_pprint(ModelData, general))),
  maybe_pause(Doer).

do_metacmd(Doer, create(Object), S0, S1) :-
  security_of(Doer,wizard),
  current_player(Agent),
  related(Spatial, Prep, Agent, Here, S0),
  declare(h(Spatial, Prep, Object, Here), S0, S1),
  player_format('You now see a ~w.~n', [Object]).
do_metacmd(Doer, destroy(Object), S0, S1) :-
  security_of(Doer,wizard),
  undeclare(h(_Spatial, _, Object, _), S0, S1),
  player_format('It vanishes instantly.~n', []).
do_metacmd(Doer, AddProp, S0, S1) :-
  security_of(Doer,wizard),
  AddProp =.. [setprop, Object | Args],
  Args \= [],
  Prop =.. Args,
  setprop(Object, Prop, S0, S1),
  player_format('Properties of ~p now include ~w~n', [Object, Prop]).
do_metacmd(Doer, DelProp, S0, S1) :-
  security_of(Doer,wizard),
  DelProp =.. [delprop, Object | Args],
  Args \= [],
  Prop =.. Args,
  delprop(Object, Prop, S0, S1),
  player_format('Deleted.~n', []).
do_metacmd(Doer, properties(Object), S0, S0) :-
  security_of(Doer,wizard),
  (declared(props(Object, PropList), S0);declared(type_props(Object, PropList), S0)),!,
  player_format('Properties of ~p are now ~w~n', [Object, PropList]).
do_metacmd(Doer, undo, S0, S1) :-
  declare(wants(Doer,undo), S0, S1),
  player_format('undo...OK~nKO...odnu~n', []).
do_metacmd(_Doer, save(Basename), S0, S0) :-
  atom_concat(Basename, '.adv', Filename),
  save_term(Filename, S0).

do_metacmd(Doer, WA, S0, S1) :- 
   ((cmd_workarround(WA, WB) -> WB\==WA)), !, do_metacmd(Doer, WB, S0, S1).



