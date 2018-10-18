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

%:- nop(ensure_loaded('adv_chat80')).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CODE FILE SECTION
:- nop(ensure_loaded('adv_main_commands')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

meta_pprint(D,K):- pprint(D,K).

% do_metacmd(Action, S0, S1)
do_metacmd(quit, S0, S1) :-
  declare(quit, S0, S1),
  player_format('Bye!~n', []).
do_metacmd(trace, S0, S0) :- admin, trace.
do_metacmd( notrace, S0, S0) :- admin, notrace.
do_metacmd(spy(Pred), S0, S0) :- admin, spy(Pred).
do_metacmd(nospy(Pred), S0, S0) :- admin, nospy(Pred).
do_metacmd(possess(NewAgent), S0, S0) :-
  wizard,
  retract(current_player(_Agent)),
  asserta(current_player(NewAgent)).
do_metacmd(Echo, S0, S0) :-
  admin,
  Echo =.. [echo|Args],
  player_format('~w~n', [Args]).
do_metacmd(state, S0, S0) :-
  wizard,
  sort(S0,S),
  meta_pprint(S, general).
do_metacmd(make, S0, S0) :-
  wizard,
  thread_signal(main,make).
do_metacmd(prolog, S0, S0) :-
  wizard,
  prolog.
do_metacmd(CLS, S0, S0) :- wizard, current_predicate(_, CLS), call(CLS), !.
do_metacmd(memory(Agent), S0, S0) :-
  wizard,
  declared(memories(Agent, Memory), S0),
  meta_pprint(Memory, general).

do_metacmd(model(Spatial, Agent), S0, S0) :-
  wizard,
  declared(memories(Agent, Memory), S0),
  thought(model(Spatial, ModelData), Memory),
  meta_pprint(ModelData, general).

do_metacmd(model(Agent), S0, S0) :-
  wizard,
  declared(memories(Agent, Memory), S0),
  forall(thought(model(_Spatial, ModelData), Memory),
   meta_pprint(ModelData, general)).

do_metacmd(create(Object), S0, S1) :-
  wizard,
  current_player(Agent),
  related(Spatial, How, Agent, Here, S0),
  declare(h(Spatial, How, Object, Here), S0, S1),
  player_format('You now see a ~w.~n', [Object]).
do_metacmd(destroy(Object), S0, S1) :-
  wizard,
  undeclare(h(_Spatial, _, Object, _), S0, S1),
  player_format('It vanishes instantly.~n', []).
do_metacmd(AddProp, S0, S1) :-
  wizard,
  AddProp =.. [setprop, Object | Args],
  Args \= [],
  Prop =.. Args,
  setprop(Object, Prop, S0, S1),
  player_format('Properties of ~p now include ~w~n', [Object, Prop]).
do_metacmd(DelProp, S0, S1) :-
  wizard,
  DelProp =.. [delprop, Object | Args],
  Args \= [],
  Prop =.. Args,
  delprop(Object, Prop, S0, S1),
  player_format('Deleted.~n', []).
do_metacmd(properties(Object), S0, S0) :-
  wizard,
  declared(props(Object, PropList), S0),
  player_format('Properties of ~p are now ~w~n', [Object, PropList]).
do_metacmd(undo, S0, S1) :-
  declare(undo, S0, S1),
  player_format('undo...OK~nKO...odnu~n', []).
do_metacmd(save(Basename), S0, S0) :-
  atom_concat(Basename, '.adv', Filename),
  save_term(Filename, S0).
do_metacmd(WA, S0, S1) :- cmd_workarround(WA, WB) -> WB\==WA, !, do_metacmd(WB, S0, S1).



