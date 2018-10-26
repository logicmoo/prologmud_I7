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
  redirect_error(help(Cmd),HelpStr),
  assert(adv:cmd_help(Cmd,HelpStr)).


%:- nop(ensure_loaded('adv_chat80')).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CODE FILE SECTION
:- nop(ensure_loaded('adv_main_commands')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%printable_state(L,S):- sort(L,S).
printable_state(S,S).


current_error(Stream) :- 
        stream_property(Stream, alias(user_error)), !. % force det. 

set_error(Stream) :- 
        set_stream(Stream, alias(user_error)). 

redirect_error(Goal, String) :- 
        current_error(OldErr),
        new_memory_file(Handle),        
        setup_call_cleanup( 
            open_memory_file(Handle, write, Err),
            setup_call_cleanup( 
                set_error(Err),
                (once(Goal),
                   flush_output(Err)), 
                set_error(OldErr)), 
            close(Err)),
        memory_file_to_string(Handle,String).

meta_pprint(D,K):- pprint(D,K).

% do_metacmd(Action, S0, S1)
:- add_help(quit,"Quits the game.").
do_metacmd(quit, S0, S1) :-
  declare(quit, S0, S1),
  player_format('Bye!~n', []).

do_metacmd(help, S0, S0) :- !,
  listing(adv:cmd_help).

:- add_help(rtrace,"Debbuging: Start the non-interactive tracer.").
do_metacmd(rtrace, S0, S0) :- admin, rtrace.

:- add_help(nortrace,"Debbuging: Stop the non-interactive tracer.").
do_metacmd(nortrace, S0, S0) :- admin, nortrace.

:- add_help(trace,"Debbuging: Start the interactive tracer.").
do_metacmd(trace, S0, S0) :- admin, trace.

:- add_help(notrace,"Debbuging: Stop the interactive tracer.").
do_metacmd(notrace, S0, S0) :- admin, notrace.

:- add_help_cmd(spy).
do_metacmd(spy(Pred), S0, S0) :- admin, spy(Pred).

:- add_help_cmd(nospy).
do_metacmd(nospy(Pred), S0, S0) :- admin, nospy(Pred).

:- add_help(possess(agent),"Take possession of a character").
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
  printable_state(S0,S),
  meta_pprint(S, general).
do_metacmd(props, S0, S0) :-
  wizard,
  printable_state(S0,S),
  include(@=<(props(_,_)),S,SP),
  reverse(SP,SPR),
  meta_pprint(SPR, general).
do_metacmd(mem, S0, S0) :-
  wizard,
  printable_state(S0,S),
  include(@>=(props(_,_)),S,SP),
  reverse(SP,SPR),
  meta_pprint(SPR, general).
do_metacmd(make, S0, S0) :-
  wizard,
  thread_signal(main,make).
do_metacmd(prolog, S0, S0) :-
  wizard,
  prolog.

do_metacmd(CLS, S0, S0) :- wizard, 
  current_predicate(_, CLS), catch(CLS,_,fail), !.

do_metacmd(memory(Agent), S0, S0) :-
  wizard,
  declared(memories(Agent, Memory), S0),
  meta_pprint(Memory, general).

do_metacmd(model(Agent), S0, S0) :-
  wizard,
  declared(memories(Agent, Memory), S0),
  thought_model(ModelData, Memory),
  meta_pprint(ModelData, general).

do_metacmd(model(Agent), S0, S0) :-
  wizard,
  declared(memories(Agent, Memory), S0),
  forall(thought(model(ModelData), Memory),
   meta_pprint(ModelData, general)).

do_metacmd(create(Object), S0, S1) :-
  wizard,
  current_player(Agent),
  related(Spatial, Prep, Agent, Here, S0),
  declare(h(Spatial, Prep, Object, Here), S0, S1),
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
  (declared(props(Object, PropList), S0);declared(type_props(Object, PropList), S0)),!,
  player_format('Properties of ~p are now ~w~n', [Object, PropList]).
do_metacmd(undo, S0, S1) :-
  declare(undo, S0, S1),
  player_format('undo...OK~nKO...odnu~n', []).
do_metacmd(save(Basename), S0, S0) :-
  atom_concat(Basename, '.adv', Filename),
  save_term(Filename, S0).

do_metacmd(WA, S0, S1) :- 
   ((cmd_workarround(WA, WB) -> WB\==WA)), !, do_metacmd(WB, S0, S1).



