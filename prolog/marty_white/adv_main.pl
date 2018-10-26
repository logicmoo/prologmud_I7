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
%:- use_module(library(pfc)).

admin :- true.  % Potential security hazzard.
wizard :- true. % Potential to really muck up game.
extra :-  true. % Fuller, but questionable if needed yet.

:- op(200,fx,'$').

:- user:ensure_loaded((.. / parser_sharing)).
:- consult(adv_debug).
:- consult(adv_util).
:- consult(adv_io).

:- consult(adv_model).
:- consult(adv_percept).

:- consult(adv_inst).
:- consult(adv_edit).

:- consult(adv_action).
:- consult(adv_agent).
:- consult(adv_eng2cmd).
:- consult(adv_floyd).
:- consult(adv_log2eng).
:- consult(adv_physics).
:- consult(adv_plan).
:- consult(adv_state).
:- consult(adv_data).

%:- consult(adv_test).
%:- consult(adv_telnet).


:- export(console_player/1).
console_player(Agent):-
  current_input(InStream),
  adv:console_info(_Id, _Alias, InStream, _OutStream, _Host, _Peer, Agent),!.
console_player(Agent):-
  Agent = 'player~1',
  (( \+ adv:console_info(_Id, _Alias, _InStream, _OutStream, _Host, _Peer, Agent))).

:- thread_local(adv:current_agent/1).
current_player(Agent):- adv:current_agent(Agent),!.
current_player(Agent):- thread_self(Id),adv:console_info(Id,_Alias,_InStream,_OutStream,_Host,_Peer, Agent).
current_player('player~1').
:- export(current_player/1).


adventure_init :-
  %guitracer,
 dmust((
  test_ordering,
  init_logging,
  (retractall(advstate(_));true),
  istate(S0),
  init_objects(S0, S1),
  %each_live_agent(must_act(look), S1, S3),
  asserta(advstate(S1)))), !,
   player_format('=============================================~n', []),
   player_format('INIT STATE~n', []),
   player_format('=============================================~n', []),
   printable_state(S1,SP), 
   pprint(SP, general),!.


adventure:- 
   adventure_init,
   player_format('=============================================~n', []),
   player_format('Welcome to Marty\'s Prolog Adventure Prototype~n', []),
   player_format('=============================================~n', []),  
  % trace,  
  mainloop,
  %main_loop(S3),
  stop_logging.

adventure :-
  stop_logging,
  player_format('adventure FAILED~n', []),
  !, fail.        


main(S0, S9) :-
  notrace((nb_setval(advstate,S0))),
  update_telnet_clients(S0,S1),
  ((nb_setval(advstate,S1),
  % pprint(S1,general),
  get_live_agents(LiveAgents, S1),
  ttyflush)),
  %dbug(liveAgents = LiveAgents),
  apply_all(LiveAgents, run_agent_pass_1(), S1, S2),
  apply_all(LiveAgents, run_agent_pass_2(), S2, S9),
  notrace((nb_setval(advstate,S9))),
  !. % Don't allow future failure to redo main.
main(S0, S0) :-
  bugout('main FAILED~n', general).

:- dynamic(adv:agent_conn/4).

update_telnet_clients(S0,S2):-
   retract(adv:agent_conn(Agent,Named,_Alias,Info)),
   create_agent_conn(Agent,Named,Info,S0,S1),
   update_telnet_clients(S1,S2).
update_telnet_clients(S0,S0).



:- dynamic(adv:console_tokens/2).
telnet_decide_action(Agent, Mem0, Mem0):-
  % If actions are queued, no further thinking required.
  thought(todo([Action|_]), Mem0),
  (declared(h(_Spatial, in, Agent, Here), Mem0)->true;Here=somewhere),
  bugout('~w @ ~w telnet: Already about to: ~w~n', [Agent, Here, Action], telnet).

telnet_decide_action(Agent, Mem0, Mem1) :-
  %dmust(thought(timestamp(T0), Mem0)),
  retract(adv:console_tokens(Agent, Words)), !,
  dmust((parse(Words, Action, Mem0),
  nop(bugout('Telnet TODO ~p~n', [Agent: Words->Action], telnet)),
  add_todo(Action, Mem0, Mem1))), !.
telnet_decide_action(Agent, Mem, Mem) :-
  nop(bugout('~w: Can\'t think of anything to do.~n', [Agent], telnet)).


%:- if(\+ prolog_load_context(reloading, t)).
:- initialization(adventure, main).
%:- endif.

main_once:- 
    dmust((
          retract(advstate(S0)),
          main(S0, S1),
          asserta(advstate(S1)),
          must_output_state(S1))
    ),!.

mainloop :-
  repeat,
    once(main_once),
    (advstate(S1)->declared(quit, S1)),
  !. % Don't allow future failure to redo mainloop.

% TODO: try converting this to a true "repeat" loop.
/*main_loop(State) :-
  declared(quit, State), !.
main_loop(State) :-
  declared(undo, State),
  current_player(Player),
  retract(undo(Player, [_, Prev|Tail])),
  assertz(undo(Player, Tail)),
  !,
  main_loop(Prev).
main_loop(S0) :-
  %repeat,
  current_player(Player),
  retract(undo(Player, [U1, U2, U3, U4, U5, U6|_])),
  assertz(undo(Player, [S0, U1, U2, U3, U4, U5, U6])),
  run_agent(Player, S0, S4),
  run_agent(floyd, S4, S5),
  %user_interact(S3, S4), !,
  %automate_agent(floyd, S4, S5),
  !,
  main_loop(S5).
main_loop(_) :-
  bugout('main_loop() FAILED!~n', general).
*/


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CODE FILE SECTION
:- nop(ensure_loaded('adv_main_commands')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


save_term(Filename, Term) :-
  \+ access_file(Filename, exist),
  open(Filename, write, FH),
  write(FH, Term),
  close(FH),
  player_format('Saved to file "~w".~n', [Filename]).
save_term(Filename, _) :-
  access_file(Filename, exist),
  player_format('Save FAILED! Does file "~w" already exist?~n', [Filename]).
save_term(Filename, _) :-
  player_format('Failed to state(Spatial, open, t) file "~w" for saving.~n', [Filename]).

