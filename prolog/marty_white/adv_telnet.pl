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

:- dynamic(adv:wants_quit/3).
:- dynamic(adv:console_info/7).
:- dynamic(adv:console_tokens/2).

:- use_module(library(socket)).

adv_server(Port) :-
 bugout(adv_server(Port)),
 tcp_socket(ServerSocket), 
 tcp_setopt(ServerSocket, reuseaddr), 
 tcp_bind(ServerSocket, Port), 
 tcp_listen(ServerSocket, 5), 
 atom_concat('mu_', Port, Alias),
 thread_create(adv_server_loop(Port, ServerSocket), _, 
   [ alias(Alias)
   ]).

peer_alias(Prefix,Peer, Host, Alias):- 
 (tcp_host_to_address(Host, Peer);Host=Peer),
 format(string(S),'~w@~w_',[Host,Prefix]),
 gensym(S,Alias),!.

adv_server_loop(Prefix, ServerSocket) :-
 tcp_accept(ServerSocket, Slave, Peer), 
 tcp_open_socket(Slave, InStream, OutStream), 
 %set_stream(InStream, buffer(false)), 
 set_stream(InStream, close_on_exec(true)), 
 set_stream(OutStream, close_on_exec(true)), 
 set_stream(InStream, close_on_abort(true)), 
 set_stream(OutStream, close_on_abort(true)), 
 peer_alias(Prefix, Peer, Host, Alias), 
 ignore(catch(thread_create(
  adv_serve_client(InStream, OutStream, Host, Peer, Alias), 
  _, 
  [ alias(Alias)
  ]), 
  error(permission_error(create, thread, Alias), _), 
  fail)), 
 !, 
 adv_server_loop(Prefix, ServerSocket).

setup_IO_props(InStream, OutStream):- 
 set_stream(InStream, tty(true)), 
 set_stream(OutStream, tty(true)), 
 % set_prolog_flag(tty_control, false), % JanW
 % set_prolog_flag(tty_control, true), % Dmiles
 current_prolog_flag(encoding, Enc), 
 set_stream(user_input, encoding(Enc)), 
 %set_stream(user_input, buffer(false)), 
 set_stream(user_output, encoding(Enc)), 
 %set_stream(user_error, encoding(Enc)), 
 set_stream(user_input, newline(detect)), 
 set_stream(user_output, newline(dos)), 
 set_stream(user_input, eof_action(eof_code)),!.

adv_serve_client(InStream, OutStream, Host, Peer, Alias) :- 
 !, 
 thread_self(Id), 

 set_prolog_IO(InStream, OutStream, OutStream),
 % set_stream(user_error, newline(dos)), 
 set_stream(OutStream, alias(user_error)), 
 set_stream(OutStream, alias(user_output)), 

 setup_IO_props(InStream, OutStream),

 set_stream(user_input, close_on_exec(false)),
 set_stream(user_input, close_on_abort(false)), 
 set_stream(user_output, close_on_exec(false)), 
 set_stream(user_output, close_on_abort(false)), 
 
 format(OutStream, 
  'Welcome to the SWI-Prolog Adventure Server!~n~q~n~n', 
  [adv_serve_client(Id,Alias,InStream,OutStream, Host, Peer)]), !, 
 call_cleanup(srv_catch(adventure_client_process(Id,Alias,InStream,OutStream, Host, Peer)), 
   adventure_client_cleanp(Id,Alias,InStream,OutStream)).
/*

 set_stream(InStream, tty(true)), 
 % set_prolog_flag(tty_control, false), 
 set_prolog_flag(tty_control, true), 

*/

/*
adv_server_client(InStream, OutStream, _, _):-
 thread_self(Id), 
 format(OutStream, 'Go away!!~n', []), 
 close(InStream), 
 close(OutStream), 
 thread_detach(Id).
*/

srv_catch(Goal):- catch(once(call(call,Goal)),E,((notrace(bugout(error_srv_catch(E,Goal))),!,fail))).
ignore_srv_catch(Goal):- ignore(srv_catch(Goal)).

adventure_client_cleanp(Id,Alias,InStream,OutStream):- 
 srv_catch((adv:console_info(Id,Alias,InStream,OutStream, Host, Peer, Agent) -> 
 ((assertz(adv:agent_discon(Agent)),
 bugout((adv:agent_discon(Agent))),
 stream_property(Err,file_no(2)),
 set_stream(Err,alias(Agent)),
 bugout(adventure_client_cleanp_agent(Id,Alias,InStream,OutStream, Host, Peer, Agent)))) ;
 bugout(failed_adventure_client_cleanp(Id,Alias,InStream,OutStream)))),
 retractall(adv:console_info(Id,Alias,InStream,OutStream, Host, Peer, Agent)),
 ignore_srv_catch(close(InStream)), 
 ignore_srv_catch(close(OutStream)),
 ignore_srv_catch(thread_detach(Id)).


:- dynamic(adv:peer_character/2).
:- dynamic(adv:peer_agent/2).
:- dynamic(adv:agent_character/2).
:- dynamic(adv:agent_discon/1).

guess_previous_agent_0(_, Peer, Agent):- adv:peer_agent(Peer, Agent),!.
guess_previous_agent_0(Host, _, Agent):- adv:peer_agent(Host, Agent),!.

guess_previous_agent(Host, Peer, Agent):- guess_previous_agent_0(Host, Peer, Agent),
 \+ adv:console_info(_Id,_Alias,_InStream,_OutStream, _Host, _Peer, Agent).

guess_previous_agent(_Host, _Peer, Agent):- gensym('telnet~',Agent).

prompt_for_agent(Id,Alias,InStream,OutStream, Host, Peer, Agent,Name):- 
 guess_previous_agent(Host, Peer, Agent), 
 ignore(adv:agent_character(Agent,Name)),
 ignore(adv:peer_character(Peer,Name)),
 ignore(adv:peer_character(Host,Name)),
 (var(Name) -> format(OutStream, 'Enter your name [or leave bank for "~w"]: ', [Agent]), read_line_to_string(InStream,Name) ; true),
 asserta_if_new(adv:agent_character(Agent,Name)),
 asserta_if_new(adv:peer_character(Peer,Name)),
 asserta_if_new(adv:peer_character(Host,Name)),
 asserta_if_new(adv:peer_agent(Peer,Agent)),
 asserta_if_new(adv:peer_agent(Host,Agent)),
 set_stream(user_output,alias(Agent)),
 asserta(adv:console_info(Id,Alias,InStream,OutStream, Host, Peer, Agent)), 
 assertz(adv:agent_conn(Agent,Name,Alias,adventure_client_process(Id,Alias,InStream,OutStream, Host, Peer))),!.

welcome_adv_tnet(OutStream):- 
  format(OutStream, '==============================================~n', []),
  format(OutStream, 'Welcome to Marty\'s Prolog Adventure Prototype~n', []),
  format(OutStream, '==============================================~n', []),
  !.

adventure_client_process(Id,Alias,InStream,OutStream, Host, Peer):- 
 prompt_for_agent(Id,Alias,InStream,OutStream, Host, Peer, Agent,_Name),
 retractall(adv:wants_quit(_,InStream,_)),
 retractall(adv:wants_quit(Id,_,_)),
 welcome_adv_tnet(OutStream),
 redraw_prompt(Agent),
 setup_console,
 repeat, 
 srv_catch(adv_tlnet_readloop(Id, InStream, Alias)),
 adv:wants_quit(_,InStream,_),!. 


tflush(OutStream):- ignore_srv_catch((flush_output(OutStream), ttyflush)).

adv_tlnet_readloop(Id, InStream, _Alias):- adv:wants_quit(Id, InStream, _Agent),!.

adv_tlnet_readloop(Id, InStream, Alias):- 
 adv:console_info(Id, Alias, InStream,_OutStream,_Host,_Peer, Agent),
 adv:console_tokens(Agent, _Words), sleep(0.1), !.

adv_tlnet_readloop(Id, InStream, Alias):- 
 srv_catch(adv:console_info(Id,Alias,InStream,OutStream, Host, Peer, Agent)), 
 tflush(OutStream),
 current_input(In), wait_for_input([In,InStream,user_input],Found,0.2),
 Found\==[], 
 %format(OutStream, '~N[~p: ~p] ==> ', [Alias, Agent]),
 tflush(OutStream),
 read_line_to_tokens(Agent,InStream,[],Words),
 tflush(OutStream),
 dmust(adv_tlnet_words(Id,Alias,InStream,OutStream, Host, Peer, Agent, Words)).


adv_tlnet_words(_Id,_Alias,_InStream,_OutStream, _Host, _Peer, _Agent, [prolog]):- !, prolog.
adv_tlnet_words(_Id,_Alias,_InStream,_OutStream, _Host, _Peer, _Agent, ['You'|_]):- !, trace,prolog.

adv_tlnet_words(Id,Alias,InStream,OutStream, Host, Peer, Agent, [quit]):-
 nop(adv_tlnet_words(Id,Alias,InStream,OutStream, Host, Peer, Agent)),
 asserta(adv:wants_quit(Id, InStream, Agent)).
adv_tlnet_words(Id,Alias,InStream,OutStream, Host, Peer, Agent, end_of_file):-
 nop(adv_tlnet_words(Id,Alias,InStream,OutStream, Host, Peer, Agent)),
 asserta(adv:wants_quit(Id, InStream, Agent)).

adv_tlnet_words(Id,Alias,InStream,OutStream, Host, Peer, Agent, Words0):-
 nop(adv_tlnet_words(Id,Alias,InStream,OutStream, Host, Peer, Agent, Words0)),
 (Words0==[]->Words=[wait];Words=Words0),
 nop((bugout('~NTelent: ~q~n', [adv:console_tokens(Agent, Words)]))), 
 assertz(adv:console_tokens(Agent, Words)),
 nop((format(OutStream, '~NYou: ~q~n', [adv:console_tokens(Agent, Words)]))), 
 !.

