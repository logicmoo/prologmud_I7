/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Bits and pieces:
%
%  LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
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

:- module(nomic_mu, [srv_mu/0,run_mu/0]).

:- ensure_loaded('./marty_white/adv_telnet').
:- ensure_loaded('./marty_white/adv_main').

mu_port(2666).

srv_mu(TwoSixSixSix) :-
  atom_concat('mu_',TwoSixSixSix,Alias),
  thread_property(_,alias(Alias)),!,  
  format('~NServer should be running on port ~w~n',[TwoSixSixSix]),
  threads, !.

srv_mu(TwoSixSixSix) :- 
  adv_server(TwoSixSixSix),
  format('~NServer is starting on port ~w~n',[TwoSixSixSix]),
  threads,
  !.
  
srv_mu:-
  mu_port(TwoSixSixSix),
  srv_mu(TwoSixSixSix),
  run_mu.

run_mu:- 
   setup_console,
   dmust(adventure),!.


usage_mu:- format('~N
You may start the server with:

 ?- srv_mu.

',[]).


  

:- initialization(srv_mu, main).

:- initialization(usage_mu).

