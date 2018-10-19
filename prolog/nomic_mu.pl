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
  %make,
  use_module(library(editline)),
  ignore(notrace(catch(('$toplevel':setup_readline),_,true))),
  % ensure_loaded('./marty_white/adv_telnet'),
  adv_server(TwoSixSixSix),
  format('~NServer is starting on port ~w~n',[TwoSixSixSix]),
  % thread_create(adventure,_),!,
  threads,
  % set_stream(user_output,alias('player~1')),!,
  % set_stream(user_input,buffer_size(1)),  
  !.
  


srv_mu:-
  mu_port(TwoSixSixSix),
  srv_mu(TwoSixSixSix),
  run_mu.

run_mu:- dmust(adventure),!.


usage_mu:- format('~N
You may start the server with:

 ?- srv_mu.

',[]).


  

:- initialization(srv_mu, main).

:- initialization(usage_mu).

