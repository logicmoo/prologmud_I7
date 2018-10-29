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

give_help(A/B) :- !,
        online_help:predicate(A, B, _, C, D), !,
        show_help(A/B, [C-D]).
give_help(A) :-
        online_help:user_index(B, A), !,
        online_help:section(B, _, C, D),
        show_help(A, [C-D]).
give_help(A) :-
        atom(A),
        atom_concat('PL_', _, A),
        function(A, B, C), !,
        show_help(A, [B-C]).
give_help(A) :-
        findall(B-C,
                online_help:predicate(A, _, _, B, C),
                D),
        D\==[], !,
        show_help(A, D).
give_help(A) :-
        format('No help available for ~w~n', [A]).

show_help(C, B) :-
  online_help:(
        predicate_property(prolog:show_help_hook(_, _),
                           number_of_clauses(A)),
        A>0,
        write_ranges_to_file(B, D),
        prolog:show_help_hook(C, D)).

show_help(_, A) :-
      online_help:
      ( current_prolog_flag(pipe, true), !,
        online_manual_stream(B),
        pager_stream(C),
        catch(show_ranges(A, B, C), _, true),
        close(B),
        catch(close(C), _, true)).
show_help(_, A) :-
  online_help:(
        online_manual_stream(B),
        show_ranges(A, B, user_output)).


