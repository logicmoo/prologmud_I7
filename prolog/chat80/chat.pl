/* @(#)chat.pl	24.1 2/23/88 */
% load.pl : Load Chat-80, for Quintus Prolog

/*
 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

/*
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/

% This file compiles all of Chat-80

/* SWI-Prolog modifications:

   - include library Quintus for enhanced compatibility
   - put discontiguous between brackets
   - rename plus/3 and index/1 to be ix_plus; my_index
   - remove last/2: system predicate with equivalent definition.
*/

:- use_module(library(quintus), [no_style_check/1]).
:- op(1150, fx, [(mode), (public)]).

:- no_style_check(single_var).
:- no_style_check((discontiguous)).

ttynl:- format('~N'),flush_output.

:- user:ensure_loaded((.. / parser_sharing)).	% misc


%:- ensure_loaded(als_chat).	% misc
:- op(400, xfy, '&').
:- op(400, xfy, '--').

:- consult(chatops).

:- consult(readin).		% sentence input, ASCII VERSION
:- consult(ptree).		% print trees
:- consult(xgrun).		% XG runtimes

:- consult(xgproc).             % XG generator

:- load_plus_xg_file(chat80,'chat80/clone.xg').
:- load_plus_xg_file(chat80,'chat80/lex.xg').

% :- compile_xg_clauses.
% :- consult(newg).		% clone + lex


:- consult(clotab).		% attachment tables
:- consult(newdic).		% syntactic dictionary
:- consult(slots).		% fits arguments into predicates
:- consult(scopes).		% quantification and scoping
:- consult(templa).		% semantic dictionary
:- consult(qplan).		% query planning
:- consult(talkr).		% query evaluation
:- consult(ndtabl).		% relation info.
:- consult(aggreg).		% aggregation operators
:- consult(world0).		% geographic data base
:- consult(rivers).
:- consult(cities).
:- consult(countr).
:- consult(contai).
:- consult(border).
:- consult(chattop).		% top level control


save_chat :-
   qsave_program(chat, [goal(hi)]).

