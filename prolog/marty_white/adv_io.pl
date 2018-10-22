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
% Marty's Tokenizer/Scanner/Lexer, written in Prolog.
:- module(adv_io,[
   read_line_to_tokens/4,
   clear_overwritten_chars/1,
   redraw_prompt/1,
   player_format/2,
   player_format/3,
   bugout/2,
   bugout/3,

   pprint/2,
   init_logging/0,
   bug/1,
   agent_to_input/2,
   get_overwritten_chars/2,
   restore_overwritten_chars/1,
   setup_console/0,setup_console/1]).


user:setup_console :- current_input(In),setup_console(In).

:- dynamic(adv:has_setup_setup_console/1).

setup_console(In):- adv:has_setup_setup_console(In),!.
setup_console(In):-
  
   assert(adv:has_setup_setup_console(In)),
   set_prolog_flag(color_term, true),
   ensure_loaded(library(prolog_history)),
   (current_prolog_flag(readline,X)-> ensure_loaded(library(X));ensure_loaded(library(editline))),
   %ensure_loaded(library(editline)),
    '$toplevel':(
          
     setup_colors,
     setup_history,
     setup_readline),!.


:- dynamic(adv:input_log/1).
init_logging :-
  get_time(StartTime),
  convert_time(StartTime, StartTimeString),
  open('input.log', append, FH),
  format(FH, '\n==== ADVENTURE INPUT, ~w\n', [StartTimeString]),
  asserta(adv:input_log(FH)).

:- dynamic(bugs/1). % Types of logging output.
%bugs([general, printer, planner, autonomous]).
bugs([general, planner, autonomous, telnet]).
%bugs([general, autonomous]).
bug(B) :-
  bugs(L),
  member(B, L).

bugout(A, B) :-
  bug(B),
  !,
  dbug(B:A).
bugout(_, _).

bugout(A, L, B) :-
  bug(B),
  !,
  dmust(maplist(simplify_dbug, L, LA)),
  ansi_format([fg(cyan)], '~N% ', []),
  ansi_format([fg(cyan)], A, LA),
  dmust((console_player(Player),redraw_prompt(Player))),!.
bugout(_, _, _).


:- export(simplify_dbug/2).
simplify_dbug(G,GG):- \+ compound(G),!,GG=G.
simplify_dbug({O},{O}):- !.
simplify_dbug(List,O):-
 ( is_list(List) -> clip_cons(List,'...'(_),O) ;
   ( List = [_|_], append(LeftSide,Open,List),
     Open \= [_|_], !, assertion(is_list(LeftSide)),
   clip_cons(LeftSide,'...'(Open),O))).
simplify_dbug(G,GG):- compound_name_arguments(G,F,GL), F\==sense_props, !,
  maplist(simplify_dbug,GL,GGL),!,compound_name_arguments(GG,F,GGL).
simplify_dbug(G,G).
is_state_list(G,_):- \+ compound(G),!,fail.
is_state_list([G1|_],{GG,'...'}):- compound(G1),G1=structure_label(GG),!.
is_state_list([_|G],GG):- is_state_list(G,GG).
clip_cons(G,GG):- is_state_list(G,GG),!.
clip_cons(List,ClipTail,{Len,LeftS,ClipTail}):- 
   length(List,Len),
   MaxLen = 5, Len>MaxLen,
   length(Left,MaxLen),
   append(Left,_,List),!,
   maplist(simplify_dbug,Left,LeftS).
clip_cons(Left,_,List):-maplist(simplify_dbug,Left,List).


pprint(Term, B) :-
  bug(B),
  !,
  player_format('~N~@~N',[prolog_pretty_print:print_term(Term, [output(current_output)])]),!.
pprint(_, _).

/*
redraw_prompt(Agent):- (Agent == 'floyd~1'),

redraw_prompt(_Agent):- 
 % console_player(Player),
   current_player(Player),
   player_format(Player,'~w@spatial> ',[Player]),!.
*/
redraw_prompt(Agent):- (Agent \== 'floyd~1'),!, 
  player_format(Agent,'~w@spatial> ',[Agent]),!.
redraw_prompt(_Agent).

player_format(Fmt,List):-
  current_player(Agent) ->
  notrace(player_format(Agent, Fmt,List)).

player_format(Agent,Fmt,List):-
  agent_output(Agent,OutStream),
  dmust(format(OutStream,Fmt,List)),!.
player_format(_, Fmt,List):- dmust(format(Fmt,List)).


agent_output(Agent,OutStream):- 
  adv:console_info(_Id,_Alias,_InStream,OutStream,_Host,_Peer, Agent).







identifer_code(Char) :- char_type(Char, csym).
identifer_code(Char) :- char_type(Char,to_lower('~')).
identifer_code(Char) :- memberchk(Char, `-'`).

punct_code(Punct) :- memberchk(Punct, `,.?;:!&\"`), !.
punct_code(Punct) :- \+ identifer_code(Punct), char_type(Punct, graph).

% -- Split a list of chars into a leading identifier and the rest.
% Fails if list does not start with a valid identifier.
identifier([-1|_String], _, _) :- !, fail. % char_type pukes on -1 (EOF)
identifier([Char|String], [Char|Tail], Rest) :-
  identifer_code(Char),
  identifier1(String, Tail, Rest).

identifier1(String, Id, Rest) :-
  identifier(String, Id, Rest), !.
identifier1(String, [], String).

% -- Split a list of chars into a leading token and the rest.
% Fails if list does not start with a valid token.
token(String, Token, Rest) :-
  identifier(String, Token, Rest), !.  % Is it an identifier?
%token(String,id(Atom),Rest) :-
%  identifier(String, Text, Rest), !, atom_codes(Atom,Text).
token([Punct|Rest], [Punct], Rest) :-
  %char_type(Punct, punct), !.  % Is it a single char token?
  punct_code(Punct), !. 

% -- Completely tokenize a string.
% Ignores unrecognized characters.
tokenize([],[]) :- !.
tokenize([-1],[`quit`]) :- !.
tokenize(String, [Token|Rest]) :-
  token(String, Token, Tail),
  !,
  tokenize(Tail, Rest).
tokenize([_BadChar|Tail], Rest) :-
  !,
  tokenize(Tail, Rest).

log_codes([-1]).
log_codes(LineCodes) :-
  ignore(notrace(catch((atom_codes(Line, LineCodes),
  adv:input_log(FH),
  format(FH, '>~w\n', [Line])),_,true))).


%!  skip_to_nl(+Input) is det.
%
%   Read input after the term. Skips   white  space and %... comment
%   until the end of the line or a non-blank character.

skip_to_nl(In) :-
    repeat,
    peek_char(In, C),
    (   C == '%'
    ->  skip(In, '\n')
    ;   char_type(C, space)
    ->  get_char(In, _),
        C == '\n'
    ;   true
    ),
    !.

% -- Input from stdin, convert to a list of atom-tokens.

read_line_to_tokens(_Agent,In,Prev,Tokens):-  
 stream_property(In,tty(Was)),
 setup_call_cleanup((  
  set_stream(In, tty(true))),
  ((setup_console(In),
    %skip_to_nl(In),
     New = '',
    % format(atom(New),'~w@spatial> ',[Agent]),
     setup_call_cleanup(prompt(Old,New),
     read_line_to_codes(In,LineCodesR),
     prompt(_,Old)), 
    read_pending_input(In,_,[]),
    append(Prev,LineCodesR,LineCodes),
    NegOne is -1,     
    dmust(line_to_tokens(LineCodes,NegOne,Tokens0)),!,
    dmust(Tokens0=Tokens))),
  set_stream(In, tty(Was))),!.

line_to_tokens([],_,[]):-!.
line_to_tokens(NegOne,NegOne,[quit]):-!.
line_to_tokens([NegOne],NegOne,[quit]):-!.
line_to_tokens(LineCodes,_NegOne,Tokens) :- 
    append(_NewLineCodes,[L],LineCodes),
    member(L,[46]),read_term_from_codes(LineCodes,Term,
     [syntax_errors(fail),var_prefix(false),
        % variables(Vars),
        variable_names(_VNs),cycles(true),dotlists(true),singletons(_)]),
    Term=..Tokens,!.
line_to_tokens(LineCodes,NegOne,Tokens) :- 
  append(NewLineCodes,[L],LineCodes),
  member(L,[10,13,32,46]),!,
  line_to_tokens(NewLineCodes,NegOne,Tokens).
line_to_tokens(LineCodes,_,Tokens):- 
  ignore(log_codes(LineCodes)),!,
  tokenize(LineCodes, TokenCodes),!,
  % Convert list of list of codes to list of atoms:
  findall(Atom, (member(Codes, TokenCodes), atom_codes(Atom, Codes)), Tokens),  
  save_to_history(LineCodes),
  !.

:- multifile(prolog:history/2).
save_to_history(LineCodes):-   
  ignore(notrace((
  atom_string(AtomLineCodes,LineCodes), 
  current_input(In),
  % dmsg(LineCodes->AtomLineCodes),
  ignore(catch('$save_history_line'(AtomLineCodes),_,true)),
  ignore(catch(prolog:history(user_input, add(AtomLineCodes)), _, true)),
  ignore(catch(prolog:history(In, add(AtomLineCodes)), _, true))))).


:- dynamic(overwritten_chars/2).

add_pending_input(Agent,C):- agent_to_input(Agent,In),add_pending_input0(In,C).
add_pending_input0(In,C):- retract(overwritten_chars(In,SoFar)),append(SoFar,[C],New),!,assert(overwritten_chars(In,New)).
add_pending_input0(In,C):- assert(overwritten_chars(In,[C])).

clear_overwritten_chars(Agent):- agent_to_input(Agent,In),retractall(overwritten_chars(In,_SoFar)).
restore_overwritten_chars(Agent):- agent_to_input(Agent,In),overwritten_chars(In,SoFar),format('~s',[SoFar]).

% agent_to_input(Agent,In):- overwritten_chars(Agent,_SoFar),In=Agent,
agent_to_input(Agent,In):- adv:console_info(_Id,_Alias,In,_OutStream,_Host, _Peer, Agent),!.
% agent_to_input(Agent,In):- stream_or_alias(In,Alias), stream_property(Agent,file_no(F)),stream_property(In,file_no(F)),stream_property(In,read),!.
agent_to_input(_Agent,In):- current_input(In).

user:bi:- agent_to_input('telnet~1',In),
   forall(stream_property(In,P),dbug(ins(P))),
   %line_position(In,LIn),
   %dbug(ins(line_position(In,LIn))),
   forall(stream_property('telnet~1',P),dbug(outs(P))),listing(overwritten_chars),
   line_position('telnet~1',LInOut),!,
   dbug(outs(line_position('telnet~1',LInOut))),!.

get_overwritten_chars(Agent,Chars):- agent_to_input(Agent,In),overwritten_chars(In,Chars).
get_overwritten_chars(_Agent,[]).


wordlist(List) --> optional_ws, wordlist1(List), optional_ws.
optional_ws --> whitespace.
optional_ws --> {true}.
wordlist1(List) --> wordlist2(List).
wordlist1([]) --> {true}.
wordlist2([X|Y]) --> word(X), whitespace, wordlist2(Y).
wordlist2([X]) --> word(X).
%wordlist([X|Y]) --> word(X), whitespace, wordlist(Y).
%wordlist([X]) --> whitespace, wordlist(X).
%wordlist([X]) --> word(X).
%wordlist([X]) --> word(X), whitespace.

%word(W) --> charlist(X), {name(W,X)}.
word(W) --> charlist(X), {atom_codes(W,X)}.

charlist([X|Y]) --> chr(X), charlist(Y).
charlist([X]) --> chr(X).

chr(X) --> [X], {X>=48}.

whitespace --> whsp, whitespace.
whitespace --> whsp.

whsp --> [X], {X<48}.

:- initialization(setup_console,program).

:- initialization(setup_console,restore).
