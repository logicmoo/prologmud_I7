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
   is_main_console/0,
   redraw_prompt/1,
   player_format/2,
   player_format/3,
   bugout/2,
   bugout/3,
   with_tty/2,

   wait_for_key/0,
                   wait_for_key/1,
                   keyboard_init/0,
   pprint/2,
   init_logging/0,
   stop_logging/0,
   bug/1,
   agent_to_input/2,
   agent_to_output/2,
   get_overwritten_chars/2,
   restore_overwritten_chars/1,
   setup_console/0,setup_console/1,

   current_error/1,set_error/1, redirect_error_to_string/2,
          messages_init/0,
          /*post_message/1,
          post_message/2,
          sv_message/2,
          svo_message/3,
          svi_message/3,
          svoi_message/4,*/
          more_prompt/0,
          ack_messages/0]).

:- dynamic(adv:wants_quit/3).
:- dynamic(adv:console_info/7).
:- dynamic(adv:console_tokens/2).


current_error(Stream) :- stream_property(Stream, alias(user_error)), !. % force det. 
current_error(Stream) :- stream_property(Stream, alias(current_error)), !. % force det. 
current_error(Stream) :- stream_property(Stream, file_no(2)), !. % force det. 
set_error(Stream) :- set_stream(Stream, alias(user_error)). 

:- meta_predicate redirect_error_to_string(0,-).
redirect_error_to_string(Goal, String) :- 
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


mutex_create_safe(M):- notrace(catch(mutex_create(M),_,true)).

messages_init:-
   mutex_create_safe(messages).

flag(N,V):-
  flag(N,_,V).

% FIXME - word wrap
% post_message(M):-
%    atom_length(M,N),N>72
post_message(M):-
   with_mutex(messages,(post_message_int(M),
                        flag(unacked_messages,_,1))),
   nop(request_screen_update(0,0,1,80)).

post_message(F,L):-
   format(atom(A),F,L),post_message(A).

post_message_int(M):-
   flag(line0,'',M),
   atom_length(M,L),
   flag(requested_cursor_row,_,0),
   flag(requested_cursor_col,_,L),!.
post_message_int(M):-
   \+(more_prompt),
   flag(line0,OL),
   atom_length(OL,OLL),
   atom_length(M,ML),
   OLL+ML=<70,
   concat_atom([OL,'  ',M],NL),
   flag(line0,_,NL),
   atom_length(NL,Len),
   flag(requested_cursor_row,_,0),
   flag(requested_cursor_col,_,Len),!.
post_message_int(M):-
   more_prompt,
   recordz(messages,M),!.
post_message_int(M):-
   flag(more_prompt,_,1),
   flag(line0,OL),
   atom_concat(OL,' [More]',NL),
   flag(line0,_,NL),!,
   post_message_int(M).

more_prompt:-flag(more_prompt,1).

ack_messages:-flag(unacked_messages,0).
ack_messages:-
   with_mutex(messages,(
      flag(line0,_,''),
      flag(unacked_messages,_,0),
      flag(more_prompt,_,0),
      flag(requested_cursor_row,_,0),
      flag(requested_cursor_col,_,0),
      gather_messages(L),
      (L=[];flag(unacked_messages,_,1),resend_messages(L))
   )),
   nop(request_screen_update(0,0,1,80)).

gather_messages([H|T]):-
   recorded(messages,H,R),
   erase(R),
   !,gather_messages(T).
gather_messages([]).

resend_messages([H|T]):-
   post_message_int(H),!,resend_messages(T).
resend_messages([]).

/*
sv_message(S,V):-
   care_about(S),
   sconj(S,SC),
   vconj(S,V,VC),
   post_message('~w ~w.',[SC,VC]),!.
sv_message(_,_).

svo_message(S,V,O):-
   care_about(S),care_about(O),
   sconj(S,SC),
   vconj(S,V,VC),
   oconj(S,O,OC),
   post_message('~w ~w ~w.',[SC,VC,OC]),!.
svo_message(S,V,_):-
   care_about(S), % not care_about(O),
   sconj(S,SC),
   vconj(S,V,VC),
   post_message('~w ~w it.',[SC,VC]),!.
svo_message(_,V,O):-
   care_about(O), % not care_about(S),
   vconj(it,V,VC),
   oconj(it,O,OC),
   post_message('It ~w ~w.',[VC,OC]),!.
svo_message(_,_,_).

svi_message(S,V,I):-
   care_about(S),
   sconj(S,SC),
   vconj(S,V,VC),
   post_message('~w ~w ~w.',[SC,VC,I]),!.
svi_message(_,_,_).

svoi_message(S,V,O,I):-
   care_about(S),care_about(O),
   sconj(S,SC),
   vconj(S,V,VC),
   oconj(S,O,OC),
   post_message('~w ~w ~w ~w.',[SC,VC,OC,I]),!.
svoi_message(S,V,_,I):-
   care_about(S), % not care_about(O),
   sconj(S,SC),
   vconj(S,V,VC),
   post_message('~w ~w it ~w.',[SC,VC,I]),!.
svoi_message(_,V,O,I):-
   care_about(O), % not care_about(S),
   vconj(it,V,VC),
   oconj(it,O,OC),
   post_message('It ~w ~w ~w.',[VC,OC,I]),!.
svoi_message(_,_,_,_).

care_about(player).
care_about(O):-
   attribute(player-dungeon,D),attribute(O-dungeon,D),
   attribute(player-level,L),attribute(O-level,L),
   attribute(O-row,R),attribute(O-column,C),
   visible(R,C),
log_stuff('viz~n',[]).
*/

:-dynamic key_translate/2.

%:-include(gheader).

%:-use_module(screen).
%:-use_module(messages).

% FIXME dynamify key translations

key_translate(['\014\'],redraw). 

key_translate(['\033\','[','A'],up).
key_translate(['\033\','[','B'],down).
key_translate(['\033\','[','C'],right).
key_translate(['\033\','[','D'],left).

key_translate(['\033\','[','5','~'],page_up).
key_translate(['\033\','[','6','~'],page_down).

key_translate(['\033\',K],meta(K)):-char_type(K,alnum).  %'

keyboard_thread(Buffer):-
   set_stream(user_input,buffer(none)),
   get_single_char(Code),char_code(Key,Code),
   append(Buffer,[Key],NB),
   !,process_buffer(NB).

process_buffer([]):-!,keyboard_thread([]).
process_buffer(NB):-
   key_translate(NNB,_),
   append(NB,X,NNB),X\=[],
   !,keyboard_thread(NB).
process_buffer(NB):-
   append(NBA,NBB,NB),
   key_translate(NBA,Key),
%   log_stuff('got key ~w.~n',[Key]),
   global_key_hook(Key),
   !,process_buffer(NBB).
process_buffer([K|T]):-
   nop(log_stuff('got key ~w.~n',[K])),global_key_hook(K),!,process_buffer(T).
/*
global_key_hook(redraw):-!,request_screen_update(redraw).
global_key_hook(meta(h)):-!,
   attribute(player-hit_points,H),
   HH is H+random(6)+1,
   attribute(player-hit_points,_,HH),
   request_screen_update(23,0,1,10).
global_key_hook(meta(t)):-!,threads.
global_key_hook(meta(l)):-
   (dungeon:player_dungeon(R,C,A,B),
    log_stuff('~w~n',[player_dungeon(R,C,A,B)]),fail;true),!.
*/
keystrokes_thread_name(keystrokes).

global_key_hook(Key):-
  keystrokes_thread_name(Keystrokes),
   with_mutex(messages,(
      nop(more_prompt),(Key=' ',nop(ack_messages);true);
      nop(ack_messages),thread_send_message(Keystrokes,Key)
   )),!.

keyboard_init:-
   keystrokes_thread_name(Keystrokes),
   (message_queue_property(_, alias(Keystrokes))->true;message_queue_create(Keystrokes)),
   thread_create(keyboard_thread([]),_,[]).

wait_for_key(Key):-
   keystrokes_thread_name(Keystrokes),
   thread_get_message(Keystrokes,Key).

wait_for_key:-wait_for_key(_).

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
init_logging :- !.
init_logging :-
  get_time(StartTime),
  convert_time(StartTime, StartTimeString),
  open('~/.nomic_mu_input.log', append, FH),
  format(FH, '\n==== ADVENTURE INPUT, ~w\n', [StartTimeString]),
  asserta(adv:input_log(FH)).
stop_logging :-
  adv:input_log(FH) -> close(FH) ; true.

:- dynamic(bugs/1). % Types of logging output.
%bugs([general, printer, planner, autonomous]).
bugs([always, general, planner, autonomous, telnet]).
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

                       
%:- set_stream(user_input,buffer_size(1)).
%:- set_stream(user_input,buffer(none)).
%:- set_stream(user_input,timeout(0.1)).


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
  player_format('~N~@~N',[our_pretty_printer(Term)]),!.
pprint(_, _).

our_pretty_printer(Term):- compound(Term),!, prolog_pretty_print:print_term(Term, [output(current_output)]).
our_pretty_printer(Term):- format(current_output,'~w',[Term]).
/*
redraw_prompt(Agent):- (Agent == 'floyd~1'),

redraw_prompt(_Agent):- 
 % console_player(Player),
   current_player(Player),
   player_format(Player,'~w@spatial> ',[Player]),!.
*/

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

redraw_prompt(Agent):- (Agent \== 'floyd~1'),!, 
  player_format(Agent,'~w@spatial> ',[Agent]),!.
redraw_prompt(_Agent).

player_format(Fmt,List):-
  current_player(Agent) ->
  notrace(player_format(Agent, Fmt,List)).

player_format(Agent,Fmt,List):-
  agent_to_output(Agent,OutStream),
  dmust(format(OutStream,Fmt,List)),!.
player_format(_, Fmt,List):- dmust(format(Fmt,List)).








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
log_codes(_) :- \+ adv:input_log(_),!.
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
    setup_console(In),
    with_tty(In,(read_line_to_codes(In,LineCodesR),read_pending_input(In,_,[]))),    
    append(Prev,LineCodesR,LineCodes),
    NegOne is -1,     
    dmust(line_to_tokens(LineCodes,NegOne,Tokens0)),!,
    dmust(Tokens0=Tokens).

:- meta_predicate with_tty(+,0).
with_tty(In,Goal):-  
 stream_property(In,tty(Was)),
 stream_property(In,timeout(TWas)), 
 New = '', % format(atom(New),'~w@spatial> ',[Agent]),
 setup_call_cleanup((  
  set_stream(In, tty(true)),set_stream(In, timeout(infinite))),    
     setup_call_cleanup(prompt(Old,New),
        (%skip_to_nl(In),
        Goal), prompt(_,Old)),
  (set_stream(In, timeout(TWas)),set_stream(In, tty(Was)))),!.

line_to_tokens([],_,[]):-!.
line_to_tokens(NegOne,NegOne,[quit]):-!.
line_to_tokens([NegOne],NegOne,[quit]):-!.
line_to_tokens(LineCodes,_NegOne,Tokens) :- 
    append(NewLineCodes,[L],LineCodes),
    member(L,[46]),
    catch((read_term_from_codes(NewLineCodes,Term,
     [syntax_errors(error),var_prefix(false),
        % variables(Vars),
        variable_names(_VNs),cycles(true),dotlists(true),singletons(_)])),_,fail),
    Tokens=[Term],!.
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

using_stream(Stream,OtherAgent):- adv:console_info(_Id,_Alias,_In,Stream,_Host, _Peer, OtherAgent).
using_stream(Stream,OtherAgent):- adv:console_info(_Id,_Alias,Stream,_Out,_Host, _Peer, OtherAgent).

agent_to_output(Agent, Stream):- adv:console_info(_Id,_Alias,_In,Stream,_Host, _Peer, Agent),!.
agent_to_output(_Agent,Stream):- current_output(Stream), \+ using_stream(Stream,_Other),!.
agent_to_output(_Agent,Stream):- stream_property(Stream, file_no(1)), \+ using_stream(Stream,_Other),!.
agent_to_output(Agent, Stream):- fail, agent_to_input(Agent,In), stream_property(In,file_no(F)),stream_property(Stream,file_no(F)),stream_property(Stream,write),!.
agent_to_output(Agent, Stream):- throw(agent_io(Agent,agent_to_output(Agent, Stream))).
                           
% agent_to_input(Agent,In):- overwritten_chars(Agent,_SoFar),In=Agent,
agent_to_input(Agent, Stream):- adv:console_info(_Id,_Alias,Stream,_Out,_Host, _Peer, Agent),!.
agent_to_input(_Agent,Stream):- current_input(Stream), \+ using_stream(Stream,_Other),!.
agent_to_input(_Agent,Stream):- stream_property(Stream, file_no(0)), \+ using_stream(Stream,_Other),!.
agent_to_input(Agent, Stream):- fail, agent_to_output(Agent,Stream), stream_property(Stream,file_no(F)),stream_property(Stream,file_no(F)),stream_property(Stream,read),!.
agent_to_input(Agent, Stream):- throw(agent_io(Agent,agent_to_input(Agent, Stream))).

is_main_console:- current_input(Stream), stream_property(Stream, file_no(0)).

user:ci:- ci('telnet~1').
user:ci(Agent):- 
   agent_to_input(Agent,In),
   agent_to_output(Agent,Out),
   forall(stream_property(In,P),dbug(ins(P))),
   listing(overwritten_chars),
   %line_position(In,LIn),
   %dbug(ins(line_position(In,LIn))),
   forall(stream_property(Out,P),dbug(outs(P))),
   line_position(Out,LInOut),!,dbug(outs(line_position(Out,LInOut))),!.

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
