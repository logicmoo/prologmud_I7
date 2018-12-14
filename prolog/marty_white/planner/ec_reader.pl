/*
% NomicMUD: A MUD server written in Prolog
%
% Some parts used Inform7, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
% 
% July 10, 1996 - John Eikenberry 
% Copyright (C) 2004 Marty White under the GNU GPL
% 
% Dec 13, 2035 - Douglas Miles
%
%
% Logicmoo Project changes:
%
% Main file.
%
*/
:- module(ec_reader,[process_ec/1]).

:- thread_local(t_l:each_file_term/1).
:- thread_local(t_l:block_comment_mode/1).
:- thread_local(t_l:echo_mode/1).

:- meta_predicate now_doing(1, ?).
:- meta_predicate each_doing(1, ?).
:- meta_predicate doing(1, *).
  
%:- use_module(library(logicmoo_startup)).
:- use_module(library(file_utils/filestreams)).

%:- initialization(ec_reader_test, main).

% expand_file_name(+WildCard, -List)

ec_reader_test :- ec_to_pl(do_ec_convert, user_output, 'ectest/*.e').

ect:- ec_load('examples/FrankEtAl2003/Story1.e').

ec_reader_testf :- covert_to_pl(outf, 'ectest/*.e').

ec_load(F):-  
  \+ etmp:ec_option(load(F), _),
  asserta(etmp:ec_option(load(F), ec_load)),
  ec_to_pl(do_ec_load, current_output, F).

covert_to_pl(Out, F):- ec_to_pl(do_ec_convert, Out, F).



:- thread_local(t_l:ec_options/2).


ec_to_pl(Why, Out, F):- dmsg(ec_to_pl(Why, Out, F)), fail.
ec_to_pl(Why, Out, F):- notrace(is_stream(F)), !, ec_in_to_pl(Why, Out, F).
ec_to_pl(Why, Out, F):- atom(F), (exists_file(F);is_absolute_file_name(F)), !, 
   setup_call_cleanup(open(F, read, Ins), 
     ec_in_to_pl(Why, Out, Ins),
     close(Ins)),!.
ec_to_pl(Why, Out, F):- atom(F), expand_file_name(F, L), L\==[], [F]\==L, !, maplist(ec_to_pl(Why, Out), L).
ec_to_pl(Why, Out, F):- 
   findall(N, absolute_file_name(F, N, [file_type(txt), file_errors(fail), expand(false), solutions(all)]), L), 
   L\=[F], !, maplist(ec_to_pl(Why, Out), L).
ec_to_pl(Why, Outs, In):- atom(In), 
  setup_call_cleanup(open(In, Ins), 
     ec_in_to_pl(Why, Outs, Ins), 
     close(Ins)).


ec_in_to_pl(Why, Out, F):- dmsg(ec_in_to_pl(Why, Out, F)), fail.
ec_in_to_pl(_, _, Ins):- retractall(last_s_l(_,_)),
  assertion(stream_property(Ins, input)), fail.
  
ec_in_to_pl(Why, Outs, Ins):- 
   atomic(Outs), is_stream(Outs), !, 
   assertion(stream_property(Outs, output)), 
   ( \+ current_output(Outs) -> 
       with_output_to(Outs, ec_io(Why, Ins)); 
         ec_io(Why, Ins)), !.

ec_in_to_pl(Why, Trans, Ins):- Trans == outf, !, 
   must(stream_property(Ins, file(InputName))), 
   atom_concat(InputName, '.pl', OutputName), 
   retractall(etmp:ec_option(_,_)),
   open(OutputName, write, Outs), 
   ec_in_to_pl(Why, Outs, Ins).

ec_in_to_pl(Why, Trans, Ins):- (var(Trans) ; Trans == trans), !, 
   stream_property(Ins, file(InputName)), 
   atom_concat(InputName, '.pl', OutputName), 
   ignore((Trans = OutputName)), 
   ec_in_to_pl(Why, OutputName, Ins). 

ec_in_to_pl(_Why, OutputName, _Ins):-  
   exists_file(OutputName), !, 
   ensure_loaded(OutputName), !.

ec_in_to_pl(Why, OutputName, Ins):-  
   \+ exists_file(OutputName), 
   retractall(etmp:ec_option(_,_)),   
   open(OutputName, write, Outs), !, 
   with_output_to(Outs, ec_io(Why, Ins)).

ec_in_to_pl(Why, Outs, Ins):- throw(unknown_ec_in_to_pl(Why, Outs, Ins)).

ec_io(Why, Ins):- dmsg(ec_io(Why, Ins)), fail.
ec_io(Why, Ins):-
  mention_s_l,
  repeat, 
  once(process_ec_stream(Why, Ins)), 
  notrace(at_end_of_stream(Ins)), !.
  


:- op(900, fx, ecread:'!').
:- op(1000, xfy, ecread:'&').
:- op(1050, xfy, ecread:'->').
:- op(1050, xfy, ecread:'<->').
:- op(1100, xfy, ecread:'|').



removed_one_ws(S):-
  peek_code(S, W), char_type(W, white), get_code(S, W), echo_format('~s', [[W]]).

removed_n_chars(_S, N):- N<1, !.
removed_n_chars(S, N):- get_code(S, _), Nm1 is N-1, removed_n_chars(S, Nm1).

trim_off_whitepace(S):- repeat, \+ removed_one_ws(S).

%process_stream_peeked213(S, " ;"):- !, till_eol(S).

read_n_save_vars(Codes):- read_n_save_vars(Codes, _).
read_n_save_vars(Codes, Vars):- ec_read3(Codes, VarNames), 
  ((VarNames={A}, atom(A)) -> Vars=[A]; Vars = VarNames), 
  asserta(etmp:temp_varnames(Vars)).


upcased_functors(G):- 
 notrace((allow_variable_name_as_functor = N, 
   current_prolog_flag(N, Was))), !, 
   setup_call_cleanup(notrace(set_prolog_flag(N, true)), 
      G, 
      notrace(set_prolog_flag(N, Was))).


%% process_ec_stream(Why, ?S) is det.
%
% Process file stream input
%
process_stream_comment(S) :- (peek_string(S, 2, W);peek_string(S, 1, W)), clause(process_stream_peeked213(S, W),Body),!,call(Body).
process_stream_peeked213(S, ";"):- !, read_line_to_string(S, Comment), echo_format('~N% ~s~n', [Comment]).
process_stream_peeked213(S, "["):- mention_s_l, write('% '), !, read_stream_until(S, [], `]`, Codes), read_n_save_vars(Codes).
process_stream_peeked213(S, "{"):- !, read_stream_until(S, [], `}`, Codes), read_n_save_vars(Codes), echo_format('~N% ~s~n', [Codes]).
process_stream_peeked213(S, "#!"):- !, till_eol(S).


%process_ec_stream(Why, S):- assertion(stream_property(S, input)).
process_ec_stream(Why, S):- notrace(at_end_of_stream(S)), !, mention_s_l, call(Why, end_of_file).
process_ec_stream(_, S) :- removed_one_ws(S), !.
process_ec_stream(_, S):- process_stream_comment(S), !.

process_ec_stream(Why, S):- maybe_mention_s_l,    
   OR = [to_lower('.'), to_lower('('), end_of_line, space, to_lower(':')], 
   write('% '),
   read_stream_until_true(S, [], char_type_inverse(Was, or(OR)), Text), 
   unpad_codes(Text, Codes), must(continue_process_ec_stream(Why, S, Codes, Was)), !.
process_ec_stream(Why, S):- read_line_to_string(S, Comment), echo_format('~N%RROOR: ~w: ~s~n', [Why, Comment]), break.

/*
process_ec_stream(Why, S):- must((read_term(S, T, [variable_names(Vs)]), put_variable_names( Vs))), 
  call(b_setval, '$variable_names', Vs), b_setval('$term', T), 
  (t_l:echo_mode(skip(items)) -> true ; write_stream_item(user_error, T)), !, 
  flush_output(user_error), 
  must(visit_script_term(T)), !, 
  echo_format('~N', []), !.

*/
   
% continue_process_ec_stream(Why, _S, [], space):- !.
continue_process_ec_stream(_Why, _S, [], _):- !.
continue_process_ec_stream(_Why, _S, [], end_of_line):- !.
continue_process_ec_stream(Why, S, NextCodes, CanBe ):-
  continue_process_ec_stream_too(Why, S, NextCodes, CanBe ),maybe_mention_s_l,!.

continue_process_ec_stream_too(Why, _S, Codes, to_lower(':')):- 
  append(Delta, [_], Codes), 
  text_to_string(Delta,DeltaS),
  normalize_space(atom(Term),DeltaS),
  process_out(Why, directive(Term)),!.
continue_process_ec_stream_too(Why, S, Codes, space):- last(Codes, Last), 
   once([Last]=`!`;char_type(Last, alpha)), !, 
   trim_off_whitepace(S), !, 
   atom_codes(Token, Codes), process_ec_stream_token(Why, Token, S), !.
continue_process_ec_stream_too(Why, S, NextCodes, _CanBe ):-  !, 
   last(NextCodes, Last), cont_one_ec_compound(S, NextCodes, Last, Term), process_out(Why, Term).

unpad_codes(Text, Codes):- text_to_string(Text, String), normalize_space(codes(Codes), String).
  
  
ec_from_atom(String, Term):- ec_read1(String, Term, _).   

ec_read3(String, Term):- 
 upcased_functors(notrace(catch(read_term_from_atom(String, Term, 
  [var_prefix(true), variable_names(Vars), 
   module(ecread)]), _, fail))), !, 
  maplist(ignore, Vars).

:- use_module(library(hybrid_db/portray_vars)).
:- dynamic(etmp:temp_varnames/1).
:- dynamic(etmp:ec_option/2).


insert_vars(Term, [], Term, []).
insert_vars(Term0, [V|LL], Term, [V=VV|Has]):-
  insert1_var(Term0, V, VV, Term1), 
  insert_vars(Term1, LL, Term, Has).


insert1_var(Term0, V, VV, Term1):- 
  debug_var(V, VV), 
  subst(Term0, V, VV, Term1).

special_directive(manualrelease).
special_directive(completion).
special_directive(ignore).
special_directive('!').
special_directive('neg').


map_callables(_, Term0, Term):- \+ callable(Term0), !, Term0=Term.
map_callables(_, Term0, Term):- []== Term0, !, Term =[].
map_callables(Call, Term0, Term):- atom(Term0), !, call(Call, Term0, Term).
map_callables(_Call, Term0, Term):- \+ compound(Term0), !, Term0=Term.
map_callables(Call, Compound=Value, Term):- compound(Compound), append_term(Compound, Value, Term0), map_callables(Call, Term0, Term).
map_callables(_, '$VAR'(HT), '$VAR'(HT)):-!.
map_callables(Call, [H|T], [HTerm|TTerm]):- !, map_callables(Call, H, HTerm), map_callables(Call, T, TTerm), !.
map_callables(Call, '$'(F, A), '$'(FF, AA)):- A==[], [] = AA, !, call(Call, F, FF).
%map_callables(Call, '$'(F, [A]), '$'(F, [AA])):- \+ special_directive(F), !, map_callables(Call, A, AA).
map_callables(Call, '$'(F, A), '$'(FF, AA)) :- call(Call, F, FF), maplist(map_callables(Call), A, AA), !.
map_callables(Call, HT, HTTerm):- !, 
 compound_name_arguments(HT, F, L), 
 map_callables(Call, '$'(F, L), '$'(FF, LL)), 
 compound_name_arguments(HTTerm, FF, LL).

fix_predname(~, neg).
fix_predname(!, neg).
fix_predname('|', ';').
fix_predname('&', ',').
fix_predname(F, not):- downcase_atom(F, not).
fix_predname(F, holds_at):- downcase_atom(F, holdsat).
fix_predname(F, Happens):- builtin_pred(Happens), downcase_atom(F, Happens), !.

my_unCamelcase(X, Y):- atom(X), fix_predname(X, Y), !.
my_unCamelcase(X, Y):- upcase_atom(X, X), !, downcase_atom(X, Y).
my_unCamelcase(X, Y):- unCamelcase(X, Y).

/*
xfr_ec(SS,O):- s_l(S,L),O=(etmp:ec_option(SS,L:S)),ground(SS),!.
xfr_ec(SS,O):- s_l(S,L),O=(etmp:ec_option(SS,L:S+X)), nb_current('$variable_names',X),!.

*/

fix_ec_term(C, C):- \+ callable(C), !.
fix_ec_term(X, Y):- \+ compound(X), !, my_unCamelcase(X, Y).
%fix_ec_term(X, Y):- xfr_ec(X, Y),X\=@=Y,!,fix_ec_term(X, Y).
fix_ec_term(neg(holds_at(N,V)),O):-fix_ec_term((holds_at(neg(N),V)),O).
fix_ec_term(t(X, [Y]), O):- !, fix_ec_term(t(X, Y), O).
fix_ec_term(load(X), load(X)).
fix_ec_term(option([N, V]), O):- !, fix_ec_term(option(N, V), O).
fix_ec_term(range([N, V, H]), O):- !, fix_ec_term(range(N, V, H), O).
% fix_ec_term(t(X, Y), O):- atom(X), is_list(Y), is_special_macro(X), SS=..[X|Y], fix_ec_term(SS, O).
fix_ec_term(t(X, Y), O):- atom(X), SS=..[X, Y], fix_ec_term(SS, O).
fix_ec_term(sort(col([S1, S2])), O):- !, fix_ec_term(subsort(S1, S2), O).
fix_ec_term(function(F, [M]), O):- fix_ec_term(function(F, M), O).
fix_ec_term(Compound=Value, Term):- compound(Compound), append_term(Compound, Value, Term0), fix_ec_term(Term0, Term).
fix_ec_term('$VAR'(HT), '$VAR'(HT)):-!.
fix_ec_term(Term1, Term):- 
  map_callables(my_unCamelcase, Term1, HTTermO),
  Term1\=@=HTTermO,!,
  fix_ec_term(HTTermO, Term).
fix_ec_term(HT, HTTermO):- !, 
 compound_name_arguments(HT, F, L), 
 maplist(fix_ec_term,L,LL),
 compound_name_arguments(HTTerm, F, LL),
 map_callables(my_unCamelcase, HTTerm, HTTermO).

verbatum_functor(function). verbatum_functor(event). 
verbatum_functor(predicate). verbatum_functor(fluent).
verbatum_functor(executable).

vars_verbatum(Term):- \+ compound(Term), !.
vars_verbatum(Term):- compound_name_arity(Term, F, A), (verbatum_functor(F);verbatum_functor(F/A)), !.

add_ec_vars(Term0, Term, Vs):- vars_verbatum(Term0), !, Term0=Term, Vs=[].
add_ec_vars(Term0, Term, Vs):- 
  findall(E, (etmp:temp_varnames(L), member(E, L)), LL), 
  sort(LL, LLS), 
  insert_vars(Term0, LLS, Term, Vs), !.

fix_ec_read(Term0, Term, Vs):- 
  add_ec_vars(Term0, Term1, Vs), 
  % pretty_numbervars(Term1, Term2), 
  fix_ec_term(Term1, Term), !, 
  retractall(etmp:temp_varnames(_)).


ec_read1(String, Term, Vs):- 
   ec_read2(String, Term0), !, 
   fix_ec_read(Term0, Term, Vs).
   

ec_read2(T, exists(Vs, Term)):- 
   cleanout(T, '{', '}', S3, SSS), !, read_n_save_vars(S3, Vs), 
   ec_read2(SSS, Term).
ec_read2(T, all(Vs, Term)):- 
   cleanout(T, '[', ']', S3, SSS), !, read_n_save_vars(S3, Vs), 
   ec_read2(SSS, Term).
ec_read2(Txt, Term):- 
   text_to_string(Txt, T), 
   atomics_to_string(List, '!=', T), List\=[_], 
   atomics_to_string(List, (\=), NewT), !, 
   ec_read2(NewT, Term).
ec_read2(T, Term):- 
   must(ec_read3(T, Term)), !.
   
   


cleanout(T, B, E, S3, SSS):-
 text_to_string(T, S1), 
 atomic_list_concat([A1, A2|Rest], B, S1), 
 atomic_list_concat(['', A2|Rest], B, S2), 
 setup_call_cleanup(
   open_string(S2, In), 
   ((read_stream_until(In, [], E, S3), S3\==[], 
     read_pending_codes(In, RestCodes, []), 
     text_to_string(RestCodes, S4))), 
     close(In)), !, 
 atomic_list_concat([A1, S4], '', SSS).


read_one_ec_compound(S, Term):- 
   read_stream_until_true(S, [], char_type_inverse(_Was, or([to_lower('.'), end_of_line])), Text), 
   unpad_codes(Text, Codes), last(Codes, Last), 
   cont_one_ec_compound(S, Codes, Last, Term).

cont_one_ec_compound(_S, Text, Last, Term):- char_type(Last, to_lower('.')),
   unpad_codes(Text, Codes), ec_from_atom(Codes, Term).

cont_one_ec_compound(_S, Text, Last, Term):- char_type(Last, to_lower(')')),
   \+ (member(T, `>&|`), member(T, Text)),
   unpad_codes(Text, Codes), ec_from_atom(Codes, Term).

cont_one_ec_compound(S, InCodes, WasLast, Term):- process_stream_comment(S), !, cont_one_ec_compound(S, InCodes, WasLast, Term).
cont_one_ec_compound(S, InCodes, WasLast, Term):- 
   (WasLast\==40-> write('% ') ; true), 
   read_stream_until_true(S, InCodes, char_type_inverse(_Was, or([to_lower('.'), end_of_line])), Text), 
   unpad_codes(Text, Codes), last(Codes, Last), 
   cont_one_ec_compound(S, Codes, Last, Term).

is_special_macro(range).
is_special_macro(mutex).
is_special_macro(ignore).
is_special_macro(reified_sort).
is_special_macro(noninertial).
is_special_macro(sort).
is_special_macro(option).
is_special_macro(reified).
is_special_macro(load).
is_special_macro(completion).
is_special_macro(xor).
%predicate, option range load fluent event noninertial xor completion

builtin_pred(initiates).
builtin_pred(terminates).
builtin_pred(releases).
builtin_pred(holds_at).
builtin_pred(happens).
builtin_pred(declipped).
builtin_pred(clipped).
builtin_pred(before).
builtin_pred(after).
builtin_pred(sort).
builtin_pred(initially).


decl_arg_sorts(fluent).
decl_arg_sorts(event).
decl_arg_sorts(executable).
decl_arg_sorts(predicate).
decl_arg_sorts(function).

% builtin_pred(releasedAt).


%s_l(F,L):- source_location(F,L),!.

:- dynamic(last_s_l/2).
maybe_mention_s_l:- last_s_l(B,L),LLL is L+5,  s_l(BB,LL), B==BB, !, (LLL<LL -> mention_s_l; true).
maybe_mention_s_l:- mention_s_l.

mention_s_l:-  notrace((flush_output, format('~N'),
  s_l(B,L), ansi_format([fg(green)], '% ~w~n', [B:L]), flush_output)),
  retractall(last_s_l(B,_)),asserta(last_s_l(B,L)).
s_l(F,L):- 
  current_stream(F,read,S),atom(F),
  stream_property(S,file_name(F)),
  ignore(stream_property(S,line_count(L));line_or_char_count(S,L);stream_property(S,line_or_char_count(L))),
  ignore(L=999),!.


% process_out(Why, load(SS)):- !, ec_to_pl(Why, Out, file(SS), current_output).
process_out(Why, SL):- fix_ec_term(SL, SO) -> SL\=@=SO, !, process_out(Why, SO).
process_out(Why, S):- must(glean_data(Why, S)),xfr_ec(S,S2), must(call(Why, S2)), !.


%process_ec(X):- xfr_ec(X,Y),X\=@=Y,process_ec(Y).
process_ec(ec_option(X,Y)):- assert(etmp:ec_option(X,Y)),dmsg(red(assert(ec_option(X,Y)))).
process_ec(axiom(X,Y)):- s_l(S,L),nb_current('$variable_names',Vs),!,process_ec(axiom(X,Y,lsv(L:S,Vs))).
process_ec(P):- assert(P),nop(dmsg(green(P))).

xfr_ec(etmp:ec_option(X,Y),ec_option(X,Y)):-!.
xfr_ec(option(N,V),O):- retractall(etmp:ec_option(N,_)),xfr_ec(ec_option(N,V),O).
xfr_ec(holds_at(N,AT),O):- AT==0, xfr_ec(initially(N),O).
xfr_ec(ec_option(X,Y),ec_option(X,Y)):-!.
xfr_ec(event(P),O):- compound_name_arity(P,F,A),compound_name_arity(PP,F,A), xfr_ec(executable(PP),O).
xfr_ec(P,P):- functor(P,F,1),decl_arg_sorts(F),!.
xfr_ec(P,P):- functor(P,F,_),is_special_macro(F),!.
xfr_ec(axiom(X,Y),axiom(X,Y)):-!.
xfr_ec(isa(X,Y),isa(X,Y)):-!.
%xfr_ec(subsort(X,Y),subsort(X,Y)):-!.
xfr_ec(range(X,Y,Z),range(X,Y,Z)):-!.
xfr_ec(happens(F, T1, T2), O):- T1==T2,!, xfr_ec(happens(F, T1), O).
xfr_ec(P,O):- P=..[C,E],C\==initially, !,xfr_ec(isa(E,C),O).

xfr_ec(isa(F, W), axiom(isa(F, W),[])):-!.
xfr_ec(subsort(F, W), axiom(subsort(F, W),[])):-!.
xfr_ec(happens(F, W), axiom(happens(F, W),[])):-!.
xfr_ec(initially(F), axiom(initially(F),[])):-!.
xfr_ec(holds_at(F, W), axiom(holds_at(F, W),[])):-!.
xfr_ec((PRE->POST),axiom(POST,List)):- /* functor(POST,F,2),builtin_pred(F),*/ conjuncts_to_list(PRE,List).
xfr_ec((PRE),axiom(POST,[])):- /* functor(POST,F,2),builtin_pred(F),*/ xfr_ec(PRE,POST).
xfr_ec(O,O).

uses_isa(C):- atom(C), \+ decl_arg_sorts(C).

do_ec_load(SS):- do_ec_convert(SS),
  process_ec(SS).
  

do_ec_convert(load(F)):- mention_s_l, exists_file(F), ec_load(F),!.

%do_ec_convert(load(F)):- exists_file(F),!,ec_to_pl(do_ec_convert, current_output, F).

do_ec_convert(SS):- must(pretty_numbervars(SS, SS1)), 
   flush_output, format('~N'), ansi_format([fg(yellow)], '~p.~n~n', [(SS1)]), flush_output, !.
   %maybe_mention_s_l.
do_ec_convert(Data):- wdmsg(do_ec_convert(Data)).

glean_data(Why, SL):- \+ compound(SL), !, dmsg(warn(glean_data(Why, SL))).
glean_data(Why, subsort(S1, S2)):- !, glean_data(Why, sort(S1)), glean_data(Why, sort(S2)), assert_gleaned(Why, subsort(S1, S2)).
glean_data(Why, sort(S)):- !, assert_gleaned(Why, sort(S)).
glean_data(Why, isa(E, S)):- !, assert_gleaned(Why, isa(E, S)).
glean_data(Why, SL):- SL=..[S, L], \+ is_special_macro(S), is_list(L), !, glean_data(Why, sort(S)), 
  maplist(glean_data(Why, hasInstance(S)), L).
glean_data(_, _).

%assert_gleaned(Why, sort(S)):-  !, call(Why, gleaned(sort(S))).
assert_gleaned(_Why, SS):-  asserta_if_new(gleaned(SS)).
%assert_gleaned(Why, SS):-  call(Why, gleaned(SS)).

glean_data(Why, hasInstance(S), E):- !, glean_data(Why, isa(E, S)).



process_ec_stream_token(Why, Atom, S):- atom_concat(New, '!', Atom), !, process_ec_stream_token(Why, New, S).
process_ec_stream_token(Why, Type, S):- normalize_space(atom(A), Type), A\==Type, !, process_ec_stream_token(Why, A, S).
process_ec_stream_token(Why, Text, S):- \+ atom(Text), !, text_to_atom(Text, Atom), process_ec_stream_token(Why, Atom, S).
process_ec_stream_token(Why, function, S):- !, read_stream_until(S, [], `:`, Text), read_line_to_string_echo(S, String), 
  append(TextL, [_], Text), 
  ec_read1(TextL, Value, _), 
  token_stringsss(String, Type), 
   process_out(Why, (function(Value, Type))).

process_ec_stream_token(Why, Type, S):- downcase_atom(Type, Event), memberchk(Event, [fluent, predicate, event]), !, 
   read_one_ec_compound(S, Value), process_out(Why, t(Event, Value)).
process_ec_stream_token(Why, reified, S):- !, read_stream_until(S, [], ` `, Text), 
   text_to_string(Text, St), atom_concat('reified_', St, Type), !, process_ec_stream_token(Why, Type, S).
process_ec_stream_token(Why, Type, S):- read_line_to_string_echo(S, String), process_ec_token_with_string(Why, Type, String).

process_ec_token_with_string(Why, Type, String):- \+ is_special_macro(Type), atomics_to_string(VList, ',', String), VList \= [_], !, 
  maplist(process_ec_token_with_string(Why, Type), VList).
process_ec_token_with_string(_, _, ""):-!.
process_ec_token_with_string(Why, Type, String):- token_stringsss(String, Out), process_out(Why, t(Type, Out)).

token_stringsss("", []):-!.
token_stringsss(String, Out):- normalize_space(string(S), String), S\==String, !, token_stringsss(S, Out).
token_stringsss(String, col(VVList)):- atomics_to_string(VList, ':', String), VList \= [_], remove_blanks(VList, VVList), !.
token_stringsss(String, VVList):- atomics_to_string(VList, ',', String), VList \= [_], remove_blanks(VList, VVList), !.
token_stringsss(String, VVList):- atomics_to_string(VList, ' ', String), remove_blanks(VList, VVList), !.
  

remove_blanks([], []).
remove_blanks([''|I], O):- !, remove_blanks(I, O).
remove_blanks([E|I], O):- string(E), normalize_space(string(EE), E), E\==EE, !, remove_blanks([EE|I], O).
remove_blanks([E|I], O):- atom(E), normalize_space(atom(EE), E), E\==EE, !, remove_blanks([EE|I], O).
remove_blanks([E|I], O):- to_atomic_value(E, EE), E\==EE, !, remove_blanks([EE|I], O).
remove_blanks([E|I], [E|O]):- remove_blanks(I, O).
%process_ec_stream_token(Why, Sort, S):- read_tokens_til_eol(S, ' ', Value), process_out(Why, t(Sort, Value)).
%process_ec_stream_token(Why, reified, " sort", S):- read_tokens_til_eol(S, Value), process_out(Why, reified_sort(Value)).

/*
*/
read_tokens_til_eol(S, Splitter, Value):-
  trim_off_whitepace(S), !, read_line_to_string_echo(S, String), 
   text_to_string(String, SS), 
   subsplit(Splitter, SS, Value).

subsplit([], String, Value):- to_atomic_value(String, Value).
subsplit([A|B], String, Value):-  
   normalize_space(string(SS), String), 
   atomic_list_concat(ValueS, A, SS), 
   maplist(subsplit(B), ValueS, Value).


subsplit(SS, Splitter, Value):- 
   atomic_list_concat(ValueA, Splitter, SS), 
   maplist(to_atomic_value, ValueA, Value).%, process_out(Why, option(List)).
   %atomics_to_string(['[', SS, ']'], ' ', NewList), ec_from_atom(NewList, Value).

to_atomic_value(A, N):- number(A), !, N=A.
to_atomic_value(A, N):- normalize_space(atom(S), A), S\==A, !, to_atomic_value(S, N).
to_atomic_value(A, N):- atom_number(A, N).
to_atomic_value(A, A).

read_stream_until(S, Buffer, [Until], Codes):- !, name(N, [Until]), char_code(N, UntilCode), !, 
 read_stream_until_true(S, Buffer, ==(UntilCode), Codes).
read_stream_until(S, Buffer, UntilCode, Codes):- integer(UntilCode), !, 
 read_stream_until_true(S, Buffer, ==(UntilCode), Codes).
read_stream_until(S, Buffer, Until, Codes):- atom(Until), atom_length(Until, 1), char_code(Until, UntilCode), !, 
 read_stream_until_true(S, Buffer, ==(UntilCode), Codes).
read_stream_until(S, Buffer, Until, Codes):- read_stream_until_true(S, Buffer, Until, Codes).

char_type_inverse(Type, or(TypeList), Code):- !, member(E, TypeList), char_type_inverse(Type, E, Code).
char_type_inverse(Type, [Spec], Code):- !, char_type_inverse(Type, Spec, Code).
char_type_inverse(Type, [Spec|List], Code):- !, char_type_inverse(_, Spec, Code), char_type_inverse(Type, List, Code).
char_type_inverse(Type, Spec, Code):- char_type(Code, Spec), Type=Spec.

read_stream_until_true(S, Buffer, Pred, Buffer):- at_end_of_stream(S), !, ignore(call(Pred, 10)).
read_stream_until_true(S, Buffer, Pred, Codes):- get_code(S, Char), put_code(Char),
  (call(Pred, Char) -> notrace(append(Buffer, [Char], Codes)) ; 
  (notrace(append(Buffer, [Char], NextBuffer)), read_stream_until_true(S, NextBuffer, Pred, Codes))).
  



% in_space_cmt(Goal):- call_cleanup(prepend_each_line(' % ', Goal), echo_format('~N', [])).
in_space_cmt(Goal):- setup_call_cleanup(echo_format('~N /*~n', []), Goal, echo_format('~N*/~n', [])).


till_eol(S):- read_line_to_string(S, String), echo_format('~N% ~s~n', [String]).

read_line_to_string_echo(S, String):- read_line_to_string(S, String), format('~s',String).
  

echo_format(_Fmt, _Args):- flush_output, t_l:block_comment_mode(Was), Was==invisible, !.
echo_format(Fmt, Args):- t_l:block_comment_mode(_), t_l:echo_mode(echo_file), !, format(Fmt, Args), flush_output.
echo_format(Fmt, Args):- t_l:echo_mode(echo_file), !, format(Fmt, Args), flush_output.
echo_format(_Fmt, _Args):- t_l:echo_mode(skip(_)), !.
echo_format(Fmt, Args):- format(Fmt, Args), flush_output, !.
%echo_format(_Fmt, _Args).


write_stream_item(Out, T):- 
  flush_output, 
  format(Out, '~N~n', []), 
  must(with_output_to(Out, portray_clause_w_vars(T))), 
  format(Out, '~N~n', []), !, flush_output(Out).




till_eof(In) :-
        repeat, 
            (   at_end_of_stream(In)
            ->  !
            ;   (read_pending_codes(In, Chars, []), 
                (t_l:echo_mode(echo_file) ->
                  echo_format('~s', [Chars]);
                  true), 
                fail)
            ).

