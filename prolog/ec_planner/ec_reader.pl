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

/*
  
 ec_reader:   
    Converts Eric Muellers DEC Reasoner files  (IBM ".e" files)
    To a Prolog readable ".e.pl" which may be maintained by hand
    

*/
:- module(ec_reader,[convert_e/1, set_ec_option/2, verbatum_functor/1, builtin_pred/1, e_to_pl/3 ]).


set_ec_option(N,V):- retractall(etmp:ec_option(N,_)),asserta(etmp:ec_option(N,V)).


% used by ec_reader
verbatum_functor(function).  verbatum_functor(event). 
verbatum_functor(predicate).  verbatum_functor(fluent).


non_list_functor(ignore).
non_list_functor(manualrelease).
non_list_functor(sort).
non_list_functor(reified_sort).
non_list_functor(reified).
non_list_functor(noninertial).
non_list_functor(mutex).
non_list_functor(completion).
is_non_sort(range).
is_non_sort(option).
is_non_sort(load).
is_non_sort(xor).
is_non_sort(P):- verbatum_functor(P).
is_non_sort(NoListF):- non_list_functor(NoListF).

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

is_quantifier_type(thereExists,( & )):- use_some.
is_quantifier_type(forAll,all).
is_quantifier_type(thereExists,exists).

% used by ec_loader

:- meta_predicate e_to_pl(1,+,+), e_to_pl(1,+,+).
:- meta_predicate map_callables(2,*,*).
:- meta_predicate in_space_cmt(0).
:- meta_predicate process_e_stream(1,*).
:- meta_predicate ec_on_read(1,*).
:- meta_predicate e_io(1,*).
:- meta_predicate upcased_functors(0).
:- meta_predicate read_stream_until_true(*,*,1,*).
:- meta_predicate process_e_stream_token(1,*,*).
:- meta_predicate continue_process_e_stream_too(1,*,*,*).
:- meta_predicate process_e_token_with_string(1,*,*).
:- meta_predicate continue_process_e_stream(1,*,*,*).

:- thread_local(t_l:each_file_term/1).
:- thread_local(t_l:block_comment_mode/1).
:- thread_local(t_l:echo_mode/1).

%:- meta_predicate now_doing(1, ?).
%:- meta_predicate each_doing(1, ?).
%:- meta_predicate doing(1, *).
  
:- meta_predicate 
   with_e_sample_tests(1),
   raise_translation_event(1,*,*).

:- use_module(library(logicmoo_common)).
%:- use_module(library(file_utils/filestreams)).

:- export(e_reader_test/0).
e_reader_test:- with_e_sample_tests(convert_e(user_output)).

:- export(e_reader_testf/0).
e_reader_testf:- with_e_sample_tests(convert_e(outdir('.'))).



:- export(with_e_sample_tests/1).
with_e_sample_tests(Out) :- 
%  call(Out, 'ectest/*.e'),  
%  call(Out, 'examples/AkmanEtAl2004/ZooWorld.e'),
  call(Out, 'ectest/ec_reader_test_ecnet.e'),
  call(Out, 'ecnet/Kidnapping.e'),
  call(Out, 'ecnet/SpeechAct.e'),
   call(Out, 'ecnet/Diving.e'),
   call(Out, 'examples/Mueller2006/Exercises/MixingPaints.e'),

  
%  call(Out, 'examples/Mueller2006/Chapter11/HungryCat.e'),
  !.
%:- initialization(e_reader_test, main).


% 
% :- meta_predicate ec_reader:must(0).

raise_translation_event(Why,What,OutputName):- call(Why,translate(What,OutputName)).


dedupe_files(SL0,SL):- maplist(relative_file_name,SL0,SL1), list_to_set(SL1,SL).
  relative_file_name(A,S):-  prolog_canonical_source(A,L), file_name_on_path(L,S), atom(S), \+ name(S,[]), !.
  relative_file_name(A,A).          

exists_all_filenames(S0, SL, Options):- 
  findall(N, (relative_from(D), 
     absolute_file_name(S0, N, 
        [relative_to(D), file_type(txt), file_errors(fail), access(read), solutions(all)|Options])), SL0),
  dedupe_files(SL0,SL),!.

:- export(resolve_local_files/2).
resolve_local_files(S0,SL):- atom(S0), expand_file_name(S0,SL), SL \= [E|_], exists_file(E), !.
resolve_local_files(S0,SL):- exists_all_filenames(S0,SL, [expand(false)]), SL \= [].
resolve_local_files(S0,SL):- exists_all_filenames(S0,SL, [expand(true)]), SL \= [].
resolve_local_files(S0,SS):- atom(S0), file_base_name(S0,S1), S0\==S1, resolve_local_files(S1,SS).

relative_from(F):- nb_current('$ec_input_file', F).
relative_from(D):- working_directory(D,D).
relative_from(F):- stream_property(_,file_name(F)).

/*
resolve_file(S0,SS):- atom(S0), exists_file(S0), !, SS=S0. 
resolve_file(S0,SS):- absolute_file_name(S0, SS, [expand(true), file_errors(fail), access(read)]), !.
resolve_file(S0,SS):- relative_from(F), absolute_file_name(S0, SS, [relative_to(F),file_errors(fail),access(read)]), !.
resolve_file(S0,SS):- atom(S0), file_base_name(S0,S1), S0\==S1, resolve_file(S1,SS).
*/

:- export(needs_resolve_local_files/2).
needs_resolve_local_files(F, L):- \+ is_stream(F), \+ is_filename(F),
  resolve_local_files(F, L), !,  L \= [], L \= [F].

:- export(calc_where_to/3).
calc_where_to(InputName,_Dir,OutputName):- atom_concat(InputName, '.pro', OutputName).

:- set_ec_option(overwrite_transated_files,always).

should_update(OutputName):- \+ exists_file(OutputName),!.
should_update(_):- etmp:ec_option(overwrite_transated_files,always),!.

:- export(include_e/1).
include_e(F):- e_to_pl(do_convert_e, current_output, F).


:- export(convert_e/1).
convert_e(F):- convert_e(outdir('.'), F).
convert_e(Out, F):- e_to_pl(do_convert_e, Out, F).

:- export(is_filename/1).
is_filename(F):- atom(F), \+ is_stream(F),
  (exists_file(F);is_absolute_file_name(F)).

%e_to_pl(Why, Out, F):- dmsg(e_to_pl(Why, Out, F)), fail.

% wildcard input file  "./foo*.e"
e_to_pl(Why, Out, F):- atom(F), \+ is_stream(F), \+ is_filename(F), 
   expand_file_name(F, L), L\==[], [F]\==L, !, maplist(e_to_pl(Why, Out), L).

% wildcard input file  logical(./foo*.e).
e_to_pl(Why, Out, F):-  \+ is_stream(F), \+ is_filename(F),
   findall(N, absolute_file_name(F, N, [file_type(txt), file_errors(fail), expand(false), solutions(all)]), L), 
   L\=[F], !, maplist(e_to_pl(Why, Out), L).

% Out is a misdirected stream
e_to_pl(Why, Outs, Ins):- 
   atomic(Outs), is_stream(Outs),
   assertion(stream_property(Outs, output)), 
   \+ current_output(Outs), !,
   with_output_to(Outs, 
    e_to_pl(current_output, Why, Ins)),!.

% Out is like a wildcard stream (but we have a real filename)
e_to_pl(Why, outdir(Dir), F):- is_filename(F), !, 
   calc_where_to(F, outdir(Dir), OutputName),
   e_to_pl(Why, OutputName, F).

% Out is like a wildcard stream (calc a real filename)
e_to_pl(Why, outdir(Dir), Ins):- must(is_stream(Ins)), !, 
   must(stream_property(Ins, file(InputName))),
   calc_where_to(InputName, outdir(Dir), OutputName),
   e_to_pl(Why, OutputName, Ins).

% Out is a filename not neding update
e_to_pl(Why, OutputName, _Ins):- is_filename(OutputName), 
   \+ should_update(OutputName),
   raise_translation_event(Why,skipped,OutputName),
   raise_translation_event(Why,ready,OutputName), !.

e_to_pl(Why, Out, F):- is_filename(F), !, 
    locally(b_setval('$ec_input_file',F),
      setup_call_cleanup(
        open(F, read, Ins),    
        e_to_pl(Why, Out, Ins),
        close(Ins))),!.

% Out is a filename not currently loadable 
e_to_pl(Why, OutputName, Ins):-  \+ is_stream(OutputName), !,
   assertion(is_stream(Ins)), assertion(stream_property(Ins, input)),
   must(should_update(OutputName)),
   raise_translation_event(Why,unskipped,OutputName),
   setup_call_cleanup(
     open(OutputName, write, Outs),
     with_output_to(Outs, 
       (raise_translation_event(Why,begining,OutputName), 
         e_to_pl(Why, current_output, Ins),
          raise_translation_event(Why,ending,OutputName))),
     close(Outs)),
   raise_translation_event(Why,ready,OutputName).

e_to_pl(Why, Out, Ins):- 
      assertion(current_output(Out)),       
      e_io(Why, Ins).

:- nb_setval(ec_input_file,[]).
        
%e_io(Why, Ins):- dmsg(e_io(Why, Ins)), fail.
e_io(Why, Ins):-  
  repeat, 
  once(process_e_stream(Why, Ins)), 
  notrace(at_end_of_stream(Ins)), !.
  


:- op(900, fx, ecread:'!').
:- op(1000, xfy, ecread:'&').
:- op(1050, xfy, ecread:'->').
:- op(1150, xfy, ecread:'<->').
:- op(1100, xfy, ecread:'|').
:- op(1150, xfy, ecread:'quantz').
:- op(1025, xfy, ecread:'thereExists').

:- op(1150, xfx, '<->').


removed_one_ws(S):-
  peek_code(S, W), char_type(W, white), get_code(S, W), echo_format('~s', [[W]]).

removed_n_chars(_S, N):- N<1, !.
removed_n_chars(S, N):- get_code(S, _), Nm1 is N-1, removed_n_chars(S, Nm1).

trim_off_whitepace(S):- repeat, \+ removed_one_ws(S).



read_n_save_vars(Type, Codes):- read_some_vars(Codes, Vars),
  asserta(etmp:temp_varnames(Type, Vars)).

read_some_vars(Codes, Vars):-
  must(e_read3(Codes, VarNames)), !, 
  varnames_as_list(VarNames, Vars).

varnames_as_list({A},[A]):- atom(A),!.
varnames_as_list({A,B},Vars):- !,varnames_as_list({A},Vars1),varnames_as_list({B},Vars2),append(Vars1,Vars2,Vars).
varnames_as_list(VarNames,Vars):- assertion(is_list(VarNames)), !, VarNames=Vars.

upcased_functors(G):- 
 notrace((allow_variable_name_as_functor = N, 
   current_prolog_flag(N, Was))), !, 
   setup_call_cleanup(notrace(set_prolog_flag(N, true)), 
      G, 
      notrace(set_prolog_flag(N, Was))).


%% process_e_stream(Why, ?S) is det.
%
% Process file stream input
%
process_stream_comment(S) :- (peek_string(S, 2, W);peek_string(S, 1, W)), clause(process_stream_peeked213(S, W),Body),!,call(Body).
process_stream_peeked213(S, "#!"):- !, echo_till_eol(S).
process_stream_peeked213(S,  ";"):- !, echo_format('%'), echo_till_eol(S).
process_stream_peeked213(S, "["):- mention_s_l, echo_format('% '), !, read_stream_until(S, [], `]`, Codes), read_n_save_vars(universal, Codes).
process_stream_peeked213(S, "{"):- mention_s_l, echo_format('% '), !, read_stream_until(S, [], `}`, Codes), read_n_save_vars(existential, Codes).


%process_e_stream(Why, S):- assertion(stream_property(S, input)).
process_e_stream(Why, S):- notrace(at_end_of_stream(S)), !, mention_s_l, call(Why, end_of_file).
process_e_stream(_, S) :- removed_one_ws(S), !.
process_e_stream(_, S):- process_stream_comment(S), !.

process_e_stream(Why, S):- maybe_mention_s_l,    
   OR = [to_lower('.'), to_lower('('), end_of_line, to_lower('='),to_lower('>'), space, to_lower(':')], 
   echo_format('% '),
   read_stream_until_true(S, [], char_type_inverse(Was, or(OR)), Text), 
   unpad_codes(Text, Codes), must(continue_process_e_stream(Why, S, Codes, Was)), !.
process_e_stream(Why, S):- read_line_to_string(S, Comment), echo_format('~N%RROOR: ~w: ~s~n', [Why, Comment]), break.


% continue_process_e_stream(Why, _S, [], space):- !.
continue_process_e_stream(_Why, _S, [], _):- !.
continue_process_e_stream(_Why, _S, [], end_of_line):- !.
continue_process_e_stream(Why, S, NextCodes, CanBe ):-
  continue_process_e_stream_too(Why, S, NextCodes, CanBe ),maybe_mention_s_l,!.

continue_process_e_stream_too(Why, _S, Codes, to_lower(':')):- 
  append(Delta, [_], Codes), 
  text_to_string(Delta,DeltaS),
  normalize_space(atom(Term),DeltaS),
  ec_on_read(Why, directive(Term)),!.
continue_process_e_stream_too(Why, S, Codes, space):- last(Codes, Last), 
   once([Last]=`!`;char_type(Last, alpha)), !, 
   trim_off_whitepace(S), !, 
   atom_codes(Token, Codes), process_e_stream_token(Why, Token, S), !.
continue_process_e_stream_too(Why, S, NextCodes, _CanBe ):-  !, 
   last(NextCodes, Last), cont_one_e_compound(S, NextCodes, Last, Term), ec_on_read(Why, Term).

unpad_codes(Text, Codes):- text_to_string(Text, String), normalize_space(codes(Codes), String).
  
  
e_from_atom(String, Term):- e_read1(String, Term, _).   

e_read3(String, Term):- 
 upcased_functors(notrace(catch(read_term_from_atom(String, Term, 
  [var_prefix(true), variable_names(Vars), 
   module(ecread)]), _, fail))), !, 
  maplist(ignore, Vars).

:- use_module(library(hybrid_db/portray_vars)).
:- dynamic(etmp:temp_varnames/2).
:- dynamic(etmp:ec_option/2).


insert_vars(Term, [], Term, []).
insert_vars(Term0, [V|LL], Term, [V=VV|Has]):-
  insert1_var(Term0, V, VV, Term1), 
  insert_vars(Term1, LL, Term, Has).


insert1_var(Term0, V, VV, Term1):- 
  debug_var(V, VV), 
  subst(Term0, V, VV, Term1).


map_callables(_, Term0, Term):- \+ callable(Term0), !, Term0=Term.
map_callables(_, Term0, Term):- []== Term0, !, Term =[].
map_callables(Call, Term0, Term):- atom(Term0), !, call(Call, Term0, Term).
map_callables(_Call, Term0, Term):- \+ compound(Term0), !, Term0=Term.
map_callables(Call, Compound=Value, Term):- fail, compound(Compound), 
  append_term(Compound, Value, Term0), map_callables(Call, Term0, Term).
map_callables(_, '$VAR'(HT), '$VAR'(HT)):-!.
map_callables(Call, [H|T], [HTerm|TTerm]):- !, map_callables(Call, H, HTerm), map_callables(Call, T, TTerm), !.
map_callables(Call, '$'(F, A), '$'(FF, AA)):- A==[], [] = AA, !, call(Call, F, FF).
%map_callables(Call, '$'(F, [A]), '$'(F, [AA])):- \+ special_directive(F), !, map_callables(Call, A, AA).
map_callables(Call, '$'(F, A), '$'(FF, AA)) :- call(Call, F, FF), maplist(map_callables(Call), A, AA), !.
map_callables(Call, HT, HTTerm):- !, 
 compound_name_arguments(HT, F, L), 
 map_callables(Call, '$'(F, L), '$'(FF, LL)), 
 compound_name_arguments(HTTerm, FF, LL).

compound_gt(P,GT):- compound(P), compound_name_arity(P, _, N), N > GT.

fix_predname(!, not).
fix_predname('|', ';').
fix_predname('&', ',').
%fix_predname('|', 'or').
%fix_predname('&', '&').
fix_predname(F, not):- downcase_atom(F, not).
fix_predname(F, holds_at):- downcase_atom(F, holdsat).
fix_predname(F, Happens):- builtin_pred(Happens), downcase_atom(F, Happens), !.


my_unCamelcase(X, Y):- atom(X), fix_predname(X, Y), !.
my_unCamelcase(X, Y):- atom(X), upcase_atom(X, X), !, downcase_atom(X, Y).
my_unCamelcase(X, Y):- unCamelcase(X, Y), !.


e_to_ec(C, C):- \+ callable(C), !.
e_to_ec('$VAR'(HT), '$VAR'(HT)):-!.
e_to_ec(X, Y):- \+ compound(X), !, must(my_unCamelcase(X, Y)).
e_to_ec(X, Y):- compound_name_arity(X, F, 0), !, my_unCamelcase(F, FF), compound_name_arity(Y, FF, 0).
e_to_ec(not(Term),not(O)):- !, e_to_ec(Term, O).
e_to_ec(Prop,O):- 
  Prop =.. [ThereExists,not(Vars),Term0],
  is_quantifier_type(ThereExists,_Exists),
  nonvar(Vars),!,
  QProp =.. [ThereExists,Vars,Term0], 
  e_to_ec(not(QProp),O).
e_to_ec(Prop,O):- 
  Prop =.. [ThereExists,Vars,Term0], 
  is_quantifier_type(ThereExists,Exists),
  is_list(Vars), forall(member(E,Vars),ground(E)),
  QProp =.. [Exists,Vars,Term0],
  insert_vars(QProp, Vars, Term, _Has),
  e_to_ec(Term,O),!.
%e_to_ec(X, Y):- e_to_ax(X, Y),X\=@=Y,!,e_to_ec(X, Y).
%e_to_ec(neg(C),O):-e_to_ec(holds_at(neg(N),V),O):- compound(C),holds_at(N,V)=C,
%e_to_ec(neg(holds_at(N,V)),O):-e_to_ec((holds_at(neg(N),V)),O).
e_to_ec(t(X, [Y]), O):- nonvar(Y), !, e_to_ec(t(X, Y), O).
e_to_ec(load(X), load(X)).
e_to_ec(include(X), include(X)).
e_to_ec(option([N, V]), O):- !, e_to_ec(option(N, V), O).
e_to_ec(range([N, V, H]), O):- !, e_to_ec(range(N, V, H), O).

e_to_ec(t(X, Y), O):- atom(X), is_non_sort(X), !, SS=..[X, Y], e_to_ec(SS, O).
e_to_ec(t(X, Y), O):- atom(X), is_list(Y), is_non_sort(X), SS=..[X|Y], e_to_ec(SS, O).
e_to_ec(t(X, Y), O):- atom(X), is_list(Y), SS=..[X, Y], e_to_ec(SS, O).
e_to_ec(sort(col([S1, S2])), O):- !, e_to_ec(subsort(S1, S2), O).
e_to_ec(function(F, [M]), O):- e_to_ec(function(F, M), O).
%e_to_ec(Compound=Value, equals(Compound,Value)).
/*
e_to_ec(Term1, Term):- 
%  map_callables(my_unCamelcase, Term1, HTTermO),
%  Term1\=@=HTTermO,!,
%  e_to_ec(HTTermO, Term). 
*/
e_to_ec(HT, HTTermO):- !, 
 compound_name_arguments(HT, F, L), 
 maplist(e_to_ec,L,LL),
 compound_name_arguments(HTTerm, F, LL),
 map_callables(my_unCamelcase, HTTerm, HTTermO).


vars_verbatum(Term):- \+ compound_gt(Term, 0), !.
vars_verbatum(Term):- compound_name_arity(Term, F, A), (verbatum_functor(F);verbatum_functor(F/A)), !.

add_ec_vars(Term0, Term, Vs):- vars_verbatum(Term0), !, Term0=Term, Vs=[].
add_ec_vars(Term0, Term, Vs):- 
  get_vars(universal, UniVars),
  get_vars(existential,ExtVars),
  insert_vars(Term0, UniVars, Term1, VsA),!,  
  add_ext_vars(VsA, ExtVars, Term1, Term, Vs), !.

add_ext_vars(Vs, [], Term, Term, Vs):- !.
add_ext_vars(VsA, LLS, Term0, Term, Vs):-  use_some,
  insert_vars((some(LLS), Term0), LLS, Term, VsB), !,
  append(VsA,VsB,Vs),!.
add_ext_vars(VsA, LLS, Term0, Term, Vs):-  
  insert_vars(exists(LLS, Term0), LLS, Term, VsB), !,
  append(VsA,VsB,Vs),!.

use_some :- fail.

get_vars(Type,LLS):- findall(E, (etmp:temp_varnames(Type,L), member(E, L)), LL), sort(LL, LLS),!.


e_read1(String, Term, Vs):- 
   e_read2(String, Term0), !, 
   add_ec_vars(Term0, Term1, Vs), !,
   retractall(etmp:temp_varnames(_,_)),
   e_to_ec(Term1, Term), !.

if_string_replace(T, B, A, NewT):-   
   atomics_to_string(List, B, T), List=[_,_|_], !,
   atomics_to_string(List, A, NewT). 


e_read2(Txt, Term):- \+ string(Txt), text_to_string(Txt, T),!, e_read2(T, Term).
e_read2(T, Term):- if_string_replace(T, '!=', (\=), NewT), !, e_read2(NewT, Term).
e_read2(T, Term):- use_some,
  if_string_replace(T,  '{', ' some( ', T1), 
  if_string_replace(T1, '}', ' ) & ', NewT), 
  e_read2(NewT, Term).
e_read2(T, Term):- 
  if_string_replace(T, '{', ' [ ', T1), 
  if_string_replace(T1, '}', ' ] thereExists ', NewT),    
  e_read2(NewT, Term).
%e_read2(T, Term):- if_string_replace(T, '[', ' forAll( ', NewT), !, e_read2(NewT, Term).
%e_read2(T, Term):- if_string_replace(T, ']', ') quantz ', NewT), !, e_read2(NewT, Term).
e_read2(T, Term):- 
   upcased_functors(read_term_from_atom(T, Term, 
     [var_prefix(true), variable_names(Vars), module(ecread)])), !,
  maplist(ignore, Vars).
e_read2(T, Term):- 
   must(e_read3(T, Term)), !.
   
   

cleanout(Orig, B, E, MidChunk, RealRemainder):-
 text_to_string(Orig, Str), 
 AfterFirstB=[_|_],
 atomic_list_concat([BeforeB|AfterFirstB], B, Str), 
         atomics_to_string(  AfterFirstB, B, AfterB),
 Remainder=[_|_],
 atomic_list_concat([Mid|Remainder], E, AfterB),
 atomics_to_string( Remainder, E, AfterE),
 atomics_to_string( [BeforeB,' ', AfterE], RealRemainder),
 atomics_to_string( [B, Mid, E], MidChunk).


read_one_e_compound(S, Term):- 
   read_stream_until_true(S, [], char_type_inverse(_Was, or([to_lower('.'), end_of_line])), Text), 
   unpad_codes(Text, Codes), last(Codes, Last), 
   cont_one_e_compound(S, Codes, Last, Term).

cont_one_e_compound(_S, Text, Last, Term):- char_type(Last, to_lower('.')),
   unpad_codes(Text, Codes), e_from_atom(Codes, Term).

cont_one_e_compound(_S, Text, Last, Term):- char_type(Last, to_lower(')')),
   \+ (member(T, `>&|`), member(T, Text)),
   unpad_codes(Text, Codes), e_from_atom(Codes, Term).

cont_one_e_compound(S, InCodes, WasLast, Term):- process_stream_comment(S), !, cont_one_e_compound(S, InCodes, WasLast, Term).
cont_one_e_compound(S, InCodes, WasLast, Term):- 
   (WasLast\==40-> echo_format('% ') ; true), 
   read_stream_until_true(S, InCodes, char_type_inverse(_Was, or([to_lower('.'), end_of_line])), Text), 
   unpad_codes(Text, Codes), last(Codes, Last), 
   cont_one_e_compound(S, Codes, Last, Term).


%s_l(F,L):- source_location(F,L),!.

:- dynamic(last_s_l/2).
maybe_mention_s_l:- last_s_l(B,L),LLL is L+5,  s_l(BB,LL), B==BB, !, (LLL<LL -> mention_s_l; true).
maybe_mention_s_l:- mention_s_l.                      

:- export(mention_s_l/0).
mention_s_l:-  must_det_l((flush_output, echo_format('~N'),
  s_l(B,L), ansi_format([fg(green)], '% ~w~n', [B:L]), flush_output)),
  retractall(last_s_l(B,_)),asserta(last_s_l(B,L)).

:- export(s_l/2).
s_l(F,L):- source_location(F,L), !.
s_l(F,L):- any_stream(F,S), any_line_count(S,L),any_line_count(_,L), !.
s_l(unknown,0).

any_stream(F,S):- stream_property(S, file_name(F)),stream_property(S, input).
any_stream(F,S):- current_stream(F, read, S), atom(F).
any_stream(F,S):- stream_property(S, file_name(F)).
any_stream(F,S):- current_stream(F, _, S), atom(F).
any_line_count(_,L):- nonvar(L),!.
any_line_count(S,L):- stream_property(S, line_count(L)).
any_line_count(S,L):- line_or_char_count(S, L).
any_line_count(S,L):- stream_property(S, line_or_char_count(L)).
any_line_count(_,0).

%ec_on_read(S):- ec_on_read(on_load_ele, S).

:- meta_predicate ec_on_each_read(1,*,*).

ec_on_read(Why, EOF):- EOF == end_of_file, !,  must(call(Why, EOF)).
ec_on_read(Why, SL):- e_to_ec(SL, SO) -> SL\=@=SO, !, ec_on_read(Why, SO).
ec_on_read(Why, Cmp):- compound_gt(Cmp, 0), 
  Cmp =.. [NonlistF, List], is_list(List), non_list_functor(NonlistF),!, 
  maplist(ec_on_each_read(Why,NonlistF), List).
ec_on_read(Why, S):- must(glean_data(Why, S)), must(call(Why, S)).



ec_on_each_read(Why, NonlistF, E):- Cmp =.. [NonlistF, E], ec_on_read(Why, Cmp).

%must(G):- tracing, !, notrace(G).
%must(G):- call(G)->true;(trace,ignore(rtrace(G)),break).

on_convert_ele(translate(Event, Outfile)):- !, must((mention_s_l, echo_format('~N% translate: ~w  File: ~w ~n',[Event, Outfile]))).
on_convert_ele(include(S0)):- resolve_local_files(S0,SS), !, maplist(include_e, SS), !.
%on_convert_ele(load(S0)):- resolve_local_files(S0,SS), !, maplist(load_e, SS), !.  
on_convert_ele(end_of_file).
on_convert_ele(SS):- must(echo_format('~N')), must(pprint_ecp([bold,fg(yellow)],SS)).


do_convert_e(SS):- on_convert_ele(SS).

:- meta_predicate with_op_cleanup(*,*,*,0).


str_repl(F,R,I,O):- if_string_replace(I,F,R,O),!.
str_repl(_,_,I,I).
replcterm(F,R,I,O):- subst(I,F,R,O),!.


get_operators(P,[]):- \+ compound_gt(P, 0), !.
get_operators([H|T],Ops):- !, get_operators(H,L),get_operators(T,R),append(L,R,Ops).
get_operators(P,Ops):- P=..[F|List],get_operators(List,More),
  (is_operator(F)->Ops=[F|More];Ops=More).

is_operator('<->').
is_operator('->').
is_operator('-->').
is_operator('<-').
is_operator(F):- current_op(N,_,F),N>800.

mid_pipe(In,[H|T],Out):- !,mid_pipe(In,H,Mid),mid_pipe(Mid,T,Out).
mid_pipe(In,[],In):-!.
mid_pipe(In,H,Out):- !, call(H,In,Out).



trim_stop(S,O):- sub_string(S, N, 1, 0, Last), 
  (Last = "." -> sub_string(S, 0, N, 1, O); 
     ((Last="\n";Last="\r";Last=" ") -> (sub_string(S, 0, N, 1, Before),trim_stop(Before,O)) ; S=O)).

clause_to_string(T,S):- 
 with_output_to(string(S0), 
  prolog_listing:portray_clause(current_output,T,
    [portrayed(false),partial(true),nl(false),fullstop(false),singletons(false)])),!,
 trim_stop(S0,S).

print_e_to_string(T, Ops, S):- member(':-', Ops), !, clause_to_string(T,S).
print_e_to_string(T, Ops, S):- member('-->', Ops), !, clause_to_string(T,S).
print_e_to_string(T, Ops, S):- member('<-', Ops), !, 
   subst(T,('<-'),(':-'),T0), 
   clause_to_string(T0,S0), !,
   mid_pipe(S0,str_repl(':-','<-'),S).
print_e_to_string(T, Ops, S):-  Ops \== [], (member('->', Ops);member('<->', Ops)),
   mid_pipe(T, [replcterm(('<->'),(':-')), replcterm(('->'),('-->'))],T0),
   clause_to_string(T0,S0),!,
   mid_pipe(S0, [str_repl(':-','<->'),str_repl('-->','->')],S).
print_e_to_string(T, Ops, S):-  member('<->', Ops), sformat(S0, '~p',[T]),
  mid_pipe(S0,str_repl('<->','<->\n  '),S).
/*.
ec_portray(','):-write(',').
user:portray(Nonvar):- nonvar(Nonvar), ec_portray(Nonvar).   */
print_e_to_string(T, _Ops, S):- 
  sformat(S, '~@',
    [(prolog_pretty_print:print_term(T, 
             [  % left_margin(1),
                                write_options([numbervars(true),
                                   quoted(true),
                                  portray(true)]),
                             %   max_length(120),
                 % indent_arguments(auto),
                  output(current_output)]),
                  flush_output)]).


to_ansi(C, [fg(C)]):- atom(C),!.
to_ansi([H|T],[H|T]):-!.
to_ansi(H,[H]).

:- export(pprint_ec/2).
pprint_ec(C, P):-
  pprint_ec_and_f(C, P, '~n').
:- export(pprint_ecp/2).
pprint_ecp(C, P):-
  pprint_ec_and_f(C, P, '.~n').

pprint_ec_and_f(C, P, AndF):-
  print_e_to_string(P, S), 
  to_ansi(C, C0), !, 
  ansi_format(C0, '~s', [S]),
  format(AndF), !.

user:portray(Term):- ec_portray_hook(Term).

ec_portray_hook(Term):- 
 setup_call_cleanup(flag(ec_portray, N, N+1), 
  ec_portray(N, Term),
  flag(ec_portray,_, N)).

ec_portray(_,Var):- var(Var),!,fail. % format('~p',[Var]),!.
ec_portray(_,'$VAR'(Atomic)):-  atom(Atomic), write(Atomic),!.
ec_portray(_,Term):- notrace(is_list(Term)),!,Term\==[], notrace(catch(text_to_string(Term,Str),_,fail)),!,format('"~s"',[Str]).
ec_portray(N,Term):- N=0, pprint_ec_no_newline(white, Term).


pprint_ec_no_newline(C, P):-
  print_e_to_string(P, S),
  to_ansi(C, C0), ansi_format(C0, '~s', [S]),
  flush_output.

print_e_to_string(P, S):- 
   get_operators(P, Ops),
   must(pretty_numbervars(P, T)), 
   with_op_cleanup(1200,xfx,(<->),
     with_op_cleanup(1200,xfx,(->),
       with_op_cleanup(1200,xfy,(<-),
          print_e_to_string(T, Ops, S)))).
   


get_op_restore(OP,Restore):- 
   findall(op(E,YF,OP),(member(YF,[xfx,xfy,yfx,fy,fx,xf,yf]),current_op(E,YF,OP)),List),
   Restore = maplist(call,List).
get_op_zero(OP,Zero):- 
   findall(op(0,YF,OP),(member(YF,[xfx,xfy,yfx,fy,fx,xf,yf])),List),
   Zero = maplist(call,List).

with_op_cleanup(_NewP,_YF,_OP,Goal):- !, Goal.
with_op_cleanup(NewP,YF,OP,Goal):-
   (current_op(OldP,YF,OP);OldP=0) -> 
   get_op_restore(OP,Restore),
   get_op_zero(OP,Zero),
   Setup = (Zero,op(NewP,YF,OP)),
   Cleanup = (op(OldP,YF,OP),Restore),
   scce_orig(Setup,Goal,Cleanup).

glean_data(Why, SL):- \+ compound(SL), !, dmsg(warn(glean_data(Why, SL))).
glean_data(Why, subsort(S1, S2)):- !, glean_data(Why, sort(S1)), glean_data(Why, sort(S2)), assert_gleaned(Why, subsort(S1, S2)).
glean_data(Why, sort(S)):- !, assert_gleaned(Why, sort(S)).
glean_data(Why, isa(E, S)):- !, assert_gleaned(Why, isa(E, S)).
glean_data(Why, SL):- SL=..[S, L], 
  \+ is_non_sort(S), is_list(L), !, 
  glean_data(Why, sort(S)), 
  maplist(glean_data(Why, hasInstance(S)), L).
glean_data(_, _).

%assert_gleaned(Why, sort(S)):-  !, call(Why, gleaned(sort(S))).
assert_gleaned(_Why, SS):-  asserta_if_new(gleaned(SS)).
%assert_gleaned(Why, SS):-  call(Why, gleaned(SS)).

glean_data(Why, hasInstance(S), E):- !, glean_data(Why, isa(E, S)).



process_e_stream_token(Why, Atom, S):- atom_concat(New, '!', Atom), !, process_e_stream_token(Why, New, S).
process_e_stream_token(Why, Type, S):- normalize_space(atom(A), Type), A\==Type, !, process_e_stream_token(Why, A, S).
process_e_stream_token(Why, Text, S):- \+ atom(Text), !, text_to_string(Text, String), atom_string(Atom,String), process_e_stream_token(Why, Atom, S).
process_e_stream_token(Why, function, S):- !, read_stream_until(S, [], `:`, Text), read_line_to_string_echo(S, String), 
  append(TextL, [_], Text), 
  e_read1(TextL, Value, _), 
  token_stringsss(String, Type), 
   ec_on_read(Why, (function(Value, Type))).

process_e_stream_token(Why, Type, S):- downcase_atom(Type, Event), memberchk(Event, [fluent, predicate, event]), !, 
   read_one_e_compound(S, Value), ec_on_read(Why, t(Event, Value)).
process_e_stream_token(Why, reified, S):- !, read_stream_until(S, [], ` `, Text), 
   text_to_string(Text, St), atom_concat('reified_', St, Type), !, process_e_stream_token(Why, Type, S).
process_e_stream_token(Why, Type, S):- read_line_to_string_echo(S, String), process_e_token_with_string(Why, Type, String).

process_e_token_with_string(Why, Type, String):- \+ is_non_sort(Type), atomics_to_string(VList, ',', String), VList \= [_], !, 
  maplist(process_e_token_with_string(Why, Type), VList).
process_e_token_with_string(_, _, ""):-!.
process_e_token_with_string(Why, Type, String):- token_stringsss(String, Out), ec_on_read(Why, t(Type, Out)).

token_stringsss("", []):-!.
token_stringsss(T, Out) :- if_string_replace(T, '  ', ' ', NewT), !, token_stringsss(NewT, Out).
token_stringsss(T, Out) :- if_string_replace(T, ': ', ':', NewT), !, token_stringsss(NewT, Out).
token_stringsss(T, Out) :- if_string_replace(T, ' :', ':', NewT), !, token_stringsss(NewT, Out).
token_stringsss(String, Out):- normalize_space(string(S), String), S\==String, !, token_stringsss(S, Out).
token_stringsss(String, VVList):- atomics_to_string(VList, ',', String), VList \= [_], remove_blanks_col(VList, VVList), !.
token_stringsss(String, col(VVList)):- atomics_to_string(VList, ':', String), VList \= [_], remove_blanks(VList, VVList), !.
token_stringsss(String, VVList):- atomics_to_string(VList, ' ', String), remove_blanks(VList, VVList), !.

remove_blanks_col(I, O):- remove_blanks(I, M),maplist(token_cols, M, O).

token_cols(String, col(VVList)):- atomics_to_string(VList, ':', String), VList \= [_], remove_blanks(VList, VVList), !.
token_cols(String,String).

remove_blanks([], []).
remove_blanks([''|I], O):- !, remove_blanks(I, O).
remove_blanks([E|I], O):- string(E), normalize_space(string(EE), E), E\==EE, !, remove_blanks([EE|I], O).
remove_blanks([E|I], O):- atom(E), normalize_space(atom(EE), E), E\==EE, !, remove_blanks([EE|I], O).
remove_blanks([E|I], O):- to_atomic_value(E, EE), E\==EE, !, remove_blanks([EE|I], O).
remove_blanks([E|I], [E|O]):- remove_blanks(I, O).
%process_e_stream_token(Why, Sort, S):- read_tokens_til_eol(S, ' ', Value), ec_on_read(Why, t(Sort, Value)).
%process_e_stream_token(Why, reified, " sort", S):- read_tokens_til_eol(S, Value), ec_on_read(Why, reified_sort(Value)).

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
   maplist(to_atomic_value, ValueA, Value).%, ec_on_read(Why, option(List)).
   %atomics_to_string(['[', SS, ']'], ' ', NewList), e_from_atom(NewList, Value).

to_atomic_value(A, N):- number(A), !, N=A.
to_atomic_value(A, N):- normalize_space(atom(S), A), S\==A, !, to_atomic_value(S, N).
to_atomic_value(A, N):- atom_number(A, N).
to_atomic_value(A, A).

:- meta_predicate(read_stream_until(+,+,*,-)).
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
read_stream_until_true(S, Buffer, Pred, Codes):- get_code(S, Char), 
  (nb_current(e_echo,nil) -> true; put(Char)),
  (call(Pred, Char) -> notrace(append(Buffer, [Char], Codes)) ; 
  (notrace(append(Buffer, [Char], NextBuffer)), read_stream_until_true(S, NextBuffer, Pred, Codes))).
  



% in_space_cmt(Goal):- call_cleanup(prepend_each_line(' % ', Goal), echo_format('~N', [])).
in_space_cmt(Goal):- setup_call_cleanup(echo_format('~N /*~n', []), Goal, echo_format('~N*/~n', [])).


echo_till_eol(S):- read_line_to_string(S, String), echo_format('~s~n', [String]).

read_line_to_string_echo(S, String):- read_line_to_string(S, String), echo_format('~s',String).
  
echo_flush:- flush_output.
:- export(echo_format/1).
echo_format(S):- echo_flush, echo_format(S,[]).
:- export(echo_format/2).
echo_format(_Fmt, _Args):- t_l:block_comment_mode(Was), Was==invisible, !.
echo_format(Fmt, Args):- t_l:block_comment_mode(_), t_l:echo_mode(echo_file), !, format(Fmt, Args), flush_output.
echo_format(Fmt, Args):- t_l:echo_mode(echo_file), !, format(Fmt, Args), flush_output.
echo_format(_Fmt, _Args):- t_l:echo_mode(skip(_)), !.
echo_format(Fmt, Args):- format(Fmt, Args), flush_output, !.
%echo_format(_Fmt, _Args).


/*
process_e_stream(Why, S):- must((read_term(S, T, [variable_names(Vs)]), put_variable_names( Vs))), 
  call(b_setval, '$variable_names', Vs), b_setval('$term', T), 
  (t_l:echo_mode(skip(items)) -> true ; write_stream_item(user_error, T)), !, 
  flush_output(user_error), 
  must(visit_script_term(T)), !, 
  echo_format('~N', []), !.

write_stream_item(Out, T):- 
  flush_output, 
  format(Out, '~N~n', []), 
  must(with_output_to(Out, portray_clause_w_vars(T))), 
  format(Out, '~N~n', []), !, flush_output(Out).


*/
   


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

