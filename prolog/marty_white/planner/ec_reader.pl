/*
% NomicMUD: A MUD server written in Prolog
%
% Some parts used Inform7, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
% 
% July 10,1996 - John Eikenberry 
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

:- thread_local(t_l:each_file_term/1).
:- thread_local(t_l:block_comment_mode/1).
:- thread_local(t_l:echo_mode/1).

:- meta_predicate now_doing(1,?).
:- meta_predicate each_doing(1,?).
:- meta_predicate doing(1,*).


till_eof(In) :-
        repeat,
            (   at_end_of_stream(In)
            ->  !
            ;   (read_pending_codes(In, Chars, []),
                (t_l:echo_mode(echo_file) ->
                  echo_format('~s',[Chars]);
                  true),
                fail)
            ).

%:- use_module(library(logicmoo_startup)).
:- use_module(library(file_utils/filestreams)).

%:- initialization(ec_reader_test,main).
ec_reader_test :- convert_ec_to_pl('ec_reader_test.e',user_output).

:- thread_local(t_l:sreader_options/2).

ec_open_input(F,S):- open(F,read,S).

convert_ec_to_pl(FileIn,Out):- notrace( \+ is_stream(Out)),!,
  setup_call_cleanup(open(Out,write,Outs),
     convert_ec_to_pl(FileIn,Outs),
     close(Outs)).
convert_ec_to_pl(In,Outs):- notrace( \+ is_stream(In)),!,  
  setup_call_cleanup(ec_open_input(In,Ins),
     convert_ec_to_pl(Ins,Outs),
     close(Ins)).
convert_ec_to_pl(Ins,Outs):- \+ current_output(Outs), !,
   with_output_to(Outs,convert_ec_to_pl(Ins,current_output)).


convert_ec_to_pl(Ins,Outs):- assertion(stream_property(Ins,input)),
    assertion(stream_property(Outs,output)),fail.

convert_ec_to_pl(Ins,_Outs):- 
  repeat,
  once(process_ec_stream(Ins)),
  notrace(at_end_of_stream(Ins)),!.


:- op(900,fx,ecread:'!').
:- op(1000,xfy,ecread:'&').
:- op(1050,xfy,ecread:'->').
:- op(1050,xfy,ecread:'<->').
:- op(1100,xfy,ecread:'|').



removed_one_ws(S):-
  peek_code(S,W),char_type(W,white),\+ char_type(W,end_of_line),get_code(S,W),echo_format('~s',[[W]]).

removed_n_chars(_S,N):- N<1,!.
removed_n_chars(S,N):- get_code(S,_),Nm1 is N-1, removed_n_chars(S,Nm1).

trim_off_whitepace(S):- repeat, \+ removed_one_ws(S).

%process_stream_peeked213(S," ;"):- !, till_eol(S).
process_stream_peeked213(S,";"):- !, read_line_to_string(S,Comment),echo_format('~N%~s~n',[Comment]).
process_stream_peeked213(S,"["):- !, read_stream_until(S,[],`]`,Codes), read_n_save_vars(Codes).
process_stream_peeked213(S,"{"):- !, read_stream_until(S,[],`}`,Codes), read_n_save_vars(Codes).
process_stream_peeked213(S,"#!"):- !, till_eol(S).

read_n_save_vars(Codes):- ec_read3(Codes,VarNames,_Vs),asserta(etmp:temp_varnames(VarNames)).
/*


end_of_file.


      [fluent,time]
      (HoldsAt(fluent,time) &
       !ReleasedAt(fluent,time+1) &
       !({event} Happens(event,time) & Terminates(event,fluent,time))) ->
      HoldsAt(fluent,time+1).

      [fluent,time]
   (HoldsAt(fluent,time) &
    !ReleasedAt(fluent,time+1) &
    !({event} Happens(event,time) & Terminates(event,fluent,time))) ->
   HoldsAt(fluent,time+1).

   [fluent,time]
   (!HoldsAt(fluent,time) &
    !ReleasedAt(fluent,time+1) &
    !({event} Happens(event,time) & Initiates(event,fluent,time))) ->
   !HoldsAt(fluent,time+1).

   [fluent,time]
   (!ReleasedAt(fluent,time) &
    !({event} Happens(event,time) & Releases(event,fluent,time))) ->
   !ReleasedAt(fluent,time+1).

   [fluent,time]
   (ReleasedAt(fluent,time) &
    !({event} Happens(event,time) &
      (Initiates(event,fluent,time) |
       Terminates(event,fluent,time)))) ->
   ReleasedAt(fluent,time+1).

*/
upcased_functors(G):- 
 notrace((allow_variable_name_as_functor = N,
   current_prolog_flag(N,Was))),!,
   setup_call_cleanup(notrace(set_prolog_flag(N,true)),
      G,
      notrace(set_prolog_flag(N,Was))).


%% process_ec_stream( ?S) is det.
%
% Process file stream input
%
process_ec_stream_meta_peek(S) :- notrace(removed_one_ws(S)),!.
process_ec_stream_meta_peek(S) :- (peek_string(S,2,W);peek_string(S,1,W)),process_stream_peeked213(S,W),!.


%process_ec_stream(S):- assertion(stream_property(S,input)).
process_ec_stream(S):- notrace(at_end_of_stream(S)),!,nop(visit_script_term_pre_expanded(end_of_file)).
process_ec_stream(S):- process_ec_stream_meta_peek(S),!.
process_ec_stream(S):- 
   OR = [to_lower('.'),to_lower('('), end_of_line, space],
   read_stream_until_true(S,[],char_type_inverse(Was,or(OR)), Text),
   unpad_codes(Text,Codes), must(continue_process_ec_stream(S,Codes,Was)),!.

process_ec_stream(S):- read_line_to_string(S,Comment),echo_format('~N%RROOR:  ~s~n',[Comment]), break.

/*
process_ec_stream(S):- must((read_term(S,T,[variable_names(Vs)]),put_variable_names( Vs))),
  call(b_setval,'$variable_names',Vs), b_setval('$term',T), 
  (t_l:echo_mode(skip(items)) -> true ; write_stream_item(user_error,T)),!,
  flush_output(user_error),
  must(visit_script_term(T)),!,
  echo_format('~N',[]),!.

*/
code_type_eot(Last):- (char_type(Last,to_lower('.'));char_type(Last,to_lower(')'))).
   
% continue_process_ec_stream(_S, [], space):- !.
continue_process_ec_stream(_S, [], end_of_line):- !.
continue_process_ec_stream(S, Codes, space):- !, atom_codes(Token, Codes), last(Codes,Last), 
   assertion(char_type(Last,alpha)),
   trim_off_whitepace(S),!, 
   process_ec_stream_token(Token,S),!.
continue_process_ec_stream(S, NextCodes, _CanBe ):-  !, % to_lower('(') | end_of_line
   last(NextCodes,Last),
   cont_one_ec_compound(S, NextCodes, Last,Term), process_out(Term).

unpad_codes(Text,Codes):- text_to_string(Text,String),normalize_space(codes(Codes),String).
  
ec_from_atom(String,Term):- ec_read1(String,Term,_). 
  
  
ec_read3(String,Term,Vars):- 
 upcased_functors(notrace(catch(read_term_from_atom(String,Term,
  [var_prefix(true),variable_names(Vars),
   module(ecread)]),_,fail))),!,
  maplist(ignore,Vars).

:- use_module(library(hybrid_db/portray_vars)).
:- dynamic(etmp:temp_varnames/1).
insert_vars(Term,[],Term,[]).
insert_vars(Term0,[V|LL],Term,[V=VV|Has]):-
  insert1_var(Term0,V,VV,Term1),
  insert_vars(Term1,LL,Term,Has).


insert1_var(Term0,V,VV,Term1):- 
  debug_var(V,VV), 
  subst(Term0,V,VV,Term1).

special_directive(manualrelease).
special_directive(completion).
special_directive(ignore).
special_directive('!').
special_directive('neg').

map_callables(_,Term0,Term):- \+ callable(Term0), !, Term0=Term.
map_callables(_,Term0,Term):- []== Term0,!, Term =[].
%map_callables(Call,Term0,Term):- atom(Term0),!,call(Call,Term0,Term).
map_callables(_Call,Term0,Term):- \+ compound(Term0),!,Term0=Term.
map_callables(_, '$VAR'(HT),'$VAR'(HT)):-!.
map_callables(Call,[H|T],[HTerm|TTerm]):- !, map_callables(Call,H,HTerm),map_callables(Call,T,TTerm),!.
map_callables(Call,  '$'(F,A), '$'(FF,AA)):- A==[],[] = AA,!,call(Call, F, FF).
map_callables(Call,  '$'(F,[A]), '$'(F,[AA])):- \+ special_directive(F), !, map_callables(Call,A,AA).
map_callables(Call,  '$'(F,A), '$'(FF,AA)) :- call(Call,F,FF), maplist(map_callables(Call),A,AA),!.
map_callables(Call, HT, HTTerm):- !, 
 compound_name_arguments(HT,F,L),
 map_callables(Call, '$'(F,L),'$'(FF,LL)),
 compound_name_arguments(HTTerm,FF,LL).

my_unCamelcase(X,Y):- atom(X), fix_predname(X,Y),!.
my_unCamelcase(X,Y):- unCamelcase(X,Y).

fix_predname(~,neg).
fix_predname(!,neg).
fix_predname(F,neg):- downcase_atom(F,not).
fix_predname(F,holds_at):- downcase_atom(F,holdsat).
fix_predname(F,Happens):- builtin_pred(Happens),downcase_atom(F,Happens),!.

fix_ec_terms(Term0,Term,Vs):- 
  findall(E,(etmp:temp_varnames(L), member(E,L)), LL),
  sort(LL,LLS),
  insert_vars(Term0,LLS,Term1,Vs),!,
  pretty_numbervars(Term1,Term2),
  my_fix_case(Term2,Term),!.

my_fix_case(Term2,Term):- map_callables(my_unCamelcase,Term2,Term).

ec_read1(String,Term,Vs):- 
  ec_read3(String,Term0,_Vars),!,
  fix_ec_terms(Term0,Term,Vs).




  %retractall(etmp:temp_varnames(_)).
ec_read1(T,Term,Vars):- ec_read2(T,Term,Vars),!.

ec_read2(T,Term,Vars):- 
   cleanout(T,'{','}',S3,SSS), read_n_save_vars(S3),
   ec_read2(SSS,Term,Vars).
ec_read2(T,Term,Vars):- 
   cleanout(T,'[',']',S3,SSS), read_n_save_vars(S3),
   ec_read2(SSS,Term,Vars).
ec_read2(T,Term,Vars):- ec_read3(T,Term,Vars).
   
cleanout(T,B,E,S3,SSS):-
 text_to_string(T,S1),
 atomic_list_concat([A1,A2|Rest],B,S1),
 atomic_list_concat(['',A2|Rest],B,S2),
 setup_call_cleanup(
   open_string(S2,In),
   ((read_stream_until(In,[],E,S3), S3\==[],
     read_pending_codes(In,RestCodes,[]),
     text_to_string(RestCodes,S4))),
     close(In)),!,
 atomic_list_concat([A1,S4],'',SSS).


 
   
%ec_read1(String,Term,Vars):- catch(read_term_from_atom(String,Term,[var_prefix(true),variable_names(Vars),module(ecread),syntax_errors(error)]),_,fail),!.
%read_prolog_like(Term):- 
/*
(!ReleasedAt(fluent,time) &
    !({event} Happens(event,time) & Releases(event,fluent,time))) ->
   !ReleasedAt(fluent,time+1) 
*/
read_one_ec_compound(S,Term):- 
   read_stream_until_true(S,[],char_type_inverse(_Was,or([to_lower('.'),end_of_line])),Text),
   unpad_codes(Text,Codes),last(Codes,Last),
   cont_one_ec_compound(S,Codes,Last,Term).

cont_one_ec_compound(_S,Text,Last,Term):- code_type_eot(Last),
   unpad_codes(Text,Codes),ec_from_atom(Codes,Term).

cont_one_ec_compound(S,InCodes,WasLast,Term):- process_ec_stream_meta_peek(S),!,cont_one_ec_compound(S,InCodes,WasLast,Term).
cont_one_ec_compound(S,InCodes,_WasLast,Term):- 
   read_stream_until_true(S,InCodes,char_type_inverse(_Was,or([to_lower('.'),end_of_line])),Text),
   unpad_codes(Text,Codes),last(Codes,Last),
   cont_one_ec_compound(S,Codes,Last,Term).

is_special(range).
is_special(sort).
is_special(option).
is_special(reified).
is_special(completion).
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
builtin_pred(executable).
builtin_pred(sort).
builtin_pred(predicate).
builtin_pred(function).
builtin_pred(event).
builtin_pred(initially).

% builtin_pred(releasedAt).



% process_out(load(SS)):- !, convert_ec_to_pl(file(SS),current_output).
process_out(t(X,[Y])):- !, process_out(t(X,Y)).
process_out(t(X,Y)):- atom(X), is_list(Y), is_special(X), SS=..[X|Y], process_out(SS).
process_out(t(X,Y)):- atom(X), SS=..[X,Y], process_out(SS).
process_out(SL):- my_fix_case(SL,SO) -> SL\=@=SO, !, process_out(SO).
process_out(S):- must(glean_data(S)), must(process_outward(S)),!.

process_outward(SS):- must(pretty_numbervars(SS,SS1)),flush_output, ansi_format([fg(yellow)],'~n~N~p.~n',[SS1]),retractall(etmp:temp_varnames(_)).

glean_data(SL):- \+ compound(SL),!.
glean_data(sort(S1,S2)):- !, glean_data(sort(S1)),glean_data(sort(S2)).
glean_data(sort(S)):- !, assert_gleaned(sort(S)).
glean_data(isa(E,S)):- !, assert_gleaned(isa(E,S)).
glean_data(SL):- SL=..[S,L], \+ is_special(S), is_list(L), !, glean_data(sort(S)),maplist(glean_data(hasInstance(S)),L).
glean_data(_).

assert_gleaned(SS):-  asserta(gleaned(SS)),pretty_numbervars(SS,SS1),flush_output, ansi_format([fg(yellow)],'~n~N% ~p.~n',[gleaned(SS1)]).

glean_data(hasInstance(S),E):- !, glean_data(isa(E,S)).



process_ec_stream_token(Atom,S):- atom_concat(New,'!',Atom),!,process_ec_stream_token(New,S).
process_ec_stream_token(Type,S):- normalize_space(atom(A),Type), A\==Type,!,process_ec_stream_token(A,S).
process_ec_stream_token(Text,S):- \+ atom(Text),!,text_to_atom(Text,Atom), process_ec_stream_token(Atom,S).
process_ec_stream_token(function,S):- !,read_stream_until(S,[],`:`, Text),read_line_to_string(S,String),
  append(TextL,[_],Text),
  ec_read1(TextL,Value, _),
  token_stringsss(String,Type),
   process_out((function(Value,Type))).

process_ec_stream_token(Type,S):- downcase_atom(Type, Event), memberchk(Event,[fluent,predicate,event]),!,
   read_one_ec_compound(S,Value), process_out(t(Event,Value)).
process_ec_stream_token(reified,S):- !,read_stream_until(S,[],` `, Text),
   text_to_string(Text,St), atom_concat('reified_',St,Type),!,process_ec_stream_token(Type,S).
process_ec_stream_token(Type,S):- read_line_to_string(S,String),process_ec_token_with_string(Type,String).

process_ec_token_with_string(Type,String):- atomics_to_string(VList,',',String),  VList \= [_], !, 
   maplist(process_ec_token_with_string(Type),VList).
process_ec_token_with_string(_,""):-!.
process_ec_token_with_string(Type,String):- token_stringsss(String,Out),process_out(t(Type,Out)).

token_stringsss("",[]):-!.
token_stringsss(String,Out):- normalize_space(string(S),String), S\==String,!,token_stringsss(S,Out).
token_stringsss(String,VVList):- atomics_to_string(VList,':',String), VList \= [_], remove_blanks(VList,VVList),!.
token_stringsss(String,VVList):- atomics_to_string(VList,' ',String), remove_blanks(VList,VVList),!.
  

remove_blanks([],[]).
remove_blanks([''|I],O):- !, remove_blanks(I,O).
remove_blanks([E|I],O):- string(E),normalize_space(string(EE),E), E\==EE,!,remove_blanks([EE|I],O).
remove_blanks([E|I],O):- atom(E),normalize_space(atom(EE),E), E\==EE,!,remove_blanks([EE|I],O).
remove_blanks([E|I],O):- to_atomic_value(E,EE),E\==EE,!,remove_blanks([EE|I],O).
remove_blanks([E|I],[E|O]):- remove_blanks(I,O).
%process_ec_stream_token(Sort,S):- read_tokens_til_eol(S,' ',Value),process_out(t(Sort,Value)).
%process_ec_stream_token(reified," sort",S):- read_tokens_til_eol(S,Value),process_out(reified_sort(Value)).

/*
*/
read_tokens_til_eol(S,Splitter,Value):-
  trim_off_whitepace(S), !, read_line_to_string(S,String), 
   text_to_string(String,SS),
   subsplit(Splitter,SS,Value).

subsplit([],String,Value):- to_atomic_value(String,Value).
subsplit([A|B],String,Value):-  
   normalize_space(string(SS),String),
   atomic_list_concat(ValueS,A,SS),
   maplist(subsplit(B),ValueS,Value).


subsplit(SS,Splitter,Value):- 
   atomic_list_concat(ValueA,Splitter,SS),
   maplist(to_atomic_value,ValueA,Value).%,process_out(option(List)).
   %atomics_to_string(['[',SS,']'],' ',NewList),ec_from_atom(NewList,Value).

to_atomic_value(A,N):- normalize_space(atom(S),A), S\==A,!,to_atomic_value(S,N).
to_atomic_value(A,N):- atom_number(A,N).
to_atomic_value(A,A).

read_stream_until(S,Buffer,[Until],Codes):- !,name(N,[Until]),char_code(N,UntilCode),!,
 read_stream_until_true(S,Buffer,==(UntilCode),Codes).
read_stream_until(S,Buffer,UntilCode,Codes):- integer(UntilCode),!,
 read_stream_until_true(S,Buffer,==(UntilCode),Codes).
read_stream_until(S,Buffer,Until,Codes):- atom(Until), atom_length(Until,1), char_code(Until,UntilCode),!,
 read_stream_until_true(S,Buffer,==(UntilCode),Codes).
read_stream_until(S,Buffer,Until,Codes):- read_stream_until_true(S,Buffer,Until,Codes).

char_type_inverse(Type, or(TypeList),Code):- !, member(E,TypeList),char_type_inverse(Type, E,Code).
char_type_inverse(Type, [Spec],Code):- !,char_type_inverse(Type, Spec,Code).
char_type_inverse(Type, [Spec|List],Code):- !,char_type_inverse(_, Spec,Code),char_type_inverse(Type, List,Code).
char_type_inverse(Type, Spec, Code):- char_type(Code,Spec), Type=Spec.

read_stream_until_true(S,Buffer,Pred,Buffer):- at_end_of_stream(S),!,ignore(call(Pred,10)).
read_stream_until_true(S,Buffer,Pred,Codes):- get_code(S,Char),  
  (call(Pred,Char) -> notrace(append(Buffer,[Char],Codes)) ; 
  (notrace(append(Buffer,[Char],NextBuffer)),read_stream_until_true(S,NextBuffer,Pred,Codes))).
  



% in_space_cmt(Goal):- call_cleanup(prepend_each_line(' % ',Goal),echo_format('~N',[])).
in_space_cmt(Goal):- setup_call_cleanup(echo_format('~N /*~n',[]),Goal,echo_format('~N*/~n',[])).


till_eol(S):- read_line_to_string(S,String),
  (t_l:echo_mode(skip(_))->true ; (echo_format('~N~s~n',[String]))).

  

echo_format(_Fmt,_Args):- flush_output, t_l:block_comment_mode(Was),Was==invisible,!.
echo_format(Fmt,Args):- t_l:block_comment_mode(_),t_l:echo_mode(echo_file),!,format(Fmt,Args),flush_output.
echo_format(Fmt,Args):- t_l:echo_mode(echo_file),!,format(Fmt,Args),flush_output.
echo_format(Fmt,Args):- format(Fmt,Args),flush_output,!.
echo_format(_Fmt,_Args).


write_stream_item(Out,T):- 
  flush_output,
  format(Out,'~N~n',[]),
  must(with_output_to(Out,portray_clause_w_vars(T))),
  format(Out,'~N~n',[]),!,flush_output(Out).



