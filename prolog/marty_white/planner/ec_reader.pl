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

trim_off_whitepace(S):- removed_one_ws(S),!,trim_off_whitepace(S).
trim_off_whitepace(_).

%process_stream_peeked213(S," ;"):- !, till_eol(S).
process_stream_peeked213(S,";"):- !, read_line_to_string(S,Comment),echo_format('~N%~s~n',[Comment]).
process_stream_peeked213(S,"["):- !, read_stream_until(S,[],`]`,Codes), read_n_save_vars(Codes).
process_stream_peeked213(S,"{"):- !, read_stream_until(S,[],`}`,Codes), read_n_save_vars(Codes).
process_stream_peeked213(S,"#!"):- !, till_eol(S).

read_n_save_vars(Codes):- ec_read3(Codes,VarNames,_Vs),asserta(temp_varnames(VarNames)).
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

process_ec_stream_meta_peek(S) :- notrace(removed_one_ws(S)), process_ec_stream_meta_peek(S).
process_ec_stream_meta_peek(S) :- (peek_string(S,2,W);peek_string(S,1,W)),process_stream_peeked213(S,W),!.


%process_ec_stream(S):- assertion(stream_property(S,input)).
process_ec_stream(S):- notrace(at_end_of_stream(S)),!,nop(visit_script_term_pre_expanded(end_of_file)).
process_ec_stream(S):- process_ec_stream_meta_peek(S), !, process_ec_stream(S).
process_ec_stream(S):-  
   OR = [to_lower('.'),to_lower('('), space, end_of_line],
   read_stream_until_true(S,[],char_type_inverse(Was,or(OR)), Text),
   unpad_codes(Text,Codes), last(Codes,Last),
   continue_process_ec_stream(S,Codes,Last,Was).
/*
process_ec_stream(S):- must((read_term(S,T,[variable_names(Vs)]),put_variable_names( Vs))),
  call(b_setval,'$variable_names',Vs), b_setval('$term',T), 
  (t_l:echo_mode(skip(items)) -> true ; write_stream_item(user_error,T)),!,
  flush_output(user_error),
  must(visit_script_term(T)),!,
  echo_format('~N',[]),!.

*/
code_type_eot(Last):- (char_type(Last,to_lower('.'));char_type(Last,to_lower(')'))).

continue_process_ec_stream(S,Codes, Last, space):- atom_codes(Token, Codes), assertion(char_type(Last,alpha)),
   process_ec_stream_token(Token,S),!.
continue_process_ec_stream(S, NextCodes, Last, _CanBe ):-  !, % to_lower('(') | end_of_line
   cont_one_ec_compound(S, NextCodes, Last,Term), process_out(Term).

unpad_codes(Text,Codes):- text_to_string(Text,String),normalize_space(codes(Codes),String).
  
ec_from_atom(String,Term):- ec_read1(String,Term,_). 
  
  
ec_read3(String,Term,Vars):- 
 upcased_functors(notrace(catch(read_term_from_atom(String,Term,
  [var_prefix(true),variable_names(Vars),
   module(ecread)]),_,fail))),!,
  maplist(ignore,Vars).

:- dynamic(temp_varnames/1).
insert_vars(Term,[],Term,[]).
insert_vars(Term0,[V|LL],Term,[V=VV|Has]):-
  insert1_var(Term0,V,VV,Term1),
  insert_vars(Term1,LL,Term,Has).
:- use_module(library(hybrid_db/portray_vars)).


insert1_var(Term0,V,VV,Term1):- 
  debug_var(V,VV), 
  subst(Term0,V,VV,Term1).

map_callables(_,Term0,Term):- \+ callable(Term0), !, Term0=Term.
map_callables(_,Term0,Term):- []== Term0,!, Term =[].
%map_callables(Call,Term0,Term):- atom(Term0),!,call(Call,Term0,Term).
map_callables(_Call,Term0,Term):- \+ compound(Term0),!,Term0=Term.
map_callables(_, '$VAR'(HT),'$VAR'(HT)):-!.
map_callables(Call,[H|T],[HTerm|TTerm]):- map_callables(Call,H,HTerm),map_callables(Call,T,TTerm),!.
map_callables(Call,  '$'(F,A), '$'(FF,AA)):- A==[],[] = AA,!,call(Call, F, FF).
map_callables(Call,  '$'(F,[A]), '$'(F,[AA])):- !, map_callables(Call,A,AA).
map_callables(Call,  '$'(F,A), '$'(FF,AA)) :- call(Call,F,FF), maplist(map_callables(Call),A,AA),!.
map_callables(Call, HT, HTTerm):- !, 
 compound_name_arguments(HT,F,L),
 map_callables(Call, '$'(F,L),'$'(FF,LL)),
 compound_name_arguments(HTTerm,FF,LL).


ec_read1(String,Term,Vs):- 
  ec_read3(String,Term0,_Vars),!,
  findall(E,(temp_varnames(L), member(E,L)), LL),
  sort(LL,LLS),
  insert_vars(Term0,LLS,Term1,Vs),!,
  pretty_numbervars(Term1,Term2),
  map_callables(unCamelcase,Term2,Term),!.
  

  %retractall(temp_varnames(_)).
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

cont_one_ec_compound(S,InCodes,WasLast,Term):- process_ec_stream_meta_peek(S), cont_one_ec_compound(S,InCodes,WasLast,Term).
cont_one_ec_compound(S,InCodes,_WasLast,Term):- 
   read_stream_until_true(S,InCodes,char_type_inverse(_Was,or([to_lower('.'),end_of_line])),Text),
   unpad_codes(Text,Codes),last(Codes,Last),
   cont_one_ec_compound(S,Codes,Last,Term).

is_special(range).
is_special(option).
is_special(reified).
is_special(completion).
%predicate, option range load fluent event noninertial xor completion

% process_out(load(SS)):- !, convert_ec_to_pl(file(SS),current_output).
process_out(t(X,Y)):- atom(X), is_list(Y), is_special(X), SS=..[X|Y], process_out(SS).
process_out(t(X,Y)):- atom(X), SS=..[X,Y], process_out(SS).
process_out(S):- glean_data(S), process_outward(S).
process_outward(SS):- pretty_numbervars(SS,SS1), format('~N~p.~n',[SS1]),retractall(temp_varnames(_)).

glean_data(SL):- \+ compound(SL),!.
glean_data(sort(S1,S2)):- !, glean_data(sort(S1)),glean_data(sort(S2)).
glean_data(sort(S)):- !, assert_gleaned(sort(S)).
glean_data(isa(E,S)):- !, assert_gleaned(isa(E,S)).
glean_data(SL):- SL=..[S,L], \+ is_special(S), is_list(L), !, glean_data(sort(S)),maplist(glean_data(hasInstance(S)),L).
glean_data(_).

assert_gleaned(SS):-  asserta(gleaned(SS)),pretty_numbervars(SS,SS1), format('~N% ~p.~n',[gleaned(SS1)]).

glean_data(hasInstance(S),E):- !,glean_data(isa(E,S)).



process_ec_stream_token(load,S):- read_line_to_string(S,String),normalize_space(atom(SS),String),
   process_out(load(SS)).
process_ec_stream_token(option,S):- !, read_tokens_til_eol(S,' ',Value),process_out(t(option,Value)).
process_ec_stream_token(sort,S):- read_tokens_til_eol(S,':',Value),SS=..[sort|Value],process_out(SS).
process_ec_stream_token(Event,S):- memberchk(Event,[fluent,predicate,event]),!,
   read_one_ec_compound(S,Value),
   process_out(t(Event,Value)).
process_ec_stream_token(Sort,S):- read_tokens_til_eol(S,' ',Value),process_out(t(Sort,Value)).
%process_ec_stream_token(reified," sort",S):- read_tokens_til_eol(S,Value),process_out(reified_sort(Value)).

/*
*/
read_tokens_til_eol(S,Splitter,Value):-
  trim_off_whitepace(S), !, read_line_to_string(S,String), 
   text_to_string(String,RString),
   normalize_space(string(SS),RString),
   atomic_list_concat(ValueA,Splitter,SS),
   maplist(to_atomic_value,ValueA,Value).%,process_out(option(List)).
   %string_list_concat(['[',SS,']'],' ',NewList),ec_from_atom(NewList,Value).

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

read_stream_until_true(S,Buffer,Pred,Buffer):- at_end_of_stream(S),ignore(call(Pred,10)),!.
read_stream_until_true(S,Buffer,Pred,Codes):- get_code(S,Char), 
  (call(Pred,Char) -> append(Buffer,[Char],Codes) ; 
  (append(Buffer,[Char],NextBuffer),read_stream_until_true(S,NextBuffer,Pred,Codes))).
  



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



