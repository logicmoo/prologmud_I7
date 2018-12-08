/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Bits and pieces:
%
% LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
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

% :- user:ensure_loaded(library(poor_bugger)).

:- meta_predicate reset_prolog_flag(0,*,*,*).
:- meta_predicate reset_prolog_flag(0,*,*).
:- meta_predicate system_default_debug(0).

reset_prolog_flag(YN, Name, SystemValue):- 
  YN -> set_prolog_flag(Name, SystemValue) ; true.

reset_prolog_flag(YN, Name, SystemValue, OverrideValue):-
  YN -> set_prolog_flag(Name, SystemValue)
   ;  set_prolog_flag(Name, OverrideValue).

system_default_debug(YN):-
  reset_prolog_flag(YN, answer_format, '~p', '~q'), 
  reset_prolog_flag(YN, answer_write_options, [quoted(true), portray(true), max_depth(10), spacing(next_argument)], 
   [quoted(true), portray(true), max_depth(4), spacing(next_argument)]), 
  reset_prolog_flag(YN, debugger_write_options, [quoted(true), portray(true), max_depth(10), attributes(portray), spacing(next_argument)], 
   [quoted(true), portray(true), max_depth(4), attributes(portray), spacing(next_argument)]), 
  reset_prolog_flag(YN, print_write_options, [portray(true), quoted(true), numbervars(true)], 
   [portray(true), quoted(true), numbervars(true)]), 

  reset_prolog_flag(YN, backtrace, true), 
  reset_prolog_flag(YN, backtrace_depth, 20, 2000), 
  reset_prolog_flag(YN, backtrace_goal_depth, 3, 4), 
  reset_prolog_flag(YN, backtrace_show_lines, true), 
  reset_prolog_flag(YN, debug, false, true), 
  reset_prolog_flag(YN, debug_on_error, true), 
  reset_prolog_flag(YN, debugger_show_context, false, true), 

  reset_prolog_flag(YN, gc, true), 

  reset_prolog_flag(YN, last_call_optimisation, true, false), 
  reset_prolog_flag(YN, optimise, false), 
  reset_prolog_flag(YN, optimise_debug, default), 

  reset_prolog_flag(YN, prompt_alternatives_on, determinism), 
  reset_prolog_flag(YN, toplevel_goal, default), 
  reset_prolog_flag(YN, toplevel_mode, backtracking), 
  reset_prolog_flag(YN, toplevel_residue_vars, false, true), 
  reset_prolog_flag(YN, toplevel_print_anon, true), 
  reset_prolog_flag(YN, toplevel_print_factorized, false, true), 
  reset_prolog_flag(YN, write_attributes, ignore),

  reset_prolog_flag(YN, warn_override_implicit_import, true), 
  reset_prolog_flag(YN, access_level, user),
  reset_prolog_flag(YN, sandboxed_load, false), 
  reset_prolog_flag(YN, save_history, true), 
  !.

:- system_default_debug(false).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
% :- nop(ensure_loaded('adv_debug')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module_transparent(dshow_call/1).
:- module_transparent(dshow_true/1).
:- module_transparent(dshow_fail/1).
:- module_transparent(dmust_tracing/1).
:- module_transparent(if_tracing/1).

:- meta_predicate(dmust_tracing(*)).
dmust_tracing(G):- notrace((tracing,cls)),!,dmust_det(G).
dmust_tracing(G):- call(G).
:- meta_predicate(if_tracing(*)).
if_tracing(G):- tracing -> notrace(G) ; true.


% '$hide'(Pred) :- '$set_predicate_attribute'(Pred, trace, false).
never_trace(_Spec):- prolog_load_context(reloading,true),!.
never_trace(Spec):- '$hide'(Spec),'$iso'(Spec),trace(Spec, -all).
:- call(ensure_loaded,library(lists)).
:- never_trace(lists:append(_,_,_)).
:- never_trace(lists:list_to_set/2).
:- never_trace(lists:member_(_,_,_)).
/*
:- never_trace(prolog_debug:assertion(_)).
*/

:- export(simplify_dbug/2).
simplify_dbug(G,GG):- \+ compound(G),!,GG=G.
simplify_dbug({O},{O}):- !.
simplify_dbug(List,O):-
 ( is_list(List) -> clip_cons(List,'...'(_),O) ;
 ( List = [_|_], append(LeftSide,Open,List),
  ((var(Open);Open \= [_|_])), !, assertion(is_list(LeftSide)),
 clip_cons(LeftSide,'...'(Open),O))).
simplify_dbug(G,GG):- compound_name_arguments(G,F,GL), F\==percept_props, !,
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


%:- never_trace(lists:member(_,_)).
%:- never_trace(lists:append(_,_,_)).
:- meta_predicate(dshow_call(*)).
dshow_call((G1,G2)):- !,dshow_fail(G1),dshow_fail(G2).
dshow_call(G):- simplify_dbug(G,GG), swi_soft_if_then(G,bugout1(success_dshow_call(GG)),(bugout1(failed_dshow_call(GG)),!,fail)).

:- meta_predicate(dshow_fail(*)).
dshow_fail('\\+'(G1)):- !, \+ dshow_true(G1).
dshow_fail(G):- simplify_dbug(G,GG), swi_soft_if_then(G, true , (bugout1(failed_dshow_call(GG)),!,fail)).

:- meta_predicate(dshow_true(*)).
dshow_true('\\+'(G1)):- !, \+ dshow_fail(G1).
dshow_true(G):- simplify_dbug(G,GG),  swi_soft_if_then(G, bugout1(success_dshow_call(GG)) , (!,fail)).

found_bug(S0,open_list(Open)) :- \+is_list(S0),
  get_open_segement(S0,Open).
found_bug(S0,duplicated_object(X,R,L)) :-
 append(Left,[prop(X,R)|_],S0),
 member(prop(X,L),Left).

get_open_segement(S0,Open):- append(Left,_,S0),is_list(Left),length(Left,N),N>2,!,append([_,_],S1,S0),get_open_segement(S1,Open).
get_open_segement(S0,S0).


check4bugs(Why, S0):- found_bug(S0,Bug), pprint(S0, always), pprint(check4bugs_found_bug(Why,Bug),always),throw(check4bugs_failed(Bug)).
 % TODO: emergency save of S0, either here or better yet, in a catch().
check4bugs(_, _).

  
:- multifile(user:portray/1).
:- dynamic(user:portray/1).
:- discontiguous(user:portray/1).
% user:portray





%:- set_prolog_flag(verbose_load,full).
:- set_prolog_flag(verbose,normal).
%:- set_prolog_flag(verbose_autoload,true).



% debug_var(_A,_Var):-!.
debug_var(X,Y):- notrace(catch(debug_var0(X,Y),_,fail)) -> true ; rtrace(debug_var0(X,Y)).

maybe_debug_var(X,Y):- notrace(maybe_debug_var0(X,Y)).
maybe_debug_var0(_,Y):- nonvar(Y),!.
maybe_debug_var0(X,_):- get_var_name(X,_),!.
maybe_debug_var0(X,Y):- (catch(debug_var0(X,Y),_,fail)) -> true ; rtrace(debug_var0(X,Y)).

debug_var(Sufix,X,Y):- notrace((flatten([X,Sufix],XS),debug_var(XS,Y))).

p_n_atom(Cmpd,UP):- sub_term(Atom,Cmpd),nonvar(Atom),\+ number(Atom), Atom\==[], catch(p_n_atom0(Atom,UP),_,fail),!.
p_n_atom(Cmpd,UP):- term_to_atom(Cmpd,Atom),p_n_atom0(Atom,UP),!.

filter_var_chars([58|X],[107, 119, 95|Y]):- filter_var_chars_trim_95(X,Y).
filter_var_chars([95|X],[95|Y]):- !, filter_var_chars_trim_95(X,Y).
filter_var_chars(X,Y):- filter_var_chars_trim_95(X,Y).



filter_var_chars_trim_95(X,Y):- filter_var_chars0(X,M),trim_95(M,Y),!.

trim_95([X],[X]).
trim_95([95|M],Y):-!, trim_95(M,Y).
trim_95([X|L],[100,X|Y]):- char_type(X,digit), trim_96(L,Y).
trim_95([X|L],[97,X|Y]):- \+ char_type(X,alpha), trim_96(L,Y).
trim_95(X,Y):- trim_96(X,Y).

trim_96([95],[]).
trim_96([],[]).
trim_96([95,95|M],Y):- trim_96([95|M],Y).
trim_96([X|M],[X|Y]):- trim_96(M,Y).



filter_var_chars0([],[]).


% WATN WHEN MAKING SYMBOLs...  `_` -> `__`

%  `-` -> `c45`
filter_var_chars0(`-`,`c45`):-!.
%  `*` -> `_xx_`
filter_var_chars0([42|T],[95,120,120,95|Rest]):-!,filter_var_chars0(T,Rest).
%  `%` -> `_pf_`
filter_var_chars0([37|T],[95,112, 102, 95| Rest]):-!,filter_var_chars0(T,Rest).
%  `-` -> `_`
filter_var_chars0([45|T],[95|Rest]):-!,filter_var_chars0(T,Rest).
%  `:` -> `_`
filter_var_chars0([42|T],[95,120,95|Rest]):-!,filter_var_chars0(T,Rest).
filter_var_chars0([H|T],[H|Rest]):-  code_type(H, prolog_identifier_continue),!,filter_var_chars0(T,Rest).
filter_var_chars0([H|T],Rest):- number_codes(H,Codes), filter_var_chars0(T,Mid),append([95, 99|Codes],[95|Mid],Rest).

atom_concat_some_left(L,R,LR):- atom_concat(L,R,LR),atom_length(R,Len),Len>0.
atom_concat_some_left(L,R,LR):- upcase_atom(L,L0),L\==L0,atom_concat(L0,R,LR),atom_length(R,Len),Len>0.
atom_concat_some_left(L,R,LR):- downcase_atom(L,L0),L\==L0,atom_concat(L0,R,LR),atom_length(R,Len),Len>0.

reduce_atomLR(L,R):- atom_concat_some_left('Cl_',LL,L),reduce_atomLR(LL,R).
reduce_atomLR(L,R):- atom_concat_some_left('U_',LL,L),reduce_atomLR(LL,R).
reduce_atomLR(L,R):- atom_concat_some_left('F_',LL,L),reduce_atomLR(LL,R).
reduce_atomLR(L,R):- atom_concat_some_left('Pf_',LL,L),reduce_atomLR(LL,R).
reduce_atomLR(L,R):- atom_concat_some_left('Kw_',LL,L),reduce_atomLR(LL,R).
reduce_atomLR(L,R):- atom_concat_some_left('Sys_',LL,L),reduce_atomLR(LL,R).
reduce_atomLR(L,L).

p_n_atom0(Atom,UP):- guess_textname(Atom,M),Atom\==M,!,p_n_atom0(M,UP).
p_n_atom0(Atom,UP):- atom(Atom),!,
  reduce_atomLR(Atom,AtomR),
  name(AtomR,[C|Was]),to_upper(C,U),filter_var_chars([U|Was],CS),name(UP,CS).
p_n_atom0(String,UP):- string(String),!,string_to_atom(String,Atom),!,p_n_atom0(Atom,UP).
p_n_atom0([C|S],UP):- !,notrace(catch(atom_codes(Atom,[C|S]),_,fail)),!,p_n_atom0(Atom,UP).

debug_var0(_,NonVar):-nonvar(NonVar),!.
debug_var0([C|S],Var):- notrace(catch(atom_codes(Atom,[C|S]),_,fail)),!,debug_var0(Atom,Var).
debug_var0([AtomI|Rest],Var):-!,maplist(p_n_atom,[AtomI|Rest],UPS),atomic_list_concat(UPS,NAME),debug_var0(NAME,Var),!.
debug_var0(Atom,Var):- p_n_atom(Atom,UP),  
  check_varname(UP),
  add_var_to_env_loco(UP,Var),!.


add_var_to_env_loco(UP,Var):- var(Var), get_var_name(Var,Prev),atomic(Prev),add_var_to_env_locovs_prev(UP,Prev,Var).
add_var_to_env_loco(UP,Var):-add_var_to_env(UP,Var).

add_var_to_env_locovs_prev(UP,Prev,_Var):- UP==Prev,!.
add_var_to_env_locovs_prev(UP,_Prev,_Var):- atom_concat_or_rtrace('_',_,UP),!.
add_var_to_env_locovs_prev(UP,_Prev,_Var):- atom_concat_or_rtrace(_,'_',UP),!.
add_var_to_env_locovs_prev(UP,_Prev,Var):-add_var_to_env(UP,Var).
add_var_to_env_locovs_prev(UP,Prev,Var):- atom_concat_or_rtrace('_',_,Prev),!,add_var_to_env(UP,Var).
add_var_to_env_locovs_prev(UP,Prev,Var):- atom_concat_or_rtrace(UP,Prev,New),add_var_to_env(New,Var).
add_var_to_env_locovs_prev(UP,_Prev,Var):- add_var_to_env(UP,Var).

check_varname(UP):- name(UP,[C|_]),(char_type(C,digit)->throw(check_varname(UP));true).
                        


resolve_char_codes('','_').
resolve_char_codes('pf','%').
%resolve_char_codes(C48,C):- notrace(catch((name(C48,[99|Codes]),number_codes(N,Codes),name(C,[N])),_,fail)),!,fail.
resolve_char_codes(C48,_):- notrace(catch((name(C48,[99|Codes]),number_codes(_,Codes)),_,fail)),!,fail.
resolve_char_codes(D1,N):- atom_concat('d',N,D1),notrace(catch(atom_number(N,_),_,fail)),!.
resolve_char_codes(C,CC):- atom_concat(C,'-',CC).

into_symbol_name(Atom,UPPER):- atomic(Atom),atomic_list_concat([Pkg|HC],'_',Atom),!,into_symbol_name([Pkg|HC],UPPER).
into_symbol_name(HC,UPPER):- maplist(resolve_char_codes,HC,RHC),atomics_to_string(RHC,'',STR),
   atom_trim_suffix(STR,'-',Trimed),string_upper(Trimed,UPPER),!.

% *PACKAGE* becomes xx_package_xx
% %MAKE-PACKAGE becomes pf_make_package

prologcase_name(I,O):-notrace(prologcase_name0(I,O)),assertion(O\=='').

prologcase_name0(String,Nonvar):-nonvar(Nonvar),!,prologcase_name(String,ProposedName),!,ProposedName==Nonvar.
prologcase_name0(String,ProposedName):- 
  string_lower(String,In),string_codes(In,Was),!,filter_var_chars(Was,CS),!,name(ProposedName,CS),!.


atom_concat_if_new(Prefix,Atom,NewAtom):-
  (atom_concat_or_rtrace(Prefix,_,Atom)-> NewAtom=Atom ; atom_concat_or_rtrace(Prefix,Atom,NewAtom)).


atom_trim_prefix(Root,Prefix,Result):- atom_concat(Prefix,Result,Root) -> true ; Result=Root.
atom_trim_suffix(Root,Suffix,Result):- atom_concat(Result,Suffix,Root) -> true ; Result=Root.

atom_concat_suffix('',Result,Result):-!.
atom_concat_suffix(Result,'',Result):-!.
atom_concat_suffix(Root,Suffix,Root):- atom_concat(_,Suffix,Root),!.
atom_concat_suffix(Root,Suffix,Result):- 
  atom_trim_prefix(Suffix,'_',Suffix2),
  atom_trim_suffix(Root,'_',Root2),
  atomic_list_concat([Root2,Suffix2],'_',Result),!.

shrink_lisp_strings(I,I).

make_pretty(I,O):- !,notrace((shrink_lisp_strings(I,O), make_pretty2(O))).
make_pretty2(O):- !,notrace((pretty1(O),pretty2(O),pretty3(O))).
%make_pretty(I,O):- is_user_output,!,shrink_lisp_strings(I,O), pretty1(O),pretty2(O),pretty3(O).
%make_pretty(I,O):- I=O, pretty1(O),pretty2(O),pretty3(O).

print_clause_plain(I):-
  current_prolog_flag(color_term, Was),
  make_pretty(I,O),
    setup_call_cleanup(set_prolog_flag(color_term, false),
     (nl,lcolormsg1((O))),
     set_prolog_flag(color_term, Was)).


%lcolormsg1(Msg):- mesg_color(Msg,Ctrl),!,ansicall_maybe(Ctrl,fmt9(Msg)).
lcolormsg1(Msg):- fmt9(Msg).

% print_clause_plain(C):- portray_clause_w_vars(O).


may_debug_var(_,_,V):- nonvar(V),!.
may_debug_var(_,_,V):- variable_name(V,_),!.
may_debug_var(L,_,_):- upcase_atom(L,L),!.
may_debug_var(L,R,V):- atom(L),atom_concat('f_',LL,L),may_debug_var(LL,R,V).
may_debug_var(L,R,V):- atomic_list_concat([_A1,A2,A3|AS],'_',L),atomic_list_concat([A2,A3|AS],'_',LL),may_debug_var(LL,R,V).
may_debug_var(L,R,V):- debug_var([L,R],V).

may_debug_var(_,V):- nonvar(V),!.
may_debug_var(_,V):- variable_name(V,_),!.
may_debug_var(R,V):- debug_var(R,V).

pretty1(H):- \+ compound(H),!.
pretty1(as_rest(Name, Rest, _)):- may_debug_var(Name,Rest).
pretty1(get_var(Env, Name, Val)):- may_debug_var('GEnv',Env),may_debug_var(Name,Val).
pretty1(deflexical(Env,_Op, Name, Val)):- may_debug_var('SEnv',Env),may_debug_var(Name,Val).
pretty1(set_var(Env,Name, Val)):- may_debug_var('SEnv',Env),may_debug_var(Name,Val).

pretty1(f_slot_value(_Env, Name, Val)):- may_debug_var(slot,Name,Val).
%pretty1(get_kw(ReplEnv, RestNKeys, test, test, f_eql, true, True)
pretty1(Env=[List|_]):- compound(List),var(Env),List=[H|_],compound(H),H=bv(_,_), may_debug_var('Env',Env),
  maplist(pretty1,List).
pretty1(Env=List):- compound(List),var(Env),List=[H|_],compound(H),H=bv(_,_), may_debug_var('Env',Env),
  maplist_not_tail(pretty1,List).
pretty1(P):- P=..[_,_|List],append(_,[Name, Val|_],List),atom(Name),var(Val),may_debug_var(Name,Val).
pretty1(debug_var(R,V)):- may_debug_var(R,V).
pretty1(bv(R,V)):- may_debug_var(R,V).
pretty1(H):-H=..[_|ARGS],must_maplist_det(pretty1,ARGS).


maplist_not_tail(_,ArgS):- var(ArgS),!.
maplist_not_tail(G,[X|ArgS]):-call(G,X),maplist_not_tail(G,ArgS).

pretty2(H):- \+ compound(H),!. % may_debug_var(F,'_Call',H).
%pretty2([H|T]):-!,maplist_not_tail(pretty2,[H|T]).
pretty2(H):-  
 dmust_det((functor(H,F,A),
   H=..[F,P1|ARGS],   
   (A>1 -> may_debug_var(F,'_Param',P1) ; true),
   must_maplist_det(pretty2,[P1|ARGS]))),!. 

pretty3(H):- \+ compound(H),!. % may_debug_var(F,'_Call',H).
pretty3(H):-pretty4(H),pretty5(H).

pretty4(H):- \+ compound(H),!. % may_debug_var(F,'_Call',H).
%pretty4([H|T]):-!,maplist_not_tail(pretty4,[H|T]).
pretty4(H):-  
 ignore(((functor(H,F,_), fail,
  nop((wl:init_args(N,F),integer(N),
   A is N + 1,   
   arg(A,H,R),may_debug_var('KeysNRest',R)))),
   H=..[F,P1|ARGS],  
   must_maplist_det(pretty4,[P1|ARGS]))),!. 

pretty5(H):- \+ compound(H),!. % may_debug_var(F,'_Call',H).
pretty5([H | B]):- pretty5(H),pretty5(B),may_debug_var('CAR',H),may_debug_var('CDR',B).
pretty5(H):-  
 dmust_det((functor(H,F,A),
   H=..[F,P1|ARGS],   
   arg(A,H,R),may_debug_var(F,'_Ret',R),   
   nop(may_debug_var(F,'_Param',P1)),
   must_maplist_det(pretty5,[P1|ARGS]))),!. 

atom_concat_or_rtrace(X,Y,Z):- tracing->atom_concat(X,Y,Z);catch(atom_concat(X,Y,Z),_,break).

