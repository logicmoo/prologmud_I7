
:- module(cycnl,[ttholds/1,ttholds/2,ttholds/3,ttholds/4,ttholds/5,ttholds/6,ttholds/7,ttholds/8,ttholds/9, 
   assertion_content/3,assertion_content/4,assertion_content/5,assertion_content/6,assertion_content/7,assertion_content/8,assertion_content/9,assertion_content/10,assertion_content/11,assertion_content/12,assertion_content/13]).
:- style_check(-singleton).

:- multifile((ttholds/1,ttholds/2,ttholds/3,ttholds/4,ttholds/5,ttholds/6,ttholds/7,ttholds/8,ttholds/9)).
:- dynamic((ttholds/1,ttholds/2,ttholds/3,ttholds/4,ttholds/5,ttholds/6,ttholds/7,ttholds/8,ttholds/9)).
:- discontiguous((ttholds/1,ttholds/2,ttholds/3,ttholds/4,ttholds/5,ttholds/6,ttholds/7,ttholds/8,ttholds/9)).

:- multifile((assertion_content/3,assertion_content/4,assertion_content/5,assertion_content/6,assertion_content/7,assertion_content/8,assertion_content/9,assertion_content/10,assertion_content/11,assertion_content/12,assertion_content/13)).
:- dynamic((assertion_content/3,assertion_content/4,assertion_content/5,assertion_content/6,assertion_content/7,assertion_content/8,assertion_content/9,assertion_content/10,assertion_content/11,assertion_content/12,assertion_content/13)).
:- discontiguous((assertion_content/3,assertion_content/4,assertion_content/5,assertion_content/6,assertion_content/7,assertion_content/8,assertion_content/9,assertion_content/10,assertion_content/11,assertion_content/12,assertion_content/13)).

:- multifile((assertion_lifting_consequent/2,assertion_lifting/2,assertion_mt/2)).
:- dynamic((assertion_lifting_consequent/2,assertion_lifting/2,assertion_mt/2)).
:- discontiguous((assertion_lifting_consequent/2,assertion_lifting/2,assertion_mt/2)).

:- set_prolog_flag(double_quotes,string).

%:- include('ac_xnl_7166.nldata').

:- dynamic(warned_ttw/1).
ttw(A):- warned_ttw(A),!.
ttw(A):- assertz(warned_ttw(A)),
         source_location(F,L),
         format(user_error,'~NWARN: ~p. % ~w:~w~n',[A,F,L]),!.

:- include('tt0_00022_cycl_renamed.nldata').

is_tt(A,R):- atom(A),!,atom_concat('tt_',R,A).
is_tt(A,A).

is_NOf(Event,W,N):-atom(W),atom_concat(Event,NOf,W),atom_concat(NA,'-of',NOf),atom_number(NA,N).

maybe_rename_tt(I,I):- \+ callable(I),!.
maybe_rename_tt(tt(I),tt(I)):-!.
maybe_rename_tt(fr(I),fr(I)):-!.
maybe_rename_tt([],[]):-!. 
maybe_rename_tt([I],S):- atom_string(I,S),!.
%maybe_rename_tt([I],S):- atom_string(I,S),!.
maybe_rename_tt(I,I):-  \+ atom(I),!. %, nop(ttw(\+ atom(I))),!.
maybe_rename_tt(I,N):- atom_number(I,N),!.
maybe_rename_tt(I,R):- rename(R,I),!,ttw(rename(R,I)).
maybe_rename_tt(I,I).

%ttholds('inflNounSingularUnchecked','TTWord-MovieHouse',['movie','house']).

dash_to_underscore(R,U):- atomic_list_concat(List,'-',R),atomic_list_concat(List,'_',U).

has_tt([R|GS]):- member(E,[R|GS]),\+ atom(E),!.
has_tt(RGS):- sub_term(Sub,RGS),atom(Sub),atom_concat('tt_',_,Sub),!.


de_frame([FN,A|RGS],(PP)):- has_tt(RGS),maplist(de_frame,[A|RGS],NARGS),!,PP=..[FN|NARGS].
de_frame([A|RGS],PP):- ws_to_string([A|RGS],PP),!.
de_frame(A,R):- de_tt_ify(A,R).

de_tt_ify(A,UU):- atom(A),atom_concat('tt_',R,A),!,dash_to_underscore(R,U),atom_concat(U,'_tt',UU).
de_tt_ify(A,R):- maybe_rename_tt(A,R) .


ws_to_string([W|List],PP):- has_tt([W|List]),!,maplist(de_frame,[W|List],FNNARGS),PP=..FNNARGS.
% ws_to_string([W|List],_):- has_tt([W|List]),!,fail.
ws_to_string([WordList],PP):- ws_to_string(WordList,PP),!.
ws_to_string([W|List],SS):- maplist(ws_to_string,[W|List],PP),SS=..[s|PP].
ws_to_string(W,S):- text_to_string(W,S).

% ['agree','tt_bride',['husband-of','tt_bride','tt_bridegroom']]
tt_expansion(F,[TT,WordList],ttholds(F,TT,PP)):- atom(TT),atom_concat('TTWord-',_Word,TT),is_list(WordList),!,ws_to_string(WordList,PP).
tt_expansion(F,[TT,Frame],ttholds(eventOf,TTT,N,PP)):- is_NOf('event',F,N),!,de_tt_ify(TT,TTT),de_frame(Frame,PP).
tt_expansion(F,[TT,Frame],ttholds(roleOf,TTT,N,PP)):- is_NOf('role',F,N),!,de_tt_ify(TT,TTT),de_frame(Frame,PP).
tt_expansion(F,List,PP):-
  maplist(de_frame,List,RNList),
  PP=..[ttholds,F|RNList],!.

tt_term_expansion(P,PP):-
  P=..[ttholds,F|List],
  tt_expansion(F,List,PP).

tt_write(P):- tt_term_expansion(P,PP)->P\==PP,!,tt_write(PP).
tt_write(P):- format('~N~p.~n',[P]).

:- ensure_loaded('tt0_00022_cycl.pl').
% library('nldata/tt0_00022_cycl.nldata')
:- absolute_file_name('tt0_00022_cycl.nldata',
        File, [access(read)]),
   open(File, read, In),
   set_stream(In, encoding(iso_latin_1)),
   tell(tt2),
   repeat,
   read(In, P),
   % DMiles: i am putting them in backwards (cuz, the hypens- confuse me if they pop out first in the debugger)
   once(tt_write(P);break),
   P==end_of_file,!,
   told,!.


%:- retractall(assertion_content(retainTerm,_,_)).