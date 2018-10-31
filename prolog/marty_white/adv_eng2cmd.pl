/*
%  NomicMUD: A MUD server written in Prolog
%
%  Some parts used Inform7, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
% 
%  July 10,1996 - John Eikenberry 
%  Copyright (C) 2004 Marty White under the GNU GPL
% 
%  Dec 13, 2035 - Douglas Miles
%
%
%  Logicmoo Project changes:
%
% Main file.
%
*/
:- if(exists_source(library(nldata/nl_iface))).
% being in user is just to help debugging from console
:- user:ensure_loaded(library(nldata/nl_iface)).
:- endif.

:- if(exists_source(library(nldata/clex_iface))).
% being in user is just to help debugging from console
:- user:ensure_loaded(library(nldata/clex_iface)).
:- endif.
/*
:- if(exists_source(library(nldata/ac_xnl_iface))).
% being in user is just to help debugging from console
:- time(user:ensure_loaded(library(nldata/ac_xnl_iface))).
:- endif.
*/
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CODE FILE SECTION
% :- ensure_loaded('adv_eng2cmd').
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cmdalias(d, down).
cmdalias(e, east).
cmdalias(i, inventory).
cmdalias(l, look).
cmdalias(n, north).
cmdalias(s, south).
cmdalias(u, up).
cmdalias(w, west).
cmdalias(x, examine).
cmdalias(z, wait).
cmdalias(a, auto).

cmdalias(turn, switch).
cmdalias(flip, switch).

preposition(Spatial, P) :- (Spatial==spatial-> !; atom(P)),
  atom(P),
  notrace(member(P, [at, down, in, inside, into, of, off, on, onto, out, over, to, under, up, with])).

preposition(_Other, P) :-
  member(P, [of, beside]).

compass_direction(D) :-
  member(D, [north, south, east, west]).

reflexive(W) :- member(W, [self, me, myself , i]). % 'i' inteferes with inventory

strip_noise_words([to|Tokens], NewTokens) :- strip_noise_words(Tokens, NewTokens).
strip_noise_words(Tokens, NewTokens) :-
  findall(Token,
          ( member(Token, Tokens),
            \+ member(Token, ['please', 'the', 'a', 'an', 'some', 'thee'])),
          NewTokens).

convert_reflexive(Agent, Words, NewWords) :-
  % Substitute Agent for 'self'.
  findall(Token,
          ( member(Word, Words),
            ( reflexive(Word), Token = Agent;
              Token = Word )),
          NewWords).


% -- parse(WordList, ActionOrQuery, Memory)
parse(Tokens, Action, Memory) :-
  (Tokens = [_] -> Tokens2 = Tokens ; strip_noise_words(Tokens, Tokens2)),
  parse2logical(Tokens2, Action, Memory).

:- discontiguous(parse2logical/3).

% %%%%%%%%%%%%%%
% parser tracing
% %%%%%%%%%%%%%%
parse2logical([rtrace|Args], Action, M) :- Args\==[], !, rtrace(parse2logical(Args, Action, M)).

% %%%%%%%%%%%%%%
% Introspection
% %%%%%%%%%%%%%%
parse2logical([model|Tail], model(Agent), Mem) :- parse2agent(Tail, Agent, Mem).
parse2logical([memory|Tail], memory(Agent), Mem) :- parse2agent(Tail, Agent, Mem).
parse2logical([whom|Tail], whois(Agent), Mem) :- parse2agent(Tail, Agent, Mem).
parse2logical([who|Tail], whois(Agent), Mem) :- parse2agent(Tail, Agent, Mem).

parse2logical([what|Tail], whatis(Agent), Mem) :- parse2object(Tail, Agent, Mem).
parse2logical([where|Tail], whereis(Agent), Mem) :- parse2object(Tail, Agent, Mem).


% %%%%%%%%%%%%%%
% Communication
% %%%%%%%%%%%%%%
parse2logical([emote|Msg], emote( act, *, Msg), _M):- !.

% %%%%%%%%%%%%%%
% Communication
% %%%%%%%%%%%%%%
parse2logical([say|Msg], emote( say, *, Msg), _M):- !.
parse2logical([ask, Object | Msg], emote( say, Object, Msg), _M):- !.
parse2logical([request, Object | Msg], emote( say, Object, Msg), _M):- !.
parse2logical([tell, Object | Msg], emote( say, Object, Msg), _M):- !.
parse2logical([talk, Object | Msg], emote( say, Object, Msg), _M):- !.
parse2logical([Object, ',' | Msg], emote( say, Object, Msg), Mem):- current_spatial(Spatial),
  thought_model(ModelData, Mem),
  in_model(h(Spatial, _, Object, _, _T), ModelData).

parse2logical(Words, Action, Mem) :- 
  fail, 
  Words \== [i], % Dont interfere with inventory
  % If not talking to someone else, substitute Agent for 'self'.
  append(Before, [Self|After], Words),
  reflexive(Self),
  thought(inst(Agent), Mem),
  append(Before, [Agent|After], NewWords),
  parse2logical(NewWords, Action, Mem).
% %%%%%%%%%%%%%%
% Movement
% %%%%%%%%%%%%%%

% get [out,in,..]
parse2logical([get, Prep, Object], goto(walk, _, Prep, Object), _Mem) :-
  preposition(spatial, Prep).
% n/s/e/w
parse2logical([Dir], Logic, Mem):- (compass_direction(Dir);Dir==escape), !, dmust(txt2goto(walk, [Dir], Logic, Mem)).
% escape .. 
parse2logical([escape|Info], Logic, Mem):- !, dmust(txt2goto(run, Info, Logic, Mem)).
% go .. 
parse2logical([go|Info], Logic, Mem):- !, dmust(txt2goto(walk, Info, Logic, Mem)).
% run .. 
parse2logical([run|Info], Logic, Mem):- !, dmust(txt2goto(run, Info, Logic, Mem)).
parse2logical([Prep], Logic, Mem) :- preposition(spatial, Prep), !, dmust(txt2goto(walk, [Prep], Logic, Mem)).
parse2logical([ExitName], Logic, Mem) :-  
  thought_model(ModelData, Mem),
  in_model(h(_Spatial, exit(ExitName), _, _, _T), ModelData),
   !, dmust(txt2goto(walk, [ExitName], Logic, Mem)).

parse2logical([get, Prep| More], Logic, Mem) :- preposition(spatial, Prep), !, dmust(txt2goto(walk, [Prep| More], Logic, Mem)).

% x shelf~1
% go on shelf~1

txt2goto(Walk,[to, Prep| More], Logic, Mem) :- !, txt2goto(Walk, [Prep| More], Logic, Mem).
% go in kitchen
% go in car
txt2goto(Walk,[ Prep, Dest], goto(Walk, _Dir, Prep, Where), Mem) :-  
  preposition(spatial, Prep),!,
  dmust(txt2place(Dest, Where, Mem)).

% go north
txt2goto(Walk,[ ExitName], goto(Walk, ExitName, _To, _Where), Mem) :-
  thought_model(ModelData, Mem),
  in_model(h(_Spatial, exit(ExitName), _, _, _T), ModelData).
% go escape
txt2goto(Walk,[ Dir], goto(Walk, Dir, _To, _Object), _Mem) :- (compass_direction(Dir);Dir==escape),!.
% go [out,in,..]
txt2goto(Walk,[ Prep], goto(Walk, _Dir, Prep, _Where), _Mem) :- preposition(spatial, Prep).
% go kitchen
txt2goto(Walk, Dest, goto(Walk, _Dir, _To, Where), Mem) :-
  txt2place(Dest, Where, Mem).


txt2place(List, Place, Mem):- is_list(List), parse2object(List,Object,Mem),
  txt2place(Object, Place, Mem).
txt2place(Dest, Place, Mem):- 
  thought_model(ModelData, Mem),
  in_model(h(_Spatial, _, _, Dest, _T), ModelData),
  Dest = Place.
  % getprop(Dest, has_rel(Spatial, Prep), ModelData).



% %%%%%%%%%%%%%%
% Take
% %%%%%%%%%%%%%%
parse2logical([get| Args], TAKE, Mem) :- parse2logical([take| Args], TAKE, Mem).
parse2logical([take, Object], take( Object), _Mem) :- !.

% %%%%%%%%%%%%%%
% Give
% %%%%%%%%%%%%%%
parse2logical([give, Object, to, Recipient], give( Object, Recipient), _Mem):- !.
parse2logical([give, Recipient, Object ], give( Object, Recipient), _Mem):- !.

% %%%%%%%%%%%%%%
% Flip Switches
% %%%%%%%%%%%%%%
parse2logical([light, Thing], Result, Mem):- !, parse2logical([switch, on, Thing], Result, Mem).
parse2logical([switch, Thing, OnOff], Result, Mem) :- preposition(_, OnOff), !, parse2logical([switch, OnOff, Thing], Result, Mem).

parse2logical([switch, OnOff| TheThing], switch(OnOff, Thing), Mem) :- parse2object(TheThing, Thing, Mem),
  preposition(Spatial, OnOff), current_spatial(Spatial).

%parse2logical([open| Thing], Result, Mem) :- parse2logical([switch, open| Thing], Result, Mem).
%parse2logical([close| Thing], Result, Mem) :- parse2logical([switch, close| Thing], Result, Mem).


% %%%%%%%%%%%%%%
% Dig
% %%%%%%%%%%%%%%
parse2logical([dig, Hole], dig( Hole, Where, Tool), Mem) :-
  thought_model(ModelData, Mem),
  thought(inst(Agent), Mem),
  in_model(h(_Spatial, _, Agent, Where, _T), ModelData),
  Tool=shovel.

parse2logical([CmdAlias|Tail], Action, Mem) :-
  cmdalias(CmdAlias, Verb),
  parse2logical([Verb|Tail], Action, Mem).

% parse2logical([look], look(Spatail), Mem) :- parse2object(Tail, Agent, Mem).

parse2logical([TheVerb|Args], Action, M) :-  fail,
  quietly_talk_db([F,Verb|Forms]),
  notrace(F==intransitive;F==transitive),
  member(TheVerb,Forms),!,
  parse2logical([Verb|Args], Action, M).

parse2logical([TheVerb|Args], Action, M) :-  fail,
  clex_verb(TheVerb,Verb,_,_),
  Verb\==TheVerb,!,
  parse2logical([Verb|Args], Action, M).

parse2logical([Verb], Verb, _M) :- !.

parse2logical([Verb|Args], Action, _M) :- fail, verbatum(Verb), !,
   Action =.. [Verb|Args].

parse2logical([Verb|TheArgs], Action, M) :-
  args2logical(TheArgs, Args, M), wdmsg( TheArgs->Args), !, 
  Action =.. [Verb|Args].

verbatum(Verb):- member(Verb, [prolog, make, agent, create, delprop, destroy, echo, quit,
  memory, model, path, properties, setprop, state, trace, notrace, whereami, whereis, whoami]).

parse2agent([], Agent, Mem):- thought(inst(Agent), Mem), !.
parse2agent(List,Agent,Mem):- parse2object(List,Agent,Mem).

parse2object(List,Agent,Mem):- append(LList,[R],List),member(R,[(?),(.)]),!,parse2object(LList,Agent,Mem).
parse2object([am, i], Agent, Mem):- thought(inst(Agent), Mem), !.

parse2object([BE| List], Agent, Mem):- fail, quietly_talk_db([_,BE,is|_More]), parse2object(List,Agent,Mem),!.
parse2object([HAS| List], Agent, Mem):- fail, quietly_talk_db([_,have|HASHAVE]), member(HAS,HASHAVE), !, parse2object(List,Agent,Mem).
parse2object([Det| Type], TheThing, Mem):-
   (nth0(_N, [(unknown), the, thee, old, some, a], Det)), !,
   parse2object(Type, TheThing, Mem).

parse2object(Type, TheThing, Mem):-
   show_call(as1object(Type, TheThing, Mem)), !.

as1object([TheThing], Thing, Mem):- !,nonvar(TheThing), as1object(TheThing, Thing, Mem).
%as1object(TheThing, Thing, Mem):- as1object(TheThing, Thing, Mem).

as1object(TheThing, Thing, _Mem):- \+ atom(TheThing),!, TheThing=Thing.
as1object(TheThing, Thing, _Mem):-  atom_number(TheThing,Thing).
as1object(TheThing, Thing, Mem):- atom_concat(TheThing,'~1',TheThing2), sub_term(Thing,Mem),atom(Thing),TheThing2==Thing,!.
as1object(TheThing, Thing, Mem):- atom_concat(TheThing,'~2',TheThing2), sub_term(Thing,Mem),atom(Thing),TheThing2==Thing,!.
as1object(TheThing, Thing, Mem):- atom_of(inst, TheThing, Thing, Mem),!.
as1object(TheThing, Thing, _Mem):- b_getval(advstate,Mem), atom_of(inst, TheThing, Thing, Mem),!.
% as1object(Thing, Thing, _Mem).


args2logical(TheArgs, [Thing], Mem):- parse2object(TheArgs, Thing, Mem),!. % TheArgs\==[Thing],!.
args2logical(TheArgs, TheArgs, _M).
  
quietly_talk_db(L):- notrace(talk_db(L)).

is_kind(Thing,inst):- b_getval(advstate,Mem), member(props(Thing,_),Mem).
is_kind(Thing,type):- b_getval(advstate,Mem), member(type_props(Thing,_),Mem).
%is_kind(Thing,inst):- b_getval(advstate,Mem), \+ member(type_props(Thing,_),Mem).

atom_of(Kind,TheThing,Thing,Mem):- sub_term_atom(Thing,Mem),is_kind(Thing,Kind),TheThing==Thing,!.
atom_of(Kind,TheThing,Thing,Mem):- sub_term_atom(Thing,Mem),is_kind(Thing,Kind),atom_concat(TheThing,_,Thing),!.
atom_of(Kind,TheThing,Thing,Mem):- sub_term_atom(Thing,Mem),is_kind(Thing,Kind),atom_concat(_,TheThing,Thing),!.
atom_of(Kind,TheThing,Thing,Mem):- sub_term_atom(Thing,Mem),is_kind(Thing,Kind),atom_contains(Thing,TheThing),!.


sub_term_atom(Term, TermO):- \+ compound(Term), !, atom(Term), TermO = Term.
sub_term_atom(Term, [Head|_]) :- nonvar(Head),
  sub_term_atom(Term, Head).
sub_term_atom(Term, [_|Tail]) :- !, nonvar(Tail),
  sub_term_atom(Term, Tail).
sub_term_atom(Term, T) :-
  \+ is_list(T),
  T =.. List,
  sub_term_atom(Term, List).

