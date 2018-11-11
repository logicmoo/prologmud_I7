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

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
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


% -- parse(Doer, WordList, ActionOrQuery, Memory)
parse_command(_Self, NotList, Action, _Memory) :- \+ is_list(NotList), !, Action = NotList.
parse_command(Self, Tokens, Action, Memory) :- 
 (Tokens = [_] -> Tokens2 = Tokens ; strip_noise_words(Tokens, Tokens2)),
 if_tracing((dmsg(parse_command(Self, Tokens -> Tokens2)))),
 parse2logical(Self, Tokens2, Action, Memory).

:- discontiguous(parse2logical/4).

% %%%%%%%%%%%%%%
% parser tracing            
% %%%%%%%%%%%%%%

parse2logical(_Self,  NotList, Action, _) :- \+ is_list(NotList), !, Action = NotList.
parse2logical(_Self, [NonAtom], Action, _) :- \+ atom(NonAtom), !, Action=NonAtom.
parse2logical(Self, [rtrace|Args], Action, M) :- Args\==[], !, rtrace(parse2logical(Self, Args, Action, M)).
parse2logical(Self, [cls|Args], Action, M) :- Args\==[], !, cls, notrace(parse2logical(Self, Args, Action, M)).

parse2logical(Self, [wait], wait(Self), _Mem) :- !.

% %%%%%%%%%%%%%%
% Introspection
% %%%%%%%%%%%%%%
parse2logical(Self, [model|Tail], inspect(Self,getprop(Target,model)), Mem) :- parse2agent(Tail, Target, Mem).
parse2logical(Self, [memory|Tail], inspect(Self,getprop(Target,memory)), Mem) :- parse2agent(Tail, Target, Mem).

parse2logical(Self, [whom|Tail], recall(Self,who,Target), Mem) :- parse2agent(Tail, Target, Mem).
parse2logical(Self, [who|Tail], recall(Self,who,Target), Mem) :- parse2agent(Tail, Target, Mem).

parse2logical(Self, [what|Tail], recall(Self,what,Target), Mem) :- parse2object(Tail, Target, Mem).
parse2logical(Self, [where|Tail], recall(Self,where,Target), Mem) :- parse2object(Tail, Target, Mem).


% %%%%%%%%%%%%%%
% Communication
% %%%%%%%%%%%%%%
parse2logical(Self, [emote|Msg], emote(Self, act, *, Msg), _M):- !.

% %%%%%%%%%%%%%%
% Communication
% %%%%%%%%%%%%%%
parse2logical(Self, [say|Msg], emote(Self, say, *, Msg), _M):- !.
parse2logical(Self, [ask, Object | Msg], emote(Self, say, Object, Msg), _M):- !.
parse2logical(Self, [request, Object | Msg], emote(Self, say, Object, Msg), _M):- !.
parse2logical(Self, [tell, Object | Msg], emote(Self, say, Object, Msg), _M):- !.
parse2logical(Self, [talk, Object | Msg], emote(Self, say, Object, Msg), _M):- !.
parse2logical(Self, [Object, ',' | Msg], emote(Self, say, Object, Msg), Mem):- current_spatial(Spatial),
 thought_model(ModelData, Mem),
 known_model(Self, h(Spatial, _, Object, _), ModelData).

parse2logical(Self, Words, Action, Mem) :- 
 fail, 
 Words \== [i], % Dont interfere with inventory
 % If not talking to someone else, substitute Agent for 'self'.
 append(Before, [Self|After], Words),
 reflexive(Self),
 thought(inst(Agent), Mem),
 append(Before, [Agent|After], NewWords),
 parse2logical(Self, NewWords, Action, Mem).
% %%%%%%%%%%%%%%
% Movement
% %%%%%%%%%%%%%%

% get [out,in,..]
parse2logical(Self, [get, Prep, Object], goto(Self, walk, loc(Self, _, Prep, Object)), _Mem) :-
 preposition(spatial, Prep).
% n/s/e/w
parse2logical(Self, [Dir], Logic, Mem):- (compass_direction(Dir);Dir==escape), !, dmust(txt2goto(Self, walk, [Dir], Logic, Mem)).
% escape .. 
parse2logical(Self, [escape|Info], Logic, Mem):- !, dmust(txt2goto(Self, run, Info, Logic, Mem)).
% go .. 
parse2logical(Self, [go|Info], Logic, Mem):- !, dmust(txt2goto(Self, walk, Info, Logic, Mem)).
% run .. 
parse2logical(Self, [run|Info], Logic, Mem):- !, dmust(txt2goto(Self, run, Info, Logic, Mem)).
parse2logical(Self, [Prep], Logic, Mem) :- preposition(spatial, Prep), !, dmust(txt2goto(Self, walk, [Prep], Logic, Mem)).
parse2logical(Self, [ExitName], Logic, Mem) :- 
 thought_model(ModelData, Mem),
 known_model(Self, h(_Spatial, exit(ExitName), _, _), ModelData),
 !, dmust(txt2goto(Self, walk, [ExitName], Logic, Mem)).

parse2logical(Self, [get, Prep| More], Logic, Mem) :- preposition(spatial, Prep), !, dmust(txt2goto(Self, walk, [Prep| More], Logic, Mem)).

% x shelf~1
% go on shelf~1

txt2goto(Self, Walk,[to, Prep| More], Logic, Mem) :- !, txt2goto(Self, Walk, [Prep| More], Logic, Mem).
txt2goto(Self, Walk,[Alias| More], Logic, Mem) :- cmdalias(Alias,Dir), !, txt2goto(Self, Walk,[Dir| More], Logic, Mem).

% go in kitchen
% go in car
txt2goto(Self, Walk,[ Prep, Dest], goto(Self, Walk, loc(Self,_Dir, Prep, Where)), Mem) :- 
 preposition(spatial, Prep),!,
 dmust(txt2place(Dest, Where, Mem)).

% go north
txt2goto(Self, Walk,[ ExitName], goto(Self, Walk, loc(Self, ExitName, _To, _Where)), Mem) :-
 thought_model(ModelData, Mem),
 known_model(Self, h(_Spatial, exit(ExitName), _, _), ModelData).
% go escape
txt2goto(Self, Walk,[ Dir], goto(Self, Walk, loc(Self, Dir, _To, _Object)), _Mem) :- (compass_direction(Dir);Dir==escape),!.
txt2goto(Self, Walk,[ Dir], goto(Self, Walk, loc(Self, Dir, _To, _Object)), _Mem) :- (Dir=down;Dir==up),!.
% go [out,in,..] 
txt2goto(Self, Walk,[ Prep], goto(Self, Walk, loc(Self, _Dir, Prep, _Where)), _Mem) :- preposition(spatial, Prep).
% go kitchen
txt2goto(Self, Walk, Dest, goto(Self, Walk, loc(Self, _Dir, _To, Where)), Mem) :-
 txt2place(Dest, Where, Mem).


txt2place(List, Place, Mem):- is_list(List), parse2object(List,Object,Mem),
 txt2place(Object, Place, Mem).
txt2place(Dest, Place, Mem):- 
 thought_model(ModelData, Mem),
 known_model(advstate, h(_Spatial, _, _, Dest), ModelData),
 Dest = Place.


% %%%%%%%%%%%%%%
% Take
% %%%%%%%%%%%%%%
parse2logical(Self, [get| Args], TAKE, Mem) :- parse2logical(Self, [take| Args], TAKE, Mem).
parse2logical(Self, [take, Object], take(Self, Object), _Mem) :- !.

% %%%%%%%%%%%%%%
% Give
% %%%%%%%%%%%%%%
parse2logical(Self, [give, Object, to, Recipient], give(Self, Object, Recipient), _Mem):- !.
parse2logical(Self, [give, Recipient, Object ], give(Self, Object, Recipient), _Mem):- !.

% %%%%%%%%%%%%%%
% Flip Switches
% %%%%%%%%%%%%%%
parse2logical(Self, [light, Thing], Result, Mem):- !, parse2logical(Self, [switch, on, Thing], Result, Mem).
parse2logical(Self, [switch, Thing, OnOff], Result, Mem) :- preposition(_, OnOff), !, parse2logical(Self, [switch, OnOff, Thing], Result, Mem).

parse2logical(Self, [switch, OnOff| TheThing], switch(Self, OnOff, Thing), Mem) :- parse2object(TheThing, Thing, Mem),
 preposition(Spatial, OnOff), current_spatial(Spatial).

%parse2logical(Self, [open| Thing], Result, Mem) :- parse2logical(Self, [switch, open| Thing], Result, Mem).
%parse2logical(Self, [close| Thing], Result, Mem) :- parse2logical(Self, [switch, close| Thing], Result, Mem).


% %%%%%%%%%%%%%%
% Dig
% %%%%%%%%%%%%%%
parse2logical(Agent, [dig, Hole], dig(Agent, Hole, Where, Tool), Mem) :-
 thought_model(ModelData, Mem),
 thought(inst(Agent), Mem),
 known_model(Agent, h(_Spatial, _, Agent, Where), ModelData),
 Tool=shovel.

parse2logical(Self, [CmdAlias|Tail], Action, Mem) :-
 cmdalias(CmdAlias, Verb),
 parse2logical(Self, [Verb|Tail], Action, Mem).

% parse2logical(Self, [look], look(Agent, Spatail), Mem) :- parse2object(Tail, Agent, Mem).

parse2logical(Self, [TheVerb|Args], Action, M) :- fail,
 quietly_talk_db([F,Verb|Forms]),
 notrace(F==intransitive;F==transitive),
 member(TheVerb,Forms),!,
 parse2logical(Self, [Verb|Args], Action, M).

parse2logical(Self, [TheVerb|Args], Action, M) :- fail,
 clex_verb(TheVerb,Verb,_,_),
 Verb\==TheVerb,!,
 parse2logical(Self, [Verb|Args], Action, M).

parse2logical(_Self, [Verb|Args], Action, _M) :- verbatum(Verb), !,
 Action =.. [Verb|Args].

parse2logical(Self, [Verb], Action, _M) :- Action=..[Verb,Self], !.

parse2logical(Self, [Verb|TheArgs], Action, M) :-
 args2logical(TheArgs, Args, M), wdmsg( TheArgs->Args), !, 
 Action =.. [Verb,Self|Args].

verbatum(Verb):- member(Verb, [prolog, make, cls, mem, props, ls, debug, cd, pwd, 
 agent, create, delprop, destroy, echo, quit,
 memory, model, path, properties, setprop, state, help, 

 rtrace, nortrace, 
 trace, notrace %, %whereami, whereis, whoami
 ]).

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
as1object(TheThing, Thing, _Mem):- atom_number(TheThing,Thing).
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

