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

preposition(_,P) :- notrace(member(P, [at, down, in, inside, into, of, off, on, onto, out, over, to, under, up, with])).

preposition(_Other, P) :-
 member(P, [of, beside]).

compass_direction(D) :- 
 member(D, [north, south, east, west, up, down]).
maybe_compass_direction(D, Actual) :- (cmdalias(D,Actual);D=Actual), compass_direction(Actual).

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
parse2logical(Self, [Object, ',' | Msg], emote(Self, say, Object, Msg), Mem):- 
 in_model(Self, h(_, Object, _), Mem).

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

flee_run_escape(flee).
flee_run_escape(run).
flee_run_escape(escape).

% get [out,in,..] Object
parse2logical(Self, [get, Prep, Object], goto_prep_obj(Self, walk, Prep, Object), _Mem) :- preposition(spatial, Prep).
% n/s/e/w/u/d
parse2logical(Self, [Dir], Logic, Mem):- maybe_compass_direction(Dir,Actual), !, dmust(txt2goto(Self, walk, [Actual], Logic, Mem)).
% escape/flee/run .. 
parse2logical(Self, [Escape|Info], Logic, Mem):- flee_run_escape(Escape), !, dmust(txt2goto(Self, run, Info, Logic, Mem)).
% out/into
parse2logical(Self, [Prep], Logic, Mem) :- preposition(spatial, Prep), !, dmust(txt2goto(Self, walk, [Prep], Logic, Mem)).
% go .. 
parse2logical(Self, [go|Info], Logic, Mem):- !, dmust(txt2goto(Self, walk, Info, Logic, Mem)).
% outside
parse2logical(Self, [ExitName], Logic, Mem) :- 
 in_model(Self, h(exit(ExitName), _, _), Mem),
 !, dmust(txt2goto(Self, walk, [ExitName], Logic, Mem)).

parse2logical(Self, [get, Prep| More], Logic, Mem) :- preposition(spatial, Prep), !, dmust(txt2goto(Self, walk, [Prep| More], Logic, Mem)).

% x shelf~1
% go on shelf~1

txt2goto(Self, run, [], goto_dir(Self, run, escape), _Mem) :- !.
txt2goto(Self, Walk,[to, Prep| More], Logic, Mem) :- !, txt2goto(Self, Walk, [Prep| More], Logic, Mem).
txt2goto(Self, Walk,[Alias| More], Logic, Mem) :- cmdalias(Alias,Dir), !, txt2goto(Self, Walk,[Dir| More], Logic, Mem).

% go in kitchen
% go in car
txt2goto(Self, Walk,[ Prep, Dest], goto_prep_obj(Self, Walk, Prep, Where), Mem) :- 
 preposition(spatial, Prep),!,
 dmust(txt2place(Dest, Where, Mem)).

% go north
txt2goto(Self, Walk,[ ExitName], goto_dir(Self, Walk, ExitName), Mem) :-
 in_model(Self, h(exit(ExitName), _, _), Mem).
% go escape
txt2goto(Self, Walk,[ Dir], goto_dir(Self, Walk, Dir), _Mem) :- (compass_direction(Dir);Dir==escape),!.
txt2goto(Self, Walk,[ Dir], goto_dir(Self, Walk, Dir), _Mem) :- (Dir=down;Dir==up),!.
% go [out,in,..] 
txt2goto(Self, Walk,[ Prep], goto_dir(Self, Walk, Prep), _Mem) :- preposition(spatial, Prep).
% go kitchen
txt2goto(Self, Walk, Dest, goto_loc(Self, Walk, Where), Mem) :-
 txt2place(Dest, Where, Mem).


txt2place(List, Place, Mem):- is_list(List), parse2object(List,Object,Mem), txt2place(Object, Place, Mem),!.
txt2place(Dest, Place, Mem):- in_model(advstate, h(_, _, Dest), Mem), Dest = Place.
txt2place(Dest, Place, Mem):- parse2object(Dest, Place, Mem).

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
 preposition(switch, OnOff).

%parse2logical(Self, [open| Thing], Result, Mem) :- parse2logical(Self, [switch, open| Thing], Result, Mem).
%parse2logical(Self, [close| Thing], Result, Mem) :- parse2logical(Self, [switch, close| Thing], Result, Mem).


% %%%%%%%%%%%%%%
% Dig
% %%%%%%%%%%%%%%
parse2logical(Agent, [dig, Hole], dig(Agent, Hole, Where, Tool), Mem) :-
 in_model(Agent, inst(Agent), Mem),
 in_model(Agent, h(_, Agent, Where), Mem),
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

parse2logical(_Self, [Verb|Args], Action, _M) :- verbatum_anon(Verb), !,
 Action =.. [Verb|Args].

parse2logical(Self, [Verb], Action, _M) :- Action=..[Verb,Self], !.

parse2logical(Self, [Verb|TheArgs], Action, M) :-
 args2logical(TheArgs, Args, M), wdmsg( TheArgs->Args), !, 
 Action =.. [Verb,Self|Args].

verbatum_anon(Verb):- member(Verb, [prolog, make, cls, mem, props, ls, debug, cd, pwd, 
 agent, create, delprop, destroy, echo, halt, getprops,
 memory, model, path, properties, setprop, state, state, help, threads,
 spy,nospy,call,
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



as1object([TheThing], Thing, Mem):- !, nonvar(TheThing), as1object(TheThing, Thing, Mem).
as1object(TheThing, Thing, _Mem):- atom(TheThing), atom_number(TheThing,Thing),!.
as1object(TheThing, Thing, Mem):- obj_props(Mem,Thing,Props),(same_word(TheThing,Thing)->true;(sub_term(Sub,Props),(atom(Sub);string(Sub)),same_word(TheThing,Sub))).
as1object(TheThing, Thing, _Mem):- \+ atom(TheThing),!, TheThing=Thing.
as1object(TheThing, Thing, Mem):- atom_of(inst, TheThing, Thing, Mem),!.
as1object(TheThing, Thing, Mem):- b_getval(advstate,Mem2),Mem2\=Mem,as1object(TheThing, Thing, Mem2).
% as1object(Thing, Thing, _Mem).

to_string_lc(S,L):- atomic(S), S\=[], string_lower(S,L).
to_string_lc(S,L):- is_list(S), maplist(to_string_lc,S,W),atomics_to_string(W,' ',L).
same_word(T1,T2):- to_string_lc(T1,S1),to_string_lc(T2,S2),S1=S2.

same_props(Props1,Props1):- !.
same_props(Props1,Props2):- each_prop(Props1,Prop1),each_prop(Props2,Prop2),same_prop(Prop1,Prop2).
each_prop(Props,Prop):- is_list(Props),!,member(PropsZ,Props),each_prop(PropsZ,Prop).
each_prop(PropC,Prop):- compound(PropC),PropC=Prop.


obj_props(Mem,Obj,Props):- var(Mem),!,b_getval(advstate,Mem2),obj_props(Mem2,Obj,Props).
obj_props(Mem,Obj,Props):- nonvar(Obj),!,obj_props(Mem,Obj2,Props),Obj=@=Obj2.
obj_props(Mem,Obj,Props):- nonvar(Props),!,obj_props_v(Mem,Obj,Props2),same_props(Props,Props2).
obj_props(Mem,Obj,Props):- obj_props_v(Mem,Obj,Props).

obj_props_v(Mem,_,_):- \+ compound(Mem),!,fail.
obj_props_v(Mem,Obj,Props):- is_list(Mem),!,member(E,Mem),obj_props_v(E,Obj,Props).
obj_props_v(props(Obj,Props),Obj,Props):- !.
obj_props_v(sense_props(_,_,Obj,_,Props),Obj,Props):- !.
obj_props_v(Term,Obj,Props):- arg(_,Term,Mem),obj_props_v(Mem,Obj,Props).

same_prop(X,Y):- X=@=Y,X=Y.

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

