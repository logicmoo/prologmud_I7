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

:- if(exists_source(library(nldata/clex_iface))).
% being in user is just to help debugging from console
:- user:ensure_loaded(library(nldata/clex_iface)).
:- endif.
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

preposition(spatial, P) :-
  member(P, [at, down, in, inside, into, of, off, on, onto, out, over, to, under, up, with]).

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

object_or_self([], Agent, Mem):- thought(inst(Agent), Mem), !.
object_or_self(List,Agent,Mem):- append(LList,[R],List),member(R,[(?),(.)]),!,object_or_self(LList,Agent,Mem).
object_or_self([am, i], Agent, Mem):- thought(inst(Agent), Mem), !.
object_or_self([is| List], Agent, Mem):-!,object_or_self(List,Agent,Mem).
object_or_self([Agent], Agent, _Mem).

parse2logical([who|Tail], whois(_Whoial, Agent), Mem) :- object_or_self(Tail, Agent, Mem).
parse2logical([whom|Tail], whois(_Whoial, Agent), Mem) :- object_or_self(Tail, Agent, Mem).
parse2logical([what|Tail], whatis(_What_Spatial, Agent), Mem) :- object_or_self(Tail, Agent, Mem).
parse2logical([where|Tail], whereis(spatial, Agent), Mem) :- object_or_self(Tail, Agent, Mem).
parse2logical([model|Tail], model(_All_Spatial, Agent), Mem) :- object_or_self(Tail, Agent, Mem).
parse2logical([memory|Tail], memory(Agent), Mem) :- object_or_self(Tail, Agent, Mem).

parse2logical([ask, Object | Msg], emote(Spatial, say, Object, Msg), _M):- current_spatial(Spatial).
parse2logical([request, Object | Msg], emote(Spatial, say, Object, Msg), _M):- current_spatial(Spatial).
parse2logical([tell, Object | Msg], emote(Spatial, say, Object, Msg), _M):- current_spatial(Spatial).
parse2logical([talk, Object | Msg], emote(Spatial, say, Object, Msg), _M):- current_spatial(Spatial).
parse2logical([say|Msg], emote(Spatial, say, *, Msg), _M):- current_spatial(Spatial).
parse2logical([emote|Msg], emote(Spatial, act, *, Msg), _M):- current_spatial(Spatial).
parse2logical([Object, ',' | Msg], emote(Spatial, say, Object, Msg), Mem) :- current_spatial(Spatial),
  thought_model(Spatial,ModelData, Mem),
  in_model(h(Spatial, _, Object, _, _), ModelData).
parse2logical(Words, Action, Mem) :- Words \== [i], % Dont interfere with inventory
  % If not talking to someone else, substitute Agent for 'self'.
  append(Before, [Self|After], Words),
  reflexive(Self),
  thought(inst(Agent), Mem),
  append(Before, [Agent|After], NewWords),
  parse2logical(NewWords, Action, Mem).


parse2logical([dig, Hole], dig(Spatial, Hole, Where, Tool), Mem) :-
  thought_model(Spatial,ModelData, Mem),
  thought(inst(Agent), Mem),
  in_model(h(Spatial, _, Agent, Where, _), ModelData),
  Tool=shovel.
parse2logical([get, Prep], goto(Spatial, (*), Prep), _Mem) :-
  preposition(Spatial, Prep).
parse2logical([get, Prep, Object], goto(Spatial, Prep, Object), _Mem) :-
  preposition(Spatial, Prep).

parse2logical([get, Object], take(Spatial, Object), _Mem) :- equals_efffectly(model, Spatial, take).

parse2logical([give, Object, to, Recipient], give(Spatial, Object, Recipient), _Mem):- equals_efffectly(model, Spatial, give).

parse2logical([go, escape], goto(Spatial, (*), escape), _Mem):- equals_efffectly(model, Spatial, escape).
parse2logical([go, Dir], goto(Spatial, (*), Dir), _Mem) :-
  compass_direction(Dir), equals_efffectly(model, Spatial, spatial).
parse2logical([go, Prep], goto(Spatial, (*), Prep), _Mem) :-
  preposition(Spatial, Prep).
parse2logical([go, ExitName], goto(Spatial, (*), ExitName), Mem) :-
  thought_model(Spatial,ModelData, Mem),
  in_model(h(Spatial, exit(ExitName), _, _, _), ModelData).
parse2logical([go, Dest], goto(Spatial, (*), Dest), Mem) :-
  thought_model(Spatial,ModelData, Mem),
  in_model(h(Spatial, _, _, Dest, _), ModelData).
  % getprop(Dest, has_rel(Spatial, How), ModelData).

parse2logical([light, Thing], Result, Mem):- !, parse2logical([switch, on, Thing], Result, Mem).
parse2logical([turn, Thing, OnOff], Result, Mem) :- preposition(_, OnOff), !, parse2logical([switch, OnOff, Thing], Result, Mem).
parse2logical([turn, OnOff, Thing], Result, Mem) :- preposition(_, OnOff), !, parse2logical([switch, OnOff, Thing], Result, Mem).
parse2logical([switch, Thing, OnOff], Result, Mem) :- preposition(_, OnOff), !, parse2logical([switch, OnOff, Thing], Result, Mem).
parse2logical([switch, OnOff, Thing], switch(Spatial, OnOff, Thing), _Mem) :-
  preposition(_, OnOff), current_spatial(Spatial).


% parse2logical([look], look(Spatail), Mem) :- object_or_self(Tail, Agent, Mem).


parse2logical([CmdAlias|Tail], Action, Mem) :-
  cmdalias(CmdAlias, Verb),
  parse2logical([Verb|Tail], Action, Mem).

parse2logical([Prep], goto(Spatial, (*), Prep), _Mem) :- preposition(Spatial, Prep).
parse2logical([Dir], Logic, Mem):- (compass_direction(Dir);Dir==escape), !, parse2logical([go, Dir], Logic, Mem).

parse2logical([ExitName], goto(Spatial, (*), ExitName), Mem) :-
  thought_model(Spatial,ModelData, Mem),
  in_model(h(Spatial, exit(ExitName), _, _, _), ModelData).

parse2logical([Verb|Args], Action, _M) :- Args\==[], Args\==['.'], action_model(Verb, Spatial),
  %member(Verb, [agent, create, delprop, destroy, echo, quit, memory, model, path, properties, setprop, state, trace, notrace, whereami, whereis, whoami]),
  Action =.. [Verb, Spatial|Args].

parse2logical([Verb|Args], Action, _M) :-
  %member(Verb, [agent, create, delprop, destroy, echo, quit, memory, model, path, properties, setprop, state, trace, notrace, whereami, whereis, whoami]),
  Action =.. [Verb|Args].


