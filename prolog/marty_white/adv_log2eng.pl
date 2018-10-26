/*
%  NomicMUD: A MUD server written in Prolog
%  Maintainer: Douglas Miles
%  Dec 13, 2035
%
%  Bits and pieces:
%
%    LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
%
%  Copyright (C) 2004 Marty White under the GNU GPL
%  Sept 20, 1999 - Douglas Miles
%  July 10, 1996 - John Eikenberry
%
%  Logicmoo Project changes:
%
% Main file.
%
*/

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CODE FILE SECTION
% :- ensure_loaded('adv_log2eng').
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reason2eng(cant(sense( Sense, It, Why)), [ 'You can''t sense', It, ' ', ly(Sense), ' here', cuz(Why)]).
reason2eng(cant(reach(Spatial, It)), [  'You can''t reach ', It, ' ', ly(Spatial), '.']).
reason2eng(cant(manipulate(Spatial, self)), [ 'You can''t manipulate yourself like that', ly(Spatial), '.']).
reason2eng(alreadyhave(It), ['You already have the', It, '.']).
reason2eng(mustgetout(_It), ['You must get out/off it first.']).
reason2eng(self_relation(_Spatial, _It), ['Can\'t put thing inside itself!']).
reason2eng(moibeus_relation( _, _), ['Topological error!']).
reason2eng(state(Dark, t),        ['It''s too ', Dark, ' to ', ly(Sense), '!']):- problem_solution(Dark, Sense, _Light).
reason2eng(mustdrop(It), [ 'You will have to drop', It, ' first.']).
reason2eng(cant(move(Spatial, _Thing)), ['Sorry, it\'s immobile', ly(Spatial), '.']).
reason2eng(cantdothat(EatCmd),    [ 'Sorry, you can\'t do: ', EatCmd, '.']).
reason2eng(R, R).

% A percept or event:
%   - is a logical description of what happened
%   - includes English or other translations
%   - may be queued for zero, one, many, or all agents.
%   - may have a timestamp
% queue_percpt(Agent, [Logical, English|_], S0, S9).
%   where Logical is always first, and other versions are optional.
%   Logical should be a term, like sees(Thing).
%   English should be a list.

% Inform notation
%   'c'        character)
%   "string"   string
%   "~"        quotation mark
%   "^"        newline
%   @          accent composition, variables 00 thru 31
%   \          line continuation
% Engish messages need to be printable from various perspectives:
%   person (1st/2nd/3rd), tense(past/present)
%   "You go south." / "Floyd wanders south."
%       {'$agent $go $1', ExitName }
%       { person(Agent), tense(go, Time), ExitName, period }
%       {'$p $t $w', Agent, go, ExitName}
%   "You take the lamp." / "Floyd greedily grabs the lamp."
%       Agent=floyd, {'%p quickly grab/T %n', Agent, grab, Thing }
%               else {'%p take/T %n', Agent, take, Thing }
%   %p  Substitute parameter as 1st/2nd/3rd person ("I"/"you"/"Floyd").
%         Implicit in who is viewing the message.
%         Pronouns: gender, reflexive, relative, nominative, demonstratve...?
%   %n  Substitute name/description of parameter ("the brass lamp").
%   /T  Modify previous word according to tense ("take"/"took").
%         Implicit in who is viewing the message?  Context when printed?
%   /N  Modify previous word according to number ("coin"/"coins").
%         What number?
%   %a  Article - A or An (indefinite) or The (definite) ?
%
%  I go/grab/eat/take
%  you go/grab/eat/take
%  she goes/grabs/eats/takes
%  floyd goes/grabs/eats/takes
%
%  eng(subject(Agent), 'quickly', verb(grab, grabs), the(Thing))
%  [s(Agent), 'quickly', v(grab, grabs), the(Thing)]

capitalize([First|Rest], [Capped|Rest]) :-
  capitalize(First, Capped).
capitalize(Atom, Capitalized) :-
  atom(Atom), % [] is an atom
  downcase_atom(Atom, Lower),
  atom_chars(Lower, [First|Rest]),
  upcase_atom(First, Upper),
  atom_chars(Capitalized, [Upper|Rest]).

context_agent(Agent, Context):-
  member(agent(Agent), Context).
context_agent(Agent, Context):-
  member(inst(Agent), Context).
% compile_eng(Context, Atom/Term/List, TextAtom).
%  Compile Eng terms to ensure subject/verb agreement:
%  If subject is agent, convert to 2nd person, else use 3rd person.
%  Context specifies agent, and (if found) subject of sentence.

compile_eng(_Context, Done, '') :- Done == [], !.
compile_eng(Context, [First|Rest], [First2|Rest2]) :-
  compile_eng(Context, First, First2),
  compile_eng(Context, Rest, Rest2).

compile_eng(Context, subj(Agent), Person) :-
  context_agent(Agent, Context),
  member(person(Person), Context).
compile_eng(Context, subj(Other), Compiled) :-
  compile_eng(Context, Other, Compiled).
compile_eng(Context, Agent, Person) :-
  context_agent(Agent, Context),
  member(person(Person), Context).
compile_eng(Context, person(Second, _Third), Compiled) :-
  member(subj(Agent), Context),
  context_agent(Agent, Context),
  compile_eng(Context, Second, Compiled).
compile_eng(Context, person(_Second, Third), Compiled) :-
  compile_eng(Context, Third, Compiled).
compile_eng(Context, tense(Verb, Tense), Compiled) :-
  verb_tensed(Context, Verb, Tense, Compiled).
compile_eng(Context, cap(Eng), Compiled) :-
  compile_eng(Context, Eng, Lowercase),
  capitalize(Lowercase, Compiled).
compile_eng(_Context, silent(_Eng), '').

compile_eng(_Context, ly(spatial), '').
compile_eng(Context, ly(Word), Spatially) :- !,
  compile_eng(Context, Word, Spatial),
  atom(Spatial),
  atom_concat(Spatial, "ly", Spatially).
compile_eng(Context, ing(Word), Spatially) :- !,
  compile_eng(Context, Word, Spatial),
  atom(Spatial),
  atom_concat(Spatial, "ing", Spatially).

compile_eng(Context, s(Word), Spatially) :- % TODO make actually plural
  compile_eng(Context, Word, Spatial),
  atom(Spatial),
  atom_concat(Spatial, "s", Spatially).

compile_eng(Context, DetWord, AThing) :-
  compound(DetWord), DetWord=..[Det, Word],
  member(Det, [the, some, a, an]),
  compile_eng(Context, [Det, Word], AThing).


%compile_eng(_Context, Atom, Atom):- \+ atom(Atom), !.
compile_eng(Context, Inst, TheThing):- inst_of(Inst, Type, N), !,
   (nth0(N, [(unknown), the, thee, old, some, a], Det) -> true; atom_concat('#',N,Det)),
   compile_eng(Context, [Det, Type], TheThing).
compile_eng(_Context, Atom, Atom).

verb_tensed(Context, Verb, past, Compiled):- 
  compile_eng(Context, Verb, Word),
  pasitfy_word(Word, Compiled).
verb_tensed(Context, Verb, _Tense, Compiled):- 
  compile_eng(Context, Verb, Compiled).


pasitfy_word(take,took).
pasitfy_word(make,made).
pasitfy_word(move,moved).
pasitfy_word(eat,ate).
pasitfy_word(eat,ate).
pasitfy_word(Verb,Compiled):- \+ atom(Verb),!,Compiled=Verb.
pasitfy_word(Verb,Compiled):- atomic_concat(Verb,'ed', Compiled).


nospace(_, ',').
nospace(_, ';').
nospace(_, ':').
nospace(_, '.').
nospace(_, '?').
nospace(_, '!').
nospace(_, '\'').
nospace('\'', _).
nospace(_, '"').
nospace('"', _).
nospace(_, Letter) :- char_type(Letter, space).
nospace(Letter, _) :- char_type(Letter, space).

no_space_words('', _).
no_space_words(_, '').
no_space_words(W1, W2) :-
  atomic(W1),
  atomic(W2),
  atom_chars(W1, List),
  last(List, C1),
  atom_chars(W2, [C2|_]),
  nospace(C1, C2).

insert_spaces([W], [W]).
insert_spaces([W1, W2|Tail1], [W1, W2|Tail2]) :-
  no_space_words(W1, W2),
  !,
  insert_spaces([W2|Tail1], [W2|Tail2]).
insert_spaces([W1, W2|Tail1], [W1, ' ', W3|Tail2]) :-
  insert_spaces([W2|Tail1], [W3|Tail2]).
insert_spaces([], []).
                    
make_atomic(Atom, Atom) :-
  atomic(Atom), !.
make_atomic(Term, Atom) :-
  term_to_atom(Term, Atom).

eng2txt(Agent, Person, Eng, Text) :-  assertion(nonvar(Eng)),
  % Find subject, if any.
  findall(subj(Subject), findterm(subj(Subject), Eng), Context),
  % Compile recognized structures.
  maplist(compile_eng([agent(Agent), person(Person)|Context]), Eng, Compiled),
  % Flatten any sub-lists.
  flatten(Compiled, FlatList),
  % Convert terms to atom-strings.
  findall(Atom, (member(Term, FlatList), make_atomic(Term, Atom)), AtomList),
  findall(Atom2, (member(Atom2, AtomList), Atom2\=''), AtomList2),
  % Add spaces.
  bugout('insert_spaces(~w)~n', [AtomList2], printer),
  insert_spaces(AtomList2, SpacedList),
  % Return concatenated atoms.
  concat_atom(SpacedList, Text).
eng2txt(_Agent, _Person, Text, Text).

%portray(ItemToPrint) :- print_item_list(ItemToPrint).  % called by print.

list2eng([], ['<nothing>']).
list2eng([Single], [Single]).
list2eng([Last2, Last1], [Last2, 'and', Last1]).
list2eng([Item|Items], [Item, ', '|Tail]) :-
  list2eng(Items, Tail).

prop2eng(_Obj, h(_Spatial, ExitDown, Object, Speaker), ['The', Object, 'has', Exit, Down, 'to', Speaker]):- ExitDown=..[Exit, Down].
prop2eng(_Obj, h(_Spatial, Held_by, Object, Speaker), ['The', Object, 'is', Held_by, Speaker]).

prop2eng( Obj, EmittingLight, ['The', Obj, 'is glowing.']):- EmittingLight == emmiting(light), !.
prop2eng(_Obj, fragile(_), ['It looks fragile.']).
prop2eng(_Obj, shiny,  ['It\'s shiny!']).
prop2eng( Obj, effect(_, _), Out):- prop2eng(Obj, adjs(special), Out), !.
prop2eng(_Obj, desc(Out), Out):- !.
prop2eng(_Obj, can_do(Eat, t), ['Able to', Eat , '.']).
prop2eng(_Obj, can_do(Eat, f), ['Unable to', Eat , '.']).

prop2eng(_Obj, can_be(eat, t), ['It looks tasty ', '!']).
prop2eng(_Obj, can_be(Eat, t), ['Can be', tense(Eat, past), '.']).
prop2eng(_Obj, can_be(Eat, f), ['Can\'t be', tense(Eat, past), '.']).

prop2eng(_Obj, state(Open, t), ['It is', Open ,  '.']).
prop2eng(_Obj, state(Open, f), ['It is not', Open , '.']).
prop2eng( Obj, inherit(Type, t), Out):-   prop2eng(Obj, adjs(Type), Out), !.
prop2eng( Obj, inherit(Type), Out):-   prop2eng(Obj, adjs(Type), Out), !.
prop2eng( Obj, inherited(Type), Out):- prop2eng(Obj, nouns(Type), Out), !.
prop2eng(_Obj, adjs(Type), [cap(Type), '.']).
prop2eng(_Obj, nouns(Type), [cap(Type), '.']).
prop2eng(_Obj, Prop, [cap(N), is, V, '.']):- Prop =..[N, V].
prop2eng(_Obj, _Prop,  []).

prop2eng_txtl( Obj, Prop, UText1):- prop2eng(Obj, Prop, UText1), UText1 \==[], !.
prop2eng_txtl(_Obj, Prop, Text):- reason2eng(Prop, Text)-> Prop\==Text, !.
prop2eng_txtl(_Obj, Prop, [String]):- format(atom(String), ' {{ ~q. }}  ', [Prop]), !.



proplist2eng(_Obj, [], []).
proplist2eng(Obj, [Prop|Tail], Text) :- !,
  proplist2eng(Obj, Tail, UText2) ->
  flatten([UText2], Text2),
  prop2eng_txtl(Obj, Prop, UText1) -> 
  flatten([UText1], Text1),
  append_if_new(Text1, Text2, Text), !.
proplist2eng(Obj, Prop, Text) :- prop2eng_txtl(Obj, Prop, Text), !.

append_if_new1(Text1, Text2, Text):- flatten([Text1], TextF1), flatten([Text2], TextF2), append([_|TextF1], _, TextF2), !, Text=Text2.

append_if_new(Text1, Text2, Text):- append_if_new1(Text1, Text2, Text), !.
append_if_new(Text2, Text1, Text):- append_if_new1(Text1, Text2, Text), !.
append_if_new(Text1, Text2, Text):- append(Text1, Text2, Text), !.

%print_percept(Agent, sense(Sense, [you_are(Spatial, Prep, Here),
%                         exits_are(Exits),
%                         here_are(Nearby)...])) :-
%  findall(X, (member(X, Nearby), X\=Agent), OtherNearby),
%  player_format('You are ~p the ~p.  Exits are ~p.~nYou see: ~p.~n',
%         [Prep, Here, Exits, OtherNearby]).

logical2eng(_Agent, [], []).
logical2eng(Agent, [Prop|Tail], Text) :- !,
  logical2eng(Agent, Tail, UText2) ->
  flatten([UText2], Text2),
  logical2eng(Agent, Prop, UText1) -> 
  flatten([UText1], Text1),
  append_if_new(Text1, Text2, Text), !.

logical2eng(Agent, sense(_See, Sensing), SensedText) :- logical2eng(Agent, Sensing, SensedText).

logical2eng(Agent, you_are(Prep, Here), [cap(subj(Agent)), person(are, is), Prep, 'the', Here, '.', '\n']).
logical2eng(Agent, you_are(_Spatial, Prep, Here), [cap(subj(Agent)), person(are, is), Prep, 'the', Here, '.', '\n']).
logical2eng(_Agent, exits_are(Exits), ['Exits are', ExitText, '.', '\n']):- list2eng(Exits, ExitText).
logical2eng(Agent, here_are(Nearby), [cap(subj(Agent)), person(see, sees), ':', SeeText, '.']):-
  findall(X, (member(X, Nearby), X\=Agent), OtherNearby),
  list2eng(OtherNearby, SeeText).

logical2eng(Agent, carrying(Spatial, Items),
            [cap(subj(Agent)), person(are, is), ly(Spatial), 'carrying:'|Text]) :-
  list2eng(Items, Text).

logical2eng(_Agent, notice_children(_See, _Parent, _Prep, []), []).
logical2eng(Agent, notice_children(Sense, Parent, Prep, List),
            [cap(Prep), 'the', Parent, subj(Agent), person(Sense, s(Sense)), ':'|Text]) :-
  list2eng(List, Text).

logical2eng(_Agent, moved( What, From, Prep, To),
            [cap(subj(What)), 'moves', ' from', From, 'to', Prep, To]).


logical2eng(_Agent, transformed(Before, After), [Before, 'turns into', After, .]).

logical2eng(_Agent, destroyed(Thing), [Thing, 'is destroyed.']).

logical2eng(Agent, sense_props(Sense, Object, PropList), 
            [cap(subj(Agent)), person(Sense, s(Sense)), Desc, '.'|PropDesc] ) :-
  select(name(Desc), PropList, SubPropList),
  proplist2eng(Object, SubPropList, PropDesc).

logical2eng(Agent, sense_props(Sense, Object, PropList), 
            [cap(subj(Agent)), person(Sense, s(Sense)), 'a', Object, '.'|PropDesc] ) :-
  proplist2eng(Object, PropList, PropDesc).

%logical2eng(_Agent, emote( say, Speaker, (*), Eng), [cap(subj(Speaker)), ': "', Text, '"']) :-  eng2txt(Speaker, 'I', Eng, Text).
logical2eng(_Agent, emoted( see, Speaker, Audience, Eng),
    ['to', Audience, ', "', Text, '"']) :-
  eng2txt(Speaker, Speaker, Eng, Text).

logical2eng(_Agent, emoted( Says, Speaker, Audience, Eng),
    [cap(subj(Speaker)), s(Says), 'to', Audience, ', "', Text, '"']) :-
  eng2txt(Speaker, 'I', Eng, Text).

logical2eng(_Agent, emote( Says, Audience, Eng),
    [cap(subj(do)), s(Says), 'to', Audience, ', "', Text, '"']) :-
  eng2txt(me, 'I', Eng, Text).

logical2eng(_Agent, time_passes, []).
% logical2eng(_Agent, time_passes, ['Time passes.']).
logical2eng(_Agent, failure(Action), ['Action failed:', Action]).

logical2eng(Agent, PropList, [cap(subj(Agent)), person(see, sees), ':'|PropDesc] ) :-
  dmust(proplist2eng(something, PropList, PropDesc)), !.

% logical2eng(_Agent, Logical, ['percept:', Logical]).


percept2txt(Agent, [_Logical, English], Text) :-
  eng2txt(Agent, you, English, Text).
percept2txt(Agent, [_Logical, English|More], Text) :-
  eng2txt(Agent, you, [English|More], Text).
percept2txt(Agent, [Logical|_], Text) :-
  logical2eng(Agent, Logical, Eng),
  eng2txt(Agent, you, Eng, Text).

the(State, Object, Text) :-
  getprop(Object, name(D), State),
  atom_concat('the ', D, Text).

an(State, Object, Text) :-
  getprop(Object, name(D), State),
  atom_concat('a ', D, Text).

num(_Singular, Plural, [], Plural).
num(Singular, _Plural, [_One], Singular).
num(_Singular, Plural, [_One, _Two|_Or_More], Plural).

expand_english(State, the(Object), Text) :-
  the(State, Object, Text).
expand_english(State, an(Object), Text) :-
  an(State, Object, Text).
expand_english(_State, num(Sing, Plur, List), Text) :-
  num(Sing, Plur, List, Text).
expand_english(_State, [], '').
expand_english(State, [Term|Tail], [NewTerm|NewTail]) :-
  expand_english(State, Term, NewTerm),
  expand_english(State, Tail, NewTail).
expand_english(_State, Term, Term).



