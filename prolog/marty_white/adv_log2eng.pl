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

capitalize([First|Rest], [Capped|Rest]) :- !,
  capitalize(First, Capped).
capitalize(Atom, Capitalized) :-
  atom(Atom), % [] is an atom
  downcase_atom(Atom, Lower),
  atom_chars(Lower, [First|Rest]),
  upcase_atom(First, Upper),
  atom_chars(Capitalized, [Upper|Rest]).

context_agent(Agent, Context):-
  declared(agent(Agent), Context).
context_agent(Agent, Context):-
  declared(inst(Agent), Context).
% compile_eng(Context, Atom/Term/List, TextAtom).
%  Compile Eng terms to ensure subject/verb agreement:
%  If subject is agent, convert to 2nd person, else use 3rd person.
%  Context specifies agent, and (if found) subject of sentence.

compile_eng(_Context, Done, '') :- Done == [], !.
compile_eng(Context, [cap(subj(Agent)), aux(be)|More], Person) :- !,
  compile_eng(Context, [cap(subj(Agent)),  person(are, is)|More], Person) .  

compile_eng(Context, [First|Rest], [First2|Rest2]) :-
  compile_eng(Context, First, First2),
  compile_eng(Context, Rest, Rest2).

compile_eng(_Context, aux(be), 'is') :- !.

compile_eng(Context, subj(Agent), Person) :-
  context_agent(Agent, Context),
  declared(person(Person), Context).
compile_eng(Context, subj(Other), Compiled) :-
  compile_eng(Context, Other, Compiled).
compile_eng(Context, Agent, Person) :-
  context_agent(Agent, Context),
  declared(person(Person), Context).
compile_eng(Context, person(Second, _Third), Compiled) :-
  declared(subj(Agent), Context),
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
  member(Det, [the, some, a, an, '']),
  compile_eng(Context, [Det, Word], AThing).

%compile_eng(_Context, Atom, Atom):- \+ atom(Atom), !.
compile_eng(Context, Inst, TheThing):- inst_of(Inst, Type, N), !,
   (nth0(N, [(unknown), '', thee, old, some, a], Det) -> true; atom_concat('#',N,Det)),
   compile_eng(Context, [Det, Type], TheThing).

/*compile_eng(Context, Prop, Text):- \+ atomic(Prop),
   log2eng(you,Prop,TextMid),!,
   compile_eng(Context,['\n'|TextMid],Text), !.
*/

compile_eng(_Context, Prop, Text):- format(atom(Text),'~w',[Prop]).


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
  flatten([Compiled], FlatList),
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
  
:- nb_setval(list2eng_e, []).

list2eng(Obj, Some, English):-
  list2eng([], Obj, Some, English).

punct_or(Punct,Else,Value):- member(Else=Value,Punct)-> true ; Else=Value.

list2eng(Punct,_Obj, [], [Nothing]):- punct_or(Punct,'<nothing>',Nothing).
list2eng(Punct, Obj, Some, English) :- \+ is_list(Some), !, 
   punct_or(Punct,log2eng,Log2Eng),
   call(Log2Eng, Obj, Some, English),!.

%list2eng(_Punct, Obj, Some, [' [' | English]) :- nb_current(list2eng_e,D), number(D), list2eng_e(['.'=']','and'=','], Obj, Some, English), !.
list2eng(Punct, Obj, Some, English) :- nb_current(list2eng_e,D),b_setval(list2eng_e,1), list2eng_e(Punct, Obj, Some, English), !,
  b_setval(list2eng_e,D).


list2eng_e(Punct, Obj, [Single], English) :- !,
  punct_or(Punct,log2eng,Log2Eng),
  call(Log2Eng, Obj, Single, Named),
  punct_or(Punct,'.',PERIOD),
  flatten([Named, PERIOD], English).

list2eng_e(Punct, Obj, [Last2, Last1], English) :-   
  punct_or(Punct,log2eng,Log2Eng),
  call(Log2Eng, Obj, Last2, Named2),
  list2eng(Obj, Last1, Named1),
  punct_or(Punct,'and',AND),
  punct_or(Punct,'.',PERIOD),
  flatten([Named2, AND, Named1, PERIOD], English).

list2eng_e(Punct, Obj, [Some| More], English) :- 
  punct_or(Punct,log2eng,Log2Eng),
  call(Log2Eng, Obj, Some, Named),
  punct_or(Punct,',',COMMA),
  list2eng_e(Punct, Obj, More, MoreNamed),
  flatten([Named, COMMA, MoreNamed], English).

list2eng_e(Punct, Obj, Some, English) :- 
  punct_or(Punct,log2eng,Log2Eng),
  call(Log2Eng, Obj, Some, English),!.
  

log2eng(Obj, Some, English):- dmust(logic2eng(Obj, Some, English)).

logic2eng(Obj, Var, [Text]):- var(Var),!, format(atom(Text),'{{~q}}',[log2eng(Obj, Var)]).
logic2eng(_Obj, '$VAR'(Prop), English):- format(atom(English), ' ?VAR-~w', [Prop]), !.
logic2eng( Obj, Prop, English):- 
  \+ ground(Prop), copy_term(Prop,Prop2),!,
  numbervars(Prop2,55,_),
  log2eng( Obj, Prop2, English).
logic2eng(_Obj, [], []).
logic2eng(_Obj, cap(Prop), cap(Prop)):-!.
logic2eng(_Obj, subj(Prop), subj(Prop)):-!.
logic2eng(_Obj,[subj(Prop)|Tail], [Prop|Tail]) :- !.
logic2eng(_Obj,[cap(Prop)|Tail], [Prop|Tail]) :- !.
logic2eng(Obj, [Prop|Tail], Text) :- !,
  dmust((log2eng(Obj, Tail, UText2) ->
  flatten([UText2], Text2),
  dmust(log2eng(Obj, Prop, UText1)) -> 
  flatten([UText1], Text1),
  append_if_new(Text1, Text2, Text))), !.

logic2eng( Obj, ~(Type), ['(','logically','not','(',Out, '))']):- dmust(log2eng(Obj, Type, Out)), !.

logic2eng(_Agent, time_passes, []).
% log2eng(_Agent, time_passes, ['Time passes.']).

logic2eng(Agent, you_are(Prep, Here), [cap(subj(Agent)), person(are, is), Prep, 'the', Here, '\n']).

logic2eng(Agent, exits_are(Exits), ['Exits are', ExitText, '\n']):- list2eng(Agent, Exits, ExitText).
logic2eng(Agent, here_are(Nearby), [cap(subj(Agent)), person(see, sees), ':', SeeText]):-
  exclude(=@=(Agent), Nearby, OtherNearby), list2eng(Agent, OtherNearby, SeeText).

logic2eng(Agent, carrying(Items),
            [cap(subj(Agent)), 'carrying:'|Text]) :-
  list2eng(Agent, Items, Text).

logic2eng(_Agent, notice_children(_See, _Parent, _Prep, []), []).
logic2eng(Agent, notice_children(Sense, Parent, Prep, List),
            [cap(Prep), 'the', Parent, subj(Agent), person(Sense, s(Sense)), ':'|Text]) :-
  list2eng(Parent, List, Text).

logic2eng(_Agent, moved( What, From, Prep, To),
            [cap(subj(What)), 'moves', ' from', From, Prep, 'to',  To]).


logic2eng(_Agent, transformed(Before, After), [Before, 'turns into', After, .]).

logic2eng(_Agent, destroyed(Thing), [Thing, aux(be), 'destroyed.']).


logic2eng(Agent, sense_props(Sense, Object, PropList), 
            [ %cap(subj(Agent)),
               subj(Agent),
               person(Sense, s(Sense))| English] ) :-
   log2eng(Agent, props(Object, PropList),English).

logic2eng(_Agent, props(Object, PropList),  [the(Object), ': ('|English] ) :- list2eng(['.'=')'],Object, PropList, English).

logic2eng(_Agent, memories(Object, PropList),  ['\n\n', the(Object), ' remembers:\n'|English] ) :- 
  reverse(PropList,PropListR),
  list2eng([','=',\n',log2eng=percept2eng],Object, PropListR, English).
logic2eng(_Agent, perceptq(Object, PropList),  ['\n\n', the(Object), ' notices:\n'|English] ) :- 
  list2eng([','=',\n'],Object, PropList, English).

logic2eng(Agent, did(Action),  [the(Agent), 'did: ', English] ) :- format(atom(English),'~q',[Action]).

logic2eng(Agent, did(Action),  [the(Agent), 'did: '|English] ) :- 
    logic2eng(Agent, Action, English ).

%log2eng(_Agent, emote( say, Speaker, (*), Eng), [cap(subj(Speaker)), ': "', Text, '"']) :-  eng2txt(Speaker, 'I', Eng, Text).
logic2eng(_Agent, emoted( see, Speaker, Audience, Eng),
    [a(Speaker), 'to', Audience, ', "', Text, '"']) :-
  eng2txt(Speaker, Speaker, Eng, Text).

logic2eng(_Agent, emoted( Says, Speaker, Audience, Eng),
    [cap(subj(Speaker)), s(Says), 'to', Audience, ', "', Text, '"']) :-
  eng2txt(Speaker, 'I', Eng, Text).

logic2eng(_Agent, emote( Says, Audience, Eng),
    [cap(subj(do)), s(Says), 'to', Audience, ', "', Text, '"']) :-
  eng2txt(me, 'I', Eng, Text).

logic2eng(_Agent, failure(Action), ['Action failed:', Action]).

logic2eng(Agent, sense(See, Sensing), [cap(subj(Agent)), person(See, s(See)), ': '|SensedText]) :- 
   log2eng(Agent, Sensing, SensedText).

%logic2eng( Obj, effect(_, _), Out):- log2eng(Obj, adjs(special), Out), !.

logic2eng(_Obj, h(_Spatial, ExitDown, Object, Speaker), [the(Object), 'has', Exit, Down, 'to', Speaker]):- 
  compound(ExitDown), 
  ExitDown=..[Exit, Down].

  
logic2eng(_Obj, timestamp(Ord,Time), [timestamp,is,Ord,'(',MinutesSecs,' ago )']):- clock_time(Now),Dif is round((Now - Time)*10)/10,
   Minutes is round(Dif) // 60,
   Secs is round(Dif) rem 60,
   (Secs<10 
     -> format(atom(MinutesSecs),'~w:0~ws',[Minutes,Secs])
     ; format(atom(MinutesSecs),'~w:~ws',[Minutes,Secs])).

logic2eng(_Obj, h(_Spatial, Held_by , Object, Speaker), [the(Object), aux(be), Held_by, Speaker]).

logic2eng(_Obj, EmittingLight, [aux(be), 'glowing']):- EmittingLight == emmiting(light), !.
logic2eng(_Obj, fragile(_), ['looks fragile']).
logic2eng(_Obj, shiny,  [aux(be), 'shiny!']).
logic2eng(_Obj, can_do(Eat), ['Able to', Eat ]).
logic2eng(_Obj, can_do(Eat, f), ['Unable to', Eat ]).


logic2eng(_Obj, state(Open), [aux(be), Open ]).
logic2eng(_Obj, state(Open, f), [aux(be), 'not', Open ]).

logic2eng( Obj, initial(Desc),  ['initially described as'| Out]):-  log2eng( Obj, Desc,  Out).
logic2eng( Obj, co(Desc),  ['(Created as: ', Out, ')']):-  list2eng( Obj, Desc,  Out).

logic2eng( Obj, inherit(Type), ['inherits',Out]):- log2eng(Obj, nouns(Type), Out), !.
logic2eng( Obj, inherit(Type, f), ['isnt '|Out]):-   log2eng(Obj, adjs(Type), Out), !.
logic2eng( Obj, inherited(Type), ['is',Out]):- log2eng(Obj, nouns(Type), Out), !.
logic2eng(_Obj, adjs(Type), [cap(Type)]).
logic2eng(_Obj, nouns(Type), [cap(Type)]).

logic2eng(_Aobj, cant(sense( Sense, It, Why)), [ 'can''t sense', It, ' ', ly(Sense), ' here', cuz(Why)]).
logic2eng(_Aobj, cant(reach(Spatial, It)), [  'can''t reach ', It, ' ', ly(Spatial)]).
logic2eng(_Aobj, cant(manipulate(Spatial, self)), [ 'can''t manipulate yourself like that', ly(Spatial)]).
logic2eng(_Aobj, alreadyhave(It), ['already have', the(It)]).
logic2eng(_Aobj, mustgetout(It), ['must get out/off ',It,' first.']).
logic2eng(_Aobj, self_relation(_Spatial, It), ['can\'t put ',It,' inside itself!']).
logic2eng(_Aobj, moibeus_relation( _, _), ['Topological error!']).
logic2eng(_Aobj, state(Dark, t),        ['It''s too ', Dark, ' to ', ly(Sense), '!']):- problem_solution(Dark, Sense, _Light).
logic2eng(_Aobj, mustdrop(It), [ 'will have to drop', It, ' first.']).
logic2eng(_Aobj, cant(move(Spatial, It)), [It,aux(be),'immobile', ly(Spatial)]).
logic2eng(_Aobj, cantdothat(EatCmd),    [ 'can\'t do: ', EatCmd]).

%log2eng(_Obj, oper(OProp, [cap(N), aux(be), V]):- Prop =..[N, V].



logic2eng(_Obj, has_rel(Quantity,Ammount,TF) , [TF,that,'has a,',Quantity,Ammount]).
 
logic2eng(_Obj, has_rel(on), ['has a surface']).
logic2eng(_Obj, has_rel(in), ['has innerds']).
logic2eng(_Obj, has_rel(exit(_)), ['has exits']).
logic2eng(_Obj, can_be(eat), ['looks tasty ', '!']).
logic2eng(_Obj, can_be(Eat), ['Can be', tense(Eat, past)]).
logic2eng(_Obj, can_be(Eat, f), ['Can\'t be', tense(Eat, past)]).

logic2eng( Obj, oper(Act,Precond,PostCond), OUT) :- 
   (xtreme_english->OUT = ['{{',if,'action: ',ActE,' test:', PrecondE,'resulting: ',PostCondE,'}}'];
   OUT = []),
   maplist(log2eng(Obj), [Act,Precond,PostCond], [ActE,PrecondE,PostCondE]).


logic2eng( Obj, Prop, English):- Prop =..[N, Spatial| VRange],Spatial==spatial,Prop2 =..[N| VRange], log2eng( Obj, Prop2, English).
logic2eng( Obj, Prop, English):- Prop =..[N, Obj1, A| VRange],Obj1==Obj,Prop2 =..[N, A| VRange], log2eng( Obj, Prop2, English).
logic2eng( Obj, Prop, English):- Prop =..[N, V, T| VRange],T==t,Prop2 =..[N, V| VRange], log2eng( Obj, Prop2, English).

logic2eng(Context, Inst, TheThing):- atom(Inst), inst_of(Inst, Type, N), !,
   (nth0(N, [(unknown), '', thee, old, some, a], Det) -> true; atom_concat('#',N,Det)),
   compile_eng(Context, [Det, Type], TheThing).

logic2eng(_Obj, desc(Out), [' "',Out,'"']):- !.
logic2eng(_, V,[String]):- (string(V);(atom(V),atom_needs_quotes(V))),!, format(atom(String), ' "~w" ', [V]), !.

logic2eng( Obj, Prop, [cap(N),of,O, aux(be), Value]):- Prop =..[N,O, V], list2eng(Obj, V, Value).
logic2eng( Obj, Prop, [cap(N), aux(be), Value]):- Prop =..[N, V], list2eng(Obj, V, Value).
%logic2eng(_Obj, Prop, [String]):- compound(Prop), !, String=''. % format(atom(String), ' \n   {{ ~q. }}\n   ', [Prop]), !.
logic2eng(_Obj, Prop, [String]):- compound(Prop), \+ xtreme_english, !, format(atom(String), ' {{ ~q }} ', [Prop]), !.
logic2eng( Obj, Prop, [cap(N), Value, aux(be),  English]):- Prop =..[N, V| Range],
   log2eng(Obj, V, Value),
   maplist(logic2eng(Obj), Range, English).

logic2eng(_Obj, Prop, [String]):- format(atom(String), '~w', [Prop]), !.

atom_needs_quotes(V):-format(atom(VV),'~q',[V]),V\==VV.

append_if_new1(Text1, Text2, Text):- flatten([Text1], TextF1), flatten([Text2], TextF2), append([_|TextF1], _, TextF2), !, Text=Text2.

xtreme_english :- true.

append_if_new(Text1, Text2, Text):- append_if_new1(Text1, Text2, Text), !.
append_if_new(Text2, Text1, Text):- append_if_new1(Text1, Text2, Text), !.
append_if_new(Text1, Text2, Text):- append(Text1, Text2, Text), !.

%print_percept(Agent, sense(Sense, [you_are(Prep, Here),
%                         exits_are(Exits),
%                         here_are(Nearby)...])) :-
%  findall(X, (member(X, Nearby), X\=Agent), OtherNearby),
%  player_format('You are ~p the ~p.  Exits are ~p.~nYou see: ~p.~n',
%         [Prep, Here, Exits, OtherNearby]).

% log2eng(_Agent, Logical, ['percept:', Logical]).

percept2eng(_Agent, [_Logical, English], English) :- !.
percept2eng(_Agent, [_Logical, English|More], [English|More]) :- !.
percept2eng(Agent, [Logical|_], Eng) :- log2eng(Agent, Logical, Eng),!.
percept2eng(Agent, LogicalEnglish, Eng) :- log2eng(Agent, LogicalEnglish, Eng).

percept2txt(Agent, LogicalEnglish, Text) :-
  percept2eng(Agent, LogicalEnglish, English),
  eng2txt(Agent, Agent, English, Text).

the(State, Object, Text) :-
  getprop(Object, name(D), State),
  compile_eng(State, D, AD),
  atom_concat('the ', AD, Text).

an(State, Object, Text) :-
  getprop(Object, name(D), State),
  compile_eng(State, D, AD),
  atom_concat('a ', AD, Text).

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



