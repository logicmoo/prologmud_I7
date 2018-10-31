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
%  Sept 20,1999 - Douglas Miles
%  July 10,1996 - John Eikenberry 
%
%  Logicmoo Project changes:
%
% Main file.
%
*/
*/



:- dynamic(adv:agent_last_action/3).

time_since_last_action(Agent,When):- 
  (adv:agent_last_action(Agent,_Action,Last),clock_time(T),When is T - Last) *-> true; clock_time(When).

set_last_action(Agent,Action):- 
   clock_time(T),
   retractall(adv:agent_last_action(Agent,_,_)),
   assertz(adv:agent_last_action(Agent,Action,T)).




% drop -> move -> touch
subsetof(touch, touch).
subsetof(move, touch).
subsetof(drop, move).
subsetof(eat, touch).
subsetof(hit, touch).
subsetof(put, drop).
subsetof(give, drop).
subsetof(take, move).
subsetof(throw, drop).
subsetof(open, touch).
subsetof(close, touch).
subsetof(lock, touch).
subsetof(unlock, touch).
subsetof(switch, touch).


% feel <- taste <- smell <- look <- listen  (by distance)
subsetof(examine, examine).
subsetof(listen, examine).
subsetof(look, examine).
% in order to smell it you have to at least be in sight distance
subsetof(smell, look).
subsetof(eat, taste).
subsetof(taste, smell).
subsetof(taste, feel).
subsetof(feel, examine).
subsetof(feel, touch).
subsetof(X,Y):- ground(subsetof(X,Y)),X=Y.

subsetof(SpatialVerb1, SpatialVerb2):- compound(SpatialVerb1), compound(SpatialVerb2), !,
  SpatialVerb1=..[Verb1,Arg1|_],
  SpatialVerb2=..[Verb2,Arg2|_],
  subsetof(Verb1, Verb2),
  subsetof(Arg1, Arg2).

subsetof(SpatialVerb, Verb2):- compound(SpatialVerb), functor(SpatialVerb, Verb, _), !,
  subsetof(Verb, Verb2).

subsetof(Verb, SpatialVerb2):- compound(SpatialVerb2), functor(SpatialVerb2, Verb2, _), !,
  subsetof(Verb, Verb2).

% proper subset - C may not be a subset of itself.
psubsetof(A, B):- A==B, !, fail.
psubsetof(A, B) :- subsetof(A, B).
psubsetof(A, C) :-
  subsetof(A, B),
  subsetof(B, C).


maybe_pause(Agent):- console_player(CP),(Agent==CP -> wait_for_input([user_input],_,0) ; true).

do_command(Agent, Action, S0, S1) :-
  do_metacmd(Agent, Action, S0, S1), !,   
  redraw_prompt(Agent).

do_command(Agent, Action, _, _) :- set_last_action(Agent,Action), fail.
  
do_command(Agent, Action, S0, S1) :-
  declared(memories(Agent, Mem), S0),
  do_introspect(Action, Answer, Mem),
  queue_percept(Agent, [answer(Answer), Answer], S0, S1), !.
  %player_format('~w~n', [Answer]).
do_command(Agent, Action, S0, S3) :-
  undeclare(memories(Agent, Mem0), S0, S1),
  copy_term(Action,ActionG),
  numbervars(ActionG,999,_),
  memorize(did(ActionG), Mem0, Mem1),
  declare(memories(Agent, Mem1), S1, S2),
  must_act(Agent, Action, S2, S3), !,
  nop(redraw_prompt(Agent)).
do_command(Agent, Action, S0, S0) :-
  player_format('Failed or No Such Command: ~w~n', Action), !,
  nop(redraw_prompt(Agent)).

% --------

do_todo(Agent, S0, S0):- 
  declared(memories(Agent, Mem0), S0),member(todo([]),Mem0),!.
do_todo(Agent, S0, S9) :- 
  undeclare(memories(Agent, Mem0), S0, S1),
  forget(todo(OldToDo), Mem0, Mem1),
  append([Action], NewToDo, OldToDo),
  memorize(todo(NewToDo), Mem1, Mem2),
  declare(memories(Agent, Mem2), S1, S2),
  apply_first_arg_state(Agent, do_command(Action), S2, S9).
do_todo(_Agent, S0, S0).

%do_todo_while(Agent, S0, S9) :-
%  declared(memories(Agent, Mem0), S0),
%  thought(todo(ToDo), Mem0),
%  append([Action], NewToDo, OldToDo),



% ---- apply_act(Agent, Action, State, NewState)
%  where the states also contain Percepts.
% In Inform, actions work in the following order:
%   game-wide preconditions
%   Player preconditions
%   objects-in-vicinity react_before conditions
%   room before-conditions
%   direct-object before-conditions
%   verb
%   objects-in-vicinity react_after conditions
%   room after-conditions
%   direct-object after-conditions
%   game-wide after-conditions
% In TADS:
%   "verification" methods perferm tests only

no_debug_cant(floyd, _).
no_debug_cant('floyd~1', _).
no_debug_cant(_, _).

apply_act(Agent, examine(How, Thing), State, NewState) :-
  (equals_efffectly(sense, Sense, _), equals_efffectly(model, Spatial, _)) ->
  Sense \== Spatial, How == Spatial, !,
  apply_act(Agent, examine(Sense, Thing), State, NewState).

apply_act(Agent, Action, State, NewState) :- no_debug_cant(Agent, Action),
  cant(Agent, Action, Reason, State),
  log2eng(Agent, Reason, Eng),
  queue_percept(Agent, [failure(Action, Reason), Eng], State, NewState), !.

apply_act(Agent, Action, State, NewState) :- \+ no_debug_cant(Agent, Action),
   \+ \+ cant(Agent, Action, _Reason, State),
  trace, rtrace(cant(Agent, Action, Reason, State)), !,
  log2eng(Agent, Reason, Eng),
  queue_percept(Agent, [failure(Action, Reason), Eng], State, NewState).

apply_act(Agent, Action, State, NewState):- act(Agent, Action, State, NewState), !.
apply_act(Agent, Act, State, NewState):- ((cmd_workarround(Act, NewAct) -> Act\==NewAct)), !, apply_act(Agent, NewAct, State, NewState).
apply_act(Agent, Action, _State, _NewState):- notrace((bugout(failed_act(Agent, Action)), fail)).

must_act(Agent, Action, State, NewState):- apply_act(Agent, Action, State, NewState) *-> ! ; fail.
% must_act(Agent, Action, S0, S1) :- rtrace(must_act(Agent, Action, S0, S1)), !.
must_act(Agent, Action, S0, S1) :-
  format(atom(Message), 'You can''t do that ~w. (unparsed (~p))', [Agent, Action]),
  queue_percept(Agent, [failure(Action), Message], S0, S1).


:- discontiguous act/4.

act(Agent, Action, State, NewState) :-
  act_verb_thing_model_sense(Action, Verb, _Thing, Spatial, Sense),
  sensory_verb(Sense, Verb),
  related(Spatial, Relation, Agent, Here, State),
  sensory_model_problem_solution(Sense, Spatial, _TooDark, _EmittingLight),
  findall(What,
          related(Spatial, child, What, Here, State),
          %(related(Spatial, descended, What, Here, State),
           %\+ (related(Spatial, inside, What, Container, State),
           %    related(Spatial, descended, Container, Here, State))),
          Nearby),
  findall(Direction, related(Spatial, exit(Direction), Here, _, State), Exits),
  !,
  queue_percept(Agent,
                [sense(Sense, [you_are(Relation, Here), exits_are(Exits), here_are(Nearby)])],
                State, NewState).

act(Agent, inventory, State, NewState) :- 
  Spatial = spatial,
  findall(What, related(Spatial, child, What, Agent, State), Inventory),
  queue_percept(Agent, [carrying(Spatial, Inventory)], State, NewState).

act(Agent, examine(Object), S0, S2) :- act(Agent, examine(see, Object), S0, S2).

act(Agent, examine(Sense, Object), S0, S2) :-
  %declared(props(Object, PropList), S0),
  ((
  findall(P, (getprop(Object, P, S0), is_prop_public(Sense,P)), PropListL),
  list_to_set(PropListL,PropList),
  queue_percept(Agent, [sense_props(Sense, Object, PropList)], S0, S1),
  (has_rel(Spatial, Relation, Object, S1); Relation='<unrelatable>'),
  % Remember that Agent might be on the inside or outside of Object.
  findall(What,
          (related(Spatial, child, What, Object, S1),
           once(can_sense( Sense, What, Agent, S1))),
          ChildrenL),
  list_to_set(ChildrenL,Children),
  queue_percept(Agent, [notice_children(Sense, Object, Relation, Children)], S1, S2))).


act(Agent, goto(Walk, Dir, Relation, Place), S0, S1):- !,
  (act_goto(Agent, Walk, Dir,  Relation, Place, S0, S1)->true;
  queue_percept(Agent,
                [failure(goto(Walk, Dir, Relation, Place)), 'You can\'t go that way'],
                S0, S1)).


act_goto(Agent, Walk, _Dir,  Relation, Object, S0, S9) :-  nonvar(Object),           % go in/on object
 get_open_traverse(Walk, Spatial, OpenTraverse),
  has_rel(Spatial, Relation, Object, S0),
  related(Spatial, OpenTraverse, Agent, Here, S0),
  related(Spatial, OpenTraverse, Object, Here, S0),
  \+ is_state(~(open), Object, S0),

  moveto(Spatial, Agent, Relation, Object, [Here, Object],
    [subj(Agent), person(get, gets), Relation, the, Object, .], S0, S1),
  must_act(Agent, look(Spatial), S1, S9).

% go n/s/e/w/u/d/in/out
act_goto(Agent, _Walk, Dir, _Relation, _Room, S0, S9) :- nonvar(Dir),  
  related(Spatial, child, Agent, Here, S0),
  related(Spatial, exit(Dir), Here, There, S0),!,
  %member(Relation, [*, to, at, through, thru]),
  has_rel(Spatial, PrepThere, There, S0),
  moveto(Spatial, Agent, PrepThere, There,
         [Here],
         [cap(subj(Agent)), person(go, goes), Dir],
         S0, S1),
  (related(Spatial, exit(RDir), There, Here, S0)-> true ; reverse_dir(Dir,RDir,S0)),
  queue_local_event(Spatial, [moved( Agent, Here, PrepThere, There), 
    the(Agent), person(arrived, arrives),from,the, RDir], [There], S1, S2),
  must_act(Agent, look(Spatial), S2, S9).

act_goto(Agent, _Walk, Dir, Relation, Room, S0, S9)  :- nonvar(Room),         % go in (adjacent) room
  has_rel(Spatial, Relation, Room, S0),!,
  get_open_traverse(Relation, Spatial, OpenTraverse),
  related(Spatial, OpenTraverse, Agent, Here, S0),
  related(Spatial, exit(Dir), Here, Room, S0),

  moveto(Spatial, Agent, Relation, Room, [Room, Here],
    [cap(subj(Agent)), person(go, goes), Dir], S0, S1),
  must_act(Agent, look(Spatial), S1, S9).

act_goto(Agent, _Walk, Dir,  _To, Room, S0, S9) :- nonvar(Room),             % go to (adjacent) room
  has_rel(Spatial, Relation, Room, S0),
  get_open_traverse(goto, Spatial, OpenTraverse),
  related(Spatial, OpenTraverse, Agent, Here, S0),
  related(Spatial, exit(Dir), Here, Room, S0),

  moveto(Spatial, Agent, Relation, Room, [Room, Here],
    [cap(subj(Agent)), person(go, goes), Dir], S0, S1),
  must_act(Agent, look(Spatial), S1, S9).

reverse_dir(Dir,RDir,S0):-
  related(Spatial, exit(Dir), Here, Room, S0),
  related(Spatial, exit(RDir), Room, Here, S0),!.
reverse_dir(Dir,RDir,S0):- 
  related(Spatial, Dir, Here, Room, S0),
  related(Spatial, RDir, Room, Here, S0),!.
reverse_dir(Dir,reverse(Dir),_).

%  sim(verb(args...), preconds, effects)
%    Agent is substituted for $self.
%    preconds are in the implied context of a State.
%  In Inform, the following are implied context:
%    actor, action, noun, second
%  Need:
%    actor/agent, verb/action, direct-object/obj1, indirect-object/obj2,
%      preposition-introducing-obj2
%sim(put(Spatial, Thing,Relation, Where),
%    ( related(Spatial, descended, Thing, $self),
%      has_sensory(Spatial, Sense, $self, Where),
%      has_rel(Spatial, Relation, Where),
%      related(Spatial, descended, $self, Here)),
%    moveto(Spatial, Thing, Relation, Where, [Here],
%      [cap(subj($self)), person('put the', 'puts a'),
%        Thing, Relation, the, Where, '.'])).

act(Agent, take( Thing), S0, S1) :-
  get_open_traverse(touch, Spatial, OpenTraverse),
  related(Spatial, OpenTraverse, Agent, Here, S0),     % Where is Agent now?
  moveto(Spatial, Thing, held_by, Agent, [Here],
    [silent(subj(Agent)), person('Taken.', [cap(Agent), 'grabs the', Thing, '.'])],
    S0, S1).
%act(Agent, get(Thing), State, NewState) :-
%  act(Agent, take( Thing), State, NewState).
act(Agent, drop(Thing), State, NewState) :-
  related(Spatial, Relation, Agent, Here, State),
  has_rel(Spatial, Relation, Here, State),
  moveto(Spatial, Thing, Relation, Here, [Here],
    [cap(subj(Agent)), person('drop the', 'drops a'), Thing, '.'], State, NewState).


act(Agent, put(Thing1, Relation, Thing2), State, NewState) :-
  act(Agent, put(spatial, Thing1, Relation, Thing2), State, NewState).

act(Agent, put(Spatial, Thing1, Relation, Dest), State, NewState) :-
  has_rel(Spatial, Relation, Dest, State),
  get_open_traverse(Open, _See, _Traverse, Spatial, OpenTraverse),
  (Relation \= in ; \+ is_state(~(Open), Dest, State)),
  reachable(Spatial, Dest, Agent, State), % what if "under" an "untouchable" thing?
  % OK, put it
  related(Spatial, OpenTraverse, Agent, Here, State),
  moveto(Spatial, Thing1, Relation, Dest, [Here],
      [cap(subj(Agent)), person('put the', 'puts a'), Thing1,
          Relation, the, Dest, '.'],
      State, NewState).

  
act(Agent, give( Thing, Recipient), S0, S9) :-
  has_rel(Spatial, held_by, Recipient, S0),
  reachable(Spatial, Recipient, Agent, S0),
  get_open_traverse(give, Spatial, OpenTraverse),
  % OK, give it
  related(Spatial, OpenTraverse, Agent, Here, S0),
  moveto(Spatial, Thing, held_by, Recipient, [Here],
    [cap(subj(Agent)), person([give, Recipient, the], 'gives you a'), Thing, '.'],
    S0, S9).

act(Agent, throw( Thing, at, Target), S0, S9) :-
  equals_efffectly(sense, Sense, see),
  can_sense( Sense, Target, Agent, S0),
  get_open_traverse(_Open, Sense, throw, Spatial, _OpenTraverse),
  % OK, throw it
  related(Spatial, Relation, Agent, Here, S0),
  thrown( Thing, Target, Relation, Here, [Here], S0, S1),
  hit( Target, Thing, [Here], S1, S9).
act(Agent, throw( Thing, Dir), S0, S9) :-
  related(Spatial, _Relation, Agent, Here, S0),
  related(Spatial, exit(Dir), Here, There, S0),
  has_rel(Spatial, PrepThere, There, S0),
  thrown( Thing, There, PrepThere, There, [Here, There], S0, S9).

act(Agent, hit( Thing), S0, S9) :-
  related(spatial, _Relation, Agent, Here, S0),
  hit( Thing, Agent, [Here], S0, S1),
  queue_percept(Agent, [true, 'OK.'], S1, S9).

act(Agent, dig( Hole, Where, Tool), S0, S9) :-
  memberchk(Hole, [hole, trench, pit, ditch]),
  memberchk(Where, [garden]),
  memberchk(Tool, [shovel, spade]),
  ((
  get_open_traverse(dig, Spatial, OpenTraverse),
  related(Spatial, OpenTraverse, Tool, Agent, S0),
  related(Spatial, in, Agent, Where, S0),
  \+ related(Spatial, _Relation, Hole, Where, S0),
  % OK, dig the hole.
  declare(h(Spatial, in, Hole, Where), S0, S1),
  setprop(Hole, has_rel(Spatial, in), S1, S2),
  setprop(Hole, can_be(move, f), S2, S3),
  declare(h(Spatial, in, dirt, Where), S3, S8),
  queue_event(
    [ created(Hole, Where),
      [cap(subj(Agent)), person(dig, digs), 'a', Hole, 'in the', Where, '.']],
    S8, S9))).

act(Agent, eat( Thing), S0, S9) :-
  getprop(Thing, can_be(eat, t), S0),
  undeclare(h(_Spatial, _, Thing, _), S0, S1),
  queue_percept(Agent, [destroyed(Thing), 'Mmmm, good!'], S1, S9).
act(Agent, eat( Thing), S0, S9) :-
  queue_percept(Agent, [failure(eat( Thing)), 'It''s inedible!'], S0, S9).

/*
act(Agent, switch(Open, Thing), S0, S) :-
  act_prevented_by(Open, TF),
  reachable(Spatial, Thing, Agent, S0),
  %getprop(Thing, can_be(open, S0),
  %\+ getprop(Thing, state(open, t), S0),
  Open = open, get_open_traverse(Open, Spatial, OpenTraverse),
  %delprop(Thing, state(Open, f), S0, S1),
  %setprop(Thing, state(open, t), S0, S1),
  setprop(Thing, state(Open, TF), S0, S2),
  related(Spatial, OpenTraverse, Agent, Here, S2),
  queue_local_event(Spatial, [setprop(Thing, state(Open, TF)),[Open,is,TF]], [Here, Thing], S2, S).

act(Agent, switch(OnOff, Thing), S0, S) :-
  reachable(Spatial, Thing, Agent, S0),
  getprop(Thing, can_be(switch, t), S0),
   getprop(Thing, effect(switch(OnOff), Term0), S0),
   subst(equivalent, $self, Thing, Term0, Term),
   call(Term, S0, S1),
  queue_percept(Agent, [true, 'OK'], S1, S).
*/
% todo
act_required_posses('lock','key',$agent).
act_required_posses('unlock','key',$agent).

act_change_opposite('lock','unlock').
act_change_opposite('open','close').

act_change_state('lock','locked',t).
act_change_state('open','opened',t).
act_change_state(Unlock,Locked,f):- act_change_state(Lock,Locked,t),act_change_opposite(Lock,Unlock).
act_change_state(switch(on),'powered',t).
act_change_state(switch(off),'powered',f).

act_change_state(switch(Open),Opened,TF):- nonvar(Open), act_change_state(Open,Opened,TF).

% act_prevented_by(Open,Opened,TF):- act_change_state(Open,Opened,TF).
act_prevented_by('open','locked',t).
act_prevented_by('close','locked',t).

act_to_cmd_thing(OpenThing, Open, Thing) :- 
  OpenThing =.. [Open, Thing],!.
act_to_cmd_thing(SwitchOnThing, SwitchOn, Thing) :- 
  SwitchOnThing =.. [Switch, On, Thing],!,
  SwitchOn=.. [Switch,On].

:- meta_predicate maybe_when(0,0).
:- meta_predicate required_reason(*,0).
:- meta_predicate unless_reason(*,0,*).
maybe_when(If,Then):- If -> Then ; true.
unless_reason(_Agent, Then,_Msg):- Then,!.
unless_reason(Agent,_Then,Msg):- player_format(Agent,'~N~p~n',Msg),!,fail.

required_reason(_Agent, Required):- Required,!.
required_reason(Agent, Required):- simplify_reason(Required,CUZ), player_format(Agent,'~N~p~n',cant(cuz(CUZ))),!,fail.

simplify_reason(_:Required, CUZ):- !, simplify_dbug(Required, CUZ).
simplify_reason(Required, CUZ):- simplify_dbug(Required, CUZ).

act(Agent, OpenThing, S0, S) :- 
   act_to_cmd_thing(OpenThing,Open, Thing), 
   act_change_state(Open, Opened, TF),!,
 dshow_fail((

   maybe_when(psubsetof(Open, touch),
      required_reason(Agent, reachable(Spatial, Thing, Agent, S0))),
   
   %getprop(Thing, can_be(open, S0),
   %\+ getprop(Thing, state(open, t), S0),

   required_reason(Agent, \+ getprop(Thing, can_be(Open, f), S0)),

   ignore(dshow_fail(getprop(Thing, can_be(Open, t), S0))),
   
   forall(act_prevented_by(Open,Locked,Prevented), 
          required_reason(Agent, \+ getprop(Thing, state(Locked, Prevented), S0))),


     %act_verb_thing_model_sense(OpenThing, Verb, Thing, Spatial, _Sense),

   %delprop(Thing, state(Open, f), S0, S1),
   %setprop(Thing, state(open, t), S0, S1),
   get_open_traverse(Open, Spatial, OpenTraverse),
      related(Spatial, OpenTraverse, Agent, Here, S0),

   apply_forall(
     (getprop(Thing, effect(Open, Term0), S0),
       subst(equivalent,$self, Thing, Term0, Term1),
       subst(equivalent,$agent, Agent, Term1, Term2),
       subst(equivalent,$here, Here, Term2, Term)),
       call(Term),S0,S1),

   setprop(Thing, state(Opened, TF), S1, S2),

   queue_local_event(Spatial, [setprop(Thing, state(Opened, TF)),[Open,is,TF]], [Here, Thing], S2, S))),!.

% used mainly to debug if things are reachable
act(Agent, touch(Thing), S0, S9) :-
  unless_reason(Agent, reachable(Spatial, Thing, Agent, S0),
          cant(reach(Spatial, Thing))),
  queue_percept(Agent, [true, 'OK.'], S0, S9).


act(Agent, emote( SAYTO, Object, Message), S0, S1) :- !, % directed message
  dmust((
  action_sensory(SAYTO, Sense),
  sensory_model(Sense, Spatial),
  get_open_traverse(SAYTO, Spatial, OpenTraverse),
  can_sense( Sense, Object, Agent, S0),
  related(Spatial, OpenTraverse, Agent, Here, S0),  
  queue_local_event(Spatial, [emoted( SAYTO, Agent, Object, Message)], [Here,Object], S0, S1))).
%act(Agent, say(Message), S0, S1) :-          % undirected message
%  related(Spatial, OpenTraverse, Agent, Here, S0),
%  queue_local_event(Spatial, [emoted( say, Agent, (*), Message)], [Here], S0, S1).

act(Agent, Wait, State, NewState) :- Wait == wait,
  queue_percept(Agent, [time_passes], State, NewState).
act(Agent, print_(Msg), S0, S1) :-
  related(Spatial, descended, Agent, Here, S0),
   queue_local_event(Spatial, [true, Msg], [Here], S0, S1).
act(_Agent, true, S, S).




cmd_workarround(VerbObj, VerbObj2):-
  VerbObj=..VerbObjL,
  notrace(cmd_workarround_l(VerbObjL, VerbObjL2)),
  VerbObj2=..VerbObjL2.

cmd_workarround_l([Verb|ObjS], [Verb|ObjS2]):-
   append(ObjS2, ['.'], ObjS).
cmd_workarround_l([Verb|ObjS], [Verb|ObjS2]):-
   append(Left, [L, R|More], ObjS), atom(L), atom(R),
   current_atom(Atom), atom_concat(L, RR, Atom), RR=R,
   append(Left, [Atom|More], ObjS2).
% look(Spatial) at screendoor
cmd_workarround_l([Verb, Relation|ObjS], [Verb|ObjS]):- is_ignorable(Relation), !.
% look(Spatial) at screen door
cmd_workarround_l([Verb1|ObjS], [Verb2|ObjS]):- verb_alias(Verb1, Verb2), !.

is_ignorable(at). is_ignorable(in). is_ignorable(to). is_ignorable(the). is_ignorable(a). is_ignorable(spatial).

verb_alias(look, examine) :- fail.


