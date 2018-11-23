
:- discontiguous aXiom//2.
 
will_touch(Agent,Thing, S0, S2):- 
  touchable(Agent,Thing, S0),S0=S2.
  

aXiom(doing, wait(Agent)) -->
 queue_agent_percept(Agent, [time_passes(Agent)]).

aXiom(doing, Action, _S0, _S9):- notrace(( \+ trival_act(Action),bugout1(aXiom(doing, Action)))),notrace(fail).

aXiom(doing, talk(Agent, Object, Message)) -->  % directed message
  can_sense(Agent, audio, Object),
  from_loc(Agent, Here),
  queue_local_event([talk(Agent, Here, Object, Message)], [Here]).

aXiom(doing, say(Agent, Message)) -->          % undirected message
  from_loc(Agent, Here),                              
  queue_local_event([say(Agent, Here, Message)], [Here]).

/*
aXiom(doing, emote(Agent, EmoteType, Object, Message)) --> !, % directed message
 dmust((
 action_sensory(EmoteType, Sense),
 can_sense(Agent, Sense, Object),
 % get_open_traverse(EmoteType, Sense), h(Sense, Agent, Here), 
 queue_local_event([emoted(Agent, EmoteType, Object, Message)], [Here,Object]))).

*/

aXiom(doing, print_(Agent, Msg)) -->
  h(descended, Agent, Here),
  queue_local_event(msg_from(Agent, Msg), [Here]).


% ==============
%  WALK WEST
% ==============
aXiom(_, status_msg(_Begin,_End)) --> [].

aXiom(doing, goto_dir(Agent, Walk, ExitName)) -->         % go n/s/e/w/u/d/in/out  
  must_act(status_msg(vBegin,goto_dir(Agent, Walk, ExitName))),
  dmust(from_loc(Agent, Here)),  
  %dmust(h(exit(ExitName), Here, _There)),
  unless(Agent,h(exit(ExitName), Here, _There),
  (aXiom(doing, leaving(Agent, Here, Walk, ExitName)),
   must_act(status_msg(vDone,goto_dir(Agent, Walk, ExitName))))).

aXiom(_, leaving(Agent, Here, Walk, ExitName)) -->
  %member(At, [*, to, at, through, thru]),
  h(exit(ExitName), Here, There),
  aXiom(_, terminates(h(_, Agent, Here))),
  queue_local_event( leaving(Agent, Here, Walk, ExitName), [Here]),
   % queue_local_event( msg([cap(subj(Agent)), leaves, Here, ing(Walk), to, the, ExitName]), [Here]).
  sg(reverse_dir(ExitName,ExitNameR)),
  dmust(aXiom(doing, arriving(Agent, There, Walk, ExitNameR))).

aXiom(_, terminates(h(Prep, Object, Here))) -->
 %ignore(sg(declared(h(Prep, Object, Here)))),
 undeclare(h(Prep, Object, Here)).

aXiom(_, arriving(Agent, Here, Walk, ReverseDir)) -->
  queue_local_event( arriving(Agent, Here, Walk, ReverseDir), [Here]),
  %sg(default_rel(PrepIn, Here)), {atom(PrepIn)},
  {PrepIn = in},
  % [cap(subj(Agent)), arrives, PrepIn, Here, ing(Walk), from, the, ReverseDir] 
  dmust(aXiom(_, initiates(h(PrepIn, Agent, Here)))),
  dmust(add_look(Agent)).

aXiom(_, initiates(h(Prep, Object, Dest))) -->
 declare(h(Prep, Object, Dest)).




% ==============
%  WALK TABLE
% ==============
aXiom(doing, goto_obj(Agent, Walk, Object)) --> 
  has_rel(At, Object), 
  aXiom(doing, goto_prep_obj(Agent, Walk, At, Object)).


% ==============
%  WALK ON TABLE
% ==============
aXiom(doing, goto_prep_obj(Agent, Walk, At, Object)) --> 
  will_touch(Agent, Object),
  has_rel(At, Object),  
  \+ is_closed(At, Object), 
  aXiom(doing, entering(Agent, Walk, Object, At)).

aXiom(doing, entering(Agent, Walk, Object, At)) -->
  from_loc(Object, Here),
  moveto(Agent, Walk, Agent, At, Object, [Here],
    [subj(Agent), person(Walk, es(Walk)), At, the, Object, .]),
  add_look(Agent).

% ==============
%  GOTO PANTRY
% ==============
aXiom(doing, goto_loc(Agent, _Walk, There)) -->           % go some room
  has_rel(exit(_), There),
  aXiom(doing, make_true(Agent, h(in, Agent, There))).

aXiom(doing, make_true(Agent, FACT)) --> 
  add_agent_goal(Agent, FACT).    

aXiom(doing, make_true(Doer, h(in, Agent, There))) -->  
  {Doer==Agent},
  has_rel(exit(_), There),
  from_loc(Agent, Here),
  agent_thought_model(Agent, ModelData),
  {find_path(Here, There, Route, ModelData)}, !,
  aXiom(doing, follow_plan(Agent, goto_loc(Agent, walk, There), Route)).

aXiom(doing, follow_plan(Agent, Name, [Step|Route])) -->
  aXiom(doing, follow_step(Agent, Name, Step)),
  aXiom(doing, follow_plan(Agent, Name, Route)).

aXiom(doing, follow_step(Agent, Name, Step)) -->
  {bugout1(follow_step(Agent, Name, Step))},
  must_act(Step).


%  sim(verb(args...), preconds, effects)
%    Agent is substituted for Agent.
%    preconds are in the implied context of a State.
%  In Inform, the following are implied context:
%    actor, action, noun, second
%  Need:
%    actor/agent, verb/action, direct-object/obj1, indirect-object/obj2,
%      preposition-introducing-obj2
%sim(put(Obj1, Obj2),
%    (  h(descended, Thing, Agent),
%      can_sense(Agent, Sense, Agent, Where),
%      has_rel(Relation, Where),
%      h(descended, Agent, Here)),
%    moveto(Agent, Put, Thing, Relation, Where, [Here],
%      [cap(subj(Agent)), person('put the', 'puts a'),
%        Thing, Relation, the, Where, '.'])).
aXiom(doing, does_put(Agent, Put, Thing1, At, Thing2)) --> 
  from_loc(Agent, Here),
  % moveto(Agent, Put, Thing1, held_by, Recipient, [Here], [cap(subj(Agent)), person([give, Recipient, the], 'gives you a'), Thing, '.'],
  moveto(Agent, Put, Thing1, At, Thing2, [Here], 
    [cap(subj(Agent)), person(Put, es(Put)), Thing1, At, Thing2, '.']).
  
aXiom(doing, take(Agent, Thing)) --> !,
  % [silent(subj(Agent)), person('Taken.', [cap(Doer), 'grabs the', Thing, '.'])]
  will_touch(Agent, Thing),
  aXiom(doing, does_put(Agent, take, Thing, held_by, Agent)).

aXiom(doing, drop(Agent, Thing)) --> !,
  will_touch(Agent, Thing), 
  h(At, Agent, Here),
  % has_rel(At, Here),
  aXiom(doing, does_put(Agent, drop, Thing, At, Here)).

aXiom(doing, put(Agent, Thing1, Prep, Thing2)) -->
  has_rel(At, Thing2),
  prep_to_rel(Thing2, Prep, At),
  (At \= in ; \+ is_closed(At, Thing2)),
  will_touch(Agent, Thing2), % what if "under" an "untouchable" thing?
  % OK, put it
  must_act( does_put(Agent, put, Thing1, At, Thing2)).

aXiom(doing, give(Agent, Thing, Recipient)) -->
  has_rel(held_by, Recipient),
  will_touch(Agent, Thing),
  will_touch(Recipient, Agent),
  % OK, give it
  must_act( does_put(Agent, give, Thing, held_by, Recipient)).

% throw ball up
aXiom(doing, throw_dir(Agent, Thing, ExitName)) --> 
  from_loc(Agent, Here),
  aXiom(doing, throw_prep_obj(Agent, Thing, ExitName, Here)).

% throw ball at catcher
aXiom(doing, throw_at(Agent, Thing, Target)) -->
  aXiom(doing, throw_prep_obj(Agent, Thing, at, Target)).

% throw ball over homeplate
aXiom(doing, throw_prep_obj(Agent, Thing, Prep, Target)) -->
  prep_to_rel(Target, Prep, Rel),
  aXiom(doing, throwing(Agent, Thing, Rel, Target)).

% is throwing the ball...
aXiom(doing, throwing(Agent, Thing, At, Target)) -->
  will_touch(Agent, Thing),
  can_sense(Agent, see, Target),
  aXiom(doing, thrown(Agent, Thing, At, Target)).

% has thrown the ball...
aXiom(doing, thrown(Agent, Thing, AtTarget, Target)) -->
  ignore((getprop(Thing, breaks_into(Broken)),
  bugout3('object ~p is breaks_into~n', [Thing], general),
  aXiom(doing, thing_transforms(Thing,Broken)))),
  aXiom(doing, disgorge(Agent, throw, Target, AtTarget, Target, [Target], 'Something falls out.')).

aXiom(doing, thing_transforms(Thing,Broken))  --> 
  undeclare(h(At, Thing, Here)),
  declare(h(At, Broken, Here)),
  queue_local_event([transformed(Thing, Broken)], Here).
  

aXiom(doing, hit_with(Agent, Thing, With)) -->
  from_loc(Agent, Here),
  hit(Agent, Thing, With, [Here]),
  queue_agent_percept(Agent, [true, 'OK.']).

aXiom(doing, hit(Agent, Thing)) -->
  from_loc(Agent, Here),
  hit(Agent, Thing, Agent, [Here]),
  queue_agent_percept(Agent, [true, 'OK.']).

hit(Doer, Target, _With, Vicinity) -->
 ignore(( % Only brittle items use this
  getprop(Target, breaks_into(Broken)),
  bugout3('target ~p is breaks_into~n', [Target], general),
  undeclare(h(Prep, Target, Here)),
  queue_local_event([transformed(Target, Broken)], Vicinity),
  declare(h(Prep, Broken, Here)),
  disgorge(Doer, hit, Target, Prep, Here, Vicinity, 'Something falls out.'))).


aXiom(doing, dig(Agent, Hole, Where, Tool)) -->
  {memberchk(Hole, [hole, trench, pit, ditch]),
  memberchk(Where, [garden]),
  memberchk(Tool, [shovel, spade])},
  open_traverse(Tool, Agent),
  h(in, Agent, Where),
  \+  h(_At, Hole, Where),
  % OK, dig the hole.
  declare(h(in, Hole, Where)),
  setprop(Hole, default_rel(in)),
  setprop(Hole, can_be(move, f)),
  declare(h(in, dirt, Where)),
  queue_event(
    [ created(Hole, Where),
      [cap(subj(Agent)), person(dig, digs), 'a', Hole, 'in the', Where, '.']]).

aXiom(doing, eat(Agent, Thing)) -->
  (getprop(Thing, can_be(eat,t)) -> 
  (undeclare(h(_, Thing, _)),queue_agent_percept(Agent, [destroyed(Thing), 'Mmmm, good!'])) ;
  queue_agent_percept(Agent, [failure(eat(Thing)), 'It''s inedible!'])).


aXiom(doing, switch(Agent, OnOff, Thing)) -->
  will_touch(Agent, Thing),
  getprop(Thing, can_be(switched(OnOff), t)),
  getprop(Thing, effect(switch(OnOff), Term0)),
  {subst(equivalent, ($(self)), Thing, Term0, Term)},
  call(Term),
  queue_agent_percept(Agent, [true, 'OK']).
/*
aXiom(doing, open(Agent, Thing)) -->
  will_touch(Agent, Thing),
  %getprop(Thing, openable),
  %\+ getprop(Thing, open),
  delprop(Thing, closed(true)),
  %setprop(Thing, open),
  setprop(Thing, closed(fail)),
  open_traverse(Agent, Here),
  queue_local_event([setprop(Thing, closed(fail)), 'Opened.'], [Here]).
aXiom(doing, close(Agent, Thing)) -->
  will_touch(Agent, Thing),
  %getprop(Thing, openable),
  %getprop(Thing, open),
  delprop(Thing, closed(fail)),
  %delprop(Thing, open),
  setprop(Thing, closed(true)),
  open_traverse(Agent, Here),
  queue_local_event([setprop(Thing, closed(true)), 'Closed.'], [Here]).
*/

aXiom(doing, inventory(Agent)) -->
  can_sense(Agent, see, Agent),
  must_act( does_inventory(Agent)).

aXiom(doing, does_inventory(Agent)) -->
  findall(What, h(child, What, Agent), Inventory),
  queue_agent_percept(Agent, [rel_to(held_by, Inventory)]).




% Agent looks
aXiom(doing, look(Agent)) --> 
  % Agent is At Here
  h(At, Agent, Here),
  % Agent looks At Here
  aXiom(doing, trys_examine(Agent, see, At, Here, depth(3))).

aXiom(doing, examine(Agent, Sense)) --> {is_sense(Sense)}, !, 
   dmust(from_loc(Agent, Place)),
   aXiom(doing, trys_examine(Agent, see, in, Place, depth(3))).

aXiom(doing, examine(Agent, Object)) --> aXiom(doing, trys_examine(Agent, see, at, Object, depth(3))). 
aXiom(doing, examine(Agent, Sense, Object)) --> aXiom(doing, trys_examine(Agent, Sense, at, Object, depth(3))), !.
aXiom(doing, examine(Agent, Sense, Prep, Object)) --> aXiom(doing, trys_examine(Agent, Sense, Prep, Object, depth(3))), !.

% listen, smell ...
aXiom(doing, Action) -->
 {notrace((Action=..[Verb,Agent|Args], 
 sensory_verb(Sense, Verb)))}, !,
 {NewAction=..[examine,Agent,Sense|Args]},
 aXiom(doing, NewAction).

% Here does not allow Sense?
aXiom(doing, trys_examine(Agent, Sense, Prep, Object, Depth)) -->
  \+ sg(can_sense_here(Agent, Sense)), !,
  must_act( failed(examine(Agent, Sense, Prep, Object, Depth), \+ can_sense_here(Agent, Sense))).
aXiom(doing, trys_examine(Agent, Sense, Prep, Object, Depth)) -->
  \+ can_sense(Agent, Sense, Object), !,
  must_act( failed(examine(Agent, Sense, Prep, Object, Depth), \+ can_sense(Agent, Sense, Object))).
aXiom(doing, trys_examine(Agent, Sense, Prep, Object, Depth)) --> aXiom(doing, does_examine(Agent, Sense, Prep, Object, Depth)).


aXiom(doing, does_examine(Agent, Sense, Prep, Object, Depth)) -->  dmust(act_examine(Agent, Sense, Prep, Object, Depth)),!.
aXiom(doing, does_examine(Agent, Sense, Object)) --> % {trace},
  %declared(props(Object, PropList)),
  findall(P, (getprop(Object, P), is_prop_public(Sense, P)), PropList),
  queue_agent_percept(Agent, [sense_props(Agent, Sense, Object, depth(2), PropList)]),
  (has_rel(At, Object); At='<unrelatable>'),
  % Remember that Agent might be on the inside or outside of Object.
  findall(What,
          (  h(child, What, Object), once(can_sense(Agent, Sense, What))),
          Children),
  queue_agent_percept(Agent, [sense_childs(Agent, Sense, Object, At, Children)]).



% used mainly to debug if things are locally accessable
aXiom(doing, touch(Agent, Thing)) --> !,
 unless_reason(Agent, will_touch(Agent, Thing),
   cant( reach(Agent, Thing))),
 queue_agent_percept(Agent, [success(touch(Agent, Thing),'Ok.')]).


aXiom(_, change_state(Agent, Open, Thing, Opened, TF)) --> !, 
  change_state(Agent, Open, Thing, Opened, TF).

aXiom(doing, Action, S0, S9) :-  
 notrace((action_verb_agent_thing(Action, Open, Agent, Thing),
 nonvar(Open), nonvar(Thing), nonvar(Agent))),
 act_change_state(Open, Opened, TF),!,
 aXiom(doing, change_state(Agent, Open, Thing, Opened, TF), S0, S9),!.


aXiom(doing, true) --> [].



/*


% Agent looks
aXiom(doing, look(Agent)) -->   
  % Agent is At Here
  h(At, Agent, Here),
  % Here allows sight
  sg(sense_here(see, Here)), !,
  % The agent does look At Here
  must_act( does_look(Agent, At, Here)).

% The agent does look At Here
aXiom(doing, does_look(Agent, At, Here)) --> !, 
  % The agent notices objects At Here
  aXiom(_, notices_objects_at(Agent, see, At, Here)),
    % The agent notices exits At Here
  aXiom(_, notices_exits_at(Agent, At, Here)).


aXiom(doing, notices_exits_at(Agent, AtHere, Here), S0, S9) :- !,
   findall(Direction, h(exit(Direction), Here, _, S0), Exits),
   queue_agent_percept(Agent, exits_are(Agent, AtHere, Here, Exits), S0, S9).
aXiom(doing, notices_exits_at(Agent, AtHere, Here)) -->
   findall(Direction, h(exit(Direction), Here, _), Exits),
   queue_agent_percept(Agent, exits_are(Agent, AtHere, Here, Exits)).

aXiom(doing, notices_objects_at(Agent, Sense, AtHere, Here), S0, S9) :- 
  findall(What,
          % all children of Here
          (h(child, What, Here, S0),
           % What can be seen
           can_sense(Agent, Sense, What, S0)),           
          
          % ( h(descended, What, Here), \+ (h(inside, What, Container), h(descended, Container, Here))),
          Nearby),
  
  queue_agent_percept(Agent, notice_children(Agent, Sense, Here, AtHere, depth(3), Nearby), S0, S9).


aXiom(doing, switch(Open, Thing)) -->
 act_prevented_by(Open, TF),
 will_touch(Agent, Thing),
 %getprop(Thing, can_be(open),
 %\+ getprop(Thing, =(open, t)),
 Open = open, traverses(Sense, Open)
 %delprop(Thing, =(Open, f)),
 %setprop(Thing, =(open, t)),
 setprop(Thing, =(Open, TF)),
 h(Sense, Agent, Here),
 queue_local_event([setprop(Thing, =(Open, TF)),[Open,is,TF]], [Here, Thing]).

aXiom(doing, switch(OnOff, Thing)) -->
 will_touch(Agent, Thing),
 getprop(Thing, can_be(switch, t)),
 getprop(Thing, effect(switch(OnOff), Term0)),
 subst(equivalent, $self, Thing, Term0, Term),
 call(Term),
 queue_agent_percept(Agent, [true, 'OK']).
*/
% todo

/*
disgorge(Doer, How, Container, At, Here, Vicinity, Msg) :-
  findall(Inner, h(child, Inner, Container), Contents),
  bugout3('~p contained ~p~n', [Container, Contents], general),
  moveto(Doer, How, Contents, At, Here, Vicinity, Msg).
disgorge(Doer, How, _Container, _At, _Here, _Vicinity, _Msg).
*/
disgorge(Doer, How, Container, Prep, Here, Vicinity, Msg) -->
  findall(Inner, h(child, Inner, Container), Contents),
   {bugout3('~p contained ~p~n', [Container, Contents], general)},
  moveto(Doer, How, Contents, Prep, Here, Vicinity, Msg).

:- defn_state_setter(moveto(agent,verb,listof(inst),domrel,dest,list(dest),msg)).
moveto(Doer, Verb, List, At, Dest, Vicinity, Msg) --> {is_list(List)},!,
 apply_map_state(moveto(Doer, Verb), List, rest(At, Dest, Vicinity, Msg)).
moveto(Doer, Verb, Object, At, Dest, Vicinity, Msg) -->
  undeclare(h(_, Object, From)),
  declare(h(At, Object, Dest)),
  queue_local_event([moved(Doer, Verb, Object, From, At, Dest), Msg], Vicinity).


event_props(thrown(Agent,  Thing, _Target, Prep, Here, Vicinity),
 [getprop(Thing, breaks_into(NewBrokenType)),
 bugout3('object ~p is breaks_into~n', [Thing], general),
 undeclare(h(_, Thing, _)),
 declare(h(Prep, NewBrokenType, Here)),
 queue_local_event([transformed(Thing, NewBrokenType)], Vicinity),
 disgorge(Agent, throw, Thing, Prep, Here, Vicinity, 'Something falls out.')]).

                                      
setloc_silent(Prep, Object, Dest) --> 
 undeclare(h(_, Object, _)),
 declare(h(Prep, Object, Dest)).


change_state(Agent, Open, Thing, Opened, TF,  S0, S):- 
 % dmust
 ((
 maybe_when(psubsetof(Open, touch),
   required_reason(Agent, will_touch(Agent, Thing, S0, _))),

 %getprop(Thing, can_be(open, S0),
 %\+ getprop(Thing, =(open, t), S0),

 required_reason(Agent, \+ getprop(Thing, can_be(Open, f), S0)),

 ignore(dshow_fail(getprop(Thing, can_be(Open, t), S0))),

 forall(act_prevented_by(Open,Locked,Prevented),
   required_reason(Agent, \+ getprop(Thing, =(Locked, Prevented), S0))),

 %delprop(Thing, =(Open, f), S0, S1),
 %setprop(Thing, =(Open, t), S0, S1),

  open_traverse(Agent, Here, S0),

 apply_forall(
  (getprop(Thing, effect(Open, Term0), S0),
  subst(equivalent,$self, Thing, Term0, Term1),
  subst(equivalent,$agent, Agent, Term1, Term2),
  subst(equivalent,$here, Here, Term2, Term)),
  call(Term),S0,S1),

 setprop(Thing, =(Opened, TF), S1, S2))),

 queue_local_event([setprop(Thing, =(Opened, TF)),msg([Thing,is,TF,Opened])], [Here, Thing], S2, S),!.

