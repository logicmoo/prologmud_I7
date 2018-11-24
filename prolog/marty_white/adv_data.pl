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

% Some Inform properties:
% light - rooms that have light in them
% can_be(eat, t) - can be eaten
% static - can't be taken or moved
% scenery - assumed to be in the room description (implies static)
% concealed - obscured, not listed, not part of 'all', but there
% found_in - lists places where scenery objects are seen
% absent - hides object entirely
% clothing - can be worn
% worn - is being worn
% container
% (opened = t) - container is open (must be opened) to be used. there is no "closed").
% can_be(open, t) - can be opened and closed
% capacity(N) - number of objects a container or supporter can hold
% state(locked, t) - cannot be opened
% can_be(lock, t), with_key
% enterable
% supporter
% article - specifies indefinite article ('a', 'le')
% cant_go
% daemon - called each turn, if it is enabled for this object
% description
% inside_description
% invent - code for inventory listing of that object
% list_together - way to handle "5 fish"
% plural - pluralized-name =  if different from singular
% when_closed - description when closed
% when_open - description when (opened = t)
% when_on, when_off - like when_closed, etc.
% Some TADS properties:
% thedesc
% pluraldesc
% is_indistinguishable
% is_visible(vantage)
% touchable($agent, actor)
% valid(verb) - is object seeable, touchable, etc.
% verification(verb) - is verb logical for this object
% Parser disambiguation:
% eliminate objs not see, touchable, etc.
% check preconditions for acting on a candidate object


:- op(1199, xfx, props).
:- op(1199, xfx, type).
% :- op(900, fy, '~').


dest_target(spatially(in,Dest),Target):- nonvar(Dest), !, dest_target(Dest,Target).
dest_target(spatially(to,Dest),Target):- nonvar(Dest), !, dest_target(Dest,Target).
dest_target(loc(_,_,_,Target),Target):- nonvar(Target), !.


type_functor(dest, spatially(in, inst)).
type_functor(dest, spatially(at, inst)).
type_functor(dest, spatially(on, inst)).
type_functor(dest, of(up,$here)).
type_functor(dest, of(west,$here)).


type_functor(nv_of_any, structure_label(term)).


type_functor(memory, goals(list(goals))).
type_functor(memory, todo(list(doing))).
%type_functor(memory, model(list(state_with_stamps))).
type_functor(event, timestamp(ordinal, timept)).

%type_functor(state_with_stamps, holds_at(h(domrel, inst, inst), timept)).

type_functor(state, type_props(type, list(nv))).
type_functor(state, props(inst, list(nv))).
type_functor(state, memories(inst, list(event))).
type_functor(state, preceptq(inst, list(event))).
type_functor(state, h(domrel, inst, inst)).


type_functor(doing, inventory(agent)). 
type_functor(doing, look(agent)).
type_functor(doing, examine(agent, optional(sense, see), optional(inst, here), optional(depth, 1))).
type_functor(event, sense_props(agent, sense, inst, depth, list(nv))).


type_functor(doing, dig(agent, holetype, prep, dest, inst)).
type_functor(doing, create(type)).

type_functor(doing, eat(agent, inst)).
type_functor(doing, hit(agent, inst, with)).
type_functor(doing, destroy(inst)).

type_functor(doing, switch(agent, tfstate, tf, inst)).
type_functor(doing, touch(agent, inst)).

type_functor(doing, touchable(agent, instance)).


%type_functor(doing, say(Message)).   % undirected message
type_functor(doing, emote(agent, emotype, dest, statement)).
type_functor(event, emoted(agent, emotype, dest, statement)).


type_functor(doing, wait(agent)).
type_functor(event, time_passes(agent)).


type_functor(doing, recall(agent, prop, inst2)).
type_functor(doing, properties(inst)).
type_functor(doing, inspect(agent, getprop(inst,nv))).
type_functor(doing, setprop(inst, nv)).
type_functor(doing, print_(agent, msg)).



type_functor(doing, give(agent, inst, agnt2)).
type_functor(doing, take(agent, inst)).
type_functor(doing, drop(agent, inst)).

type_functor(doing, goto_dir(agent, movetype, dir)).
type_functor(doing, goto_obj(agent, movetype, obj)).
type_functor(doing, goto_prep_obj(agent, movetype, domrel, obj)).

type_functor(doing, goto_loc(agent, movetype, dest)).

type_functor(doing, throw(agent, inst, dest)).
type_functor(doing, put(agent, inst, dest)).
type_functor(event, moved(agent, how, inst, from, prop, to)).



type_functor(event, carrying(agent, list(inst))).
type_functor(event, destroyed(inst)).
type_functor(event, did(action)).
type_functor(event, exits_are(agent, in, dest, list(exit))).
type_functor(event, notice_children(agent, sense, dest, domrel, depth, list(inst))).
type_functor(event, failed(doing, msg)).
type_functor(event, transformed(inst, inst2)).



type_functor(nv, adjs(list(text))).
type_functor(nv, can_be(actverb, tf)).
type_functor(nv, knows_verbs(actverb, tf)).
type_functor(nv, cant_go(inst, dir, text)).
type_functor(nv, class_desc(list(text))).
type_functor(nv, co(list(nv))).
type_functor(nv, desc(sv(text))).
type_functor(nv, door_to(inst)).
type_functor(nv, effect(verb_targeted, script)).
type_functor(nv, breaks_into = (type)).
type_functor(nv, has_rel(domrel, tf)).
type_functor(nv, has_sense(sense)).
type_functor(nv, inherit(type, tf)).
type_functor(nv, inherited(type)).
type_functor(nv, inheriting(type)).
type_functor(nv, inst(sv(term))).
type_functor(nv, isnt(type)).
type_functor(nv, name = (sv(text))).
type_functor(nv, nominals(list(text))).
type_functor(nv, nouns(list(text))).
type_functor(nv, oper(doing, preconds, postconds)).
type_functor(nv, =(name, value)).



is_state_info(StateInfo):- \+ compound(StateInfo),!,fail.
is_state_info(StateInfo):- functor(StateInfo,F,A),
  (functor_arity_state(F,A)->true; (A>2, functor_arity_state(F,2))). 
   
functor_arity_state(F,A):- functor(TypeFunctor,F,A), type_functor(state, TypeFunctor).
functor_arity_state(type,2).

push_to_state(StateInfo):- \+ compound(StateInfo),!.
push_to_state(StateInfo):- is_list(StateInfo),!,maplist(push_to_state,StateInfo).
push_to_state(type(Type, Conj)):-  !, push_to_state(props(type(Type), Conj)).
push_to_state(props(type(Type), Conj)):- !, props_to_list(Conj,List), push_to_state(type_props(Type, List)).
push_to_state(props(Obj, Conj)):-  props_to_list(Conj,List) -> Conj\== List,!, push_to_state(props(Obj, List)).
push_to_state(StateInfo):- StateInfo=..[F,Obj,E1,E2|More],functor_arity_state(F,2),!,StateInfoNew=..[F,Obj,[E1,E2|More]],!,push_to_state(StateInfoNew).
push_to_state(StateInfo):- is_state_info(StateInfo),!, declare(StateInfo,istate,_).
push_to_state(StateInfo):- forall(arg(_,StateInfo,Sub),push_to_state(Sub)).

correct_props(_Obj,PropsIn,PropsOut):- props_to_list(PropsIn,PropsOut), !.

props_to_list(Nil,[]):- assertion(\+ var(Nil)), Nil==[],!.
props_to_list(Atom,[inherit(Atom,t)]):- atom(Atom).
props_to_list(NC,[nc(NC)]):- \+ compound(NC),!.
props_to_list(@(Atom),[inherit(Atom,t)]):- atom(Atom).
props_to_list(~(Atom),[inherit(Atom,f)]):- atom(Atom).
props_to_list(oper(_,_,_),[]):-!.
props_to_list(~(can_be(Atom)),[can_be(Atom,f)]):- atom(Atom).
props_to_list( (can_be(Atom)),[can_be(Atom,t)]):- atom(Atom).
props_to_list([A|B],ABL):- !,
  props_to_list(A,AL),
  props_to_list(B,BL),
  append(AL,BL,ABL).
props_to_list((A,B),ABL):- !,
  props_to_list(A,AL),
  props_to_list(B,BL),
  append(AL,BL,ABL).
props_to_list(SV,[N=V]):- SV=..[N,V], single_valued_prop(N),!.
props_to_list(Other,[Other]).


:- dynamic(istate/1).

istate([ structure_label(istate),
   h(in, 'floyd~1', pantry),
   h(in, 'player~1', kitchen),
   h(worn_by, 'watch~1', 'player~1'),
   h(held_by, 'bag~1', 'player~1'),
   
   h(in, 'coins~1', 'bag~1'),
   h(held_by, 'wrench~1', 'floyd~1'),
   props('coins~1', [inherit(coins, t)]), 

 % Relationships

 h(exit(south), pantry, kitchen), % pantry exits south to kitchen
 h(exit(north), kitchen, pantry),
 h(exit(down), pantry, basement),
 h(exit(up), basement, pantry),
 h(exit(south), kitchen, garden),
 h(exit(north), garden, kitchen),
 h(exit(east), kitchen, dining_room),
 h(exit(west), dining_room, kitchen),
 h(exit(north), dining_room, living_room),
 h(exit(east), living_room, dining_room),
 h(exit(south), living_room, kitchen),
 h(exit(west), kitchen, living_room),

 h(in, a(shelf), pantry), % shelf is in pantry
 h(in, a(locker), pantry), % locker is in pantry
 h(in, a(rock), garden),
 h(in, a(fountain), garden),
 h(in, a(mushroom), garden),
 h(in, a(shovel), basement), % FYI shovel has no props (this is a lttle test to see what happens)
 h(in, a(videocamera), living_room),
 h(in, a(fireplace), living_room),
 h(in, screendoor, kitchen),
 h(in, a(crate), kitchen),
 h(in, a(apple), a(crate)),
 h(in, screendoor, garden),
 h(in, brklamp, garden)
 ]).


term_expansion(StateInfo,( :- push_to_state(StateInfo))):- is_state_info(StateInfo).


props('floyd~1', [name = ('Floyd the robot'), powered = f,  inherit(autonomous,t), 
   inherit(robot,t)]).

props('player~1', [name = ($self),inherit(console,t), inherit(humanoid,t)]).



basement props place,
 desc('This is a very dark basement.'),
 (dark= t).

dining_room props place.


:- push_to_state([
 props(garden, [
 inherit(place,t),
 % goto($agent, Prep, Dir, dir, result) provides special handling for going in a direction.
 cant_go($agent, up, 'You lack the ability to fly.'), 
 oper( /*garden, */ goto_dir($agent, _, south), 
   % precond(Test, FailureMessage)   
   precond(getprop(screendoor, (opened = t)), ['you must open the door first']),
   % body(clause)
   body(inherited)
 ),
 % cant_go provides last-ditch special handling for Go.
 cant_go($agent, _Dir, 'The fence surrounding the garden is too tall and solid to pass.')
 ]),
 props(kitchen, [inherit(place,t), desc('cooking happens here')]),
  h(reverse(on), a(table), a(table_leg)),
  h(on, a(box), a(table)),
  h(in, a(bowl), a(box)),
  h(in, a(flour), a(bowl)),
  h(in, a(table), kitchen), % a table is in kitchen
  h(on, a(lamp), a(table)), % a lamp is on the table

  h(in, a(sink), kitchen),
  h(in, a(plate), a(sink)),
  h(in, a(cabinate), kitchen), 
  h(in, a(cup), a(cabinate))]).

props(living_room, [inherit(place,t)]).

props(pantry, [
 inherit(place,t),
 nouns(closet),
 nominals(kitchen),
 desc('You\'re in a dark pantry.'),
 (dark= t)
]).

% Things
props('bag~1', [inherit(bag,t)]).

props(brklamp, 
  inherit(broken,t), 
  name = ('possibly broken lamp'),
  effect(switch(on), print_(_Agent,"Switch is flipped")),
  effect(hit, ['print_'("Hit brklamp"), setprop($self, inherit(broken,t))]),
  inherit(lamp,t)).

                  
props(screendoor, [
  % see DM4
  door_to(kitchen),
  door_to(garden),
  opened = f,
  inherit(door,t)
]).


door type
   ~can_be(take),
   can_be(open),
   can_be(close),
   (opened = t),
   nouns(door),
   fully_corporial.

food type
  can_be(eat),
  object,
  measurable.

%:- op(0, xfx, props).

:- multifile(extra_decl/2).
:- dynamic(extra_decl/2).

extra_decl(T,PP):- extra_decl0(T,P), correct_props(T,P,PP).
extra_decl0(T,P):-
 member(type_props(T,P), 
 [
   type_props(broken, [
    name = ('definately broken'),
    effect(switch(on), true),
    effect(switch(off), true),
    can_be(switch, t),
    adjs([dented]), 
    adjs($class)
   ]),
  type_props(mushroom, [
  % See DM4
  name = ('speckled mushroom'),
  % singular,
  inherit(food,t),
  nouns([mushroom, fungus, toadstool]),
  adjs([speckled]),
  % initial(description used until initial state changes)
  initial('A speckled mushroom grows out of the sodden earth, on a long stalk.'),
  % description(examination description)
  desc('The mushroom is capped with blotches, and you aren\'t at all sure it\'s not a toadstool.'),
  can_be(eat),
  % before(VERB, CODE) -- Call CODE before default code for VERB.
  %      If CODE succeeds, don't call VERB.
  before(eat, (random(100) =< 30, die('It was poisoned!'); 'yuck!')),
  after(take,
    (initial, 'You pick the mushroom, neatly cleaving its thin stalk.'))
  ]),

        type_props(door, [
         can_be(take, f),
         can_be(open, t),
         can_be(close, t),
         (opened = t),
         nouns(door),
         inherit(fully_corporial,t)
        ]),

   type_props(unthinkable, [
    can_be(examine, f), 
    adjs($class), 
    class_desc(['kind is normally unthinkable'])]),

   type_props(thinkable, [
    can_be(examine, t), 
    nouns($self), 
    adjs($class), 
    class_desc(['kind is normally thinkable'])]),

   type_props(noncorporial, [
    can_be(examine, f), 
    can_be(touch, f),
    inherit(thinkable,t),
    adjs($class), 
    inherit(fully_corporial,f),
    class_desc(['direct inheriters are completely noncorporial'])]), 

  type_props(only_conceptual, [ 
             adjs($class), 
    inherit(noncorporial, t), 
    inherit(thinkable, t), 
    class_desc(['kind is only conceptual'])]), 


   type_props(partly_noncorporial, [
    inherit(fully_corporial,t),
   adjs($class), 
    inherit(noncorporial,t),
    class_desc(['kind is both partly corporial and non-corporial'])]),

   type_props(fully_corporial, [
    can_be(touch, t),
    can_be(examine, t), 
    inherit(thinkable,t),
    cleanliness=clean,
    adjs($class), 
    class_desc(['kind is corporial'])]),

        type_props(object, [
         can_be(examine, t), 
         adjs(physical),
         can_be(move, t),
         inherit(fully_corporial, t), 
         inherit(thinkable,t),
         class_desc(['kind is an Movable Object'])]), 

        type_props(untakeable, [
         adjs($class), 
         can_be(take, f), 
         class_desc(['kind is an Immobile Object'])]), 


   type_props(furnature, [
    can_be(examine, t), 
    inherit(untakeable, t),
    inherit(fully_corporial, t), 
    inherit(surface, t), 
    inherit(thinkable,t),
    adjs(physical),
    class_desc(['kind is furnature'])]),

  % People
  type_props(character, [
   has_rel(worn_by, t), 
   has_rel(held_by, t), 
   % overridable defaults
   mass(50), volume(50), % liters  (water is 1 kilogram per liter)
   knows_verbs(eat, t),
   knows_verbs(examine, t),
   knows_verbs(touch, t),
   has_sense(see),
   inherit(perceptq,t),
   inherit(memorize,t),
   inherit(autoscan,t),
   inherit(partly_noncorporial,t)
  ]),

   type_props(natural_force, [
    knows_verbs(eat, f),

    knows_verbs(examine, t),
    can_be(touch, f),
    has_rel(held_by, f), 
    has_rel(worn_by, f), 
    has_sense(see),
    inherit(no_perceptq, t), 
    inherit(noncorporial,t),
    inherit(character,t)
   ]),

   type_props(humanoid, [
   knows_verbs(eat, t),
    volume(50), % liters  (water is 1 kilogram per liter)
    mass(50), % kilograms
    inherit(character,t),
    inherit(memorize,t),
    inherit(player,t),
 
    % players use power but cant be powered down
    can_be(switch(off), f), (powered= t)
   ]),

  type_props(autonomous, [inherit(autoscan,t)]),

  type_props(robot, [
  knows_verbs(eat, f),
  inherit(autonomous,t),
  emitting(see,light),
  volume(50), mass(200), % density(4) % kilograms per liter
  nouns([robot]), 
  adjs([metallic]), 
  desc('Your classic robot: metallic with glowing red eyes, enthusiastic but not very clever.'),
  can_be(switch, t),
  inherit(memorize,t),
  nouns($class), 
  inherit(shiny,t),
  inherit(character,t),
  (powered= t),
  % TODO: 'floyd~1' should `look($agent)` when turned back on.
   effect(switch(on), setprop($self, (powered= t))),
   effect(switch(off), setprop($self, (powered= f)))
  ]),

  % Places
  type_props(place, [
     volume_capacity = (10000),
     default_rel = in,
     has_rel(in, t),
     nouns([here,$self]),
     adjs([locally]),
     can_be(move, f),
     can_be(take, f),
       oper( discard($agent, Thing), 
           precond(h(child,$agent,Thing),['dont have']), % precond(Test, FailureMessage)
           body(take($agent, Thing, in, $self))),     % body(clause)
     % inherit(container,t),
     has_rel(exit(_), t)
  ]),

  type_props(container, [
    default_rel = in,
    has_rel(in, t),
    opened = f,
      oper( put($agent, Thing, in, $self), 
          precond(( ~(getprop(Thing, inherit(liquid,t)))), ['liquids would spill out']), % precond(Test, FailureMessage)
          body(take($agent, Thing, in, $self)))     % body(clause)
      % inherit(flask, f), 
    % adjs(flask, f)
   ]),

 type_props(console, [adjs(physical), nominals([console]), nouns([player])]), 
 type_props(telnet, [adjs([remote]), nouns([player])]), 
  type_props(bag, [
   inherit(container,t),
   inherit(object,t),
   volume_capacity = (10)
  ]),

  type_props(cup, [inherit(flask,t)]),

  type_props(flask, [
    adjs(physical),
    oper( put($agent, Thing, in, $self), 
     % precond(Test, FailureMessage)
     precond(getprop(Thing, inherit(fully_corporial,t)), ['non-physical would spill out']),
    % body(clause)
     body(take($agent, Thing, in, $self))),
    inherit(container, t), 
    inherit(object, t)
   ]),

  type_props(bowl, [  
   inherit(uncloseable,t), 
   inherit(flask, t), 
   volume_capacity = (2),
   breaks_into = (shards),
   cleanliness = dirty,
   name = ('porcelain bowl'),
   desc('This is a modest glass cooking bowl with a yellow flower motif glazed into the outside surface.')
  ]),
  type_props(plate, [
   inherit(surface, t),
   inherit(object,t),
   volume_capacity = (2),
   breaks_into = (shards),
   cleanliness = dirty,
   name = ('plate')
  ]),
  type_props(fireplace, [
   has_rel(on,f),
   has_rel(over,t),
   inherit(uncloseable,t),
   volume_capacity = (20), 
   inherit(furnature,t)
  ]),
  type_props(box, [
   inherit(container,t),
   inherit(object,t),
   volume_capacity = (11), 
   inherit(cardboard,t), 
   opened = f
  ]),
        type_props(crate, [
         inherit(container,t),
         inherit(object,t),
         volume_capacity = (13), 
         inherit(wooden,t), 
         (opened = t)
        ]),
        type_props(locker, [
         inherit(container,t),
         inherit(object,t),
         volume_capacity = (13), 
         inherit(metal,t), 
         opened = f
        ]),
        type_props(wooden, [
          breaks_into = (splinters),
          can_be(burn, t)
        ]),
        type_props(metal, [
          can_be(burn, f)
        ]),
        type_props(cardboard, [
          inherit(paper,t)
        ]),
        type_props(paper, [
          can_be(burn, t)
        ]),
  type_props(sink, [
   cleanliness = dirty,
   inherit(uncloseable,t),
   inherit(flask, t),   
   inherit(furnature,t),   
   volume_capacity = (5)  
  ]),
  type_props(uncloseable, [
     can_be(close,f),
     can_be(open,f),
     (opened =t),
     inherit(container,t)
  ]),
  type_props(cabinate, [
   inherit(container,t),
   inherit(furnature,t),
   volume_capacity = (10)
  ]),
  type_props(fountain, [   
   volume_capacity = (150),
   inherit(place,t),
   inherit(sink, t)
  ]),

 type_props(measurable, [adjs($class), ammount = some]), 

 
 % shiny things are fully_corporial
 type_props(shiny, [adjs($class), inherit(object, t), inherit(fully_corporial, t)]), 

 type_props(coins, [inherit(shiny,t),inherit(measurable,t)]),
  
  type_props(flour,[inherit(food,t),inherit(measurable,t)]),
 type_props(lamp, [
 name = ('shiny brass lamp'),
 nouns(light),
 nominals(brass),
 inherit(shiny,t),
 can_be(switch, t),
 (powered= t),
 inherit(object,t),
 emitting(see,light),
 effect(switch(on), setprop($self, emitting(see,light))),
 effect(switch(off), delprop($self, emitting(see,light))),
 breaks_into = (inherit(broken_lamp,t))
 ]),
 type_props(broken_lamp, [
 name = ('dented brass lamp'),
 % TODO: prevent user from referring to 'broken_lamp'
 nouns(light),
 nominals(brass),
 adjs(dented),
 can_be(switch, t),
 effect(switch(on), true),
 effect(switch(off), true) % calls true(S0, S1) !
 ]),

   type_props(surface, [has_rel(on, t),default_rel = on, adjs(physical), cleanliness=clean]), 

   type_props(shelf, [inherit(surface, t),adjs(physical),inherit(furnature, t)]), 

   type_props(table, [inherit(surface, t),adjs(physical), default_rel=on]), 

   type_props(wrench, [inherit(shiny,t)]),
   type_props(videocamera, [
   inherit(memorize,t),
   inherit(perceptq,t), 
   can_be(switch, t),
    effect(switch(on), setprop($self, (powered= t))),
    effect(switch(off), setprop($self, (powered= f))),
   (powered= t),
   has_sense(see),
   breaks_into = (broken_videocam)
   ]),
   type_props(broken_videocam, [can_be(switch, f), (powered= f), inherit(videocamera,t)])
 ]).

:- op(0, xfx, props).

