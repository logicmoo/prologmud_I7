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
% state(opened, t) - container is open (must be opened) to be used. there is no "closed").
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
% plural - pluralized-name if different from singular
% when_closed - description when closed
% when_open - description when state(opened, t)
% when_on, when_off - like when_closed, etc.
% Some TADS properties:
% thedesc
% pluraldesc
% is_indistinguishable
% is_visible(vantage)
% is_reachable(actor)
% valid(verb) - is object seeable, reachable, etc.
% verification(verb) - is verb logical for this object
% Parser disambiguation:
% eliminate objs not see, reachable, etc.
% check preconditions for acting on a candidate object


%:- op(900, xfx, props).
:- op(900, fy, '~').

dest_target(spatially(in,Dest),Target):- nonvar(Dest), !, dest_target(Dest,Target).
dest_target(spatially(to,Dest),Target):- nonvar(Dest), !, dest_target(Dest,Target).
dest_target(loc(_,_,_,Target),Target):- nonvar(Target), !.


type_functor(dest, spatially(in, inst)).
type_functor(dest, spatially(at, inst)).
type_functor(dest, spatially(on, inst)).
type_functor(dest, of(up,$here)).
type_functor(dest, of(west,$here)).


type_functor(nv_of_any, structure_label(term)).


type_functor(memory, goals(list_of(goals))).
type_functor(memory, todo(list_of(doing))).
type_functor(memory, model(list_of(state_with_stamps))).
type_functor(event, timestamp(ordinal, timept)).

type_functor(state_with_stamps, holds_at(h(domain, domrel, inst, inst), timept)).

type_functor(state, type_props(type, list_of(nv))).
type_functor(state, props(inst, list_of(nv))).
type_functor(state, memories(inst, list_of(event))).
type_functor(state, preceptq(inst, list_of(event))).
type_functor(state, h(domain, domrel, inst, inst)).


type_functor(doing, inventory(agnt)). 
type_functor(doing, look(agnt, optional(domain, spatial))).
type_functor(doing, examine(agnt, inst)).
type_functor(doing, examine(agnt, sense, inst)).
type_functor(event, sense_props(agnt, sense, inst, list_of(nv))).


type_functor(doing, dig(agnt, holetype, loc, inst)).
type_functor(doing, create(type, msg)).

type_functor(doing, eat(agnt, inst)).
type_functor(doing, hit(agnt, inst)).
type_functor(doing, destroy(inst, msg)).

type_functor(doing, switch(agnt, tfstate, tf, inst)).
type_functor(doing, touch(agnt, inst)).

%type_functor(doing, say(Message)).   % undirected message
type_functor(doing, emote(agnt, emotype, dest, statement)).
type_functor(event, emoted(agnt, emotype, dest, statement)).


type_functor(doing, wait(agnt)).
type_functor(event, time_passes(agnt)).


type_functor(doing, recall(agnt, prop, inst2)).
type_functor(doing, properties(inst)).
type_functor(doing, inspect(agnt, getprop(inst, propname))).
type_functor(doing, setprop(inst, nv)).
type_functor(doing, print_(agnt, msg)).



type_functor(doing, give(agnt, inst, agnt2)).
type_functor(doing, take(agnt, inst)).
type_functor(doing, drop(agnt, inst)).

type_functor(doing, goto(agnt, movetype, dest)).
type_functor(doing, throw(agnt, inst, dest)).
type_functor(doing, put(agnt, inst, dest)).
type_functor(event, moved(inst, dest1, verb, dest2)).



type_functor(event, carrying(agnt, list_of(inst))).
type_functor(event, destroyed(inst)).
type_functor(event, did(doing)).
type_functor(event, sense_each(agnt, sense, list_of(event))).
type_functor(event, exits_are(agnt, dest, list_of(exit))).
type_functor(event, notice_children(agnt, sense, dest, domrel, list_of(inst))).
type_functor(event, failed(doing, msg)).
type_functor(event, transformed(inst, inst2)).



type_functor(nv, adjs(list_of(text))).
type_functor(nv, can_be(actverb, tf)).
type_functor(nv, can_do(actverb, tf)).
type_functor(nv, cant_go(inst, dir, text)).
type_functor(nv, class_desc(list_of(text))).
type_functor(nv, co(list_of(nv))).
type_functor(nv, desc(sv(text))).
type_functor(nv, door_to(inst)).
type_functor(nv, effect(verb_targeted, script)).
type_functor(nv, fragile(type)).
type_functor(nv, has_rel(domain, domrel, tf)).
type_functor(nv, has_sense(sense)).
type_functor(nv, inherit(type, tf)).
type_functor(nv, inherited(type)).
type_functor(nv, inheriting(type)).
type_functor(nv, inst(sv(term))).
type_functor(nv, isnt(type)).
type_functor(nv, name(sv(text))).
type_functor(nv, nominals(list_of(text))).
type_functor(nv, nouns(list_of(text))).
type_functor(nv, oper(doing, preconds, postconds)).
type_functor(nv, state(tfstate, tf)).



istate([
  structure_label(istate),

  props('floyd~1', [name('Floyd the robot'), inherit(autonomous,t), % can_do(eat, f), 
   inherit(robot,t)]),
  props('player~1', [name($self),inherit(console,t), inherit(humanoid,t)]),

  memories('player~1',
   [ structure_label(mem('player~1')),
    timestamp(0,Now),
    model([]),
    goals([]),
    todo([look('player~1')]),
    inst('player~1'),
    name('player~1')
  ]),

  % props(telnet, [inherit(telnet,t),isnt(console),inherit('player~1')]),
	
	
	
  h(Spatial, in, 'floyd~1', pantry),
	
	
	h(Spatial, in, 'player~1', kitchen),
	h(Spatial, worn_by, 'watch~1', 'player~1'),
	h(Spatial, held_by, 'bag~1', 'player~1'),
	
  h(Spatial, in, 'coins~1', 'bag~1'),
  h(Spatial, held_by, 'wrench~1', 'floyd~1'),
 props('coins~1', [inherit(coins, t)]), 

 % Relationships

 h(Spatial, exit(south), pantry, kitchen), % pantry exits south to kitchen
 h(Spatial, exit(north), kitchen, pantry),
 h(Spatial, exit(down), pantry, basement),
 h(Spatial, exit(up), basement, pantry),
 h(Spatial, exit(south), kitchen, garden),
 h(Spatial, exit(north), garden, kitchen),
 h(Spatial, exit(east), kitchen, dining_room),
 h(Spatial, exit(west), dining_room, kitchen),
 h(Spatial, exit(north), dining_room, living_room),
 h(Spatial, exit(east), living_room, dining_room),
 h(Spatial, exit(south), living_room, kitchen),
 h(Spatial, exit(west), kitchen, living_room),

 h(Spatial, in, a(shelf), pantry), % shelf is in pantry
 h(Spatial, in, a(rock), garden),
 h(Spatial, in, a(fountain), garden),
 h(Spatial, in, a(mushroom), garden),
 h(Spatial, in, a(shovel), basement), % FYI shovel has no props (this is a lttle test to see what happens)
 h(Spatial, in, a(videocamera), living_room),
 h(Spatial, in, screendoor, kitchen),
 h(Spatial, in, screendoor, garden),
 h(Spatial, in, brklamp, garden),


 props(basement, [
 inherit(place,t),
 desc('This is a very dark basement.'),
 TooDark
 ]),
 props(dining_room, [inherit(place,t)]),
 props(garden, [
 inherit(place,t),
 % goto(Agent, Prep, Dir, dir, result) provides special handling for going in a direction.
 cant_go(Agent, up, 'You lack the ability to fly.'), 
 oper( /*garden, */ goto(Agent, _, loc(Agent, south, _Prep, _Object)), 
   % precond(Test, FailureMessage)
   precond(getprop(screendoor, state(opened, t)), ['you must open the door first']),
   % body(clause)
   body(inherited)
 ),
 % cant_go provides last-ditch special handling for Go.
 cant_go(Agent, _Dir, 'The fence surrounding the garden is too tall and solid to pass.')
 ]),
 props(kitchen, [inherit(place,t)]),
  h(Spatial, reverse(on), a(table), a(table_leg)),
  h(Spatial, on, a(box), a(table)),
  h(Spatial, in, a(bowl), a(box)),
  h(Spatial, in, a(flour), a(bowl)),
  h(Spatial, in, a(table), kitchen), % a table is in kitchen
  h(Spatial, on, a(lamp), a(table)), % a lamp is on the table

  h(Spatial, in, a(sink), kitchen),
  h(Spatial, in, a(plate), a(sink)),
  h(Spatial, in, a(cabinate), kitchen), 
  h(Spatial, in, a(cup), a(cabinate)),
  

 props(living_room, [inherit(place,t)]),

 props(pantry, [
 inherit(place,t),
 nouns(closet),
 nominals(kitchen),
 desc('You\'re in a dark pantry.'),
 TooDark
 ]),

 % Things
 props('bag~1', [inherit(bag,t)]),

 props(brklamp, [
  inherit(broken,t), 
  name('possibly broken lamp'),
  effect(switch(on), print_(_Agent,"Switch is flipped")),
  effect(hit, ['print_'("Hit brklamp"), setprop($self, inherit(broken,t))]),
  inherit(lamp,t)
 ]),

 props(screendoor, [
  % see DM4
  door_to(kitchen),
  door_to(garden),
  state(opened, f),
  inherit(door,t)
 ])

]) :-  clock_time(Now), Agent = $agent, 
 sensory_model_problem_solution(_Sense, Spatial, TooDark, _EmittingLight).


%:- op(0, xfx, props).

:- multifile(extra_decl/2).
:- dynamic(extra_decl/2).
extra_decl(T,P):-
 Agent = $agent, 
 sensory_model_problem_solution(Sense, Spatial, TooDark, EmittingLight),
 member(type_props(T,P), 
 [
   type_props(broken, [
    name('definately broken'),
    effect(switch(on), true),
    effect(switch(off), true),
    can_be(switch, t),
    adjs([dented]), 
    adjs($class)
   ]),
  type_props(mushroom, [
  % Sense DM4
  name('speckled mushroom'),
  % singular,
  inherit(food,t),
  nouns([mushroom, fungus, toadstool]),
  adjs([speckled]),
  % initial(description used until initial state changes)
  initial('A speckled mushroom grows out of the sodden earth, on a long stalk.'),
  % description(examination description)
  desc('The mushroom is capped with blotches, and you aren\'t at all sure it\'s not a toadstool.'),
  can_be(eat, t),
  % before(VERB, CODE) -- Call CODE before default code for VERB.
  %      If CODE succeeds, don't call VERB.
  before(eat, (random(100) =< 30, die('It was poisoned!'); 'yuck!')),
  after(take,
    (initial, 'You pick the mushroom, neatly cleaving its thin stalk.'))
  ]),

  type_props(door, [
   can_be(move, f),
   can_be(open, t),
   can_be(close, t),
   state(opened, t),
   nouns(door),
   inherit(corporial,t)
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
    inherit(corporial,f),
    class_desc(['direct inheriters are completely noncorporial'])]), 

  type_props(only_conceptual, [ 
             adjs($class), 
    inherit(noncorporial, t), 
    inherit(thinkable, t), 
    class_desc(['kind is only conceptual'])]), 


   type_props(partly_noncorporial, [
    inherit(corporial,t),
   adjs($class), 
    inherit(noncorporial,t),
    class_desc(['kind is both partly corporial and non-corporial'])]),

   type_props(corporial, [
    can_be(touch, t),
    can_be(examine, t), 
    inherit(thinkable,t),
    state(cleanliness,clean),
    adjs($class), 
    class_desc(['kind is corporial'])]),

        type_props(object, [
         can_be(examine, t), 
         adjs(physical),
         can_be(move, t), 
         inherit(corporial, t), 
         inherit(thinkable,t),
         class_desc(['kind is an Movable Object'])]), 

        type_props(immobile, [
         adjs($class), 
         can_be(move, f), 
         class_desc(['kind is an Immobile Object'])]), 


   type_props(furnature, [
    can_be(examine, t), 
    inherit(immobile, t),
    inherit(corporial, t), 
    inherit(surface, t), 
    inherit(thinkable,t),
    adjs(physical),
    class_desc(['kind is furnature'])]),

  % People
  type_props(character, [
   has_rel(Spatial, worn_by, t), 
   has_rel(Spatial, held_by, t), 
   % overridable defaults
   mass(50), volume(50), % liters  (water is 1 kilogram per liter)
   can_do(eat, t),
   can_do(examine, t),
   can_do(touch, t),
   has_sense(Sense),
   inherit(perceptq,t),
   inherit(memorize,t),
   inherit(autoscan,t),
   inherit(partly_noncorporial,t)
  ]),

   type_props(natural_force, [
    can_do(eat, f),

    can_do(examine, t),
    can_be(touch, f),
    has_rel(Spatial, held_by, f), 
    has_rel(Spatial, worn_by, f), 
    has_sense(Sense),
    inherit(no_perceptq, t), 
    inherit(noncorporial,t),
    inherit(character,t)
   ]),

   type_props(humanoid, [
   can_do(eat, t),
    volume(50), % liters  (water is 1 kilogram per liter)
    mass(50), % kilograms
    inherit(character,t),
    inherit(memorize,t),
    inherit(player,t),
 
    % players use power but cant be powered down
    can_be(switch(off), f), state(powered, t)
   ]),

  type_props(autonomous, [inherit(autoscan,t)]),

  type_props(robot, [
  can_do(eat, f),
  inherit(autonomous,t),
  EmittingLight,
  volume(50), mass(200), % density(4) % kilograms per liter
  nouns([robot]), 
  adjs([metallic]), 
  desc('Your classic robot: metallic with glowing red eyes, enthusiastic but not very clever.'),
  can_be(switch, t),
  inherit(memorize,t),
  nouns($class), 
  inherit(shiny,t),
  inherit(character,t),
  state(powered, t),
  % TODO: 'floyd~1' should `look(Agent, Spatial)` when turned back on.
   effect(switch(on), setprop($self, state(powered, t))),
   effect(switch(off), setprop($self, state(powered, f)))
  ]),

  % Places
  type_props(place, [
     volume_capacity(10000),
     has_rel(Spatial, in, t), 
     can_be(move, f), 
     has_rel(Spatial, exit(_), t),
     inherit(container,t)
  ]),

  type_props(container, [
    has_rel(Spatial, in, t),     
    oper( put(Agent, Spatial, Thing, in, $self), 
    % precond(Test, FailureMessage)
    precond(( ~(getprop(Thing, inherit(liquid,t)))), ['liquids would spill out']),
    % body(clause)
    body(move(Agent, Spatial, Thing, in, $self)))
      % inherit(flask, f), 
    % adjs(flask, f)
   ]),

 type_props(console, [adjs(physical), nominals([console]), nouns([player])]), 
 type_props(telnet, [adjs([remote]), nouns([player])]), 
  type_props(bag, [
   inherit(container,t),
   inherit(object,t),
   volume_capacity(10),
   TooDark
  ]),

  type_props(cup, [inherit(flask,t)]),

  type_props(flask, [
    adjs(physical),
    oper( put(Agent, Spatial, Thing, in, $self), 
     % precond(Test, FailureMessage)
     precond(getprop(Thing, inherit(corporial,t)), ['non-physical would spill out']),
    % body(clause)
     body(move(Agent, Spatial, Thing, in, $self))),
    inherit(container, t), 
    inherit(object, t)
   ]),

  type_props(bowl, [
   inherit(flask, t), 
   volume_capacity(2),
   fragile(shards),
   state(cleanliness,dirty),
   name('porcelain bowl'),
   desc('This is a modest glass cooking bowl with a yellow flower motif glazed into the outside surface.')
  ]),
  type_props(plate, [
   inherit(surface, t),
   inherit(object,t),
   volume_capacity(2),
   fragile(shards),
   state(cleanliness,dirty),
   name('plate')
  ]),
  type_props(box, [
   inherit(container,t),
   inherit(object,t),
   volume_capacity(11), 
   fragile(splinters),
   can_be(open, t), 
   state(opened, f),
   TooDark
  ]),
  type_props(sink, [
   state(cleanliness,dirty),
   inherit(flask, t),   
   inherit(furnature,t),   
   volume_capacity(5)  
  ]),
  type_props(cabinate, [
   inherit(container,t),
   inherit(furnature,t),
   volume_capacity(10)
  ]),
  type_props(fountain, [   
   volume_capacity(150),
   inherit(place,t),
   inherit(flask, t), 
%   state(locked, f), 
   can_be(lock, f)
  ]),

 type_props(measurable, [adjs($class), has_rel(quantity, ammount, t)]), 

 
 % shiny things are corporial
 type_props(shiny, [adjs($class), inherit(object, t), inherit(corporial, t)]), 

 type_props(coins, [inherit(shiny,t),inherit(measurable,t)]),
  type_props(food,[can_be(eat, t),inherit(object,t),inherit(measurable,t)]),
  type_props(flour,[inherit(food,t),inherit(measurable,t)]),
 type_props(lamp, [
 name('shiny brass lamp'),
 nouns(light),
 nominals(brass),
 inherit(shiny,t),
 can_be(switch, t),
 state(powered, t),
 inherit(object,t),
 EmittingLight,
 effect(switch(on), setprop($self, EmittingLight)),
 effect(switch(off), delprop($self, EmittingLight)),
 fragile(inherit(broken_lamp,t))
 ]),
 type_props(broken_lamp, [
 name('dented brass lamp'),
 % TODO: prevent user from referring to 'broken_lamp'
 nouns(light),
 nominals(brass),
 adjs(dented),
 can_be(switch, t),
 effect(switch(on), true),
 effect(switch(off), true) % calls true(S0, S1) !
 ]),

   type_props(surface, [has_rel(Spatial, on, t),adjs(physical), state(cleanliness,clean)]), 

   type_props(shelf, [inherit(surface, t),adjs(physical),inherit(furnature, t)]), 

   type_props(table, [inherit(surface, t),adjs(physical),has_rel(Spatial, reverse(on), t)]), 

   type_props(wrench, [inherit(shiny,t)]),
   type_props(videocamera, [
   inherit(memorize,t),
   inherit(perceptq,t), 
   can_be(switch, t),
    effect(switch(on), setprop($self, state(powered, t))),
    effect(switch(off), setprop($self, state(powered, f))),
   state(powered, t),
   has_sense(Sense),
   fragile(broken_videocam)
   ]),
   type_props(broken_videocam, [can_be(switch, f),state(powered, f), inherit(videocamera,t)])
 ]).

