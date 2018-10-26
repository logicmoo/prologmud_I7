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

% Some Inform properties:
%   light - rooms that have light in them
%   can_be(Spatial, eat, t) - can be eaten
%   static - can't be taken or moved
%   scenery - assumed to be in the room description (implies static)
%   concealed - obscured, not listed, not part of 'all', but there
%   found_in - lists places where scenery objects are seen
%   absent - hides object entirely
%   clothing - can be worn
%   worn - is being worn
%   container
%   state(Spatial, opened, t) - container is open (must be opened) to be used. there is no "closed").
%   can_be(Spatial, open, t) - can be opened and closed
%   capacity(N) - number of objects a container or supporter can hold
%   state(Spatial, locked, t) - cannot be opened
%   can_be(Spatial, lock, t), with_key
%   enterable
%   supporter
%   article - specifies indefinite article ('a', 'le')
%   cant_go
%   daemon - called each turn, if it is enabled for this object
%   description
%   inside_description
%   invent - code for inventory listing of that object
%   list_together - way to handle "5 fish"
%   plural - pluralized-name if different from singular
%   when_closed - description when closed
%   when_open - description when state(Spatial, opened, t)
%   when_on, when_off - like when_closed, etc.
% Some TADS properties:
%   thedesc
%   pluraldesc
%   is_indistinguishable
%   is_visible(vantage)
%   is_reachable(actor)
%   valid(verb) - is object seeable, reachable, etc.
%   verification(verb) - is verb logical for this object
% Parser disambiguation:
%   eliminate objs not see, reachable, etc.
%   check preconditions for acting on a candidate object


%:- op(900, xfx, props).
:- op(900, fy, '~').

istate([
       structure_label(istate),

       props('floyd~1', [name('Floyd the robot'), inherit(autonomous,t),  % can_do(Spatial, eat, f), 
           inherit(robot,t)]),
       props('player~1', [name($self),inherit(console,t), inherit(humanoid,t)]),

       % props(telnet, [inherit(telnet,t),isnt(console),inherit('player~1')]),
	
	
       h(Spatial, in, 'floyd~1', pantry),
	
	
	h(Spatial, in, 'player~1', kitchen),
	h(Spatial, worn_by, 'watch~1', 'player~1'),
	h(Spatial, held_by, 'bag~1', 'player~1'),
	
        h(Spatial, in, 'coins~1', 'bag~1'),
        h(Spatial, held_by, 'wrench~1', 'floyd~1'),

  props('coins~1',[inherit(coins,t)]),
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
       h(Spatial, in, a(table), kitchen), % a table is in kitchen
       h(Spatial, on, a(lamp), the(table)), % a lamp is on the table
       h(Spatial, in, a(rock), garden),
       h(Spatial, in, a(fountain), garden),
  h(Spatial, in, a(mushroom), garden),
  h(Spatial, reverse(on), a(table), a(table_leg)),
  h(Spatial, on, a(box), a(table)),
  h(Spatial, in, a(bowl), a(box)),
  h(Spatial, in, a(flour), a(bowl)),
  h(Spatial, in, a(shovel), basement), % FYI shovel has no props (this is a lttle test to see what happens)
  h(Spatial, in, a(videocamera), living_room),
  h(Spatial, in, screendoor, kitchen),
  h(Spatial, in, screendoor, garden),
  h(Spatial, in, brklamp, garden),

       type_props(unthinkable, [
          can_be(Spatial, examine(_), f),
          class_desc(['It is normally unthinkable'])]),

       type_props(thinkable, [
          can_be(Spatial, examine(_), t),
          class_desc(['It is normally thinkable'])]),

       type_props(only_conceptual, [   
          can_be(Spatial, examine(Spatial), f),
          inherit(thinkable,t),
          class_desc(['It is completely conceptual'])]),

       type_props(noncorporial, [
          can_be(Spatial, examine(Spatial), f),
          can_be(Spatial, touch, f),
          inherit(thinkable,t),
          desc(['It is completely non-corporial'])]),

       type_props(partly_noncorporial, [
          inherit(corporial,t),
          inherit(noncorporial,t),
          class_desc(['It is both partly corporial and non-corporial'])]),

       type_props(corporial, [
          can_be(Spatial, touch, t),
          can_be(Spatial, examine(Spatial), t),
          inherit(thinkable,t),
          class_desc(['It is corporial'])]),

       type_props(object, [
          can_be(Spatial, touch, t),
          can_be(Spatial, examine(Spatial), t),
          inherit(thinkable,t),
          class_desc(['It is an Object'])]),

       type_props(object, [
          can_be(Spatial, touch, t),
          can_be(Spatial, examine(Spatial), t),
          inherit(thinkable,t),
          class_desc(['It is an Object'])]),

  % People
   type_props(character, [
       has_rel(Spatial, held_by),
       has_rel(Spatial, worn_by),
       % overridable defaults
       mass(50), volume(50), % liters     (water is 1 kilogram per liter)
       can_do(Spatial, eat, t),
       can_do(Spatial, examine, t),
       can_do(Spatial, touch, t),
       has_sense(Sense),
       inherit(perceptq,t),
       inherit(memorize,t),
       iherit(partly_noncorporial)
   ]),

      type_props(natural_force, [
          ~has_rel(Spatial, held_by),
          ~has_rel(Spatial, worn_by),
          can_do(Spatial, eat, f),

          can_do(Spatial, examine, t),
          can_be(Spatial, touch, f),
          has_sense(Sense),
          iherit(character)
      ]),

       type_props(humanoid, [
         can_do(Spatial, eat, t),
           volume(50), % liters     (water is 1 kilogram per liter)
           mass(50), % kilograms
            inherit(character,t),
            inherit(memorize,t),
            inherit(player,t),
            % players use power but cant be powered down
            can_be(Spatial, switch(off), f), state(Spatial, powered, t)
      ]),

  type_props(robot, [
    can_do(Spatial, eat, f),
    inherit(autonomous,t),
    EmittingLight,
    volume(50), mass(200), % density(4) % kilograms per liter
    nouns(robot),
    adjs(metallic),
    desc('Your classic robot: metallic with glowing red eyes, enthusiastic but not very clever.'),
    can_be(Spatial, switch, t),
    inherit(memorize,t),
    inherit(shiny,t),
    inherit(character,t),
    state(Spatial, powered, t),
    % TODO: 'floyd~1' should `look(Spatial)` when turned back on.
        effect(switch(on), setprop($self, state(Spatial, powered, t))),
        effect(switch(off), setprop($self, state(Spatial, powered, f)))
  ]),

  % Places
  type_props(place, [can_be(Spatial, move, f), inherit(container,t), volume_capacity(10000), has_rel(exit(_), t)]),

  type_props(container, [

         oper(put(Spatial, Thing, in, $self),
            % precond(Test, FailureMessage)
            precond(~getprop(Thing, inherit(liquid,t)), ['liquids would spill out']),
           % body(clause)
            body(move(Spatial, Thing, in, $self))),
         has_rel(Spatial, in)
       ]),

  type_props(flask, [

           oper(put(Spatial, Thing, in, $self),
              % precond(Test, FailureMessage)
              precond(getprop(Thing, inherit(corporial,t)), ['non-physical would spill out']),
             % body(clause)
              body(move(Spatial, Thing, in, $self))),
           inherit(object,t),
           inherit(container,t)
       ]),

  props(basement, [
    inherit(place,t),
    desc('This is a very dark basement.'),
    TooDark
  ]),
  props(dining_room, [inherit(place,t)]),
  props(garden, [
    inherit(place,t),
    % goto(dir, result) provides special handling for going in a direction.
    goto(up, 'You lack the ability to fly.'),
    effect(goto(_, north), getprop(screendoor, state(Spatial, opened, t))),
    oper(/*garden, */ goto(_, north),
         % precond(Test, FailureMessage)
         precond(getprop(screendoor, state(Spatial, opened, t)), ['you must open the door first']),
         % body(clause)
         body(inherited)
    ),
    % cant_go provides last-ditch special handling for Go.
    cant_goto('The fence surrounding the garden is too tall and solid to pass.')
  ]),
  props(kitchen, [inherit(place,t)]),
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

  type_props(bag, [
    inherit(container,t),
    inherit(object,t),
    volume_capacity(10),
    TooDark
  ]),
  type_props(bowl, [
    inherit(container,t),
    inherit(object,t),
    volume_capacity(2),
    fragile(shards),
    inherit(flask,t),
    name('porcelain bowl'),
    desc('This is a modest glass cooking bowl with a yellow flower motif glazed into the outside surface.')
  ]),
  type_props(box, [
    inherit(container,t),
    inherit(object,t),
    volume_capacity(15),
    fragile(splinters),
    %can_be(Spatial, open, t),
    state(Spatial, opened, f),
    %can_be(Spatial, lock, t),
    state(Spatial, locked, t),
    TooDark
  ]),

  type_props(measurable,[has_rel(quantity,ammount,t)]),
  
  % shiny things are corporial
  type_props(shiny, [adjs(shiny),inherit(object,t), inherit(corporial,t)]),

  type_props(coins, [inherit(shiny,t),inherit(measurable,t)]),
       type_props(food,[can_be(Spatial, eat, t),inherit(object,t),inherit(measurable,t)]),
       type_props(flour,[inherit(food,t),inherit(measurable,t)]),
  type_props(lamp, [
    name('shiny brass lamp'),
    nouns(light),
    nominals(brass),
    inherit(shiny,t),
    can_be(Spatial, switch, t),
    state(Spatial, powered, t),
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
    can_be(Spatial, switch, t),
    effect(switch(on), true),
    effect(switch(off), true) % calls true(S0, S1) !
  ]),
       props(brklamp, [
         inherit(broken,t), 
         name('possibly broken lamp'),
         effect(switch(on), print_("Switch is flipped")),
         effect(hit, ['print_'("Hit brklamp"), setprop($self, inherit(broken,t))]),
         inherit(lamp,t)
       ]),

       type_props(broken, [
          name('definately broken'),
          effect(switch(on), true),
          effect(switch(off), true),
          can_be(Spatial, switch, t),
          adjs(dented),
          adjs(broken)
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
    can_be(Spatial, eat, t),
    % before(VERB, CODE) -- Call CODE before default code for VERB.
    %                      If CODE succeeds, don't call VERB.
    before(eat, (random(100) =< 30, die('It was poisoned!'); 'yuck!')),
    after(take,
          (initial, 'You pick the mushroom, neatly cleaving its thin stalk.'))
  ]),

    type_props(door, [
       can_be(Spatial, move, f),
       can_be(Spatial, open, t),
       can_be(Spatial, close, t),
       state(Spatial, opened, t),
       nouns(door),
       inherit(corporial,t)
    ]),

    props(screendoor, [
      % see DM4
      door_to(kitchen),
      door_to(garden),
      state(Spatial, opened, f),
      inherit(door,t)
    ]),

  type_props(shelf, [has_rel(Spatial, on), can_be(Spatial, move, f)]),
  type_props(table, [has_rel(Spatial, on), has_rel(Spatial, under)]),
  type_props(wrench, [inherit(shiny,t)]),
  type_props(videocamera, [
    inherit(memorize,t),
    inherit(perceptq,t),    
    can_be(Spatial, switch, t),
        effect(switch(on), setprop($self, state(Spatial, powered, t))),
        effect(switch(off), setprop($self, state(Spatial, powered, f))),
    state(Spatial, powered, t),
    has_sense(Sense),
    fragile(broken_videocam)
  ]),
  type_props(broken_videocam, [can_be(Spatial, switch, f),state(Spatial, powered, f), inherit(videocamera,t)])
         
]) :-
  sensory_model_problem_solution(Sense, Spatial, TooDark, EmittingLight).


%:- op(0, xfx, props).

