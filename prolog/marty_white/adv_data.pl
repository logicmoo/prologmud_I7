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
%   state(Spatial, open, t) - container is state(Spatial, open, t) (must be state(Spatial, open, t) to be used. there is no "closed").
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
%   when_open - description when state(Spatial, open, t)
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


:- op(900, xfx, props).
:- op(900, fy, '~').

istate([
       structure_label(initial_state),
       
       props('floyd~1', [inherit(instance), name('Floyd the robot'), inherit(autonomous), 
	% can_do(Spatial, eat, f), 
              inherit(robot), inherit(instance)]),
       props('player~1', [inherit(instance), name($self),inherit(console), inherit(humanoid)]),
	
       % props(telnet, [inherit(instance), inherit(telnet),isnt(console),inherit('player~1')]),
	
	
       h(Spatial, in, 'floyd~1', pantry),
	
	
	h(Spatial, in, 'player~1', kitchen),
	h(Spatial, worn_by, 'watch~1', 'player~1'),
	h(Spatial, held_by, 'bag~1', 'player~1'),
	
       h(Spatial, in, 'coins~1', 'bag~1'),
       h(Spatial, held_by, 'wrench~1', 'floyd~1'),

  props('coins~1',[inherit(coins),inherit(instance)]),
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
  h(Spatial, on, a(lamp), a(table)), % a lamp is on a table
  h(Spatial, in, a(rock), garden),
  h(Spatial, in, a(mushroom), garden),
  h(Spatial, under, a(table), a(table_leg)),
  h(Spatial, on, a(box), a(table)),
  h(Spatial, in, a(bowl), a(box)),
  h(Spatial, in, a(flour), a(bowl)),
  h(Spatial, in, a(shovel), basement), % FYI shovel has not props (this is a lttle test to see what happens)
  h(Spatial, in, a(videocamera), living_room),
  h(Spatial, in, screendoor, kitchen),
  h(Spatial, in, screendoor, garden),

       class_props(unthinkable, [
          can_be(Spatial, examine(_), f),
          class_desc(['It is normally unthinkable'])]),

       class_props(thinkable, [
          can_be(Spatial, examine(_), t),
          class_desc(['It is normally thinkable'])]),

       class_props(only_conceptual, [   
          can_be(Spatial, examine(Spatial), f),
          inherit(thinkable),
          class_desc(['It is completely conceptual'])]),

       class_props(noncorporial, [
          can_be(Spatial, examine(Spatial), f),
          can_be(Spatial, touch, f),
          inherit(thinkable),
          desc(['It is completely non-corporial'])]),

       class_props(partly_noncorporial, [
          inherit(corporial),
          inherit(noncorporial),
          class_desc(['It is both partly corporial and non-corporial'])]),

       class_props(corporial, [
          can_be(Spatial, touch, t),
          can_be(Spatial, examine(Spatial), t),
          inherit(thinkable),
          class_desc(['It is corporial'])]),

  % People
   class_props(character, [
       has_rel(Spatial, held_by),
       has_rel(Spatial, worn_by),
       % overridable defaults
       mass(50), volume(50), % liters     (water is 1 kilogram per liter)
       can_do(Spatial, eat, t),
       can_do(Spatial, examine, t),
       can_do(Spatial, touch, t),
       has_sense(Sense),
       inherit(perceptq),
       inherit(memorize),
       iherit(partly_noncorporial)
   ]),

      class_props(natural_force, [
          ~has_rel(Spatial, held_by),
          ~has_rel(Spatial, worn_by),
          can_do(Spatial, eat, f),

          can_do(Spatial, examine, t),
          can_be(Spatial, touch, f),
          has_sense(Sense),
          iherit(character)
      ]),

       class_props(humanoid, [
         can_do(Spatial, eat, t),
           volume(50), % liters     (water is 1 kilogram per liter)
           mass(50), % kilograms
            inherit(character),
            inherit(memorize),
            inherit(player),
            % players use power but cant be powered down
            can_be(Spatial, switch, f), state(Spatial, powered, t)
      ]),

  class_props(robot, [
    can_do(Spatial, eat, f),
    inherit(autonomous),
    EmittingLight,
    volume(50), mass(200), % density(4) % kilograms per liter
    nouns(robot),
    adjs(metallic),
    desc('Your classic robot: metallic with glowing red eyes, enthusiastic but not very clever.'),
    can_be(Spatial, switch, t),
    inherit(memorize),
    inherit(shiny),
    inherit(character),
    state(Spatial, powered, t),
    % TODO: 'floyd~1' should `look(Spatial)` when turned back on.
        effect(switch(Spatial, on), setprop($self, state(Spatial, powered, t))),
        effect(switch(Spatial, off), setprop($self, state(Spatial, powered, f)))
  ]),

  % Places
  class_props(place, [can_be(Spatial, move, f), inherit(container), volume_capacity(10000), has_rel(exit(_), t)]),

  class_props(container, [

         oper(put(Spatial, Thing, in, $self),
            % precond(Test, FailureMessage)
            precond(~getprop(Thing, inherit(liquid)), ['liquids would spill out']),
           % body(clause)
            body(move(Spatial, Thing, in, $self))),
         has_rel(Spatial, in)
       ]),

  class_props(flask, [

           oper(put(Spatial, Thing, in, $self),
              % precond(Test, FailureMessage)
              precond(getprop(Thing, inherit(corporial)), ['non corporial would spill out']),
             % body(clause)
              body(move(Spatial, Thing, in, $self))),

           inherit(container)
       ]),

  props(basement, [
    inherit(place),
    desc('This is a very dark basement.'),
    TooDark
  ]),
  props(dining_room, [inherit(place)]),
  props(garden, [
    inherit(place),
    % goto(Spatial, dir, result) provides special handling for going in a direction.
    goto(Spatial, up, 'You lack the ability to fly.'),
    effect(goto(Spatial, _, north), getprop(screendoor, state(Spatial, open, t))),
    oper(/*garden, */ goto(Spatial, _, north),
         % precond(Test, FailureMessage)
         precond(getprop(screendoor, state(Spatial, open, t)), ['you must open the door first']),
         % body(clause)
         body(inherited)
    ),
    % cant_go provides last-ditch special handling for Go.
    cant_goto(Spatial, 'The fence surrounding the garden is too tall and solid to pass.')
  ]),
  props(kitchen, [inherit(place)]),
  props(living_room, [inherit(place)]),
  props(pantry, [
    inherit(place),
    nouns(closet),
    nominals(kitchen),
    desc('You\'re in a dark pantry.'),
    TooDark
  ]),

  % Things
  
  class_props(bag, [
    inherit(container),
    volume_capacity(10),
    TooDark
  ]),
  class_props(bowl, [
    inherit(container),
    volume_capacity(2),
    fragile(shards),
    inherit(flask),
    name('porcelain bowl'),
    desc('This is a modest glass cooking bowl with a yellow flower motif glazed into the outside surface.')
  ]),
  class_props(box, [
    inherit(container),
    volume_capacity(15),
    fragile(splinters),
    %can_be(Spatial, open, t),
    state(Spatial, open, f),
    %can_be(Spatial, lock, t),
    state(Spatial, locked, t),
    TooDark
  ]),

  class_props(measurable,[has_rel(quantity,ammount,t)]),
  
  % shiny things are corporial
  class_props(shiny, [adjs(shiny), inherit(corporial)]),

  class_props(coins, [inherit(shiny),inherit(measurable)]),
  class_props(flour,[can_be(Spatial, eat, t),inherit(measurable)]),
  class_props(lamp, [
    name('shiny brass lamp'),
    nouns(light),
    nominals(brass),
    inherit(shiny),
    can_be(Spatial, switch, t),
    state(Spatial, powered, t),
    EmittingLight,
    effect(switch(Spatial, on), setprop($self, EmittingLight)),
    effect(switch(Spatial, off), delprop($self, EmittingLight)),
    fragile(broken_lamp)
  ]),
  class_props(broken_lamp, [
    name('dented brass lamp'),
    % TODO: prevent user from referring to 'broken_lamp'
    nouns(light),
    nominals(brass),
    adjs(dented),
    can_be(Spatial, switch, t),
    effect(switch(Spatial, on), true),
    effect(switch(Spatial, off), true) % calls true(S0, S1) !
  ]),
       props(iLamp, [
         inherit(broken), 
         effect(switch(Spatial, on), print_("Switch is flipped")),
         effect(hit, ['print_'("Hit iLamp"), setprop($self, inherit(broken))]),
         inherit(lamp)
       ]),
       class_props(broken, [
          effect(switch(Spatial, on), true),
          effect(switch(Spatial, off), true),
          can_be(Spatial, switch, t),
          adjs(broken)
       ]),
  class_props(mushroom, [
    % Sense DM4
    name('speckled mushroom'),
    % singular,
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
  props(screendoor, [
    can_be(Spatial, move, f),
    % see DM4
    door_to(garden),
    %can_be(Spatial, open, t)
    state(Spatial, open, f)
  ]),
  props(shelf, [has_rel(Spatial, on), can_be(Spatial, move, f)]),
  props(table, [has_rel(Spatial, on), has_rel(Spatial, under)]),
  class_props(wrench, [inherit(shiny)]),
  class_props(videocamera, [
    inherit(memorize),
    inherit(perceptq),
    inherit('instance'),
    can_be(Spatial, switch, t),
        effect(switch(Spatial, on), setprop($self, state(Spatial, powered, t))),
        effect(switch(Spatial, off), setprop($self, state(Spatial, powered, f))),
    state(Spatial, powered, t),
    has_sense(Sense),
    fragile(broken_videocam)
  ]),
  class_props(broken_videocam, [can_be(Spatial, switch, f),state(Spatial, powered, f), inherit(videocamera)])
         
]) :-
  sensory_model_problem_solution(Sense, Spatial, TooDark, EmittingLight).


:- op(0, xfx, props).

