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


% Entire state of simulation & agents is held in one list, so it can be easy
% to roll back.  The state of the simulation consists of:
%   object properties
%   object relations
%   percept queues for agents
%   memories for agents (actually logically distinct from the simulation)
% Note that the simulation does not maintain any history.
% TODO: change state into a term:
%   ss(Objects, Relationships, PerceptQueues, AgentMinds)
% TODO:
%   store initial state as clauses which are collected up and put into a list,
%     like the operators are, to provide proper prolog variable management.

:- op(900, xfx, props).
:- op(900, fy, '~').

istate([
  props(floyd, [inherit(instance), name('Floyd the robot'), inherit(autonomous), 
    % can_do(Spatial, eat, f), 
    inherit(floyd_ish), inherit(instance)]),
  props(player1, [inherit(instance), name('Player#1'),inherit(console), inherit(console_player)]),

  h(Spatial, in, floyd, pantry),
      
       h(Spatial, in, player1, kitchen),
       h(Spatial, worn_by, watch, player1),
       h(Spatial, held_by, bag, player1),


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

  h(Spatial, in, shelf, pantry), % shelf is in pantry
  h(Spatial, on, lamp, table),
  h(Spatial, held_by, wrench, floyd),
  h(Spatial, in, rock, garden),
  h(Spatial, in, mushroom, garden),
  h(Spatial, in, coins, bag),
  h(Spatial, in, table, kitchen),
  h(Spatial, under, table, table_leg),
  h(Spatial, on, box, table),
  h(Spatial, in, bowl, box),
  h(Spatial, in, flour, bowl),
  h(Spatial, in, shovel, basement), % FYI shovel has not props
  h(Spatial, in, videocamera, living_room),
  h(Spatial, in, screendoor, kitchen),
  h(Spatial, in, screendoor, garden),

       props(unthinkable , [
          can_be(Spatial, examine(_), f),
          class_desc(['It is normally unthinkable'])]),

       props(thinkable , [
          can_be(Spatial, examine(_), t),
          class_desc(['It is normally thinkable'])]),

       props(only_conceptual , [   
          can_be(Spatial, examine(Spatial), f),
          inherit(thinkable),
          class_desc(['It is completely conceptual'])]),

       props(noncorporial , [
          can_be(Spatial, examine(Spatial), f),
          can_be(Spatial, touch, f),
          inherit(thinkable),
          desc(['It is completely non-corporial'])]),

       props(partly_noncorporial, [
          inherit(corporial),
          inherit(noncorporial),
          class_desc(['It is both partly corporial and non-corporial'])]),

       props(corporial , [
          can_be(Spatial, touch, t),
          can_be(Spatial, examine(Spatial), t),
          inherit(thinkable),
          class_desc(['It is corporial'])]),

  % People
   props(character , [
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

      props(natural_force , [
          ~has_rel(Spatial, held_by),
          ~has_rel(Spatial, worn_by),
          can_do(Spatial, eat, f),

          can_do(Spatial, examine, t),
          can_be(Spatial, touch, f),
          has_sense(Sense),
          iherit(character)
      ]),

       props(console_player, [
         can_do(Spatial, eat, t),
           volume(50), % liters     (water is 1 kilogram per liter)
           mass(50), % kilograms
            inherit(character),
            inherit(memorize),
            inherit(player),
            % players use power but cant be powered down
            can_be(Spatial, switch, f), state(Spatial, powered, t),
         inherit(console)
      ]),
       props(telnet_player, [
         can_do(Spatial, eat, t),
           volume(50), % liters     (water is 1 kilogram per liter)
           mass(50), % kilograms
            inherit(character),
            inherit(memorize),
            inherit(player),
            % players use power but cant be powered down
            can_be(Spatial, switch, f), state(Spatial, powered, t),
         inherit(telnet)
      ]),

  props(floyd_ish, [
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
    % TODO: floyd should `look(Spatial)` when turned back on.
        effect(switch(Spatial, on), setprop($self, state(Spatial, powered, t))),
        effect(switch(Spatial, off), setprop($self, state(Spatial, powered, f)))
  ]),

  % Places
  props(place , [can_be(Spatial, move, f), inherit(container), volume_capacity(10000), has_rel(exit(_), t)]),

  props(container, [

         oper(put(Spatial, Thing, in, $self),
            % precond(Test, FailureMessage)
            precond(~getprop(Thing, inherit(liquid)), ['liquids would spill out']),
           % body(clause)
            body(move(Spatial, Thing, in, $self))),
         has_rel(Spatial, in)
       ]),

  props(flask, [

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
  
  props(bag, [
    inherit(container),
    volume_capacity(10),
    TooDark
  ]),
  props(bowl, [
    inherit(container),
    volume_capacity(2),
    fragile(shards),
    inherit(flask),
    name('porcelain bowl'),
    desc('This is a modest glass cooking bowl with a yellow flower motif glazed into the outside surface.')
  ]),
  props(box, [
    inherit(container),
    volume_capacity(15),
    fragile(splinters),
    %can_be(Spatial, open, t),
    state(Spatial, open, f),
    %can_be(Spatial, lock, t),
    state(Spatial, locked, t),
    TooDark
  ]),

  props(shiny , [adjs(shiny), inherit(corporial)]),
  props(coins , [inherit(shiny)]),
  flour props [can_be(Spatial, eat, t)],
  props(lamp, [
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
  props(broken_lamp , [
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
         inherit(lamp), inherit(broken),
         effect(switch(Spatial, on), print_("Switch is flipped")),
         effect(hit, ['print_'("Hit iLamp"), setprop($self, inherit(broken))])
       ]),
       props(broken, [
          effect(switch(Spatial, on), true),
          effect(switch(Spatial, off), true),
          can_be(Spatial, switch, t),
          adjs(broken)
       ]),
  mushroom props [
    % Sense DM4
    name('speckled mushroom'),
    singular,
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
  ],
  props(screendoor, [
    can_be(Spatial, move, f),
    % see DM4
    door_to(garden),
    %can_be(Spatial, open, t)
    state(Spatial, open, f)
  ]),
  props(shelf , [has_rel(Spatial, on), can_be(Spatial, move, f)]),
  props(table , [has_rel(Spatial, on), has_rel(Spatial, under)]),
  props(wrench , [inherit(shiny)]),
  props(videocamera , [
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
  props(broken_videocam , [can_be(Spatial, switch, f),state(Spatial, powered, f), inherit(videocamera)]),
         
  end_of_list
]) :-
  sensory_model_problem_solution(Sense, Spatial, TooDark, EmittingLight).


:- op(0, xfx, props).

