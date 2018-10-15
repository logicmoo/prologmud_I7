/* Space Race -- first one to bring their moonrocks to Saturn wins, by Paul Lee.
   Consult this file and issue the command:   start.  */

:- dynamic at/2, i_am_at/1, alive/1, confirm/1.   /* Needed by SWI-Prolog. */
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

/* This defines my current location. */
i_am_at(earth).

/* defines the security code to the mars_lab */
code(333).

/* defines whether or not the correct code was entered at the mars_lab */
confirm(false).

/* These facts describe how to travel from planet to planet and within each planet. */
path(earth, fly, moon).
path(moon, forward, cave).
path(cave, turn, moon_station).
path(moon_station, fly, mars) :- at(gas, cargohold).
path(moon_station, fly, mars) :- write('You need gas to travel to Mars!'), nl, !, fail.
path(moon_station, lightspeed, saturn) :- at(rockets, cargohold), at(moonrocks, cargohold).


/* on mars, stumble upon mars_lab and break in by guessing security code -- HIDDEN OBJECT component */
path(mars_station, fly_back, moon) :- at(gas, cargohold).
path(mars_station, fly_back, moon) :- write('You need gas to travel back to the Moon!'), nl, !, fail.
path(mars, forward, mars_lab).
path(mars_lab, enter, rocket_room) :- confirm(true).
path(rocket_room, exit, mars_lab).
path(mars_lab, turn, mars_station).
path(mars, turn, mars_station).
path(mars_station, fly, jupiter) :- at(gas, cargohold).
path(mars_station, fly, jupiter) :- write('You need gas to travel to Jupiter!'), nl, !, fail.
path(mars_station, lightspeed, saturn) :- at(rockets, cargohold), at(moonrocks, cargohold).

/* as on every planet, will need to get gas on jupiter */
path(jupiter_station, fly_back, mars) :- at(gas, cargohold).
path(jupiter_station, fly_back, mars) :- write('You need gas to travel back to Mars!'), nl, !, fail.
path(jupiter, turn, jupiter_station).
path(jupiter_station, fly, saturn) :- at(moonrocks, cargohold), at(gas, cargohold).
path(jupiter_station, lightspeed, saturn) :- at(rockets, cargohold), at(moonrocks, cargohold).
path(jupiter_station, fly, saturn) :- at(moonrocks, cargohold), at(rockets, cargohold).
path(jupiter_station, fly, saturn) :- at(moonrocks, cargohold), at(gas, cargohold).
path(jupiter_station, fly, saturn) :- at(moonrocks, cargohold), at(rockets, cargohold), at(gas, cargohold).
path(jupiter_station, fly, saturn) :- 
        write('Are you crazy?! You are not ready to finish the race yet!'), nl,
        !, fail.

/* These facts tell where the various objects in the game
   are located. */

at(gas, moon_station).
at(gas, mars_station).
at(gas, jupiter_station).
at(moonrocks, cave).
at(rockets, rocket_room).

/* These rules describe how to pick up an object. */

take(X) :-
        at(X, cargohold),
        write('You already have this in your cargohold!'),
        nl, !.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(at(X, cargohold)),
        write('Added to cargohold.'),
        nl, !.

take(_) :-
        write('That is not here.'),
        nl.


/* These rules describe how to put down an object. */

takeoff(X, Place) :-
        retract(at(X, cargohold)),
        assert(at(X, Place)),
        %% write(X), nl,
        %% write(Place), nl,
        write('over and out'),
        nl, !.

drop(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, cargohold)),
        assert(at(X, Place)),
        write('OK.'),
        nl, !.

drop(_) :-
        write('Not in the cargobay'),
        nl.

/* These rules define the six direction letters as calls to go/1. */

forward :- go(forward).

turn :- go(turn).

enter :- go(enter).

exit :- go(exit).

fly :- i_am_at(Place), go(fly),
        takeoff(gas, Place).

fly_back :- i_am_at(Place), go(fly_back),
        takeoff(gas, Place).

lightspeed :- at(rockets, cargohold), go(lightspeed), nl.

lightspeed :- write('You need new rockets to use lightspeed'), nl.

guess(X) :- i_am_at(mars_lab), code(X), assert(confirm(true)), retract(confirm(false)), nl,
        write('You guessed correctly! Type enter and exit to get in and out.'), nl, !.

guess(_) :- i_am_at(mars_lab), write('Guess again!'), nl, !.

guess(_) :- write('There is nothing to guess!'), nl.



/* This rule tells how to move in a given direction. */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        look, !.

go(_) :-
        write('That is impossible due to the constraints of space travel.').

/* This rule tells how to look about you. */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl.

/* tells the current inventory of the playear. */
i :-
        write('Inventory: '), nl,
        at(X, cargohold),
        write(X), nl,
        fail.

/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
        at(X, Place),
        write('Theres '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).

/* Under UNIX, the   halt.  command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final  halt.  */

finish :-
        nl,
        write('The game is over. Please enter the   halt.   command.'),
        nl, !.


/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.                         -- to start the game.'), nl,
        write('fly.  fly_back.  lightspeed.   -- to takeoff and fly'), nl,
        write('forward.    turn.              -- to go in that direction'), nl,
        write('take(Object).                  -- to pick up an object.'), nl,
        write('drop(Object).                  -- to put down an object.'), nl,
        write('i.                             -- check inventory'), nl,
        write('look.                          -- to look around you again.'), nl,
        write('guess.                         -- use to enter security code at mars_lab'), nl,
        write('lightspeed.                    -- use to fly directly to saturn. requires rockets'), nl,
        write('instructions.                  -- to see this message again.'), nl,
        write('halt.                          -- to end the game and quit.'), nl,
        nl.

/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.

/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(earth) :-
        write('     |     | | '), nl,
        write('    / \\    | | Welcome to the Space Race!'), nl,
        write('   |--o|===|-| Be the first to reach Saturn with'), nl,
        write('   |---|   | | rocks from the Moon and you win!'), nl,
        write('  /     \\  | | '), nl,
        write(' | SPACE | | | Make sure you re-fuel on every planet.'), nl,
        write(' |  RACE |=| | And, be sure to look around on Mars'), nl,
        write(' |_______| |_| for something that could turn'), nl,
        write('  |@| |@|  | | out to be very useful.'), nl,
        write('___________|_| '), nl.

describe(moon) :-
        write('Welcome to the Moon!'), nl,
        write('         ___---___'), nl,
        write('      .--         --.'), nl,
        write('    ./   ()      .-. \\.'), nl,
        write('   /   o    .   (   )  \\'), nl,
        write('  / .            '-'    \\'), nl,
        write(' | ()    .  O         .  |'), nl,
        write('|                         |'), nl,
        write('|    o           ()       |'), nl,
        write('|       .--.          O   |'), nl,
        write(' | .   |    |            |'), nl,
        write('  \\    `.__.\\''    o   .  /'), nl,
        write('   \\                  /'), nl,
        write('    `\\  o    ()      /\''), nl,
        write('      `--___   ___--\''), nl,
        write('            ---'), nl.

describe(cave) :-
        write('You are in a dark and mysterious cave. Look!'), nl,
        write(''), nl,
        write('There are moonrocks here!'), nl.

describe(moon_station) :-
        write('Welcome to the Moon Station: The one stop shop to get you to Mars'), nl,
        write('use take and fly to refuel and takeoff'), nl,
        write(''), nl,
        write('                   .-.'), nl,
        write('    .-""`""-.    |(@ @)'), nl,
        write(' _/`oOoOoOoOo`\\_ \\ \\-/'), nl,
        write('''.-=-=-=-=-=-=-.''  / \\'), nl,
        write('  `-=.=-.-=.=-''    \\ /\\'), nl,
        write('     ^  ^  ^       _H_ \\'), nl.

describe(mars_station) :-
        write('Welcome to the Mars Station: The one stop shop to get you to Jupiter'), nl,
        write('use take and fly to refuel and takeoff'), nl,
        write(''), nl,
        write('                   .-.'), nl,
        write('    .-""`""-.    |(@ @)'), nl,
        write(' _/`oOoOoOoOo`\\_ \\ \\-/'), nl,
        write('''.-=-=-=-=-=-=-.''  / \\'), nl,
        write('  `-=.=-.-=.=-''    \\ /\\'), nl,
        write('     ^  ^  ^       _H_ \\'), nl.

describe(jupiter_station) :-
        write('Welcome to the Jupiter Gas Station: The one stop shop to get you to Saturn'), nl,
        write('use take and fly to refuel and takeoff'), nl,
        write(''), nl,
        write('                   .-.'), nl,
        write('    .-""`""-.    |(@ @)'), nl,
        write(' _/`oOoOoOoOo`\\_ \\ \\-/'), nl,
        write('''.-=-=-=-=-=-=-.''  / \\'), nl,
        write('  `-=.=-.-=.=-''    \\ /\\'), nl,
        write('     ^  ^  ^       _H_ \\'), nl.

describe(mars) :-
        write('Welcome to the Red Planet'), nl,
        write(''), nl,
        write('There used to be a famous labratory here.'), nl,
        write('I wonder if it still exists...?'), nl.

descrite(mars_lab) :- confirm(true), 
        write('The door is wide open,'), nl,
        write('type enter and exit'), nl,
        write('to make your way through.'), nl.

describe(mars_lab) :-
        write('This is the legendary Mars Lab!'), nl,
        write('Try to guess the security code and see what''s inside.'), nl,
        write('hint: it''s 3 of the same number').

describe(rocket_room) :-
        write('Welcome to the secret Rocket Room!'), nl,
        write('Take these supercharged rockets, and'), nl,
        write('use lightspeed at the next station you visit!'), nl, !.

describe(jupiter) :-
        write('Welcome to Jupiter.'), nl,
        write('Almost there.'), nl,
        write('If you were lucky enough to get into'), nl,
        write('the Mars Lab, try using lightspeed to get to'), nl,
        write('Saturn without refueling...'), nl, !.

describe(saturn) :-
        write('Congratulations!!  You are the first to arrive with the moonrocks!'), nl,
        write('You win the race!'), nl,
        write('                                          _.oo.'), nl,
        write('                  _.u[[/;:,.         .odMMMMMM'), nl,
        write('               .o888UU[[[/;:-.  .o@P^    MMM^'), nl,
        write('              oN88888UU[[[/;::-.        dP^'), nl,
        write('             dNMMNN888UU[[[/;:--.   .o@P^'), nl,
        write('            ,MMMMMMN888UU[[/;::-. o@^'), nl,
        write('            888888888UU[[[/o@^-..'), nl,
        write('           oI8888UU[[[/o@P^:--..'), nl,
        write('        .@^  YUU[[[/o@^;::---..'), nl,
        write('      oMP     ^/o@P^;:::---..'), nl,
        write('   .dMMM    .o@^ ^;::---...'), nl,
        write('  dMMMMMMM@^       ^^^^'), nl,
        write(' YMMMUP^'), nl, finish, !.



