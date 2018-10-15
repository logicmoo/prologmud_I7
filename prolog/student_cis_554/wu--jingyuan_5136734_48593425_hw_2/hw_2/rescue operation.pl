/* Rescue Operation --- designed by Wu Jingyuan
   Consult this file and issue the command:   start.  */

:- dynamic at/2, i_am_at/1, alive/1,count/1.   /* Needed by SWI-Prolog. */
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

/* This defines my current location. */

i_am_at(meadow).

/* This defines how many objects you have now. */

count(0).

/* These facts describe how the rooms are connected. */

path(meadow,n,market).
path(meadow,e,'dark forest'):- 
        at(flashlight, in_hand),
        at(batteries,in_hand),
        write('You have the flashlight. You can walk in the dark forest.'),nl.
path(meadow,e,'dark forest') :-
        at(flashlight,in_hand),
        \+ at(batteries, in_hand),
        write('You do not have the batteries. You cannot use the flashlight.'),nl,
        !,fail.
path(meadow,e,'dark forest'):-
        write('Go into the dark forest without a light? Are you crazy?'),nl,
        !,fail.

path(meadow,s,'wood village').

path(market,s,meadow).

path('wood village',n,meadow).
path('wood village',e,'death valley') :-
        alive(zombie),
        nl,!.
path('wood village',e,'death valley').

path('dark forest',s,'death valley') :-
        alive(zombie),
        nl,!.
path('dark forest',s,'death valley').
path('dark forest',w,meadow).

path('death valley',w,'wood village').
path('death valley',n,'dark forest') :-
        at(flashlight,in_hand),
        at(batteries,in_hand),
        write('You have the flashlight. You can walk in the dark forest.'),nl.
path('death valley',n,'dark forest') :-
        at(flashlight,in_hand),
        \+ at(batteries, in_hand),
        write('You do not have the batteries. You cannot use the flashlight.'),nl,
        !,fail.
path('death valley',n,'dark forest') :-
        write('Go into the dark forest without a light? Are you crazy?'),nl,
        !,fail.
path('death valley',s,'secret chamber').

path('secret chamber',n,'death valley').



/* These facts tell where the various objects in the game
   are located. */


at(flashlight,market).
at(boat,'wood village').
at(batteries,'wood village').
at(gun,meadow).
at(spell,'dark forest').
at(tent,'wood village').
at('poison pill','dark forest').
at(parachute,market).


/* This fact specifies that the spider is alive. */

alive(zombie).


/* These rules describe how to pick up an object. */

take(X) :-
        at(X, in_hand),
        write('You''re already holding it!'),
        nl, !.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        count(3),
        write('You can just take at most three objects. To take the'),nl,
        write('object you want, you must drop something.'),nl,
        nl,!.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(at(X, in_hand)),
        retract(count(N)),
        M is N+1,
        assert(count(M)),
        write('OK.'),
        nl, !.

take(_) :-
        write('I don''t see it here.'),
        nl.


/* These rules describe how to put down an object. */

drop(X) :-
        at(X, in_hand),
        i_am_at(Place),
        retract(at(X, in_hand)),
        assert(at(X, Place)),
        retract(count(N)),
        M is N-1,
        assert(count(M)),
        write('OK.'),
        nl, !.

drop(_) :-
        write('You aren''t holding it!'),
        nl.


/* These rules define the six direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).


/* This rule tells how to move in a given direction. */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, 'death valley'),
        alive(zombie),
        at(gun,in_hand),
        write('The death valley is full of zombies!'),nl,
        retract(i_am_at(Here)),
        assert(i_am_at('death valley')),
        look,!.
go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, 'death valley'),
        alive(zombie),
        \+ at(gun,in_hand),
        write('The death valley is full of zombies!'),nl,
        write('You do not have weapons to kill the zombies.'),nl,
        write('Your brain is eaten by the zombies!'),!,die.
go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, 'death valley'),
        write('There is no zombies. You can walk in the death valley'),nl,
        write('safely.'),
        retract(i_am_at(Here)),
        assert(i_am_at('death valley')),
        nl,!.
go(Direction) :-
        i_am_at(Here),
        path(Here,Direction,'secret chamber'),
        alive(zombie),
        write('The entrance of the secret chamber is blocked by the '),nl,
        write('zombies. You cannot go to the secret chamber.'),nl,!,fail.
go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, 'secret chamber'),
        \+ at(boat,in_hand),
        write('You do not have the boat! The lake is full of ghosts.'),nl,
        write('They drag you down to the bottom of the lake.'),nl,
        !,die.
go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, 'secret chamber'),
        at(boat,in_hand),
        write('You have a boat and you can cross the river.'),nl,
        retract(i_am_at(Here)),
        assert(i_am_at('secret chamber')),
        look, !.
go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        look, !.

go(_) :-
        write('You can''t go that way.').


/* This rule tells how to look about you. */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl.


/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).

/* This rule tells what items the user are holding now. */

i :-
    at(Item, in_hand),
    write('You have '),write(Item),write('.'),nl,fail.
i.



/* This rule tells how to use the object. */

use(gun) :-
        i_am_at('death valley'),
        alive(zombie),
        retract(alive(zombie)),
        write('You have successfully killed all the zombies!'),nl,!.

use(spell) :-
        i_am_at('secret chamber'),
        write('You open the door of the secret chamber! You finally'),nl,
        write('see your lovely princess!'),nl,
        !,finish.

use(_) :-
        write('Nothing changes.'),nl.



/* This rule tells how to die. */

die :-
        !, finish.


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
        write('You are a prince. Your princess has been taken away to a'),nl,
        write('secret chamber by your enemy. The journey is destined to be '),nl,
        write('dangerous and threatening! My hero! Go to rescue your lovely'),nl,
        write('princess! Take care!'),nl,
        write('Reminder: You can just take at most three objects.'),nl,
        write('Available commands are:'), nl,
        write('start.                   -- to start the game.'), nl,
        write('n.  s.  e.  w.           -- to go in that direction.'), nl,
        write('take(Object).            -- to pick up an object.'), nl,
        write('drop(Object).            -- to put down an object.'), nl,
        write('i.                       -- to see the objects that you are'),nl,
        write('                            currently holding.'),nl,
        write('use(Object).             -- to use the object.'),nl,
        write('look.                    -- to look around you again.'), nl,
        write('instructions.            -- to see this message again.'), nl,
        write('halt.                    -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(meadow) :-
        write('You are in a meadow.'),nl,
        write('Go north, arrive at the market.'),nl,
        write('Go east, arrive at the dark valley.'),nl,
        write('Go south, arrive at the wood village.'),nl.
describe(market) :-
        write('You are in the market.'),nl,
        write('Go south, arrive at the meadow.'),nl.
describe('wood village') :-
        write('You are in the wood village.'),nl,
        write('Go north, arrive at the meadow.'),nl,
        write('Go east, arrive at the death valley.'),nl.
describe('death valley') :-
        alive(zombie),
        write('You are in the death valley. The zombies are coming!'),nl,
        write('Use your wisdom to kill all the zombies. Or, you can '),nl,
        write('leave in the west or north.'),nl,
        write('Go north, arrive at the dark forest.'),nl,
        write('Go west, arrive at the wood village.'),nl,
        write('Go south, there is a ghost lake.'),nl,!.
describe('death valley') :-
        write('You are in the death valley. There are no zombies. '),nl,
        write('You can be safe in the death valley.'),nl,
        write('Go north, arrive at the dark forest.'),nl,
        write('Go west, arrive at the wood village.'),nl,
        write('Go south, there is a ghost lake.'),nl.
describe('dark forest') :-
        write('You are in the dark forest. There lives an elderly '),nl,
        write('witch. Since you have saved her before, the witch gives'),nl,
        write('you a spell and a poison pill. They may be useful in the'),nl,
        write('coming journey.'),nl,
        write('Go west, arrive at the meadow.'),nl,
        write('Go south, arrive at the death valley.'),nl.
describe('secret chamber') :-
        write('You are in the entrance of the secret chamber. There is'),nl,
        write('an oldstone gate in front of you. You need a key to open'),nl,
        write('the gate of the secret chamber!'),nl,
        write('Go north, arrive at the death valley.'),nl.












