/* TREASURE HUNT -- an adventure game -- By Akriti Bahal.
   Consult this file and issue the command:   start.  */

:- dynamic at/2, i_am_at/1, alive/1.   /* Needed by SWI-Prolog. */
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

/* This defines my current location. */

i_am_at(home).


/* These facts describe how the rooms are connected. */

path(witch, d, park).

path(park, u, witch).
path(park, w, park_entrance).

path(road, u, thief).
path(thief, d, road).
path(park_entrance, e, park) :- at(energy_drink, in_hand).
path(park_entrance, e, park) :-
write('You are about to collapse! Find energy drink and drink it!!'), nl, !, fail.
path(park_entrance, s, home).


path(home, n, park_entrance) :- at(torch, in_hand), at(batteries, in_hand).
path(home, n, park_entrance) :- at(torch, in_hand),
write('Oops! Your torch is out of batteries. Find batteries to continue treasure hunt!'), nl, !, fail.
path(home, n, park_entrance) :-
        write('Going for treasure hunt without a torch is not possible.'), nl,
        !, fail.
path(home, s, school).

path(school, n, home).
path(school, w, road).

path(road, e, school).

path(classroom, w, school).
path(witch, u, treasure_chest).
path(treasure_chest, d, park).

path(school, s, yard) :-
write('Answer this to enter the yard.'), nl,
write('What five letter word becomes shorter when you add two letters to it?'), nl,
read(X),
(X = 'short' -> write('You successfully entered the yard!'), nl, !;
write('Oops! Wrong answer. You cannot enter the yard until you answer correctly.'), nl, !, fail).


path(yard, n, school).

path(classroom, e, closet).
path(closet, w, classroom).

path(school, e, classroom) :- at(classroom_key, in_hand).
path(school, e, classroom) :-
        write('The door appears to be locked.'), nl,
        fail.



/* These facts tell where the various objects in the game
   are located. */

at(treasure_chest, witch).
at(chest_key, yard).
at(knife, school).
at(torch, school).
at(energy_drink, closet).
at(batteries, classroom).
at(classroom_key, road).


/* This fact specifies that the witch is alive. */

alive(witch).
alive(thief).


/* These rules describe how to pick up an object. */

take(X) :-
        at(X, in_hand),
        write('You''re already holding it!'),
        nl, !.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(at(X, in_hand)),
        write('OK.'),
        nl, !.

take(_) :-
        write('I don''t see it here.'),
        nl.
		
/* This will tell what you are holding currently. */

		
i :-
at(_, in_hand),
write('You have: '), nl,
list_things.
i :-
write('You have nothing'), nl.

list_things :-
at(X, in_hand),
tab(2), write(X), nl,
fail.
list_things.


/* These rules describe how to put down an object. */

drop(X) :-
        at(X, in_hand),
        i_am_at(Place),
        retract(at(X, in_hand)),
        assert(at(X, Place)),
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

u :- go(u).

d :- go(d).


/* This rule tells how to move in a given direction. */

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




/* These rules tell how to handle killing the lion and the spider. */

kill :- 
i_am_at(road),
at(knife, in_hand),
retract(alive(thief)),
write('You killed the thief! Now take the classroom key and continue your treasure hunt!'), nl,!.

kill :-
        i_am_at(road),
        write('Oh!  You have been killed by the thief.'), nl,
        !, die.

kill :-
        i_am_at(park),
        write('Oh! The witch has evil potions to kill you!'), nl.


kill :-
        i_am_at(witch),
        at(knife, in_hand),
        retract(alive(witch)),
        write('You stabbed the witch in her stomach. The witch has'), nl,
        write('died'), nl, !.

kill :-
        i_am_at(witch),
        write('You can''t kill the witch by beating her with you hand.'), nl,
        write('You need a knife for it.'), nl.

kill :-
        write('There is nothing to kill here.'), nl.


/* This rule tells how to die. */

die :-
        !, finish.


/* Under UNIX, the   halt.  command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final  halt.  */

finish :-
        nl,
        write(' Game over. Enter halt command.'),
        nl, !.


/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.                   -- to start the game.'), nl,
        write('n.  s.  e.  w.  u.  d.   -- to go in north, south, east, west, up, down, direction.'), nl,
        write('take(Object).            -- to pick up an object.'), nl,
        write('i                        -- to see the things you are currently holding.'), nl,
        write('kill.                    -- to attack an enemy.'), nl,
		write('kill.                    -- to attack an enemy.'), nl,
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

describe(home) :-
        at(treasure_chest, in_hand), at(chest_key, in_hand),
		write('You used the chest key to open the treasure chest.'), nl,
        write('Congratulations !! You have all the gold. You are rich now!'), nl,
        write('You won the game.'), nl,
        finish, !.
		

		describe(home) :-
		at(treasure_chest, in_hand),
		write('You are at home but you need to find the chest key to open the treasure chest. Go back!!'), nl.

describe(home) :-
        write('You are at home. To north there is the park entrance.'), nl,
        write('To the south is the haunted school. Your task'), nl,
        write('is to get the treasure chest with all the gold back to your home.'), nl.
        


describe(school) :-
        write('You are in the haunted school. There is a'), nl,
        write('classroom in the east,'), nl,
        write('a yard in the south and a vast road in the west.'), nl.

describe(road) :-
		alive(thief),
        at(knife, in_hand),
        write('You are on the vast road. It''s dangerous here! There are'), nl,
        write('thiefs waiting to kill you! Go back! Or kill them with your knife!'), nl.

describe(classroom) :-
        write('You are in the classroom. There is a secret closet somewhere here.'), nl.
		
describe(closet) :-
		write('You entered the secret closet.'), nl.
		
describe(yard) :-
		write('You are in the yard.'), nl.
			  
describe(park_entrance) :-
        write('You are standing at the park entrance. To the east'), nl,
        write('is the park, where you can begin your treasure hunt.'), nl.


describe(park) :-
        alive(witch),
        at(treasure_chest, in_hand),
        write('The witch sees you with the chest and attacks you with her evil potion!'), nl,
        write('    ...it is over in seconds....'), nl,
        die.
		
describe(road) :-
		alive(thief),
		at(classroom_key, in_hand),
		write('The thief sees you with the key and shoots you!'), nl,
		write(' You are dead!'), nl,
		die.
		

describe(park) :-
        alive(witch),
        write('The witch is looking at you with evil looks! She has'), nl,
        write('an evil potion to kill you. I would advise you to leave'), nl,
        write('quietly....'), nl, !.
		
describe(park) :-
        write('There is a witch here, that is moaning.'), nl.

describe(witch) :-
        alive(witch),
        write('You jumped on the witch and are about to kill the witch.'), nl.
        

describe(witch) :-
        write('You are on top of the dead witch!'), nl.
