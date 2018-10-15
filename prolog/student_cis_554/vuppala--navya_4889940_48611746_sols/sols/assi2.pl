/* ASSI2 -- a sample adventure game, by Navya Vuppala.
   Consult this file and issue the command:   start.  */

:- dynamic at/2, i_am_at/1, alive/1.   /* Needed by SWI-Prolog. */
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

/* This defines my current location. */
i_am_at(labyrinth).

/* These facts describe how the rooms are connected. */

path(labyrinth, u, dragonworld).
path(labyrinth, d, lavamountains).
path(labyrinth, e, kingslanding).
path(kingslanding, e, elisisland):- at(parachute,in_hand).
path(kingslanding, e, elisisland):- write('You Want to fly in a jet plane without a parachute? Are you crazy! Go get parachute first.	'),nl,!,fail.
path(labyrinth, w, blackcastle).
path(blackcastle,w,cornucopia) :- at(teslacar,in_hand).
path(labyrinth, s, eerie).
path(eerie, s, wheeloffortune).
path(labyrinth, n, thecapitol) :-at(teslacar,in_hand),at(electricitycapsule,in_hand),at(timecapsule,in_hand),at(key,in_hand).
path(labyrinth, n, thecapitol) :- write('The Capitol is very far, you cant walk there. First go get a vehicle and a key to locked door.!'),nl,!,fail.
path(thecapitol, n,presidentsarena).
path(presidentsarena,l1,lockeddoor1).
path(presidentsarena,l2,lockeddoor2).

path(dragonworld, d, labyrinth).
path(lavamountains,u,labyrinth).
path(kingslanding, w,labyrinth ).
path(elisisland,w,labyrinth).
path(blackcastle, e, labyrinth).
path(cornucopia,e,labyrinth).
path(eerie, n, labyrinth).
path(wheeloffortune, n, labyrinth).
path(thecapitol,s,labyrinth).
path(lockeddoor1,q,labyrinth).

/* These facts tell where the various objects in the game
   are located. */

at(magiclamp, cornucopia).
at(key,wheeloffortune).
at(parachute, labyrinth).
at(teslacar, elisisland).
at(timecapsule,labyrinth).


/* These rules describe how to pick up an object. */

take(X) :-
        at(X, in_hand),
        write('You''re already holding it!'),
        nl, !.

take(timecapsule):-
	i_am_at(labyrinth),
	\+ at(timecapsule, in_hand),
        retract(at(timecapsule, labyrinth)),
        assert(at(timecapsule, in_hand)),
	assert(at(timecapsule, labyrinth)),
        write('OK.'),
        nl, !.

take(magiclamp):-
	i_am_at(cornucopia),
	\+ at(electricitycapsule,cornucopia),
	assert(at(electricitycapsule,cornucopia)),
	retract(at(magiclamp,cornucopia)),
	write('Wow look, the ginie has gifted you with unlimited electricity capsule for your car.!'),nl,
	write('Now your car is complete.'),nl,
	write('Oops, the magic lamp has disappeared now.'),nl,
	write('After you take the capsule, go w. to go back to labyrinth'),nl,nl,
	write('There is electricitycapsule here'),nl,!.

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


/* These rules describe how to put down an object. */

drop(timecapsule):-
	at(timecapsule, in_hand),
	write('Dropping the time capsule will end your game, I know you certainly dont wanna do that.'),nl,
	write('You cannot drop the time capsule'),nl,
	write('Enter halt. to end the game'),nl,!.
	
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

l1:-go(l1).

l2:-go(l2).

q :-go(q).


/* This rule tells how to move in a given direction. */

go(l1):-
	i_am_at(presidentsarena),
	path(presidentsarena,l1,lockeddoor1),
	retract(i_am_at(presidentsarena)),
	assert(i_am_at(lockeddoor1)),
	look,
	retract(at(timecapsule,in_hand)),!.
	
go(l2):-
	i_am_at(presidentsarena),
	path(presidentsarena,l2,lockeddoor2),
	retract(i_am_at(presidentsarena)),
	assert(i_am_at(lockeddoor2)),
	look,!.

go(q):-
        i_am_at(lockeddoor1),
        path(lockeddoor1, q, labyrinth),
        retract(i_am_at(lockeddoor1)),
        assert(i_am_at(labyrinth)),
        look, !.

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
		at(timecapsule,in_hand),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        look, !.

go(_) :-
        write('You can''t go that way. You either dont have the time capsule or are going in the wrong direction.').


/* This rule tells how to look about you. */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl.

/*To know what all items you are currently holding*/

hand :- 
        at(X, in_hand),
        write('There is a '), write(X), write(' in your hand.'), nl,
        fail.

/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).


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
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.                   -- to start the game.'), nl,
        write('n.  s.  e.  w.  u.  d. l1. l2. q. -- to go in that direction.'), nl,
        write('take(Object).            -- to pick up an object.'), nl,
        write('drop(Object).            -- to put down an object.'), nl,
        write('look.                    -- to look around you again.'), nl,
	write('hand. 			-- to know the objects in your hand.'), nl,
        write('instructions.            -- to see this message again.'), nl,
        write('halt.                    -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(labyrinth) :-
	write('Your are in a labyrinth. If you want to win the throne you need some necessary items from different kingdoms.'), nl,
	write('Go in a (n.,s.,e.,w.,u.,d.) direction to know about a kingdom'), nl,
	write('You have Limited time of 40 minutes to complete the journey.'), nl,
	write('There are unlimited time capsules in the labyrinth, each capsule gives you 40min.'), nl, 
	write('You cannot take more than one capsule unless you are left with 0 min'), nl,
	write('Wait first look around, there are some items for you, Go take them.'), nl.

describe(kingslanding) :-
	write('Welcome to the land ruled by the mighty and generous king Elon Musk. His Palace is on Elis Island to the east.'),nl,
	write('If you are afraid to meet the king, then go west to go back to labyrinth.'),nl,
	write('However, Elon doesnt disappoint his guest, so be brave and go meet him in his Palace.'),nl,
	write('Use the jet plane lying at the shore and go e. to fly to Elis Island. Dont forget to take the parachute with you.'),nl.

describe(elisisland) :-
	write('Welcome to beautiful Elis Island. You met Elon and saw how kind and smart he is.'),nl,
	write('Wow! Look around, he has gifted you with a brand new Tesla Model S car minimizable to a toy size car.'),nl,
	write('Go take it and put it in your pocket. Do remember this is an Incomplete object and you need to obtain the fuel to get the car working.'),nl,
	write('Go to west to go back to labyrinth and find the fuel.'),nl.

describe(eerie) :-
	write('Welcome to Eerie- the place of strangeness.'), nl,
	write('If you think the place is too wierd and scary go back labyrinth which is in the west.' ), nl,
	write('If you feel gutsy and want to conquer your fear then go south-s. and spin the wheeloffortune to know what hidden object this place beholds for you.'),nl.

describe(wheeloffortune) :-
	write('You have just spinned the wheel of fortune. Look around, the key is the hidden object. Take it soon before it disappears'),nl,
	write('Now, go north to go back to labyrinth'),nl.

describe(blackcastle) :-
	write('Welcome to the magical and mysterious Black Castle. In this Castle is a Ginie who wishes your grant. He rests in a lamp lying in between the cornucopia of Gold. Ask him for only
something you need. Dont be greedy for otherwise the Ginie will take back all your possessions.'),nl,
	write('Go west to wake the ginie up or go east to reach labyrinth. The choice is yours!'),nl.

describe(cornucopia) :-
	write('Dont be mesmerized by the cornucopia of gold you see lying around.'),nl,
	write('This is a trap by the Genie. look around for the magiclamp. lying around.'),nl,
	write('Once you take it the genie will wake up and grant your wish'),nl,
	write('Remember once the genie grants your wish the magic lamp disappears for ever till the game ends'),nl.

describe(thecapitol):-
	write('Welcome to Capitol-the modern world of flying saucers and hanging buildings.'),nl,
write('Surprised by the advancement of this  place? Well, try to peek into the mesmerizingly beautiful presidents arena located to north.'),nl,
write('That is where you will find the throne.'),nl.

describe(lockeddoor1):-
	write('You have choosen the wrong door!'),nl,
	write('Oops you have almost only 0 minutes of time, go back to labyrinth to collect another time capsule.'),nl,
	write('Use the quick underground passage-q. to reach labyrinth before you get timed out,'),nl,
	write('Later get back here and try your luck with other door.'),nl.
	
describe(lockeddoor2):-
	write('Congratulations!!! You have chosen the right door. You just won the throne and an iphone6s.'),nl,
	write('Now take a selfie with The Throne and post it on facebook. Good bye!'),nl.

describe(dragonworld) :-
	write('Watch out, there are angry flying dragons every where popping out their firey toungue to catch a prey. Go down to reach labyrinth before you become a toasted chicken for the dragons'),nl.

describe(lavamountains) :-
	write('Hot molten lava overflowing from the high peaked mountains. Go up to reach labyrinth before you melt due to heat'),nl.

describe(presidentsarena) :-
	write('You have almost reached your destination.'),nl,
	write('Now either select lockeddoor1-l1. or lockeddoor2-l2. to get to the throne'),nl,
	write('Use the key you found at eerie to unlock any door'),nl.





