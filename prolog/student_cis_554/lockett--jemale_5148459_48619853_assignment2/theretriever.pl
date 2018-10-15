/* The Finder--an adventure game for CIS 554, by Jemale Lockett.
   Consult this file and issue the command: start. */

:- dynamic i_am_at/1, at/2, holding/1, stick_count/1, carrying/1, distracted/1, hand_count/1, wearing_backpack/0, headphones_returned/0, watch_returned/0, bandana_returned/0.
:- retractall(at(_, _)), retractall(i_am_at(_)),
	retractall(stick_count(_)), retractall(carrying(_)), retractall(distracted(_)), retractall(hand_count(_)),
	retractall(wearing_backpack), retractall(headphones_returned), retractall(watch_returned), retractall(holding(_)),
	retractall(bandana_returned).
i_am_at(porch).

dog(pomeranian).
  at(pomeranian, park).
dog(pitbull).
  at(pitbull, barber).
dog(rottweiler).
  at(rottweiler, barber).
dog(bull_terrier).
  at(bull_terrier, britton).
dog(german_shepard).
  at(german_shepard, anders_driveway).

/*Facts detailing the world*/

/*GAME MAP*/			     /*These number correspond to paths on a drawn out map*/
path(porch, n, anders).              /*1*/
  path(anders, s, porch).

path(porch, s, house):- watch_returned, headphones_returned.               /*2*/
path(porch, s, house):- write('You can''t go inside yet!'), nl, !, fail.
  path(house, n, porch).

path(porch, e, garage).              /*3*/
  path(garage, w, porch).

path(porch, w, britton).             /*4*/
  path(britton, e, porch).

path(anders, w, anders_driveway).    /*5*/
  path(anders_driveway, e, anders).

path(anders, e, barber).            %6
  path(barber, w, anders ).         %7

path(anders, n, anders_shed).
  path(anders_shed, s, anders).


path(barber, n, deli):- distracted(pitbull).
path(barber, n, deli):- write('There is a pitbull in front of the store driving away business.'), nl, !, fail.
  path(deli, s, barber).

path(barber, e, gas_station):- distracted(rottweiler).
path(barber, e, gas_station):- write('There''s a really mean looking rottweiler you don''t want to mess with'), nl, !, fail.
  path(gas_station, w, barber).

path(anders_driveway, s, britton).
  path(britton, n, anders_driveway).

path(anders_driveway, n, lot):- distracted(german_shepard).
path(anders_driveway, n, lot):- write('The Anders'' german shepard is blocking the way.'), nl, !, fail.
  path(lot, s, anders_driveway).

path(britton, w, knoll):- distracted(bull_terrier).              %8
path(britton, w, knoll):- write('There''s a bull terrier who has this territory marked.'), nl, !, fail.

  path(knoll, e, britton).

path(britton, s, britton_garage).          %9
  path(britton_garage, n, britton).



path(knoll, w, park_entrance).  %10
  path(park_entrance, e, knoll).

path(park_entrance, n, park):- (holding(bandana); carrying(bandana); bandana_returned).
path(park_entrance, n, park):-  write('There''s a group of kids blocking the entrance to the park.'),nl,
                                write('They tell you to retrieve the bandana of one of the kids who'), nl,
				write('lost it getting a haircut'), nl, !,  fail.

  path(park, s, park_entrance).

path(park, n, swings):- distracted(pomeranian).
path(park, n, swings):- write('There''s a cute little pomeranian that you''ll scare to death if you go near the swings'), nl, !,  fail.
  path(swings, s, park).

path(park, w, trees).
path(trees, e, park).



/* ITEM LOCATIONS */

%at(watch, swings).
at(crowbar, garage).
at(backpack, anders_shed).
at(tape, britton_garage).
at(bandana, barber).
at(special_bandana, gas_station).
%at(meat, deli).
at(headphones, knoll).


at(stick, trees).
at(stick, anders_driveway).
at(stick, porch).

%at(rotweiler, park).
%at(pitbull, park).

/* Variables */
stick_count(0).
hand_count(0). %number of items in hands, max 2




/* These rules describe how to pick up an object. */
take(stick):-
	i_am_at(Place),
	at(stick, Place),
	hand_count(X),
	X < 2, !,
	retract(at(stick, Place)),
	assert(holding(stick)),

	%increase hand count
	Y is X + 1,
	retract(hand_count(X)),
	assert(hand_count(Y)),

	%increase stick count
	stick_count(Z),
	A is Z + 1,
	retract(stick_count(Z)),
	assert(stick_count(A)),

	write('Picked up a stick in hands'),
	assert(at(stick, trees)), !,
	nl.

take(stick) :-
	i_am_at(Place),
	at(stick, Place), /* sticks can be in multiple places*/
	wearing_backpack, !,

	%increase stick count
	stick_count(X),
	Y is X + 1,
	retract(stick_count(X)),
	assert(stick_count(Y)),

	retract(at(stick, Place)),
	assert(carrying(stick)),
	write('Put stick in backpack'),
	assert(at(stick, trees)), !,

	nl.

take(stick) :-
	write('Either it\'s not here or you don''t have room for it'), nl, !,  fail.

take(backpack):-
	i_am_at(Place),
	at(backpack, Place),

	hand_count(X),
	X < 2, !,
	retract(hand_count(X)),
	Y is X + 1,
	assert(hand_count(Y)),

	retract(at(backpack, Place)),
	assert(holding(backpack)),
	write('Aquired the backpack but a strap is broken.'), nl.

take(backpack):-
	write('You don''t have enough hands'), !, nl, fail.

take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
	%check if space
	hand_count(A),
	A < 2,!,
        retract(at(X, Place)),
        assert(holding(X)),

	%increase hand count
        hand_count(Y),
	Z is Y + 1,
	retract(hand_count(Y)),
	assert(hand_count(Z)),

        write('OK.'),
        !, nl.
take(X) :-
        i_am_at(Place),
        at(X, Place),
	%backpack is functional?
	wearing_backpack, !,
        retract(at(X, Place)),
        assert(carrying(X)),
        write('OK.'),
        !, nl.

take(_) :-
        write('Your hands may be full or the item is missing.'),
        nl.


/* These rules describe how to put down an object. */

drop(X) :-
        holding(X),
	X = stick,
	%fix hand_count
	hand_count(Y),
	retract(hand_count(Y)),
	Z is Y - 1,
	assert(hand_count(Z)),

        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('OK.'),
        !,

	stick_count(A),
	B is A - 1,
	retract(stick_count(A)),
	assert(stick_count(B)),
	nl.

drop(X) :-
        holding(X),

	%fix hand_count
	hand_count(Y),
	retract(hand_count(Y)),
	Z is Y - 1,
	assert(hand_count(Z)),

        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('OK.'),
        !,
	nl.

drop(_) :-
        write('You aren''t holding it!'),
        nl.


/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).


/* This rule tells how to move in a given direction. */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        !, look.

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
	\+dog(X),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).


/* This rule tells how to get rid of sticks*/
throw_stick :-
	holding(stick), !,
	write('throwing stick...'), nl,
	%fix stick count
	retract(stick_count(X)),
	Y is X - 1,
	assert(stick_count(Y)),

	%remove stick
        retract(holding(stick)),
	hand_count(A),
	B is A - 1,
	retract(hand_count(A)),
	assert(hand_count(B)),

	distract_dog,
	write('You threw a stick. You now have '), write(Y),
	write(' sticks left'), nl.

throw_stick :-
	carrying(stick), !,
	write('throwing stick...'), nl,
	%fix stick count
	retract(stick_count(X)),
	Y is X - 1,
	assert(stick_count(Y)),

	%remove stick
	retract(carrying(stick)),

	distract_dog,
	write('You threw a stick. You now have '), write(Y),
	write(' sticks left'), nl.


throw_stick :- write('You don''t have any sticks!'), nl, fail.

distract_dog :-
	i_am_at(Place),
	nl,
	at(X, Place),
	dog(X), !,
	assert(distracted(X)).

distract_dog:- write('no dog here.'), nl, !.

/* List the Items in Inventory */

i:-
	inventory.

inventory:-
	holding(X),
	write(X), write(' in hand.'),  nl,
	fail.

inventory:-
	carrying(Y),
	write(Y), write(' in backpack.'), nl,
	fail.

inventory:- write('That''s all you have'), nl.

/* This rule tells how to die. */

die :-
        finish.


/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

finish :-
        nl,
        write('The game is over. Please enter the "halt." command.'),
        nl.


/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             -- to start the game.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction.'), nl,
        write('take(Object).      -- to pick up an object.'), nl,
	write('inventory. (or i.) -- to see your inventory.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
	write('use(Object).	  -- to use an object.'), nl,
	write('throw_stick.	  -- to throw a stick.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */
describe(porch) :- headphones_returned,
		   watch_returned, !,
		   write('All items returned! You can enter home if you want.'), nl.

describe(porch) :- write('You are on your front porch because your.'), nl,
		   write('mother wants you to be active.'),nl,
		   write('You\'ve decided to become The Finder'), nl,
		   write('finding and returning objects for people.'),nl,
		   write('You can return to your precious internet'), nl,
		   write('when you\'ve found and returned 3 items to their'), nl,
		   write('rightful owners.'), nl,
		   write('To the north is the Anders home, to the west is the'),nl,
		   write('Brittons. To the east is your own garage.'), nl.

describe(anders) :- holding(watch), !,
		    write('Watch returned!'), nl,
		    retract(holding(watch)),
		    assert(watch_returned).

describe(anders) :- carrying(watch), !,
		    write('Watch returned!'), nl,
		    retract(carrying(watch)),
		    assert(watch_returned).


describe(anders) :- write('You are at the Anders home. Jeremy tells you'), nl,
                    write('that he lost hiw watch at the park. He mentions'), nl,
                    write('not to go past the german shepard in the driveway.'), nl,
		    write('Their driveway is to the west, and their shed is'), nl,
		    write('straight ahead to the north. Exit to the'), nl,
		    write('east of the house to head to the barbershop.'), nl.



describe(britton) :- holding(headphones), !,
	             write('Headphones returned!'), nl,
		     retract(holding(headphones)),
                     assert(headphones_returned).

describe(britton) :- carrying(headphones), !,
	             write('Headphones returned!'), nl,
		     retract(carrying(headphones)),
                     assert(headphones_returned).


describe(britton) :- write('You are at the Britton home. Brittany tries to'), nl,
	             write('get you to leave because she is too'), nl,
                     write('busy looking for her headphones she lost.'), nl,
		     write('To the west is a grassy knoll and to the south is the Brittons'' garage.').


describe(knoll) :- write('You are on a grassy knoll. To the west is the entrance to the local park.'), nl.

describe(britton_garage):- write('You are in the Britton garage. There''s not much here.'), nl.


describe(park_entrance):-holding(bandana), !,
	                 write('Bandana returned! You may enter park (n.)'), nl,
			 assert(bandana_returned),
			 retract(holding(bandana)).

describe(park_entrance):-carrying(bandana), !,
	                 write('Bandana returned! You may enter park (n.)'), nl,
			 assert(bandana_returned),
			 retract(carrying(bandana)).

describe(park_entrance) :- write('You are at the entrance to the park, which is to the north.'), nl.

describe(park) :- write('You are at the park. There are trees to the west and swings to the north.'), nl.

describe(garage) :- write('You are in your garage'), nl.

describe(anders_shed):- write('You are in the Anders shed.'), nl.

describe(anders_driveway):- write('You are in the Anders driveway. There'), nl,
			    write('appears to be an empty lot to the north'),nl,
			    write('and the Britton house to the south.'), nl.

describe(lot):- write('There is nothing here. You should have listened to Jeremy.'), nl.


describe(barber):- write('You are at the barber but you don''t'), nl,
                   write('really need a haircut...'), nl,
	           write('There is a deli to the north and a gas_station to the east.'), nl.

describe(gas_station):- write('You are at the gas station.'), nl.

describe(deli) :- write('You are in the deli.'), nl, receive_meat.

describe(trees):- write('You are at the trees.'), nl.

describe(swings):- write('You are at the swings.'), nl.

describe(house):-write('You made it back into the house! But you had so'), nl,
	         write('much fun retrieving things that you''re gonna go'), nl,
		 write('out and continue doing it! Look out for later chapters'), nl,
		 write('where things get bigger and better (think Uncharted!)'),
		 finish.

receive_meat :-
	wearing_backpack,
	assert(carrying(meat)),
	write('receiving meat'), nl.

receive_meat :- write('No room for meat being given').


use(swings) :-
	     write('You don''t see the watch so decide to take a swing'), nl,
	     write('As you''re swinging higher and higher you notice'), nl,
	     write('that there is a glimmer atop the swingset. It''s'), nl,
	     write('Jeremy''s watch! You''ve retrieved it!'), nl,
	     assert(carrying(watch)).

use(stick) :- throw_stick, !.

use(tape) :- holding(tape),
	     holding(backpack),
	     !,
	     retract(holding(backpack)),
	     assert(wearing_backpack),
	     retract(holding(tape)),
	     assert(carrying(tape)),
	     %fix hand count
	     hand_count(X),
	     retract(hand_count(X)),
	     Y is X - 2,
	     assert(hand_count(Y)),

	     write('Now wearing backpack'), nl.
use(Object) :-
	write('Using object'), nl.































