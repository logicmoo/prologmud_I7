/* Dragon ball z : An adventerous trip
   Search for fire to dragon to enter earth.  */

:- dynamic at/2, i_am_at/1, my_fuel/1.   /* Needed by SWI-Prolog. */
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(fuel(_)).

/* This defines my current location. */

i_am_at(mercury).

/* Intial fuel */
fuel(200).

/* Checks the fuel is greater than 0. everytime */
checkfuel(Fuel) :-  Fuel > 0.


/* These facts describe how planets and stars are connected. */

/* path from mercury to polestar and back. */
path(mercury, e, polestar).
path(polestar, w, mercury).

/* path from mercury to venus and back. */
path(mercury, w, venus).
path(venus, e, mercury).


/* path from mercury to sun and back. */
path(mercury, s, sun) :- at(lifejacket, in_hand).
path(mercury, s, sun) :-
        write('Going to sun without life jacket?  Are you crazy?'), nl,
        !, fail.
path(sun, n, mercury).


/* path from mercury to earth and back. */
path(mercury, n, earth) :- at(fire, in_hand).
path(mercury, n, earth) :-      write('The dragon will not allow you without fire in hand.'), nl,
        !, fail.
path(earth, s, mercury).


/* path from milky way back to venus and back. */
/* need to pick up bullets from milkyway. */
path(venus, s, milkyway) :- at(bullets, in_hand).
path(venus, s, milkyway) :-     
	write('The gun is not filled with enough bullets.'), nl,
        !, fail.
path(milkyway, n, venus).


/* path from sun to mercury and back*/
/*need lifejacket to enter sun*/
path(mercury, s, sun) :- at(lifejacket, in_hand).
path(mercury, s, sun) :-
        write('Going to sun without life jacket?  Are you crazy?'), nl,
        !, fail.


/*path from venus to box and back*/
/*need to give starcount to open the box*/
path(venus, w, box) :- at(starcount, in_hand).
path(venus, w, box) :-
        write('You do not have the star count yet.'), nl,
        fail.
path(box, e, venus).

/* These facts tell where the various objects in the game
   are located. */

at(lifejacket, box).
at(fire, sun).
at(bullets, polestar).
at(starcount, milkyway).



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



/* This rule tells how to move in a given direction. */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
	    fuel(Fuel),
        checkfuel(Fuel),!,
    	NewFuel is Fuel - 10,nl,
    	write('Fuel left is '),
    	write(NewFuel),nl,
	retract(fuel(Fuel)),
	assert(fuel(NewFuel)),
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



finish :-
        nl,
        write('The game is over. Please enter the   halt.   command.'),
        nl, !.


i :-
	write('We found these items in your inventory: '), nl,
	notice_objects_at(in_hand),nl,
        nl.


/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.                   -- to start the game.'), nl,
        write('n.  s.  e.  w.   -- to go in that direction.'), nl,
        write('take(Object).            -- to pick up an object.'), nl,
        write('drop(Object).            -- to put down an object.'), nl,
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

describe(_) :-
        fuel(Fuel),
	    \+ checkfuel(Fuel)
		,!,
        write('Sorry ! Fuel is up'), nl,
        write('and you lost the game.'), nl,
        finish, !.

describe(earth) :-
        at(fire, in_hand),
        write('Congratulations!!  You have entered earth'), nl,
        write('and won the game.'), nl,
        finish, !.

describe(mercury) :-
        write('You are at mercury and have a spaceship which will take you along your journey.'),nl,
		write('Be careful, you cannot travel once the fuel in the spaceship is over !!! '),nl,
		write('To the north is earth in which you have to enter and be cautious, '), nl,
		write('the dragon wont let you in without the fire'),nl,
        write('To the south is sun from where you have to pick up fire and cannot '), nl,
        write('enter without a life jacket. To the east is pole star which has'), nl,
        write('bullets to fight the shooting stars on the way to milky way from venus'), nl,
        write('which is south of venus. Go to the west of venus and there is locked box'), nl,
        write('Star count of the stars at the milky way is the code key to open the box.'), nl.
		
describe(venus) :-
        write('You have to pick up lifejacket in the box which is towards west from here.'), nl,
        write('The key of the box is count of stars at the milky way.'), nl,
        write('You need enough bullets in the gun to fight on the way to milky way'), nl,
	    write('which is to the south.'), nl.

describe(milkyway) :-
        write('Take the count of stars from milkyway'), nl,
        write('and then head back to venus!'), nl.

describe(sun) :-
        write('Procure fire from the sun, and now head to the mercury'), nl,
        write('and earth from there, offer fire to the dragon.'), nl.

describe(polestar) :-
        write('You reached the polestar. Load your gun with bullets'), nl,
	    write('to fight shooting stars on the milky way, take bullets here and'), nl,
	    write('rush to west.'), nl.

describe(box) :-
        write('Take life jacket from here'), nl,
	    write('and rush to the sun to take fire.'), nl.



