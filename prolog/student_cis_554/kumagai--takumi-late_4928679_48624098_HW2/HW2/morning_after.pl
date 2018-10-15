/* The Morning After a Party, by Takumi Kumagai. */

:- dynamic i_am_at/1, at/2, holding/1, timeLeft/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(holding(_)), retractall(timeLeft(_)).

:- write('You wake up in your livingroom with a headache.'),nl.
:- write('That''s right. You partied all night last night, and the house is a mess!'),nl,nl.

:- write('You get a glass of water and take a look at your smartphone.'),nl.
:- write('You see a text message from your girlfriend saying she is very excited about the date.'),nl,nl.

:- write('Now you remember you have a date with your girlfriend today'),nl.
:- write('and realize you have to leave in 10 minutes to make it to the date on time.'),nl,nl.

:- write('Since you are feeling very sick, it takes you one minute to crawl from one room to another.'),nl.
:- write('You have to find your car key, and get to your car in the garage.'),nl.

i_am_at(livingroom).

timeLeft(10).

path(livingroom, u, bedroom).
path(bedroom, d, livingroom).

path(livingroom, d, basement).
path(basement, u, livingroom).

path(livingroom, n, closet) :- holding(closetKey).
path(livingroom, n, closet) :- write('The closed appears to be locked'), nl, !, fail.
path(closet, s, livingroom).

path(livingroom, s, garage).
path(garage, n, livingroom).

path(livingroom, e, bathroom).
path(bathroom, w, livingroom).


at(closetKey, bedroom).
at(carKey, box).

/* These rules describe how to pick up an object. */


take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.


/* These rules describe how to put down an object. */

drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('OK.'),
        !, nl.

drop(_) :-
        write('You aren''t holding it!'),
        nl.


/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).

u :- go(u).

d :- go(d).


/* This rule tells how to move in a given direction. */

go(_) :-
		timeLeft(X),
		X =:= 0,        
		write('You have ran out of time. You lose.'), nl,
        finish, !.
		

go(Direction) :-
		i_am_at(Here),
        path(Here, Direction, There),
		timeLeft(X),
		retract(timeLeft(X)),
		decrement(X,Y),
		assert(timeLeft(Y)),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        write('You have '), write(Y), write(' minutes left'), nl,
        !, describe(There).

go(_) :-
		timeLeft(X),
		write('You can''t go that way.'),nl,
        write('You have '), write(X), write(' minutes left').


decrement(X,Y) :- 
		Y is X-1.
		
/* This rule tells how to look about you. */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl.

open :-
		i_am_at(closet),
		retract(at(carKey, box)),
		assert(holding(carKey)),
		write('You now have a car key!'),
		nl,!.
		
open :- 
		write('There is nothing to open here!'), nl.
		
		

/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).

i :-
		holding(X),
		write('you have a '), write(X), write('.'), nl,
		fail.
i.


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
        write('start.             		-- to start the game.'), nl,
        write('n.  s.  e.  w.  u.  d.		-- to go in that direction.'), nl,
        write('take(Object).		-- to pick up an object.'), nl,
        write('drop(Object).		-- to put down an object.'), nl,
        write('look.			-- to look around you again.'), nl,
        write('open.			-- to open a box, if there is any.'), nl,
        write('instructions.		-- to see this message again.'), nl,
        write('halt.			-- to end the game and quit.'), nl,
        write('i.			-- to see the items you have.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(livingroom) :- write('You are in livingroom.'), nl.
describe(bedroom) :- write('You are in bedroom.'), nl.
describe(basement) :- write('You are in basement.'), nl.
describe(closet) :- 
		holding(carKey),
		write('You are in closet.'), nl.
describe(closet) :- write('There is a box. You don''t remember what is in it.'), nl.		
describe(garage) :-
        holding(carKey),
        write('You are in garage. You see your car. Now you can drive out from your garage and make it to the date!'), nl,
        write('You won the game.'), nl,
        finish, !.
describe(garage) :- 
		write('You are in garage. You see your car, but you don''t have the key.'), nl.
describe(bathroom) :- write('You are in bathroom.'), nl.

