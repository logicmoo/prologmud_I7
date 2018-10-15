/* <The name of this game>, by <your name goes here>. */

:- dynamic i_am_at/1, at/2, holding/1, status/1, not_destroyed/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

/* create a closet that needs to be destroyed to continue */
not_destroyed(closet).

i_am_at(bedroom).
%i_am_at(school). % for testing purposes

/* create paths linking rooms to each other. */
path(bedroom , n, bathroom).
path(bedroom , s, kitchen).
path(bedroom, e, closet).

path(closet, w, bedroom).

path(kitchen, n, bedroom).

/* Need to be wearing a suit or a fancy suit while holding a lunch to walk out the door */
path(kitchen, s, school):- (status(suit); status(fancy_suit), holding(sack_lunch)).
path(kitchen, s, school):-
	status(pajamas),
	writeln('Those are some really frilly pajamas.'),
	writeln('It would be a close second to being naked at school.'),
	writeln('Get dressed!'),
	!, fail.
path(kitchen, s, school):-
	writeln('School is long and hard.  You should pack a lunch in a bag.'),
	!, fail.

path(bathroom , s, bedroom).
path(bathroom , e, shower).
path(bathroom , w, toilet).

path(shower, w, bathroom).
path(toilet, e, bathroom).

path(school, s, classroom1).
path(classroom1, e, quiz_room):-
	writeln('Think you''r a smarty pants?').

/* Setting up the quiz rooms, here how you get somewhere matters (for points) */
path(classroom1, w, quiz_room):-
	writeln('Ha got you!  Detention is too good for you.'),
	writeln('Off to take a quiz.').

path(quiz_room, s, question1).

path(question1, e, question2):-
	writeln('Hmm, maybe that was too easy!'),
	writeln('Minus 1 life!'),
        subtract_life(1).


path(question1, w, question2):-
	writeln('Ha got you! Again!'),
	writeln('Minus 2 lives for you!'),
        subtract_life(2).

path(question2, e, question3):-
	writeln('A lucky guess!'),
	writeln('Minus 1 life!'),
        subtract_life(1).


path(question2, w, question3):-
	writeln('WRONG. Who knows this stuff anyway?  Except, it''s on a test!'),
	writeln('Minus 2 lives for you!'),
        subtract_life(2).

path(question3, e, lunchroom):-
	writeln('Why would you count the stars anyway.'),
	writeln('Fine.  Go to lunch.').

path(question3, w, lunchroom):-
	writeln('WRONG. Guess someone was bored.'),
	writeln('Minus 2 lives for you!'),
        subtract_life(2).


path(_, _).

/* place items in the world */
at(plain_bread, kitchen).
at(water, kitchen).
at(bottle, kitchen).

/* person status, for internal states */
status(need_to_pee).
status(pajamas).
status(alive).

/* declare list of things that person can use. */
use(suit):-
	holding(suit),
	status(wet),
	writeln('Too wet from shower! This will destroy the suit.').
use(suit) :-
	holding(suit),
	retract(holding(suit)),
	writeln('You feel like ready to tackle the world.'),
	retract(status(pajamas)),
	assert(status(suit)).

use(tie) :-
	holding(tie),
	status(suit),

	retract(status(suit)),
	assert(status(fancy_suit)),
	writeln('My my, someone looks dashing!').

use(tie):-
	holding(tie),
        writeln('It might be better to put something on first.'),
	writeln('This is a school after all').


use(towel):-
	status(wet),
	retract(status(wet)),
	writeln('All dry now!').
use(towel) :-
	writeln('Already dry.').

use(_) :-
	writeln('You can''t use that!').


/* things that can be combined together to create new items */
combine(plain_bread, bottled_water):-
	holding(bottled_water),
	holding(plain_bread),
	writeln('You just made a sack lunch!'),
	retract(holding(bottled_water)),
	retract(holding(plain_bread)),
	assert(holding(sack_lunch)).


combine(bottled_water, plain_bread):-
	holding(bottled_water),
	holding(plain_bread),
	writeln('You just made a sack lunch!'),
	retract(holding(bottled_water)),
	retract(holding(plain_bread)),
	assert(holding(sack_lunch)).

combine(water, bottle):-
	holding(bottle),
	holding(water),
	writeln('You just made bottled_water!'),
	retract(holding(bottle)),
	retract(holding(water)),
	assert(holding(bottled_water)).

combine(bottle, water):-
	holding(bottle),
	holding(water),
	writeln('You just made bottled_water!'),
	retract(holding(bottle)),
	retract(holding(water)),
	assert(holding(bottled_water)).

combine(suit, tie) :-
	holding(suit),
	holding(tie),
	writeln('That is one heck of a fancy suit.'),
	retract(holding(suit)),
	retract(holding(tie)),
	assert(holding(fancy_suit)).

combine(item1, item2) :-
	holding(item1), holding(item2),
	writeln('These do not combine!').

combine(_, _) :-
	writeln('Are you sure you have both ingredients?').

/* These rules describe how to pick up an object. */
take(X) :-
        holding(X),
        writeln('You''re already holding it!'),
        !.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        writeln('OK.'),
        !.

take(_) :-
        writeln('I don''t see it here.'),
        nl, fail.


/* These rules describe how to put down an object. */

drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        writeln('OK.'),
        !.

drop(_) :-
        writeln('You aren''t holding it!'),
        nl.


/* a life counter for quiz section */
subtract_life(X):-
    nb_getval(life, Life),
    Nlife is Life - X,
    nb_setval(life, Nlife),
    Nlife =< 0,
    retract(status(alive)),!,
    fail.



subtract_life(_).

/* see life counter */
display_life :-
	nb_getval(life, Life),
	write('You have '), write(Life), writeln(' remaining.').


/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).

i :- inventory.

/* This rule tells how to move in a given direction. */

go(Direction) :-
	status(alive),
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        !, look.

go(_) :-
	status(alive),
        writeln('You can''t go that way.').

go(_) :-
        i_am_at(Here),
        retract(i_am_at(Here)),
        assert(i_am_at(death)),
	!, look.

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

/* These rules set up a loop to mention all objects in inventory. */
inventory :-
	writeln('You own: '),
	list_inventory.

inventory :-
	writeln('You own nothing!').

list_inventory:-
	holding(X),
	writeln(X), fail.
list_inventory.

/* This is for internal use to debug*/
my_status :-
	writeln('Currently in state: '),
	list_status.

list_status:-
	status(X),
	writeln(X), fail.
list_status.




/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

finish :-
        nl,
        writeln('The game is over. Please enter the "halt." command.'),
        nl.


/* This rule just writelns out game instructions. */

instructions :-
        nl,
        writeln('Enter commands using standard Prolog syntax.'),
        writeln('Available commands are:'),
        writeln('start.             -- to start the game.'),
        writeln('n.  s.  e.  w.     -- to go in that direction.'),
        writeln('take(Object).      -- to pick up an object.'),
        writeln('drop(Object).      -- to put down an object.'),
        writeln('look.              -- to look around you again.'),
	writeln('inventory or i     -- to view your current inventory.'),
	writeln('combine(Item1, Item2) -- to make new items from inventory.'),
	writeln('use(Item1)         -- use item in inventory'),

	writeln('instructions.      -- to see this message again.'),
        writeln('halt.              -- to end the game and quit.'),
        nl.

/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(bedroom) :-
	writeln('It is a school day.'),
	writeln('n -- go to the bathroom.'),
	writeln('s -- go to the kitchen.'),
	writeln('e -- go to the closet.').


describe(kitchen):-
	writeln('The kitchen smells like baking bread.'),
        writeln('n -- return to bedroom'),
	writeln('s -- head to school!').

describe(closet):-
	not_destroyed(closet),
	status(wet),
	writeln('AIIIEEEEEE.  The door is melting!'),nl,
        retract(not_destroyed(closet)),

        assert(at(towel, closet)),
        assert(at(suit, closet)),
        assert(at(tie, closet)),

        describe(closet).


describe(closet):-
	not_destroyed(closet),

	writeln('The closet is closed.'),
	writeln('This closet is immune to earth, fire, air.'),
	writeln('w -- back to bedroom.'), !, fail.


describe(closet):-
	writeln('The closet is musty.  You should clean it at some point.'),
	writeln('But you can do that tomorrow.'),

	writeln('w -- back to bedroom.').


describe(bathroom) :-
	writeln('You are in the bathroom!'),
	writeln('s -- back to bedroom'),
	writeln('e -- to shower.'),
	writeln('w -- to use the toilet.').

describe(shower):-
	writeln('A nice warm shower shower wakes you up fully.'),
	writeln('You emerge from the shower dripping wet.'),
	writeln('w -- return to bathroom'),

	assert(status(wet)),

	status(smelly),
	retract(status(smelly)).


describe(toilet):-
	status(need_to_pee),
	writeln('Well that felt nice!'),
	retract(status(need_to_pee)),

	writeln('e -- return to bathroom').

describe(toilet) :-
	writeln('I don''t need to pee.'),
	writeln('e -- return to bathroom').

describe(school) :-
	writeln('Here be dragons.  Enter at your own risk.'),
	writeln('s -- Enter').


describe(classroom1) :-
	writeln('Welcome to your first class of the day!'),
	writeln('You in the fancy suit!'),
	writeln('Pop quiz or detention?'),

	writeln('e -- for pop quiz'),nl,
	writeln('w -- for detention').

describe(quiz_room) :-
	writeln('Welcome to the quiz room!'),
	writeln('The rules are simple.'),
	writeln('You have 3 lives.'),
	writeln('Answer correctly, 1 penalty.'),
	writeln('Answer incorrectly, 2 penalty.'),
	writeln('You run out of lives, you don''t make it to lunch.'),nl,

	writeln('Ready?'),nl,

	nb_setval(life, 3),


	writeln('s -- take a quiz').

describe(question1):-
	display_life,

        writeln('What is 1 + 1?'),

	writeln('e -- 2.  That was easy!'),
	writeln('w -- Not 2.').

describe(question2):-
	display_life,

        writeln('What is the capital of Zimbabwe?'),

	writeln('e -- Harare.'),
	writeln('w -- Abuja.').

describe(question3):-
	display_life,

        writeln('Are there more sand grains on Earth than stars in the sky?'),

	writeln('e -- No.'),
	writeln('w -- Yes.').

describe(lunchroom):-
	writeln('Congrats on making it to the lunch room.'),
	writeln('As you eat your plain bread and drink your water.'),
	writeln('You contemplate whether or not it is worth'),
	writeln('playing the sequel:  Classroom 2:  Recess'),
	finish, !.

describe(death):-
	writeln('Rightly or wrongly you have failed to last 1 day in school.'),
	writeln('Try again?'),
	finish.



