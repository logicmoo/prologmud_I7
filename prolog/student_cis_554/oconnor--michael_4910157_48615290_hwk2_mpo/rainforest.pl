/* Rainforest Escape, by Michael OConnor. */

:- dynamic i_am_at/1, at/2, holding/1, i_get_caught/1, is_caught/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

i_am_at(rfJail).

i_get_caught(13).

inventory :- holding(X), write(X), nl, fail.
inventory :- write('That''s all you got my friend.').

move_time(_) :- i_get_caught(X), X = 0, 
write('You have been caught by your jailers, and this time they won''t just throw you in a cell...'),
die, fail.
move_time(_) :- i_get_caught(X), Y is X - 1, retract(i_get_caught(X)), assert(i_get_caught(Y)).

is_caught(_) :- i_get_caught(X), X = 0, 
write('You have been caught by your jailers, and this time they won''t just throw you in a cell...'),
die.

/*winning game path*/
path(rfJail, n, baseCamp) :- 
write('You kick open the cell door and decide to run for freedom.'), nl, nl.

path(baseCamp, n, checkPoint).
path(checkPoint, s, baseCamp).

path(checkPoint, e, stream).
path(stream, w, checkPoint).

path(stream, s, cougarBed).
path(cougarBed, n, stream).

path(cougarBed, e, waterfall) :- holding(meat),
write('You throw the meat at the cougar. She eats it and lets you continue on.'), nl, nl.
path(cougarBed, e, waterfall) :- holding(macheteHilt), 
write('You try to hit kill the cougar with the machete hilt and she mauls your leg.  You are now much slower.'), nl, nl,
move_time(_), move_time(_), move_time(_), !, fail.
path(cougarBed, e, waterfall) :- 
write('A cougar is licking her lips and you think better of running by.'), nl, nl, 
        !, fail.
path(waterfall, w, cougarBed).
		
path(waterfall, n, clearing) :- 
write('You move away from the powerful waterfall full of wonder.'), nl, nl.
path(clearing, s,waterfall).

path(clearing, e, denseForest) :- 
write('From the calm of the clearing you plunge into the darkness and foreboding of the dense forest.'), nl, nl.
path(denseForest, w, clearing).

path(denseForest, s, cliff) :- holding(macheteHilt), holding(macheteBlade),
write('Using your machete you hack your way quickly through the dense forest and make sure not to touch any spiderwebs.'), nl, nl.
path(denseForest, s, cliff) :- move_time(_), move_time(_), move_time(_),
write('Without a machete it takes you a very long to get out of the dense forest while avoiding spiderwebs.'), nl, nl.
path(cliff, n, denseForest) :- write('Fearful of the unknown you back away from the cliff.'), nl, nl.

path(cliff, s, freedom) :-
write('You leap from the cliff to the abyss below, hoping that where you land is better than where you escaped from.'), nl, nl, finish.
path(cliff, e, freedom) :-
write('You leap from the cliff to the abyss below, hoping that where you land is better than where you escaped from.'), nl, nl, finish.
path(cliff, w, freedom) :-
write('You leap from the cliff to the abyss below, hoping that where you land is better than where you escaped from.'), nl, nl, finish.

at(meat, baseCamp).
at(macheteHilt, checkPoint).
at(oddLookingMound, clearing).

/* These rules describe how to pick up an object. */

take(oddLookingMound) :-
        holding(macheteBlade),
        write('You''re already dug this up!'),
        !, nl.

take(oddLookingMound) :-
        i_am_at(clearing),
        at(X, clearing),
        retract(at(X, clearing)),
        assert(holding(macheteBlade)),
        write('You dug up a machete blade!.'),
        !, nl.

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

i :- inventory.


/* This rule tells how to move in a given direction. */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
		move_time(_),		
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
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).


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
        write('drop(Object).      -- to put down an object.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('instructions.      -- to see this message again.'), nl,
		write('i.                 -- to see what you are currently holding.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(rfJail) :- write('You are in small iron jail cell somewhere in the rainforest.  You notice that the jail cell door faces north and seems rusted.'), nl.

describe(baseCamp) :- write('You are in the jailers basecamp, but it seems they are out doing evil jailer things.'),
write(' To your north looks like a checkpoint.  To the east and west is dense forest filled with spiderwebs'),
write('The jailers will find you in '), i_get_caught(X), write(X), write(' hours.'), nl.

describe(checkPoint) :- write('You have now reached the base entrance checkpoint. To your north and west you see more dense forest and spiderwebs.'),
write(' To your east you spy a bubbling stream. '),
write('The jailers will find you in '), i_get_caught(X), write(X), write(' hours.'), nl.

describe(stream) :- write('You are at a tranquil stream that passes through the forest. You know that if you follow it south you will find a cougar den. '),
write('To the north and east you see spooky eyes peering through the dense brush. '),
write('The jailers will find you in '), i_get_caught(X), write(X), write(' hours.'), nl.

describe(cougarBed) :- write('You are at a cougar den. She is well known in town for mauling all that get to close.'),
write(' Behind her to the east is a magnificent waterfall. To the south and west you see vicious monkeys holding guns like in Rise of the Planet of the Apes. '),
write('The jailers will find you in '), i_get_caught(X), write(X), write(' hours.'), nl.

describe(waterfall) :- write('You are at a towering waterfall.  Looking up and feeling the water spray on your face you feel rejuvinated.'),
write(' To your north you think you what looks like light at the end of a tunnel. '),
write('You can not head through the waterfall to your easat and the south is covered in spiderwebs. '),
write('The jailers will find you in '), i_get_caught(X), write(X), write(' hours.'), nl.

describe(clearing) :- write('You are in an immense clearing. You see the destruction your jailers have done to the forest as tree stumps cover the landscape.'),
write('You know you have to keep heading east to try and escape and ultimately stop them. '),
write('The jailers will find you in '), i_get_caught(X), write(X), write(' hours.'), nl.

describe(denseForest) :- write('You are in the dense forest.  The eyes, monkeys, and spiders from before can be behind any tree or leaf.'),
write('You know the only way forward is south, otherwise you will get lost'),
write('The jailers will find you in '), i_get_caught(X), write(X), write(' hours.'), nl. 

describe(cliff) :- write('You are at the edge of a plateau.  Every direction but the way you came leads to an immense abyss too deep to see what lies below.'),
write('The jailers will find you in '), i_get_caught(X), write(X), write(' hours.'), nl.
