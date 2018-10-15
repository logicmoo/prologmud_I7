/* Out of Inferno, Muruo Liu. */
:- dynamic i_am_at/1, at/2, holding/1, timer/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(locked(_)).
/* The place I am at at the beginning*/
i_am_at(bedroom).

/* The remaining timer*/
timer(20).
locked(1).
/* define the path between different rooms*/
path(livingroom, s, bedroom).
path(livingroom, n, bathroom).
path(livingroom, w, door).
path(livingroom, e, studyroom).
path(bedroom, n, livingroom).
path(bathroom, s, livingroom).
path(door, e, livingroom).
path(studyroom, w, livingroom).




/* define the things at different places*/
at(key1, bedroom).
at(key2, bathroom).
at(browser, studyroom).
at(gun, livingroom).
at(television, livingroom).
at(safe, livingroom).

/* define movable things*/
movable(key1).
movable(key2).
movable(gun).
movable(bullets).
movable(cable).
movable(code).

/**  define unmovable things*/
unmovable(television).
unmovable(safe).
unmovable(browser).

/* define the actions taken when taking different direction*/
n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).


/* actions taken when moving*/
go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        retract(timer(T)),
        Tn is T - 1,
        assert(timer(Tn)),
        look, !, check.
        

go(_) :-
        write('You cannot go that way'), nl.
getTime(J) :- retract(timer(N)), J is N, assert(timer(N)).
/* take a look at the room and see things at there*/
look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_object_at(Place), !,
        nl.

/* print out which room it is*/
describe(Place) :-
        write('I am at '), write(Place).

/* print out the things in the room*/
notice_object_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.
notice_object_at(_).

notice_object_at(_).

/* take movable things*/
take(X) :-
        movable(X),
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take(X) :-
        unmovable(X),
        i_am_at(Place),
        at(X, Place),
        write('It is unmovable, so you cannot take it. You can only use it.'),
        !, nl.

take(X) :-
        holding(X),
        write('You have hold it!'), !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.

/* use things*/
use(browser) :-
        holding(browser),
        write('Find nothing!'), nl.

use(browser) :-
        holding(key1),
        holding(key2),
        i_am_at(studyroom),
        assert(at(cable, studyroom)),
        retract(timer(T)),
        Tn is T - 1,
        assert(timer(Tn)),
        write('Find a cable!'), nl, !,
        check.

use(browser) :-
        i_am_at(studyroom),
        write('Nothing happens. The browser is locked and you need two keys to open it.'),
        retract(timer(T)),
        Tn is T - 1,
        assert(timer(Tn)), !,
        nl, check.
use(browser) :-
        holding(browser),
        write('Find nothing!'), nl.

use(television) :- 
        holding(code),
        write('Find nothing!'), nl.

use(television) :-
        holding(cable),
        i_am_at(livingroom),
        assert(at(code, livingroom)),
        retract(timer(T)),
        Tn is T - 1,
        assert(timer(Tn)),
        write('Find a code!'), nl, !,
        check.

use(television) :-
        i_am_at(livingroom),
        retract(timer(T)),
        Tn is T - 1,
        assert(timer(Tn)),
        write('Nothing happens. The cable of the TV is missing.'), !,
        nl, check.

use(safe) :-
        holding(bullets),
        write('Find nothing!'), nl.

use(safe) :-
        holding(code),
        i_am_at(livingroom),
        assert(at(bullets, livingroom)),
        retract(timer(T)),
        Tn is T - 1,
        assert(timer(Tn)),
        write('Find some bullets!'), nl, !,
        check.

use(safe) :-
        i_am_at(livingroom),
        retract(timer(T)),
        Tn is T - 1,
        assert(timer(Tn)),
        write('Nothing happens. The safe needs some code to open.'), !,
        nl, check.

use(gun) :-
        holding(gun),
        holding(bullets),
        i_am_at(door), !,
        retract(locked(_)),
        assert(locked(0)),
        write('The lock has been broken!'), nl.

use(gun) :-
        holding(gun),
        retract(timer(T)),
        Tn is T - 1,
        assert(timer(Tn)),
        write('Nothing happens. Don''t do silly thing.'), nl, !,
        check.

use(gun) :-
        retract(timer(T)),
        Tn is T - 1,
        assert(timer(Tn)),
        write('You have no gun yet!'), nl, !,
        check.

use(X) :-
       holding(X),
       retract(timer(T)),
       Tn is T - 1,
       assert(timer(Tn)),
       write('Nothing happens.'), !,
       nl, check.

use(X) :-
       movable(X),
       retract(timer(T)),
       Tn is T - 1,
       assert(timer(Tn)),
       write('Nothing happens.'), !,
       nl, check.

use(_) :-
       write('I don''t see it here.'),
       retract(timer(T)),
       Tn is T - 1,
       assert(timer(Tn)), !,
       nl, check.

/* drop things*/
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



/* gameover*/
gameover :-
        write('Game Over. You have run out of timer'), nl.

/* check whether timer has run out*/
check :-
        getTime(N), N < 1, gameover, Y is N, assert(timer(Y)).
check :-
        getTime(N), N >= 1, write('You still have time!'), nl.
/* win*/
win :-
        write('Congratulations! You have escaped!'), nl.

/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

/* instructions of the game*/
instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             -- to start the game.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction. Every successful walk will consume timer'), nl,
        write('take(Object).      -- to pick up a movable object.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
        write('use(Object).       -- to use an unmovable item. Each action of use will consume timer, no matter whether it is a proper use.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        write('getTime.           -- to get the remaining time.'), nl,
        write('route.             -- to get the places you can go to and their directions.'), nl,
        write('itemList.          -- to get the things you are holding.'), nl,
        write('openTheDoor.       -- open the door.'), nl.

/* the background of the game*/
background :- 
        write('You are trapped in big fire. '), nl, 
        write('What makes things worse is that the door is locked. '), nl,
        write('You have to find a way to escape in limited time! Good Luck!'), nl.

/* open the door*/
openTheDoor :-
        i_am_at(door),
        locked(0),
        win.

openTheDoor :-
        i_am_at(door),
        locked(1),
        write('The door has been locked! You cannot get out of here now!'), nl.

openTheDoor :-
        write('The door is not here!'), nl.


/* start the game */
start :-
      background,
      instructions,
      look.

/* get the item you are holding*/
itemList :- 
       holding(X),
       write('I am holding a '), write(X), nl,
       fail.

itemList.

/* get the places you can go to and their directions*/
route :- 
       i_am_at(Here),
       path(Here, X, Y),
       write('press ') , write(X), write('. '), write('and you can reach '), write(Y), 
       nl, fail.

route.

