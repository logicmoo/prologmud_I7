/* <The House that Stief Built>, by <Aryeh Stiefel>. */

:- dynamic i_am_at/1, at/2, holding/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(holding(_)).

i_am_at(outside_stadium).

/* Describes the connections between the rooms */
path(outside_stadium, n, ticket_booth).
path(outside_stadium, w, gate6).

path(ticket_booth, s, outside_stadium).

path(gate6, e, outside_stadium).
path(gate6, n, m_concourse) :- holding(ticket).
path(gate6, n, m_concourse) :- 
    write('You can not enter without a ticket'), nl, !, fail.

path(m_concourse, w, bathroom).
path(m_concourse, n, seats1).
path(m_concourse, e, escalator1).

path(bathroom, e, m_concourse).
path(bathroom, w, atm).

path(atm, e, bathroom).

path(seats1, s, m_concourse).

path(escalator1, n, s_concourse).
path(escalator1, w, m_concourse).

path(s_concourse, s, escalator1).
path(s_concourse, n, seats2).
path(s_concourse, w, concessions) :- holding(money).
path(s_concourse, w, concessions) :- 
    write('You need to get money before you can enter the concession stand'), nl, !, fail.
path(s_concourse, e, escalator2).

path(seats2, s, s_concourse).

path(concessions, e, s_concourse).

path(escalator2, w, s_concourse).
path(escalator2, n, seats3) :- holding(popcorn).
path(escalator2, n, seats3) :- write('You need to get popcorn '),
    write('before sitting down'), nl, !, fail.
path(escalator2, e, emergency_exit).


at(ticket, ticket_booth).
at(money, atm).
at(popcorn, concessions).


/* These rules describe how to pick up an object. */

take(X) :-
        holding(X),
        write('You are already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take(_) :-
        write('I do not see it here.'),
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
        write('You are not holding it!'),
        nl.

i :- inventory.

inventory :- holding(X), write('You are holding '), write(X), nl,
    fail.

inventory :- \+holding(_), write('You are not holding anything'), nl, fail.

inventory.

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
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).


/* This rule tells how to die. */

die :-
        !, finish.


/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

finish :-
        nl,
        write('The game is over. Please enter the halt. command.'),
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
        write('halt.              -- to end the game and quit.'), nl,
        write('i.                 -- to check your inventory.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(outside_stadium) :-
    write('You are outside the stadium'), nl,
    write('To the north is the ticket booth.'), nl,
    write('To the west is the entrance to the stadium.'), nl.

describe(ticket_booth) :-
    write('You are in the ticket booth.'), nl,
    write('Purchase your tickets here.'), nl,
    write('Then head south back outside the stadium.'), nl.

describe(gate6) :- 
    write('You are outside of Gate 6,'), nl,
    write('the main entrance to the stadium.'), nl,
    write('Go north to enter through the gate,'), nl,
    write('into the main concourse.'), nl,
    write('To the east is back outside the stadium'), nl.

describe(m_concourse) :- 
    write('You are now in the main concourse.'), nl,
    write('To the west are the bathrooms.'), nl,
    write('To the north are seats.'), nl,
    write('To the east is an escalator.'), nl.

describe(bathroom) :-
    write('Do you need to use the facilities?'), nl,
    write('You are now in the bathroom.'), nl,
    write('To the west is the atm and'), nl,
    write('to the east is the main concourse.'), nl.

describe(atm) :-
    write('Welcome to the atm machine at the stadium,'), nl,
    write('Here you can take out money if you need'), nl,
    write('To the east you can head back to the bathroom'), nl.

describe(seats1) :- 
    write('These are not your seats!'), nl,
    write('Go back south to the main concourse.'), nl.

describe(escalator1) :-
    write('You just got off the escalator.'), nl,
    write('To the north is the small concourse.'), nl,
    write('To the west is the main concourse again'), nl.

describe(s_concourse) :- 
    write('You are in the small concourse.'), nl,
    write('To the north are more seats.'), nl,
    write('To the west are concession stands.'), nl,
    write('To the south is the first escalator'), nl,
    write('Going east will bring you up the second escalator.'), nl.

describe(seats2) :- 
    write('These are not your seats!'), nl,
    write('Go back south to the small concourse.'), nl.

describe(concessions) :-
    holding(money),
    write('You are at the concession stands where'), nl,
    write('you can buy popcorn to enjoy during the game.'), nl,
    write('Going east will bring you back to the small concourse.'), nl.

describe(escalator2) :-
    write('You just got off the second escalator.'), nl,
    write('To the north is another section of seats.'), nl,
    write('To the east is an emergency exit'), nl,
    write('Going back west will bring you back to the small concourse'), nl.

describe(emergency_exit) :-
    write('You just exited the stadium.'), nl,
    write('You lose'), nl,
    die.

describe(seats3) :-
    holding(popcorn),
    write('You have arrived at your seats'), nl,
    write('Congrats! You win!'), nl,
    finish, !.

describe(seats3) :-
    write('These are your seats but'), nl,
    write('you do not have your popcorn'), nl,
    write('Go south to get back to the third escalator'), 
    nl.
