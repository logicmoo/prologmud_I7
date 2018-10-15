/* A Doha Dilemma (based on a true story), by Rigel. */

:- dynamic i_am_at/1, at/2, holding/1, examined/1, time/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

i_am_at(restaurant).
time(30).

path(restaurant, n, bathroom).
path(bathroom, s, restaurant).
path(restaurant, s, outside).
path(outside, n, restaurant).
path(outside, s, lobby).
path(outside, w, street).
path(street, e, outside).
path(street, n, taxi) :- holding(wallet), \+ holding(few_dollars), 
        write('Your wallet is empty. You need money to take a taxi!'), nl,
        !, fail.
path(street, n, taxi) :- \+ holding(wallet),
        write('You don''t have your wallet - how do you plan on taking a taxi?'), nl,
        !, fail.
path(street, n, taxi) :- holding(few_dollars).
path(lobby, n, outside).
path(lobby, w, outside_room).
path(lobby, e, front_desk).
path(front_desk, w, lobby).
path(outside_room, s, lobby).
path(outside_room, n, room) :- holding(key_card).
path(outside_room, n, room) :-
        write('You lost your key card! Better go get a new one.'), nl,
        !, fail.
path(room,s,outside_room).
path(taxi, n, gig).

inside(tissue_box, few_dollars).

at(tissue_box, bathroom).
at(key_card, front_desk).
at(wallet, room).
at(trumpet, room).

in(tissue_box, few_dollars).

requires(few_dollars, wallet).

examinable(tissue_box).


/* This rule describes how you decrement the time you have left. */

decr_time(X, X1) :-
    time(X),
    X1 is X-1,
    X1 is 1, 
    write('You have '), write(X1), write(' minute left.'), nl.

decr_time(X, X1) :-
    time(X),
    X1 is X-1,
    write('You have '), write(X1), write(' minutes left.'), nl.


/* These rules describe how to pick up an object. */

take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        requires(X,Y), \+ holding(Y),
        write('You can''t take '), write(X), write(' yet - you need a '), write(Y), write('.'),
        !, nl.

take(X) :-
        examinable(X),
        write('Why would you want to take that?'), !, nl.

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

/* These rules describe how to check your inventory. */

i :- write('Inventory:'), nl,
     holding(X),
     write(X), nl,
     fail.

i :- \+ holding(_), write('Your inventory is empty.'), !, nl.

i.
    

/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).


/* This rule tells how to move in a given direction. */

go(_) :-
        time(X),
        X is 0,
        die, !.

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        time(X),
        decr_time(X, Y),
        retract((time(X))),
        assert(time(Y)),
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


/* This rule describes how to examine an object */

examine(X) :- examinable(X),
              assert(examined(X)),
              in(X, Y),
              write('You found a '), write(Y), write(' in the '), write(X), write('!'), nl,
              i_am_at(Place),
              assert(at(Y,Place)),
              !, nl.

examine(X) :- examinable(X),
              assert(examined(X)),
              write('There''s nothing special about '), write(X), write('.'), !, nl.

examine(_) :- write('You can''t examine that.'), nl.


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
        write('examine(Object)    -- to examine an object.'), nl,
        write('i.                 -- to check your inventory.'), nl,
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

describe(restaurant) :- write('You are in a restaurant. To the north is the bathroom, to the south is the exit.'), nl,
                        write('You''re on tour with the Penn Glee Club in Doha right now, and you need to make it to your gig.'), nl,
                        write('You''re already late. Hurry up!'), nl.

describe(bathroom) :- write('You''re in the bathroom at the restaurant.'), nl,
                      write('To the south is the restaurant.'), nl.

describe(outside) :- write('You''re outside. To the north is the restaurant, to the south is the hotel, and to the west is the street.'), nl.

describe(lobby) :- write('You''re in the hotel lobby. To the west is the elevators, to the east is the front desk,'), nl,
                   write('and to the north is the exit.'), nl.

describe(front_desk) :- write('You''re at the front desk. To the west is the hotel lobby.'), nl.

describe(outside_room) :- write('You''re outside your hotel room. To the north is your hotel room, to the south are the elevators.'), nl.

describe(room) :- write('You''re in your hotel room. The door is to the south.'), nl,
                  write('Everybody has already left for the gig. Hurry!'), nl.

describe(street) :- write('You''re out by the street. Taxis are wizzing by to the north, and the outside of the restaurant is to the east.'), nl.

describe(taxi) :- write('You''re in a taxi. To the south is the street and the north is the gig.'), nl,
                  write('Do you have everything you need?'), nl.

describe(gig) :- holding(trumpet), nl,
                 write('You made it to the gig. Congratulations!'),
                 finish, !.

describe(gig) :- write('You didn''t bring your instrument, you idiot! What are you supposed to play?'), nl,
                 write('The rest of the band starts performing and you sit on the sidelines.'),
                 die, !.