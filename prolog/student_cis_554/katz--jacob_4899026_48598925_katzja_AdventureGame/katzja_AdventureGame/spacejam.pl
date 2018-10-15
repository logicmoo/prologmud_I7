/* SpaceJam, by Jacob Katz. CIS 554 Fall 2014. */

:- dynamic i_am_at/1, at/2, holding/1, count/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

i_am_at(atrium).

path(classroom, s, atrium).

path(atrium, n, classroom).
path(atrium, w, lockerroom).
path(atrium, e, gym) :- holding(full_secret_stuff).
path(atrium, e, gym) :- holding(empty_secret_stuff),
        write('You need to fill your secret stuff'), nl,
        write('bottle before you can drink it!'), nl,
        write('Go fill the secret stuff in the bathroom sink '),nl,
        write('and then come win the game!'), nl, !.
path(atrium, e, gym) :- write('You can''t go into the gym without your'), nl,
                        write('secret stuff! If you do, you will lose!'), nl,
                        write('Go find your secret stuff, fill it up'), nl,
                        write('in the sink, and then come back to help'), nl,
                        write('win the game!'), nl, !, fail.

path(gym, w, atrium).

path(lockerroom, e, atrium).
path(lockerroom, s, bathroom_sink).
path(lockerroom, w, locker) :- holding(combination), !.

path(locker, e, lockerroom).

path(bathroom_sink, n, lockerroom).

at(combination, classroom).
at(empty_secret_stuff, locker).
at(full_secret_stuff, bathroom_sink).
at(game, gym).

/*Start timer at 16 moves, which is 2x the most optimal path*/

count(16).

decrement :- count(CurTime),
            NewTime is CurTime-1,
            retract(count(CurTime)),
            assert(count(NewTime)), !.

/* These rules describe how to pick up an object. */

take(full_secret_stuff) :-
        i_am_at(bathroom_sink),
        at(full_secret_stuff, bathroom_sink),
        !, take_full_secret_stuff.

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

take_full_secret_stuff :-
        i_am_at(bathroom_sink),
        at(full_secret_stuff, bathroom_sink),
        holding(empty_secret_stuff),
        retract(at(full_secret_stuff, bathroom_sink)),
        assert(holding(full_secret_stuff)),
        write('OK.'), !, nl.

take_full_secret_stuff :-
        write('You need the empty secret stuff bottle!'), nl, fail.


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


inventory :- what_are_you_holding(_).

what_are_you_holding(Obj) :- holding(Obj),
                            write('Holding: ' ),write(Obj),nl,fail.

what_are_you_holding(_).


/* This rule tells how to move in a given direction. */

go(Direction) :-
        count(X),
        X>=0,
        i_am_at(atrium),
        holding(full_secret_stuff),
        path(atrium, Direction, gym),
        retract(i_am_at(atrium)),
        assert(i_am_at(gym)),!,
        win.

go(Direction) :-
        count(X),
        X>=0,
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        !,
        decrement,
        look, nl,
        write('You have '), write(X), write(' moves until it is too late!'),nl.

go(_) :-
        count(X),
        X<0, !,
        write('Unfortunately you didn''t make it in time! :('),nl,
        write('Your team lost the game because you were too slow.'), nl,
        write('Maybe next time!'),
        die.

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

/* This rule tells how to win. */

win :-
        champion.


/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

finish :-
        nl,
        write('The game is over. Please enter the "halt." command.'),
        nl.

champion :-
        nl,
        write('CONGRATS! You made it to the big game with your secret stuff!'),nl,
        write('Now you will win the big game just as you won this adventure.'), nl,
        write('The game is now over. Please enter the "halt." command.'), nl.

/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             -- to start the game.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction.'), nl,
        write('i                  -- inventory.'),nl,
        write('take(Object).      -- to pick up an object.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look,
        count(X),
        write('You have '), write(X), write(' moves to finish!').


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(atrium) :- write('You are at the atrium.'), nl,
                    write('To the north is the classroom.'), nl,
                    write('To the west is the lockerroom.'), nl,
                    write('To the east is the gym, where you can '), nl,
                    write('go win the game for the team; but you won''t '), nl,
                    write('be able to unless you can find your '), nl,
                    write('secret stuff, fill it up, and get to the gym. '), nl.

describe(classroom) :- write('You are in the classroom right now.'), nl.

describe(gym) :-
        holding(full_secret_stuff),
        write('You finally arrived with your secret stuff.'), nl,
        write('Let''s win this game now!'), nl,
        finish, !.

describe(bathroom_sink) :-
        holding(empty_secret_stuff),!,
        write('Now that you have your empty secret stuff bottle,'), nl,
        write('you can fill it up here! After you''ve done that'), nl,
        write('head over to the gym and help win this game!'), nl.

describe(bathroom_sink) :-
        holding(full_secret_stuff),!,
        write('You already have the full secret stuff! Get to'), nl,
        write('the gym as fast as you can so you can successfully'), nl,
        write('win the game!'), nl.

describe(bathroom_sink) :-
        write('You need to get your secret stuff bottle so'), nl,
        write('you can fill it up here. Until then, nothing for'), nl,
        write('you to do here! '), nl,
        write('To return to the lockerroom, go NORTH.'), nl.

describe(lockerroom) :-
        holding(combination),!,
        write('Now that you have the locker combination, you can'), nl,
        write('go to the WEST and access the locker.'), nl,
        write('To the SOUTH is the bathroom sink where, once'), nl,
        write('you have the secret stuff bottle, you can'), nl,
        write('fill it up and go win the game!'), nl.

describe(lockerroom) :-
        write('You''re in the locker room, you need a'), nl,
        write('locker combination to access a locker.'), nl,
        write('To the SOUTH is the bathroom sink where, once'), nl,
        write('you have the secret stuff bottle, you can'), nl,
        write('fill it up and go win the game!'), nl,!.

describe(lockerroom) :-
        write('You''re in the locker room, you need a'), nl,
        write('locker combination to access a locker.'), nl,
        write('To the SOUTH is the bathroom sink where, once'), nl,
        write('you have the secret stuff bottle, you can'), nl,
        write('fill it up and go win the game!'), nl,!.

describe(locker) :-
        write('You made it to the locker!'), nl,
        write('Now you can get the secret stuff bottle,'), nl,
        write('but be aware that you still have to'), nl,
        write('fill it up in the bathroom sink!'), nl.
