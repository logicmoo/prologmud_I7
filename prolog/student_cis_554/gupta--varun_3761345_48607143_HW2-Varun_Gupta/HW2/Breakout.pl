/* BreakOut -- a sample adventure game, by Varun Gupta.
   Consult this file and issue the command:   start.  */

:- dynamic at/2, i_am_at/1, alive/1.   /* Needed by SWI-Prolog. */
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

/* This defines my current location. */

i_am_at(cell).


/* These facts describe how the rooms are connected. */

path(cell, d, hole).

path(hole, u, cell).
path(cell, w, hallway) :- at(key, in_hand).
path(cell, w, hallway) :-
        write('Cell door is locked. Look for a key. '),
        !, fail.

path(hallway, n, kitchen).
path(kitchen, s, hallway).
path(hallway, s, rec_room).
path(rec_room, n, hallway).
path(hallway, w, wardens_office).
path(wardens_office, e, hallway).
path(rec_room, w, entrance).
path(entrance, e, rec_room).
path(entrance, w, outside).

/* These facts tell where the various objects in the game
   are located. */

at(gun, rec_room).
at(key, hole).
at(food, kitchen).
at(bullets, wardens_office).
at(25, health).


/* This fact specifies that the warden is alive. */

alive(warden).

/* These rules describe how to pick up an object. */

take(X) :-
        at(X, in_hand),
        write('You''re already holding it!'),
        nl, !.

take(X) :-
        i_am_at(wardens_office),
        at(gun, in_hand),
        retract(at(gun, in_hand)),
        retract(at(X, wardens_office)),
        assert(at(gun_bullets, in_hand)),
        write('loaded your gun with bullets'),
        nl, !.

take(X) :-
        i_am_at(rec_room),
        at(bullets, in_hand),
        retract(at(X, rec_room)),
        retract(at(bullets, in_hand)),
        assert(at(gun_bullets, in_hand)),
        write('loaded the gun with your bullets'),
        nl, !.

take(X) :-
        i_am_at(kitchen),
        retract(at(X, kitchen)),
        at(Y, health),
        retract(at(Y, health)),
        assert(at(100, health)),
        write('You found food! You\'re health is back to 100 percent.'),
        nl, !.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        (  at(Y, in_hand) ->
            retract(at(Y, in_hand)),
            assert(at(X, in_hand))  
        ;
            assert(at(X, in_hand))  
        ),
        write('OK.'),
        nl, !.

take(_) :-
        write('I don''t see it here.'),
        nl.


/* These rules define the six direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).

u :- go(u).

d :- go(d).

/* This rule tells whats in your inventory. */

i :- 
        nl,
        (  at(X, in_hand) ->
             write(X)
        ;
             writeln('Nothing in hand.')
        ),
        nl.
   
/* This rule tells your current health */

health :-
        nl,
        at(X, health),
        write(X),
        nl.

/* This rule tells how to move in a given direction. */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        at(X, health),
        retract(at(X, health)),
        Y is X - 5,
        assert(at(Y, health)),

        (   Y is 0 ->
                write('You passed out from exhaustion and hunger.'),
                nl, die
        ;
                write('Youre health is now '), write(Y), nl,
                assert(i_am_at(There)),
                look, !
        ).

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


/* These rules tell how to handle killing the warden. */

kill :-
        i_am_at(entrance),
        at(gun_bullets, in_hand),
        retract(alive(warden)),
        write('You and the warden both draw'), nl,
        write('but you are too quick as you pull out your gun'), nl,
        write('and quickly shoot him down.'),
        nl, !.

kill :-
        write('I see nothing to kill here.'), nl.


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
        write('n.  s.  e.  w.  u.  d.   -- to go in that direction.'), nl,
        write('take(Object).            -- to pick up an object.'), nl,
        write('kill.                    -- to attack an enemy.'), nl,
        write('look.                    -- to look around you again.'), nl,
        write('instructions.            -- to see this message again.'), nl,
        write('halt.                    -- to end the game and quit.'), nl,
        write('health.                  -- to check your health.'), nl,
        write('i.                       -- to check your inventory.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.

/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(cell) :-
    at(X, in_hand),
    write('The cell door is to the west and locked.'), nl.

describe(cell) :-
        write('You are a prisoner in a maximum security prison.'), nl,
        write('You are currently in your cell.'), nl,
        write('Your mission is to escape with your life.'), nl,
        write('You are famished as the guards have not fed you for days'), nl,
        write('and you will pass out soon.'), nl,
        write('The cell door is to the west which leads to the hallway'), nl,
        write('There also seems to be a small crawlspace beneath(down) you.'), nl.

describe(hole) :-
        write('You are in a small crawlspace. Above(up) is your cell.'), nl.

describe(hallway) :-
        write('You are in a hallway.  To the north is a kitchen'), nl,
        write('To the south, there is a rec_room'), nl,
        write('Finally, to the west, there is there warden\'s office.'), nl.

describe(kitchen) :-
        write('You are in the kitchen'), nl.

describe(rec_room) :-
        write('This is the rec_room.'), nl,
        write('The prison entrance is to the west.'), nl.

describe(wardens_office) :-
        write('You are in the wardens office but he seems to be away.'), nl.

describe(entrance) :-
        alive(warden),
        at(gun_bullets, in_hand),
        write('You can see the exit to the west.'), nl,
        write('But the warden has spotted you! Protect yourself!'), nl.

describe(entrance) :-
        alive(warden),
        write('You can see the exit to the west.'), nl,
        write('But the warden has spotted you and shot you down.'), nl,
        die.

describe(entrance) :-
        write('The warden is dead. Flee to freedom!'), nl.

describe(outside) :-
        write('You have escaped!! Congrats!'), nl,
        finish, !.
