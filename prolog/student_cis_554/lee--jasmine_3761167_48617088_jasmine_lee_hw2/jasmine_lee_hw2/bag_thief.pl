/* BEVERLY HILLS THIEF -- an adventure game by Jasmine Lee. */

:- dynamic at/2, i_am_at/1, asleep/1, visible_object/1, holding/1, fed/1, battery_life/1, hammer_count/1.   

/* Needed by SWI-Prolog. */

:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(visible_object(_)), retractall(holding(_)), 
   retractall(battery_life(_)), retractall(hammer_count(_)).

/* This defines my current location. */

i_am_at(garage).

/* set up facts specific to this game */

visible_object(flashlight).
visible_object(meatloaf).
battery_life(4).
hammer_count(0).

/* These facts describe how the rooms are connected. */

path(garage, n, kitchen).
path(kitchen, s, garage).

path(kitchen, u, fridge).
path(fridge, d, kitchen).

path(kitchen, w, living_room).
path(living_room, e, kitchen).

path(living_room, s, lobby).
path(lobby, n, living_room).

path(garage, w, lobby).
path(lobby, e, garage).

path(lobby, s, study).
path(study, n, lobby).

path(lobby, u, hallway).
path(hallway, d, lobby).

path(hallway, n, closet) :- fed(bambi).
path(hallway, n, closet) :- write('You notice that Paris''s precious chihuahua Bambi stands in front of the entrance to the room, glaring at you with its hungry eyes. You better leave now before it barks and wakes the owner up.'), nl, nl,
        !, fail.
path(closet, s, hallway).

path(hallway, s, bedroom).
path(bedroom, n, hallway).

path(hallway, e, clothes_closet).
path(clothes_closet, w, hallway).

/* These facts tell where the various objects in the game
   are located. */

at(flashlight, living_room).
at(nightgoggles, study).
at(carkey, bedroom).
at(meatloaf, fridge).
at(handbag, closet).
at(hammer, lobby).

/* These rules describe how to pick up an object. */

take(fridge) :-
        i_am_at(kitchen),
        write('It''s too heavy to lift.'), nl,
	!, fail.

take(box) :-
        i_am_at(closet),
        write('It is firmly nailed to the ground!'), nl,
	!, fail.

take(X) :-
        at(X, in_bag),
        write('You already have it!'),
        nl, !.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(at(X, in_bag)),
        write('OK.'),
        nl, !.

take(_) :-
        write('I don''t see it here.'),
        nl, !, fail.


/* These rules describe how to put down an object. */

drop(X) :-
        at(X, in_bag),
        i_am_at(Place),
        retract(at(X, in_bag)),
        update_holding(X),
        assert(at(X, Place)),
        write('OK.'),
        nl, !.

drop(_) :-
        write('You don''t have it!'),
        nl, !, fail.

update_holding(X) :-
        \+ holding(X).

update_holding(X) :-
        holding(X),
        retract(holding(X)).


/* These rules define the six direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).

u :- go(u).

d :- go(d).


/* This rule tells how to move in a given direction. */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
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
        visible_object(X),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).

/* Tells what the player has in the bag. */

inventory :-
        at(X, in_bag),
        write('There is a '), write(X), write(' in the bag.'), 
        nl, fail.

/* Tells what the player is holding */

using :-
        holding(X),
        write('You are holding the '), write(X), write('.'),
        nl, fail.

/* These rules are specific to this game. */

use(flashlight) :-
        \+ battery_life(0),
        at(flashlight, in_bag),
        hold(flashlight),
        lower_battery_life,
        make_visible(nightgoggles),
        make_visible(hammer),
        make_invisible(carkey),
        flashlight_on, !.

use(flashlight) :-
        at(flashlight, in_bag),
        hold(flashlight),
        make_invisible(nightgoggles),
        make_invisible(hammer),
        write('Your flashlight is out of batteries.'), nl, !, fail.

use(nightgoggles) :-
        at(nightgoggles, in_bag),
        hold(nightgoggles), 
        make_visible(carkey),
        make_invisible(hammer),
        goggles_on, !.
        
use(fridge) :-
        i_am_at(kitchen), !, 
        u.

use(fridge) :- 
        write('The fridge is in the kitchen.'), nl, !, fail.

use(hammer) :- 
        at(hammer, in_bag),
        hold(hammer),
        i_am_at(closet),
        bump_hammer_count,
        hit, !.

use(meatloaf) :-
        at(meatloaf, in_bag),
        i_am_at(hallway),
        assert(fed(bambi)), !.

use(carkey) :-
        at(carkey, in_bag),
        i_am_at(garage),
        write('Congratulations!'), nl,
	write('You have successfully completed the mission by stealing the hand bag'), nl, 
	write('and discovering the car keys to the Bentley.'), nl,        !, 
	write('You smirk under your breath as the engine turns on with a roar.'), nl,
	write('You drive through the garage door into the beautiful Hollywood sunrise.'), nl, finish.

use(X) :-
        at(X, in_bag), 
        hold(X),
        !.

use(_) :-
        write('You can''t use this item.'), nl, !, fail.

hit :- 
        hammer_count(3),
        make_visible(handbag),
        write('You slam the hammer one last time against the box which breaks into pieces.'), nl,
        write('Right at the center of the box, you spot the limited edition Hermes handbag!'), nl.

hit :- 
        hammer_count(2),
        write('You hit the box with the hammer once more, making a crack on the right side of the box. Any time now.'),
        nl.

hit :- 
        hammer_count(1),
        write('You swing the hammer behind you and hit the box hard. This leaves behind a crack on the left.'), nl.

hit :- 
        write('The box has already been broken.'), nl, !, fail.

flashlight_on :- 
        i_am_at(study),
        \+ at(nightgoggles, in_bag),
        write('You spot some nightgoggles on the desk.'), nl.

flashlight_on :- 
        i_am_at(bedroom),
        write('Your flashlight hits Paris''s sleepy face.'), nl,
        write('She detects your presence and screams at the top of her lungs!!'), nl,
        finish.

flashlight_on :-
        i_am_at(closet),
        \+ at(handbag, in_bag),
        write('Your flashlight shines across a mysterious wooden box.'), nl.

flashlight_on :-
        i_am_at(lobby),
        \+ at(hammer, in_bag),
        write('You spot a hammer.'), nl.

flashlight_on.

goggles_on :-
        i_am_at(bedroom),
        \+ at('access key', in_bag),
        write('You spot an access key under the bed.'), nl.

goggles_on :- !.

hold(X) :- holding(X).

hold(X) :-
        retractall(holding(_)),
        assert(holding(X)).

exit :-
        i_am_at(fridge), !,
        d.

exit :- 
        write('Please specify which direction you''d like to go.').

bump_hammer_count :-
        retract(hammer_count(X)),
        Y is X + 1,
        assert(hammer_count(Y)).

lower_battery_life :-
        retract(battery_life(X)),
        Y is X - 1,
        assert(battery_life(Y)).

make_visible(X) :-
	visible_object(X).

make_visible(X) :-
	assert(visible_object(X)).

make_invisible(X) :- \+ visible_object(X).

make_invisible(X) :-
        retract(visible_object(X)).
        
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
        write('use(Object).             -- to use an object. Note: You can only use one object at any given time.'), nl,
        write('drop(Object).            -- to put down an object.'), nl,
        write('inventory.               -- to see what''s currently in your bag.'), nl,
        write('using.                   -- to see what you are currently holding.'), nl,
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

describe(garage) :-
	at(carkey, in_bag),
        at(handbag, in_bag),
        write('You have returned to the garage with the stolen hand bag,'), nl,
	write('and the carkey. Time to leave this house!'), nl, !.

describe(garage) :-
	\+ at(carkey, in_bag),
        at(handbag, in_bag),
        write('You are back in the garage with the stolen hand bag,'), nl,
        write('To the north is an unlocked door to the kitchen;'), nl, 
	write('to the west is the entrance to the lobby of the mansion'), nl,
	write('but you must also fetch the car keys before you can leave.'), nl, !.

describe(garage) :-
	\+ at(handbag, in_bag),
        at(carkey, in_bag),
        write('You are back in the garage with the carkey.'), nl,
        write('To the north is an unlocked door to the kitchen;'), nl, 
	write('to the west is the entrance to the lobby of the mansion'), nl,
	write('But wait - you forgot the most important thing - to steal the hand bag!'), nl, !.

describe(garage) :-
        write('You stand next to Paris Hilton''s pink studded Bentley car in the garage at 5 in the morning.'), nl,
	write('To the north is an unlocked door to the kitchen;'), nl, 
	write('to the west is the entrance to the lobby of the mansion'), nl,
	write('And how did you end up here, you ask?'), nl,
	write('You are the stealthiest thief in all of Beverly Hills.'), nl,
	write('Tonight you have decided to undertake one of the riskiest projects yet,'), nl,
	write('which is to steal Paris Hilton''s most expensive handbag.'), nl,
	write('Your mission, should you accept it, is to find and take Paris''s'), nl,
	write('limited edition Hermes Birkin hand bag, then return to the garage'), nl,
	write('with her car keys so that you escape fashionably.'), nl.

describe(kitchen) :-
	write('You are in the kitchen. The garage door is to the south;'), nl,
	write('The living room is to the west.'), nl,
	write('You look around and see the most enormous fridge you''ve seen in your whole life.'), nl,
	write('You''d be able to feed the whole city with the amount of food in this fridge, you thought to yourself.'), nl.

describe(fridge) :-
	write('You are inside the fridge. Type exit if you want to close the fridge.'), nl.

describe(living_room) :-
	write('You are in the living room. The kitchen is to the east;'), nl,
	write('The lobby is to the south.'), nl.

describe(lobby) :-
        write('You are in the lobby. The living room is to the north;'), nl,
	write('the garage is to the east; and the study is to the south.'), nl,
	write('An elegant spiral staircase leads directly to the second floor'), nl.

describe(study) :-
        write('You are in the study. The lobby is to the north.'), nl,
	write('You wonder why Paris would ever need a study room.'), nl,
	write('Does she even read?'), nl.

describe(hallway) :-
        write('You are in the second floor hallway.'), nl,
	write('To the north is the entrance to a room;'), nl,
	write('To the east is the walk-in closet;'), nl,
	write('To the south is Paris''s bedroom.'), nl.

describe(bedroom) :-
	holding(flashlight),
        \+ battery_life(0),
        write('Your blinding flashlight hits Paris''s sleepy face.'), nl,
        write('She detects your presence and screams at the top of her lungs!!'), nl, !.

describe(bedroom) :-
	holding(nightgoggles),
        write('You are inside the bedroom.'), nl,
	write('You put on the nightgoggles and tip-toe quietly across the room.'), nl, !.

describe(bedroom) :-
        write('You are inside the bedroom.'), nl,
	write('It is pitch dark and you can barely see anything.'), nl,
	write('As you make your way across the room, you trip over'), nl,
	write('a pair of heels and fall flat on your face.'), nl,
        write('Oh no, how could you be so clumsy?'), nl,
	write('The sound startles Paris and she wakes up with a scream!!'), nl,
	finish.

describe(closet) :-
        holding(flashlight),
        \+ battery_life(0),
        write('You are in an unfurnished closet. The hallway is to the south.'), nl, !.

describe(closet) :-
	write('You are inside a dark closet. The hallway is to the south.'), nl,
	write('You suspect there might be something in the far corner,'), nl,
	write('but you can''t quite make out what it is...'), nl.

describe(clothes_closet) :-
        holding(flashlight),
        \+ battery_life(0),
        write('You are in the walk-in closet. The exit is to the west.'), nl,
	write('The two sides of the room are lined with shelves full of clothing'), nl,
	write('in every color and style anyone could ever dream of.'), nl,
	write('If there was ever an earthquake, the clothes in this room would most certainly bury someone alive.'), nl, !.

describe(clothes_closet) :-
	write('You are in the walk-in closet. The exit is to the west.'), nl.
