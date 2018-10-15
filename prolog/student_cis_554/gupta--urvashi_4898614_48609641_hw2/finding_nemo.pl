/* FINDING NEMO :: A Game by Urvashi Gupta
   Issue the command:   start.  */

:- dynamic at/2, i_am_at/1, alive/1, timer/1, check_timer/1.   /* Needed by SWI-Prolog. */
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(timer(_)), retractall(check_timer(_)).

/* This defines my current location. */

i_am_at(reef).


/* These facts describe how the ocean is connected. */

path(reef, w, beach).
path(beach, e, reef).

path(reef, e, midocean).
path(midocean, w, reef).

path(reef, n, ocean_surface).
path(ocean_surface, s, reef).

path(ocean_surface, n, ocean) :- at(dory, in_hand).
path(ocean_surface, n, ocean) :-
        write('Sorry! The whales can only speak to Dory! Better find her!'), nl,
        !, fail.
path(ocean, s, ocean_surface).

path(reef, s, deep_sea) :- at(seashell, in_hand), at(stick, in_hand).
path(reef, s, deep_sea) :-
        write('The corals are covering the deep sea.'), nl,
        write('They only move when you tune in to Seashells and Sticks!'), nl,
        fail.
path(deep_sea, n, reef).

path(ocean, w, green_sea).
path(green_sea, e, ocean).

path(ocean, e, red_sea).
path(red_sea, w, ocean).

path(red_sea, n, greenlands).
path(greenlands, s, red_sea).

path(red_sea, e, junkyard) :- at(leaves, in_hand).
path(red_sea, e, junkyard) :-
        write('Can''t enter the junkyard!'), nl,
        write('Its super dirty here! Find something to clean it up!'), nl,
        !, fail.
path(junkyard, w, red_sea).

path(red_sea, s, skylands).
path(skylands, n, red_sea).

path(junkyard, clean, toothyard).
path(toothyard, w, junkyard).
path(green_sea, cut, green_sea_surface) :- at(tooth, in_hand).
path(green_sea, cut, green_sea_surface) :-
        write('Sorry! We need to find something to cut this net fast! '), nl,
        !, fail.
path(green_sea_surface, e, green_sea).


/* These facts tell where the various objects in the game
   are located. */

at(stick, ocean_surface).
at(seashell, beach).
at(dory, deep_sea).
at(nemo, green_sea_surface).
at(leaves, greenlands).
at(tooth, toothyard).


/* This fact mentions the total time a player has
before his game finishes */

timer(300).

check_timer(A):- A \= 0.


/* These rules display the amount of time left to play the game. */

time :-
        timer(X),
        write('You have '),
	write(X),
	write(' seconds left.'),
        nl, !.


/* These rules describe how to pick up an object. */

take(X) :-
        at(X, in_hand),
        write('One at a time. You''re already holding the object!'),
        nl, !.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(at(X, in_hand)),
        write('OK.'),
        nl, !.

take(_) :-
        write('I can''t find the object you are trying to take.'),
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
        write('Don''t drop things you aren''t holding!'),
        nl.


/* These rules define the four direction letters as calls to go/1
and allows the players to cut and clean */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).

clean :- go(clean).

cut :- go(cut).


/* This rule tells how to move in a given direction. */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        timer(OldTime),
        check_timer(OldTime), !,
	NewTime is OldTime - 10,
	retract(timer(OldTime)),
	assert(timer(NewTime)),
        look, !.

go(_) :-
        write('There is no point in using this command right now.').


/* This rule tells how to look around you. */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl.

/* This rule displays all the items in your inventory. */


i :-
	write('We found these items in your inventory: '), nl,
	notice_objects_at(in_hand),
	time,
        nl.

inventory :-
	write('We found these items in your inventory: '), nl,
	notice_objects_at(in_hand),
	time,
	nl.


/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
        at(X, Place),
        write('FOUND :: '), write(X), nl,
        fail.

notice_objects_at(_).


/* This rule tells how to die. */

die :-
        !, finish.


/* Under UNIX, the halt.  command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final  halt.  */

finish :-
        nl,
        write('The game is over. Please enter the ''halt.''  command.'),
        nl, !.


/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.                   -- to start the game.'), nl,
        write('n.  s.  e.  w.           -- to go in that direction.'), nl,
        write('take(Object).            -- to pick up an object.'), nl,
        write('drop(Object).            -- to put down an object.'), nl,
        write('clean.                   -- to clean objects.'), nl,
        write('cut.                     -- to cut objects'), nl,
        write('look.                    -- to look around you again.'), nl,
        write('i.                       -- to see the objects currently held.'), nl,
	write('inventory.		   -- to see the objects currently held.'), nl,
	write('time.                    -- to check the amount of time left.'), nl,
        write('instructions.            -- to see this message again.'), nl,
        write('halt.                    -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.


/* These rules describe the various ocean areas.  Depending on
   circumstances, a room may have more than one description. */

describe(_) :-
	timer(OldTime),
	\+ check_timer(OldTime),
	write('Oh nooo! Time is up! Nemo is gone! :''('), nl,
	write('You can still use basic commands but what''s the point of moving around anymore? :('), nl,
	finish, !.


describe(reef) :-
        at(nemo, in_hand),
        write('Nemo is back home! You and Dory make a good team! Don''t lose him again. '), nl,
        finish, !.

describe(reef) :-
        write('You are in a small reef. To the north is the ocean surface'), nl,
        write('with whales; to the south is the closed deep blue sea of'), nl,
        write('corals; to the west is the beautiful beach; to the east is the mid ocean.'), nl,
        write('You must find nemo quickly before the fishermen take '), nl,
        write('him away. Hurry! '), nl.

describe(beach) :-
        write('Welcome to the sunny beach! The reef is to the east.'), nl,
        write('Stay here and listen to some tunes played by our very own-'), nl,
        write('Seashells and Sticks Band! Remember that seashells are incomplete without sticks.'), nl.

describe(ocean_surface) :-
        write('Welcome to the ocean surface! Go higher to reach the whales. '), nl,
        write('Go south to reach the reef again.'), nl,
        write('Remember that sticks are incomplete without seashells. '), nl.

describe(ocean) :-
        at(dory, in_hand),
        write('Whale: Yeaayy! I can understand where you need to go now! '), nl,
        write('Whale: Lets GOOOOOOO!!! I will drop you to the write place.'), nl,
        write('Go west to reach the green sea;'), nl,
        write('go east to reach the dark red sea.'), nl,
        write('go south to reach the reef again.'), nl.

describe(ocean) :-
    write('welcome to Ocean surface! Sorry! The whales can only speak to Dory! Better find her!'), nl.

describe(deep_sea) :-
        at(seashell, in_hand),
        at(stick, in_hand),
        write('The corals have moved. Swim south and wake up Dory!'), nl,
        write('You are close to finding Nemo!'), nl.

describe(deep_sea) :-
        write('Yes! You have found dory!'), nl,
        write('Go north to reach the reef.'), nl.

describe(green_sea) :- at(tooth, in_hand), at(nemo, in_hand),
        write('Welcome to the Green Sea!'), nl.

describe(green_sea) :- at(tooth, in_hand),
        write('Awesome! You found the golden tooth!'), nl,
        write('Lets CUT and free Nemo!'), nl.

describe(green_sea) :-
        write('Welcome to the Green Sea!'), nl,
        write('The fishermen are closing in. Nemo is trapped in a net!'), nl,
        write('Find a sharp object to cut the net! Fast!'), nl,
        write('Go east to reach the ocean again.'), nl.

describe(red_sea) :-
        write('Welcome to the Red Sea!'), nl,
        write('Go East for the junkyard place, full of broken plastic and sharks teeth!'), nl,
        write('Go north for the Greenlands; south for the SkyLands; west for the ocean'), nl.

describe(toothyard) :-
        write('You have uncovered the golden tooth!'), nl,
        write('Go west to go back to the junkyard and save nemo!'), nl.


describe(junkyard) :- at(leaves, in_hand), at(tooth, in_hand),
        write('Welcome to the junkyard!'), nl,
        write('Go west to reach the red sea.'), nl.

describe(junkyard) :- at(leaves, in_hand),
        write('Awesome!'), nl,
        write('You found something to CLEAN up this junk! Lets look for that tooth!'), nl.

describe(junkyard) :-
        write('Welcome to the junkyard!'), nl.

describe(greenlands) :-
        write('Welcome to the GreenLands!'), nl,
        write('Go south to go back to the red sea!'), nl.


describe(skylands) :-
        write('Welcome to the SkyLands!'), nl,
        write('Land of crocodiles! Haahahahha! Runnnn!'), nl.

describe(midocean) :-
        write('Welcome to the Mid Ocean Wave!'), nl,
        write('The Jellyfish Land! Be careful! They sting! A LOT!!'), nl.

 describe(green_sea_surface) :-
        write('Found Nemo! Lets take him back home!'), nl,
        write('Go east to reach the reef.'), nl.
