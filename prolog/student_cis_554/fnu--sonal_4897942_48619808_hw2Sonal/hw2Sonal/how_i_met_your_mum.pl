/* How I met your Mum, by FNU Sonal Ektaa. */

:- dynamic i_am_at/1, at/2, holding/2, alive/1,energy/1, wearing/2.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

/* This defines the current location. */
i_am_at(home).
alive(barney).
energy(100).


/* These facts describe how to go from one place to another. */

path(home, d, mcLaren).
path(mcLaren, u, home).

path(closet, w, home).
path(chest, n, home).
path(home,s,chest).

path(home, e, closet) :- holding(key,in_hand).
path(home, e, closet) :-
        write('The door appears to be locked.'), nl,
	fail.

path(mcLaren, e, restaurant).
path(restaurant, w, mcLaren).

path(restaurant, s, barney).
path(barney, n, restaurant).

path(closet, e, drawer):-holding(suit,in_hand).
path(closet, e, drawer):-
	write('How do you pick a tie without a suit?'),nl,
	fail.
path(drawer,w,closet).
path(robins_house_enterance, w, restaurant).

path(robins_house_enterance, s, robins_house).

path(restaurant, e, robins_house_enterance):-
	holding(blue_french_horn,in_hand),
        wearing(suit,tie),
	holding(ring,in_hand).

path(restaurant, e, robins_house_enterance):-
	holding(blue_french_horn,in_hand),
        wearing(tie,suit),
	holding(ring,in_hand).

path(restaurant, e, robins_house_enterance):-
        holding(blue_french_horn,in_hand),
        wearing(suit,tie),
        write('What are you going to propose her without a ring!?'), nl,
        write('That''s a NO!'), nl,
        fail.

path(restaurant, e, robins_house_enterance):-
        holding(blue_french_horn,in_hand),
        wearing(tie,suit),
        write('What are you going to propose her without a ring!?'), nl,
        write('That''s a NO!'), nl,
        fail.

path(restaurant, e, robins_house_enterance):-
        holding(ring,in_hand),
	wearing(suit,tie),
        write('Thats not you Ted, there is something missing?'), nl,
        write('How about the blue french horn!'), nl,
        fail.

path(restaurant, e, robins_house_enterance):-
        holding(ring,in_hand),
	wearing(tie,suit),
        write('Thats not you Ted, there is something missing?'), nl,
        write('How about the blue french horn!'), nl,
        fail.


path(restaurant, e, robins_house_enterance):-
        holding(ring,in_hand),
	holding(blue_french_horn,in_hand),
        write('Where is the suit Mr.? Go get properly dressed'), nl,
        !,fail.

path(restaurant, e, robins_house_enterance):-
	write('Yes there is something called true love! But is that how you want to do it?'), nl,
        write('Gosh! Check!Do you have a ring? Are you wearing a suit with a matching tie? '),nl,
	write('What about the blue french horn? '), nl,
        fail.

at(energy_drink, mcLaren).
at(blue_french_horn, restaurant).
at(suit,closet).
at(key, chest).
at(ring, barney).
at(tie, drawer).


/* These rules describe how to pick up an object. */

check_energy(X):-
	X\=0.

energy_left:-
	energy(X),
	write('Energy left is: '),nl,
	write(X),nl.

take(X) :-
        holding(X,in_hand),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X,in_hand)),
        write('OK.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.


/* These rules describe how to put down an object. */

drop(X) :-
        holding(X,in_hand),
        i_am_at(Place),
        retract(holding(X,in_hand)),
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

go(Direction) :-
        i_am_at(Here),
	 path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
	energy(Initial),
	check_energy(Initial),!,
	Current is Initial-10,
	retract(energy(Initial)),
	assert(energy(Current)),
	energy_left,
	nl,
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

notice_inventory(X):-
	holding(F,X),
	write(F),nl,fail.


notice_inventory(_).


/* This rule tells how to die. */

die :-
       !, finish.
/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

finish :-
        nl,
        write('The game is over. Please enter the   halt.   command.'),
        nl, !.

hit :-
        i_am_at(barney),
        holding(blue_french_horn,in_hand),
        retract(alive(barney)),
        write('You hit barney with the blue french horn....'), nl,
        write('he seems unconcious.'), nl,
	write('OMG!lets get out of the restaurant from the north.'),
        nl, !.

hit :-
        i_am_at(barney),
        write('Barney is stronger than you! Punching him won''t help!'), nl.
hit :-
        write('I see nothing to hit here.'), nl.


wear(X,Y) :-
	holding(X,in_hand),
	holding(Y,in_hand),
	assert(wearing(X,Y)),!,
        retract(holding(X,in_hand)),
	retract(holding(Y,in_hand)),
	write('great that just fits perfect!'),nl.


wear(X,Y) :-
        wearing(X,Y),
        write('You''re already wearing it!'),
        !,nl.

wear(_,_):-
	write('You don''t have both your tie and suit! Get them together!'),nl.

inventory:-
	write('You have the following items with you!'),nl,
	notice_inventory(in_hand),nl.

i:-
	write('You have the following items with you!'),nl,
	notice_inventory(in_hand),nl.
drink:-
	write('You are using your energy drink'),nl,
	energy(Initial),
	check_energy(Initial),!,
	Current is Initial+50,
	retract(energy(Initial)),
	assert(energy(Current)),nl,
	retract(holding(energy_drink,in_hand)),nl,
	energy_left,nl.

/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.              -- to start the game.'), nl,
        write('n. s.  e.  w. u. d. -- to go in that direction.'), nl,
        write('take(Object).       -- to pick up an object.'), nl,
        write('drop(Object).       -- to put down an object.'), nl,
        write('look.               -- to look around you again.'), nl,
	write('inventory.	   -- check inventory.'),nl,
	write('energy_left.	   -- check energy left.'),nl,
	write('drink.              -- use energy_drink.'),nl,
	write('i.                  -- check inventory.'),nl,
	write('wear(Object,Object).-- to wear clothes.'),nl,
	write('hit.                -- to attack an enemy.'), nl,
        write('instructions.       -- to see this message again.'), nl,
        write('halt.               -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.

describe(_):-
	energy(Initial),
	\+check_energy(Initial),
	write('Sorry no energy left! You lose!'),nl,
	write('Today is not the day you propose Robin!'),nl,
	finish,!.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */
describe(mcLaren) :-
	write('You are at McLarens!'),nl,
	write('There is a restaurant on the east.'),nl,
	write('Your home is above.'),nl.

describe(robins_house) :-
        write('Congratulations!!  You have got Robin a blue french horn and the ring! She is all impressed and wants to mary you!'), nl,
        write('You won the game.'),nl,
        finish, !.

describe(chest):-
	write('You are at the chest!'),nl,
	write('There is the home enterance to the north.'),nl.

describe(home) :-
        write('You are in your home. Its a wonderful day outside!'),nl,
        write('Lets ask out Robin today! Did that make you a little nervous?'),nl,
        write('Let the blue french horn do its trick! Lets get it from the restaurant!'), nl,
        write('Lets suit up, get the blue frnch horn from the restaurant, a ring and hit Robins house.'), nl,
	write('McLarens, your favorite hangout place, is below your house.'),nl,
	write('There is a closet to your east and something glittery to your south.'),nl.

describe(closet) :-
	holding(key,in_hand),
        write('Heres a closet with all your clothes!How about waearing a suit.'),nl,
	write('Oh! no can''t as yet!Need a matching tie! There is a drawer to the east!'), nl,
	write('The home enterance is to the west.'),nl.
describe(closet):-
	write('You are at the closet!'),nl,
	write('The home enterance is to your west.'),nl.

describe(drawer) :-
	write('You are at the drawer.'),nl,
	write('The closet is to your west.'),nl.

describe(robins_house_enterance) :-
	holding(blue_french_horn, in_hand),
	holding(ring,in_hand),
	wearing(_,_),
        write('The moment of truth! You have got everything?'), nl,
        write('There.. her room is on the south.'),nl.

describe(restaurant) :-
	alive(barney),
        holding(ring,in_hand),
        write('Barney saw you with the ring he was going to give Robin!!!'), nl,
        write('	it is over now! '), nl,
        die.

describe(restaurant) :-
        alive(barney),
	write('You are at the restaurant! '), nl,
	write('Mclarens is to your west.'), nl,
        write('There is barney here on your south! '), nl,
        write('Gosh! He is going to talk you out of it!'),nl.

describe(restaurant) :-
        write('You are at the restaurant! '), nl,
	write('Mclarens is to your west.'), nl,
        write('Robin''s house is to your east.'),nl.

describe(barney) :-
        alive(barney),
	write('Heres Barney!'),nl,
        write(' He is going to propose Robin first? You are a good guy Ted but you ought to do something about it! '),nl,
	write('Knock him down. Well you got to do what you got to do!'),nl,
	write('The restaurant is on your north.'),nl.

describe(barney) :-
        write('Barney is on the floor unconcious.'),nl.



