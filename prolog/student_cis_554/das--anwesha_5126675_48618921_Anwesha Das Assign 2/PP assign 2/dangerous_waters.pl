/* <Dangerous Waters>, by <Anwesha Das>. */

:- dynamic i_am_at/1, at/2, holding/1, armed/1, seen/1, floor_at/1, not_seen/1, floor_at/1, not_picked/1, time/1, count/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(armed(_)), retractall(not_seen(_)), retractall(floor_at(_)), retractall(not_picked(_)), retractall(count(_)), retractall(time(_)).

/* This tells your current location. */
i_am_at(deck).
floor_at(deck).

time(90).
count(_).

/* Directions */
path(corridor, e, boiler_room):- holding(chloroform), count(Z), retract(count(Z)), assert(count(3)), !.

path(corridor, e, boiler_room):- retract(floor_at(_)), assert(floor_at(boiler_room)), retract(i_am_at(_)), assert(i_am_at(corridor)),  write('You see two men with guns sitting outside the room. You don''t have anything to attack the guards or defend yourself!!!'), nl, 
				 write('Unfortunately, customs did not allow you to carry your gun with you on the ship. So you have to find a weapon on the ship itself.'), nl,
				 write('Aah! Idea! Maybe you can steal some chloroform from the medical clinic on board (which, as far as you remember, is on the deck), '),
				 write('and use it incapacitate the guards!'), nl, !, fail.

path(deck, w, stairs) :- count(Z), retract(count(Z)), assert(count(2)).
path(deck, s, elevator) :- count(Z), retract(count(Z)), assert(count(5)).
path(deck, n, office) :- count(Z), retract(count(Z)), assert(count(5)).
path(deck, e, clinic) :- count(Z), retract(count(Z)), assert(count(5)).

path(stairs, f, room) :- count(Z), retract(count(Z)), assert(count(8)). 
path(stairs, b, corridor) :- count(Z), retract(count(Z)), assert(count(10)).
path(stairs, d, deck) :- count(Z), retract(count(Z)), assert(count(10)).

path(clinic, n, stairs) :- count(Z), retract(count(Z)), assert(count(6)).
path(clinic, s, elevator) :- count(Z), retract(count(Z)), assert(count(8)).
path(clinic, w, office):- count(Z), retract(count(Z)), assert(count(5)).

path(elevator, d, deck) :- count(Z), retract(count(Z)), assert(count(8)).
path(elevator, b, corridor) :- count(Z), retract(count(Z)), assert(count(9)).
path(elevator, f, room) :- count(Z), retract(count(Z)), assert(count(5)).

path(room, w, stairs) :- count(Z), retract(count(Z)), assert(count(3)).
path(room, n, elevator) :- count(Z), retract(count(Z)), assert(count(4)).

path(office, s, elevator) :- count(Z), retract(count(Z)), assert(count(8)).
path(office, n, stairs) :- count(Z), retract(count(Z)), assert(count(4)).
path(office, e, clinic) :- count(Z), retract(count(Z)), assert(count(5)).
path(office, w, baggage_hold) :- count(Z), retract(count(Z)), assert(count(7)).

path(baggage_hold, e, office) :- count(Z), retract(count(Z)), assert(count(7)).

path(corridor, w, elevator) :- count(Z), retract(count(Z)), assert(count(1)).
path(boiler_room, w, corridor) :- count(Z), retract(count(Z)), assert(count(3)).

/* Locations of objects */
at(case, boiler_room).
at(keypass, office).
at(chloroform, clinic).
at(toolkit, baggage_hold).
at(laptop, room).


/* This fact specifies that the bomb is armed. */

armed(bomb).

/* This fact specifies that the footage is not seen. */
not_seen(footage).

/* This fact specifies that the lock is not picked. */
not_picked(lock).



/* These rules describe how to pick up an object. */

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
        nl, !.

take(_) :-
        write('I don''t see it here.'),
        nl.

 
/* These rules describe how to toss the choloroform */

toss :- i_am_at(boiler_room),
	holding(chloroform),
        write('The guards have fainted. The coast is clear. Quick, swipe the keypass at the door to enter. '), !,
        nl.

toss :- holding(chloroform),
	write(' You can''t toss it here! Save it for the men guarding the bomb!'), !,
	nl.

toss :- write(' There is nothing to toss!'), 
	nl.

/* This rule describes how to swipe the keypass */
swipe :- holding(keypass), i_am_at(boiler_room), write('You are now inside the boiler room. You hear ticking noise coming from the black case infront of you. Open it quick!'), !, nl.

/* This rule tells the time */
time :- time(T), write(T), write(' min(s) remaining! '), nl.

/* These rules describe how to disarm the bomb */
disarm :- i_am_at(boiler_room),
	write('The bomb has been disarmed!!!! Congratulations! You just saved the lives of 800 people! '), nl, nl,
	retract(armed(bomb)), !,
	finish,
        nl.

disarm :- write('You have to get to the bomb first to disarm it!! Hurry!!'), !,
	nl.


/* These rules describe how to open an object */
open :- i_am_at(baggage_hold),
	write('You open the bag and can see the toolkit'), !,
	nl.

open :- i_am_at(boiler_room),
	write('You open the case and see the bomb. Hurry!! Disarm it quickly before it goes off!!!!!'), !,
	nl.

open :- i_am_at(clinic),
	write('You open the cabinet and spot the bottle of chloroform.'), !, nl.

open :- write('There is nothing to open!'), !,
	nl.

/* These rules tells how to connect phone to laptop */
connect :- i_am_at(room), 
	write('You see the footage, which shows the Captain going to the boiler room, swiping his all access keypass and putting a black case in there.'), nl,
	write('You decide your next task should be to steal the keypass from the Captain''s office up on the deck!'), nl, nl,
	write('You see the stairs to your west and the elevator to the north.'), nl,
	retract(not_seen(footage)), !, nl. 

connect :- write('There is nothing to connect!'), !, nl.

/* This rule tells how to pick a lock */
pick :- i_am_at(office), write('You are now inside the captain''s office. It is a mess! Looks like someone went through it in a hurry. You see an empty safe and a ripped up sofa.'), nl, nl,
	write('You see the keypass you need lying on the table infront of you.'), !, nl.

/* Maps */
map :- floor_at(deck), i_am_at(office), write(' North - Stairs '), nl, write(' East - Medical Clinic '), nl, write(' West - Baggage Hold '), nl, write(' South - Elevator '), !, nl.
map :- floor_at(deck), i_am_at(baggage_hold), write(' East - Captain''s Office '), !, nl.
map :- floor_at(deck), i_am_at(clinic), write(' North - Stairs '), nl, write(' West - Captain''s Office '), nl, write(' South - Elevator '), !, nl.	
map :- floor_at(deck), i_am_at(deck), write(' North - Captain''s Office '), nl, write(' East - Medical Clinic '), nl, write(' West - Stairs '), nl, write(' South - Elevator '), !, nl.
map :- floor_at(first), i_am_at(room), write(' North - Elevator '), nl, write(' West - Stairs '), !, nl.
map :- floor_at(boiler_room), i_am_at(corridor), write(' West - Elevator '), nl, write(' East - Boiler Room '), !, nl.
map :- floor_at(boiler_room), i_am_at(boiler_room), write(' West - Corridor '), !, nl.

/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).

d :- go(d).

f :- go(f).

b :- go(b).


/* This rule tells how to move in a given direction. */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
	time(X), X > 0, count(Z),
	retract(i_am_at(Here)),
        assert(i_am_at(There)),
	retract(time(X)), 
	Y is X-Z, 
	assert(time(Y)),
        Y>0, describe(There), nl, write(Y), write(' min(s) remaining!'), !, nl.

go(_) :- time(X), X =< 0, nl, nl, write('OH NO!! 90 minutes are up! Bomb explodes in... 3...2...1.... BOOOOM!'), finish, !, fail.

go(_) :- write('You can''t go that way.'), !, nl.


/* This rule tells how to look about you. */

look :-
        i_am_at(Place),
        notice_objects_at(Place),
	fail, nl.


/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).


finish :-
        nl, nl,
        write('The game is over. Please enter the "halt." command.'), 
        nl.

i :- holding(X), write(X), nl, fail.


/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             	 -- to start the game.'), nl,
        write('n.  s.  e.  w. d. f. b.	 -- to go in that direction.'), nl,
	write('connect.      		 -- to connect phone to laptop.'), nl, 
	write('open.			 -- to open the bag, cabinet and the black case.'), nl,       
	write('take(Object).      	 -- to pick up an object.'), nl,
	write('pick.			 -- to pick the lock.'), nl,	
        write('toss.      		 -- to toss the chloroform.'), nl,
	write('swipe.      		 -- to open the boiler room door.'), nl,
	write('time.		         -- to tell you the time remaining till the bomb goes off.'), nl,
	write('disarm.		         -- to disarm the bomb.'), nl,
        write('instructions.     	 -- to see this message again.'), nl,
	write('map.			 -- to view a map of the floor you are on.'), nl,
        write('i.			 -- to view an inventory of what all you are holding.'), nl,	
        write('halt.            	 -- to end the game and quit.'), nl, nl, nl.
        


/* This rule prints out instructions and tells where you are. */

start :-
        instructions, nl, nl, nl,
	write('You are Agent Clark, one of the best spies in the world.'), nl,
	write('You are currently on vacation on the famous cruise ship, Amadea. '), nl, nl,
	write('However, on the third day of the voyage, while you are enjoying the view from the deck, you receive an urgent email from your boss.'), nl,
	write('It says the Captain of the Amadea has planted a bomb on the ship and has abandoned it this morning!'), nl,
	write('The bomb goes off in 90 MINUTES. You have to disarm the bomb and save the ship and its people before the time is up!'), nl,nl,
	write('The email has the security footage attached, which you download, but can''t view it for some reason.'), nl, nl, nl,
	write('You need to use your laptop, which is in your room on the first floor, to connect your phone and view the footage!!'), nl, nl,
        write('You are on the deck. You see elevator to south. You see stairs to the west.'), !, nl.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */
		  

describe(deck) :-   assert(floor_at(deck)), write('You are on the deck. You see elevator to south. You see stairs to the west. You see the Captain''s office to the north. You also see the medical clinic to your east.'), nl.

describe(stairs) :- floor_at(deck), write('You have reached the stairs. You can go down to your room on the first floor(f) or down till the boiler room (b). '), !, nl.
describe(stairs) :- floor_at(first), write('You have reached the stairs. You can go up to the deck(d) or down till the boiler room (b). '), !, nl.

describe(elevator) :- floor_at(deck), write('You have entered the elevator. You can go down to your room on the first floor(f) or down till the boiler room (b). '), !, nl.
describe(elevator) :- floor_at(first), write('You have entered the elevator. You can go up to the deck(d) or down till the boiler room (b) . '), !, nl.
describe(elevator) :- floor_at(boiler_room), write('You have entered the elevator. You can go up to the deck(d) or to your room on the first floor (f). '), !, nl.


describe(room) :- retract(floor_at(_)), assert(floor_at(first)), write('You are now in your room. You see your laptop. You can connect your phone now to view the footage.'), !, nl.

describe(corridor) :- retract(floor_at(_)), assert(floor_at(boiler_room)), retract(i_am_at(_)), assert(i_am_at(corridor)), write('You are now in the corridor. The elevator is to the west. Looking around, you see the boiler room to the east.'), !, nl.

describe(boiler_room) :- holding(chloroform), retract(floor_at(_)), assert(floor_at(boiler_room)),
			write('The two men are still there. Quick, toss the chloroform bottle at them!'), !, nl.

describe(office) :- holding(toolkit), retract(floor_at(_)), assert(floor_at(deck)), not_picked(lock), write('You are back to the Captain''s office door.'), nl,
		    write('Quickly pick the lock before somebody notices... hurry!!'), !, nl.
		    
describe(office) :- retract(floor_at(_)), assert(floor_at(deck)), write('The door is locked. You need to pick the lock. You look for your handy toolkit (which you usually keep in your pocket). But you cannot find it! Then you remember that is inside your checked in suitcase, which is inside the baggage room.'), 
		    nl, nl, write('Fortunately on the very first day, you had toured the ship. Thanks to your photographic memory, you remember that the baggage hold is towards the west.'), !,  nl.

describe(office) :- retract(floor_at(_)), assert(floor_at(deck)), write('You are now inside the captain''s office. It is a mess! Looks like someone went through it in a hurry. You see an empty safe and a ripped up sofa. You see the elevator to the south, the stairs to the north, the clinic to the east and the baggage hold to the west.'), !, nl.

describe(clinic) :- retract(floor_at(_)), assert(floor_at(deck)), 
		    write('You are now inside the clinic.'), nl,
		    write('Luckily it is empty and the ship doctor is nowhere to be seen. Hopefully you can find some chloroform in here.'), nl, nl,
		    write('You see a closed cabinet. You should probably open it.'), !, nl.

describe(baggage_hold) :- retract(floor_at(_)), assert(floor_at(deck)), write('You are now inside the baggage hold'), nl, nl,
		          write('You can see your bag lying right infront of you. You can open it.'), !, nl.