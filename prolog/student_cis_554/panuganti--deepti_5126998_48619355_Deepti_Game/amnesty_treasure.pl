/* Sunken Treasure -- a sample adventure game, by Deepti Panuganti.
   Consult this file and issue the command:   start.  */

:- dynamic at/2, i_am_at/1, alive/1, wearing/1,dead/1,count/1.  /* Needed by SWI-Prolog. */
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).


count(0).


/* This defines my current location. */

i_am_at(sea).


/* These facts describe how the rooms are connected. */

path(sea, s, trench).
path(trench, n, sea).
path(sea, e, palace_entrance).
path(palace_entrance, w, sea).
path(palace_entrance, s, palace_door).
path(palace_door, n, palace_entrance).
path(locked_door, u, shelf).
path(shelf, d, locked_door).
path(sunken_ship, s, sea) :-
	wearing(invisibility_cloak),!,nl.
path(sunken_ship, s, sea) :-
	write('The merman can see you! Put your invisibility_cloak back on.'),nl,fail.
path(through_trench, s, shell).
path(shell, n, through_trench).

path(sea, n, sunken_ship) :- 
    alive(invisibility_cloak),
    wearing(invisibility_cloak),!,nl.
path(sea, n, sunken_ship) :- 
    at(invisibility_cloak,in_hand),
    dead(invisibility_cloak),
    write('The spell on the invisibility_cloak is wearing off! You need to recast the spell.'),
    nl,!,fail.
path(sea, n, sunken_ship) :-
	alive(invisibility_cloak),
	at(invisibility_cloak,in_hand),
	write('The merman can still see you! Wear your invisibility_cloak.'),nl,!, fail.

path(sea, n, sunken_ship) :- 
    write('There is a vicious looking merman with '), nl,
    write('a spear guarding the ship!'), nl,
    write('If he sees you who knows what he''ll do.'), nl,
    write('You better not go there.'), 
    nl,!,fail.

path(sea, w, indefinitesea) :- 
    write('Don''t give up already!'),nl,
    write('You can find the treasure.'),
    nl,!,fail.

  path(indefinitesea, e, sea).

 path(squid, w, palace_entrance). 


path(palace_entrance, e, squid).
path(squid, e, guarded_door).
path(guarded_door, w, palace_entrance).

path(king, s, palace_entrance).
path(palace_entrance, n, king) :-
    wearing(invisibility_cloak),
    at(chest,in_hand),
    write('The king can''t see you!'),nl,
    write('Remove the invisibility_cloak first.'),
    nl,!,fail.

path(palace_entrance, n, king) :-
    at(chest,in_hand),!.

path(palace_entrance, n, king) :-
    wearing(invisibility_cloak), 
    alive(invisibility_cloak),!,nl.

path(palace_entrance, n, king) :-
    at(invisibility_cloak,in_hand),
    alive(invisibility_cloak),
    write('Don''t just hold the invisibility_cloak. Wear it!'),!,fail,nl.

path(palace_entrance, n, king) :-
	at(invisibility_cloak, in_hand),
	write('The spell on the invisibility_cloak doesn''t seem to be working properly.'), nl, !, fail.

path(palace_entrance, n, king) :- 
    write('You don''t want the king to '),nl,
    write('see you lurking around.'),
    nl,fail.

path(trench, u, above_trench).
path(above_trench, d, trench).
path(through_trench, n, trench).
path(trench, s, through_trench).

path(sunken_ship, e, corridor) :-
	wearing(invisibility_cloak),
	write('There is huge shark in front of you. Lucky you have the invisibility_cloak.'),nl,!.

path(sunken_ship, e, corridor):- 
    write('Uh-oh! There is a huge shark right '),nl,
    write('in front of you!'),nl, 
    write('Get out of there ASAP.'),
    nl.

 path(corridor, w, sunken_ship) :- 
 	wearing(invisibility_cloak),nl,!.

path(corridor, w, sunken_ship) :-
 	write('What did you do with your invisibility_cloak?!'),nl,
 	write('The shark can see you. Put it back on!'),nl,!,fail.
 

path(locked_door, e, sunken_ship).
path(sunken_ship, w, locked_door) :- 
    at(key, in_hand).
path(sunken_ship, w, locked_door) :- 
    write('The door is locked.'),
    nl,fail.

path(locked_door, u, chest).
path(chest, d, locked_door).

/* These facts tell where the various objects in the game are located. */

at(key, corridor). 
at(trident, palace_door).
at(invisibility_cloak, guarded_door).
at(chest, shelf).


/* This fact specifies that the squid is alive. */

alive(squid).
alive(shell).
alive(shark).
dead(invisibility_cloak).


/* These rules describe how to pick up an object. */

take(invisibility_cloak) :-
		count(N),
        N<2,
        retract(count(N)),
        Y is N+1, 
        assert(count(Y)),
		i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(at(X, in_hand)),
        write('OK. But the invisibility_cloak doesn''t seem to be working very well'), nl,
        write('Maybe the spell is wearing off. You better find a way to recast it.'),
        nl, !.

take(invisibility_cloak) :-
        i_am_at(Place),
        at(_, Place),
        write('You can only hold 2 objects at a time. You have to drop something.'),
        nl, !,fail.

take(invisibility_cloak) :-
        write('It isn''t here.'),
        nl, !,fail.


take(X) :-
        at(X, in_hand),
        write('You''re already holding it!'),
        nl, !.

take(X) :-
        count(N),
        N<2,
        retract(count(N)),
        Y is N+1, 
        assert(count(Y)),
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(at(X, in_hand)),
        
        write('OK.'),
        nl, !.  

take(_) :-
        i_am_at(Place),
        at(_, Place),
        write('You can only hold 2 objects at a time. You have to drop something.'),
        nl, !,fail.  

take(_) :-
		
        write('It isn''t here'),
        nl,!,fail.


 wear(invisibility_cloak) :-
        dead(invisibility_cloak),
        write('It''s not working! What''s the point of wearing it?'),nl,fail,!.

 wear(X):-
        assert(wearing(X)). 

remove(X) :-
        retract(wearing(X)).  

i :-
        at(X,in_hand),
        write(X),nl,fail.       


/* These rules describe how to put down an object. */

drop(invisibility_cloak) :-
        at(X, in_hand),
        i_am_at(Place),
        retract(at(X, in_hand)),
        assert(at(X, Place)),
        retract(wearing(invisibility_cloak)),
        count(N),
        retract(count(N)),
        Y is N-1, 
        assert(count(Y)),
        write('OK.'),
        nl, !.

drop(X) :-
        at(X, in_hand),
        i_am_at(Place),
        retract(at(X, in_hand)),
        assert(at(X, Place)),
        count(N),
        retract(count(N)),
        Y is N-1, 
        assert(count(Y)),
        write('OK.'),
        nl, !.

drop(_) :-
        write('You aren''t holding it!'),
        nl.


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
        write('You can''t go that way.'),fail.


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


/* These rules tell how to handle the shark and squid */

attack :-
        i_am_at(corridor),
        write('Did you really think you could kill a shark?'),nl,
        write('It ate you for supper.'), nl,!, die.

attack :-
        i_am_at(sea),
        write('You try to fight but '),
        write('the merman is too tough for you.'),nl,
        write('You should escape while you still can.'), nl.

attack :-
        i_am_at(squid),
        at(trident, in_hand),
        retract(alive(squid)),
        write('You attack the squid with your trident,'), nl,
        write('piercing it until it falls to the sand below.'), nl,
        write('That seems to have done the trick.'), nl,
        write('There is a door to the east.'),
        nl, !.

attack :-
        i_am_at(above_trench),
        write('You are no match for the jellyfish.'), nl,
        write('As you get stung over and over you start to lose conciousness '), nl,write('and die.'),nl,!,die.

attack :-
        write('There is nothing to attack here.'),nl,write('Boy, you''re violent!'), nl,fail.

break :-
        i_am_at(shell),
        alive(shell),
        at(trident,in_hand),
        retract(alive(shell)),
        assert(dead(shell)),
        assert(at(spellbook, shell)),
        write('The shell broke open!'), nl,
        write('You find a spellbook inside.'),nl,
        write('You should take it and search for the spell'),
        nl,!.

break :-
        i_am_at(shell),
        alive(shell),
        write('Wow that is one strong shell!'),nl,
        write('You are not able to break it with your bare hands.'),nl,
        write('You may need something else.'),
        nl,!,fail.

break :-
        i_am_at(shell),
        write('It''s already open!').

search :-
 		at(spellbook,in_hand),
 		retract(dead(invisibility_cloak)),
 		assert(alive(invisibility_cloak)),
 		write('You look through the book carefully and after a lot of squinting you find the right spell!'),nl,
 		write('You say the incantation aloud and the spell is cast.'),nl,
 		write('Your invisibility_cloak is now working!'),nl,!.

 search :-
 		i_am_at(through_trench),
 		write('You should get closer to see what it is.'),nl.
 	
 search :-
        write('You don''t notice anything special'),nl.





/* This rule tells how to die. */

die :-
        !, finish.


/* Under UNIX, the   halt.  command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final  halt.  */

finish :-
        nl,
        write('Game Over!'),nl,write('Please enter the   halt.   command.'),
        nl, !.


/* This rule just writes out game instructions. */

help :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.                   -- to start the game.'), nl,
        write('n.  s.  e.  w.  u.  d.   -- to go in that direction.'), nl,
        write('take(Object).            -- to pick up an object.'), nl,
        write('drop(Object).            -- to put down an object.'), nl,
        write('wear(Object).		    -- to wear something.'),nl,
	    write('attack.                  -- to attack an enemy.'), nl,
	    write('break.                   -- to break an object'),nl,
        write('look.                    -- to look around you again.'), nl,
        write('search                   -- to search an object more closely.'),nl,
        write('i.                       -- to see what you are currently holding.'),nl,
        write('help.                    -- to see this message again.'), nl,
        write('halt.                    -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        help,
        write('You are underwater in the lost city of Atlantica.'),nl,
        write('You have been banished for badmouthing your king.'),nl,
        write('to get back in his good books find the lost treasure and present it to him.'),nl,
        write('To the north there is a huge shipwreck covered in seaweed.'),nl,
        write('To the south there is a formidable looking trench.'),nl,
        write('To the east there is the King''s palace and '),nl,
        write('to the west is an unending blue expanse of sea.'),nl,
        write('Good luck!'),
        nl.


/* These rules describe the various rooms.  Depending on circumstances, a room may have more than one description. */

describe(sea) :-
		write('You are back in the middle of the city.'), nl,
		write('To the north there is a huge shipwreck covered in seaweed.'),nl,
        write('To the south there is a formidable looking trench.'),nl,
        write('To the east there is the King''s palace and '),nl,
        write('to the west is an unending blue expanse of sea.'),nl.
       

describe(palace_entrance) :-
        alive(squid),
        write('You are in the King''s palace. Don''t be seen.'),nl,
        write('To the north is the King''s court.'), nl,
        write('To the south is a small door and to the east there '), nl,
        write('is a smaller room blocked by a giant squid. The exit is to the west.'), nl.

describe(palace_entrance) :-
        write('You are in the King''s palace. Don''t be seen.'),nl,
        write('To the north is the King''s court.'), nl,
        write('To the south is a small door and to the east there '), nl,
        write('is a smaller room. The exit is to the west.'), nl.

describe(trench) :-
        write('You are staring at a huge trench. You can '), nl,
        write('either go above it or continue south through it.'), nl, write('Go back north to get out.'),nl.

describe(sunken_ship) :-
        write('You are inside the ship! There is a long dark corridor '), nl,
        write('to the east and a fancy looking door to the west.'), nl,
        write('The exit is to the south.'),
        nl.

describe(above_trench) :-
        write('There is a swarm of jellyfish here! Get out!'), nl.

describe(through_trench) :-
        write('You are in the trench. Yikes! Looks creepy.'),nl, 
        write('You see something shining in the distance to the south.'), nl,
        write('The exit is to the north.'), nl.

describe(squid) :-
        alive(squid),
        at(trident, in_hand),
        write('The squid looks like it is going to attack! Watch out!'), nl.

describe(squid) :-
        alive(squid),
        write('The squid wraps it''s tentacles around your neck '),
        write('and strangles you. You die.'), nl, die.

describe(squid):-
	write('The door is open.'),nl.


describe(guarded_door):-
	write('You are inside a tiny passageway that is '),nl,
    write('lit by candles. Yes, they can glow underwater.'),nl,
    write('The exit is to the west').

describe(palace_door) :-
        write('It is a small room that looks like '),nl,
        write('nobody has used it in a long time.'),nl,
        write('The exit is to the north.'),nl.

describe(king) :-
	at(chest,in_hand), 
    write('The king is very pleased with you!'),nl,
    write('So pleased that he decided to forgive you!'),nl,
    write('You can now go back home. Congratulations!'),nl,finish.

describe(king) :-
	wearing(invisibility_cloak), 
    write('Just a bunch of boring ministers discussing politics.'),nl.

describe(locked_door) :-
        at(chest,in_hand),
        write('You are in a room full of shelves and cabinets.'), nl,
        write('The exit is to the east.'),nl,!.

describe(locked_door) :-
        write('You are in a room full of shelves and cabinets.'), nl,
        write('Then you see it. A treasure chest! It is in a shelf above you.'), nl,
        write('The exit is to the east.'),nl.


describe(shelf) :-
	write('Almost there!'),nl.

describe(shell) :-
    alive(shell),
    write('You are in front of a beautiful Oyster shell. It is closed.'),nl,write('You might need to break it to get it open.'),nl.

describe(shell) :-
    write('You are in front of a beautiful Oyster shell. The exit is to the north.'),nl.

describe(corridor) :-
    write('At the end of the corridor is a staircase.'),nl,
    write('As you reach the foot of the stairs you see something glimmer.'),nl.








