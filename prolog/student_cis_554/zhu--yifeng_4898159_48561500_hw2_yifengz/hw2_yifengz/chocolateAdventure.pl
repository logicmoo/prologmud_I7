/* <Chocolate adventure>, by <Yifeng Zhu(yifengz)>. */

:- dynamic i_am_at/1, at/2, holding/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

/*this defines my current place*/
i_am_at(openarea).

/*This fact tells that I currently have money and flashlight*/
holding(money).
holding(flashlight).


/*this defines how multiple places are connected*/
path(openarea, w, warehouse).
path(warehouse, e, openarea).

path(openarea, s, house).
				
path(house, n, openarea).
path(openarea, e, grocery).
path(grocery, w, openarea).

path(openarea, n, door) :- holding(badge).
path(openarea, n, door) :-
        write('The door is locked, you need to have your badge.'), nl,
        !, fail.

path(door, s, openarea).

path(door, n, cave) :- holding(flashlight), holding(battery).
path(door, n, cave) :- 
        write('You need to have flashlight and battery to enter the cave.'), nl,
        !, fail.

path(cave, s, door).

/* These facts tell where the various objects in the game
   are located. */	
at(badge,openarea).   
at(battery, warehouse).
at(witch, house).
at(chocolate , house).
at(food, grocery).
at(drink, grocery).
at(hammer, cave).
at(sealedBox,cave).

/*This rule tells you how to open a sealedBox in the cave*/
open :-
		i_am_at(Place),
		holding(hammer),
		at(sealedBox,Place),
		retract(at(sealedBox,cave)),
		assert(at(magicStick,cave)),
		write('You successfully used your hammer to break the sealedBox'),nl,
		write('Now you can would see magicStick, grab them!'),
		!,nl.
open :-
		i_am_at(Place),
		at(sealedBox, Place),
		write('You need to use a hammer to break the sealedBox.'),
		!,nl.
open :-
		write('Sorry, I dont see anything you can open here.'),nl.
		
/*This rule tells you how to consume food or drink*/
consume(X) :-
		holding(X),
		retract(holding(X)),
		write('You just consumed '),write(X),
		!,nl.
consume(X) :-
	    write('You cannot consume '),write(X),nl,
		write('You do not own it'),nl.
		
/* These rules describe how to buy an object. */
buy(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

buy(X) :-
        i_am_at(Place),
        at(X, Place),
		holding(money),
        retract(at(X, Place)),
		retract(holding(money)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

buy(X) :-
        i_am_at(Place),
        at(X, Place),
        write('You dont have money!'),
        !, nl.
		
buy(_) :-
        write('I don''t see it here.'),
        nl.


/* These rules describe how to return an object. */
cancel(X) :-
        holding(X),
        i_am_at(grocery),
        retract(holding(X)),
        assert(at(X, grocery)),
		assert(holding(money)),
        write('OK.'),
        !, nl.

cancel(X) :-
        holding(X),
        i_am_at(warehouse),
        retract(holding(X)),
        assert(at(X, warehouse)),
		assert(holding(money)),
        write('OK.'),
        !, nl.
		
cancel(X) :-
        holding(X),
        write('You can only return at grocery or warehouse.'),
        !, nl.
		
cancel(_) :-
        write('You aren''t holding it!'),
        nl.

/* These rules describe how to pick up an object in the cave. */
take(X) :-
        holding(X),
        write('You''re already holding it!'),
        nl, !.

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

/* These rules describe how to put down an object in the cave. */
drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('OK.'),
        nl, !.

drop(_) :-
        write('You aren''t holding it!'),
        nl.

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

/*These rules tell you what you have right now*/
i :-
		holding(X),
		write('You have '), write(X),nl,
		fail.
		
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
        write('take(Object).      -- to pick up an object in the cave or house.'), nl,
        write('drop(Object).      -- to put down an object in the cave or house.'), nl,
		write('buy(Object).      -- to buy an object in grocery or warehouse.'), nl,
        write('cancel(Object).      -- to return an object in grocery or warehouse.'), nl,
        write('look.              -- to look around you again.'), nl,
		write('i.		 -- to find out what you have in hand right now.'),nl,
		write('open				 -- to open sealedBox.'),nl,
		write('consume			 --to consume food or drink you have.'),nl,
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */
describe(openarea) :-
		holding(chocolate),
        write('Congratulations!'), nl,
		write('You found chocolate for your friends,'),nl,
        write('and won the game.'), nl,
		finish,!.
		
describe(openarea) :-
        write('You are in the openarea.'), nl,
		write('To the north is a locked door which you need to use badge to open;'),nl,
        write('to the south is a small shaded black house;'), nl,
        write('to the west is a large warehouse; to the east is a modern grocery.'), nl,
        write('You task is to safely explore those places'), nl,
        write('and find lots of chocolate for your friends.'), nl.
		
		
describe(house) :-
		at(witch, house),
		holding(magicStick),
		retract(at(witch,house)),
		retract(holding(magicStick)),
        write('Wow! You defeated the witch.'), nl,!.
		
describe(house) :-
		at(witch,house),
		write('Woo, you met the witch without magicStick she killed you.'),nl,
		write('Next time make sure you grab the magicStick'),nl,
		write('you need to enter the door and explore inside.'),nl,
		die,
		!,fail.
describe(house) :-
		write('No one is here, take whatever you see if any'),nl.
		
describe(warehouse) :-
		write('You are in a large warehouse'),nl,
		write('look around and buy something you need.'),nl,
		write('To the east is the openarea where you can go back.'),nl.
		
describe(grocery) :-
		write('You are in a modern grocery store'),nl,
		write('look around and buy something you need.'),nl,
		write('To the west is the openarea where you can go back.'),nl.

describe(door) :-
		write('You are at the door with you badge'),nl,
		write('and you would find a cave to its north.'),nl,
		write('To the south is the openarea where you can go back.'),nl.

describe(cave) :-
        write('This is the cave you have been looking for!'),nl,
		write('Grab as many things as you want.'),nl,
		write('To the south is the door you just entered.'),nl.



