/* Rescue Princess, by Lanlan Pang. */

:- dynamic i_am_at/1, at/2, holding/1, buy_only_at/2, alive/1, money/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)),
   retractall(buy_only_at(_, _)), retractall(alive(_)), retractall((money(_)).

/* This defines my current location and initial money. */

i_am_at(logcabin).
money(0).

/* These facts describe how the rooms are connected. */

path(treasurechest, e, tunnel).
path(tunnel, w, treasurechest).
path(tunnel, s, crystalball).
path(crystalball, n, tunnel).
path(tunnel, n, smalldoor).
path(smalldoor, s, tunnel) :- at(badge, in_hand).
path(smalldoor, s, tunnel) :-
                write('Oh, the door seems to be loocked, there is '), nl,
				write('a guardian angle, could you find a badge and give'), nl,
				write('it to the angle, he will open the door for you.'), nl, fail.
path(smalldoor, w, rockcave).
path(rockcave, e, smalldoor).
path(smalldoor, e, logcabin).
path(logcabin, w, smalldoor).
path(logcabin, n, store).
path(store, s, logcabin).
path(logcabin, e, bridge) :- at(horse, in_hand).
path(logcabin, e, bridge) :-
                  write('Oh, you can not across the bridge without horse.'), nl, fail.
path(bridge, w, logcabin).
path(store, e, forest).
path(forest, w, store).
path(tunnel, e, entrance_wetland).
path(entrance_wetland, w, tunnel).
path(entrance_wetland, s, wetland).
path(wetland, n, entrance_wetland).
path(bridge, e, river).
path(river, w, bridge).
path(river, n, volcano).
path(volcano, s, river).
path(river, e, devil_monster).
path(devil_monster, w, river).

/* These facts tell where the various objects in the game
   are located. */

at(diamond, treasurechest).
at(ruby, rockcave).
at(magic_wand, crystalball).
at(badge, forest).
/* at(horse, entrance_wetland). */
buy_only_at(grass, store).
buy_only_at(vitamin, store).


/* This fact specifies that the monster is alive. */
alive(devil_monster).


/* These rules describe how to pick up an object. */

take(X) :-
        at(X, in_hand),
        write('You''re already holding it!'),
        nl, !.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(at(X, in_hand)),
        write('OK.'),
        nl, !.

take(_) :-
        write('I don''t see it here.'),
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
        write('You aren''t holding it!'),
        nl.
		
/* The rules describe how to sell an item. */

sell(X) :-
        i_am_at(store),
        at(X, in_hand),
        (at(diamond, in_hand) ; at(ruby, in_hand)),
        retract(at(X, in_hand)),
        retract(money(M)),
        N is M + 100,
        assert(money(N)),
        write('Your balance is: '), 
        write(N), nl, !.
sell(X) :-
        i_am_at(store),
        at(X, in_hand),
        write('You can not sell this item at store!'), nl, !.
sell(_) :-
        i_am_at(market),
        write('You don''t have it!'), nl, !.
sell(_) :-
        write('You''re not at the store!'),
        nl.
		
/* The rules describe how to buy an item. */

buy(X) :-
        i_am_at(store),
        buy_only_at(X, store),
        money(M), M >= 100,
        retract(buy_only_at(X, store)),
        assert(at(X, in_hand)),
        retract(money(M)),
        N is M - 100,
        assert(money(N)),
        write('Your balance is : '), 
        write(N), nl, !.
buy(X) :-
        i_am_at(store),
        buy_only_at(X, store),
        write('You don''t have enough money!'), nl, !.
buy(_) :-
        i_am_at(store),
        write('This item is not available in the store!'), nl, !.
buy(_) :-
        write('You''re not at the store!'),
        nl.


/* These rules define the direction letters as calls to go. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).

/* The rules display all the items you're now holding. */
display :-
        at(X, in_hand),
        write(X), nl, fail.

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

/* These rules tell how to handle killing the devil_monster. */

kill :-
       i_am_at(wetland),
	   write('There is a spider here, you died!'), nl,
	   !, die.
kill :-
        i_am_at(devil_monster),
        at(magic_wand, in_hand),
        retract(alive(devil_monster)),
        write('You hack repeatedly at the devil_monster''s back.  Slimy ichor'), nl,
        write('gushes out of the devil_monster''s back, and gets all over you.'), nl,
        write('I think you have killed it, despite the continued twitching.'), nl, 
		write('You successfully rescue the princess, congratulations!'), nl, 
		finish, !.		
kill :-
        write('I see nothing inimical here.'), nl.
kill :-
        i_am_at(devil_monster),
        write('Run! You can not win the devil_monster without the magic_wand.'), nl.		

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
		write('sell(Object).       -- to sell an item at the store.'), nl,
		write('buy(Object).       -- to buy an item at the store.'), nl,
		write('kill               -- to attack an enemy.'), nl,
        write('look.              -- to look around you again.'), nl,
		write('display.           -- to display the item you are holding.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */
   
describe(treasurechest) :-
        at(diamond, in_hand),
        write('You are now in a treasure chest.'), nl,
        write('You have got the diamond, the chest is empty now.'), nl,
		write('To the east is the tunnel.'), nl, !.

describe(treasurechest) :-
		write('You are now in a treasure chest.'), nl,
        write('Please take the diamond.'), nl,
		write('To the east is the tunnel.'), nl.
		
describe(crystalball) :-
        at(magic_wand, in_hand),
        write('You are in a crystal ball.'), nl,
		write('You have got the magic_wand, the crystalball is empty now.'), nl,
        write('To the north is the locked tunnel.'), nl, !.

describe(crystalball) :-
        write('You are in a crystal ball.'), nl,
		write('Please take the magic_wand.'), nl,
        write('To the north is the locked tunnel.'), nl.

describe(tunnel) :-
        write('You are in a tunnel.'), nl,
        write('You give your badge to the guardian angle of the tunnel, you can enter the tunnel now'), nl,
        write('To the west is the treasure chest.'), nl,
		write('To the east is the entrance of wetland.'), nl,
		write('To the south is the crystal ball.'), nl,
		write('To the north is a unlocked small door.'), nl.

describe(smalldoor) :-
        write('You are in a small unlocked door.'), nl,
        write('To the west is the rock cave.'), nl,
		write('To the east is the log cabin.'), nl,
		write('To the south is the locked tunnel.'), nl.
		
describe(rockcave) :-
        at(ruby, in_hand),
        write('You are in a rock cave.'), nl,
		write('You have got the ruby and the rock cave is empty now'), nl,
        write('To the east is a small unlocked door'), nl, !.

describe(rockcave) :-
        write('You are in a rock cave please take the ruby.'), nl,
        write('To the east is a small unlocked door'), nl.
		
describe(logcabin) :-
        write('You are in a log cabin.'), nl,
        write('To the west is a unlocked small door.'), nl,
		write('To the east is the bridge which is the road to the LOST LAND.'), nl,
		write('To the north is the store.'), nl.

describe(store) :-
        write('You are at a store.'), nl,
		write('Ruby worth 100 golden coins, diamond worth 100 golden coins.'), nl,
		write('grass costs 100 golden coins, vitamin costs 100 golden coins.'), nl,
		write('Your balance is: '), money(X), write(X), write(' golden coins'), nl,
		write('Please sell things to earn money or just buy something.'), nl,
		write('To the south is the log cabin, to the east is the forest.'), nl.

describe(forest) :-
        at(badge, in_hand),
        write('You are in the forest.'), nl,
		write('You have got the badge and forest is empty now'), nl,
        write('To the west is the store'), nl, !.
		
describe(forest) :-
        write('You are in forest and please take the badge.'), nl,
        write('To the west is the store.'), nl.

describe(bridge) :-
        write('To the west is the log cabin.'), nl,
		write('To the east is the river.'), nl.
		
describe(entrance_wetland) :-
        at(grass, in_hand),
        at(vitamin, in_hand),
        write('You are at entrance of wetlan. You have got the horse. It''s empty now'), nl,
		write('To the west is the locked tunnel.'), nl,
		write('To the south is the wetland. It''s dangerous! '), nl,
		assert(at(horse, in_hand)), !.
		
describe(entrance_wetland) :-
        at(horse, in_hand),
        write('You are at entrance of wetlan. You notice the horse is hidden in this place.'), nl,
		write('You use the mixed vitamin grass to allure the horse then get it.'), nl,
        write('To the west is the locked tunnel.'), nl,
		write('To the south is the wetland. It''s dangerous! '), nl, !.

describe(entrance_wetland) :-
        at(grass, in_hand),
        write('You are at entrance of wetlan. Someting hidden here! Try to find it! The grass is poinsonous'), nl,
		write('clue: vitamin'), nl,
		write('To the west is the locked tunnel.'), nl,
		write('To the south is the wetland. It''s dangerous! '), nl, !.

describe(entrance_wetland) :-
        write('You are at entrance of wetlan. Someting hidden here! Try to find it!'), nl,
		write('clue: vitamin and grass.'), nl,
		write('To the west is the locked tunnel.'), nl,
		write('To the south is the wetland. It''s dangerous! '), nl.

describe(wetland) :-
        write('You are at the wetlan. It''s dangerous! This is a spider room! Kill or leave?'), nl,
		write('To the west is the locked tunnel.'), nl,
		write('To the north is the entrance of wetland.'), nl.
		
describe(river) :-
        write('You are at river now.'), nl,
		write('To the north is the volcano, it''s dangerous.'), nl,
		write('To the west is the bridge.'), nl,
		write('To the east is the devil_monster.'), nl.

describe(volcano) :-
        write('You are at volcano now. You are unable to survive at volcano, the valcano '), nl,
	    write('is eruppting! You died!'), nl,
	    die.		

describe(devil_monster) :-
        write('You meet the devil_monster, this is the final boss! Kill it or leave? If want to kill it , just type kill.'), nl,
		write('To the west is the river.'), nl.

