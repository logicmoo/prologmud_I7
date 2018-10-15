/* <Castle Escaping>, by <Xiaoxiang Hu>. */

:- dynamic i_am_at/1, at/2, holding/1, path/3.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(holding(_)),
	retractall(path(_,_,_)).

/* places */
dark(warehouse).
dark(armor_house).
dark(hidden_room).
dark(kitchen).
dark(chamber).

bright(shop).
bright(hall).
bright(bedroom).
bright(corridor_w).
bright(corridor_e).
bright(corridor_m).
bright(bridge).

/* paths */
path(corridor_w, e, corridor_m).
path(corridor_m, e, corridor_e).
path(corridor_m, w, corridor_w).
path(corridor_e, w, corridor_m).

path(corridor_w, s, warehouse).
path(corridor_m, s, bedroom).
path(corridor_e, s, armor_house).

path(bedroom, n, corridor_m).
path(armor_house, n, corridor_e).

path(corridor_m, n, bridge).
%path(bridge, n, hall).
path(bridge, s, corridor_m).

path(hall, n, entrance).
path(hall, s, bridge).
path(hall, w, chamber).
path(hall, e, kitchen).

%path(entrance, n, outside).
path(entrance, w, shop).
path(entrance, s, hall).

path(shop, e, entrance).
path(chamber, e, hall).
path(kitchen, w, hall).

path(hidden_room, s, kitchen).


/* objects */
describe(warehouse) :- write('A warehouse, dirty and mess.').
	describe(garbage) :- write('Some garbage lying on the ground.').
		describe(ironwire) :- write('Some iron wire hidden in garbage.').
		describe(battery) :- write('A flashlight battery.').
	describe(door) :- write('A locked door, on the north side of the room.').
	describe(unlocked_door) :- write('A unlocked door, n the north side of the room.').

describe(corridor_w) :- write('A long corridor, it''s its west side.').
describe(corridor_m) :- write('A long corridor, it''s its middle side.').
describe(corridor_e) :- write('A long corridor, it''s its east side.').

describe(bedroom) :- write('A luxury bedroom.').
	describe(shelf) :- write('A shelf, with something on it.').
		describe(candles(20)) :- write('Some candles, can be used for lighting.').
		describe(redkey) :- write('A red key.').
	describe(bed) :- write('A large bed made by red wood.').
		describe(sheet) :- write('Bed sheet, made by wool.').
		% fire
		describe(burning_sheet) :- write('A burning... Run away!!!').
	describe(fireplace) :- write('Fireplace on the wall, still burning.').
	% after sniff
	describe(snuffed_fireplace) :- write('The fireplace is snuffed.').
		describe(firewood) :- write('Not elegent, but can be used as wood sticks').

describe(armor_house) :- write('A place to store armors.').
	describe(armor_suit) :- write('A status wearing armor suit.').
	describe(largebox) :- write('Empty box.').
	describe(bucket) :- write('Can be filled with water.').
	describe(bag) :- write('A big bag, seems to be empty.').
	% after fill water
	describe(fullbucket) :- write('Bucket filled with water').
	
describe(bridge) :- write('A bridge connects two parts of the castle.').
	describe(bridgeguard) :- write('A guard at another end, stop anyone from passing').
	describe(river) :- write('A river runs across the castle.').

describe(shop) :- write('You may buy something here.').
	% shop_item
	% describe(candles) :- write().
	describe(sale_sword) :- write('Great weapon., Price: 200').
	describe(sale_gamehint) :- write('You can buy a game hint here, Price: 200').
	describe(sale_candles) :- write('You can buy some candle here, Price: 200 for 10').
	
describe(entrance) :- write('The entrance of the castle.').
	describe(gateguard) :- write('Of course there is a guard at the entrance.').
	describe(stone) :- write('This castle is build 200 years ago, it says.').
	
describe(hidden_room) :- write('Congratulations, you get to the hidden room.').
	describe(box) :- write('A box, but need password to open.').
	describe(wall) :- write('If you are enough familiar with this castle, '), nl,
							write('You would know the password. Just one letter.'), nl,
							write('Use input(X) to input the letter, if it is uppercase'), nl,
							write('Please input the corresponding lowercase.').
	describe(openbox) :- write('It is open.').
		describe(treasure) :- write('You made it.').
	
describe(chamber) :- write('Office region. A large desk in the middle.').
	describe(bookshelf) :- write('A tall bookshelf.').
		describe(archbook) :- write('A book about British building style.').
		describe(novel) :- write('Mastering C++ in 21 days.').
		describe(note) :- write('I love the design of the castle, just as I love English.').
	describe(desk) :- write('An office desk.').
		describe(locked_drawer) :- write('Locked drawer.').
		% after unlock
		describe(drawer) :- write('A drawer, with some files in it.').
			describe(money(100)) :- write('Some money.').
			describe(mappart1) :- write('Some part of the map of the castle.').
	describe(cabinet) :- write('A cabinet.').
		describe(money(110)) :- write('Some money(2).').
	describe(photo) :- write('A large photo hanging on the wall.').
		describe(tied_greenkey) :- write('A green key. But it''s tied by a rope fixed on the wall.').
		% cut off.
		describe(greenkey) :- write('A green key.').

describe(hall) :- write('What a great open area.').
	describe(painting) :- write('Looks like a masterpiece.').
	% after cutt off
	describe(cutpainting) :- write('Cut Painting. Something is hidden behind.').
		describe(securitybox) :- write('A security box is embeded in the wall.').
		% open using green key.
		describe(opened_securitybox) :- write('A opened security box.').
			describe(mappart2) :- write('Another part of the map.').
			describe(money(120)) :- write('Some money.').

describe(kitchen) :- write('Lots of food.').
	describe(goldplate) :- write('Maybe it can be used as money.').
	describe(scissors) :- write('Ordinary thing in kitchen.').
	describe(refrigerator) :- write('A large refrigerator in the kitchen').
		describe(ice) :- write('Lots of ice...').
	describe(worldmap) :- write('A map of world, hanging on the north wall.').
	describe(flashlight_nopower) :- write('A flashlight, but run out of power').
	
	describe(bagofice) :- write('Really heavy, can be used as a hammer.').

describe(flashlight) :- write('A flashlight, candles are no longer useful.').
describe(sword) :- write('A great weapon.').

/* object can be pick up */
takeable(battery).
takeable(ironwire).
takeable(candles(_)).
takeable(redkey).
takeable(firewood).
takeable(armor_suit).
takeable(bag).
takeable(bucket).
takeable(fullbucket).
takeable(sword).
takeable(gamehint).
takeable(treasure).
takeable(archbook).
takeable(novel).
takeable(note).
takeable(money(_)).
takeable(greenkey).
takeable(mappart1).
takeable(mappart2).
takeable(goldplate).
takeable(scissors).
takeable(ice).
takeable(flashlight_nopower).
takeable(sale_sword).
takeable(sale_gamehint).
takeable(sale_candles).

/* special objects */
describe_item(mappart1) :- nl,
		write('+------------+----     ---+-------------+'), nl,
		write('|    shop                               |'), nl,
		write('+------------+-----   ----+--         --+'), nl,
		write('|   chamber                   kitchen   |'), nl,
		write('+------------+----                    --+'), nl,
		write('             |   bridge   |              '), nl,
		write('+------------+----   -----+-------------+'), nl,
		write('| corridor_w   co                    _e |'), nl,
		write('+-----   ----+----   ----          ----- '), nl,
		write('| warehouse  |                          |'), nl,
		write('+------------+------------+-------------+'), !.

describe_item(mappart2) :- nl,
		write('+------------+----     ---+-------------+'), nl,
		write('|                               en_room |'), nl,
		write('+------------+-----   ----+-------------+'), nl,
		write('|         r       hall                  |'), nl,
		write('+------------+----   -----+-------------+'), nl,
		write('             |   bridge   |              '), nl,
		write('+------------+----   -----+-------------+'), nl,
		write('| c       _w   corri    m    corridor_e |'), nl,
		write('+-----   ----+----   -----+-----   -----+'), nl,
		write('|            |   bedroom  | armor_house |'), nl,
		write('+------------+------------+-------------+'), !.

		
describe_item(map) :- nl,
		write('+------------+----     ---+-------------+'), nl,
		write('|    shop       entrance  | hidden_room |'), nl,
		write('+------------+-----   ----+-------------+'), nl,
		write('|   chamber       hall        kitchen   |'), nl,
		write('+------------+----   -----+-------------+'), nl,
		write('             |   bridge   |              '), nl,
		write('+------------+----   -----+-------------+'), nl,
		write('| corridor_w   corridor_m    corridor_e |'), nl,
		write('+-----   ----+----   -----+-----   -----+'), nl,
		write('| warehouse  |   bedroom  | armor_house |'), nl,
		write('+------------+------------+-------------+'), !.

describe_item(candles(X)) :- write('You have '), write(X), write(' candles left.'), !.
describe_item(money(X)) :- write('You have '), write(X), write(' dollars left.'), !.

describe_item(gamehint) :- write('Have you noticed that a bag of ice can break the world map in the kitchen?').
		
describe_item(Obj) :-
		describe(Obj).
		
/* visibility */
dark_visible(warehouse).
dark_visible(garbage).
dark_visible(ironwire).
%dark_visible(battery).
dark_visible(door).
dark_visible(unlocked_door).
dark_visible(corridor_w).
dark_visible(corridor_m).
dark_visible(corridor_e).
dark_visible(bedroom).
dark_visible(shelf).
dark_visible(candles(_)).
dark_visible(redkey).
dark_visible(bed).
dark_visible(sheet).
dark_visible(burning_sheet).
dark_visible(fireplace).
dark_visible(snuffed_fireplace).
dark_visible(firewood).
dark_visible(armor_house).
dark_visible(bridge).
dark_visible(bridgeguard).
dark_visible(river).
dark_visible(shop).
% dark_visible(candle).
dark_visible(sale_sword).
dark_visible(sale_gamehint).
dark_visible(sale_candles).
dark_visible(entrance).
dark_visible(money(_)).
dark_visible(gateguard).
dark_visible(stone).
dark_visible(chamber).
dark_visible(hall).
dark_visible(painting).
dark_visible(cutpainting).
dark_visible(securitybox).
dark_visible(openedsecuritybox).
dark_visible(mappart2).
dark_visible(kitchen).


/* at relation */
at(garbage, warehouse).
	at(battery, garbage).
	at(ironwire, garbage).
at(door, warehouse).
% unlocked door

at(shelf, bedroom).
	at(candles(20), shelf).
	at(redkey, shelf).
at(bed, bedroom).
	at(sheet, bed).
at(fireplace, bedroom).
%
	at(firewood, snuffed_fireplace).

at(armor_suit, armor_house).
at(largebox, armor_house).
at(bucket, armor_house).
at(bag, armor_house).

at(bridgeguard, bridge).
at(river, bridge).

at(sale_sword, shop).
at(sale_gamehint, shop).
at(sale_candles, shop).

at(gateguard, entrance).
at(stone, entrance).

at(box, hidden_room).
at(wall, hidden_room).
% at(openbox, hidden_room).
at(treasure, openbox).

at(bookshelf, chamber).
	at(archbook, bookshelf).
	at(novel, bookshelf).
	at(note, bookshelf).
at(desk, chamber).
	at(locked_drawer, desk).
	% at(drawer, desk).
		at(money(100), drawer).
		at(mappart1, drawer).
at(cabinet, chamber).
	at(money(110), cabinet).
at(photo, chamber).
	at(tied_greenkey, photo).
	% at(greenkey, photo).

at(painting, hall).
% cutpainting
	at(securitybox, cutpainting).
	% opened curl.. box.
		at(mappart2, opened_securitybox).
		at(money(120), opened_securitybox).

at(goldplate, kitchen).
at(scissors, kitchen).
at(refrigerator, kitchen).
	at(ice, refrigerator).
at(flashlight_nopower, kitchen).
at(worldmap, kitchen).


open_bridge :-
		retract(at(bridgeguard, bridge)), % bridge is open.
		assert(path(bridge, n, hall)).
		
		
/* uses, interaction */
% unlock the warehouse door
use(ironwire, door) :-
		holding(ironwire), at(door, warehouse),
		retract(at(door, warehouse)),
		assert(at(unlocked_door, warehouse)),
		assert(path(warehouse, n, corridor_w)),
		write('The door is unlocked now.'), nl, !.

		
% burn the sheet
use(candles(X), sheet) :-
		holding(candles(X)), X > 0, at(sheet, bed),
		retract(at(sheet, bed)),
		assert(at(burning_sheet, bed)),
		open_bridge,
		write('You burned the sheet on bed, guards will come soon.'), nl, !.
		
% sniff the fire
use(fullbucket, fireplace) :-
		holding(fullbucket), at(fireplace, bedroom),
		retract(at(fireplace, bedroom)),
		retract(holding(fullbucket)),
		assert(holding(bucket)),
		assert(at(snuffed_fireplace, bedroom)),
		write('The fireplace is snuffed.'), nl, !.
		
% get water
use(bucket, river) :-
		holding(bucket),
		retract(holding(bucket)),
		assert(holding(fullbucket)),
		write('Filled the bucket with water.'), nl, !.

% attack guard
use(firewood, bridgeguard) :-
		holding(firewood), at(bridgeguard, bridge),
		retract(holding(firewood)),
		open_bridge,
		write('Lucky! The bridge guard falls into the river.'), nl, !.

% attack entrance guard
use(sword, gateguard) :-
		holding(sword), at(gateguard, entrance),
		retract(at(gateguard, entrance)),
		assert(path(entrance, n, outside)),
		write('You defeated the gate guard. The entrance is open now.'), nl, !.

% use red key
use(redkey, locked_drawer) :-
		holding(redkey), at(locked_drawer, desk),
		retract(holding(redkey)),
		retract(at(locked_drawer, desk)),
		assert(at(drawer, desk)),
		write('The drawer is unlocked now.'), nl, !.

% cut off rope
use(scissors, tied_greenkey) :-
		holding(scissors), at(tied_greenkey, photo),
		retract(at(tied_greenkey, photo)),
		assert(at(greenkey, photo)),
		write('The green key is cut off from the role now.'), nl, !.
		
% cut painting
use(scissors, painting) :-
		holding(scissors), at(painting, hall),
		retract(at(painting, hall)),
		assert(at(cutpainting, hall)),
		write('You split the painting into two pieces.'), nl, !.

% use green key
use(greenkey, securitybox) :-
		holding(greenkey), at(securitybox, cutpainting),
		retract(at(securitybox, cutpainting)),
		assert(at(opened_securitybox, cutpainting)),
		write('The security box is unlocked.'), nl, !.

% combine map
use(mappart1, mappart2) :-
		holding(mappart1), holding(mappart2),
		retract(holding(mappart1)), retract(holding(mappart2)),
		assert(holding(map)),
		write('You get the whole map of the castle.'), nl, !.

% make icebag
use(ice, bag) :-
		holding(ice), holding(bag),
		retract(holding(ice)), retract(holding(bag)),
		assert(holding(bagofice)),
		write('You put all ice into the bag.'), nl, !.
		
% worldmap
use(bagofice, worldmap) :-
		holding(bagofice), at(worldmap, kitchen),
		retract(at(worldmap, kitchen)),
		retract(holding(bagofice)),
		assert(path(kitchen, n, hidden_room)),
		write('You throw the bag of ice to the wall...'), nl,
		write('A huge hole appears on the wall, what''s in it?'), nl, !.

% flashlight
use(battery, flashlight_nopower) :-
		holding(battery), holding(flashlight_nopower),
		retract(holding(battery)), retract(holding(flashlight_nopower)),
		assert(holding(flashlight)),
		write('Get battery on. The flashlight works!'), nl, !.

use(_, _) :-
		write('Nothing happens.'), nl.

/* initial condition */		
i_am_at(warehouse).
holding(candles(0)).
holding(money(0)).

%holding(flashlight).

within(Object, Location) :- at(Object, Location).
within(Object, Location) :- at(Object, Place), within(Place, Location).

/* take an object. */
% special take
% buy things
take(sale_sword) :-
		i_am_at(shop), at(sale_sword, shop),
		holding(money(X)), X < 200,
		write('Not enough money.'), !, nl.

take(sale_sword) :-
		i_am_at(shop), at(sale_sword, shop),
		holding(money(X)), X >= 200,
		retract(holding(money(X))), NewX is X - 200,
		assert(holding(money(NewX))),
		assert(holding(sword)),
		retract(at(sale_sword, shop)),
		!, nl.

take(sale_gamehint) :-
		i_am_at(shop), at(sale_gamehint, shop),
		holding(money(X)), X < 200,
		write('Not enough money.'), !, nl.
		
take(sale_gamehint) :-
		i_am_at(shop), at(sale_gamehint, shop),
		holding(money(X)), X > 200,
		retract(holding(money(X))), NewX is X - 200,
		assert(holding(money(NewX))),
		assert(holding(gamehint)),
		retract(at(sale_gamehint, shop)),
		!, nl.
		
take(sale_candles) :-
		i_am_at(shop), at(sale_candles, shop),
		holding(money(X)), X < 200,
		write('Not enough money.'), !, nl.
		
take(sale_candles) :-
		i_am_at(shop), at(sale_candles, shop),
		holding(money(X)), X > 200,
		retract(holding(money(X))), NewX is X - 200,
		assert(holding(money(NewX))),
		holding(candles(Y)),
		retract(holding(candles(Y))), NewY is Y + 10,
		assert(holding(candles(NewY))),
		!, nl.

% take candle, money.
take(candles(X)) :-
		i_am_at(Location), within(candles(X), Location), % place is valid
        retractall(at(candles(X), _)),
		holding(candles(Num)),
		retract(holding(candles(Num))),
		NewNum is Num + X,
        assert(holding(candles(NewNum))),
        write('Get it.'),
        !, nl.

take(money(X)) :-
		i_am_at(Location), within(money(X), Location), % place is valid
        retractall(at(money(X), _)),
		holding(money(Num)),
		retract(holding(money(Num))),
		NewNum is Num + X,
        assert(holding(money(NewNum))),
        write('Get it.'),
        !, nl.	

take(goldplate) :-
		i_am_at(Location), within(goldplate, Location),
		retractall(at(goldplate, _)),
		holding(money(Num)),
		retract(holding(money(Num))),
		NewNum is Num + 200,
        assert(holding(money(NewNum))),
        write('Get it.'),
        !, nl.	
		

take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.
		
take(X) :-
		i_am_at(Location), within(X, Location), % place is valid
		takeable(X), % can take it
        retractall(at(X, _)),
        assert(holding(X)),
        write('Get it.'),
        !, nl.

take(X) :-
		i_am_at(Location), within(X, Location), % place is valid
		\+takeable(X), % can take it
        write('Sorry, you cannot take it.'),
        !, nl.
		
take(_) :-
        write('I do not see it here.'),
        nl.

/* These rules define the direction letters as calls to go 1. */
n :- go(n).
s :- go(s).
e :- go(e).
w :- go(w).

/* This rule tells how to move in a given direction. */
subtract_candles :-
		holding(candles(X)), X > 0,
		retract(holding(candles(X))), NewX is X -1,
		assert(holding(candles(NewX))).
subtract_candles.
		
/* move rules */
go(n) :-
		i_am_at(entrance),
		path(entrance, n, outside),
		write('Sucessfully escaped. You win!'), nl,
		win_judge, nl, !.

go(w) :-
        i_am_at(entrance),
        path(entrance, w, shop),
		\+ holding(armor_suit),
		write('You must wear an armor suit. Otherwise, the shop keeper would know you are escaping.'), nl,
		!.
		
go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
		write('Go to '), write(There), nl,
		subtract_candles,
		!.
go(_) :-
        write('You can''t go that way.').

/* last password */
input(i) :-
		retract(at(box, hidden_room)),
		assert(at(openbox, hidden_room)),
		write('Correct.'), !.
		
input(_) :-
		write('Incorrect.'), !.

/* This rule tells how to look about you. */
look :-
        i_am_at(Place),
        describe_ex(Place),
        nl,
		check, !.

/* check objects in some place */
lightable :- (
		(holding(candles(X)), X > 0);
		holding(flashlight)
		), !.

check :-
		i_am_at(Location),
		check(Location).
		
check(Place) :- % todo: add description
		\+ lightable,
		at(X, Place), dark_visible(X),
        write(X), write(' -- '), describe(X), nl,
        fail.
		
check(Place) :-
		lightable,
		at(X, Place),
        write(X), write(' -- '), describe(X), nl,
        fail.
		
check(_).

/* show my status */
status :-
		i_am_at(Place),
		write('You are at '), write(Place), nl,
		write('Your items:'), nl,
		holding(Obj),
		write(Obj), write(' -- '), describe_item(Obj), nl,
		fail.

status.


/* when to end */
win_judge :- holding(treasure),
			write('You got the treasure, perfect!'),
			finish, !.

win_judge :-
			write('There is some hidden treasure inside the castle.'), nl,
			write('Can you find it?'),
			finish, !.

/* This rule tells how to die. */
die :-
        finish.

/* describe the place, used for look */
describe_ex(Someplace) :- write('You are at '), write(Someplace), nl,
		describle_darkness(Someplace),
		describe(Someplace).

describle_darkness(Someplace) :- dark(Someplace),
		write('It is really dark in this room.'), nl.
describle_darkness(Someplace) :- \+ dark(Someplace),
		write('It is bright in this room.'), nl.

		

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
		write('Welcome to Escaping The CASTLE game.'), nl,
		write('You got trapped in a castle. Trying to escape.'), nl,
        write('Your goal is to find out items that helps you to escape.'), nl,
		write('Get some treasure back if you are lucky enough.'), nl,nl,
		write('These are operations you could take :'), nl,
        write('start.             -- to start the game.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction.'), nl,
        write('take(Object).      -- to pick up an object.'), nl,
        write('look.              -- to look at the room.'), nl,
        write('check(Place).      -- to check what are over there.'), nl,
		write('inventory.      -- to list all your belongs.'), nl,
		write('use(ObjA, ObjB). -- all actions, including combine things, unlock doors...'), nl,
        write('halt.              -- to end the game and quit.'), nl,nl,
		write('Remember:'), nl,
		write('If a room is too dark, you cannot see something.'), nl,
		write('Candles will be running out when moving.'), nl,
		write('Guards would let you pass unless you defeat them.'), nl,
        nl.

introduction :-
		write('When I wake up, I found my self in a warehouse.'), nl,
		write('The only thing I could remember is that I went to an old castle.'), nl,
		write('But got trapped and lost conciousness.'), nl,
		write('Where am I? It is really dark around.'), nl,
		write('I can only see a ray of light shines through the closed door.'), nl,
		nl.

inventory :- status.
/* This rule prints out instructions and tells where you are. */

start :-
		instructions,
		nl,
		introduction.		
