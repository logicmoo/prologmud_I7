/* Outrun, by Ziyi Yang */

:- dynamic i_am_at/1, car_at/1, at/2, holding/1, locked/1, incar/0, ignited/0,
	miles/1, answer/1, stolen/0.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(car_at(_)),
	retractall(holding(_)), retractall(locked(_)), retractall(incar),
	retractall(ignited), retractall(miles(_)), retractall(answer(_)),
	retractall(stolen).

path(living_room, n, garage).
path(garage, s, living_room).

path(living_room, s, bedroom).
path(bedroom, n, living_room).

path(living_room, e, study).
path(study, w, living_room).

path(living_room, w, bathroom).
path(bathroom, e, living_room).

path(garage, w, shelf).
path(shelf, e, garage).

path(downtown, s, mall).
path(mall, n, downtown).

/* The distance between each two places. */
road(garage, 5, gasstation).
road(garage, 30, bank).
road(garage, 100, downtown).

road(gasstation, 5, garage).
road(gasstation, 25, bank).
road(gasstation, 110, downtown).

road(bank, 30, garage).
road(bank, 25, gasstation).
road(bank, 90, downtown).

road(downtown, 100, garage).
road(downtown, 110, gasstation).
road(downtown, 90, bank).

/* Items you can take with. */
at(key, living_room).
at(smartphone, study).
at(charger, bathroom).
at(battery, shelf).

/* This is player's initial state */
i_am_at(living_room).
car_at(garage).
locked(car).
locked(safe).
miles(10).

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
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.


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

/* This rule tells how to move in a given direction. */

go(_) :-
		incar,
		write('You are in the car. You can only drive.'), nl, !, fail.

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
		incar, car_at(X),
		write('The car is in the '), write(X), nl,
		inspect_car, nl, inspect_fuel, !.

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

inventory :- write('Items you have: '), nl,
	holding(X), write(X), nl, fail.
inventory :- true.

i :- inventory.

getin :-
	incar,
	write('You are already in your car.'), nl, !, fail.

getin :-
	\+ (i_am_at(X), car_at(X)),
	write('You are not with your car.'), nl, !, fail.

getin :-
	locked(car),
	write('Your car is locked. You can''t get in.'), nl, !, fail.
	
getin :-
	assert(incar),
	write('Now you are in your car.'), nl,
	look.
	
getoff :-
	incar,
	retract(incar),
	write('You are now out of your car.'), nl,
	car_at(X), describe(X), nl, notice_objects_at(X), !.
	
getoff :-
	write('You are already out of your car.'), nl, !, fail.
	
drive(_) :-
	\+ incar,
	write('You are not in your car. You can''t drive.'), nl, !, fail.
	
drive(_) :-
	\+ ignited,
	write('You have not ignited your car. You can''t drive.'), nl, !, fail.

drive(Here) :-
		car_at(Here),
		write('You can''t drive to the same place.'), nl, !, fail.

drive(Somewhere) :-
		\+ road(_, _, Somewhere),
		write('You can''t drive there'), nl, !, fail.
		
drive(There) :-
		car_at(Here), i_am_at(Here),
		road(Here, Dist, There),
		retract(car_at(Here)), assert(car_at(There)),
		retract(i_am_at(Here)), assert(i_am_at(There)),
		miles(X), Y is X - Dist,
		retract(miles(X)),
		assert(miles(Y)), fail.

drive(downtown) :-
		holding(smartphone),
		write('Along the long way to the downtown, someone kept'), nl,
		write('calling your phone. You have to answer it'), nl,
		write('and your car strayed away. You crashed into a light'), nl,
		write('pole and got injured. You will need to spend your'), nl,
		write('weekend at hospital.'), nl, !, die.

drive(_) :-
		holding(smartphone),
		write('When driving, someone kept calling your phone.'), nl,
		write('Luckily the travel is short so everything is fine.'), nl,
		write('But you started to wonder whether should you get away'), nl,
		write('from the phone...'), nl, nl, fail.

drive(_) :- !, look.

use(_) :-
	incar,
	write('You are inside your car. Focus on driving,'), nl,
	write('don''t use any items!'), nl, !, fail.

use(safe) :-
	i_am_at(bedroom), \+ locked(safe),
	write('The safe is already unlocked.'), nl, !.

use(safe) :-
	i_am_at(bedroom),
	write('What''s the combination? You can hardly remember it.'), nl,
	write('But you decided to give it a try.'), nl,
	write('Enter the combination and end with a period.'), nl,
	read(X), X == 1248,
	assert(holding(bankcard)),
	retract(locked(safe)),
	write('Great! You opened it up and got your banking card.'), nl, !.
	
use(safe) :-
	i_am_at(bedroom),
	write('No, it does not open the safe.'), nl, !, fail.
	
use(X) :-
	\+ holding(X),
	write('Your don''t have a '), write(X), write(' .'),
	nl, !, fail.
	
use(key) :-
	\+ (i_am_at(X), car_at(X)),
	write('Your are not with your car.'), nl, !, fail.
	
use(key) :-
	locked(car),
	retract(locked(car)),
	write('The car is now unlocked.'), nl, !.
	
use(key) :-
	assert(locked(car)),
	write('The car is now locked.'), nl, !.

use(smartphone) :-
	\+ holding(charger),
	write('The smart phone is not working. The battery is completely'), nl,
	write('dead. You need a charger.'), nl, !, fail.
	
use(smartphone) :-
	write('You turned on your smart phone with the charger connected'), nl,
	write('to the wall and found a memo with an unannotated number: 1248.'), nl, !.
	
use(battery) :-
	i_am_at(X), car_at(Y), X \= Y,
	write('You are not with your car.'), nl, !, fail.

use(battery) :-
	assert(ignited),
	retract(holding(battery)),
	write('You replaced your car''s battery and you can now ignite'), nl,
	write('your car.'), nl, !.
	
use(_) :-
	write('Your can''t use it here.'), nl, !, fail.

/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

finish :-
        nl,
        write('The game is over. Please enter the "halt." command.'),
        nl.

die :-
		!, finish.

/* This rule just writes out game instructions. */

instructions :-
        nl,
		write('You are living in a large house in rural area,'), nl,
		write('away from downtown. Your neighbourhood has all been'), nl,
		write('out for vacation. Void of anything meaningful to do,'), nl,
		write('you decided to spend the weekend in front of TV with'), nl,
		write('your favourite potato chips. Realizing you have just'), nl,
		write('run out of chips yesterday, you decided to drive all'), nl,
		write('the way to the downtown to get your favourite snack'), nl,
		write('so that you can really enjoy your endless weekend.'), nl, nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.           -- to start the game.'), nl,
        write('n.  s.  e.  w.   -- to go in that direction.'), nl,
        write('take(Object).    -- to pick up an object.'), nl,
        write('drop(Object).    -- to put down an object.'), nl,
		write('use(Object).     -- to use an object.'), nl,
		write('getin.           -- to get in your car.'), nl,
		write('getoff.          -- to get off your car.'), nl,
		write('drive(Place).    -- to drive your car to other places (only when you are in the car).'), nl,
        write('look.            -- to look around you again.'), nl,
        write('instructions.    -- to see this message again.'), nl,
		write('inventory. i.    -- to show what items you have.'), nl,
        write('halt.            -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.

describe(living_room) :-
		holding(potato),
		write('You have finally gotten back to your house with your favourite'), nl,
		write('potato chips and you are ready to begin your TV series and potato'), nl,
		write('marathon! Enjoy the weekend!'), nl, !, finish.
		
describe(living_room) :-
		write('You are in your living room. You have your 108 inch'), nl,
		write('ultra HD television and your super soft leather couch here.'), nl,
		write('You decided not to turn on the TV because you don''t have'), nl,
		write('potato chips.'), nl, nl,
		write('To the north is your garage'), nl,
		write('To the south is your bedroom.'), nl,
		write('To the east is your study.'), nl,
		write('To the west is the bathroom.'), nl.
		
describe(study) :-
		write('You are in your study. All books here remain untouched'), nl,
		write('for 10 years since they have been bought. The only thing'), nl,
		write('meaningful to you is the WiFi coverage here.'), nl, nl,
		write('To the west is the living room.'), nl.
		
describe(garage) :-
		write('You are in your garage. You see your car down the steps.'), nl, nl,
		write('To the south is the living room.'), nl,
		write('To the west is the shelf.'), nl.
		
describe(bedroom) :-
		write('This is your bedroom. Your safe is here but you don''t know'), nl,
		write('what''s inside it, neither can you remember its combination.'), nl, nl,
		write('To the north is the living room.'), nl.

describe(bathroom) :-
		write('This is your bathroom. You have your automatic massage'), nl,
		write('bathtub here. But you only take showers.'), nl, nl,
		write('To the east is the living room.'), nl.
		
describe(shelf) :-
		write('This is your large shelf stacked with spare parts of your car.'), nl, nl,
		write('To the east is the garage.'), nl.
		
describe(gasstation) :-
		write('Welcome to Ultra Petrol gas station, the closest one in your town,'), nl,
		write('and you can only pay in cash.'), nl, nl,
		write('Do you want to refuel (yes./no.)? '),
		read(X), assert(answer(X)), fail.
		
describe(gasstation) :-
		answer(X), X = yes,
		holding(money),
		retract(miles(_)),
		assert(miles(300)),
		retract(answer(X)),
		write('Your tank is full now.'), nl, !.

describe(gasstation) :-
		answer(X), X = yes,
		retract(miles(_)),
		assert(miles(60)),
		retract(answer(X)),
		write('You didn''t bring enough money. You only added a few litres of fuel.'), nl, !.
		
describe(gasstation) :-
		retract(answer(_)),
		write('Ok.'), nl, !.
		
describe(bank) :-
		write('Welcome to Bank of Potato, the only branch near your county.'), nl,
		write('It''s weekend so the bank is closed. However you can still draw'), nl,
		write('some money from the auto teller machine, if you brought your'), nl,
		write('banking card.'), nl,
		\+ holding(bankcard),
		write('You didn''t bring your banking card.'), nl, !.
		
describe(bank) :-
		write('Do you want to withdraw money (yes./no.)? '),
		read(X), assert(answer(X)), fail.
		
describe(bank) :-
		answer(X), X = yes,
		\+ holding(money),
		assert(holding(money)),
		retract(answer(X)),
		write('You have withdrawn all the money in your card.'), nl, !.
		
describe(bank) :-
		answer(X), X = yes,
		retract(answer(X)),
		write('Your deposit is empty now. You can''t withdraw any more.'), nl, !, fail.
		
describe(bank) :-
		retract(answer(_)),
		write('Ok.'), nl, !.

describe(downtown) :-
		stolen,
		write('You got back to the parking lot and nowhere could you find'), nl,
		write('your car. It must have been stolen. You need to call for help,'), nl,
		write('and definitely, your happy weekend is no longer there.'), nl, !, die.
		
describe(downtown) :-
		write('You are in the downtown. People and shops are everywhere.'), nl,
		write('However you only care about the shopping mall to the south,'), nl,
		write('which sells your favourite potato chips. Just when you got'), nl,
		write('off your car, you saw some suspicious peeps on your super'), nl,
		write('luxury exotic limousine. Maybe you should not forget to'), nl,
		write('lock your car...'), nl, nl,
		write('To the south is the mall.'), nl.
		
describe(mall) :-
		write('You got to the mall. Non-fat, no-sugar, low sodium, no deal.'), nl,
		write('You just want to rush to the value pack snacks shelf.'), nl, fail.
		
describe(mall) :-
		\+ holding(potato),
		assert(holding(potato)),
		nl,
		write('There it is! You got your chips! You can''t wait to'), nl,
		write('enjoy it at home.'), nl, fail.
		
describe(mall) :-
		\+ locked(car),
		assert(stolen), fail.
		
describe(mall) :- true.

inspect_car :-
		\+ ignited,
		write('You try to start your car but it didn''t work. It seems the'), nl,
		write('starter has run out of battery and you need to replace it'), nl,
		write('with a new one. Fortunately you have foreseen it and bought'), nl,
		write('a new one the other day. Check it out on the shelf.'), nl, !.

inspect_car :-
		write('You started your car. You are ready to go.'), nl,
		write('Use drive(Place) command to get to other places.'), nl, nl,
		write('You can drive to:'), nl,
		road(X, Dist, Y), i_am_at(Z), X = Z,
		write(Y), write(' is '), write(Dist), write(' miles away.'), nl, fail.
inspect_car :- true.

inspect_fuel :-
		miles(X), X < 0,
		write('Your car has run out of fuel on the half way.'), nl,
		write('You can only stand along the road hopelessly begging for help.'), nl,
		write('Your weekend is ruined.'), !, die.

inspect_fuel :-
		miles(X), X =< 10,
		write('Your car is low on fuel. You can only drive '),
		write(X), write(' miles until you refuel.'), !.

inspect_fuel :-
		miles(X),
		write('You can drive '), write(X), write(' miles.').