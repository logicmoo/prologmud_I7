/* TheSimsMini -- a mock up and heavily stripped down version of the all time favourite pc game The Sims® series. Consult the following to start game:   start.  */
:- dynamic iAmAt/1.
:- dynamic simMood/1.
:- dynamic working/1.
:- dynamic atMultiple/3.
:- dynamic lastUsed/1.
:- dynamic expired/1.
iAmAt(fridge).
simMood(50).
atMultiple(coin,trashcan,2).
atMultiple(fridge_part,trashcan,1).
atMultiple(oven_part,trashcan,1).
atMultiple(food,fridge,1).
usable(fridge_part,fridge).
usable(oven_part,oven).
usable(coin,computer).
usable(food,oven).
simCallable(singAGram).
simCallable(fridgeTechnician).
simCallable(ovenTechnician).
path(fridge,w,oven).
path(fridge,n,trashcan).
path(fridge,e,computer).
path(computer,e,bed).
path(oven,e,fridge).
path(trashcan,s,fridge).
path(computer,w,fridge).
path(bed,w,computer).
lastUsed(nothing).

hasMood(X):-
	simMood(Mood),
	Mood>X.
updateMood(Delta):-
	retract(simMood(Mood)),
	NewMood is Mood+Delta,
	assert(simMood(NewMood)),
	NewMood >= 100,
	finish.
updateMood(_).

updateLastUsedObject(X):-
	lastUsed(Object),
	retract(lastUsed(Object)),
	assert(lastUsed(X)).

at(X,Y):-
	atMultiple(X,Y,Count),
	Count > 0.

updateCount(X,Place,Delta):-
	at(X,Place),
	retract(atMultiple(X,Place,OldCount)),
	NewCount is OldCount+Delta,
	assert(atMultiple(X,Place,NewCount)).
updateCount(X,Place,Delta):-
	assert(atMultiple(X,Place,Delta)).

go(Direction):-
	iAmAt(Place),
	path(Place,Direction,NextLocation),
	retract(iAmAt(Place)),
	assert(iAmAt(NextLocation)),
	look,!.
go(_):-
	write('You cannot go that way').

s:-
	go(s).
w:-
	go(w).
e:-
	go(e).
n:-
	go(n).
i:-
	at(X,sim),
	write('there is a '),write(X),write(' in your pocket'),nl,
	fail.
i.

describe(fridge) :-
	working(fridge),
	write('Here is a fridge. The fridge is working now. You can grab food from fridge and cook it in the oven'), nl,!.

describe(fridge):-
	write('Here is a fridge. The fridge is broken. You cannot open it. Try to get yourself some parts to fix it. You can obtain parts from salvaging the trash can, or call a repair techinician for 1 coin.'),nl.

describe(oven):-
	working(oven),
	at(food,sim),
	write('Here is an oven. The oven is working now. You can cook food to boost your mood'),nl,!.

describe(oven):-
	working(oven),
	write('Here is an Oven. The oven is working now. But you don\'t have any food. Get some from the fridge'),nl,!.

describe(oven):-
	write('Here is an oven. The oven is broken. You cannot use it. Try to get yourself some parts to fix it. You can obtain parts from salvaging the trash can, or call a repair techinician for 1 coin.'),nl.

describe(bed):-
	write('Here is a bed. Have a good rest! You can elevate your mood up to 50 directly.'),nl.

describe(trashcan):-
	write('Here is a trash can. The trash can is a good place to find surprising stuff.'),nl.

describe(computer):-
	\+ expired(computer),
	write('You\'re at a computer. The computer has had the latest Sims game nstalled! Why not give it a try to boost mood?'),nl,!.

describe(computer):-
	write('You\'re at a computer. However you cannot use it because its only game has expired.'),nl.

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.           -- to start the game.'), nl,
	write('stat.            -- show current sim\'s mood value.'),nl,
        write('n.  s.  e.  w.   -- to go in that direction.'), nl,
	write('i               -- show what the player is holding.'),nl,
        write('take(Object).    -- to pick up an object.'), nl,
        write('use(Object).     -- to use an object in your pocket at the current location.'), nl,
        write('use.             -- to use an object.'), nl,
        write('look.            -- to look around you again.'), nl,
        write('instructions.	-- to see this message again.'), nl,
        write('halt.            -- to end the game and quit.'), nl,
	write('dial.            -- to call a service. call [fridge/oven]RepairTechnician to call a repair technician. Call SingAGram to call for an SingAGram to boost your mood. Both services cost 1 coin.'),nl,
        nl.
take(X):-
	at(X,sim),
	write('You have no room to hold it!'),
	nl,!,fail.

take(X):-
	iAmAt(Place),
	at(X,Place),
	updateCount(X,Place,-1),
	updateCount(X,sim,1),
	write('Ok.'),
	nl,!.

take(_):-
	write('The item isn\'t here.'),
	nl.

use:-
	iAmAt(fridge),
	working(fridge),
	at(food,fridge),
	hasMood(10),!,
	take(food),
	updateMood(-10),
	write('You searched hard and found some raw food.'),nl,
	updateLastUsedObject(fridge),!.
use:-
	iAmAt(fridge),
	working(fridge),
	hasMood(10),
	write('You searched hard and found no food. It took effort. Your mood went down by 10.'),nl,
	updateLastUsedObject(fridge),!.

use:-
	iAmAt(fridge),
	hasMood(10),
	write('You cannot use the fridge because it is broken.'),nl,!.
use:-
	iAmAt(fridge),
	write('Your sim is too tired to do that. Try get some sleep.'),nl,!.

use:-
	iAmAt(oven),
	working(oven),
	at(food,sim),
	write('You cooked some food. You elevated your mood by 20.'),nl,
	updateCount(food,sim,-1),
	updateMood(20),
	updateLastUsedObject(oven),!.
use:-
	iAmAt(oven),
	working(oven),
	at(food,sim),
	write('Your sim is too tired to operate the oven. Try get some sleep.'),nl,!.

use:-
	iAmAt(oven),
	working(oven),
	write('You must get food first before you can use the oven. Food is in the fridge'),nl,!.
use:-
	iAmAt(oven),
	write('The oven is broken. You must fix it before you can use it.'),nl,!.
use:-
	iAmAt(trashcan),
	lastUsed(trashcan),
	write('You just searched the trash can. It\'s empty now. Try salvage it again later for surpirses.'),nl,!.
use:-
	iAmAt(trashcan),
	at(fridge_part,trashcan),
	hasMood(20),
	take(fridge_part),
	write('Through hard dumpster diving, you found a fridge part. It seems like you can you can use it to fix your fridge. Your mod went down by 20 because the trash can smelled bad.'),nl,
	updateMood(-20),
	updateLastUsedObject(trashcan),!.
use:-
	iAmAt(trashcan),
	at(oven_part,trashcan),
	hasMood(20),
	take(oven_part),
	write('Through hard dumpster diving, you found an oven part. It seems like you can use it to fix your oven. Your mood went down by 20 because the trash can smelled bad.'),nl,
	updateMood(-20),
	updateLastUsedObject(trashcan),!.
use:-
	iAmAt(trashcan),
	working(oven),
	working(fridge),
	at(coin,trashcan),
	hasMood(20),
	write('Through hard dumpster diving, you found a coin. You can use it to do whatever you want. Your mood went down by 20 because the trash can smelled bad.'),nl,
	updateLastUsedObject(trashcan),!,
	take(coin),updateMood(-20),!.
use:-
	iAmAt(trashcan),
	hasMood(20),
	write('Oh no. You found nothing here after a hard search. You mood went down by 20.'),nl,
	updateMood(-20),!.
use:-
	iAmAt(trashcan),
	write('You\'re too tired to dumpster dive. You need to take a rest.'),nl,!.

use:-
	iAmAt(computer),
	\+ expired(computer),
	\+ lastUsed(computer),
	write('The Sims game is so much fun. You mood went up by 15. Unfortunately its licence just went expired, so your sim can no longer play it unless you pay 1 coin by using the pay command.'),nl,
	updateMood(15),
	updateLastUsedObject(computer),
	assert(expired(computer)),!.
use:-
	iAmAt(computer),
	\+ expired(computer),
	write('Your eyes are sour because of the intriguing Sims game. Take a break before playing it again!'),nl,!.
use:-
	iAmAt(computer),
	write('Your copy of The Sims game has expired. You need to pay 1 coin to renew your Sims game.'),nl,!.
use:-
	iAmAt(bed),
	hasMood(50),
	write('You are energized. You cannot fall into sleep.'),nl,
	updateLastUsedObject(bed),!.
use:-
	iAmAt(bed),
	simMood(Mood),
	updateMood(50-Mood),
	write('Your mood went up to 50 after a tight sleep.'),nl,
	updateLastUsedObject(bed),!.

use(Object):-
	\+ at(Object,sim),
	write('You do not have '),
	write(Object),
	write(' with you.'),nl,!.
use(Object):-
	iAmAt(Place),
	\+ usable(Object,Place),
	write('You do not know how to use '),
	write(Object),
	write(' at '),
	write(Place),nl,!.
use(_):-
	iAmAt(fridge),
	hasMood(20),
	updateMood(-20),
	write('You used your fridge part to fix the fridge. Your fridge is working now! Your mood went down by 20 due to the hard work.'),nl,
	updateCount(fridge_part,sim,-1),
	assert(working(fridge)),!.

use(_):-
	iAmAt(oven),
	hasMood(20),
	updateMood(-20),
	write('You used your oven part to fix the oven. Your oven is working now! Your mood went down by 20 due to the hard work.'),nl,
	updateCount(oven_part,sim,-1),
	assert(working(oven)),!.


use(_):-
	iAmAt(computer),
	expired(computer),
	write('You used your coin to renew your The Sims game licence. You can play it one more time!'),nl,!,
	retract(expired(computer)),
	updateCount(coin,sim,-1),!.
use(_):-
	iAmAt(computer),
	write('You do not need a coin to activate the game. Hurray up and play it when it\'s free!'),nl,!.

use(_):-
	write('You are too tired to fix anything. Try to get some sleep.'),nl,!.

dial(Someone):-
	\+ simCallable(Someone),
	write('You forgot the number of '),
	write(Someone),
	write('.'),nl,!.
dial(_):-
	\+ at(coin,sim),
	write('You do not have enough coin to call anyone'),nl,!.

dial(Someone):-
	Someone = fridgeTechnician,
	assert(working(fridge)),
	write('The fridge technician came and inspected your fridge and made sure your fridge is working properly. You lost 1 coin to pay for his work.'),nl,!.

dial(Someone):-
	Someone = ovenTechnician,
	assert(working(oven)),
	write('The oven technician came and inspected your oven and made sure your oven is working properly. You paid 1 coin to him for his hard work.'),nl,!.

dial(_):-
	write('\'Da-Da-Di-Da-Di, we like the party.\' sings the sing-a-gram. Your mood is elevated by 30 for this beautiful song. You lost 1 coin at the same time, though.'),nl
	,updateMood(30),!.


look:-
        iAmAt(Place),
        describe(Place),
        nl.
start:-
	instructions,
        look.

finish :-
        nl,
        write('Congratulations! Your mood is now maximized. You have reached your goal. Please enter the   halt.   command.'),
        nl, !.
stat:-
	simMood(Mood),
	write('Your current mood is '),write(Mood),write('.').



























