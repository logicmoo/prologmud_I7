/* 'A gradstudent's first 45 minutes in Philly', by Dominik Bollmann. */

:- dynamic time/1, i_am_at/1, at/2, holding/1.
:- retractall(time(_)), retractall(leaves(_,_,_)),
	retractall(at(_, _)), retractall(i_am_at(_)).

time(-1).
i_am_at(airport).

/* the leaves predicate specifies which transportation method leaves
 * when and how long it takes. */
leaves(subway, 20, 30).
leaves(shuttle, _, 25).

/* predicates acting on leaves facts. */
can_take(Transport, LeavingTime) :-
	time(Now), leaves(Transport, LeavingTime, _),
	Now < LeavingTime.

arrives_on_time(Transport, DurationTime) :-
	time(Now), leaves(Transport, _, DurationTime),
	X is (Now + DurationTime), (X < 45 ; X =:= 45).

/* the map of our game is encoded via paths. */
path(airport, n, infodesk).
path(infodesk, e, ticketcenter).
path(airport, e, sign).

path(sign, e, subway) :- holding(subway_ticket), holding(baggage).
path(sign, e, subway) :-
	holding(subway_ticket),
	write('With your subway ticket in your hand, you easily pass '), nl,
	write('the subway station entrance door. However, you notice '), nl,
	write('that you completely forgot about bringing ... YOUR BAGGAGE! '), nl,
	write('Will it still be there where you left it?'), nl, !, fail.
path(sign, e, subway) :-
	write('You need a valid subway ticket to pass the '), nl,
	write('subway entrance.'), nl,
	fail.

path(sign, n, ticketcenter).
path(outside, n, airport).

path(pickup, e, outside) :- holding(shuttle_ticket), holding(baggage).
path(pickup, e, outside) :-
	holding(shuttle_ticket),
	write('With your shuttle ticket in your hand, the driver of '), nl,
	write('Penn\s pickup service lets you pass. However, you notice '), nl,
	write('that you completely forgot about bringing ... YOUR BAGGAGE! '), nl,
	write('Will it still be there where you left it?'), nl, !, fail.
path(pickup, e, outside) :-
	write('In order to use Penn\'s pickup shuttle service, '), nl,
	write('the driver requests you to have a ticket. '), nl,
	fail.

path(dropoff, n, upenn).
path(storage, e, dropoff).

% make all paths two-way, i.e., undirected.
path(X, s, Y) :- path(Y, n, X).
path(X, w, Y) :- path(Y, e, X).

at(baggage, airport).
at(train, subway).
at(subway_ticket, ticketcenter).
at(pennflyer, infodesk).

/* These rules describe how to pick up an object. */

/* that clause takes care of a special case of taking an object,
   namely, taking the train at the subway station. */
take(train) :-
	describe(train), finish, !, fail.

take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
	assert(holding(X)),
        write('OK.'), nl,
	count_time,
        !, nl.

take(_) :-
        write('I don''t see it here. '),
        nl.

/* These rules describe how to put down an object. */

drop(X) :-
        holding(X),
        i_am_at(Place),
	retract(holding(X)),
        assert(at(X, Place)),
        write('OK.'), nl,
	count_time,
        !, nl.

drop(_) :-
        write('You aren''t holding it!'),
        nl.

investigate(pennflyer) :-
	holding(pennflyer),
	assert(at(shuttle_ticket, ticketcenter)),
	write('Reading the pennflyer in detail you learn that the '), nl,
	write('ticket center also sells tickets for Penn''s private '), nl,
	write('pickup shuttle service! '), nl, nl,
	count_time, nl, !.

investigate(Object) :-
	holding(Object),
	write('Investigating your '), write(Object), write(' doesn''t '), nl,
	write('reveal anything interesting.'), nl, nl,
	count_time, nl, !.

investigate(Object) :-
	write('You can''t investigate '), write(Object), write(' because '), nl,
	write('you aren''t holding it.'), nl.

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
	count_time, nl,
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl.


/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
	at(baggage, Place),
	write('Your baggage is here.'), nl,
	fail.

notice_objects_at(Place) :-
        at(X, Place), \+X = baggage,
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).
	

count_time :-
	time(Now), Now < 45,
	retract(time(Now)),
	NewTime is Now + 1,
	assert(time(NewTime)),
	X is NewTime + 15,
	write('It''s 8:'), write(X), write(' am now.'), nl,
	!.

count_time :-
	write('It''s 9:00 am and the graduate orientation at the SEAS '), nl,
	write('school is starting right now. You didn''t make it on time... :-('), nl,
	finish, !, fail.

i :- inventory.

inventory :-
	holding(baggage),
	write('You''re carrying your baggage with you.'), nl,
	fail.

inventory :-
	holding(Object), \+Object = baggage,
	write('You''re holding a '), write(Object), write('.'), nl,
	fail.

inventory.



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
        write('start.               -- to start the game.'), nl,
        write('n.  s.  e.  w.       -- to go in that direction.'), nl,
        write('take(Object).        -- to pick up an object.'), nl,
        write('drop(Object).        -- to put down an object.'), nl,
	write('investigate(Object). -- to investigate an object.'), nl,
	write('i. inventory.        -- to view your inventory.'), nl,
        write('look.                -- to look around you again.'), nl,
        write('instructions.        -- to see this message again.'), nl,
        write('halt.                -- to end the game and quit.'), nl,
        nl,
	write('Note: Everything you do takes 1 minute. So choose '), nl,
	write('your actions wisely! '), nl,
	nl.


/* This rule prints out instructions and tells where you are. */

start :-
	instructions,
	describe(start),
        look.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(start) :-
	write('You are D., a new master\'s student at Penn, '), nl,
	write('who has just arrived to the airport in Philly '), nl,
	write('for the very first time. You don\'t really know '), nl,
	write('anything about Philly and in particular on how '), nl,
	write('to get around. All you know is that you\'re '), nl,
	write('way too late and that you have to be at the '), nl,
        write('graduate student introductory meeting held by '), nl,
	write('the SEAS school at Penn at 9:00 am. '), nl, nl.

describe(airport) :-
	write('You are in the main airport hall. Nearby to '), nl,
	write('the east you see a big sign showing a subway map; '), nl,
	write('moreover, in the distant north you can spot an '), nl,
	write('info desk. Also, to the south there is the airport '), nl,
	write('exit.'), nl.

describe(infodesk) :-
	write('You are at the airport\'s info desk. The friendly '), nl,
	write('lady tells you that there is a subway line going '), nl,
	write('to Penn''s campus. She also tells you that'), nl,
	write('you can buy a subway ticket at the nearby ticket '), nl,
	write('center, which is located east; to the south is the '), nl,
	write('airport hall.'), nl.

describe(ticketcenter) :-
	write('You\'re at the ticket center. To obtain a ticket, type '), nl,
	write('take(some_ticket_type). To the west, is the info desk; to '), nl,
	write('the south there is a big subway sign.'), nl.

describe(sign) :-
	write('You\'re at the subway sign now. It tells you that '), nl,
	write('there is a connection going from the airport to '), nl,
	write('the Penn campus. To the east nearby you see the '), nl,
	write('subway station entrance; to the north you see '), nl,
	write('the ticket center; to the west you see the airport '), nl,
	write('hall.').

describe(subway) :-
	can_take(subway, LeavingTime),
	write('You\'re at the airport\'s subway station now. '), nl,
	write('You easily spot the subway that will take you to '), nl,
	write('Penn\'s campus. Unfortunately, however, you notice '), nl,
	write('that this subway will leave not earlier than in '), nl,
	time(Now), X is (LeavingTime - Now), write(X),
	write(' minute(s) from this station and that it will take '), nl,
	write('30 minutes in train to arrive at Penn''s campus. So '), nl,
	write('So you''re going to be late if you take it!'), nl, !.

describe(subway) :-
	write('You\'re at the airport\'s subway station now. '), nl,
	write('Unfortunately, however, the subway to Penn\'s campus '), nl,
	write('already left and there won\'t be another one within '), nl,
	write('the next 40 minutes. You better try looking for some '), nl,
	write('other transportation method!'), nl.

describe(train) :-
	write('You enter the train at the airport subway station, '), nl,
	write('wait for '),
	time(Now), leaves(subway, LeavingTime, _),
	X is LeavingTime - Now, write(X),
	write(' minutes, and then the subway train finally leaves. '), nl,
	write('But as you guessed the ride takes about 30 minutes '), nl,
	write('until you get off at Penn\'s campus and there is no '), nl,
	write('way to make it to graduate orientation at the SEAS '), nl,
	write('school on time anymore :-('), nl.

describe(outside) :-
	write('You are outside of the airport now. To the north, there '), nl,
	write('is the airport hall. To the west you see '), nl,
	write('Penn''s personal pickup service that will bring you '), nl,
	write('directly to the Penn campus! '), nl.

describe(pickup) :-
	arrives_on_time(shuttle, DurationTime), time(Now), Y is (Now + DurationTime),
	write('You arrive at the Penn shuttle pickup service with '), nl,
	write('all your baggage, get into it, and it takes you to Penn''s '), nl,
	write('campus in '), write(DurationTime), write(' minutes.'), nl, nl,
	write('Perfect! You made it to the SEAS orientation in '), nl,
	write(Y), write(' minutes, and even have time to make new friends there '), nl,
	write('before orientation starts. Congratulations, you successfully '), nl,
	write('completed your first challenge as a graduate student! :-)'), nl,
	finish.

describe(pickup) :-
	write('You arrive at the Penn shuttle pickup service with '), nl,
	write('all your baggage, get into it, and it needs '),
	leaves(shuttle, _, DurationTime), write(DurationTime), nl, 
	write('minutes to arrive at Penn''s campus.'), nl,
	write('If you hadn''t procrastinated too much at the airport, that '), nl,
	write('would have been sufficient to arrive on time. In your case, '), nl,
	write('however, you got to the shuttle service way too late to still '), nl,
	write('be on on time at the graduate orientation... :-('),
	finish.