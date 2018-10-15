% Space Escape, by Gabe Colton 

%makes i_am_at, at, holding, alive retract/assertable
%then retracts all instances of those predicates in the database
:- dynamic i_am_at/1, at/2, holding/1, alive/1, path/3, 
	health/1, loaded/1, hand/1, selfdes_init/1, infrared_vision/1, lasers/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)),
    retractall(path(_,_,_)), retractall(health(_)), retractall(loaded(_)),
    retractall(hand(_)), retractall(sd_init(_)), retractall(infrared_vision(_)),
    retractall(lasers(_)).

%sets initial some conditions.
hand(attached).
health(injured).
loaded(no).
alive(alien).
%self destruct sequence not initiated
selfdes_init(not_init).
infrared_vision(off).
lasers(active).

%Location at beginning of game
i_am_at('Engine Room').

%defines paths between rooms on the space ship

path('Engine Room', e, 'Arsenal').
path('Arsenal', w, 'Engine Room').

path('Engine Room', s, 'Infirmary').
path('Infirmary', n, 'Engine Room').

path('Infirmary', e, 'Dining Hall').
path('Dining Hall', w, 'Infirmary'). 

path('Dining Hall', s, 'Command Deck'):- 
	write('Opening this door requires the ID of a commanding officer'),
	nl,
	fail.

path('Command Deck', n, 'Dining Hall'). 

path('Command Deck', w, 'Hallway').
path('Hallway', e, 'Command Deck').

path('Hallway', w, 'Escape Pod Entrance'):-
%if you have used and are still holding the goggles, and if the lasers are active.
	\+ infrared_vision(off),
	holding(infrared_goggles),
	lasers(active),
	write('You are able to maneuver through the deadly lasers.'),
	nl, write('You deactivate the laser field.'),nl,
	retract(lasers(active)),!.

path('Hallway', w, 'Escape Pod Entrance').
	
  
path('Escape Pod Entrance', w, 'Escape Pod'):-
	holding(blaster),	
	write('Unfortunately the blaster is too big to fit in the Escape Pod.'),
	nl,!,
	fail.

path('Escape Pod Entrance', w, 'Escape Pod').


path('Escape Pod Entrance', e, 'Hallway').
path('Escape Pod', e, 'Escape Pod Entrance'). 


%defines where each item is
	at(blaster, 'Arsenal').
	at(burst_cartridge, 'Arsenal').
	at(id, 'Arsenal'). 
	at(infrared_goggles, 'Arsenal').
	at(medicine,'Infirmary').
	at(self_destruct_button, 'Command Deck').
	at(launch_button, 'Escape Pod'). 

%These rules describe how to pick up an object. 

take(id) :-
		i_am_at('Arsenal'), 
		hand(attached),
		at(id, 'Arsenal'),
		
		retract(hand(attached)),
		assert(at(hand,'Arsenal')),
		
		retract(at(id, 'Arsenal')),
		assert(holding(id)),
		
		write('When you try to take the id, you realize your commanding officer '),
		nl,
		write('is still holding it very tight. You pull harder'),
		nl,
		write('and you take the id, but you detach his hand from'),
		nl,
		write('his arm in the process.'),
		!, nl. 
		
take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(self_destruct_button):-
		write('You can''t take that button, but you can use it.'), !, fail.

take(launch_button):-
		write('You can''t take that button, but you can use it.'), !, fail.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('You have taken the '), write(X), write('.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.


% These rules describe how to put down an object. 

drop(infrared_goggles) :-
		holding(infrared_goggles),
        i_am_at(Place),
        retract(holding(infrared_goggles)),
        assert(at(infrared_goggles, Place)),
        assert(infrared_vision(off)),
        write('You drop the goggles on the ground.'),
        !, nl.
drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('You drop the '), write(X), write(' on the ground.'),
        !, nl.

drop(_) :-
        write('You aren''t holding it!'),
        nl.

%These rules describe how and where each object can be used.

%makes it so the only item you can use is a loaded blaster
%otherwise, you are injured and are forced to retreat to the Infirmary

use(blaster) :- 
		holding(blaster),
		loaded(yes),
		alive(alien),
		i_am_at('Dining Hall'),
		write('You blast the alien right in its weakspot'), nl,
		write('The alien is no longer a threat'),
		retract(alive(alien)), 
		!.

use(blaster) :- 
		i_am_at('Dining Hall'),
		alive(alien),
		health(healthy),
		retract(health(healthy)),
		assert(health(injured)), 
		use(blaster),
		nl,
		write('You''re injured by the alien and'),
		nl,
		write('forced to retreat to the Infirmary.'),
		nl,
		go(w),!.

use(blaster) :- 
		holding(blaster),
		loaded(no),
		write('You have to load it first!'), !.

use(_) :- 
		i_am_at('Dining Hall'),
		alive(alien),
		health(healthy),
		retract(health(healthy)),
		assert(health(injured)),
		nl,
		write('Before you can do that,'),
		nl,
		write('you''re injured by the alien and'),
		nl,
		write('forced to retreat to the Infirmary.'),
		nl,
		go(w),!.

use(id) :-
		holding(id),
		i_am_at('Dining Hall'),
		asserta(path('Dining Hall', s, 'Command Deck')),
		write('You swipe the ID card and enter the Command Deck'),
		nl,
		go(s),
		retract(path('Dining Hall', s, 'Command Deck')),
		!.
		

use(blaster) :- 
		holding(blaster),
		loaded(no),
		write('You have to load it first!'), !.


		
use(burst_cartridge):-
		holding(blaster),
		holding(burst_cartridge),
		retract(loaded(no)),
		assert(loaded(yes)),
		retract(holding(burst_cartridge)),
		write('Your blaster is now loaded'),!.

use(medicine) :-
		holding(medicine),
		health(injured),		
		alive(alien),
		i_am_at('Dining Hall'),
		write('You are now healthy, and your vision has returned'),
		retract(health(injured)),
		assert(health(healthy)),
		look,
		!.
		
use(medicine) :-
		health(injured),
		holding(medicine),
		write('You are now healthy, and your vision has returned'),
		retract(health(injured)),
		assert(health(healthy)),
		!.
use(medicine) :-
		holding(medicine),
		write('You don''t need to use the medicine '),
		write('because you''re already healthy'), !.		

use(self_destruct_button) :-
		selfdes_init(not_init),
		i_am_at('Command Deck'),
		holding(hand),
		write('You use your commanding officer''s severed hand to'),
		nl,
		write('initiate the ship''s self destruct sequence.'),
		nl, 
		write('The ship will self-destruct shortly after the escape pod launches.'),
		nl,
		retract(selfdes_init(not_init)),
		!.

		
use(self_destruct_button) :-
		selfdes_init(not_init),
		i_am_at('Command Deck'),
		write('The computer requires the finger prints of a commanding officer'),
		nl,
		write('to cause a system override and initiate the self destruct sequence.'),
		nl,
		!,fail.

use(self_destruct_button) :-
		i_am_at('Command Deck'),
		write('The self destruct sequence has already been initiated.'),
		!, nl.
		
		
use(infrared_goggles) :-
		retract(infrared_vision(off)),
		write('You put on the infrared goggles and now can see infrared signatures.'),
		nl,!.

use(launch_button):-
		i_am_at('Escape Pod'),
		\+ selfdes_init(not_init), 
		write('You successfully escape the ship.'),nl,
		write('You survived the alien onslaught, blew up the ship,'), nl, 
		write('and successfully destroyed the energy orb'),nl,
		write('preventing the rest of the alien army from using its power.'),
		!, finish. 
use(launch_button):-
		i_am_at('Escape Pod'),
		write('You have not initialized the ship''s self destruct sequence.'),
		nl, !, fail. 
		
use(hand) :- 
		holding(hand),
		i_am_at('Command Deck'),
		use(self_destruct_button),!.
		
use(X) :-
		holding(X),
		write('Looks like you can''t use that here'),!.

use(_) :-
		write('You can''t use something you don''t have').  
		

% These rules define the direction letters as calls to go/1. 

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).


% This rule tells how to move in a given direction. 

go(w) :- 
		i_am_at('Hallway'),
		infrared_vision(off),
	    lasers(active),
		write('The deadly lasers slice your body into 700 pieces.'),
		nl, !, die.
		
		
go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        look, !.

go(_) :-
        write('You can''t go that way.').


% This rule tells how to look about you. 

look :-
        i_am_at('Infirmary'),
        health(injured),
        describe('Infirmary'),
        notice_objects_at('Infirmary'),
        nl,!.
look :-
        i_am_at(Place),
        health(injured),
        describe(Place),
        write('Looks like you''re injured though. '), nl,
				write('Your vision is greatly impaired. '), nl,
				write('Better go find some medicine '), 
				write('before you''ll be able to see what''s here'),
				nl, !.
look :- 
		i_am_at(Place),		
        nl,
        describe(Place),
        nl, 
        notice_objects_at(Place),
        nl.


% These rules set up a loop to mention all the objects
  % in your vicinity. 

notice_objects_at(Place) :-
        at(X, Place),
        write('You see the '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).


% This rule tells how to die. 

die :-
        write('You died.'),
        finish.




finish :-
        nl,
        write('The game is over. Please enter the "halt." command.'),
        nl.


% This rule just writes out game instructions. 

help :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             -- to start the game.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction.'), nl,
        write('take(Object).      -- to pick up an object.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
        write('use(Object).       -- to use an object (e.g. push a button, shoot a gun)'), nl,
        write('look.              -- to look around you again.'), nl,
        write('i.                 -- to show your inventory.'), nl,
        write('help.              -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,nl,
        write('Your goal is to initiate the ship''s self destruct sequence'),nl,
        write('in order to destroy the energy orb that powers the ship'),nl,
        write('and then escape using the Escape Pod'), nl,nl.


% This rule prints out instructions and tells where you are. 

start :-
        help,
        look.



%this rule prints out everything the player is holding

i :- 
		holding(_),
		write('You are holding the following:'), nl,!,
		holding(X),
        write(X), nl,
        fail.	

% These rules describe the various rooms.  Depending on
  % circumstances, a room may have more than one description. 

describe(Place) :- 
		health(injured), 
		write('It seems like you''re in the '), 
		write(Place), nl. 

describe('Engine Room') :-
		write('You are in the Engine Room. Not much to see here'), 
		nl,		
		write('other than a large energy orb that supplies power to the ship.'),
		nl. 
		
describe('Infirmary') :-
		write('You are in the Infirmary. It is very bright in here.'),
		nl,
		write('The walls and cabinets are mostly empty,'),
		nl,
		write('and there is blood splattered everywhere.'),
		nl.

describe('Command Deck') :-
		write('You are in the Command Deck. Many of your fallen'),
		nl, 
		write('comrades are eviscerated and their entrails'),
		nl, 
		write('seem to be jamming up most of the controls.'),
		nl.

describe('Dining Hall') :-
		alive(alien),
		write('You are in the Dining Hall and see a murderous alien.'),
		nl,
		write('In your healthy state, it views you as a threat'),
		nl,
		write('It lunges at you!!!!'), 
		!,
		nl.
		
describe('Dining Hall') :-
		write('The alien''s amorphous body has'),
		nl,
		write('shrivelled into a pile of mucous').
		
describe('Arsenal') :-
		hand(attached), 
		write('You are in the Arsenal. Most of the ships weapons'),
		nl,
		write('stock was ransacked during the fight.'), 
		nl,
		write('You also see your commanding officer.'),
		nl, 
		write('He is dead and is holding his ID in his hand'),
		!, nl.

describe('Arsenal') :-
		at(hand, 'Arsenal'),
		write('You are in the Arsenal. Most of the ships weapons'),
		nl,
		write('stock was ransacked during the fight.'), 
		nl,
		write('You also see your commanding officer.'),
		nl, 
		write('He is dead and his severed hand lays next to his body.'),
		!, nl.

describe('Arsenal') :-
		write('You are in the Arsenal. Most of the ships weapons'),
		nl,
		write('stock was ransacked during the fight.'), 
		nl,
		write('You also see your commanding officer.'),
		nl, 
		write('His hand-less arm is unnerving '),
		nl.

describe('Hallway') :-
		write('You are in an unassuming hallway, and see the words'),
		nl, 
		write('Escape Pod written on a door to the west,'),
		nl.

describe('Escape Pod Entrance') :-
		
		write('You are now at the entrance to the Escape Pod.'),
		nl.
		
describe('Escape Pod') :-
		
		write('You are now in the Escape Pod'),
		nl,
		write('all you have to do is launch yourself to safety'), nl.
					
