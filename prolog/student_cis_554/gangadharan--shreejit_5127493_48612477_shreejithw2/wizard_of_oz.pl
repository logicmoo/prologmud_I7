/* WIZARD OF OZ - A small game by Shreejit Gangadharan.
   Consult this file and issue the command:   start.  */

:- dynamic at/2, i_am_at/1, alive/1.   /* Needed by SWI-Prolog. */
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

/* This defines my current location. */

i_am_at(house_in_kansas).


/* These facts describe how the rooms are connected. */

path(house_in_kansas, n, kansas_landscape).
path(house_in_kansas, d, couch_in_kansas).
path(couch_in_kansas, u, field_in_munchkin_county).

path(field_in_munchkin_county, n, hardware_store).
path(hardware_store, s, field_in_munchkin_county).
path(hardware_store, u, hardware_store_attic).
path(hardware_store_attic, d, hardware_store).

path(field_in_munchkin_county, w, yellow_brick_road_east_1).
path(yellow_brick_road_east_1, e, field_in_munchkin_county).

path(yellow_brick_road_east_1, w, yellow_brick_road_east_2).
path(yellow_brick_road_east_2, e, yellow_brick_road_east_1).

path(yellow_brick_road_east_2, w, yellow_brick_road_east_3).
path(yellow_brick_road_east_3, e, yellow_brick_road_east_2).

path(yellow_brick_road_east_3, w, emerald_city) :- at(scarecrow, party), at(tin_man, party), at(lion, party), at(toto, inventory),
        at(silver_shoes, inventory).
path(yellow_brick_road_east_3, w, emerald_city) :- 
        write('You need the scarecrow, tin man and lion in your party and Toto and silver shoes in your inventory to see the '),
        write('Wizard of Oz! Go back and figure how to get them '),
        write('to join your party.'), nl, !, fail.

path(emerald_city, e, yellow_brick_road_east_3).


/* These facts tell where the various objects in the game
   are located. */

at(toto, couch_in_kansas).
at(silver_shoes, field_in_munchkin_county).
at(kiss, field_in_munchkin_county).

at(oil_can, hardware_store).
at(large_knife, hardware_store).

/* This fact specifies that the spider is alive. */

alive(ww_east).


/* These rules describe how to pick up an object. */

add :-  nb_getval(i_size, C), 
        CNew is C + 1, 
        nb_setval(i_size, CNew).

sub :-  nb_getval(i_size, C), 
        CNew is C - 1, 
        nb_setval(i_size, CNew).

take(X) :-
        at(X, inventory),
        write('You''re already holding it!'),
        nl, !.

take(_) :-
        add,
        nb_getval(i_size, CounterValue),
        nb_getval(i_limit, CounterLimit),
        CounterValue > CounterLimit,
        sub,
        write('You have too many items! Drop some.'),
        nl, !.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(at(X, inventory)),
        write('OK.'),
        nl, !.

take(_) :-
        write('I don''t see it here.'),
        nl.

/* These rules describe how to put down an object. */

drop(X) :-
        at(X, inventory),
        i_am_at(Place),
        retract(at(X, inventory)),
        assert(at(X, Place)),
        sub,
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

/* These rules show the inventory */

i :-
        at(X, inventory),
        write('You have a '),
        write(X),
        write(' in your inventory.'),
        nl, fail.

/* These commands are for listing the party */

p :-
        at(X, party),
        write(X),
        write(' is a part of your party.'),
        nl, fail.

/* These rules tell how to handle interact the lion and the spider. */

interact(tin_man) :-
        at(oil_can, inventory),
        i_am_at(yellow_brick_road_east_2),
        write('You apply the oil onto the tin man\'s joints and it\'s able to move again! It tells you that it needs to meet the Wizard'),
        write('the Wizard of Oz to get a heart. It decides to accompany you! '),
        assert((at(tin_man, party))),
        nl, !.
interact(tin_man) :-
        i_am_at(yellow_brick_road_east_2),
        write('The joins are rusted. The tin man could walk if they were lubricated.'), 
        nl, !.

interact(scarecrow) :-
        at(large_knife, inventory), 
        i_am_at(yellow_brick_road_east_1),
        write('You\'ve freed the scarecrow. It seemed to have tried cutting the branch it was on and got tied up. It tells you it '),
        write('needs to meet the Wizard of Oz to get a brain. It decides to accompany you! '),
        assert((at(scarecrow, party))),
        nl, !.
interact(scarecrow) :-
        write('You need to free him from the rope. You could cut it in some way.'), 
        nl, !.

interact(lion) :-
        at(toto, inventory), 
        i_am_at(yellow_brick_road_east_3),
        write('Toto runs after the cat and rescues the lion!. It tells you it '),
        write('needs to meet the Wizard of Oz to get courage. It decides to accompany you! '),
        assert((at(lion, party))),
        nl, !.

interact(lion) :-
        at(vodka_bottle, inventory), 
        i_am_at(yellow_brick_road_east_3),
        write('The \'liquid courage\' gives the lion a brief spell of courage to chase away the cat. It tells you it '),
        write('needs to meet the Wizard of Oz to get courage. It decides to accompany you! '),
        assert((at(lion, party))),
        nl, !.

interact(lion) :-
        write('The lion is too busy running from the cat. I wonder if you could chase the cat away with something?'), 
        nl, !.

interact(_) :-
        write('Nothing happens.'),
        nl, !.
/* This rule tells how to die. */

die :-
        !, finish.


/* Under UNIX, the   halt.  command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final  halt.  */

finish :-
        nl,
        write('The game is over. Please enter the   halt.   command.'),
        nl, !.


/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.                   -- to start the game.'), nl,
        write('n.  s.  e.  w.  u.  d.   -- to go in that direction.'), nl,
        write('take(Object).            -- to pick up an object.'), nl,
        write('drop(Object).            -- to put down an object.'), nl,
        write('interact.                -- interact with an object.'), nl,
        write('look.                    -- to look around you again.'), nl,
        write('instructions.            -- to see this message again.'), nl,
        write('halt.                    -- to end the game and quit.'), nl,
        write('i.                       -- to check the inventory.'), nl,
        write('p.                       -- to check people in your party.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        nb_setval(i_size, 0),
        nb_setval(i_limit, 3),
        instructions,
        look, !.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(kansas_landscape) :-
        write('A hurricane hits Kansas! Your house is blown away and you\'re left in the middle of nowhere! You die.'),
        finish, !.

describe(couch_in_kansas) :-
        write('You\'re playing with your dog Toto when your house starts to tremble and the windows start to shake. '),
        write('You look outside and it\'s a huge hurricane! You frantically search for something to grab on to. You find '),
        write('Toto barking furiously and pick him up. You two brace for impact and expect the worst. '),
        assert((at(toto, inventory))),
        add,
        nl,
        write('Press u to follow the hurricane (you can\'t do anything else).'),
        nl.
        
describe(house_in_kansas) :-
        write('Hello Dorothy! You\'re being raised by Uncle Henry and Aunt Em in the bleak landscape of a Kansas farm. '),
        write('One day when your Uncle and Aunt leave the house, and ask you to come with them to sell the wheat. '),
        nl,
        write('You can exit the house and join them by going north.'),
        nl,
        write('or stay in the house and laze around by sitting down.').

describe(field_in_munchkin_county) :-
        at(silver_shoes, inventory),
        write('Now that you\'re wearing the silver shoes, you ask the Good Witch of the North how to get back home. She tells you '),
        write('to go to the Emerald City and ask the Wizard of Oz.'),
        nl,
        write('You can go west to start on the yellow brick road.'),
        nl,
        write('There\'s also a hardware shop in the north.'),
        !.

describe(field_in_munchkin_county) :-
        write('Now that was no normal hurricane! You\'ve been transported to the Land of Oz! You\'re in a field in Munchkin County, '),
        write('and as you step out of the house, you see a lot of munchkins! You look at the bottom of the house and see a pair of feet. '),
        write('You\'ve killed the Wicked Witch of the East and have freed the munchkins from her evil grip. The Good Witch of the North '),
        write('teleports to you and removes the Silver Shoes from the witch and offers them to you. She also offers a kiss for protection.'),
        nl.

describe(house_in_kansas) :-
        at(silver_shoes, inventory),
        write('Well done Dorothy! You just defeated the Wicked Witch of the West and are now back in Kansas!'), nl, finish, !.

describe(hardware_store) :-
        write('You enter a hardware store with a really grumpy looking man sitting at the desk. You see there are two items for sale.'),
        nl,
        write('The exit is towards the south. There also seems to be a room upstairs.').

describe(hardware_store_attic) :-
        at(large_knife, inventory), 
        write('It\s dark and dusty. You cut through the spider-webs with your knife. You find a vodka_bottle lying beside a bunch of '),
        write('boxes. '),
        nl,
        assert((at(vodka_bottle, hardware_store_attic))),
        write('You think about the passage down to the store.'),
        nl.

describe(hardware_store_attic) :-
        write('It\s dark and dusty. There are way too many spider-webs blocking your way.'),
        nl,
        write('You need to go back down to the store.'),
        nl.

describe(yellow_brick_road_east_1) :-
        at(scarecrow, party),
        write('You see a tree with a piece of rope tied to a branch. You could'),
        nl,
        write('Proceed west along the yellow brick road or'),
        nl,
        write('backtrack east'),
        nl.

describe(yellow_brick_road_east_1) :-
        write('You hop along the yellow brick road and you see a \'scarecrow\' tied to a tree branch with a piece of rope. If only you '),
        write('could cut him down. You could'),
        nl,
        write('Proceed west along the yellow brick road or'),
        nl,
        write('backtrack east'),
        nl.

describe(yellow_brick_road_east_2) :-
        at(tin_man, party),
        write('You see a corrugated shed with lots of oil lying around. You could'),
        nl,
        write('Proceed west along the yellow brick road or'),
        nl,
        write('backtrack east'),
        nl.

describe(yellow_brick_road_east_2) :-
        write('You hop along the yellow brick road and you see a \'tin_man\' with too much rust to move. There\'s nothing you can do to'),
        write('help him now. You could'),
        nl,
        write('Proceed west along the yellow brick road or'),
        nl,
        write('backtrack east'),
        nl.

describe(yellow_brick_road_east_3) :-
        at(lion, party),
        write('You see an empty field. Do you'),
        nl,
        write('Proceed west along the yellow brick road or'),
        nl,
        write('backtrack east'),
        nl.

describe(yellow_brick_road_east_3) :-
        write('You hop along the yellow brick road and you see a dejected \'lion\' in the middle of a field being chased by a small cat. '),
        write('The lion is huge and strong, but seems to lack confidence. Is there something you can do to encourage it? '),
        nl,
        write('Proceed west along the yellow brick road or'),
        nl,
        write('backtrack east'),
        nl.

describe(emerald_city) :- 
        write('You\'re at the Emerald City! You wait for the Wizard of Oz. More to come in Part 2!'),
        nl, finish, !.