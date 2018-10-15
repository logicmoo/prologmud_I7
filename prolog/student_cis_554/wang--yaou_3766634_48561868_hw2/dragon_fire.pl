/* Dragon Fire, by Yaou Wang. */

:- dynamic i_am_at/1, at/2, holding/1, money/1, cost/2, alive/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

/* These are the initial conditions of the game. */

money(100).

i_am_at(home).

path(home, n, closet) :- holding(key).
path(home, n, closet) :-
      write('The door seems to be locked... you seem to have left the key somewhere...'), nl,
      !, fail.
path(home, s, field).
path(closet, s, home).
path(field, n, home).
path(home, e, town).
path(town, w, home).
path(town, n, shop).
path(town, e, mountain).
path(town, s, forrest).
path(shop, s, town).
path(forrest, n, town).
path(forrest, s, unicorn).
path(unicorn, n, forrest).
path(mountain, w, town).
path(mountain, e, dragon).
path(dragon, w, mountain).

at(translator, shop).
at(dress, shop).
at(ring, shop).
at(necklace, shop).
at(sword, shop).
at(key, field).

alive(unicorn).
alive(dragon).

cost(sword, 50).
cost(translator, 1000).
cost(dress, 500).
cost(ring, 1000).
cost(necklace, 1050).

/* These rules govern how to buy things with money (the limited resource) */

affordable(X, Money, Leftover) :-
      cost(X, Cost),
      Money > Cost, Leftover is Money - Cost.

buy(X) :-
      holding(X),
      write('You''re already holding it!'),
      !, nl.

buy(X) :-
      at(X, shop),
      i_am_at(shop),
      cost(X, Cost),
      money(Money),
      affordable(X, Money, Leftover),
      retract(money(Money)),
      assert(money(Leftover)),
      retract(cost(X,Cost)),
      take(X),
      write('You have bought it!'), nl,
      write('You now have '), write(Leftover), write(' dollars left.'),
      !, nl.

buy(_) :-
      i_am_at(shop),
      write('You can''t afford it!'), nl, !, fail.

buy(_) :-
      write('There is nothing to buy!'), nl, !, fail.

/* These rules describe how to pick up an object. */

take(X) :-
        i_am_at(shop),
	at(X, shop),
	cost(X, _),
        write('You have to buy it!'),
        nl, !, fail.

take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract((at(X, Place))),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl, !, fail.


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

/* The inventory command */

i :-
        holding(X),
        write('You are holding a '), write(X), nl,
        fail.
i.

/* Checks how much money you have */

wallet :-
        money(X),
        write('You have '), write(X), write(' gold coins in your wallet.'), nl.

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


/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(shop) :-
        at(X, shop),
        cost(X, Y),
        write('The '), write(X), write(' costs '),
        write(Y), write(' gold coins.'), nl,
        fail.

notice_objects_at(_).

/* These rules tell how to handle killing the dragon and the unicorn */

kill :-
        i_am_at(unicorn),
        retract(alive(unicorn)),
        write('You have caught the unicorn off-guard and killed it!'), nl,
        write('However, before dying the unicorn cast a curse to this'), nl,
        write('land and you have been hit! You are now cursed to forever'), nl,
        write('roam this land in a mindless state.'), nl,
        die, !.

kill :-
        i_am_at(dragon),
        alive(dragon),
        holding(magic_sword),
        retract(alive(dragon)),
        assert(holding(crown)),
        write('A great battle ensues and you try to attack the dragon on all'), nl,
        write('sides. The dragon flew to the sky and showered fireballs on you!'), nl,
        write('With one desperate attempt you threw your enchanted sword with all'), nl,
        write('your force and... behold! It penetrated the head of the dragon!'), nl,
        write('You have slain the dragon with your magical sword! From its'), nl,
        write('liar you have retrieved the lost crown.'), nl, !.

kill :-
        i_am_at(dragon),
        alive(dragon),
        holding(sword),
        write('You slashed the dragon with your sword, but it is too dull to have'), nl,
        write('any effects! The dragon, now full of fury, killed you with its sharp'), nl,
        write('claws!'), nl, die, !.

kill :-
        i_am_at(dragon),
        alive(dragon),
        write('You have awakened and provoked the dragon. Your weak body with no'), nl,
        write('magical protection is no match for the dragon! With a big breath of'), nl,
        write('fire you are burnt alive.'), nl,
        die, !.

kill :-
        i_am_at(dragon),
        write('You cannot kill something that is already dead.'), nl, !.

kill :-
        write('There is nothing here to attack.'), nl.

/* This rule tells how to die. */

die :-
        write('You have died and lost the game.'), nl,
        !, finish.


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
        write('buy(Object).       -- to purchase an object with your money.'), nl,
        write('take(Object).      -- to pick up an object.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
        write('i.                 -- to see what you are currently holding.'), nl,
        write('wallet.            -- to see how much money you have in your wallet.'), nl,
        write('kill.              -- to attack an enemy.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(home) :-
        write('You are now comfortably in your home. To the north'), nl,
        write('is a little closet that you have not opened for a'), nl,
        write('while. To the south is the field that you have plowed.'), nl,
        write('To the east is the center of the town that you live in.'), nl,
        write('The town you live in is terrorized by a new dragon that'), nl,
        write('has arrived on the scene, killed the king, and took'), nl,
        write('the royal crown to the mountain. You, as the best'), nl,
        write('warrior in town, wants to kill the dragon and be hailed'), nl,
        write('as the hero of all time.'), nl.

describe(field) :-
        write('You are in the field of corn that you have planted.'), nl,
        write('To the north is your own home.'), nl.

describe(closet) :-
	money(X),
	Y is 1000 + X,
        retract(money(X)),
        assert(money(Y)),
        write('Turns out you hid all your savings in your closet! You'), nl,
        write('have found 1000 gold coins!'), nl.

describe(town) :-
        holding(crown),
        write('Congratulations! You have successfully returned the crown'), nl,
        write('back to the town and killed the dragon. You will be hailed'), nl,
        write('as the greatest hero!'), nl,
        write('You have won the game!'), nl,
        finish, !.

describe(town) :-
        write('You are now at the town center. To the north of you is the'), nl,
        write('town''s only shop. To the south of you is a wild forrest.'), nl,
        write('To the east of you is the tall mountain where the dragon is.'), nl,
        write('To the west of you is your own home. Rumor is spread here'), nl,
        write('that you need a magical sword to successfully slay the dragon'), nl,
        write('and also that there is a magical unicorn south of the forrest'), nl,
        write('that can help enchant any weapon.'), nl.

describe(shop) :-
        write('You are now in a little shop. To the south of you is the'), nl,
        write('town center.'), nl.

describe(forrest) :-
        write('This is a magical forrest. To the south is a glowing unicorn.'), nl,
        write('To the north is the town center.'), nl.

describe(unicorn) :-
        holding(translator),
        holding(sword),
        retract(holding(sword)),
        assert(holding(magic_sword)),
        write('With the magical translator the unicorn understood your need'), nl,
        write('and enchanted your sword with magic! Go north to return to the'), nl,
        write('forrest.'), nl, !.

describe(unicorn) :-
        holding(translator),
        write('With the magical translator the unicorn understood your need.'), nl,
        write('Please get a weapon so he can enchant it. Go north to return to'), nl,
        write('the forrest.'), nl, !.

describe(unicorn) :-
        write('You cannot communicate with the unicorn! Maybe the translator at'), nl,
        write('the shop will help... Go north to return to the forrest.'), nl.

describe(mountain) :-
        alive(dragon),
        write('You are standing now on top of a windy and dark mountain. To the'), nl,
        write('west is the path down to the town. To your east sleeps a giant'), nl,
        write('dragon.'), nl, !.

describe(mountain) :-
        write('You are standing now on top of a windy and dark mountain. To the'), nl,
        write('west is the path down to the town. To your east is a dead body of'), nl,
        write('a giant dragon.'), nl.

describe(dragon) :-
        alive(dragon),
        write('You are face to face with a sleeping dragon... it is the largest'), nl,
        write('creature you have ever seen. To escape to the mountain peak, go west.'), nl, !.

describe(dragon) :-
        write('You are standing on top of a dead dragon. Its body heat is still'), nl,
        write('rising into the air. Go west to go back to the mountain peak.'), nl.


