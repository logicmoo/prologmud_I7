/* Harry Potter and the Philosopher''s Stone 
    By Karthik Alle.
    Consult this file and issue the command: start. */

:- dynamic at/2, i_am_at/1, did_not_speak/1, spoke/1, h_magic/1, magic/1, not_dead/1, not_stunned/1, not_transformed/1, can_stupefy_troll/1, can_stupefy/1, alohomora/0, not_disarmed/1. /* Needed by SWI-Prolog. */

:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(h_magic(_)), retractall(not_dead(_)),
    retractall(not_stunned(_)), retractall(can_stupefy_troll(_)), retractall(not_transformed(_)), retractall(not_stunned(_)),
    retractall(can_stupefy(_)).

/* This defines my current location. */

i_am_at(gryffindor_fireplace).

/* These facts describe how the rooms are connected. */

path(gryffindor_fireplace, front, dumbledore).
path(gryffindor_fireplace, right, common_room_door) :- 
        did_not_speak(dumbledore),
        write('You cannot go out unless you speak to Dumbledore'), nl, !, fail.

path(gryffindor_fireplace, right, common_room_door).
path(gryffindor_fireplace, left, my_room).

path(dumbledore, left, my_room).
path(dumbledore, right, common_room_door).

path(my_room, back, gryffindor_fireplace).

path(common_room_door, front, great_hall).
path(common_room_door, left, strange_stairs).
path(common_room_door, back, gryffindor_fireplace) :-
    not_transformed(malfoy).

path(common_room_door, back, gryffindor_fireplace) :-
    write('Only a Gryffindor can enter the house.'), nl,
    write('You are now Malfoy, you can only enter the Slytherin House.'), nl,!, fail.

path(great_hall, back, common_room_door).
path(great_hall, front, slytherin_house) :- 
    not_transformed(malfoy), 
    write('Only a Slytherin can enter the house. Try the polyjuice potion in the potions room.'), nl, !, fail.

path(great_hall, front, slytherin_house).

path(great_hall, right, dumbledore_office) :-
    not_transformed(malfoy).

path(great_hall, right, dumbledore_office) :-
    write('Only Harry can enter Dumbledore''s office. You are now Malfoy and can only enter the Slytherin House.'), nl, !, fail.

path(great_hall, left, long_corridor).

path(long_corridor, left, girls_washroom).
path(long_corridor, right, potions_room).

path(potions_room, back, long_corridor).
path(girls_washroom, back, long_corridor).

path(slytherin_house, back, great_hall) :- 
    assert(not_transformed(malfoy)), 
    write('You have transformed back to Harry!'), nl, nl.

path(long_corridor, back, great_hall).
path(dumbledore_office, back, great_hall).

path(strange_stairs, back, common_room_door).

path(strange_stairs, front, locked_room) :-
        write('The door is locked'), nl,
        write('Perform the alohomora. Spell to open it.'), nl, !, fail.

path(locked_room, front, trap_door) :- at(invisible_cloak, in_hand).
path(locked_room, right, death).
path(locked_room, left, death).
path(locked_room, back, strange_stairs).

path(locked_room, front, trap_door) :- 
        write('Where is your cloak!?!'), nl, 
        write('You are not invisible, the dogs ate each of your heads off!'), nl, !, die.


path(trap_door, back, locked_room) :-
        write('You have climbed back to the room.'), nl.


path(trap_door, right, wizards_chess) :- 
        at(ron, in_hand),
        h_magic(X),
        X > 60,
        write('The chess game has begun and you have won! Thanks to Ron!'), nl,
        write('But you lost your magic by 60'), nl,
        decrease_magic_by(60),
        assert(at(magic_potion, wizards_chess)), !.


path(trap_door, right, wizards_chess) :-
        at(ron, in_hand),
        h_magic(X),
        X > 60,
        write('You don''t have enough Magic to play wizards chess'), nl, !, fail.

path(trap_door, right, wizards_chess) :-
        write('You cannot play wizards_chess like Ron! You can never win this alone!'), nl, 
        write('He is kidnapped by the Slytherins, go rescue him!'), nl, !, fail.

path(trap_door, back, trap_door).

path(wizards_chess, front, last_chamber) :-
        (
            at(hermione, in_hand) ;
            at(ron, in_hand)
        ),
        write('Leave both Hermione and Ron if you want to fight Voldemort!'), nl, !, fail.

path(wizards_chess, front, last_chamber).

path(last_chamber, _, voldemort_kill).

/* These facts tell where the various objects in the game
   are located. */

at(wand, my_room).
at(hermione, girls_washroom).
at(ron, slytherin_house).
at(chest, dumbledore_office).
at(philosopher_stone, last_chamber).

can_stupefy_troll(troll).
can_stupefy(vincent).
can_stupefy(gregory).
can_stupefy(voldemort).

/* This fact specify that harry spoke to dumbledore */

did_not_speak(dumbledore).
not_dead(troll).
not_transformed(malfoy).
not_stunned(gregory).
not_stunned(vincent).
not_stunned(voldemort).
not_disarmed(voldemort).

magic(X) :-
    X is 100,
    assert(h_magic(X)), !.

decrease_magic :-
    h_magic(X),
    Y is X - 10,
    retract(h_magic(_)),
    assert(h_magic(Y)), !.

decrease_magic_by(Z) :-
    h_magic(X),
    Y is X - Z,
    retract(h_magic(_)),
    assert(h_magic(Y)),
    my_magic, !.

my_magic :-
    h_magic(X),
    X < 0,
    write('You don''t have any magic left.'), nl, !.

my_magic :-
    h_magic(X),
    write('Your magic is now '), write(X), nl, !.

brew(magic_potion) :-
    retractall(h_magic(_)), 
    assert(h_magic(100)),
    write('Your magic has replenished to 100!'), nl, !.

brew(polyjuice_potion) :-
    retract(not_transformed(malfoy)), write('You have now transformed to Malfoy.'), nl, 
    write('You will transform back to Harry only when you enter and exit the Slytherin House.'), !.

alohomora :-
        at(hermione, in_hand),
        i_am_at(strange_stairs),
        h_magic(X),
        X < 20,
        write('You need 20 magic to perform alohomora.'), nl, !.

alohomora :-
        at(hermione, in_hand),
        i_am_at(strange_stairs),
        h_magic(X),
        X > 20,
        decrease_magic_by(20),
        retractall(i_am_at(_)),
        assert(i_am_at(locked_room)),
        describe(locked_room), !.

alohomora :-
        at(hermione, in_hand),
        i_am_at(dumbledore_office),
        h_magic(X),
        X < 20,
        write('You need 20 magic to perform alohomora.'), nl, !.

alohomora :-
        at(hermione, in_hand),
        i_am_at(dumbledore_office),
        h_magic(X),
        X > 20,
        retract(at(chest, dumbledore_office)),
        assert(at(invisible_cloak, dumbledore_office)),
        write('There is the invisible cloak in the chest'), nl, !.

alohomora :-
        at(hermione, in_hand),
        write('There is nothing here to open'), nl, !.

alohomora :-
        write('You need hermione to teach you how to perform the spell'), nl.

expelliarmus :-
        i_am_at(last_chamber),
        not_disarmed(voldemort),
        retractall(not_disarmed(_)),
        decrease_magic_by(70),
        write('You have disarmed Voldemort, now stupefy him.'), nl, !.

expelliarmus :-
        i_am_at(last_chamber),
        write('You have disarmed Voldemort, now stupefy him.'), nl, !.

expelliarmus :-
        i_am_at(last_chamber),
        decrease_magic_by(70),
        retractall(not_disarmed(_)),
        write('You have disarmed Voldemort, now stupefy him.'), nl, !.

expelliarmus :-
        write('There is no weapon here to disarm.'), nl, !.

stupefy(Y) :-
        at(wand, in_hand),
        i_am_at(last_chamber),
        not_disarmed(Y),
        write('Wrong spell! Voldemort peformed the Avada Kedavra and you are dead!'), nl, !, die.

stupefy(Y) :-
        at(wand, in_hand),
        i_am_at(last_chamber),
        can_stupefy(Y),
        decrease_magic_by(30),
        retract(not_stunned(Y)),
        assert(stunned(Y)),
        write('Your stun spell hit Voldemort and he Disapparated!'), nl, !.

stupefy(Y) :-
        at(wand, in_hand),
        can_stupefy_troll(Y),
        not_dead(troll),
        i_am_at(girls_washroom),
        h_magic(M),
        M < 70,
        write('You need 70 magic to stun the Troll!'), nl, !.

stupefy(Y) :-
        at(wand, in_hand),
        can_stupefy_troll(Y),
        i_am_at(girls_washroom),
        not_dead(troll),
        retract(not_dead(troll)),
        write('The troll was stunned!'), nl,
        decrease_magic_by(70), !.

stupefy(Y) :-
        at(wand, in_hand),
        can_stupefy_troll(Y),
        i_am_at(slytherin_house),
        h_magic(M),
        M < 30,
        write('You need 30 magic to stun '),write(Y),write('!'), nl, !.

stupefy(Y) :-
        at(wand, in_hand),
        can_stupefy(Y),
        i_am_at(slytherin_house),
        retract(not_stunned(Y)),
        assert(stunned(Y)),
        write(Y),write(' was stunned!'), nl,
        decrease_magic_by(30), !.

stupefy(Y) :-
        at(wand, in_hand),
        write('There is no '), write(Y), write(' here to stupefy.'), nl, !.

stupefy(_) :-
        write('You need a wand to perform spells'), nl, !.

/* These rules describe how to pick up an object. */

take(X) :-
        i_am_at(dumbledore_office),
        X = chest,
        write('You cannot take the chest, Perform the alohomora. spell to open it.'), nl, !.

take(_) :-
        i_am_at(last_chamber),
        not_stunned(voldemort),
        write('You cannot take the stone when Voldemort is there'), nl, !.

take(_) :-
        i_am_at(last_chamber),
        write('Well done Harry! You have retrieved the philosopher''s stone'), nl, finish, !.

take(_) :-
        i_am_at(wizards_chess),
        brew(magic_potion),
        retract(at(magic_potion, wizards_chess)),
        write('You drank the potion and your magic is replenished.'), nl, !.

take(_) :-
        i_am_at(girls_washroom),
        not_dead(troll),
        write('First stupefy the troll and then take Hermione!'), nl, !.

take(_) :-
        i_am_at(slytherin_house),
        not_stunned(vincent),
        not_stunned(gregory),
        write('Stupefy Vincent and Gregory and then take Ron!'), nl, !.

take(_) :-
        i_am_at(slytherin_house),
        not_stunned(vincent),
        write('Stupefy Vincent and then take Ron!'), nl, !.

take(_) :-
        i_am_at(slytherin_house),
        not_stunned(gregory),
        write('Stupefy Gregory and then take Ron!'), nl, !.

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
leave(X) :-
        drop(X).

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

inventory :- 
        write('You have the following in your inventory:'), nl,
        list_inventory.

list_inventory :-
        at(X, in_hand),
        write(X), nl,
        fail.

list_inventory :-
        write(''), nl.

/* These rules define the six direction letters as calls to go/1. */

front :- go(front).

back :- go(back).

right :- go(right).

left :- go(left).

i :- inventory.

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
        notice_objects_at(Place), !.


/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
        at(X, Place),
        write('There is '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).

/* This rule tells how to die. */

die :-
        !, finish.


/* Under UNIX, the   halt.  command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final  halt.  */

finish :-
        nl,
        write('The game is over. Please enter the halt. command.'),
        nl, !.


/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.                           -- to start the game.'), nl,
        write('front. back. right. left.        -- to go in that direction.'), nl,
        write('cast(Spell, On).                 -- to cast a spell on someone'), nl,
        write('spells.                          -- to see list of your spells.'), nl,
        write('take(Object).                    -- to pick up an object.'), nl,
        write('drop(Object). or leave(Object).  -- to put down an object.'), nl,
        write('i. or inventory.                 -- to check your inventory.'), nl,
        write('my_magic.                        -- to check how much magic is left.'), nl,
        write('look.                            -- to look around you again.'), nl,
        write('instructions.                    -- to see this message again.'), nl,
        write('halt.                            -- to end the game and quit.'), nl,
        nl, nl,
        write('Make sure you check your magic from time to time.'), nl, nl.

/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        write('Hello Harry Potter, Welcome to Hogwarts! You are at the Gryffindors Fireplace.'), nl,
        magic(_),
        look,
        write('Your magic is 100. Brew magic potion at the potions room to replenish your magic'), nl, nl.

/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

spells :-
        write('stupefy(X).          -- The Stunning Spell that renders a victim unconscious.'), nl,
        write('expelliarmus.        -- Unlocks and opens doors and windows.'), nl,
        write('alohomora.           -- A charm that unlocks and opens doors and windows'), nl.

describe(gryffindor_fireplace) :-
        did_not_speak(dumbledore),
        at(wand, in_hand),
        write('Dumbledore is standing by the fire infront of you and has been waiting to talk to you.'), nl,
        write('Your room is to the left.'), nl,
        write('If you want to go out of the Gryffindor Commons, the exit is to the right.'), nl, !.

describe(gryffindor_fireplace) :-
        did_not_speak(dumbledore),
        write('Dumbledore is standing by the fire infront of you and has been waiting to talk to you.'), nl,
        write('Your room is to the left, where your wand must be!'), nl,
        write('If you want to go out of the Gryffindor Commons, the exit is to the right.'), nl.

describe(gryffindor_fireplace) :-
        at(wand, in_hand),
        write('Dumbledore has vanished!'), nl,
        write('Your room is to the left.'), nl,
        write('If you want to go out, the exit is to the right.'), nl, !.

describe(gryffindor_fireplace) :-
        write('Dumbledore has vanished!'), nl,
        write('Your room is to the left, where you must have forgot your wand.'), nl,
        write('If you want to go out, the exit is to the right.'), nl.

describe(dumbledore) :-
        retract(did_not_speak(dumbledore)),
        assert(spoke(dumbledore)),
        assert(at(deluminator, gryffindor_fireplace)),
        retractall(i_am_at(_)),
        assert(i_am_at(gryffindor_fireplace)),
        write('Harry Potter, I have a dangerous task for you boy!'), nl,
        write('You need to retrieve the Philosopher''s stone before it falls into the wrong hands.'), nl,
        write('Use all the magic you have learnt. But use them carefully, as you have limited magic power.'), nl,
        write('Remember to always hold your friends.'), nl,
        write('These are dark times Harry, really dark times!'), nl, !.

describe(dumbledore) :-
        describe(gryffindor_fireplace).

describe(my_room) :-
        at(wand, in_hand), !, write('Ron is not in your room, wonder where he is!'), nl.

describe(my_room) :-
        write('Ron is not in your room, wonder where he is!'), nl,
        write('To perform magic, you need your wand.'), nl.

describe(common_room_door) :-
        write('You are infront of the Gryffindor''s Common Door, with the fat lady staring at you.'), nl,
        write('Turn back if you want to enter the Gryffindor''s Common'), nl,
        write('Go front towards the Great Hall.'), nl,
        write('There is a mysterious stairs leading to an old door to your left.'), nl,
        write('It was never there before, maybe it leads to the Philosopher''s stone.'), nl.

describe(strange_stairs) :-
        write('You are infront of an old door! It seems to be locked.'), nl,
        write('You can go front and perform the alohomora. Spell.'), nl,
        write('You can go back to your Gryffindor common room door.').

describe(great_hall) :-
        write('You are at the great hall is lit by thousands and thousands of candles.'), nl,
        write('Straight ahead is the path to Slytherin''s house.'), nl,
        write('To your left is a long corridor.'), nl,
        write('To your right is the Gargoyle Corridor that leads to Dumbledore''s Office.'), nl, nl,
        write('Turn back if you want to go to Gryffindor''s Commons'), nl, nl,
        describe(neville).

describe(neville) :-
        not_transformed(malfoy),
        at(hermione, in_hand),
        at(ron, in_hand),
        write('Neville Longbottom says ''Good that you saved both! Now go get the stone.'''), nl.

describe(neville) :-
        not_transformed(malfoy),
        at(hermione, in_hand),
        write('Neville Longbottom says'), nl,
        write('''Good that you saved Hermione.'),nl,
        write('Ron has been kidnapped by Malfoy! He must be in the Slytherin''s House'''), nl.

describe(neville) :-
        not_transformed(malfoy),
        at(ron, in_hand),
        write('Neville Longbottom says'), nl,
        write('''Good that you saved Ron.'), nl,
        write('I heard that Hermione has been crying all day in the girls washroom.'''), nl.

describe(neville) :-
        not_transformed(malfoy),
        write('Neville Longbottom runs to you and says'),nl, 
        write('''Hey Harry! I heard that Hermione has been crying all day in the girls washroom.'), nl, 
        write(' And Ron has been kidnapped by Malfoy! He must be in the Slytherin''s House'''), nl.

describe(neville).

describe(slytherin_house) :-
        not_stunned(vincent),
        not_stunned(gregory),
        write('You are inside the Slytherin House.'), nl,
        write('Vincent and Gregory are guarding Ron.'), nl,
        write('Stupefy each of them and rescue Ron!'), nl.

describe(slytherin_house) :-
        not_stunned(vincent),
        write('You are inside the Slytherin House.'), nl,
        write('Vincent is guarding Ron, while Gregory is in a stunned state.'), nl,
        write('Stupefy Vincent rescue Ron!'), nl.

describe(slytherin_house) :-
        not_stunned(gregory),
        write('You are inside the Slytherin House.'), nl,
        write('Gregory is guarding Ron, while Vincent is in a stunned state.'), nl,
        write('Stupefy Gregory and rescue Ron!'), nl.        

describe(slytherin_house) :-
        write('You are inside the Slytherin House.'), nl,
        write('Vincent and Gregory are in a stunned state'), nl.

describe(dumbledore_office) :-
        at(deluminator, in_hand),
        write('There are a vast number of portraits of past headmasters, all evidently asleep.'), nl.


describe(dumbledore_office) :-
        write('The Office is too dark, you can''t see anything.'), nl,
        write('Dumbledore must have left his deluminator when he vanished.'), nl,
        write('Go Pick it Up!'), nl, fail.

describe(potions_room) :-
        write('You can brew potions, following specific recipes and using ingredients.'), nl,
        write('brew(magic_potion). to heal your magic power.'), nl,
        write('brew(polyjuice_potion). to turn into Malfoy so that you can enter the Slytherin House.'), nl,
        write('Once you are done, go back to the Long Corridor').

describe(long_corridor) :-
        at(hermione, in_hand),
        write('You have reached the end of corridor.'), nl,
        write('There is the girls washroom to your right.'), nl,
        write('The potions room to your right.'), nl,
        write('Turn back to go to the Great Hall.').

describe(long_corridor) :-
        write('You have reached the end of corridor.'), nl,
        write('A troll is eventually making its way from the dungeons up to the first floor, heading into the girls bathroom to your left. Rescue hermione!'), nl,
        write('There is the potions room to your right.'), nl,
        write('Turn back to go to the Great Hall.').

describe(girls_washroom) :-
        not_dead(troll),
        write('The troll is attacking hermione! Cast a spell on the troll and save her!'), nl, !.

describe(girls_washroom) :-
        at(hermione, in_hand),
        write('The girls washroom is empty.'), nl, !.
        
describe(girls_washroom) :-
        write('You are at the girls washroom'), nl.

describe(locked_room) :-
        at(invisible_cloak, in_hand),
        write('Stand still! There is a three headed dog sleeping infront of you.'), nl,
        write('One wrong step and you are dead!'), nl, nl,
        write('There is a trap door in front of you, enter quietly.'), nl,
        write('Go back if you want to exit to the strange stairs.'), nl.

describe(locked_room) :-
        write('Stand still! There is a three headed dog sleeping infront of you.'), nl,
        write('There is a trap door infront of you.'), nl,
        write('You need to be invisible to enter the trap door.'), nl,
        write('Make a wrong step and you are dead!'), nl,
        write('Go back quitely to the strange stairs.'), nl.

describe(trap_door) :-
        write('To your right is the wizards chess.'), nl,
        write('Turn back if you want to slide up the trap door'), nl.

describe(death) :-
        at(invisible_cloak, in_hand),
        write('Even though you are invisible, you are noisy.'), nl,
        write('The dogs woke up and ate you!'), !, die.

describe(wizards_chess) :-
        write('There is all rumble over the floor after the chess game.'), nl,
        write('In front of you is the last chamber, and you can hear Voldemort''s Voice from inside'), nl,
        write('It is really dangerous to take Hermione and Ron with you inside that door.'), nl, !.

describe(last_chamber) :-
        not_disarmed(voldemort),
        not_stunned(voldemort),
        write('There is Voldemort holding his wand at you and yelling:'), nl,
        write('''Harry Potter, the boy who lived! You cannot take the stone when I am here!'''), nl, nl,
        write('Disarm him using the expelliarmus. charm or he will kill you.'), nl,
        write('Do something else and you are dead!'), nl.

describe(last_chamber) :-
        not_disarmed(voldemort),
        write('Stupefy Voldemort and take the stone.'), nl,
        write('Do something else and you are dead!'), nl.

describe(last_chamber) :-
        write('Take the philosopher''s stone to win the game!'), nl.

describe(voldemort_kill) :-
        write('Wrong Move! Voldemort peformed the Avada Kedavra and you are dead!'), nl, die, !.