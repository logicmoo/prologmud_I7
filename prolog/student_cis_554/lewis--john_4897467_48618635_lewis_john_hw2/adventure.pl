/* Look Before You Leap, by John Lewis. */

:- dynamic i_am_at/1, at/2, holding/1, dead/1, inspected/1, unvisited/1,
        dark/1, invisible/1, locked/1, empty/1, alive/1, health/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

/* The player's state */

i_am_at(hamlet).
health(100).

/* The paths between areas */

path(hamlet, n, apothecary).
path(outside_church, e, hamlet).
path(hamlet, e, narrow_pass).

path(hamlet, d, well) :-
    unvisited(well), !,
    retract(unvisited(well)),
    write('You decide to jump down the well. That was rather silly, '), nl,
    write('you land in a heap at the bottom only to notice there was a '), nl,
    write('ladder you could have used.'), nl,
    decrease_health(30).

path(hamlet, d, well).

path(outside_church, n, church).
path(beach, n, outside_church).

path(church, d, crypts) :-
        dark(crypts),
        holding(torch), !,
        retract(holding(torch)),
        retract(dark(crypts)),
        write('When you arrive in the crypt you light a series of braziers'), nl,
        write('and cast the spent torch aside, having sufficiently'), nl,
        write('illuminated the area.'), nl.

path(church, d, crypts).
path(crypts, u, church).

path(crypts, _, crypts) :-
        dark(crypts), !,
        write('You stumble blindly through the dark and hit your head.'), nl,
        write('That''s going to leave a mark.'), nl,
        decrease_health(20).

path(crypts, n, fancy_tomb).

path(narrow_pass, e, chasm).

path(chasm, e, outside_castle) :-
        invisible(bridge),
        write('You walk out over the chasm and promptly fall. It briefly '), nl,
        write('occurs to you that that wasn''t wise before you hit the bottom'),
        nl,
        !, die.

path(chasm, e, outside_castle) :-
        locked(castle),
        holding(ring), !,
        retract(locked(castle)),
        write('When you near the castle gates the signet ring you''re'), nl,
        write('wearing glows momentarily and the castle gates slowly'), nl,
        write('open granting you passage. Neat!'), nl.

path(chasm, e, outside_castle).

path(outside_castle, n, outside_castle) :-
        locked(castle), !,
        write('The castle gates are closed tight with no hope of being '), nl,
        write('forced open.'), nl.

path(outside_castle, n, castle).

path(castle, n, castle) :-
        alive(vampire), !,
        write('The vampire is not impressed by your bravery and takes a'), nl,
        write('sizable chunk out of your neck before you manage to stagger'), nl,
        write('away.'), nl,
        decrease_health(50).

path(castle, n, throne_room).

path(X, w, Y) :- path(Y, e, X).
path(X, s, Y) :- path(Y, n, X).
path(X, u, Y) :- path(Y, d, X).

/* The locations of items */
at(coin, well).
at(jar, apothecary).
at(sand, beach).
at(driftwood, beach).
at(crown, throne_room).

/* The state of areas */
unvisited(hamlet).
unvisited(well).
unvisited(castle).
dark(crypts).
invisible(bridge).
locked(castle).
empty(basin).
alive(vampire).

/* Prominent features of areas that can be inspected. */
feature_of(hamlet, well).
feature_of(hamlet, fire).
feature_of(beach, sand).
feature_of(hamlet, sky).
feature_of(beach, sky).
feature_of(outside_church, sky).
feature_of(narrow_pass, sky).
feature_of(chasm, sky).
feature_of(outside_castle, sky).
feature_of(fancy_tomb, sarcophagus).
feature_of(church, altar).
feature_of(chasm, chasm).
feature_of(outside_castle, gate).
feature_of(castle, vampire).

/* These rules describe how to pick up an object. */

take(crown) :-
        i_am_at(throne_room), !,
        write('You eagerly grab the crown and put it on your head. You'), nl,
        write('wait a few seconds but nothing magical happens, even so you'), nl,
        write('feel pretty cool wearing a crown.'), nl, nl,
        write('Congratulations you have beaten the game!'),
        finish.

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

d :- go(d).

u :- go(u).


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

/* This rule tells how to inspect a thing or item */

inspect(Object) :-
        i_am_at(Place),
        feature_of(Place, Object), !,
        describe_feature(Object),
        nl.

inspect(Object) :-
        holding(Object), !,
        write('A plain '), write(Object), write('.'), nl.

inspect(Object) :-
        i_am_at(Place),
        at(Object, Place), !,
        write('There is a '), write(Object), write(' here.'), nl.

inspect(_) :-
    write('Nothing to see here.'), nl.

/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).

/* This rule list all items the player is holding. */

i :-
    write('You are holding: '), nl,
    holding(X),
    write(X), nl,
    fail.

i.

/* This rule tells how to use an item */

use(X) :-
    \+ holding(X), !,
    write('You don''t have that.'), nl.

use(coin) :-
    i_am_at(church), !,
    retract(holding(coin)),
    retract(empty(basin)),
    write('You put the coin into the offering box and the basin fills'), nl,
    write('with holy water!'), nl.

use(jar) :-
    i_am_at(church),
    \+ empty(basin), !,
    retract(holding(jar)),
    assert(holding(holy_water)),
    write('You fill the jar with holy water.'), nl.

use(holy_water) :-
    i_am_at(castle), !,
    retract(holding(holy_water)),
    retract(alive(vampire)),
    write('You splash the vampire with the holy water. The vampire howls'), nl,
    write('in pain and vanishes in a puff of smoke. You feel rather proud'), nl,
    write('of yourself.'), nl.

use(sand) :-
    i_am_at(chasm), !,
    retract(holding(sand)),
    retract(invisible(bridge)),
    write('You start throwing sand at the gap and find that it sticks'), nl,
    write('in the air revealing the location of an invisible bridge.'), nl.

use(driftwood) :-
    i_am_at(hamlet), !,
    retract(holding(driftwood)),
    assert(holding(torch)),
    write('You light the end of the driftwood in the fire giving you a'), nl,
    write('makeshift torch.'), nl.

use(_) :-
    write('You can''t use that here'), nl.


/* This rule decreases the player health, causing them to die if
    their health reaches zero */

decrease_health(Dmg) :-
    health(MyHealth),
    retract(health(MyHealth)),
    NewHealth is MyHealth - Dmg, nl,
    write('You take '), write(Dmg), write('damage!'), nl,
    NewHealth > 0, !,
    assert(health(NewHealth)),
    write('You have '), write(NewHealth), write('health remaining.'), nl, nl.

decrease_health(_) :-
    die.

/* This rule tells how to die. */

die :-
        write('You have died.'),
        assert(dead(me)),
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
        write('d.  u.             -- to go down or up'), nl,
        write('i.                 -- list the items in your inventory.'), nl,
        write('take(Object).      -- to pick up an object.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
        write('use(Object).       -- to use an object.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('inspect(Object)    -- to look at an object or thing in the area.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(_) :-
        dead(me), !, true.

describe(hamlet) :-
        unvisited(hamlet),
        retract(unvisited(hamlet)),
        write('You are an intrepid treasure hunter and lately you''ve'), nl,
        write('been chasing rumors of a magical crown which has led'), nl,
        write('you to this remote region.'), nl, nl,
        fail.

describe(hamlet) :-
        write('You are in a small deserted hamlet. There is a dried up well'), nl,
        write('leading down here as well as a large bonfire. To the North is'), nl,
        write('is an open hut. Off to the West is a church and to the'), nl,
        write('there is a narrow_pass.'), nl.

describe(well) :-
        write('You are at the bottom of the well. There doesn''t'), nl,
        write('seem to be anywhere to go except up the ladder.'), nl.

describe(apothecary) :-
        write('You are inside what appears to have been the residence'), nl,
        write('of an apothecary. There are many shelves full of jars'), nl,
        write('of various substances.'), nl.

describe(outside_church) :-
        write('You are outside the church to the North. To the south'), nl,
        write('lies the beach.  The hamlet is to the East.'), nl.

describe(beach) :-
        write('You are on the beach. It is covered in white sand and'), nl,
        write('driftwood. To the North is the church.'), nl.

describe(church) :-
        empty(basin), !,
        write('You are in a small church. In the center is an altar with'), nl,
        write('an empty basin and an offering box. There are stairs'), nl,
        write('leading down to the crypt.'), nl.

describe(church) :-
        write('You are in a small church. In the center is an altar with'), nl,
        write('a basin of holy water and an offering box. There are'), nl,
        write('stairs leading down to the crypt.'), nl.

describe(crypts) :-
        dark(crypts), !,
        write('You are in the crypt. It is very dark, you can''t tell'), nl,
        write('which directions it goes.  There are stairs leading up'), nl,
        write('to the church.'), nl.

describe(crypts) :-
        write('You are in the crypt. To the north is a larger decorated'), nl,
        write('chamber.  There are stairs leading up to the church.'), nl.

describe(fancy_tomb) :-
        write('You are in the decorate chamber. There is a large'), nl,
        write('sarcophagus in the center of the room which is slightly'), nl,
        write('ajar.  It looks like it must have been someone important.'), nl.

describe(narrow_pass) :-
        write('You are in a narrow pass. To the West is the hamlet,'), nl,
        write('the path continues to the East.'), nl.

describe(chasm) :-
        invisible(bridge), !,
        write('To the East you can see a castle but your way is blocked'), nl,
        write('by a large chasm with no apparent way to cross it. The'), nl,
        write('narrow pass is back to the West.  While you stand there'), nl,
        write('pondering a bird bumps into the air in the chasm. Weird.'), nl.

describe(chasm) :-
        write('You are at the chasm. The sand covers the invisible bridge'), nl,
        write('making it safe to cross. To the East is a castle, to the'), nl,
        write('West is a narrow pass.'), nl.

describe(outside_castle) :-
        locked(castle), !,
        write('There is a large castle immediately North, but the gate is'), nl,
        write('shut and no one appears to be home. To the West is the'),
        write('chasm.'), nl.

describe(outside_castle) :-
        write('There is a large castle immediately North with gates wide'), nl,
        write('open. To the West is the chasm.'), nl.

describe(castle) :-
        alive(vampire), !,
        write('You are in a large, richly decorated room.  It doesn''t'), nl,
        write('look like anyone has been here for a while though. Maybe that'), nl,
        write('vampire over by the throne room knows why everyone is'), nl,
        write('gone. To the North is the throne room, South leads outside.'), nl.

describe(castle) :-
        write('You are in a large, richly decorated room.'), nl,
        write('To the North is the throne room, South leads outside.'), nl.

describe(throne_room) :-
        write('You are in the throne room and there lying on the throne'), nl,
        write('in front of the you is the crown, yours for the taking!'), nl.

/* These rules describe prominent features of different areas */

describe_feature(sky) :-
        write('The sky is clear and blue with a large cloud that '), nl,
        write('looks kind of like a duck floating along.'), nl.

describe_feature(well) :-
        write('The well appears to be very deep. You notice a ladder leading '),
        write('down into the well but you can''t make out what is at the bottom.'),
        retract(unvisited(well)).

describe_feature(sarcophagus) :-
        inspected(sarcophagus), !,
        write('The skeleton is still lying there minding his own business.'), nl.

describe_feature(sarcophagus) :-
        assert(at(ring, fancy_tomb)),
        assert(inspected(sarcophagus)),
        write('Inside the sarcophagus is a skeleton in the remains of'), nl,
        write('some rich garment.  On his finger is a golden signet ring.'), nl.

describe_feature(fire) :-
        write('The fire burns brightly.'), nl.

describe_feature(sand) :-
        write('There''s lots and lots of sand. I''m sure no one would'), nl,
        write('mind if you took some.'), nl.

describe_feature(altar) :-
        empty(basin), !,
        write('There is an empty basin and small box for alms with a coin'), nl,
        write('sized slot.'), nl.

describe_feature(altar) :-
        write('The basin is full of holy water.'), nl.

describe_feature(chasm) :-
        invisible(bridge), !,
        write('If you strain you eyes really hard it almost looks as if'), nl,
        write('there is something in the gap. It''s probably just an'), nl,
        write('illusion.'), nl.

describe_feature(chasm) :-
        write('The sand shows where the bridge is.'), nl.

describe_feature(gate) :-
        write('There really doesn''t appear to be any way to open this'), nl,
        write('gate.  If only there was a doorbell you could ring.'), nl.

describe_feature(vampire) :-
        write('Batsy over there doesn''t look too friendly.'), nl.
