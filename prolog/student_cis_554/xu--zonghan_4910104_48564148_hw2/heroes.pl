/* <Legendary Heroes>, by <Zonghan Xu>. */

:- dynamic i_am_at/1, in/1, at/2, holding/1, 
level/1, experience/1, nextLevelExp/1, attack/1, defense/1, health/1, fullhealth/1, money/1,
c_level/1, c_attack/1, c_defense/1, c_health/1, c_fullhealth/1,
hydra_health/1,
sell/1, wear/1, chest/1, casket/1.

:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

i_am_at(forest).
in(peace).

path(forest, n, marketplace).
path(marketplace, s, forest).

path(forest, s, marsh).
path(marsh, n, forest).

path(forest, e, cave).
path(cave, w, forest).

path(forest, w, grassland).
path(grassland, e, forest).


/* Houses' location information */
locate(woodenHouse, forest).
locate(shop, marketplace).
locate(witch, grassland).
locate(chest, marketplace).
locate(casket, forest).

/* Items' location information */
at(key, grassland).
chest(close).
casket(close).
treasure(15).
masterpiece(pendant_of_courage).


/* Shop specific inforamtion */
sell([sword_of_hellfire, hellstorm_helmet]).

/* Puzzle information */
solution(andrew).
reward(dragon_scale_shield).

/* Attributes of equipments */
price(sword_of_hellfire, 20).
price(hellstorm_helmet, 10).

attackplus(sword_of_hellfire, 15).
attackplus(hellstorm_helmet, 2).
attackplus(dragon_scale_shield, 0).
attackplus(pendant_of_courage, 5).
defenseplus(sword_of_hellfire, 1).
defenseplus(hellstorm_helmet, 12).
defenseplus(dragon_scale_shield, 5).
defenseplus(pendant_of_courage, 5).


/* Hero's initial state. */
level(5).

experience(0).

nextLevelExp(10).

attack(10).

defense(3).

health(20).

fullhealth(20).

money(5).

wear([]).

holding(nothing).

/* Creature's initial state. */
c_level(1).

c_attack(1).

c_defense(1).

c_health(10).

c_fullhealth(10).

/* Hydra's initial state. */
hydra_level(20).

hydra_attack(30).

hydra_defense(20).

hydra_health(50).

hydra_fullhealth(50).

/* These rules prints equipments shop has */
print_equipments_shop([Head|Tail]) :-
        write('We have '), write(Head), write('. '),
        price(Head, E_price),
        attackplus(Head, E_attack),
        defenseplus(Head, E_defense),
        write('Its price: '), write(E_price), write('. '), 
        write('Its attack: +'), write(E_attack), write('. '), 
        write('Its defense: +'), write(E_defense), write('.'), nl,
        print_equipments_shop(Tail).

print_equipments_shop(_).

/* These rules prints equipments you wear */
print_equipments([Head|Tail]) :-
        write('You are wearing '), write(Head), write('. '),
        attackplus(Head, E_attack),
        defenseplus(Head, E_defense),
        write('Its attack: +'), write(E_attack), write('. '), 
        write('Its defense: +'), write(E_defense), write('.'), nl,
        print_equipments(Tail).

print_equipments(_).

/* These rules tell you the current state of the hero */
state :-   
        level_state,
        experience_state,
        physical_state,
        health_state,
        !, nl.

physical_state :-
        attack(A),
        defense(D),
        write('Your attack value is '), write(A), nl,
        write('Your defense value is '), write(D), nl.

level_state :-
        level(L),
        write('Your current level is '), write(L), nl.

experience_state :-
        experience(E),
        nextLevelExp(NE),
        write('Your current experience points is '), write(E), write('/'), write(NE), write('.'), nl.

health_state :-
        health(H),
        fullhealth(FH),
        (
            H =< 0 ->
            retract(in(battle)),
            assert(in(peace)),
            write('Your health value is '), write(0), write('/'), write(FH), write('.'), nl,
            write('You are dead!'), nl,
            !, die, fail;
            H > 0 ->
            write('Your health value is '), write(H), write('/'), write(FH), write('.'), nl
        ).

pocket :-
        money(M),
        write('Your current money is '), write(M), write(' unit(s).'), !, nl.

i :-
        holding(H),
        write('You are currently holding '), write(H), !, write('.'), nl.

equipment :-
        wear(E),
        E = [],
        write('You are wearing nothing now!'), !, nl.

equipment :-
        wear(E),
        print_equipments(E), !, nl.

/* These rules tell you the current state of the creature */
c_health_state :-
        c_health(CH),
        c_fullhealth(CFH),
        (
            CH =< 0 ->
            write('Its health value is '), write(0), write('/'), write(CFH), nl,
            write('The creature is dead! And the battle is over!'), nl,
            gain_exp,
            gain_money,
            retract(in(battle)),
            assert(in(peace));
            CH > 0 ->
            write('Its health value is '), write(CH), write('/'), write(CFH), nl
        ).

hydra_health_state :-
        hydra_health(HH),
        hydra_fullhealth(HFH),
        (
            HH =< 0 ->
            write('Its health value is '), write(0), write('/'), write(HFH), nl,
            write('The hydra is dead! And the battle is over! You get the treasure [Celestial Necklace of Bliss] and you win the game!!!'), nl,
            finish;
            HH > 0 ->
            write('Its health value is '), write(HH), write('/'), write(HFH), nl
        ).

/* These rules tell you how to gain money */
gain_money :-
        money(M),
        c_level(CL),
        Max_money is CL * 2,
        random(CL, Max_money, Gain_money),
        Current_money is M + Gain_money,
        retract(money(M)),
        assert(money(Current_money)),
        write('You gained '), write(Gain_money), write(' unit(s) of money.'), nl,
        pocket.

/* These rules tell you how to gain experience and upgrade */
gain_exp :-
        level(L),
        experience(E),
        nextLevelExp(NE),
        attack(A),
        defense(D),
        fullhealth(FH),

        c_level(CL),
        Max_Gain_exp is CL * 2,
        random(CL, Max_Gain_exp, Gain_exp),
        Current_exp is E + Gain_exp,
        (
            Current_exp < NE -> write('You gained '), write(Gain_exp), write(' points of experience.'), nl,
            retract(experience(E)),
            assert(experience(Current_exp)),
            experience_state;

            Current_exp >= NE ->
            Remain_exp is Current_exp - NE,
            Current_level is L + 1,
            retract(experience(E)),
            assert(experience(Remain_exp)),
            retract(level(L)),
            assert(level(Current_level)),

            Current_NE is NE + Current_level,
            retract(nextLevelExp(NE)),
            assert(nextLevelExp(Current_NE)),

            Current_FH is FH + 5,
            Current_A is A + 2,
            Current_D is D + 1,
            retract(fullhealth(FH)),
            retract(attack(A)),
            retract(defense(D)),
            assert(fullhealth(Current_FH)),
            assert(attack(Current_A)),
            assert(defense(Current_D)),

            write('You gained '), write(Gain_exp), write(' points of experience.'), nl,
            write('Congratulations, You upgrade! '), nl,
            level_state,
            experience_state,
            physical_state,
            write('Your maximum health is '), write(Current_FH), write(' now.'), nl
        ).


/* These rules tell the special situation, i.e. in battle */
is_battle :-
        in(battle),
        write('You are in battle now!'), nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands in battle are:'), nl,
        write('attack.              -- to attack the creature.'), nl,
        write('escape.              -- to quit the battle.'), nl,
        !, nl.

at_shop :-
        i_am_at(marketplace),
        locate(shop, marketplace),
        !, nl.

at_shop :-
        write('You are not at the shop!'), 
        !, nl.

/* These rules give the hero health back */


/* These rules tell how to enter into a house */
enter :-
        is_battle,
        !, nl.

enter :-
        health(H),
        fullhealth(FH),
        i_am_at(forest),
        locate(woodenHouse, forest),
        retract(health(H)),
        assert(health(FH)),
        write('Oh, you got your energy back. You can adventure now!'), nl,
        health_state, !, nl.

enter :-
        sell(E),
        i_am_at(marketplace),
        locate(shop, marketplace),
        (
            E = [] ->
            write('Sorry, all the equipments are sold out!'), !, fail, nl;

            write('Hi, what would you wanna buy? Type[buy(equipment_exact_name).]'), nl,
            pocket,
            print_equipments_shop(E),
            !, nl
        ).       

/* These rules tell how to buy a equipment */
buy(E) :-
        i_am_at(Marketplace),
        (
            Marketplace \= marketplace ->
            write('You are not at the shop!'), !, fail, nl;

            sell(List),
            (
                \+ member(E, List) ->
                write('There is no such equipements called '), write(E), nl,
                write('Plsease type[buy(equipment_exact_name).]'), !, fail, nl;
                deal(E)
            )
        ).

deal(E) :-
        money(M),
        price(E, P),
        (
            M < P ->
            write('You do not have enough money to buy!'), nl,
            fail;
            M >= P ->
            write('You successfully bought '), write(E), write('!'), nl,
            
            sell(Slist),
            delete(Slist, E, Current_Slist),
            retract(sell(Slist)),
            assert(sell(Current_Slist)),

            wear(Wlist),
            append(Wlist, [E], Current_Wlist),
            retract(wear(Wlist)),
            assert(wear(Current_Wlist)),

            Current_money is M - P,
            retract(money(M)),
            assert(money(Current_money)),

            attack(A),
            defense(D),
            attackplus(E, Aplus),
            defenseplus(E, Dplus),
           
            Current_A is A + Aplus,
            Current_D is D + Dplus,
            retract(attack(A)),
            retract(defense(D)),
            assert(attack(Current_A)),
            assert(defense(Current_D)),
            physical_state,
            pocket,
            equipment
        ).

/* These rules tell how to encounter a creature */
approach :-
        is_battle,
        !, nl.
        
approach :-
        i_am_at(marsh),
        battle(marsh),
        !, nl.

approach :-
        i_am_at(cave),
        level(L),
        (
            L >= 7 ->  battle(cave), !, nl;
            L < 6 -> write('Uh oh, your level is too low [the minimum level should be above 7], try to make you stronger!'), !, nl
        ).

approach :-
        i_am_at(grassland),
        puzzle(grassland),
        !, nl.

approach :-
        open,
        !, nl.

approach :- 
        write('Here is nothing!'), 
        !, nl.

/* These rules tell how to solve puzzle */
open :-
        i_am_at(marketplace), !,
        holding(H),
        chest(State),
        (
            State = open ->
            write('This chest is open and nothing in it now!'), !, nl;
            (
                H \= key ->
                write('Uh oh, this chest need a key! Try to find it!'), !, nl;
                write('You open the chest!'), nl,
                write('The key disappeared!'), nl, 
                retract(holding(H)),
                assert(holding(nothing)),
                treasure(Gain_money),

                money(M),
                Current_money is M + Gain_money,
                retract(money(M)),
                assert(money(Current_money)),
                write('You gained '), write(Gain_money), write(' unit(s) of money.'), nl,
                pocket,

                retract(chest(close)),
                assert(chest(open)),
                !, nl  
            )
        ).

open :-
        i_am_at(forest), !,
        holding(H),
        casket(State),
        (
            State = open ->
            write('This casket is open and nothing in it now!'), !, nl;
            (
                H \= key ->
                write('Uh oh, this casket need a key! Try to find it!'), !, nl;

                retract(holding(H)),
                assert(holding(nothing)),
                masterpiece(M),

                attack(A),
                defense(D),
                attackplus(M, Aplus),
                defenseplus(M, Dplus),

                write('You open the casket!'), nl,
                write('The key disappeared!'), nl,
                write('You find '), write(M), write('.'), nl,
                write('Its attack: +'), write(Aplus), write('. '), nl,
                write('Its defense: +'), write(Dplus), write('.'), nl,

                wear(Wlist),
                append(Wlist, [M], Current_Wlist),
                retract(wear(Wlist)),
                assert(wear(Current_Wlist)),
           
                Current_A is A + Aplus,
                Current_D is D + Dplus,
                retract(attack(A)),
                retract(defense(D)),
                assert(attack(Current_A)),
                assert(defense(Current_D)),
                physical_state,

                retract(casket(close)),
                assert(casket(open)),
                !, nl  
            )
        ).

/* These rules tell how to solve puzzle */
puzzle(Location) :-
        Location = grassland,
        write('The witch is giving you a puzzle.'), nl,
        write('Here is the puzzle: '), nl,
        write('1. Dudley did not walk out of the store with either Flubsub or Jarix, and his alien does not develop fins when placed in water.'), nl,
        write('2. Jarix (which is not the name of the alien Andrew picked)has eyes that glow in the dark.'), nl,
        write('3. Karen left the toy store with the alien Wattin.'), nl,
        write('4. Andrew does not member the alien that develops fins, and Dudley does not member the alien that blows bubbles.'), nl,
        write('The question is: who got the flubsub toy which has bubbles feature.'), nl,
        write('Type answer(name) to answer the questions. Options are: andrew, dudley, georgina, karen.'), !, nl.

answer(Solution) :-
        i_am_at(Location),
        (
            Location \= grassland ->
            write('You are not talking with the witch!'), !, fail, nl;

            (
                Solution = andrew ->
                write('You are right! The witch gave you Dragon Scale Shield as a reward!'), nl,
                gift, !, nl;
                write('You are wrong! Keep thinking and try again!'), !, nl
            )
            
        ).

gift :-
        reward(E),
        wear(Wlist),

        (
            member(E, Wlist) ->
            write('Uh oh, you have already had one Dragon Scale Shield, you cannot wear another one!'), 
            nl, !, fail;

            append(Wlist, [E], Current_Wlist),
            retract(wear(Wlist)),
            assert(wear(Current_Wlist)),

            attack(A),
            defense(D),
            attackplus(E, Aplus),
            defenseplus(E, Dplus),
           
            Current_A is A + Aplus,
            Current_D is D + Dplus,
            retract(attack(A)),
            retract(defense(D)),
            assert(attack(Current_A)),
            assert(defense(Current_D)),
            equipment,
            physical_state
        ).

/* These rules tell how to fight with creature */
battle(Location) :-
        Location = marsh,
        retract(in(peace)),
        assert(in(battle)),

        c_level(CL),
        c_attack(CA),
        c_defense(CD),
        c_health(CH),
        c_fullhealth(CFH),

        random(2, 10, C_level),
        Max_C_Attack is C_level * 2,
        Max_C_Defense is C_level * 2,
        Max_health is C_level * 2,
        random(C_level, Max_C_Attack, C_Attack),
        random(C_level, Max_C_Defense, C_Defense),
        random(C_level, Max_health, C_fullhealth),
        
        retract(c_level(CL)),
        retract(c_attack(CA)),
        retract(c_defense(CD)),
        retract(c_health(CH)),
        retract(c_fullhealth(CFH)),

        assert(c_level(C_level)),
        assert(c_attack(C_Attack)),
        assert(c_defense(C_Defense)),
        assert(c_health(C_fullhealth)),
        assert(c_fullhealth(C_fullhealth)),

        write('You encounter a creature whose level is '), write(C_level), nl,
        write('Its attack value is '), write(C_Attack), nl,
        write('Its defense value is '), write(C_Defense), nl,
        c_health_state,
        is_battle, !, nl.


battle(Location) :-
        Location = cave,
        retract(in(peace)),
        assert(in(battle)),

        hydra_level(CL),
        hydra_attack(CA),
        hydra_defense(CD),

        write('You encounter a hydra whose level is '), write(CL), nl,
        write('Its attack value is '), write(CA), nl,
        write('Its defense value is '), write(CD), nl,
        hydra_health_state,
        is_battle, !, nl.

/* These rules describe how to do in battle. */
min(Attack, Defense, Damage) :- 
        Attack =< Defense -> Damage is 1 ; Damage is Attack - Defense.

attack :-
        \+in(battle),
        write('There is no creature to be attacked!'), !, nl.

attack :-
        in(battle),
        i_am_at(marsh),

        attack(A),
        defense(D),
        health(H),

        c_attack(CA),
        c_defense(CD),
        c_health(CH),

        min(A, CD, H_C_Damage),
        min(CA, D, C_H_Damage),
        
        write('Creature gave you '), write(C_H_Damage), write(' points of damage.'), nl,
        Remain_H is H - C_H_Damage,
        retract(health(H)),
        assert(health(Remain_H)),
        health_state,

        write('You gave creature '), write(H_C_Damage), write(' points of damage.'), nl,
        Remain_CH is CH - H_C_Damage,
        retract(c_health(CH)),
        assert(c_health(Remain_CH)),
        c_health_state,
        !, nl.

attack :-
        in(battle),
        i_am_at(cave),
        
        attack(A),
        defense(D),
        health(H),

        hydra_attack(HA),
        hydra_defense(HD),
        hydra_health(HH),

        min(A, HD, H_C_Damage),
        min(HA, D, C_H_Damage),
        
        write('Hydra gave you '), write(C_H_Damage), write(' points of damage.'), nl,
        Remain_H is H - C_H_Damage,
        retract(health(H)),
        assert(health(Remain_H)),
        health_state,

        write('You gave Hydra '), write(H_C_Damage), write(' points of damage.'), nl,
        Remain_HH is HH - H_C_Damage,
        retract(hydra_health(HH)),
        assert(hydra_health(Remain_HH)),
        hydra_health_state,
        !, nl.

escape :-
        in(battle),
        i_am_at(marsh),
        retract(in(battle)),
        assert(in(peace)),
        write('You have successfully escaped!'),
        !, nl.

escape :-
        in(battle),
        i_am_at(cave),
        write('You cannot escape! Keep fighting!'),
        !, fail, nl.
        
escape :-
        write('There is no need to escape!'), nl.


/* These rules describe how to pick up an object. */
take(_) :-
        is_battle,
        !, nl.

take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        holding(Old),
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        retract(holding(Old)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.


/* These rules describe how to put down an object. */
drop(_) :-
        is_battle,
        !, nl.

drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(holding(nothing)),
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
        is_battle,
        !, nl.

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
        is_battle,
        !, nl.

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl,
        notice_specials_at(Place),
        nl.


/* These rules set up a loop to mention all the objects
   in your vicinity. */
notice_objects_at(_) :-
        is_battle,
        !, nl.

notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        describe(X), nl,
        fail.

notice_objects_at(_).


/* These rules set up a loop to mention all the specials
   in your vicinity. */
notice_specials_at(_) :-
        is_battle,
        !, nl.

notice_specials_at(Place) :-
        locate(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        describe(X), nl,
        fail.

notice_specials_at(_).


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
        write('enter.             -- to enter into building.'), nl,
        write('approach.          -- to approach to discover.'), nl,
        write('take(Object).      -- to pick up an object.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
        write('state.             -- to check the state of the hero.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('pocket.            -- to check the money you have.'), nl,
        write('i.                 -- to check what you are holding.'), nl,
        write('equipment.         -- to check the equipment you wear.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */
start :-
        is_battle,
        !, nl.

start :-
        instructions,
        look.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(forest) :- 
                write('You are in the forest.'), nl.

describe(woodenHouse) :- 
                write('You can type [enter.] to have a sound sleep to get your energy back.'), nl.

describe(marsh) :-
                write('You are in the marsh. There are a lot of creatures lurking around. You can type [approach.] to encounter and fight with them.'), nl.

describe(marketplace) :-
                write('You are in the busy marketplace.'), nl.

describe(shop) :-
                write('You can type [enter.] to see the equipment(s) seller currently has.'), nl.

describe(cave) :-
                write('This is a big cave. And there is a hydra here! Type [approach.] to encoutner and fight with this monster!'), nl.

describe(grassland) :-
                write('This is an eerie grassland. Something is waiting for you...'), nl.

describe(witch) :-
                write('You can type [approach.] to chat with the witch.'), nl.

describe(chest) :-
                write('The chest is locked! Type [approach.] to try to open it.'), nl.

describe(casket) :-
                write('The casket is locked! Type [approach.] to try to open it.'), nl.

