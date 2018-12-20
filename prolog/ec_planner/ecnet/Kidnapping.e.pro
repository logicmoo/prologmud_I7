
:-include(library('ec_planner/ec_test_incl')).
% 
%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; DEV-MUC3-0008
%; Kidnapping
%;
%; @article{Mueller:2004c,
%;   author = "Erik T. Mueller",
%;   year = "2004",
%;   title = "Understanding script-based stories using commonsense reasoning",
%;   journal = "Cognitive Systems Research",
%;   volume = "5",
%;   number = "4",
%;   pages = "307--340",
%; }
%;
% ecnet/Kidnapping.e:24
% 
% ignore SkyOf, GroundOf, Near, WalkFromTo, RunFromTo
 /*
.
*/
.

 /*
.
*/
.

 /*
.
*/
.

 /*
.
*/
.

 /*
.
*/
.

% ignore RollAlong, Diameter, Move, HoldSome
 /*
.
*/
.

 /*
.
*/
.

 /*
.
*/
.

 /*
.
*/
.

% ignore On, DoorUnlock, DoorLock
 /*
.
*/
.

 /*
.
*/
.

 /*
.
*/
.

% ignore WalkDownStaircase, WalkUpStaircase
 /*
.
*/
.

 /*
.
*/
.

% 
% ecnet/Kidnapping.e:30
% ignore Request, KnowRequest, Order, KnowOrder, SayGoodbye
 /*
.
*/
.

 /*
.
*/
.

 /*
.
*/
.

 /*
.
*/
.

 /*
.
*/
.

% ignore IntentionToWalkIn, InvitedIn
 /*
.
*/
.

 /*
.
*/
.

% ignore Snowing
 /*
.
*/
.

% ignore Like, Dislike, LikeSnow
 /*
.
*/
.

 /*
.
*/
.

 /*
.
*/
.

% 
% load foundations/Root.e
% ecnet/Kidnapping.e:36
% load foundations/EC.e
% load answers/Mueller2003/Ontology.e
% 
%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; integer
%;
% 
% sort diameter: integer
 /*
.
*/
.
.
.

% 
%; object
% 
% sort object
 /*
.
*/
.

% 
% sort agent: object
 /*
.
*/
.
.
.

% 
% sort physobj: object
 /*
.
*/
.
.
.

% sort bed: physobj
 /*
.
*/
.
.
.

% sort snowflake: physobj
 /*
.
*/
.
.
.

% sort sky: physobj
 /*
.
*/
.
.
.

% 
% sort stuff: physobj
 /*
.
*/
.
.
.

% 
% sort surface: physobj
 /*
.
*/
.
.
.

% sort ground: surface
 /*
.
*/
.
.
.

% 
% sort snow: stuff
 /*
.
*/
.
.
.

% sort ball
 /*
.
*/
.

% 
% sort food: physobj
 /*
.
*/
.
.
.

% sort fruit: food
 /*
.
*/
.
.
.

% sort orange: fruit
 /*
.
*/
.
.
.

% sort salad: food
 /*
.
*/
.
.
.

% 
% sort clothing: physobj
 /*
.
*/
.
.
.

% sort scarf: clothing
 /*
.
*/
.
.
.

% sort hat: clothing
 /*
.
*/
.
.
.

% 
% sort vegetablematter: physobj
 /*
.
*/
.
.
.

% sort coal: vegetablematter
 /*
.
*/
.
.
.

% 
% sort bodypart: physobj
 /*
.
*/
.
.
.

% sort hand: bodypart
 /*
.
*/
.
.
.

% 
% sort papertowels: physobj
 /*
.
*/
.
.
.

% sort device: physobj
 /*
.
*/
.
.
.

% sort electronicdevice: device
 /*
.
*/
.
.
.

% sort lamp: electronicdevice
 /*
.
*/
.
.
.

% 
% sort cat: physobj
 /*
.
*/
.
.
.

% sort horse: physobj
 /*
.
*/
.
.
.

% 
% sort weapon: physobj
 /*
.
*/
.
.
.

% sort gun: weapon
 /*
.
*/
.
.
.

% sort bomb: weapon
 /*
.
*/
.
.
.

% sort bullet: weapon
 /*
.
*/
.
.
.

% 
%; location
% 
% sort location
 /*
.
*/
.

% sort room: location, outside: location
 /*
.
*/
.
.
.

 /*
.
*/
.
.
.

% 
%; portal
% 
% sort portal
 /*
.
*/
.

% sort door: portal, staircase: portal
 /*
.
*/
.
.
.

 /*
.
*/
.
.
.

% sort street: portal
 /*
.
*/
.
.
.

% sort track: portal
 /*
.
*/
.
.
.

% 
% sort building
 /*
.
*/
.

% 
% sort fire: object
 /*
.
*/
.
.
.

% sort smoke: physobj
 /*
.
*/
.
.
.

% 
% sort furniture: physobj
 /*
.
*/
.
.
.

% sort chair: furniture
 /*
.
*/
.
.
.

% sort table: furniture
 /*
.
*/
.
.
.

% 
% sort bill: physobj
 /*
.
*/
.
.
.

% sort ticket: physobj
 /*
.
*/
.
.
.

% sort envelope: physobj
 /*
.
*/
.
.
.

% 
% sort text: physobj
 /*
.
*/
.
.
.

% sort book: text
 /*
.
*/
.
.
.

% sort letter: text
 /*
.
*/
.
.
.

% sort menu: text
 /*
.
*/
.
.
.

% 
% sort paper: physobj
 /*
.
*/
.
.
.

% 
% sort content
 /*
.
*/
.

% sort script
 /*
.
*/
.

% 
% sort container: physobj
 /*
.
*/
.
.
.

% sort cigarette: physobj
 /*
.
*/
.
.
.

% sort ashtray: physobj
 /*
.
*/
.
.
.

% sort umbrella: physobj
 /*
.
*/
.
.
.

% 
% sort pen: physobj
 /*
.
*/
.
.
.

% 
%; End of file.
% load answers/Mueller2004c/OTSpaceM.e
 /*
.
*/
.

% load answers/Mueller2004c/RTSpaceM.e
 /*
.
*/
.

% load answers/Mueller2003/Feeling.e
% 
%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; feeling = emotion, attitude, ...
%;
%; The Feeling representation includes simple positive, neutral, and
%; negative emotions, and positive, neutral, and negative attitudes
%; toward objects.
%;
% 
%; emotions
% 
%; agent is happy.
% fluent Happy(agent)
 /*
.
*/
.

% 
%; agent is emotionally neutral or calm.
% fluent Calm(agent)
 /*
.
*/
.

% 
%; agent is unhappy.
% fluent Unhappy(agent)
 /*
.
*/
.

% 
%; At any moment, an agent is in one of three emotional states:
% xor Happy, Calm, Unhappy
 /*
.
*/
.

% 
%; agent becomes happy.
% event BecomeHappy(agent)
 /*
.
*/
.

% 
%; agent becomes calm.
% event BecomeCalm(agent)
 /*
.
*/
.

% 
%; agent becomes unhappy.
% event BecomeUnhappy(agent)
 /*
.
*/
.

% 
%; A number of effect and precondition axioms deal with the transitions
%; from one emotional state to another:
% ecnet/Kidnapping.e:41
% [agent,time]% 
% Initiates(BecomeHappy(agent),Happy(agent),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:41
% [agent,time]% 
% HoldsAt(Calm(agent),time) ->
% Terminates(BecomeHappy(agent),Calm(agent),time).
 /*
holds_at(calm(Agent), Time) ->
    terminates(becomeHappy(Agent), calm(Agent), Time).
*/
.

% 
% 
% ecnet/Kidnapping.e:41
% [agent,time]% 
% HoldsAt(Unhappy(agent),time) ->
% Terminates(BecomeHappy(agent),Unhappy(agent),time).
 /*
holds_at(unhappy(Agent), Time) ->
    terminates(becomeHappy(Agent), unhappy(Agent), Time).
*/
.

% 
% 
% ecnet/Kidnapping.e:41
% [agent,time]% 
% Happens(BecomeHappy(agent),time) ->
% !HoldsAt(Happy(agent),time).
 /*
happens(becomeHappy(Agent), Time) ->
    not(holds_at(happy(Agent), Time)).
*/
.

% 
% 
% ecnet/Kidnapping.e:41
% [agent,time]% 
% Initiates(BecomeCalm(agent),Calm(agent),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:41
% [agent,time]% 
% HoldsAt(Happy(agent),time) ->
% Terminates(BecomeCalm(agent),Happy(agent),time).
 /*
holds_at(happy(Agent), Time) ->
    terminates(becomeCalm(Agent), happy(Agent), Time).
*/
.

% 
% 
% ecnet/Kidnapping.e:41
% [agent,time]% 
% HoldsAt(Unhappy(agent),time) ->
% Terminates(BecomeCalm(agent),Unhappy(agent),time).
 /*
holds_at(unhappy(Agent), Time) ->
    terminates(becomeCalm(Agent), unhappy(Agent), Time).
*/
.

% 
% 
% ecnet/Kidnapping.e:41
% [agent,time]% 
% Happens(BecomeCalm(agent),time) -> !HoldsAt(Calm(agent),time).
 /*
happens(becomeCalm(Agent), Time) ->
    not(holds_at(calm(Agent), Time)).
*/
.

% 
% 
% ecnet/Kidnapping.e:41
% [agent,time]% 
% Initiates(BecomeUnhappy(agent),Unhappy(agent),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:41
% [agent,time]% 
% HoldsAt(Happy(agent),time) ->
% Terminates(BecomeUnhappy(agent),Happy(agent),time).
 /*
holds_at(happy(Agent), Time) ->
    terminates(becomeUnhappy(Agent), happy(Agent), Time).
*/
.

% 
% 
% ecnet/Kidnapping.e:41
% [agent,time]% 
% HoldsAt(Calm(agent),time) ->
% Terminates(BecomeUnhappy(agent),Calm(agent),time).
 /*
holds_at(calm(Agent), Time) ->
    terminates(becomeUnhappy(Agent), calm(Agent), Time).
*/
.

% 
% 
% ecnet/Kidnapping.e:41
% [agent,time]% 
% Happens(BecomeUnhappy(agent),time) -> !HoldsAt(Unhappy(agent),time).
 /*
happens(becomeUnhappy(Agent), Time) ->
    not(holds_at(unhappy(Agent), Time)).
*/
.

% 
% 
%; anger
% 
% fluent AngryAt(agent,agent)
 /*
.
*/
.

% 
% event BecomeAngryAt(agent,agent)
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:41
% [agent1,agent2,time]% 
% Initiates(BecomeAngryAt(agent1,agent2),AngryAt(agent1,agent2),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:41
% [agent1,agent2,time]% 
% Terminates(BecomeHappy(agent1),AngryAt(agent1,agent2),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:41
% [agent1,agent2,time]% 
% Happens(BecomeAngryAt(agent1,agent2),time) ->
% Happens(BecomeUnhappy(agent1),time).
 /*
happens(becomeAngryAt(Agent1, Agent2), Time) ->
    happens(becomeUnhappy(Agent1), Time).
*/
.

% 
% 
%; attitudes
% 
%; agent likes object.
% fluent Like(agent,object)
 /*
.
*/
.

%; agent loves object.
% fluent Love(agent,object)
 /*
.
*/
.

%; agent dislikes object.
% fluent Dislike(agent,object)
 /*
.
*/
.

% 
%; agent likes snow.
% fluent LikeSnow(agent)
 /*
.
*/
.

% 
%; A trigger axiom states that
%; if an agent is awake, likes snow, and is in a room that
%; looks out onto a location where it is snowing, that agent
%; becomes happy:
% ecnet/Kidnapping.e:41
% [agent,room,outside,time]% 
% !HoldsAt(Happy(agent),time) &
% HoldsAt(Awake(agent),time) &
% HoldsAt(LikeSnow(agent),time) &
% HoldsAt(At(agent,room),time) &
% LookOutOnto(room)=outside &
% HoldsAt(Snowing(outside),time) ->
% Happens(BecomeHappy(agent),time).
 /*
not(holds_at(happy(Agent), Time)), holds_at(awake(Agent), Time), holds_at(likeSnow(Agent), Time), holds_at(at(Agent, Room), Time), lookOutOnto(Room)=Outside, holds_at(snowing(Outside), Time) ->
    happens(becomeHappy(Agent), Time).
*/
.

% 
% 
%; We introduced LikeSnow above since Like
%; can only be used to represent that an agent likes a
%; particular object, not snow in general.
% 
% event Smile(agent)
 /*
.
*/
.

% 
%; End of file.
% load answers/Mueller2004c/Condition.e
% 
%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; human health
% 
% fluent Alive(agent)
 /*
.
*/
.

% 
% fluent Dead(agent)
 /*
.
*/
.

% noninertial Dead
 /*
.
*/
.

% 
% fluent Injured(agent)
 /*
.
*/
.

% 
% event Kill(object,agent)
 /*
.
*/
.

% event Injure(object,agent)
 /*
.
*/
.

% event HealInjured(agent)
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:42
% [agent,time] % HoldsAt(Alive(agent),time) <-> !HoldsAt(Dead(agent),time).
 /*
holds_at(alive(Agent), Time) <->
    not(holds_at(dead(Agent), Time)).
*/
.
.

% 
% ecnet/Kidnapping.e:42
% [agent,time] % HoldsAt(Injured(agent),time) -> HoldsAt(Alive(agent),time).
 /*
holds_at(injured(Agent), Time) ->
    holds_at(alive(Agent), Time).
*/
.

% 
% 
% ecnet/Kidnapping.e:42
% [object,agent,time]% 
% Terminates(Kill(object,agent),Alive(agent),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:42
% [object,agent,time]% 
% Initiates(Injure(object,agent),Injured(agent),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:42
% [agent,time]% 
% Terminates(HealInjured(agent),Injured(agent),time).
 /*
.
*/
.

% 
% 
% fluent Intact(physobj)
 /*
.
*/
.

% 
% fluent Damaged(physobj)
 /*
.
*/
.

% 
% fluent Destroyed(physobj)
 /*
.
*/
.

% 
%; At any time, a physical object is either intact, damaged, or destroyed:
% xor Intact, Damaged, Destroyed
 /*
.
*/
.

% 
% event Damage(object,physobj)
 /*
.
*/
.

% 
% event Destroy(object,physobj)
 /*
.
*/
.

% 
% event Repair(object,physobj)
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:42
% [object,physobj,time]% 
% Happens(Damage(object,physobj),time) ->
% HoldsAt(Intact(physobj),time).
 /*
happens(damage(Object, Physobj), Time) ->
    holds_at(intact(Physobj), Time).
*/
.

% 
% 
% ecnet/Kidnapping.e:42
% [object,physobj,time]% 
% Initiates(Damage(object,physobj),Damaged(physobj),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:42
% [object,physobj,time]% 
% Terminates(Damage(object,physobj),Intact(physobj),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:42
% [object,physobj,time]% 
% Happens(Destroy(object,physobj),time) ->
% (HoldsAt(Intact(physobj),time)|
%  HoldsAt(Damaged(physobj),time)).
 /*
happens(destroy(Object, Physobj), Time) ->
    (   holds_at(intact(Physobj), Time)
    ;   holds_at(damaged(Physobj), Time)
    ).
*/
.

% 
% 
% ecnet/Kidnapping.e:42
% [object,physobj,time]% 
% Initiates(Destroy(object,physobj),Destroyed(physobj),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:42
% [object,physobj,time]% 
% Terminates(Destroy(object,physobj),Intact(physobj),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:42
% [object,physobj,time]% 
% Terminates(Destroy(object,physobj),Damaged(physobj),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:42
% [object,physobj,time]% 
% Initiates(Repair(object,physobj),Intact(physobj),time).
 /*
.
*/
.

% 
% 
%; end of file.
% load answers/Mueller2004c/Gun.e
% 
%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
% 
% fluent Loaded(gun,bullet)
 /*
.
*/
.

% noninertial Loaded
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:43
% [gun,bullet,time]% 
% HoldsAt(Inside(bullet,gun),time) <->
% HoldsAt(Loaded(gun,bullet),time).
 /*
holds_at(inside(Bullet, Gun), Time) <->
    holds_at(loaded(Gun, Bullet), Time).
*/
.
.

% 
% 
% event Shoot(agent,gun,object)
 /*
.
*/
.

% 
% event ShootInjure(agent,gun,agent)
 /*
.
*/
.

% 
% event ShootKill(agent,gun,agent)
 /*
.
*/
.

% 
% event ShootDamage(agent,gun,physobj)
 /*
.
*/
.

% 
% event ShootDestroy(agent,gun,physobj)
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:43
% [agent,gun,bullet,object,time]% 
% HoldsAt(Inside(bullet,gun),time) ->
% Terminates(Shoot(agent,gun,object),
%            Inside(bullet,gun),
%            time).
 /*
holds_at(inside(Bullet, Gun), Time) ->
    terminates(shoot(Agent, Gun, Object),
               inside(Bullet, Gun),
               Time).
*/
.

% 
% 
% ecnet/Kidnapping.e:43
% [agent,gun,bullet,object,location1,location2,time]% 
% HoldsAt(Inside(bullet,gun),time) &
% HoldsAt(At(gun,location1),time) &
% location1 != location2 ->
% Terminates(Shoot(agent,gun,object),At(bullet,location2),time).
 /*
holds_at(inside(Bullet, Gun), Time), holds_at(at(Gun, Location1), Time), Location1\=Location2 ->
    terminates(shoot(Agent, Gun, Object),
               at(Bullet, Location2),
               Time).
*/
.

% 
% 
% ecnet/Kidnapping.e:43
% [agent,gun,bullet,object,location,time]% 
% HoldsAt(At(object,location),time) &
% HoldsAt(Inside(bullet,gun),time) ->
% Initiates(Shoot(agent,gun,object),At(bullet,location),time).
 /*
holds_at(at(Object, Location), Time), holds_at(inside(Bullet, Gun), Time) ->
    initiates(shoot(Agent, Gun, Object),
              at(Bullet, Location),
              Time).
*/
.

% 
% 
% ecnet/Kidnapping.e:43
% [agent,gun,object,time]% 
% Happens(Shoot(agent,gun,object),time) ->
% HoldsAt(Holding(agent,gun),time) &
% ({bullet} HoldsAt(Loaded(gun,bullet),time)) &
% ({location} HoldsAt(At(agent,location),time) &
%             HoldsAt(At(object,location),time)).
 /*
happens(shoot(Agent, Gun, Object), Time) ->
    holds_at(holding(Agent, Gun), Time),
    exists([Bullet], holds_at(loaded(Gun, Bullet), Time)),
    exists([Location],
            (holds_at(at(Agent, Location), Time), holds_at(at(Object, Location), Time))).
*/
.

% 
% 
% ecnet/Kidnapping.e:43
% [agent1,gun,agent2,time]% 
% Happens(Shoot(agent1,gun,agent2),time) ->
% Happens(ShootInjure(agent1,gun,agent2),time) |
% Happens(ShootKill(agent1,gun,agent2),time).
 /*
(   ( happens(shoot(Agent1, Gun, Agent2), Time)->happens(shootInjure(Agent1, Gun, Agent2), Time)
    )
;   happens(shootKill(Agent1, Gun, Agent2), Time)
).
*/
.

% 
% 
% ecnet/Kidnapping.e:43
% [agent1,gun,bullet,agent2,time]% 
% HoldsAt(Inside(bullet,gun),time) &
% Happens(ShootKill(agent1,gun,agent2),time) ->
% Happens(Kill(bullet,agent2),time).
 /*
holds_at(inside(Bullet, Gun), Time), happens(shootKill(Agent1, Gun, Agent2), Time) ->
    happens(kill(Bullet, Agent2), Time).
*/
.

% 
% 
% ecnet/Kidnapping.e:43
% [agent1,gun,bullet,agent2,time]% 
% HoldsAt(Inside(bullet,gun),time) &
% Happens(ShootInjure(agent1,gun,agent2),time) ->
% Happens(Injure(bullet,agent2),time).
 /*
holds_at(inside(Bullet, Gun), Time), happens(shootInjure(Agent1, Gun, Agent2), Time) ->
    happens(injure(Bullet, Agent2), Time).
*/
.

% 
% 
% ecnet/Kidnapping.e:43
% [agent,gun,physobj,time]% 
% Happens(Shoot(agent,gun,physobj),time) ->
% Happens(ShootDamage(agent,gun,physobj),time) |
% Happens(ShootDestroy(agent,gun,physobj),time).
 /*
(   ( happens(shoot(Agent, Gun, Physobj), Time)->happens(shootDamage(Agent, Gun, Physobj), Time)
    )
;   happens(shootDestroy(Agent, Gun, Physobj), Time)
).
*/
.

% 
% 
% ecnet/Kidnapping.e:43
% [agent,gun,bullet,physobj,time]% 
% HoldsAt(Inside(bullet,gun),time) &
% Happens(ShootDamage(agent,gun,physobj),time) ->
% Happens(Damage(bullet,physobj),time).
 /*
holds_at(inside(Bullet, Gun), Time), happens(shootDamage(Agent, Gun, Physobj), Time) ->
    happens(damage(Bullet, Physobj), Time).
*/
.

% 
% 
% ecnet/Kidnapping.e:43
% [agent,gun,bullet,physobj,time]% 
% HoldsAt(Inside(bullet,gun),time) &
% Happens(ShootDestroy(agent,gun,physobj),time) ->
% Happens(Destroy(bullet,physobj),time).
 /*
holds_at(inside(Bullet, Gun), Time), happens(shootDestroy(Agent, Gun, Physobj), Time) ->
    happens(destroy(Bullet, Physobj), Time).
*/
.

% 
% 
%; End of file.
% load answers/Mueller2003/Sleep.e
% 
%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; The Sleep representation deals with the activity of sleeping and
%; body posture.
%; It is similar to the finite automaton representation of sleep
%; used in ThoughtTreasure \fullcite[chap. 7]{Mueller:1998}.
%;
%; @book{Mueller:1998,
%;   author = "Erik T. Mueller",
%;   year = "1998",
%;   title = "Natural Language Processing with \uppercase{T}hought\uppercase{T}reasure",
%;   address = "New York",
%;   publisher = "Signiform",
%; }
%;
% 
%; sleep
% 
%; agent wakes up.
% event WakeUp(agent)
 /*
.
*/
.

% 
%; agent gets tired.
% event GetTired(agent)
 /*
.
*/
.

% 
%; agent falls asleep.
% event FallAsleep(agent)
 /*
.
*/
.

% 
%; agent is asleep.
% fluent Sleep0(agent)
 /*
.
*/
.

%; agent is awake and in bed.
% fluent Sleep1(agent)
 /*
.
*/
.

%; agent is awake, out of bed, and undressed.
% fluent Sleep2(agent)
 /*
.
*/
.

%; agent is awake and dressed.
% fluent Sleep3(agent)
 /*
.
*/
.

%; agent is tired and dressed.
% fluent Sleep4(agent)
 /*
.
*/
.

%; agent is tired and undressed.
% fluent Sleep5(agent)
 /*
.
*/
.

%; agent is in bed, waiting to fall asleep.
% fluent Sleep6(agent)
 /*
.
*/
.

% 
%; At any time, an agent is in one of seven sleep states:
% xor Sleep0, Sleep1, Sleep2, Sleep3, Sleep4, Sleep5, Sleep6
 /*
.
*/
.

% 
%; constraints
% 
%; agent is asleep.
% fluent Asleep(agent)
 /*
.
*/
.

%; agent is awake.
% fluent Awake(agent)
 /*
.
*/
.

% noninertial Asleep
 /*
.
*/
.

% noninertial Awake
 /*
.
*/
.

% 
%; Sleep0 indicates that the agent is asleep:
% ecnet/Kidnapping.e:44
% [agent,time] % HoldsAt(Asleep(agent),time) <-> HoldsAt(Sleep0(agent),time).
 /*
holds_at(asleep(Agent), Time) <->
    holds_at(sleep0(Agent), Time).
*/
.
.

% 
% 
%; In all other sleep states, the agent is awake:
% ecnet/Kidnapping.e:44
% [agent,time]% 
% HoldsAt(Awake(agent),time) <->
% HoldsAt(Sleep1(agent),time) |
% HoldsAt(Sleep2(agent),time) |
% HoldsAt(Sleep3(agent),time) |
% HoldsAt(Sleep4(agent),time) |
% HoldsAt(Sleep5(agent),time) |
% HoldsAt(Sleep6(agent),time).
 /*
holds_at(awake(Agent), Time) <->
    (   holds_at(sleep1(Agent), Time)
    ;   holds_at(sleep2(Agent), Time)
    ;   holds_at(sleep3(Agent), Time)
    ;   holds_at(sleep4(Agent), Time)
    ;   holds_at(sleep5(Agent), Time)
    ;   holds_at(sleep6(Agent), Time)
    ).
*/
.
.

% 
% 
%; A number of axioms are used to specify the transitions of
%; a finite automaton.
%;--
% 
%; Waking up causes a transition from Sleep0
%; to Sleep1:
% ecnet/Kidnapping.e:44
% [agent,time] % Terminates(WakeUp(agent),Sleep0(agent),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:44
% [agent,time] % Initiates(WakeUp(agent),Sleep1(agent),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:44
% [agent,time] % Happens(WakeUp(agent),time) -> HoldsAt(Sleep0(agent),time).
 /*
happens(wakeUp(Agent), Time) ->
    holds_at(sleep0(Agent), Time).
*/
.

% 
% 
%;--
% 
%; Getting out of bed causes a transition from Sleep1
%; to Sleep2:
% ecnet/Kidnapping.e:44
% [agent,bed,time] % Terminates(RiseFrom(agent,bed),Sleep1(agent),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:44
% [agent,bed,time] % Initiates(RiseFrom(agent,bed),Sleep2(agent),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:44
% [agent,bed,time]% 
% Happens(RiseFrom(agent,bed),time) -> HoldsAt(Sleep1(agent),time).
 /*
happens(riseFrom(Agent, Bed), Time) ->
    holds_at(sleep1(Agent), Time).
*/
.

% 
% 
%;--
% 
%; Getting dressed causes a transition from Sleep2
%; to Sleep3, the normal state of awakeness:
% ecnet/Kidnapping.e:44
% [agent,time] % Terminates(GetDressed(agent),Sleep2(agent),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:44
% [agent,time] % Initiates(GetDressed(agent),Sleep3(agent),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:44
% [agent,time] % Happens(GetDressed(agent),time) -> HoldsAt(Sleep2(agent),time).
 /*
happens(getDressed(Agent), Time) ->
    holds_at(sleep2(Agent), Time).
*/
.

% 
% 
%;--
% 
%; Getting tired causes a transition from Sleep3
%; to Sleep4:
% ecnet/Kidnapping.e:44
% [agent,time] % Terminates(GetTired(agent),Sleep3(agent),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:44
% [agent,time] % Initiates(GetTired(agent),Sleep4(agent),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:44
% [agent,time] % Happens(GetTired(agent),time) -> HoldsAt(Sleep3(agent),time).
 /*
happens(getTired(Agent), Time) ->
    holds_at(sleep3(Agent), Time).
*/
.

% 
% 
%;--
% 
%; Getting undressed causes a transition from Sleep4
%; to Sleep5:
% ecnet/Kidnapping.e:44
% [agent,time] % Terminates(GetUndressed(agent),Sleep4(agent),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:44
% [agent,time] % Initiates(GetUndressed(agent),Sleep5(agent),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:44
% [agent,time] % Happens(GetUndressed(agent),time) -> HoldsAt(Sleep4(agent),time).
 /*
happens(getUndressed(Agent), Time) ->
    holds_at(sleep4(Agent), Time).
*/
.

% 
% 
%;--
% 
%; Lying on a bed causes a transition from Sleep5
%; to Sleep6:
% ecnet/Kidnapping.e:44
% [agent,bed,time] % Terminates(LieOn(agent,bed),Sleep5(agent),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:44
% [agent,bed,time] % Initiates(LieOn(agent,bed),Sleep6(agent),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:44
% [agent,bed,time] % Happens(LieOn(agent,bed),time) -> HoldsAt(Sleep5(agent),time).
 /*
happens(lieOn(Agent, Bed), Time) ->
    holds_at(sleep5(Agent), Time).
*/
.

% 
% 
%;--
% 
%; Falling asleep causes a transition from Sleep6
%; to Sleep0:
% ecnet/Kidnapping.e:44
% [agent,time] % Terminates(FallAsleep(agent),Sleep6(agent),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:44
% [agent,time] % Initiates(FallAsleep(agent),Sleep0(agent),time).
 /*
.
*/
.

% 
% 
% ecnet/Kidnapping.e:44
% [agent,time] % Happens(FallAsleep(agent),time) -> HoldsAt(Sleep6(agent),time).
 /*
happens(fallAsleep(Agent), Time) ->
    holds_at(sleep6(Agent), Time).
*/
.

% 
% 
%;--
% 
%; agent acts on being in state Sleep5.
% fluent ActOnSleep5(agent)
 /*
.
*/
.

% noninertial ActOnSleep5
 /*
.
*/
.

% 
%; We reduce the number of models by asserting that
%; an agent only acts on being in state Sleep5 while in
%; that state:
% ecnet/Kidnapping.e:44
% [agent,time]% 
% !HoldsAt(Sleep5(agent),time) ->
% !HoldsAt(ActOnSleep5(agent),time).
 /*
not(holds_at(sleep5(Agent), Time)) ->
    not(holds_at(actOnSleep5(Agent), Time)).
*/
.

% 
% 
%; Undressed is like IntentionToPlay
%; ActOnSleep5 is like ActOnIntentionToPlay
% 
%; A trigger axiom states that if an agent is in state Sleep5,
%; the agent acts on this state, the agent is in a room, and
%; a bed is at the room, the agent lies on the bed:
% ecnet/Kidnapping.e:44
% [agent,room,bed,time]% 
% HoldsAt(Sleep5(agent),time) &
% HoldsAt(ActOnSleep5(agent),time) &
% HoldsAt(At(agent,room),time) &
% HoldsAt(At(bed,room),time) ->
% Happens(LieOn(agent,bed),time).
 /*
holds_at(sleep5(Agent), Time), holds_at(actOnSleep5(Agent), Time), holds_at(at(Agent, Room), Time), holds_at(at(Bed, Room), Time) ->
    happens(lieOn(Agent, Bed), Time).
*/
.

% 
% 
%; A precondition axiom states that for
%; an agent to lie on a bed,
%; the agent must be in state Sleep5,
%; the agent must act on this state, and
%; there must be a room such that
%; the agent is in the room and the bed is in the room:
% ecnet/Kidnapping.e:44
% [agent,bed,time]% 
% Happens(LieOn(agent,bed),time) ->
% HoldsAt(Sleep5(agent),time) &
% HoldsAt(ActOnSleep5(agent),time) &
% ecnet/Kidnapping.e:44
% {room}% 
%  HoldsAt(At(agent,room),time) &
%  HoldsAt(At(bed,room),time).
 /*
exists([Room],  (happens(lieOn(Agent, Bed), Time)->holds_at(sleep5(Agent), Time), holds_at(actOnSleep5(Agent), Time), holds_at(at(Agent, Room), Time), holds_at(at(Bed, Room), Time))).
*/
.

% 
% 
%; (body) posture
% 
%; agent lies on physobj.
% event LieOn(agent,physobj)
 /*
.
*/
.

% 
%; agent sits on physobj.
% event SitOn(agent,physobj)
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:44
% [agent,physobj,time]% 
% Happens(SitOn(agent,physobj),time) ->
% ecnet/Kidnapping.e:44
% {location}% 
%  HoldsAt(At(agent,location),time) &
%  HoldsAt(At(physobj,location),time).
 /*
exists([Location],  (happens(sitOn(Agent, Physobj), Time)->holds_at(at(Agent, Location), Time), holds_at(at(Physobj, Location), Time))).
*/
.

% 
% 
%; agent rises from physobj.
% event RiseFrom(agent,physobj)
 /*
.
*/
.

% 
%; agent is lying on physobj.
% fluent LyingOn(agent,physobj)
 /*
.
*/
.

%; agent is sitting on physobj.
% fluent SittingOn(agent,physobj)
 /*
.
*/
.

%; agent is standing.
% fluent Standing(agent)
 /*
.
*/
.

% 
%; agent is lying down.
% fluent Lying(agent)
 /*
.
*/
.

%; agent is sitting.
% fluent Sitting(agent)
 /*
.
*/
.

% noninertial Lying
 /*
.
*/
.

% noninertial Sitting
 /*
.
*/
.

% 
%; At any time, an agent is either lying, sitting, or standing:
% xor Lying, Sitting, Standing
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:44
% [agent,physobj,time]% 
% HoldsAt(LyingOn(agent,physobj),time) ->
% HoldsAt(Lying(agent),time).
 /*
holds_at(lyingOn(Agent, Physobj), Time) ->
    holds_at(lying(Agent), Time).
*/
.

% 
% 
% ecnet/Kidnapping.e:44
% [agent,physobj,time]% 
% HoldsAt(SittingOn(agent,physobj),time) ->
% HoldsAt(Sitting(agent),time).
 /*
holds_at(sittingOn(Agent, Physobj), Time) ->
    holds_at(sitting(Agent), Time).
*/
.

% 
% 
%; State constraints represent that an agent can lie or sit
%; on at most one object at a time:
% ecnet/Kidnapping.e:44
% [agent,physobj1,physobj2,time]% 
% HoldsAt(LyingOn(agent,physobj1),time) &
% HoldsAt(LyingOn(agent,physobj2),time) ->
% physobj1=physobj2.
 /*
holds_at(lyingOn(Agent, Physobj1), Time), holds_at(lyingOn(Agent, Physobj2), Time) ->
    Physobj1=Physobj2.
*/
.

% 
% 
% ecnet/Kidnapping.e:44
% [agent,physobj1,physobj2,time]% 
% HoldsAt(SittingOn(agent,physobj1),time) &
% HoldsAt(SittingOn(agent,physobj2),time) ->
% physobj1=physobj2.
 /*
holds_at(sittingOn(Agent, Physobj1), Time), holds_at(sittingOn(Agent, Physobj2), Time) ->
    Physobj1=Physobj2.
*/
.

% 
% 
%; An effect axiom states that if an agent is standing and
%; lies on a physical object, the agent will be lying on
%; the physical object:
% ecnet/Kidnapping.e:44
% [agent,physobj,time]% 
% HoldsAt(Standing(agent),time) ->
% Initiates(LieOn(agent,physobj),
%           LyingOn(agent,physobj),
%           time).
 /*
holds_at(standing(Agent), Time) ->
    initiates(lieOn(Agent, Physobj),
              lyingOn(Agent, Physobj),
              Time).
*/
.

% 
% 
%; An effect axiom states that if an agent
%; lies on a physical object, the agent will no longer
%; be standing:
% ecnet/Kidnapping.e:44
% [agent,physobj,time]% 
% Terminates(LieOn(agent,physobj),
%            Standing(agent),
%            time).
 /*
.
*/
.

% 
% 
%; An effect axiom states that if an agent is standing and
%; sits on a physical object, the agent will be sitting on
%; the physical object:
% ecnet/Kidnapping.e:44
% [agent,physobj,time]% 
% HoldsAt(Standing(agent),time) ->
% Initiates(SitOn(agent,physobj),
%           SittingOn(agent,physobj),
%           time).
 /*
holds_at(standing(Agent), Time) ->
    initiates(sitOn(Agent, Physobj),
              sittingOn(Agent, Physobj),
              Time).
*/
.

% 
% 
%; An effect axiom states that if an agent
%; sits on a physical object, the agent will no longer
%; be standing:
% ecnet/Kidnapping.e:44
% [agent,physobj,time]% 
% Terminates(SitOn(agent,physobj),
%            Standing(agent),
%            time).
 /*
.
*/
.

% 
% 
%; An effect axiom states that if an agent
%; is sitting or lying on a physical object and
%; the agent rises from the physical object,
%; the agent will be standing:
% ecnet/Kidnapping.e:44
% [agent,physobj,time]% 
% (HoldsAt(SittingOn(agent,physobj),time) |
%  HoldsAt(LyingOn(agent,physobj),time)) ->
% Initiates(RiseFrom(agent,physobj),
%           Standing(agent),
%           time).
 /*
holds_at(sittingOn(Agent, Physobj), Time);holds_at(lyingOn(Agent, Physobj), Time) ->
    initiates(riseFrom(Agent, Physobj),
              standing(Agent),
              Time).
*/
.

% 
% 
%; An effect axiom states that if an agent is sitting on
%; a physical object and the agent rises from the physical
%; object, the agent will no longer be sitting on the
%; physical object:
% ecnet/Kidnapping.e:44
% [agent,physobj,time]% 
% HoldsAt(SittingOn(agent,physobj),time) ->
% Terminates(RiseFrom(agent,physobj),
%            SittingOn(agent,physobj),
%            time).
 /*
holds_at(sittingOn(Agent, Physobj), Time) ->
    terminates(riseFrom(Agent, Physobj),
               sittingOn(Agent, Physobj),
               Time).
*/
.

% 
% 
%; An effect axiom states that if an agent is lying on
%; a physical object and the agent rises from the physical
%; object, the agent will no longer be lying on the
%; physical object:
% ecnet/Kidnapping.e:44
% [agent,physobj,time]% 
% HoldsAt(LyingOn(agent,physobj),time) ->
% Terminates(RiseFrom(agent,physobj),
%            LyingOn(agent,physobj),
%            time).
 /*
holds_at(lyingOn(Agent, Physobj), Time) ->
    terminates(riseFrom(Agent, Physobj),
               lyingOn(Agent, Physobj),
               Time).
*/
.

% 
% 
%; dressing
% 
%; agent gets undressed.
% event GetDressed(agent)
 /*
.
*/
.

%; agent gets dressed.
% event GetUndressed(agent)
 /*
.
*/
.

%; agent is dressed.
% fluent Dressed(agent)
 /*
.
*/
.

% 
%; Effect axioms deal with getting dressed and undressed:
% ecnet/Kidnapping.e:44
% [agent,time] % Initiates(GetDressed(agent),Dressed(agent),time).
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:44
% [agent,time] % Terminates(GetUndressed(agent),Dressed(agent),time).
 /*
.
*/
.

% 
% 
%; End of file.
% load answers/Mueller2003/SpeechAct.e
% 
% gun Gun1
 /*
.
*/
.
.

% bullet Bullet1
 /*
.
*/
.
.

% HoldsAt(Intact(Gun1),0).
 /*
.
*/
.

% 
% HoldsAt(Intact(Bullet1),0).
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:50
% 
% agent Perp1
 /*
.
*/
.
.

% 
% agent HumanTarget1
 /*
.
*/
.
.

% HoldsAt(Calm(HumanTarget1),0).
 /*
.
*/
.

% 
% HoldsAt(Alive(HumanTarget1),0).
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:56
% HoldsAt(Awake(HumanTarget1),0).
 /*
.
*/
.

% 
% HoldsAt(Standing(HumanTarget1),0).
 /*
.
*/
.

% 
% HoldsAt(Sleep2(HumanTarget1),0).
 /*
.
*/
.

% 
% !HoldsAt(Injured(HumanTarget1),0).
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:60
% [object] % !HoldsAt(Holding(HumanTarget1,object),0).
 /*
.
*/
.

% 
% HoldsAt(At(HumanTarget1,Outside1),0).
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:62
% [physobj1,physobj2] % !HoldsAt(Inside(physobj1,physobj2),0).
 /*
.
*/
.

% 
% 
%; prune
% sort shoota, shootb, shooto, shooth, shootp
 /*
.
*/
.

 /*
.
*/
.

 /*
.
*/
.

 /*
.
*/
.

 /*
.
*/
.

% event! Shoot(shoota,shootb,shooto)
 /*
.
*/
.

% event! ShootInjure(shoota,shootb,shooth)
 /*
.
*/
.

% ecnet/Kidnapping.e:68
% event! ShootKill(shoota,shootb,shooth)
 /*
.
*/
.

% event! ShootDamage(shoota,shootb,shootp)
 /*
.
*/
.

% event! ShootDestroy(shoota,shootb,shootp)
 /*
.
*/
.

% shoota! Perp1
 /*
.
*/
.
.

% shootb! Gun1
 /*
.
*/
.
.

% shooto! HumanTarget1
 /*
.
*/
.
.

% ecnet/Kidnapping.e:74
% shooth! HumanTarget1
 /*
.
*/
.
.

% 
%; room-scale topological space
% outside Outside1
 /*
.
*/
.
.

% outside Outside2
 /*
.
*/
.
.

% room Inside1
 /*
.
*/
.
.

% ecnet/Kidnapping.e:80
% door Door1
 /*
.
*/
.
.

% building Building1
 /*
.
*/
.
.

% street Street1
 /*
.
*/
.
.

% Side1(Door1)=Outside2.
 /*
.
*/
.

% 
% Side2(Door1)=Inside1.
 /*
.
*/
.

% 
% LookOutOnto(Inside1)=Outside1.
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:86
% Floor(Inside1)=1.
 /*
.
*/
.

% 
% BuildingOf(Inside1)=Building1.
 /*
.
*/
.

% 
% Side1(Street1)=Outside1.
 /*
.
*/
.

% 
% Side2(Street1)=Outside2.
 /*
.
*/
.

% 
% 
% HoldsAt(Calm(Perp1),0).
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:92
% HoldsAt(Alive(Perp1),0).
 /*
.
*/
.

% 
% HoldsAt(Awake(Perp1),0).
 /*
.
*/
.

% 
% HoldsAt(Standing(Perp1),0).
 /*
.
*/
.

% 
% HoldsAt(Sleep2(Perp1),0).
 /*
.
*/
.

% 
% !HoldsAt(Injured(Perp1),0).
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:97
% [object] % !HoldsAt(Holding(Perp1,object),0).
 /*
.
*/
.

% 
% HoldsAt(At(Gun1,Outside2),0).
 /*
.
*/
.

% 
% HoldsAt(At(Perp1,Outside2),0).
 /*
.
*/
.

% 
% HoldsAt(At(Bullet1,Outside2),0).
 /*
.
*/
.

% 
% HoldsAt(DoorIsOpen(Door1),0).
 /*
.
*/
.

% 
% HoldsAt(DoorUnlocked(Door1),0).
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:103
% [agent1,agent2] % !HoldsAt(ThreatenedBy(agent1,agent2),0).
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:104
% [agent1,agent2] % !HoldsAt(AngryAt(agent1,agent2),0).
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:105
% [physobj1,physobj2] % !HoldsAt(Inside(physobj1,physobj2),0).
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:106
% [agent,object] % !HoldsAt(Love(agent,object),0).
 /*
.
*/
.

% 
% 
%; narrative
% Happens(PickUp(Perp1,Gun1),0).
 /*
.
*/
.

% 
% Happens(PickUp(Perp1,Bullet1),1).
 /*
.
*/
.

% 
% Happens(PutInside(Perp1,Bullet1,Gun1),2).
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:112
% Happens(WalkStreet21(Perp1,Street1),3).
 /*
.
*/
.

% 
% Happens(Threaten(Perp1,HumanTarget1,Gun1),4).
 /*
.
*/
.

% 
% Happens(Grab(Perp1,HumanTarget1),5).
 /*
.
*/
.

% 
% Happens(WalkStreet12(Perp1,Street1),6).
 /*
.
*/
.

% 
% Happens(WalkThroughDoor12(Perp1,Door1),7).
 /*
.
*/
.

% 
% Happens(LetGoOf(Perp1,HumanTarget1),8).
 /*
.
*/
.

% 
% ecnet/Kidnapping.e:118
% Happens(Shoot(Perp1,Gun1,HumanTarget1),9).
 /*
.
*/
.

% 
% Happens(ShootKill(Perp1,Gun1,HumanTarget1),9).
 /*
.
*/
.

% 
% 
% range time 0 10
 /*
.
*/
.

% range offset 0 3
 /*
.
*/
.

% range diameter 0 0
 /*
.
*/
.

% ecnet/Kidnapping.e:124
% 
% completion Happens
 /*
.
*/
.

% 
%; End of file.
