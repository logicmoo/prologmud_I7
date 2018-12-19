
:-include(library('ec_planner/ec_test_incl')).
% loading('ecnet/Kidnapping.e')
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
ignore(skyOf).
*/

 /*
ignore(groundOf).
*/

 /*
ignore(near).
*/

 /*
ignore(walkFromTo).
*/

 /*
ignore(runFromTo).
*/

% ignore RollAlong, Diameter, Move, HoldSome
 /*
ignore(rollAlong).
*/

 /*
ignore(diameter).
*/

 /*
ignore(move).
*/

 /*
ignore(holdSome).
*/

% ignore On, DoorUnlock, DoorLock
 /*
ignore(on).
*/

 /*
ignore(doorUnlock).
*/

 /*
ignore(doorLock).
*/

% ignore WalkDownStaircase, WalkUpStaircase
 /*
ignore(walkDownStaircase).
*/

 /*
ignore(walkUpStaircase).
*/

% 
% ecnet/Kidnapping.e:30
% ignore Request, KnowRequest, Order, KnowOrder, SayGoodbye
 /*
ignore(request).
*/

 /*
ignore(knowRequest).
*/

 /*
ignore(order).
*/

 /*
ignore(knowOrder).
*/

 /*
ignore(sayGoodbye).
*/

% ignore IntentionToWalkIn, InvitedIn
 /*
ignore(intentionToWalkIn).
*/

 /*
ignore(invitedIn).
*/

% ignore Snowing
 /*
ignore(snowing).
*/

% ignore Like, Dislike, LikeSnow
 /*
ignore(like).
*/

 /*
ignore(dislike).
*/

 /*
ignore(likeSnow).
*/

% 
% load foundations/Root.e
% ecnet/Kidnapping.e:36
% load foundations/EC.e% load answers/Mueller2003/Ontology.e
% loading('ecnet/Ontology.e')
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
subsort(diameter,integer).
*/
subsort(diameter,integer).
sort(diameter).
sort(integer).

% 
%; object
% 
% sort object
 /*
sort(object).
*/
sort(object).

% 
% sort agent: object
 /*
subsort(agent,object).
*/
subsort(agent,object).
sort(agent).
sort(object).

% 
% sort physobj: object
 /*
subsort(physobj,object).
*/
subsort(physobj,object).
sort(physobj).
sort(object).

% sort bed: physobj
 /*
subsort(bed,physobj).
*/
subsort(bed,physobj).
sort(bed).
sort(physobj).

% sort snowflake: physobj
 /*
subsort(snowflake,physobj).
*/
subsort(snowflake,physobj).
sort(snowflake).
sort(physobj).

% sort sky: physobj
 /*
subsort(sky,physobj).
*/
subsort(sky,physobj).
sort(sky).
sort(physobj).

% 
% sort stuff: physobj
 /*
subsort(stuff,physobj).
*/
subsort(stuff,physobj).
sort(stuff).
sort(physobj).

% 
% sort surface: physobj
 /*
subsort(surface,physobj).
*/
subsort(surface,physobj).
sort(surface).
sort(physobj).

% sort ground: surface
 /*
subsort(ground,surface).
*/
subsort(ground,surface).
sort(ground).
sort(surface).

% 
% sort snow: stuff
 /*
subsort(snow,stuff).
*/
subsort(snow,stuff).
sort(snow).
sort(stuff).

% sort ball
 /*
sort(ball).
*/
sort(ball).

% 
% sort food: physobj
 /*
subsort(food,physobj).
*/
subsort(food,physobj).
sort(food).
sort(physobj).

% sort fruit: food
 /*
subsort(fruit,food).
*/
subsort(fruit,food).
sort(fruit).
sort(food).

% sort orange: fruit
 /*
subsort(orange,fruit).
*/
subsort(orange,fruit).
sort(orange).
sort(fruit).

% sort salad: food
 /*
subsort(salad,food).
*/
subsort(salad,food).
sort(salad).
sort(food).

% 
% sort clothing: physobj
 /*
subsort(clothing,physobj).
*/
subsort(clothing,physobj).
sort(clothing).
sort(physobj).

% sort scarf: clothing
 /*
subsort(scarf,clothing).
*/
subsort(scarf,clothing).
sort(scarf).
sort(clothing).

% sort hat: clothing
 /*
subsort(hat,clothing).
*/
subsort(hat,clothing).
sort(hat).
sort(clothing).

% 
% sort vegetablematter: physobj
 /*
subsort(vegetablematter,physobj).
*/
subsort(vegetablematter,physobj).
sort(vegetablematter).
sort(physobj).

% sort coal: vegetablematter
 /*
subsort(coal,vegetablematter).
*/
subsort(coal,vegetablematter).
sort(coal).
sort(vegetablematter).

% 
% sort bodypart: physobj
 /*
subsort(bodypart,physobj).
*/
subsort(bodypart,physobj).
sort(bodypart).
sort(physobj).

% sort hand: bodypart
 /*
subsort(hand,bodypart).
*/
subsort(hand,bodypart).
sort(hand).
sort(bodypart).

% 
% sort papertowels: physobj
 /*
subsort(papertowels,physobj).
*/
subsort(papertowels,physobj).
sort(papertowels).
sort(physobj).

% sort device: physobj
 /*
subsort(device,physobj).
*/
subsort(device,physobj).
sort(device).
sort(physobj).

% sort electronicdevice: device
 /*
subsort(electronicdevice,device).
*/
subsort(electronicdevice,device).
sort(electronicdevice).
sort(device).

% sort lamp: electronicdevice
 /*
subsort(lamp,electronicdevice).
*/
subsort(lamp,electronicdevice).
sort(lamp).
sort(electronicdevice).

% 
% sort cat: physobj
 /*
subsort(cat,physobj).
*/
subsort(cat,physobj).
sort(cat).
sort(physobj).

% sort horse: physobj
 /*
subsort(horse,physobj).
*/
subsort(horse,physobj).
sort(horse).
sort(physobj).

% 
% sort weapon: physobj
 /*
subsort(weapon,physobj).
*/
subsort(weapon,physobj).
sort(weapon).
sort(physobj).

% sort gun: weapon
 /*
subsort(gun,weapon).
*/
subsort(gun,weapon).
sort(gun).
sort(weapon).

% sort bomb: weapon
 /*
subsort(bomb,weapon).
*/
subsort(bomb,weapon).
sort(bomb).
sort(weapon).

% sort bullet: weapon
 /*
subsort(bullet,weapon).
*/
subsort(bullet,weapon).
sort(bullet).
sort(weapon).

% 
%; location
% 
% sort location
 /*
sort(location).
*/
sort(location).

% sort room: location, outside: location
 /*
subsort(room,location).
*/
subsort(room,location).
sort(room).
sort(location).

 /*
subsort(outside,location).
*/
subsort(outside,location).
sort(outside).
sort(location).

% 
%; portal
% 
% sort portal
 /*
sort(portal).
*/
sort(portal).

% sort door: portal, staircase: portal
 /*
subsort(door,portal).
*/
subsort(door,portal).
sort(door).
sort(portal).

 /*
subsort(staircase,portal).
*/
subsort(staircase,portal).
sort(staircase).
sort(portal).

% sort street: portal
 /*
subsort(street,portal).
*/
subsort(street,portal).
sort(street).
sort(portal).

% sort track: portal
 /*
subsort(track,portal).
*/
subsort(track,portal).
sort(track).
sort(portal).

% 
% sort building
 /*
sort(building).
*/
sort(building).

% 
% sort fire: object
 /*
subsort(fire,object).
*/
subsort(fire,object).
sort(fire).
sort(object).

% sort smoke: physobj
 /*
subsort(smoke,physobj).
*/
subsort(smoke,physobj).
sort(smoke).
sort(physobj).

% 
% sort furniture: physobj
 /*
subsort(furniture,physobj).
*/
subsort(furniture,physobj).
sort(furniture).
sort(physobj).

% sort chair: furniture
 /*
subsort(chair,furniture).
*/
subsort(chair,furniture).
sort(chair).
sort(furniture).

% sort table: furniture
 /*
subsort(table,furniture).
*/
subsort(table,furniture).
sort(table).
sort(furniture).

% 
% sort bill: physobj
 /*
subsort(bill,physobj).
*/
subsort(bill,physobj).
sort(bill).
sort(physobj).

% sort ticket: physobj
 /*
subsort(ticket,physobj).
*/
subsort(ticket,physobj).
sort(ticket).
sort(physobj).

% sort envelope: physobj
 /*
subsort(envelope,physobj).
*/
subsort(envelope,physobj).
sort(envelope).
sort(physobj).

% 
% sort text: physobj
 /*
subsort(text,physobj).
*/
subsort(text,physobj).
sort(text).
sort(physobj).

% sort book: text
 /*
subsort(book,text).
*/
subsort(book,text).
sort(book).
sort(text).

% sort letter: text
 /*
subsort(letter,text).
*/
subsort(letter,text).
sort(letter).
sort(text).

% sort menu: text
 /*
subsort(menu,text).
*/
subsort(menu,text).
sort(menu).
sort(text).

% 
% sort paper: physobj
 /*
subsort(paper,physobj).
*/
subsort(paper,physobj).
sort(paper).
sort(physobj).

% 
% sort content
 /*
sort(content).
*/
sort(content).

% sort script
 /*
sort(script).
*/
sort(script).

% 
% sort container: physobj
 /*
subsort(container,physobj).
*/
subsort(container,physobj).
sort(container).
sort(physobj).

% sort cigarette: physobj
 /*
subsort(cigarette,physobj).
*/
subsort(cigarette,physobj).
sort(cigarette).
sort(physobj).

% sort ashtray: physobj
 /*
subsort(ashtray,physobj).
*/
subsort(ashtray,physobj).
sort(ashtray).
sort(physobj).

% sort umbrella: physobj
 /*
subsort(umbrella,physobj).
*/
subsort(umbrella,physobj).
sort(umbrella).
sort(physobj).

% 
% sort pen: physobj
 /*
subsort(pen,physobj).
*/
subsort(pen,physobj).
sort(pen).
sort(physobj).

% 
%; End of file.
% load answers/Mueller2004c/OTSpaceM.e
 /*
load('answers/Mueller2004c/OTSpaceM.e').
*/
load('answers/Mueller2004c/OTSpaceM.e').

% load answers/Mueller2004c/RTSpaceM.e
 /*
load('answers/Mueller2004c/RTSpaceM.e').
*/
load('answers/Mueller2004c/RTSpaceM.e').

% load answers/Mueller2003/Feeling.e
% loading('ecnet/Feeling.e')
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
fluent(happy(agent)).
*/
fluent(happy(agent)).

% 
%; agent is emotionally neutral or calm.
% fluent Calm(agent)
 /*
fluent(calm(agent)).
*/
fluent(calm(agent)).

% 
%; agent is unhappy.
% fluent Unhappy(agent)
 /*
fluent(unhappy(agent)).
*/
fluent(unhappy(agent)).

% 
%; At any moment, an agent is in one of three emotional states:
% xor Happy, Calm, Unhappy
 /*
xor([happy,calm,unhappy]).
*/
xor([happy,calm,unhappy]).

% 
%; agent becomes happy.
% event BecomeHappy(agent)
 /*
event(becomeHappy(agent)).
*/
event(becomeHappy(agent)).

% 
%; agent becomes calm.
% event BecomeCalm(agent)
 /*
event(becomeCalm(agent)).
*/
event(becomeCalm(agent)).

% 
%; agent becomes unhappy.
% event BecomeUnhappy(agent)
 /*
event(becomeUnhappy(agent)).
*/
event(becomeUnhappy(agent)).

% 
%; A number of effect and precondition axioms deal with the transitions
%; from one emotional state to another:
% ecnet/Kidnapping.e:41
% [agent,time]% 
% Initiates(BecomeHappy(agent),Happy(agent),time).
 /*
initiates(becomeHappy(Agent),happy(Agent),Time).
*/
axiom(initiates(becomeHappy(Agent),happy(Agent),Time),
      []).

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
axiom(terminates(becomeHappy(Agent),calm(Agent),Time),
      [holds_at(calm(Agent),Time)]).

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
axiom(terminates(becomeHappy(Agent),
		 unhappy(Agent),
		 Time),
      [holds_at(unhappy(Agent),Time)]).

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
axiom(holds_at(neg(happy(Agent)),Time),
      [happens(becomeHappy(Agent),Time)]).

% 
% 
% ecnet/Kidnapping.e:41
% [agent,time]% 
% Initiates(BecomeCalm(agent),Calm(agent),time).
 /*
initiates(becomeCalm(Agent),calm(Agent),Time).
*/
axiom(initiates(becomeCalm(Agent),calm(Agent),Time),
      []).

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
axiom(terminates(becomeCalm(Agent),happy(Agent),Time),
      [holds_at(happy(Agent),Time)]).

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
axiom(terminates(becomeCalm(Agent),
		 unhappy(Agent),
		 Time),
      [holds_at(unhappy(Agent),Time)]).

% 
% 
% ecnet/Kidnapping.e:41
% [agent,time]% 
% Happens(BecomeCalm(agent),time) -> !HoldsAt(Calm(agent),time).
 /*
happens(becomeCalm(Agent), Time) ->
    not(holds_at(calm(Agent), Time)).
*/
axiom(holds_at(neg(calm(Agent)),Time),
      [happens(becomeCalm(Agent),Time)]).

% 
% 
% ecnet/Kidnapping.e:41
% [agent,time]% 
% Initiates(BecomeUnhappy(agent),Unhappy(agent),time).
 /*
initiates(becomeUnhappy(Agent),unhappy(Agent),Time).
*/
axiom(initiates(becomeUnhappy(Agent),
		unhappy(Agent),
		Time),
      []).

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
axiom(terminates(becomeUnhappy(Agent),
		 happy(Agent),
		 Time),
      [holds_at(happy(Agent),Time)]).

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
axiom(terminates(becomeUnhappy(Agent),
		 calm(Agent),
		 Time),
      [holds_at(calm(Agent),Time)]).

% 
% 
% ecnet/Kidnapping.e:41
% [agent,time]% 
% Happens(BecomeUnhappy(agent),time) -> !HoldsAt(Unhappy(agent),time).
 /*
happens(becomeUnhappy(Agent), Time) ->
    not(holds_at(unhappy(Agent), Time)).
*/
axiom(holds_at(neg(unhappy(Agent)),Time),
      [happens(becomeUnhappy(Agent),Time)]).

% 
% 
%; anger
% 
% fluent AngryAt(agent,agent)
 /*
fluent(angryAt(agent,agent)).
*/
fluent(angryAt(agent,agent)).

% 
% event BecomeAngryAt(agent,agent)
 /*
event(becomeAngryAt(agent,agent)).
*/
event(becomeAngryAt(agent,agent)).

% 
% ecnet/Kidnapping.e:41
% [agent1,agent2,time]% 
% Initiates(BecomeAngryAt(agent1,agent2),AngryAt(agent1,agent2),time).
 /*
initiates(becomeAngryAt(Agent1,Agent2),
	  angryAt(Agent1,Agent2),
	  Time).
*/
axiom(initiates(becomeAngryAt(Agent1,Agent2),
		angryAt(Agent1,Agent2),
		Time),
      []).

% 
% 
% ecnet/Kidnapping.e:41
% [agent1,agent2,time]% 
% Terminates(BecomeHappy(agent1),AngryAt(agent1,agent2),time).
 /*
terminates(becomeHappy(Agent1),
	   angryAt(Agent1,Agent2),
	   Time).
*/
axiom(terminates(becomeHappy(Agent1),
		 angryAt(Agent1,Agent2),
		 Time),
      []).

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
axiom(happens(becomeUnhappy(Agent1),Time),
      [happens(becomeAngryAt(Agent1,Agent2),Time)]).

% 
% 
%; attitudes
% 
%; agent likes object.
% fluent Like(agent,object)
 /*
fluent(like(agent,object)).
*/
fluent(like(agent,object)).

%; agent loves object.
% fluent Love(agent,object)
 /*
fluent(love(agent,object)).
*/
fluent(love(agent,object)).

%; agent dislikes object.
% fluent Dislike(agent,object)
 /*
fluent(dislike(agent,object)).
*/
fluent(dislike(agent,object)).

% 
%; agent likes snow.
% fluent LikeSnow(agent)
 /*
fluent(likeSnow(agent)).
*/
fluent(likeSnow(agent)).

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
axiom(requires(becomeHappy(Agent),Time),
      [ holds_at(neg(happy(Agent)),Time),
	holds_at(awake(Agent),Time),
	holds_at(likeSnow(Agent),Time),
	holds_at(at(Agent,Room),Time),
	equals(lookOutOnto(Room),Outside),
	holds_at(snowing(Outside),Time)
      ]).

% 
% 
%; We introduced LikeSnow above since Like
%; can only be used to represent that an agent likes a
%; particular object, not snow in general.
% 
% event Smile(agent)
 /*
event(smile(agent)).
*/
event(smile(agent)).

% 
%; End of file.
% load answers/Mueller2004c/Condition.e
% loading('ecnet/Condition.e')
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
fluent(alive(agent)).
*/
fluent(alive(agent)).

% 
% fluent Dead(agent)
 /*
fluent(dead(agent)).
*/
fluent(dead(agent)).

% noninertial Dead
 /*
noninertial(dead).
*/
noninertial(dead).

% 
% fluent Injured(agent)
 /*
fluent(injured(agent)).
*/
fluent(injured(agent)).

% 
% event Kill(object,agent)
 /*
event(kill(object,agent)).
*/
event(kill(object,agent)).

% event Injure(object,agent)
 /*
event(injure(object,agent)).
*/
event(injure(object,agent)).

% event HealInjured(agent)
 /*
event(healInjured(agent)).
*/
event(healInjured(agent)).

% 
% ecnet/Kidnapping.e:42
% [agent,time] % HoldsAt(Alive(agent),time) <-> !HoldsAt(Dead(agent),time).
 /*
holds_at(alive(Agent), Time) <->
    not(holds_at(dead(Agent), Time)).
*/
axiom(holds_at(neg(dead(Agent)),Time),
      [holds_at(alive(Agent),Time)]).
axiom(holds_at(alive(Agent),Time),
      [holds_at(neg(dead(Agent)),Time)]).

% 
% ecnet/Kidnapping.e:42
% [agent,time] % HoldsAt(Injured(agent),time) -> HoldsAt(Alive(agent),time).
 /*
holds_at(injured(Agent), Time) ->
    holds_at(alive(Agent), Time).
*/
axiom(holds_at(alive(Agent),Time),
      [holds_at(injured(Agent),Time)]).

% 
% 
% ecnet/Kidnapping.e:42
% [object,agent,time]% 
% Terminates(Kill(object,agent),Alive(agent),time).
 /*
terminates(kill(Object,Agent),
	   alive(Agent),
	   Time).
*/
axiom(terminates(kill(Object,Agent),
		 alive(Agent),
		 Time),
      []).

% 
% 
% ecnet/Kidnapping.e:42
% [object,agent,time]% 
% Initiates(Injure(object,agent),Injured(agent),time).
 /*
initiates(injure(Object,Agent),
	  injured(Agent),
	  Time).
*/
axiom(initiates(injure(Object,Agent),
		injured(Agent),
		Time),
      []).

% 
% 
% ecnet/Kidnapping.e:42
% [agent,time]% 
% Terminates(HealInjured(agent),Injured(agent),time).
 /*
terminates(healInjured(Agent),injured(Agent),Time).
*/
axiom(terminates(healInjured(Agent),
		 injured(Agent),
		 Time),
      []).

% 
% 
% fluent Intact(physobj)
 /*
fluent(intact(physobj)).
*/
fluent(intact(physobj)).

% 
% fluent Damaged(physobj)
 /*
fluent(damaged(physobj)).
*/
fluent(damaged(physobj)).

% 
% fluent Destroyed(physobj)
 /*
fluent(destroyed(physobj)).
*/
fluent(destroyed(physobj)).

% 
%; At any time, a physical object is either intact, damaged, or destroyed:
% xor Intact, Damaged, Destroyed
 /*
xor([intact,damaged,destroyed]).
*/
xor([intact,damaged,destroyed]).

% 
% event Damage(object,physobj)
 /*
event(damage(object,physobj)).
*/
event(damage(object,physobj)).

% 
% event Destroy(object,physobj)
 /*
event(destroy(object,physobj)).
*/
event(destroy(object,physobj)).

% 
% event Repair(object,physobj)
 /*
event(repair(object,physobj)).
*/
event(repair(object,physobj)).

% 
% ecnet/Kidnapping.e:42
% [object,physobj,time]% 
% Happens(Damage(object,physobj),time) ->
% HoldsAt(Intact(physobj),time).
 /*
happens(damage(Object, Physobj), Time) ->
    holds_at(intact(Physobj), Time).
*/
axiom(holds_at(intact(Physobj),Time),
      [happens(damage(Object,Physobj),Time)]).

% 
% 
% ecnet/Kidnapping.e:42
% [object,physobj,time]% 
% Initiates(Damage(object,physobj),Damaged(physobj),time).
 /*
initiates(damage(Object,Physobj),
	  damaged(Physobj),
	  Time).
*/
axiom(initiates(damage(Object,Physobj),
		damaged(Physobj),
		Time),
      []).

% 
% 
% ecnet/Kidnapping.e:42
% [object,physobj,time]% 
% Terminates(Damage(object,physobj),Intact(physobj),time).
 /*
terminates(damage(Object,Physobj),
	   intact(Physobj),
	   Time).
*/
axiom(terminates(damage(Object,Physobj),
		 intact(Physobj),
		 Time),
      []).

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
axiom(holds_at(intact(Physobj),Time) ; holds_at(damaged(Physobj),Time),
      [happens(destroy(Object,Physobj),Time)]).

% 
% 
% ecnet/Kidnapping.e:42
% [object,physobj,time]% 
% Initiates(Destroy(object,physobj),Destroyed(physobj),time).
 /*
initiates(destroy(Object,Physobj),
	  destroyed(Physobj),
	  Time).
*/
axiom(initiates(destroy(Object,Physobj),
		destroyed(Physobj),
		Time),
      []).

% 
% 
% ecnet/Kidnapping.e:42
% [object,physobj,time]% 
% Terminates(Destroy(object,physobj),Intact(physobj),time).
 /*
terminates(destroy(Object,Physobj),
	   intact(Physobj),
	   Time).
*/
axiom(terminates(destroy(Object,Physobj),
		 intact(Physobj),
		 Time),
      []).

% 
% 
% ecnet/Kidnapping.e:42
% [object,physobj,time]% 
% Terminates(Destroy(object,physobj),Damaged(physobj),time).
 /*
terminates(destroy(Object,Physobj),
	   damaged(Physobj),
	   Time).
*/
axiom(terminates(destroy(Object,Physobj),
		 damaged(Physobj),
		 Time),
      []).

% 
% 
% ecnet/Kidnapping.e:42
% [object,physobj,time]% 
% Initiates(Repair(object,physobj),Intact(physobj),time).
 /*
initiates(repair(Object,Physobj),
	  intact(Physobj),
	  Time).
*/
axiom(initiates(repair(Object,Physobj),
		intact(Physobj),
		Time),
      []).

% 
% 
%; end of file.
% load answers/Mueller2004c/Gun.e
% loading('ecnet/Gun.e')
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
fluent(loaded(gun,bullet)).
*/
fluent(loaded(gun,bullet)).

% noninertial Loaded
 /*
noninertial(loaded).
*/
noninertial(loaded).

% 
% ecnet/Kidnapping.e:43
% [gun,bullet,time]% 
% HoldsAt(Inside(bullet,gun),time) <->
% HoldsAt(Loaded(gun,bullet),time).
 /*
holds_at(inside(Bullet, Gun), Time) <->
    holds_at(loaded(Gun, Bullet), Time).
*/
axiom(holds_at(loaded(Gun,Bullet),Time),
      [holds_at(inside(Bullet,Gun),Time)]).
axiom(holds_at(inside(Bullet,Gun),Time),
      [holds_at(loaded(Gun,Bullet),Time)]).

% 
% 
% event Shoot(agent,gun,object)
 /*
event(shoot(agent,gun,object)).
*/
event(shoot(agent,gun,object)).

% 
% event ShootInjure(agent,gun,agent)
 /*
event(shootInjure(agent,gun,agent)).
*/
event(shootInjure(agent,gun,agent)).

% 
% event ShootKill(agent,gun,agent)
 /*
event(shootKill(agent,gun,agent)).
*/
event(shootKill(agent,gun,agent)).

% 
% event ShootDamage(agent,gun,physobj)
 /*
event(shootDamage(agent,gun,physobj)).
*/
event(shootDamage(agent,gun,physobj)).

% 
% event ShootDestroy(agent,gun,physobj)
 /*
event(shootDestroy(agent,gun,physobj)).
*/
event(shootDestroy(agent,gun,physobj)).

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
axiom(terminates(shoot(Agent,Gun,Object),
		 inside(Bullet,Gun),
		 Time),
      [holds_at(inside(Bullet,Gun),Time)]).

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
axiom(terminates(shoot(Agent,Gun,Object),
		 at(Bullet,Location2),
		 Time),
      [ holds_at(inside(Bullet,Gun),Time),
	holds_at(at(Gun,Location1),Time),
	diff(Location1,Location2)
      ]).

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
axiom(initiates(shoot(Agent,Gun,Object),
		at(Bullet,Location),
		Time),
      [ holds_at(at(Object,Location),Time),
	holds_at(inside(Bullet,Gun),Time)
      ]).

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
axiom(holds_at(holding(Agent,Gun),Time) ',' exists([Bullet],
	     holds_at(loaded(Gun,Bullet),Time)) ',' exists([Location],
	     holds_at(at(Agent,Location),Time) ',' holds_at(at(Object,Location),Time)),
      [happens(shoot(Agent,Gun,Object),Time)]).

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
axiom(happens(shootInjure(Agent1,Gun,Agent2),
	      Time),
      [happens(shoot(Agent1,Gun,Agent2),Time)]) ; happens(shootKill(Agent1,Gun,Agent2),Time).

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
axiom(requires(kill(Bullet,Agent2),Time),
      [ holds_at(inside(Bullet,Gun),Time),
	happens(shootKill(Agent1,Gun,Agent2),
		Time)
      ]).

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
axiom(requires(injure(Bullet,Agent2),Time),
      [ holds_at(inside(Bullet,Gun),Time),
	happens(shootInjure(Agent1,Gun,Agent2),
		Time)
      ]).

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
axiom(happens(shootDamage(Agent,Gun,Physobj),
	      Time),
      [happens(shoot(Agent,Gun,Physobj),Time)]) ; happens(shootDestroy(Agent,Gun,Physobj),Time).

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
axiom(requires(damage(Bullet,Physobj),Time),
      [ holds_at(inside(Bullet,Gun),Time),
	happens(shootDamage(Agent,Gun,Physobj),
		Time)
      ]).

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
axiom(requires(destroy(Bullet,Physobj),Time),
      [ holds_at(inside(Bullet,Gun),Time),
	happens(shootDestroy(Agent,Gun,Physobj),
		Time)
      ]).

% 
% 
%; End of file.
% load answers/Mueller2003/Sleep.e
% loading('ecnet/Sleep.e')
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
event(wakeUp(agent)).
*/
event(wakeUp(agent)).

% 
%; agent gets tired.
% event GetTired(agent)
 /*
event(getTired(agent)).
*/
event(getTired(agent)).

% 
%; agent falls asleep.
% event FallAsleep(agent)
 /*
event(fallAsleep(agent)).
*/
event(fallAsleep(agent)).

% 
%; agent is asleep.
% fluent Sleep0(agent)
 /*
fluent(sleep0(agent)).
*/
fluent(sleep0(agent)).

%; agent is awake and in bed.
% fluent Sleep1(agent)
 /*
fluent(sleep1(agent)).
*/
fluent(sleep1(agent)).

%; agent is awake, out of bed, and undressed.
% fluent Sleep2(agent)
 /*
fluent(sleep2(agent)).
*/
fluent(sleep2(agent)).

%; agent is awake and dressed.
% fluent Sleep3(agent)
 /*
fluent(sleep3(agent)).
*/
fluent(sleep3(agent)).

%; agent is tired and dressed.
% fluent Sleep4(agent)
 /*
fluent(sleep4(agent)).
*/
fluent(sleep4(agent)).

%; agent is tired and undressed.
% fluent Sleep5(agent)
 /*
fluent(sleep5(agent)).
*/
fluent(sleep5(agent)).

%; agent is in bed, waiting to fall asleep.
% fluent Sleep6(agent)
 /*
fluent(sleep6(agent)).
*/
fluent(sleep6(agent)).

% 
%; At any time, an agent is in one of seven sleep states:
% xor Sleep0, Sleep1, Sleep2, Sleep3, Sleep4, Sleep5, Sleep6
 /*
xor([sleep0,sleep1,sleep2,sleep3,sleep4,sleep5,sleep6]).
*/
xor([sleep0,sleep1,sleep2,sleep3,sleep4,sleep5,sleep6]).

% 
%; constraints
% 
%; agent is asleep.
% fluent Asleep(agent)
 /*
fluent(asleep(agent)).
*/
fluent(asleep(agent)).

%; agent is awake.
% fluent Awake(agent)
 /*
fluent(awake(agent)).
*/
fluent(awake(agent)).

% noninertial Asleep
 /*
noninertial(asleep).
*/
noninertial(asleep).

% noninertial Awake
 /*
noninertial(awake).
*/
noninertial(awake).

% 
%; Sleep0 indicates that the agent is asleep:
% ecnet/Kidnapping.e:44
% [agent,time] % HoldsAt(Asleep(agent),time) <-> HoldsAt(Sleep0(agent),time).
 /*
holds_at(asleep(Agent), Time) <->
    holds_at(sleep0(Agent), Time).
*/
axiom(holds_at(sleep0(Agent),Time),
      [holds_at(asleep(Agent),Time)]).
axiom(holds_at(asleep(Agent),Time),
      [holds_at(sleep0(Agent),Time)]).

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
axiom(holds_at(sleep1(Agent),Time) ; holds_at(sleep2(Agent),Time) ; holds_at(sleep3(Agent),Time) ; holds_at(sleep4(Agent),Time) ; holds_at(sleep5(Agent),Time) ; holds_at(sleep6(Agent),Time),
      [holds_at(awake(Agent),Time)]).
axiom(holds_at(awake(Agent),Time),
      [ holds_at(sleep1(Agent),Time) ; holds_at(sleep2(Agent),Time) ; holds_at(sleep3(Agent),Time) ; holds_at(sleep4(Agent),Time) ; holds_at(sleep5(Agent),Time) ; holds_at(sleep6(Agent),Time)
      ]).

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
terminates(wakeUp(Agent),sleep0(Agent),Time).
*/
axiom(terminates(wakeUp(Agent),sleep0(Agent),Time),
      []).

% 
% 
% ecnet/Kidnapping.e:44
% [agent,time] % Initiates(WakeUp(agent),Sleep1(agent),time).
 /*
initiates(wakeUp(Agent),sleep1(Agent),Time).
*/
axiom(initiates(wakeUp(Agent),sleep1(Agent),Time),[]).

% 
% 
% ecnet/Kidnapping.e:44
% [agent,time] % Happens(WakeUp(agent),time) -> HoldsAt(Sleep0(agent),time).
 /*
happens(wakeUp(Agent), Time) ->
    holds_at(sleep0(Agent), Time).
*/
axiom(holds_at(sleep0(Agent),Time),
      [happens(wakeUp(Agent),Time)]).

% 
% 
%;--
% 
%; Getting out of bed causes a transition from Sleep1
%; to Sleep2:
% ecnet/Kidnapping.e:44
% [agent,bed,time] % Terminates(RiseFrom(agent,bed),Sleep1(agent),time).
 /*
terminates(riseFrom(Agent,Bed),
	   sleep1(Agent),
	   Time).
*/
axiom(terminates(riseFrom(Agent,Bed),
		 sleep1(Agent),
		 Time),
      []).

% 
% 
% ecnet/Kidnapping.e:44
% [agent,bed,time] % Initiates(RiseFrom(agent,bed),Sleep2(agent),time).
 /*
initiates(riseFrom(Agent,Bed),
	  sleep2(Agent),
	  Time).
*/
axiom(initiates(riseFrom(Agent,Bed),
		sleep2(Agent),
		Time),
      []).

% 
% 
% ecnet/Kidnapping.e:44
% [agent,bed,time]% 
% Happens(RiseFrom(agent,bed),time) -> HoldsAt(Sleep1(agent),time).
 /*
happens(riseFrom(Agent, Bed), Time) ->
    holds_at(sleep1(Agent), Time).
*/
axiom(holds_at(sleep1(Agent),Time),
      [happens(riseFrom(Agent,Bed),Time)]).

% 
% 
%;--
% 
%; Getting dressed causes a transition from Sleep2
%; to Sleep3, the normal state of awakeness:
% ecnet/Kidnapping.e:44
% [agent,time] % Terminates(GetDressed(agent),Sleep2(agent),time).
 /*
terminates(getDressed(Agent),sleep2(Agent),Time).
*/
axiom(terminates(getDressed(Agent),
		 sleep2(Agent),
		 Time),
      []).

% 
% 
% ecnet/Kidnapping.e:44
% [agent,time] % Initiates(GetDressed(agent),Sleep3(agent),time).
 /*
initiates(getDressed(Agent),sleep3(Agent),Time).
*/
axiom(initiates(getDressed(Agent),sleep3(Agent),Time),
      []).

% 
% 
% ecnet/Kidnapping.e:44
% [agent,time] % Happens(GetDressed(agent),time) -> HoldsAt(Sleep2(agent),time).
 /*
happens(getDressed(Agent), Time) ->
    holds_at(sleep2(Agent), Time).
*/
axiom(holds_at(sleep2(Agent),Time),
      [happens(getDressed(Agent),Time)]).

% 
% 
%;--
% 
%; Getting tired causes a transition from Sleep3
%; to Sleep4:
% ecnet/Kidnapping.e:44
% [agent,time] % Terminates(GetTired(agent),Sleep3(agent),time).
 /*
terminates(getTired(Agent),sleep3(Agent),Time).
*/
axiom(terminates(getTired(Agent),sleep3(Agent),Time),
      []).

% 
% 
% ecnet/Kidnapping.e:44
% [agent,time] % Initiates(GetTired(agent),Sleep4(agent),time).
 /*
initiates(getTired(Agent),sleep4(Agent),Time).
*/
axiom(initiates(getTired(Agent),sleep4(Agent),Time),
      []).

% 
% 
% ecnet/Kidnapping.e:44
% [agent,time] % Happens(GetTired(agent),time) -> HoldsAt(Sleep3(agent),time).
 /*
happens(getTired(Agent), Time) ->
    holds_at(sleep3(Agent), Time).
*/
axiom(holds_at(sleep3(Agent),Time),
      [happens(getTired(Agent),Time)]).

% 
% 
%;--
% 
%; Getting undressed causes a transition from Sleep4
%; to Sleep5:
% ecnet/Kidnapping.e:44
% [agent,time] % Terminates(GetUndressed(agent),Sleep4(agent),time).
 /*
terminates(getUndressed(Agent),sleep4(Agent),Time).
*/
axiom(terminates(getUndressed(Agent),
		 sleep4(Agent),
		 Time),
      []).

% 
% 
% ecnet/Kidnapping.e:44
% [agent,time] % Initiates(GetUndressed(agent),Sleep5(agent),time).
 /*
initiates(getUndressed(Agent),sleep5(Agent),Time).
*/
axiom(initiates(getUndressed(Agent),
		sleep5(Agent),
		Time),
      []).

% 
% 
% ecnet/Kidnapping.e:44
% [agent,time] % Happens(GetUndressed(agent),time) -> HoldsAt(Sleep4(agent),time).
 /*
happens(getUndressed(Agent), Time) ->
    holds_at(sleep4(Agent), Time).
*/
axiom(holds_at(sleep4(Agent),Time),
      [happens(getUndressed(Agent),Time)]).

% 
% 
%;--
% 
%; Lying on a bed causes a transition from Sleep5
%; to Sleep6:
% ecnet/Kidnapping.e:44
% [agent,bed,time] % Terminates(LieOn(agent,bed),Sleep5(agent),time).
 /*
terminates(lieOn(Agent,Bed),sleep5(Agent),Time).
*/
axiom(terminates(lieOn(Agent,Bed),
		 sleep5(Agent),
		 Time),
      []).

% 
% 
% ecnet/Kidnapping.e:44
% [agent,bed,time] % Initiates(LieOn(agent,bed),Sleep6(agent),time).
 /*
initiates(lieOn(Agent,Bed),sleep6(Agent),Time).
*/
axiom(initiates(lieOn(Agent,Bed),
		sleep6(Agent),
		Time),
      []).

% 
% 
% ecnet/Kidnapping.e:44
% [agent,bed,time] % Happens(LieOn(agent,bed),time) -> HoldsAt(Sleep5(agent),time).
 /*
happens(lieOn(Agent, Bed), Time) ->
    holds_at(sleep5(Agent), Time).
*/
axiom(holds_at(sleep5(Agent),Time),
      [happens(lieOn(Agent,Bed),Time)]).

% 
% 
%;--
% 
%; Falling asleep causes a transition from Sleep6
%; to Sleep0:
% ecnet/Kidnapping.e:44
% [agent,time] % Terminates(FallAsleep(agent),Sleep6(agent),time).
 /*
terminates(fallAsleep(Agent),sleep6(Agent),Time).
*/
axiom(terminates(fallAsleep(Agent),
		 sleep6(Agent),
		 Time),
      []).

% 
% 
% ecnet/Kidnapping.e:44
% [agent,time] % Initiates(FallAsleep(agent),Sleep0(agent),time).
 /*
initiates(fallAsleep(Agent),sleep0(Agent),Time).
*/
axiom(initiates(fallAsleep(Agent),sleep0(Agent),Time),
      []).

% 
% 
% ecnet/Kidnapping.e:44
% [agent,time] % Happens(FallAsleep(agent),time) -> HoldsAt(Sleep6(agent),time).
 /*
happens(fallAsleep(Agent), Time) ->
    holds_at(sleep6(Agent), Time).
*/
axiom(holds_at(sleep6(Agent),Time),
      [happens(fallAsleep(Agent),Time)]).

% 
% 
%;--
% 
%; agent acts on being in state Sleep5.
% fluent ActOnSleep5(agent)
 /*
fluent(actOnSleep5(agent)).
*/
fluent(actOnSleep5(agent)).

% noninertial ActOnSleep5
 /*
noninertial(actOnSleep5).
*/
noninertial(actOnSleep5).

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
axiom(holds_at(neg(actOnSleep5(Agent)),Time),
      [holds_at(neg(sleep5(Agent)),Time)]).

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
axiom(requires(lieOn(Agent,Bed),Time),
      [ holds_at(sleep5(Agent),Time),
	holds_at(actOnSleep5(Agent),Time),
	holds_at(at(Agent,Room),Time),
	holds_at(at(Bed,Room),Time)
      ]).

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
axiom(holds_at(sleep5(Agent),Time) ',' holds_at(actOnSleep5(Agent),Time) ',' holds_at(at(Agent,Room),Time) ',' holds_at(at(Bed,Room),Time) ',' some([Room]),
      [happens(lieOn(Agent,Bed),Time)]).

% 
% 
%; (body) posture
% 
%; agent lies on physobj.
% event LieOn(agent,physobj)
 /*
event(lieOn(agent,physobj)).
*/
event(lieOn(agent,physobj)).

% 
%; agent sits on physobj.
% event SitOn(agent,physobj)
 /*
event(sitOn(agent,physobj)).
*/
event(sitOn(agent,physobj)).

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
axiom(holds_at(at(Agent,Location),Time) ',' holds_at(at(Physobj,Location),Time) ',' some([Location]),
      [happens(sitOn(Agent,Physobj),Time)]).

% 
% 
%; agent rises from physobj.
% event RiseFrom(agent,physobj)
 /*
event(riseFrom(agent,physobj)).
*/
event(riseFrom(agent,physobj)).

% 
%; agent is lying on physobj.
% fluent LyingOn(agent,physobj)
 /*
fluent(lyingOn(agent,physobj)).
*/
fluent(lyingOn(agent,physobj)).

%; agent is sitting on physobj.
% fluent SittingOn(agent,physobj)
 /*
fluent(sittingOn(agent,physobj)).
*/
fluent(sittingOn(agent,physobj)).

%; agent is standing.
% fluent Standing(agent)
 /*
fluent(standing(agent)).
*/
fluent(standing(agent)).

% 
%; agent is lying down.
% fluent Lying(agent)
 /*
fluent(lying(agent)).
*/
fluent(lying(agent)).

%; agent is sitting.
% fluent Sitting(agent)
 /*
fluent(sitting(agent)).
*/
fluent(sitting(agent)).

% noninertial Lying
 /*
noninertial(lying).
*/
noninertial(lying).

% noninertial Sitting
 /*
noninertial(sitting).
*/
noninertial(sitting).

% 
%; At any time, an agent is either lying, sitting, or standing:
% xor Lying, Sitting, Standing
 /*
xor([lying,sitting,standing]).
*/
xor([lying,sitting,standing]).

% 
% ecnet/Kidnapping.e:44
% [agent,physobj,time]% 
% HoldsAt(LyingOn(agent,physobj),time) ->
% HoldsAt(Lying(agent),time).
 /*
holds_at(lyingOn(Agent, Physobj), Time) ->
    holds_at(lying(Agent), Time).
*/
axiom(holds_at(lying(Agent),Time),
      [holds_at(lyingOn(Agent,Physobj),Time)]).

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
axiom(holds_at(sitting(Agent),Time),
      [holds_at(sittingOn(Agent,Physobj),Time)]).

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
axiom(equals(Physobj1,Physobj2),
      [ holds_at(lyingOn(Agent,Physobj1),Time),
	holds_at(lyingOn(Agent,Physobj2),Time)
      ]).

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
axiom(equals(Physobj1,Physobj2),
      [ holds_at(sittingOn(Agent,Physobj1),Time),
	holds_at(sittingOn(Agent,Physobj2),Time)
      ]).

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
axiom(initiates(lieOn(Agent,Physobj),
		lyingOn(Agent,Physobj),
		Time),
      [holds_at(standing(Agent),Time)]).

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
terminates(lieOn(Agent,Physobj),
	   standing(Agent),
	   Time).
*/
axiom(terminates(lieOn(Agent,Physobj),
		 standing(Agent),
		 Time),
      []).

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
axiom(initiates(sitOn(Agent,Physobj),
		sittingOn(Agent,Physobj),
		Time),
      [holds_at(standing(Agent),Time)]).

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
terminates(sitOn(Agent,Physobj),
	   standing(Agent),
	   Time).
*/
axiom(terminates(sitOn(Agent,Physobj),
		 standing(Agent),
		 Time),
      []).

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
axiom(initiates(riseFrom(Agent,Physobj),
		standing(Agent),
		Time),
      [ holds_at(sittingOn(Agent,Physobj),Time) ; holds_at(lyingOn(Agent,Physobj),Time)
      ]).

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
axiom(terminates(riseFrom(Agent,Physobj),
		 sittingOn(Agent,Physobj),
		 Time),
      [holds_at(sittingOn(Agent,Physobj),Time)]).

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
axiom(terminates(riseFrom(Agent,Physobj),
		 lyingOn(Agent,Physobj),
		 Time),
      [holds_at(lyingOn(Agent,Physobj),Time)]).

% 
% 
%; dressing
% 
%; agent gets undressed.
% event GetDressed(agent)
 /*
event(getDressed(agent)).
*/
event(getDressed(agent)).

%; agent gets dressed.
% event GetUndressed(agent)
 /*
event(getUndressed(agent)).
*/
event(getUndressed(agent)).

%; agent is dressed.
% fluent Dressed(agent)
 /*
fluent(dressed(agent)).
*/
fluent(dressed(agent)).

% 
%; Effect axioms deal with getting dressed and undressed:
% ecnet/Kidnapping.e:44
% [agent,time] % Initiates(GetDressed(agent),Dressed(agent),time).
 /*
initiates(getDressed(Agent),dressed(Agent),Time).
*/
axiom(initiates(getDressed(Agent),
		dressed(Agent),
		Time),
      []).

% 
% ecnet/Kidnapping.e:44
% [agent,time] % Terminates(GetUndressed(agent),Dressed(agent),time).
 /*
terminates(getUndressed(Agent),dressed(Agent),Time).
*/
axiom(terminates(getUndressed(Agent),
		 dressed(Agent),
		 Time),
      []).

% 
% 
%; End of file.
% load answers/Mueller2003/SpeechAct.e% 
% gun Gun1
 /*
t(gun,gun1).
*/
sort(gun).
t(gun,gun1).

% bullet Bullet1
 /*
t(bullet,bullet1).
*/
sort(bullet).
t(bullet,bullet1).

% HoldsAt(Intact(Gun1),0).
 /*
holds_at(intact(gun1),0).
*/
axiom(holds_at(intact(gun1),0),[]).

% 
% HoldsAt(Intact(Bullet1),0).
 /*
holds_at(intact(bullet1),0).
*/
axiom(holds_at(intact(bullet1),0),[]).

% 
% ecnet/Kidnapping.e:50
% 
% agent Perp1
 /*
t(agent,perp1).
*/
sort(agent).
t(agent,perp1).

% 
% agent HumanTarget1
 /*
t(agent,humanTarget1).
*/
sort(agent).
t(agent,humanTarget1).

% HoldsAt(Calm(HumanTarget1),0).
 /*
holds_at(calm(humanTarget1),0).
*/
axiom(holds_at(calm(humanTarget1),0),[]).

% 
% HoldsAt(Alive(HumanTarget1),0).
 /*
holds_at(alive(humanTarget1),0).
*/
axiom(holds_at(alive(humanTarget1),0),[]).

% 
% ecnet/Kidnapping.e:56
% HoldsAt(Awake(HumanTarget1),0).
 /*
holds_at(awake(humanTarget1),0).
*/
axiom(holds_at(awake(humanTarget1),0),[]).

% 
% HoldsAt(Standing(HumanTarget1),0).
 /*
holds_at(standing(humanTarget1),0).
*/
axiom(holds_at(standing(humanTarget1),0),[]).

% 
% HoldsAt(Sleep2(HumanTarget1),0).
 /*
holds_at(sleep2(humanTarget1),0).
*/
axiom(holds_at(sleep2(humanTarget1),0),[]).

% 
% !HoldsAt(Injured(HumanTarget1),0).
 /*
not(holds_at(injured(humanTarget1),0)).
*/
axiom(initially(neg(injured(humanTarget1))),[]).

% 
% ecnet/Kidnapping.e:60
% [object] % !HoldsAt(Holding(HumanTarget1,object),0).
 /*
not(holds_at(holding(humanTarget1,Object),0)).
*/
axiom(initially(neg(holding(humanTarget1,Object))),[]).

% 
% HoldsAt(At(HumanTarget1,Outside1),0).
 /*
holds_at(at(humanTarget1,outside1),0).
*/
axiom(holds_at(at(humanTarget1,outside1),0),[]).

% 
% ecnet/Kidnapping.e:62
% [physobj1,physobj2] % !HoldsAt(Inside(physobj1,physobj2),0).
 /*
not(holds_at(inside(Physobj1,Physobj2),0)).
*/
axiom(initially(neg(inside(Physobj1,Physobj2))),[]).

% 
% 
%; prune
% sort shoota, shootb, shooto, shooth, shootp
 /*
sort(shoota).
*/
sort(shoota).

 /*
sort(shootb).
*/
sort(shootb).

 /*
sort(shooto).
*/
sort(shooto).

 /*
sort(shooth).
*/
sort(shooth).

 /*
sort(shootp).
*/
sort(shootp).

% event! Shoot(shoota,shootb,shooto)
 /*
event(shoot(shoota,shootb,shooto)).
*/
event(shoot(shoota,shootb,shooto)).

% event! ShootInjure(shoota,shootb,shooth)
 /*
event(shootInjure(shoota,shootb,shooth)).
*/
event(shootInjure(shoota,shootb,shooth)).

% ecnet/Kidnapping.e:68
% event! ShootKill(shoota,shootb,shooth)
 /*
event(shootKill(shoota,shootb,shooth)).
*/
event(shootKill(shoota,shootb,shooth)).

% event! ShootDamage(shoota,shootb,shootp)
 /*
event(shootDamage(shoota,shootb,shootp)).
*/
event(shootDamage(shoota,shootb,shootp)).

% event! ShootDestroy(shoota,shootb,shootp)
 /*
event(shootDestroy(shoota,shootb,shootp)).
*/
event(shootDestroy(shoota,shootb,shootp)).

% shoota! Perp1
 /*
t(shoota,perp1).
*/
sort(shoota).
t(shoota,perp1).

% shootb! Gun1
 /*
t(shootb,gun1).
*/
sort(shootb).
t(shootb,gun1).

% shooto! HumanTarget1
 /*
t(shooto,humanTarget1).
*/
sort(shooto).
t(shooto,humanTarget1).

% ecnet/Kidnapping.e:74
% shooth! HumanTarget1
 /*
t(shooth,humanTarget1).
*/
sort(shooth).
t(shooth,humanTarget1).

% 
%; room-scale topological space
% outside Outside1
 /*
t(outside,outside1).
*/
sort(outside).
t(outside,outside1).

% outside Outside2
 /*
t(outside,outside2).
*/
sort(outside).
t(outside,outside2).

% room Inside1
 /*
t(room,inside1).
*/
sort(room).
t(room,inside1).

% ecnet/Kidnapping.e:80
% door Door1
 /*
t(door,door1).
*/
sort(door).
t(door,door1).

% building Building1
 /*
t(building,building1).
*/
sort(building).
t(building,building1).

% street Street1
 /*
t(street,street1).
*/
sort(street).
t(street,street1).

% Side1(Door1)=Outside2.
 /*
side1(door1) = outside2.
*/
equals(side1(door1),outside2).

% 
% Side2(Door1)=Inside1.
 /*
side2(door1) = inside1.
*/
equals(side2(door1),inside1).

% 
% LookOutOnto(Inside1)=Outside1.
 /*
lookOutOnto(inside1) = outside1.
*/
equals(lookOutOnto(inside1),outside1).

% 
% ecnet/Kidnapping.e:86
% Floor(Inside1)=1.
 /*
floor(inside1) = 1.
*/
equals(floor(inside1),1).

% 
% BuildingOf(Inside1)=Building1.
 /*
buildingOf(inside1) = building1.
*/
equals(buildingOf(inside1),building1).

% 
% Side1(Street1)=Outside1.
 /*
side1(street1) = outside1.
*/
equals(side1(street1),outside1).

% 
% Side2(Street1)=Outside2.
 /*
side2(street1) = outside2.
*/
equals(side2(street1),outside2).

% 
% 
% HoldsAt(Calm(Perp1),0).
 /*
holds_at(calm(perp1),0).
*/
axiom(holds_at(calm(perp1),0),[]).

% 
% ecnet/Kidnapping.e:92
% HoldsAt(Alive(Perp1),0).
 /*
holds_at(alive(perp1),0).
*/
axiom(holds_at(alive(perp1),0),[]).

% 
% HoldsAt(Awake(Perp1),0).
 /*
holds_at(awake(perp1),0).
*/
axiom(holds_at(awake(perp1),0),[]).

% 
% HoldsAt(Standing(Perp1),0).
 /*
holds_at(standing(perp1),0).
*/
axiom(holds_at(standing(perp1),0),[]).

% 
% HoldsAt(Sleep2(Perp1),0).
 /*
holds_at(sleep2(perp1),0).
*/
axiom(holds_at(sleep2(perp1),0),[]).

% 
% !HoldsAt(Injured(Perp1),0).
 /*
not(holds_at(injured(perp1),0)).
*/
axiom(initially(neg(injured(perp1))),[]).

% 
% ecnet/Kidnapping.e:97
% [object] % !HoldsAt(Holding(Perp1,object),0).
 /*
not(holds_at(holding(perp1,Object),0)).
*/
axiom(initially(neg(holding(perp1,Object))),[]).

% 
% HoldsAt(At(Gun1,Outside2),0).
 /*
holds_at(at(gun1,outside2),0).
*/
axiom(holds_at(at(gun1,outside2),0),[]).

% 
% HoldsAt(At(Perp1,Outside2),0).
 /*
holds_at(at(perp1,outside2),0).
*/
axiom(holds_at(at(perp1,outside2),0),[]).

% 
% HoldsAt(At(Bullet1,Outside2),0).
 /*
holds_at(at(bullet1,outside2),0).
*/
axiom(holds_at(at(bullet1,outside2),0),[]).

% 
% HoldsAt(DoorIsOpen(Door1),0).
 /*
holds_at(doorIsOpen(door1),0).
*/
axiom(holds_at(doorIsOpen(door1),0),[]).

% 
% HoldsAt(DoorUnlocked(Door1),0).
 /*
holds_at(doorUnlocked(door1),0).
*/
axiom(holds_at(doorUnlocked(door1),0),[]).

% 
% ecnet/Kidnapping.e:103
% [agent1,agent2] % !HoldsAt(ThreatenedBy(agent1,agent2),0).
 /*
not(holds_at(threatenedBy(Agent1,Agent2),0)).
*/
axiom(initially(neg(threatenedBy(Agent1,Agent2))),[]).

% 
% ecnet/Kidnapping.e:104
% [agent1,agent2] % !HoldsAt(AngryAt(agent1,agent2),0).
 /*
not(holds_at(angryAt(Agent1,Agent2),0)).
*/
axiom(initially(neg(angryAt(Agent1,Agent2))),[]).

% 
% ecnet/Kidnapping.e:105
% [physobj1,physobj2] % !HoldsAt(Inside(physobj1,physobj2),0).
 /*
not(holds_at(inside(Physobj1,Physobj2),0)).
*/
axiom(initially(neg(inside(Physobj1,Physobj2))),[]).

% 
% ecnet/Kidnapping.e:106
% [agent,object] % !HoldsAt(Love(agent,object),0).
 /*
not(holds_at(love(Agent,Object),0)).
*/
axiom(initially(neg(love(Agent,Object))),[]).

% 
% 
%; narrative
% Happens(PickUp(Perp1,Gun1),0).
 /*
happens(pickUp(perp1,gun1),0).
*/
axiom(happens(pickUp(perp1,gun1),0),[]).

% 
% Happens(PickUp(Perp1,Bullet1),1).
 /*
happens(pickUp(perp1,bullet1),1).
*/
axiom(happens(pickUp(perp1,bullet1),1),[]).

% 
% Happens(PutInside(Perp1,Bullet1,Gun1),2).
 /*
happens(putInside(perp1,bullet1,gun1),2).
*/
axiom(happens(putInside(perp1,bullet1,gun1),2),[]).

% 
% ecnet/Kidnapping.e:112
% Happens(WalkStreet21(Perp1,Street1),3).
 /*
happens(walkStreet21(perp1,street1),3).
*/
axiom(happens(walkStreet21(perp1,street1),3),[]).

% 
% Happens(Threaten(Perp1,HumanTarget1,Gun1),4).
 /*
happens(threaten(perp1,humanTarget1,gun1),4).
*/
axiom(happens(threaten(perp1,humanTarget1,gun1),4),[]).

% 
% Happens(Grab(Perp1,HumanTarget1),5).
 /*
happens(grab(perp1,humanTarget1),5).
*/
axiom(happens(grab(perp1,humanTarget1),5),[]).

% 
% Happens(WalkStreet12(Perp1,Street1),6).
 /*
happens(walkStreet12(perp1,street1),6).
*/
axiom(happens(walkStreet12(perp1,street1),6),[]).

% 
% Happens(WalkThroughDoor12(Perp1,Door1),7).
 /*
happens(walkThroughDoor12(perp1,door1),7).
*/
axiom(happens(walkThroughDoor12(perp1,door1),7),[]).

% 
% Happens(LetGoOf(Perp1,HumanTarget1),8).
 /*
happens(letGoOf(perp1,humanTarget1),8).
*/
axiom(happens(letGoOf(perp1,humanTarget1),8),[]).

% 
% ecnet/Kidnapping.e:118
% Happens(Shoot(Perp1,Gun1,HumanTarget1),9).
 /*
happens(shoot(perp1,gun1,humanTarget1),9).
*/
axiom(happens(shoot(perp1,gun1,humanTarget1),9),[]).

% 
% Happens(ShootKill(Perp1,Gun1,HumanTarget1),9).
 /*
happens(shootKill(perp1,gun1,humanTarget1),9).
*/
axiom(happens(shootKill(perp1,gun1,humanTarget1),9),[]).

% 
% 
% range time 0 10
 /*
range(time,0,10).
*/
range(time,0,10).

% range offset 0 3
 /*
range(offset,0,3).
*/
range(offset,0,3).

% range diameter 0 0
 /*
range(diameter,0,0).
*/
range(diameter,0,0).

% ecnet/Kidnapping.e:124
% 
% completion Happens
 /*
completion(happens).
*/
completion(happens).

% 
%; End of file.
