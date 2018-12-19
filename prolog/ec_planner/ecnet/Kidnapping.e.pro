% ecnet/Kidnapping.e:1
% translate: begining  File: ecnet/Kidnapping.e.pro 
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
ignore(skyOf).
ignore(groundOf).
ignore(near).
ignore(walkFromTo).
ignore(runFromTo).
% ignore RollAlong, Diameter, Move, HoldSome
ignore(rollAlong).
ignore(diameter).
ignore(move).
ignore(holdSome).
% ignore On, DoorUnlock, DoorLock
ignore(on).
ignore(doorUnlock).
ignore(doorLock).
% ignore WalkDownStaircase, WalkUpStaircase
ignore(walkDownStaircase).
ignore(walkUpStaircase).
% 
% ecnet/Kidnapping.e:30
% ignore Request, KnowRequest, Order, KnowOrder, SayGoodbye
ignore(request).
ignore(knowRequest).
ignore(order).
ignore(knowOrder).
ignore(sayGoodbye).
% ignore IntentionToWalkIn, InvitedIn
ignore(intentionToWalkIn).
ignore(invitedIn).
% ignore Snowing
ignore(snowing).
% ignore Like, Dislike, LikeSnow
ignore(like).
ignore(dislike).
ignore(likeSnow).
% 
% load foundations/Root.e
load('foundations/Root.e').
% ecnet/Kidnapping.e:36
% load foundations/EC.e
load('foundations/EC.e').
% load answers/Mueller2003/Ontology.e
load('answers/Mueller2003/Ontology.e').
% load answers/Mueller2004c/OTSpaceM.e
load('answers/Mueller2004c/OTSpaceM.e').
% load answers/Mueller2004c/RTSpaceM.e
load('answers/Mueller2004c/RTSpaceM.e').
% load answers/Mueller2003/Feeling.e
load('answers/Mueller2003/Feeling.e').
% load answers/Mueller2004c/Condition.e
load('answers/Mueller2004c/Condition.e').
% ecnet/Kidnapping.e:42
% load answers/Mueller2004c/Gun.e
load('answers/Mueller2004c/Gun.e').
% load answers/Mueller2003/Sleep.e
load('answers/Mueller2003/Sleep.e').
% load answers/Mueller2003/SpeechAct.e
load('answers/Mueller2003/SpeechAct.e').
% 
% gun Gun1
t(gun,gun1).
% bullet Bullet1
t(bullet,bullet1).
% ecnet/Kidnapping.e:48
% HoldsAt(Intact(Gun1),0).
holds_at(intact(gun1),0).
% 
% HoldsAt(Intact(Bullet1),0).
holds_at(intact(bullet1),0).
% 
% 
% agent Perp1
t(agent,perp1).
% 
% agent HumanTarget1
t(agent,humanTarget1).
% ecnet/Kidnapping.e:54
% HoldsAt(Calm(HumanTarget1),0).
holds_at(calm(humanTarget1),0).
% 
% HoldsAt(Alive(HumanTarget1),0).
holds_at(alive(humanTarget1),0).
% 
% HoldsAt(Awake(HumanTarget1),0).
holds_at(awake(humanTarget1),0).
% 
% HoldsAt(Standing(HumanTarget1),0).
holds_at(standing(humanTarget1),0).
% 
% HoldsAt(Sleep2(HumanTarget1),0).
holds_at(sleep2(humanTarget1),0).
% 
% !HoldsAt(Injured(HumanTarget1),0).
not(holds_at(injured(humanTarget1),0)).
% 
% ecnet/Kidnapping.e:60
% [object] % !HoldsAt(Holding(HumanTarget1,object),0).
not(holds_at(holding(humanTarget1,Object),0)).
% 
% HoldsAt(At(HumanTarget1,Outside1),0).
holds_at(at(humanTarget1,outside1),0).
% 
% ecnet/Kidnapping.e:62
% [physobj1,physobj2] % !HoldsAt(Inside(physobj1,physobj2),0).
not(holds_at(inside(Physobj1,Physobj2),0)).
% 
% 
%; prune
% sort shoota, shootb, shooto, shooth, shootp
sort(shoota).
sort(shootb).
sort(shooto).
sort(shooth).
sort(shootp).
% event! Shoot(shoota,shootb,shooto)
event(shoot(shoota,shootb,shooto)).
% event! ShootInjure(shoota,shootb,shooth)
event(shootInjure(shoota,shootb,shooth)).
% ecnet/Kidnapping.e:68
% event! ShootKill(shoota,shootb,shooth)
event(shootKill(shoota,shootb,shooth)).
% event! ShootDamage(shoota,shootb,shootp)
event(shootDamage(shoota,shootb,shootp)).
% event! ShootDestroy(shoota,shootb,shootp)
event(shootDestroy(shoota,shootb,shootp)).
% shoota! Perp1
t(shoota,perp1).
% shootb! Gun1
t(shootb,gun1).
% shooto! HumanTarget1
t(shooto,humanTarget1).
% ecnet/Kidnapping.e:74
% shooth! HumanTarget1
t(shooth,humanTarget1).
% 
%; room-scale topological space
% outside Outside1
t(outside,outside1).
% outside Outside2
t(outside,outside2).
% room Inside1
t(room,inside1).
% ecnet/Kidnapping.e:80
% door Door1
t(door,door1).
% building Building1
t(building,building1).
% street Street1
t(street,street1).
% Side1(Door1)=Outside2.
side1(door1) = outside2.
% 
% Side2(Door1)=Inside1.
side2(door1) = inside1.
% 
% LookOutOnto(Inside1)=Outside1.
lookOutOnto(inside1) = outside1.
% 
% ecnet/Kidnapping.e:86
% Floor(Inside1)=1.
floor(inside1) = 1.
% 
% BuildingOf(Inside1)=Building1.
buildingOf(inside1) = building1.
% 
% Side1(Street1)=Outside1.
side1(street1) = outside1.
% 
% Side2(Street1)=Outside2.
side2(street1) = outside2.
% 
% 
% HoldsAt(Calm(Perp1),0).
holds_at(calm(perp1),0).
% 
% ecnet/Kidnapping.e:92
% HoldsAt(Alive(Perp1),0).
holds_at(alive(perp1),0).
% 
% HoldsAt(Awake(Perp1),0).
holds_at(awake(perp1),0).
% 
% HoldsAt(Standing(Perp1),0).
holds_at(standing(perp1),0).
% 
% HoldsAt(Sleep2(Perp1),0).
holds_at(sleep2(perp1),0).
% 
% !HoldsAt(Injured(Perp1),0).
not(holds_at(injured(perp1),0)).
% 
% ecnet/Kidnapping.e:97
% [object] % !HoldsAt(Holding(Perp1,object),0).
not(holds_at(holding(perp1,Object),0)).
% 
% HoldsAt(At(Gun1,Outside2),0).
holds_at(at(gun1,outside2),0).
% 
% HoldsAt(At(Perp1,Outside2),0).
holds_at(at(perp1,outside2),0).
% 
% HoldsAt(At(Bullet1,Outside2),0).
holds_at(at(bullet1,outside2),0).
% 
% HoldsAt(DoorIsOpen(Door1),0).
holds_at(doorIsOpen(door1),0).
% 
% HoldsAt(DoorUnlocked(Door1),0).
holds_at(doorUnlocked(door1),0).
% 
% ecnet/Kidnapping.e:103
% [agent1,agent2] % !HoldsAt(ThreatenedBy(agent1,agent2),0).
not(holds_at(threatenedBy(Agent1,Agent2),0)).
% 
% ecnet/Kidnapping.e:104
% [agent1,agent2] % !HoldsAt(AngryAt(agent1,agent2),0).
not(holds_at(angryAt(Agent1,Agent2),0)).
% 
% ecnet/Kidnapping.e:105
% [physobj1,physobj2] % !HoldsAt(Inside(physobj1,physobj2),0).
not(holds_at(inside(Physobj1,Physobj2),0)).
% 
% ecnet/Kidnapping.e:106
% [agent,object] % !HoldsAt(Love(agent,object),0).
not(holds_at(love(Agent,Object),0)).
% 
% 
%; narrative
% Happens(PickUp(Perp1,Gun1),0).
happens(pickUp(perp1,gun1),0).
% 
% Happens(PickUp(Perp1,Bullet1),1).
happens(pickUp(perp1,bullet1),1).
% 
% Happens(PutInside(Perp1,Bullet1,Gun1),2).
happens(putInside(perp1,bullet1,gun1),2).
% 
% ecnet/Kidnapping.e:112
% Happens(WalkStreet21(Perp1,Street1),3).
happens(walkStreet21(perp1,street1),3).
% 
% Happens(Threaten(Perp1,HumanTarget1,Gun1),4).
happens(threaten(perp1,humanTarget1,gun1),4).
% 
% Happens(Grab(Perp1,HumanTarget1),5).
happens(grab(perp1,humanTarget1),5).
% 
% Happens(WalkStreet12(Perp1,Street1),6).
happens(walkStreet12(perp1,street1),6).
% 
% Happens(WalkThroughDoor12(Perp1,Door1),7).
happens(walkThroughDoor12(perp1,door1),7).
% 
% Happens(LetGoOf(Perp1,HumanTarget1),8).
happens(letGoOf(perp1,humanTarget1),8).
% 
% ecnet/Kidnapping.e:118
% Happens(Shoot(Perp1,Gun1,HumanTarget1),9).
happens(shoot(perp1,gun1,humanTarget1),9).
% 
% Happens(ShootKill(Perp1,Gun1,HumanTarget1),9).
happens(shootKill(perp1,gun1,humanTarget1),9).
% 
% 
% range time 0 10
range(time,0,10).
% range offset 0 3
range(offset,0,3).
% range diameter 0 0
range(diameter,0,0).
% ecnet/Kidnapping.e:124
% 
% completion Happens
completion(happens).
% 
%; End of file.
% ecnet/Kidnapping.e:128
% translate: ending  File: ecnet/Kidnapping.e.pro 
