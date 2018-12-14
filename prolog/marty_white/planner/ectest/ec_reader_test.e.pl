%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: foundations/Root.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

sort(boolean).
sort(integer).
reified_sort(predicate).
reified_sort(function).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: foundations/EC.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%; Event Calculus (EC)
%;
%; @incollection{MillerShanahan:2002,
%;   author = "Rob Miller and Murray Shanahan",
%;   year = "2002",
%;   title = "Some alternative formulations of the event calculus",
%;   editor = "Antonis C. Kakas and Fariba Sadri",
%;   booktitle = "Computational Logic: Logic Programming and Beyond: Essays in Honour of \uppercase{R}obert \uppercase{A}. \uppercase{K}owalski, Part \uppercase{II}",
%;   series = "Lecture Notes in Computer Science",
%;   volume = "2408",
%;   pages = "452--490",
%;   address = "Berlin",
%;   publisher = "Springer",
%; }
%;

% gleaned(subsort(time,integer)).

subsort(time,integer).
% gleaned(subsort(offset,integer)).

subsort(offset,integer).
reified_sort(fluent).
reified_sort(event).
predicate(happens(event,time)).
predicate(holds_at(fluent,time)).
predicate(releases(fluent,time)).
predicate(initiates(event,fluent,time)).
predicate(terminates(event,fluent,time)).
predicate(releases(event,fluent,time)).
predicate(trajectory(fluent,time,fluent,offset)).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: foundations/DEC.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%; Discrete Event Calculus (DEC)
%;
%; @article{Mueller:2004a,
%;   author = "Erik T. Mueller",
%;   year = "2004",
%;   title = "Event calculus reasoning through satisfiability",
%;   journal = "Journal of Logic and Computation",
%;   volume = "14",
%;   number = "5",
%;   pages = "703--730",
%; }
%;

% gleaned(subsort(time,integer)).

subsort(time,integer).
% gleaned(subsort(offset,integer)).

subsort(offset,integer).
reified_sort(fluent).
reified_sort(event).
predicate(happens(event,time)).
predicate(holds_at(fluent,time)).
predicate(releasedAt(fluent,time)).
predicate(initiates(event,fluent,time)).
predicate(terminates(event,fluent,time)).
predicate(releases(event,fluent,time)).
exists([Event],(holds_at(Fluent,Time),neg(releasedAt(Fluent,Time+1)),neg((happens(Event,Time),terminates(Event,Fluent,Time)))->holds_at(Fluent,Time+1))).
exists([Event],(neg(holds_at(Fluent,Time)),neg(releasedAt(Fluent,Time+1)),neg((happens(Event,Time),initiates(Event,Fluent,Time)))->neg(holds_at(Fluent,Time+1)))).
exists([Event],(neg(releasedAt(Fluent,Time)),neg((happens(Event,Time),releases(Event,Fluent,Time)))->neg(releasedAt(Fluent,Time+1)))).
exists([Event],(releasedAt(Fluent,Time),neg((happens(Event,Time),(initiates(Event,Fluent,Time);terminates(Event,Fluent,Time))))->releasedAt(Fluent,Time+1))).
happens(Event,Time),initiates(Event,Fluent,Time)->holds_at(Fluent,Time+1),neg(releasedAt(Fluent,Time+1)).
happens(Event,Time),terminates(Event,Fluent,Time)->neg(holds_at(Fluent,Time+1)),neg(releasedAt(Fluent,Time+1)).
happens(Event,Time),releases(Event,Fluent,Time)->releasedAt(Fluent,Time+1).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: foundations/ECCausal.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%; Causal Constraints
%;
%; @inproceedings{Shanahan:1999a,
%;   author = "Murray Shanahan",
%;   year = "1999",
%;   title = "The ramification problem in the event calculus",
%;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}ixteenth \uppercase{I}nternational \uppercase{J}oint \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
%;   pages = "140--146",
%;   address = "San Mateo, CA",
%;   publisher = "Morgan Kaufmann",
%; }
%;

predicate(started(fluent,time)).
predicate(stopped(fluent,time)).
exists([Event],<->(started(Fluent,Time),(holds_at(Fluent,Time);happens(Event,Time),initiates(Event,Fluent,Time)))).
exists([Event],<->(stopped(Fluent,Time),(neg(holds_at(Fluent,Time));happens(Event,Time),terminates(Event,Fluent,Time)))).
predicate(initiated(Fluent,Time)).
predicate(terminated(Fluent,Time)).
exists([Event],<->(initiated(Fluent,Time),(started(Fluent,Time),neg((happens(Event,Time),terminates(Event,Fluent,Time)))))).
exists([Event],<->(terminated(Fluent,Time),(stopped(Fluent,Time),neg((happens(Event,Time),initiates(Event,Fluent,Time)))))).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: foundations/ECTraj.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%; @incollection{MillerShanahan:2002,
%;   author = "Rob Miller and Murray Shanahan",
%;   year = "2002",
%;   title = "Some alternative formulations of the event calculus",
%;   editor = "Antonis C. Kakas and Fariba Sadri",
%;   booktitle = "Computational Logic: Logic Programming and Beyond: Essays in Honour of \uppercase{R}obert \uppercase{A}. \uppercase{K}owalski, Part \uppercase{II}",
%;   series = "Lecture Notes in Computer Science",
%;   volume = "2408",
%;   pages = "452--490",
%;   address = "Berlin",
%;   publisher = "Springer",
%; }
%;

predicate(clipped(time,fluent,time)).
predicate(declipped(time,fluent,time)).
predicate(trajectory(fluent,time,fluent,offset)).
predicate(antiTrajectory(fluent,time,fluent,offset)).
happens(Event,Time),initiates(Event,Fluent,Time),0<Offset,trajectory(Fluent,Time,Fluent2,Offset),neg(clipped(Time,Fluent,Time+Offset))->holds_at(Fluent2,Time+Offset).
happens(Event,Time),terminates(Event,Fluent,Time),0<Offset,antiTrajectory(Fluent,Time,Fluent2,Offset),neg(declipped(Time,Fluent,Time+Offset))->holds_at(Fluent2,Time+Offset).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: ecnet/Ontology.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

% gleaned(subsort(diameter,integer)).

subsort(diameter,integer).
%; object

sort(object).
% gleaned(subsort(agent,object)).

subsort(agent,object).
% gleaned(subsort(physobj,object)).

subsort(physobj,object).
% gleaned(subsort(bed,physobj)).

subsort(bed,physobj).
% gleaned(subsort(snowflake,physobj)).

subsort(snowflake,physobj).
% gleaned(subsort(sky,physobj)).

subsort(sky,physobj).
% gleaned(subsort(stuff,physobj)).

subsort(stuff,physobj).
% gleaned(subsort(surface,physobj)).

subsort(surface,physobj).
% gleaned(subsort(ground,surface)).

subsort(ground,surface).
% gleaned(subsort(snow,stuff)).

subsort(snow,stuff).
sort(ball).
% gleaned(subsort(food,physobj)).

subsort(food,physobj).
% gleaned(subsort(fruit,food)).

subsort(fruit,food).
% gleaned(subsort(orange,fruit)).

subsort(orange,fruit).
% gleaned(subsort(salad,food)).

subsort(salad,food).
% gleaned(subsort(clothing,physobj)).

subsort(clothing,physobj).
% gleaned(subsort(scarf,clothing)).

subsort(scarf,clothing).
% gleaned(subsort(hat,clothing)).

subsort(hat,clothing).
% gleaned(subsort(vegetablematter,physobj)).

subsort(vegetablematter,physobj).
% gleaned(subsort(coal,vegetablematter)).

subsort(coal,vegetablematter).
% gleaned(subsort(bodypart,physobj)).

subsort(bodypart,physobj).
% gleaned(subsort(hand,bodypart)).

subsort(hand,bodypart).
% gleaned(subsort(papertowels,physobj)).

subsort(papertowels,physobj).
% gleaned(subsort(device,physobj)).

subsort(device,physobj).
% gleaned(subsort(electronicdevice,device)).

subsort(electronicdevice,device).
% gleaned(subsort(lamp,electronicdevice)).

subsort(lamp,electronicdevice).
% gleaned(subsort(cat,physobj)).

subsort(cat,physobj).
% gleaned(subsort(horse,physobj)).

subsort(horse,physobj).
% gleaned(subsort(weapon,physobj)).

subsort(weapon,physobj).
% gleaned(subsort(gun,weapon)).

subsort(gun,weapon).
% gleaned(subsort(bomb,weapon)).

subsort(bomb,weapon).
% gleaned(subsort(bullet,weapon)).

subsort(bullet,weapon).
%; location

sort(location).
sort(col([room,'location,_outside',location])).
%; portal

sort(portal).
sort(col([door,'portal,_staircase',portal])).
% gleaned(subsort(street,portal)).

subsort(street,portal).
% gleaned(subsort(track,portal)).

subsort(track,portal).
sort(building).
% gleaned(subsort(fire,object)).

subsort(fire,object).
% gleaned(subsort(smoke,physobj)).

subsort(smoke,physobj).
% gleaned(subsort(furniture,physobj)).

subsort(furniture,physobj).
% gleaned(subsort(chair,furniture)).

subsort(chair,furniture).
% gleaned(subsort(table,furniture)).

subsort(table,furniture).
% gleaned(subsort(bill,physobj)).

subsort(bill,physobj).
% gleaned(subsort(ticket,physobj)).

subsort(ticket,physobj).
% gleaned(subsort(envelope,physobj)).

subsort(envelope,physobj).
% gleaned(subsort(text,physobj)).

subsort(text,physobj).
% gleaned(subsort(book,text)).

subsort(book,text).
% gleaned(subsort(letter,text)).

subsort(letter,text).
% gleaned(subsort(menu,text)).

subsort(menu,text).
% gleaned(subsort(paper,physobj)).

subsort(paper,physobj).
sort(content).
sort(script).
% gleaned(subsort(container,physobj)).

subsort(container,physobj).
% gleaned(subsort(cigarette,physobj)).

subsort(cigarette,physobj).
% gleaned(subsort(ashtray,physobj)).

subsort(ashtray,physobj).
% gleaned(subsort(umbrella,physobj)).

subsort(umbrella,physobj).
% gleaned(subsort(pen,physobj)).

subsort(pen,physobj).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: ecnet/RTSpace.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%; RTSpace: room-scale topological space
%;
%; We use topological and metric representations of space,
%; at two levels of granularity---room-scale and object-scale.
%; The RTSpace representation deals with topological space at
%; the scale of rooms and outdoor locations.
%; This representation of space consists of locations, which
%; are connected by portals. There are two types of locations:
%; rooms and outside areas (outsides).
%;
%; object is at location.

fluent(at(object,location)).
manualrelease(at).
partOf(Object1,Object2)->releasedAt(at(Object1,Location),Time).
%; A state constraint says that an object
%; is at one location at a time:

holds_at(at(Object,Location1),Time),holds_at(at(Object,Location2),Time)->Location1=Location2.
%; connectivity
%; Side one of portal is location.

function(side1(portal),location).
%; Side two of portal is location.

function(side2(portal),location).
%; The building of room is building.

function(buildingOf(room),building).
%; object is at a location that has portal.

fluent(nearPortal(object,portal)).
noninertial(nearPortal).
%; A state constraint says that an object is near
%; a portal if and only if there is a location such that
%; the object is at the location and one of the sides
%; of the portal is the location:

<->(holds_at(nearPortal(Object,Portal),Time),((side1(Portal,Location);side2(Portal,Location)),holds_at(at(Object,Location),Time))).
%; locking and unlocking doors
%; agent unlocks door.

event(doorUnlock(agent,door)).
%; agent locks door.

event(doorLock(agent,door)).
%; door is unlocked.

fluent(doorUnlocked(door)).
%; A precondition axiom states that
%; for an agent to unlock a door,
%; the agent must be awake,
%; the door must not already be unlocked, and
%; the agent must be near the door:

happens(doorUnlock(Agent,Door),Time)->holds_at(awake(Agent),Time),neg(holds_at(doorUnlocked(Door),Time)),holds_at(nearPortal(Agent,Door),Time).
%; An effect axiom states that
%; if an agent unlocks a door,
%; the door will be unlocked:

initiates(doorUnlock(Agent,Door),doorUnlocked(Door),Time).
%; A precondition axiom states that
%; for an agent to lock a door,
%; the agent must be awake,
%; the door must be unlocked, and
%; the agent must be near the door:

happens(doorLock(Agent,Door),Time)->holds_at(awake(Agent),Time),holds_at(doorUnlocked(Door),Time),holds_at(nearPortal(Agent,Door),Time).
%; An effect axiom states that
%; if an agent locks a door,
%; the door will no longer be unlocked.

terminates(doorLock(Agent,Door),doorUnlocked(Door),Time).
%; A state constraint says that if a door is open,
%; it is unlocked:

holds_at(doorIsOpen(Door),Time)->holds_at(doorUnlocked(Door),Time).
%; opening and closing doors
%; agent opens door.

event(doorOpen(agent,door)).
%; agent closes door.

event(doorClose(agent,door)).
%; door is open.

fluent(doorIsOpen(door)).
%; A precondition axiom states that
%; for an agent to open a door,
%; the agent must be awake,
%; the door must not already be open,
%; the door must be unlocked, and
%; the agent must be near the door:

happens(doorOpen(Agent,Door),Time)->holds_at(awake(Agent),Time),neg(holds_at(doorIsOpen(Door),Time)),holds_at(doorUnlocked(Door),Time),holds_at(nearPortal(Agent,Door),Time).
%; An effect axiom states that
%; if an agent opens a door,
%; the door will be open:

initiates(doorOpen(Agent,Door),doorIsOpen(Door),Time).
%; A precondition axiom states that
%; for an agent to close a door,
%; the agent must be awake,
%; the door must be open,
%; the door must be unlocked, and
%; the agent must be near the door:

happens(doorClose(Agent,Door),Time)->holds_at(awake(Agent),Time),holds_at(doorIsOpen(Door),Time),holds_at(doorUnlocked(Door),Time),holds_at(nearPortal(Agent,Door),Time).
%; An effect axiom states that
%; if an agent closes a door,
%; the door will no longer be open:

terminates(doorClose(Agent,Door),doorIsOpen(Door),Time).
%; passing through doors
%; agent walks through side one of door.

event(walkThroughDoor12(agent,door)).
%; agent walks through side two of door.

event(walkThroughDoor21(agent,door)).
%; Precondition axioms state that
%; for an agent to walk through a side of a door,
%; the agent must be awake and standing,
%; the door must be open, and
%; the agent must be at the side of the door that
%; the agent walks through:

happens(walkThroughDoor12(Agent,Door),Time)->holds_at(awake(Agent),Time),holds_at(standing(Agent),Time),holds_at(doorIsOpen(Door),Time),holds_at(at(Agent,side1(Door)),Time).
happens(walkThroughDoor21(Agent,Door),Time)->holds_at(awake(Agent),Time),holds_at(standing(Agent),Time),holds_at(doorIsOpen(Door),Time),holds_at(at(Agent,side2(Door)),Time).
%; Effect axioms state that
%; if an agent walks through one side of a door,
%; the agent will be at the other side of the door:

side2(Door,Location)->initiates(walkThroughDoor12(Agent,Door),at(Agent,Location),Time).
side1(Door,Location)->initiates(walkThroughDoor21(Agent,Door),at(Agent,Location),Time).
side1(Door,Location)->terminates(walkThroughDoor12(Agent,Door),at(Agent,Location),Time).
side2(Door,Location)->terminates(walkThroughDoor21(Agent,Door),at(Agent,Location),Time).
%; walking from one end of a street to another
%; agent walks from the first end of street to the second end.

event(walkStreet12(agent,street)).
%; agent walks from the second end of street to the first end.

event(walkStreet21(agent,street)).
%; Precondition axioms state that
%; for an agent to walk from one end of a street to another,
%; the agent must be awake,
%; the agent must be standing, and
%; the agent must be at the first end of the street:

happens(walkStreet12(Agent,Street),Time)->holds_at(awake(Agent),Time),holds_at(standing(Agent),Time),holds_at(at(Agent,side1(Street)),Time).
happens(walkStreet21(Agent,Street),Time)->holds_at(awake(Agent),Time),holds_at(standing(Agent),Time),holds_at(at(Agent,side2(Street)),Time).
%; Effect axioms state that
%; if an agent walks from one end of a street to another,
%; the agent will be at the other end of the street:

side2(Street,Location)->initiates(walkStreet12(Agent,Street),at(Agent,Location),Time).
side1(Street,Location)->initiates(walkStreet21(Agent,Street),at(Agent,Location),Time).
side1(Street,Location)->terminates(walkStreet12(Agent,Street),at(Agent,Location),Time).
side2(Street,Location)->terminates(walkStreet21(Agent,Street),at(Agent,Location),Time).
%; floors
%; The floor of room is integer.

function(floor(room),integer).
%; walking up and down staircases
%; agent walks down staircase.

event(walkDownStaircase(agent,staircase)).
%; agent walks up staircase.

event(walkUpStaircase(agent,staircase)).
%; Precondition axioms state that
%; for an agent to walk down (up) a staircase,
%; the agent must be awake, standing, and
%; at the top (bottom) of the staircase:

happens(walkDownStaircase(Agent,Staircase),Time)->holds_at(awake(Agent),Time),holds_at(standing(Agent),Time),holds_at(at(Agent,side2(Staircase)),Time).
happens(walkUpStaircase(Agent,Staircase),Time)->holds_at(awake(Agent),Time),holds_at(standing(Agent),Time),holds_at(at(Agent,side1(Staircase)),Time).
%; Effect axioms state that
%; if an agent walks down (up) a staircase,
%; the agent will be at the bottom (top) of the staircase:

side1(Staircase,Room)->initiates(walkDownStaircase(Agent,Staircase),at(Agent,Room),Time).
side2(Staircase,Room)->terminates(walkDownStaircase(Agent,Staircase),at(Agent,Room),Time).
side2(Staircase,Room)->initiates(walkUpStaircase(Agent,Staircase),at(Agent,Room),Time).
side1(Staircase,Room)->terminates(walkUpStaircase(Agent,Staircase),at(Agent,Room),Time).
%; A state constraint says that if an agent is outside,
%; the agent is dressed.

holds_at(at(Agent,Outside),Time)->holds_at(dressed(Agent),Time).
%; room looks out onto outside.

function(lookOutOnto(room),outside).
%; location1 is adjacent to location2.

predicate(adjacent(location,location)).
%; A state constraint says that
%; two locations are adjacent if and only if
%; they have a portal in common:

<->(adjacent(Location1,Location2),(side1(Portal,Location1),side2(Portal,Location2)));side2(Portal,Location1),side1(Portal,Location2).
%; The ground of outside is ground.

function(groundOf(outside),ground).
%; The sky of outside is sky.

function(skyOf(outside),sky).
%; State constraints fix the location of ground and sky:

groundOf(Outside,Ground)->holds_at(at(Ground,Outside),Time).
skyOf(Outside,Sky)->holds_at(at(Sky,Outside),Time).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: ecnet/OTSpace.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%; OTSpace: object-scale topological space
%;
%; The OTSpace representation deals with topological space at
%; the scale of objects such as agents (humans and animals)
%; and physical objects.
%;
%; PartOf
%; physobj is a part of object.

predicate(partOf(physobj,object)).
%; A state constraint says that if a physical object
%; is part of an object, the location of the
%; physical object is the same as the location of the object:

partOf(Physobj,Object),holds_at(at(Object,Location),Time)->holds_at(at(Physobj,Location),Time).
%; rolling a snowball bigger
%; agent rolls stuff1 along stuff2.

event(rollAlong(agent,stuff,stuff)).
%; The diameter of ball is diameter.

fluent(diameter(ball,diameter)).
%; A state constraint says that a ball has a unique diameter:

holds_at(diameter(Ball,Diameter1),Time),holds_at(diameter(Ball,Diameter2),Time)->Diameter1=Diameter2.
%; Effect axiom state that if an agent rolls some snow along
%; some other snow, the diameter of the first snow will increase:

holds_at(diameter(Snow1,Diameter1),Time),Diameter2=Diameter1+1->initiates(rollAlong(Agent,Snow1,Snow2),diameter(Snow1,Diameter2),Time).
holds_at(diameter(Snow1,Diameter1),Time)->terminates(rollAlong(Agent,Snow1,Snow2),diameter(Snow1,Diameter1),Time).
%; A precondition axiom states that
%; for an agent to roll some snow along some other snow,
%; there must be a location such that
%; the agent is at the location,
%; the first snow is at the location, and
%; the second snow is at the location:
%;[agent,snow1,snow2,time]
%;Happens(RollAlong(agent,snow1,snow2),time) ->
%;{location}
%;HoldsAt(At(agent,location),time) &
%;HoldsAt(At(snow1,location),time) &
%;HoldsAt(At(snow2,location),time).
%; motion
%; object moves (in place).

event(move(object)).
%; Holding
%; agent is holding physobj.

fluent(holding(agent,physobj)).
%; agent holds or picks up physobj.

event(hold(agent,physobj)).
%; agent picks up some stuff1 from stuff2.

event(holdSome(agent,stuff,stuff)).
%; agent releases or lets go of physobj.

event(letGoOf(agent,physobj)).
%; An effect axiom states that if an agent holds
%; a physical object, the agent will be holding the
%; physical object:

initiates(hold(Agent,Physobj),holding(Agent,Physobj),Time).
%; A precondition axiom states that
%; for an agent to hold a physical object,
%; there must be a location such that
%; the agent is at the location and
%; the physical object is at the location:
%;[agent,physobj,time]
%;Happens(Hold(agent,physobj),time) ->
%;{location}
%;  HoldsAt(At(agent,location),time) &
%;  HoldsAt(At(physobj,location),time).
%; An effect axiom states that if an agent
%; lets go of a physical object, the agent is no longer holding
%; the physical object:

terminates(letGoOf(Agent,Physobj),holding(Agent,Physobj),Time).
%; A precondition axiom states that
%; for an agent to let go of a physical object,
%; the agent must be holding the physical object:

happens(letGoOf(Agent,Physobj),Time)->holds_at(holding(Agent,Physobj),Time).
%; A releases axiom states that if an agent holds
%; a physical object,
%; the physical object's location will be released
%; from inertia:

releases(hold(Agent,Physobj),at(Physobj,Location),Time).
%; A state constraint says that if an agent is holding
%; a physical object and the agent is at a location,
%; the physical object is also at the location:

holds_at(holding(Agent,Physobj),Time),holds_at(at(Agent,Location),Time)->holds_at(at(Physobj,Location),Time).
%; A releases axiom states that if an agent holds
%; a physical object,
%; the locations of the parts of the physical object
%; will be released from inertia:

partOf(Physobj1,Physobj2)->releases(hold(Agent,Physobj2),at(Physobj1,Location),Time).
%; Further, if an agent holds a physical object,
%; the locations of the physical objects of which
%; the physical object is a part
%; will be released from inertia:

partOf(Physobj1,Physobj2)->releases(hold(Agent,Physobj1),at(Physobj2,Location),Time).
%;[agent,physobj,location1,location2,time]
%;(!{object} PartOf(physobj,object)) &
%;HoldsAt(At(agent,location1),time) &
%;location1 != location2 ->
%;Terminates(LetGoOf(agent,physobj),At(physobj,location2),time).

exists([Object],(neg(partOf(Physobj,Object)),holds_at(at(Agent,Location),Time)->initiates(letGoOf(Agent,Physobj),at(Physobj,Location),Time))).
%;[agent,physobj1,physobj2,location1,location2,time]
%;PartOf(physobj1,physobj2) &
%;(!{object} PartOf(physobj2,object)) &
%;HoldsAt(At(agent,location1),time) &
%;location1 != location2 ->
%;Terminates(LetGoOf(agent,physobj1),At(physobj2,location2),time).

exists([Object],(partOf(Physobj1,Physobj2),neg(partOf(Physobj2,Object)),holds_at(at(Agent,Location),Time)->initiates(letGoOf(Agent,Physobj1),at(Physobj2,Location),Time))).
%; An effect axiom states that if an agent is at a location
%; and lets go of a physical object, the physical object
%; will be at the location:

holds_at(at(Agent,Location),Time)->initiates(letGoOf(Agent,Physobj),at(Physobj,Location),Time).
%; An effect axiom states that if an agent picks up
%; some stuff out of some other stuff, the agent will
%; be holding the first stuff:

initiates(holdSome(Agent,Stuff1,Stuff2),holding(Agent,Stuff1),Time).
%; A precondition axiom states that
%; for an agent to pick up some stuff out of some other stuff,
%; the first stuff must be a part of the second stuff and
%; there must be a location such that the agent is at the location,
%; the first stuff is at the location, and the second stuff is
%; at the location:

happens(holdSome(Agent,Stuff1,Stuff2),Time)->partOf(Stuff1,Stuff2),holds_at(at(Agent,Location),Time),holds_at(at(Stuff1,Location),Time),holds_at(at(Stuff2,Location),Time).
%; A releases axiom states that if an agent picks up some
%; stuff out of some other stuff,
%; the first stuff's location will be released
%; from inertia:

releases(holdSome(Agent,Stuff1,Stuff2),at(Stuff1,Location),Time).
%; Inside
%; physobj1 is inside physobj2.

fluent(inside(physobj,physobj)).
%; agent puts physobj1 inside physobj2.

event(putInside(agent,physobj,physobj)).
%; agent takes physobj1 out of physobj2.

event(takeOutOf(agent,physobj,physobj)).
%; A state constraint says that a physical object cannot
%; be inside itself:

holds_at(inside(Physobj1,Physobj2),Time)->Physobj1\=Physobj2.
holds_at(inside(Physobj1,Physobj2),Time)->Physobj1\=Physobj2.
%; A state constraint says that if a physical object is
%; inside another physical object, the second physical object
%; is not inside the first physical object:

holds_at(inside(Physobj1,Physobj2),Time)->neg(holds_at(inside(Physobj2,Physobj1),Time)).
%; An effect axiom states that if an agent puts a physical
%; object inside another physical object, the first
%; physical object will be inside the second physical object:

initiates(putInside(Agent,Physobj1,Physobj2),inside(Physobj1,Physobj2),Time).
%; An effect axiom states that if an agent puts a physical
%; object inside another physical object, the agent will
%; no longer be holding the first physical object:

terminates(putInside(Agent,Physobj1,Physobj2),holding(Agent,Physobj1),Time).
%; A precondition axiom states that
%; for an agent to put a physical object inside another
%; physical object,
%; the agent must be holding the first physical object
%; and there must be a location such that
%; the agent is at the location and
%; the second physical object is at the location:
%;[agent,physobj1,physobj2,time]
%;Happens(PutInside(agent,physobj1,physobj2),time) ->
%;HoldsAt(Holding(agent,physobj1),time) &
%;{location}
%; HoldsAt(At(agent,location),time) &
%; HoldsAt(At(physobj2,location),time).
%; An effect axiom states that
%; if an agent takes a physical object out of another
%; physical object, the first physical object
%; will no longer be inside the second physical object:

terminates(takeOutOf(Agent,Physobj1,Physobj2),inside(Physobj1,Physobj2),Time).
%; A precondition axiom states that
%; for an agent to take a physical object out of another
%; physical object,
%; the first physical object must be inside the second physical object
%; and there must be a location such that
%; the agent is at the location,
%; the first physical object is at the location, and
%; the second physical object is at the location:

happens(takeOutOf(Agent,Physobj1,Physobj2),Time)->holds_at(inside(Physobj1,Physobj2),Time),holds_at(at(Agent,Location),Time),holds_at(at(Physobj1,Location),Time),holds_at(at(Physobj2,Location),Time).
%; A releases axiom states that if an agent puts a physical
%; object inside another physical object,
%; the first physical object's location will be released
%; from inertia:

releases(putInside(Agent,Physobj1,Physobj2),at(Physobj1,Location),Time).
%; A state constraint says that if a physical object is inside
%; another physical object and the second physical object is
%; at a location, the first physical object is also at the location:

holds_at(inside(Physobj1,Physobj2),Time),holds_at(at(Physobj2,Location),Time)->holds_at(at(Physobj1,Location),Time).
%; An effect axiom states that if an agent takes a physical
%; object out of another physical object,
%; the agent will be holding the first physical object:

initiates(takeOutOf(Agent,Physobj1,Physobj2),holding(Agent,Physobj1),Time).
%; On
%; physobj1 is on physobj2.

fluent(on(physobj,physobj)).
%; agent places physobj1 on physobj2.

event(placeOn(agent,physobj,physobj)).
%; agent takes physobj1 off of physobj2.

event(takeOffOf(agent,physobj,physobj)).
%; A state constraint says that a physical object cannot
%; be on itself:

holds_at(on(Physobj1,Physobj2),Time)->Physobj1\=Physobj2.
%; A state constraint says that if a physical object is
%; on another physical object, the second physical object
%; is not on the first physical object:

holds_at(on(Physobj1,Physobj2),Time)->neg(holds_at(on(Physobj2,Physobj1),Time)).
%; An effect axiom states that if an agent places a physical
%; object on another physical object, the first
%; physical object will be on the second physical object:

initiates(placeOn(Agent,Physobj1,Physobj2),on(Physobj1,Physobj2),Time).
%; An effect axiom states that if an agent places a physical
%; object on another physical object, the agent will
%; no longer be holding the first physical object:

terminates(placeOn(Agent,Physobj1,Physobj2),holding(Agent,Physobj1),Time).
%; A precondition axiom states that
%; for an agent to place a physical object on another
%; physical object,
%; the agent must be holding the first physical object
%; and there must be a location such that
%; the agent is at the location and
%; the second physical object is at the location:
%;[agent,physobj1,physobj2,time]
%;Happens(PlaceOn(agent,physobj1,physobj2),time) ->
%;HoldsAt(Holding(agent,physobj1),time) &
%;{location}
%; HoldsAt(At(agent,location),time) &
%; HoldsAt(At(physobj2,location),time).
%; An effect axiom states that
%; if an agent takes a physical object off of another
%; physical object, the first physical object
%; will no longer be on the second physical object:

terminates(takeOffOf(Agent,Physobj1,Physobj2),on(Physobj1,Physobj2),Time).
%; An effect axiom states that if an agent takes a physical
%; object off of another physical object,
%; the agent will be holding the first physical object:

initiates(takeOffOf(Agent,Physobj1,Physobj2),holding(Agent,Physobj1),Time).
%; A precondition axiom states that
%; for an agent to take a physical object off of another
%; physical object,
%; the first physical object must be on the second physical object
%; and there must be a location such that
%; the agent is at the location and
%; the first physical object is at the location:
%; the second physical object is at the location:

happens(takeOffOf(Agent,Physobj1,Physobj2),Time)->holds_at(on(Physobj1,Physobj2),Time),holds_at(at(Agent,Location),Time),holds_at(at(Physobj1,Location),Time),holds_at(at(Physobj2,Location),Time).
%; A releases axiom states that if an agent places a physical
%; object on another physical object,
%; the first physical object's location will be released
%; from inertia:

releases(placeOn(Agent,Physobj1,Physobj2),at(Physobj1,Location),Time).
%; A state constraint says that if a physical object is on
%; another physical object and the second physical object is
%; at a location, the first physical object is also at the location:

holds_at(on(Physobj1,Physobj2),Time),holds_at(at(Physobj2,Location),Time)->holds_at(at(Physobj1,Location),Time).
fluent(near(agent,object)).
event(walkFromTo(agent,object,object)).
event(walkFrom(agent,object)).
event(runFromTo(agent,object,object)).
initiates(walkFromTo(Agent,Object1,Object2),near(Agent,Object2),Time).
terminates(walkFromTo(Agent,Object1,Object2),near(Agent,Object1),Time).
happens(walkFromTo(Agent,Object1,Object2),Time)->holds_at(at(Agent,Location),Time),holds_at(at(Object1,Location),Time),holds_at(at(Object2,Location),Time).
initiates(runFromTo(Agent,Object1,Object2),near(Agent,Object2),Time).
terminates(runFromTo(Agent,Object1,Object2),near(Agent,Object1),Time).
happens(runFromTo(Agent,Object1,Object2),Time)->holds_at(at(Agent,Location),Time),holds_at(at(Object1,Location),Time),holds_at(at(Object2,Location),Time).
terminates(walkFrom(Agent,Object),near(Agent,Object),Time).
holds_at(near(Agent,Object),Time),holds_at(at(Agent,Location),Time),holds_at(at(Object,Location),Time),side1(Door,Location),happens(walkThroughDoor12(Agent,Door),Time)->happens(walkFrom(Agent,Object),Time).
holds_at(near(Agent,Object),Time),holds_at(at(Agent,Location),Time),holds_at(at(Object,Location),Time),side2(Door,Location),happens(walkThroughDoor21(Agent,Door),Time)->happens(walkFrom(Agent,Object),Time).
holds_at(near(Agent,Object),Time),holds_at(at(Agent,Room),Time),holds_at(at(Object,Room),Time),side1(Staircase,Room),happens(walkUpStaircase(Agent,Staircase),Time)->happens(walkFrom(Agent,Object),Time).
holds_at(near(Agent,Object),Time),holds_at(at(Agent,Room),Time),holds_at(at(Object,Room),Time),side2(Staircase,Room),happens(walkDownStaircase(Agent,Staircase),Time)->happens(walkFrom(Agent,Object),Time).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: ecnet/OMSpace.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%; OMSpace: object-scale metric space
%;
%; The OMSpace representation deals with metric space at
%; the scale of objects.
%;
%; @article{Morgenstern:2001,
%;   author = "Morgenstern, Leora",
%;   year = "2001",
%;   title = "Mid-sized axiomatizations of commonsense problems: A case study in egg cracking",
%;   journal = "Studia Logica",
%;   volume = "67",
%;   pages = "333--384",
%; }
%;
%; @article{Shanahan:2003,
%;   author = "Shanahan, Murray",
%;   year = "2004",
%;   title = "An attempt to formalise a non-trivial benchmark problem in common sense reasoning",
%;   journal = "Artificial Intelligence",
%;   volume = "153",
%;   pages = "141--165",
%; }
%;

% gleaned(subsort(height,integer)).

subsort(height,integer).
% gleaned(subsort(distance,integer)).

subsort(distance,integer).
%; Height
%; The height of object is height.

fluent(height(object,height)).
%; State constraint represent the fact that each
%; object has a unique height:

holds_at(height(Object,Height1),Time),holds_at(height(Object,Height2),Time)->Height1=Height2.
holds_at(height(Object,Height),Time).
%; falling
%; physobj1 is falling from physobj2 to physobj3.

fluent(fallingFromTo(physobj,physobj,physobj)).
%; physobj1 starts falling from physobj2 to physobj3.

event(startFallingFromTo(physobj,physobj,physobj)).
%; physobj1 collides with physobj2.

event(collideWith(physobj,physobj)).
%; An effect axiom states that if a first physical object starts
%; falling from a second physical object to a third physical
%; object, the first physical object will be falling from the
%; second physical object to the third physical object:

initiates(startFallingFromTo(Physobj1,Physobj2,Physobj3),fallingFromTo(Physobj1,Physobj2,Physobj3),Time).
%; A precondition axiom states that for
%; a first physical object to start
%; falling from a second physical object to a third physical
%; object,
%; the height of the first physical object and the
%; second physical object must be the same.

happens(startFallingFromTo(Physobj1,Physobj2,Physobj3),Time),holds_at(height(Physobj1,Height1),Time),holds_at(height(Physobj2,Height2),Time)->Height1=Height2.
%; A state constraint says that a physical object
%; cannot fall from itself, cannot fall to itself,
%; and cannot fall from and to the same physical object:

holds_at(fallingFromTo(Physobj1,Physobj2,Physobj3),Time)->Physobj1\=Physobj2,Physobj1\=Physobj3,Physobj2\=Physobj3.
%; A state constraint says that the sky cannot fall:

neg(holds_at(fallingFromTo(Sky,Physobj1,Physobj2),Time)).
%; A releases axiom states that if
%; if a first physical object starts
%; falling from a second physical object to a third physical
%; object, the height of the first physical object
%; will be released from inertia:

releases(startFallingFromTo(Physobj1,Physobj2,Physobj3),height(Physobj1,Height),Time).
%; A trajectory axiom states that
%; if a first physical object starts falling
%; from a second physical object
%; to a third physical object
%; at a time and
%; the first physical object has a height at the time,
%; then the first physical object will have a height
%; equal to the height minus an offset
%; at a time equal to the time plus the offset:

holds_at(height(Physobj1,Height1),Time),Height2=Height1-Offset->trajectory(fallingFromTo(Physobj1,Physobj2,Physobj3),Time,height(Physobj1,Height2),Offset).
%; A trigger axiom states that
%; if a first physical object is falling
%; from a second physical object
%; to a third physical object and
%; the height of the first physical object
%; is the same as the height of the third physical object,
%; the first physical object collides with the
%; third physical object:

holds_at(fallingFromTo(Physobj1,Physobj2,Physobj3),Time),holds_at(height(Physobj1,Height),Time),holds_at(height(Physobj3,Height),Time)->happens(collideWith(Physobj1,Physobj3),Time).
%; An effect axiom states that
%; if a first physical object is falling
%; from a second physical object
%; to a third physical object and
%; the first physical object collides with
%; the third physical object,
%; the first physical object will be on the third physical object:

holds_at(fallingFromTo(Physobj1,Physobj2,Physobj3),Time)->initiates(collideWith(Physobj1,Physobj3),on(Physobj1,Physobj3),Time).
%; An effect axiom states that
%; if a physical object collides with another
%; physical object,
%; the height of the first physical object will
%; be the height of the second physical object:

holds_at(height(Physobj2,Height),Time)->initiates(collideWith(Physobj1,Physobj2),height(Physobj1,Height),Time).
%;[physobj1,physobj2,height1,height2,time]
%;HoldsAt(Height(physobj2,height1),time) &
%;height1 != height2 ->
%;Terminates(CollideWith(physobj1,physobj2),
%;           Height(physobj1,height2),
%;           time).
%; An effect axiom states that
%; if a first physical object is falling
%; from a second physical object
%; to a third physical object and
%; the first physical object collides with
%; the third physical object,
%; the first physical object will no longer be
%; falling from the second physical object to the
%; third physical object:

holds_at(fallingFromTo(Physobj1,Physobj2,Physobj3),Time)->terminates(collideWith(Physobj1,Physobj3),fallingFromTo(Physobj1,Physobj2,Physobj3),Time).
%; flying
%; agent is flying from physobj1 to physobj2.

fluent(flyingFromTo(agent,physobj,physobj)).
%; agent starts flying from physobj1 to physobj2.

event(startFlyingFromTo(agent,physobj,physobj)).
%; agent reaches physobj.

event(reach(agent,physobj)).
%; An effect axiom states that if an agent starts
%; flying from a physical object to another physical object,
%; the agent will be flying from the first physical object
%; to the second physical object:

initiates(startFlyingFromTo(Agent,Physobj1,Physobj2),flyingFromTo(Agent,Physobj1,Physobj2),Time).
%; A precondition axiom states that for
%; an agent to start flying from a physical object to
%; another physical object,
%; the height of the agent and
%; the first physical object must be the same:

happens(startFlyingFromTo(Agent,Physobj1,Physobj2),Time),holds_at(height(Agent,Height1),Time),holds_at(height(Physobj1,Height2),Time)->Height1=Height2.
%; A state constraint says that an agent
%; cannot fly from and to the same physical object:

holds_at(flyingFromTo(Agent,Physobj1,Physobj2),Time)->Physobj1\=Physobj2.
%; A releases axiom states that if an agent
%; starts flying from a physical object to another
%; physical object, the height of the agent will
%; be released from inertia:

releases(startFlyingFromTo(Agent,Physobj1,Physobj2),height(Agent,Height),Time).
%; A trajectory axiom states that
%; if an agent starts flying from
%; from a physical object
%; to another physical object
%; at a time and
%; the agent has a height at the time,
%; then the agent will have a height
%; equal to the height plus an offset
%; at a time equal to the time plus the offset:

holds_at(height(Agent,Height1),Time),Height2=Height1+Offset->trajectory(flyingFromTo(Agent,Physobj1,Physobj2),Time,height(Agent,Height2),Offset).
%; A trigger axiom states that
%; if an agent is flying
%; from a physical object
%; to another physical object and
%; the height of the agent
%; is the same as the height of the second physical object,
%; the agent reaches the second physical object:

holds_at(flyingFromTo(Agent,Physobj1,Physobj2),Time),holds_at(height(Agent,Height),Time),holds_at(height(Physobj2,Height),Time)->happens(reach(Agent,Physobj2),Time).
%; An effect axiom states that
%; if an agent reaches a physical object,
%; the height of the agent will be the
%; height of the physical object:

holds_at(height(Physobj,Height),Time)->initiates(reach(Agent,Physobj),height(Agent,Height),Time).
%;[agent,physobj,height1,height2,time]
%;HoldsAt(Height(physobj,height1),time) &
%;height1!=height2 ->
%;Terminates(Reach(agent,physobj),Height(agent,height2),time).
%; An effect axiom states that
%; if an agent is flying
%; from a physical object
%; to another physical object and
%; the agent reaches the second physical object,
%; the agent will no longer be
%; flying from the first physical object
%; to the second physical object:

holds_at(flyingFromTo(Agent,Physobj1,Physobj2),Time)->terminates(reach(Agent,Physobj2),flyingFromTo(Agent,Physobj1,Physobj2),Time).
%; A releases axiom states that
%; if an agent holds a physical object,
%; the height of the physical object is released from inertia:

releases(hold(Agent,Physobj),height(Physobj,Height),Time).
%;[agent,physobj,height1,height2,time]
%;(!{object} PartOf(physobj,object)) &
%;HoldsAt(Height(physobj,height1),time) &
%;height1 != height2 ->
%;Terminates(LetGoOf(agent,physobj),Height(physobj,height2),time).

exists([Object],(neg(partOf(Physobj,Object)),holds_at(height(Physobj,Height),Time)->initiates(letGoOf(Agent,Physobj),height(Physobj,Height),Time))).
%; A state constraint says that
%; if an agent is holding a physical object and
%; the height of the agent is height,
%; the height of the physical object is height:

holds_at(holding(Agent,Physobj),Time),holds_at(height(Agent,Height),Time)->holds_at(height(Physobj,Height),Time).
%; A state constraint says that if a physical object
%; is part of an object,
%; the height of the physical object
%; is the same as the height of the object:

partOf(Physobj,Object),holds_at(height(Object,Height),Time)->holds_at(height(Physobj,Height),Time).
%;event Catch(agent,physobj)
%;event HitFromTo(agent,physobj,object,object)
%;fluent Distance(physobj,physobj,distance)
%;fluent FlyingAcrossFromTo(physobj,object,object)
%;[agent,physobj1,physobj2,physobj3,time]
%;Initiates(HitFromTo(agent,physobj1,physobj2,physobj3),
%;          FlyingAcrossFromTo(physobj1,physobj2,physobj3),
%;          time).
%;[agent,physobj1,physobj2,physobj3,distance,time]
%;Releases(HitFromTo(agent,physobj1,physobj2,physobj3),
%;         Distance(physobj1,physobj2,distance),
%;         time).
%;[agent,physobj1,physobj2,physobj3,distance,time]
%;Releases(HitFromTo(agent,physobj1,physobj2,physobj3),
%;         Distance(physobj1,physobj3,distance),
%;         time).
%;[physobj1,physobj2,physobj3,offset,time]
%;Trajectory(FlyingAcrossFromTo(physobj1,physobj2,physobj3),time,
%;           Distance(physobj1,physobj2,offset),offset).
%;[physobj1,physobj2,physobj3,distance1,distance2,offset,time]
%;HoldsAt(Distance(physobj2,physobj3,distance1),time) &
%;distance2 = distance1 - time ->
%;Trajectory(FlyingAcrossFromTo(physobj1,physobj2,physobj3),time,
%;           Distance(physobj1,physobj3,distance2),offset).
%;[agent,physobj1,physobj2,physobj3,time]
%;HoldsAt(FlyingAcrossFromTo(physobj1,physobj2,physobj3),time) ->
%;Initiates(Catch(agent,physobj1),
%;          Holding(agent,physobj1),
%;          time).
%;[agent,physobj1,physobj2,physobj3,time]
%;HoldsAt(FlyingAcrossFromTo(physobj1,physobj2,physobj3),time) ->
%;Terminates(Catch(agent,physobj1),
%;           FlyingAcrossFromTo(physobj1,physobj2,physobj3),
%;           time).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: ecnet/GSpace.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%; GSpace: grid space
%;
%; @book{Mueller:1998,
%;   author = "Erik T. Mueller",
%;   year = "1998",
%;   title = "Natural Language Processing with \uppercase{T}hought\uppercase{T}reasure",
%;   address = "New York",
%;   publisher = "Signiform",
%; }
%;

% gleaned(subsort(coord,integer)).

subsort(coord,integer).
sort(grid).
%; object is at (coord1, coord2) in grid.

fluent(gridAt(grid,object,coord,coord)).
%; agent walks from (coord1, coord2)
%; to (coord3, coord4) in grid.

event(gridWalk(grid,agent,coord,coord,coord,coord)).
%; A state constraint says that for a given grid an
%; object is at one cell in that grid at a time:

holds_at(gridAt(Grid,Object,Coord1,Coord2),Time),holds_at(gridAt(Grid,Object,Coord3,Coord4),Time)->Coord1=Coord3,Coord2=Coord4.
%; An effect axiom states that
%; if an agent walks from one cell in a grid to another cell,
%; the agent will be at second cell:

initiates(gridWalk(Grid,Agent,Coord1,Coord2,Coord3,Coord4),gridAt(Grid,Agent,Coord3,Coord4),Time).
%; An effect axiom states that
%; if an agent walks from one cell in a grid to another cell,
%; the agent will no longer be at the first cell:

terminates(gridWalk(Grid,Agent,Coord1,Coord2,Coord3,Coord4),gridAt(Grid,Agent,Coord1,Coord2),Time).
%; A precondition axiom states that for an agent to walk
%; from one cell in a grid to another cell, the agent
%; must be at the first cell, the second cell must not
%; be occupied, and the first cell must be adjacent to
%; the second cell:

exists([Object],(happens(gridWalk(Grid,Agent,Coord1,Coord2,Coord3,Coord4),Time)->holds_at(gridAt(Grid,Agent,Coord1,Coord2),Time),neg(holds_at(gridAt(Grid,Object,Coord3,Coord4),Time)),(Coord1=Coord3;Coord1=Coord3+1;Coord1=Coord3-1),(Coord2=Coord4;Coord2=Coord4+1;Coord2=Coord4-1))).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: ecnet/PolySpace.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%; @phdthesis{Cassimatis:2002,
%;   author = "Nicholas L. Cassimatis",
%;   year = "2002",
%;   title = "Polyscheme: A Cognitive Architecture for Integrating Multiple Representation and Inference Schemes",
%;   address = "Cambridge, MA",
%;   school = "Program in Media Arts and Sciences, School of Architecture and Planning, Massachusetts Institute of Technology",
%; }
%;
%; sorts

sort(object).
% gleaned(subsort(xcoord,integer)).

subsort(xcoord,integer).
% gleaned(subsort(ycoord,integer)).

subsort(ycoord,integer).
sort(grid).
sort(shape).
sort(color).
%; constants

shape(round).
shape(square).
color(red).
color(green).
%; predicates, fluents, and events

predicate(equal(object,object)).
predicate(shape(object,shape)).
predicate(color(object,color)).
fluent(location(grid,object,xcoord,ycoord)).
event(move(grid,object,xcoord,ycoord,xcoord,ycoord)).
%; axioms

equal(Object1,Object2)->equal(Object2,Object1).
%; objects have unique shape

shape(Object,Shape1),shape(Object,Shape2)->Shape1=Shape2.
%; objects have unique color

color(Object,Color1),color(Object,Color2)->Color1=Color2.
%; if objects are the same, they have the same shape

exists([Shape],(equal(Object1,Object2)->shape(Object1,Shape),shape(Object2,Shape))).
%; if objects are the same, they have the same color

exists([Color],(equal(Object1,Object2)->color(Object1,Color),color(Object2,Color))).
%; if objects are the same, they have the same location

equal(Object1,Object2)->holds_at(location(Grid,Object1,Xcoord1,Ycoord1),Time),holds_at(location(Grid,Object2,Xcoord2,Ycoord2),Time)->Xcoord1=Xcoord2,Ycoord1=Ycoord2.
%; object in one location at a time

holds_at(location(Grid,Object,Xcoord1,Ycoord1),Time),holds_at(location(Grid,Object,Xcoord2,Ycoord2),Time)->Xcoord1=Xcoord2,Ycoord1=Ycoord2.
%; objects have locations

holds_at(location(Grid,Object,xcoord,ycoord),Time).
%; different objects are not at same location

holds_at(location(Grid,Object1,Xcoord1,Ycoord1),Time),holds_at(location(Grid,Object2,Xcoord1,Ycoord1),Time)->equal(Object1,Object2).
%; moving to a location causes an object to be at that location

initiates(move(Grid,Object,Xcoord1,Ycoord1,Xcoord2,Ycoord2),location(Grid,Object,Xcoord2,Ycoord2),Time).
%; moving to a location causes the object no longer to be at its previous
%; location

terminates(move(Grid,Object,Xcoord1,Ycoord1,Xcoord2,Ycoord2),location(Grid,Object,Xcoord1,Ycoord1),Time).
%;; allow diagonal movements
%;[grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
%;Happens(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),time) ->
%;HoldsAt(Location(grid,object,xcoord1,ycoord1),time) &
%;(xcoord1=xcoord2 |
%; xcoord1=xcoord2+1 |
%; xcoord1=xcoord2-1) &
%;(ycoord1=ycoord2 |
%; ycoord1=ycoord2+1 |
%; ycoord1=ycoord2-1).
%; only allow right angle movements

happens(move(Grid,Object,Xcoord1,Ycoord1,Xcoord2,Ycoord2),Time)->holds_at(location(Grid,Object,Xcoord1,Ycoord1),Time),(Xcoord1=Xcoord2,(Ycoord1=Ycoord2+1;Ycoord1=Ycoord2-1);Ycoord1=Ycoord2,(Xcoord1=Xcoord2+1;Xcoord1=Xcoord2-1)).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: ecnet/HandTo.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

event(handTo(agent,agent,physobj)).
initiates(handTo(Agent1,Agent2,Physobj),holding(Agent2,Physobj),Time).
terminates(handTo(Agent1,Agent2,Physobj),holding(Agent1,Physobj),Time).
happens(handTo(Agent1,Agent2,Physobj),Time)->holds_at(holding(Agent1,Physobj),Time).
event(shakeHands(agent,agent)).
event(writeOn(agent,paper,pen)).
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: ecnet/Container.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%;
%; Container: container
%;
%; linkage to OTSpace(M):

happens(takeOutOf(Agent,Container1,Container2),Time)->holds_at(containerIsOpen(Container2),Time).
happens(putInside(Agent,Container1,Container2),Time)->holds_at(containerIsOpen(Container2),Time).
%; agent opens container.

event(containerOpen(agent,container)).
%; agent closes container.

event(containerClose(agent,container)).
%; container is open.

fluent(containerIsOpen(container)).
fluent(containerClosed(container)).
noninertial(containerClosed).
<->(holds_at(containerClosed(Container),Time),neg(holds_at(containerIsOpen(Container),Time))).
%; A precondition axiom states that
%; for an agent to open a container,
%; the agent must be awake,
%; the container must not already be open, and
%; the agent must be holding the container.

happens(containerOpen(Agent,Container),Time)->holds_at(awake(Agent),Time),neg(holds_at(containerIsOpen(Container),Time)),holds_at(holding(Agent,Container),Time).
%; An effect axiom states that
%; if an agent opens a container,
%; the container will be open:

initiates(containerOpen(Agent,Container),containerIsOpen(Container),Time).
%; A precondition axiom states that
%; for an agent to close a container,
%; the agent must be awake,
%; the container must be open, and
%; the agent must be holding the container.

happens(containerClose(Agent,Container),Time)->holds_at(awake(Agent),Time),holds_at(containerIsOpen(Container),Time),holds_at(holding(Agent,Container),Time).
%; An effect axiom states that
%; if an agent closes a container,
%; the container will no longer be open:

terminates(containerClose(Agent,Container),containerIsOpen(Container),Time).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: ecnet/SpeechAct.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%; The SpeechAct representation deals with a few speech acts
%; \fullcite{Searle:1969}.
%;
%; @book{Searle:1969,
%;   author = "John R. Searle",
%;   year = "1969",
%;   title = "Speech Acts: An Essay in the Philosophy of Language",
%;   address = "Cambridge",
%;   publisher = "Cambridge University Press",
%; }
%;
%; We handle
%; the illocutionary acts of
%; inviting someone into one's house (a form of request) and
%; greeting someone,
%; and the expressive speech act of crying for joy.
%;
%; inviting in
%; agent1 invites agent2 into room.

event(inviteIn(agent,agent,room)).
%; agent1 is invited into room by agent2.

fluent(invitedIn(agent,room,agent)).
%; A precondition axiom states that for
%; an agent to invite another agent into a room,
%; the first agent must be in the room and
%; there must be an outside area such that
%; the second agent is at the outside area and
%; the outside area is adjacent to the room:

happens(inviteIn(Agent1,Agent2,Room),Time)->holds_at(at(Agent1,Room),Time),holds_at(at(Agent2,Outside),Time),adjacent(Room,Outside).
%; An effect axiom states that if
%; an agent invites another agent into a room,
%; the second agent will be invited into the room by the first agent:

initiates(inviteIn(Agent1,Agent2,Room),invitedIn(Agent2,Room,Agent1),Time).
%; agent intends to walk into room.

event(intendToWalkIn(agent,room)).
%; agent has the intention to walk into room.

fluent(intentionToWalkIn(agent,room)).
%; agent acts on the intention to walk into room.

fluent(actOnIntentionToWalkIn(agent,room)).
noninertial(actOnIntentionToWalkIn).
%; A trigger axiom states that
%; if an agent is invited into a room by another agent,
%; the first agent likes the second agent, and
%; the first agent does not already have the intention to
%; walk into the room,
%; the first agent intends to walk into the room:

holds_at(invitedIn(Agent1,Room,Agent2),Time),holds_at(like(Agent1,Agent2),Time),neg(holds_at(intentionToWalkIn(Agent1,Room),Time))->happens(intendToWalkIn(Agent1,Room),Time).
%; An effect axiom states that
%; if an agent intends to walk into a room,
%; the agent will have the intention to walk into the room:

initiates(intendToWalkIn(Agent,Room),intentionToWalkIn(Agent,Room),Time).
%; Two trigger axioms state that
%; if an agent has the intention to walk into a room,
%; the agent acts on the intention to walk into the room,
%; the agent is at a location,
%; side one (two) of a door is the room,
%; side two (one) of the door is the location,
%; agent will walk through side two (one) of the door:

holds_at(intentionToWalkIn(Agent,Room),Time),holds_at(actOnIntentionToWalkIn(Agent,Room),Time),holds_at(at(Agent,Location),Time),side1(Door,Room),side2(Door,Location)->happens(walkThroughDoor21(Agent,Door),Time).
holds_at(intentionToWalkIn(Agent,Room),Time),holds_at(actOnIntentionToWalkIn(Agent,Room),Time),holds_at(at(Agent,Location),Time),side2(Door,Room),side1(Door,Location)->happens(walkThroughDoor12(Agent,Door),Time).
%; Two effect axioms state that
%; if side one (two) of a door is a room and
%; an agent walks through side two (one) of the door,
%; the agent will no longer have the intention to
%; walk into the room:

side1(Door,Room)->terminates(walkThroughDoor21(Agent,Door),intentionToWalkIn(Agent,Room),Time).
side2(Door,Room)->terminates(walkThroughDoor12(Agent,Door),intentionToWalkIn(Agent,Room),Time).
%; agent greets object.

event(greet(agent,object)).
event(sayPleasedToMeet(agent,agent)).
%; agent says goodbye to object.

event(sayGoodbye(agent,object)).
event(talkAbout(agent,content)).
event(converse(agent,agent)).
happens(converse(Agent1,Agent2),Time)->holds_at(at(Agent1,Location),Time),holds_at(at(Agent2,Location),Time).
%; A precondition axiom states that for
%; an agent to greet an object,
%; there must be a location such that
%; the agent is at the location and
%; the object is at the location:

happens(greet(Agent,Object),Time)->holds_at(at(Agent,Location),Time),holds_at(at(Object,Location),Time).
happens(sayGoodbye(Agent,Object),Time)->holds_at(at(Agent,Location),Time),holds_at(at(Object,Location),Time).
%; speech: expression of emotions
%; agent cries for joy.

event(cryForJoy(agent)).
%; A precondition axiom states that for
%; an agent to cry for joy,
%; the agent must be happy:

happens(cryForJoy(Agent),Time)->holds_at(happy(Agent),Time).
event(threaten(Agent,Agent,weapon)).
event(releaseFromThreat(Agent,Agent)).
fluent(threatenedBy(Agent,Agent)).
happens(threaten(Agent1,Agent2,Weapon),Time)->holds_at(holding(Agent1,Weapon),Time),holds_at(at(Agent1,Location),Time),holds_at(at(Agent2,Location),Time).
happens(threaten(Agent1,Agent2,Weapon),Time)->happens(becomeAngryAt(Agent2,Agent1),Time).
initiates(threaten(Agent1,Agent2,Weapon),threatenedBy(Agent2,Agent1),Time).
terminates(releaseFromThreat(Agent1,Agent2),threatenedBy(Agent2,Agent1),Time).
event(order(Agent,Agent,physobj)).
fluent(knowOrder(Agent,Agent,physobj)).
initiates(order(Agent1,Agent2,Physobj),knowOrder(Agent2,Agent1,Physobj),Time).
happens(order(Agent1,Agent2,Physobj),Time)->holds_at(at(Agent1,Location),Time),holds_at(at(Agent2,Location),Time).
event(request(Agent,Agent,Physobj)).
fluent(knowRequest(Agent,Agent,Physobj)).
initiates(request(Agent1,Agent2,Physobj),knowRequest(Agent2,Agent1,Physobj),Time).
happens(request(Agent1,Agent2,Physobj),Time)->holds_at(at(Agent1,Location),Time),holds_at(at(Agent2,Location),Time).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: ecnet/Sleep.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%; sleep
%; agent wakes up.

event(wakeUp(agent)).
%; agent gets tired.

event(getTired(agent)).
%; agent falls asleep.

event(fallAsleep(agent)).
%; agent is asleep.

fluent(sleep0(agent)).
%; agent is awake and in bed.

fluent(sleep1(agent)).
%; agent is awake, out of bed, and undressed.

fluent(sleep2(agent)).
%; agent is awake and dressed.

fluent(sleep3(agent)).
%; agent is tired and dressed.

fluent(sleep4(agent)).
%; agent is tired and undressed.

fluent(sleep5(agent)).
%; agent is in bed, waiting to fall asleep.

fluent(sleep6(agent)).
%; At any time, an agent is in one of seven sleep states:

xor([sleep0,sleep1,sleep2,sleep3,sleep4,sleep5,sleep6]).
%; constraints
%; agent is asleep.

fluent(asleep(agent)).
%; agent is awake.

fluent(awake(agent)).
noninertial(asleep).
noninertial(awake).
%; Sleep0 indicates that the agent is asleep:

<->(holds_at(asleep(Agent),Time),holds_at(sleep0(Agent),Time)).
%; In all other sleep states, the agent is awake:

<->(holds_at(awake(Agent),Time),holds_at(sleep1(Agent),Time));holds_at(sleep2(Agent),Time);holds_at(sleep3(Agent),Time);holds_at(sleep4(Agent),Time);holds_at(sleep5(Agent),Time);holds_at(sleep6(Agent),Time).
%; A number of axioms are used to specify the transitions of
%; a finite automaton.
%;--
%; Waking up causes a transition from Sleep0
%; to Sleep1:

terminates(wakeUp(Agent),sleep0(Agent),Time).
initiates(wakeUp(Agent),sleep1(Agent),Time).
happens(wakeUp(Agent),Time)->holds_at(sleep0(Agent),Time).
%;--
%; Getting out of bed causes a transition from Sleep1
%; to Sleep2:

terminates(riseFrom(Agent,Bed),sleep1(Agent),Time).
initiates(riseFrom(Agent,Bed),sleep2(Agent),Time).
happens(riseFrom(Agent,Bed),Time)->holds_at(sleep1(Agent),Time).
%;--
%; Getting dressed causes a transition from Sleep2
%; to Sleep3, the normal state of awakeness:

terminates(getDressed(Agent),sleep2(Agent),Time).
initiates(getDressed(Agent),sleep3(Agent),Time).
happens(getDressed(Agent),Time)->holds_at(sleep2(Agent),Time).
%;--
%; Getting tired causes a transition from Sleep3
%; to Sleep4:

terminates(getTired(Agent),sleep3(Agent),Time).
initiates(getTired(Agent),sleep4(Agent),Time).
happens(getTired(Agent),Time)->holds_at(sleep3(Agent),Time).
%;--
%; Getting undressed causes a transition from Sleep4
%; to Sleep5:

terminates(getUndressed(Agent),sleep4(Agent),Time).
initiates(getUndressed(Agent),sleep5(Agent),Time).
happens(getUndressed(Agent),Time)->holds_at(sleep4(Agent),Time).
%;--
%; Lying on a bed causes a transition from Sleep5
%; to Sleep6:

terminates(lieOn(Agent,Bed),sleep5(Agent),Time).
initiates(lieOn(Agent,Bed),sleep6(Agent),Time).
happens(lieOn(Agent,Bed),Time)->holds_at(sleep5(Agent),Time).
%;--
%; Falling asleep causes a transition from Sleep6
%; to Sleep0:

terminates(fallAsleep(Agent),sleep6(Agent),Time).
initiates(fallAsleep(Agent),sleep0(Agent),Time).
happens(fallAsleep(Agent),Time)->holds_at(sleep6(Agent),Time).
%;--
%; agent acts on being in state Sleep5.

fluent(actOnSleep5(agent)).
noninertial(actOnSleep5).
%; We reduce the number of models by asserting that
%; an agent only acts on being in state Sleep5 while in
%; that state:

neg(holds_at(sleep5(Agent),Time))->neg(holds_at(actOnSleep5(Agent),Time)).
%; Undressed is like IntentionToPlay
%; ActOnSleep5 is like ActOnIntentionToPlay
%; A trigger axiom states that if an agent is in state Sleep5,
%; the agent acts on this state, the agent is in a room, and
%; a bed is at the room, the agent lies on the bed:

holds_at(sleep5(Agent),Time),holds_at(actOnSleep5(Agent),Time),holds_at(at(Agent,Room),Time),holds_at(at(Bed,Room),Time)->happens(lieOn(Agent,Bed),Time).
%; A precondition axiom states that for
%; an agent to lie on a bed,
%; the agent must be in state Sleep5,
%; the agent must act on this state, and
%; there must be a room such that
%; the agent is in the room and the bed is in the room:

happens(lieOn(Agent,Bed),Time)->holds_at(sleep5(Agent),Time),holds_at(actOnSleep5(Agent),Time),holds_at(at(Agent,Room),Time),holds_at(at(Bed,Room),Time).
%; (body) posture
%; agent lies on physobj.

event(lieOn(agent,physobj)).
%; agent sits on physobj.

event(sitOn(agent,physobj)).
happens(sitOn(Agent,Physobj),Time)->holds_at(at(Agent,Location),Time),holds_at(at(Physobj,Location),Time).
%; agent rises from physobj.

event(riseFrom(agent,physobj)).
%; agent is lying on physobj.

fluent(lyingOn(agent,physobj)).
%; agent is sitting on physobj.

fluent(sittingOn(agent,physobj)).
%; agent is standing.

fluent(standing(agent)).
%; agent is lying down.

fluent(lying(agent)).
%; agent is sitting.

fluent(sitting(agent)).
noninertial(lying).
noninertial(sitting).
%; At any time, an agent is either lying, sitting, or standing:

xor([lying,sitting,standing]).
holds_at(lyingOn(Agent,Physobj),Time)->holds_at(lying(Agent),Time).
holds_at(sittingOn(Agent,Physobj),Time)->holds_at(sitting(Agent),Time).
%; State constraints represent that an agent can lie or sit
%; on at most one object at a time:

holds_at(lyingOn(Agent,Physobj1),Time),holds_at(lyingOn(Agent,Physobj2),Time)->Physobj1=Physobj2.
holds_at(sittingOn(Agent,Physobj1),Time),holds_at(sittingOn(Agent,Physobj2),Time)->Physobj1=Physobj2.
%; An effect axiom states that if an agent is standing and
%; lies on a physical object, the agent will be lying on
%; the physical object:

holds_at(standing(Agent),Time)->initiates(lieOn(Agent,Physobj),lyingOn(Agent,Physobj),Time).
%; An effect axiom states that if an agent
%; lies on a physical object, the agent will no longer
%; be standing:

terminates(lieOn(Agent,Physobj),standing(Agent),Time).
%; An effect axiom states that if an agent is standing and
%; sits on a physical object, the agent will be sitting on
%; the physical object:

holds_at(standing(Agent),Time)->initiates(sitOn(Agent,Physobj),sittingOn(Agent,Physobj),Time).
%; An effect axiom states that if an agent
%; sits on a physical object, the agent will no longer
%; be standing:

terminates(sitOn(Agent,Physobj),standing(Agent),Time).
%; An effect axiom states that if an agent
%; is sitting or lying on a physical object and
%; the agent rises from the physical object,
%; the agent will be standing:

(holds_at(sittingOn(Agent,Physobj),Time);holds_at(lyingOn(Agent,Physobj),Time))->initiates(riseFrom(Agent,Physobj),standing(Agent),Time).
%; An effect axiom states that if an agent is sitting on
%; a physical object and the agent rises from the physical
%; object, the agent will no longer be sitting on the
%; physical object:

holds_at(sittingOn(Agent,Physobj),Time)->terminates(riseFrom(Agent,Physobj),sittingOn(Agent,Physobj),Time).
%; An effect axiom states that if an agent is lying on
%; a physical object and the agent rises from the physical
%; object, the agent will no longer be lying on the
%; physical object:

holds_at(lyingOn(Agent,Physobj),Time)->terminates(riseFrom(Agent,Physobj),lyingOn(Agent,Physobj),Time).
%; dressing
%; agent gets undressed.

event(getDressed(agent)).
%; agent gets dressed.

event(getUndressed(agent)).
%; agent is dressed.

fluent(dressed(agent)).
%; Effect axioms deal with getting dressed and undressed:

initiates(getDressed(Agent),dressed(Agent),Time).
terminates(getUndressed(Agent),dressed(Agent),Time).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: ecnet/Sleeping.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

option(modeldiff,on).
ignore([love,threatenedBy]).
ignore([lookOutOnto,floor,buildingOf,skyOf,groundOf]).
ignore([inside,near]).
ignore(see).
ignore(actOnSleep5).
option(renaming,off).
load('foundations/Root.e').
load('foundations/EC.e').
load('answers/Mueller2003/Ontology.e').
load('answers/Mueller2004c/RTSpaceM.e').
load('answers/Mueller2004c/OTSpaceM.e').
load('answers/Mueller2004c/Cognition.e').
load('answers/Mueller2003/Sleep.e').
door(door1).
room(room0).
room(room1).
side1(door1,room0).
side2(door1,room1).
agent(sleeper1).
bed(bed1).
outside(outside1).
%; initial state

neg(holds_at(holding(Agent,Object),0)).
neg(holds_at(sittingOn(Agent,Physobj),0)).
neg(holds_at(lyingOn(Agent,Physobj),0)).
holds_at(dressed(sleeper1),0).
holds_at(awake(sleeper1),0).
holds_at(sleep3(sleeper1),0).
holds_at(standing(sleeper1),0).
holds_at(doorUnlocked(door1),0).
holds_at(doorIsOpen(door1),0).
holds_at(at(sleeper1,room0),0).
holds_at(at(bed1,room1),0).
%; narrative

happens(getTired(sleeper1),0).
happens(walkThroughDoor12(sleeper1,door1),1).
happens(getUndressed(sleeper1),2).
happens(lieOn(sleeper1,bed1),3).
happens(fallAsleep(sleeper1),4).
happens(dream(sleeper1),5).
happens(wakeUp(sleeper1),6).
happens(riseFrom(sleeper1,bed1),7).
happens(getDressed(sleeper1),8).
happens(walkThroughDoor21(sleeper1,door1),9).
range(time,0,10).
range(offset,0,0).
range(diameter,0,0).
completion(happens).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: ecnet/Rest.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%; @article{Mueller:InPress,
%;   author = "Erik T. Mueller",
%;   year = "in press",
%;   title = "Modelling space and time in narratives about restaurants",
%;   journal = "Literary and Linguistic Computing",
%; }
%;

option(renaming,off).
option(encoding,3).
load('foundations/Root.e').
load('foundations/EC.e').
load('answers/Mueller2003/Ontology.e').
load('answers/MuellerInPress/RepRest.e').
door(mainEntrance1).
%; room-scale topological space

outside(street1).
room(diningRoom1).
door(kitchenDoor1).
room(kitchen1).
side1(mainEntrance1,street1).
side2(mainEntrance1,diningRoom1).
side1(kitchenDoor1,diningRoom1).
side2(kitchenDoor1,kitchen1).
agent(customer1).
menu(menu1).
chair(chair1).
food(food1).
holds_at(at(customer1,street1),0).
holds_at(hungry(customer1),0).
holds_at(at(chair1,diningRoom1),0).
holds_at(at(menu1,diningRoom1),0).
holds_at(on(menu1,table1),0).
holds_at(at(food1,kitchen1),0).
waiter(waiter1).
cook(cook1).
%; props

table(table1).
bill(bill1).
%; restaurant

restaurant(restaurant1).
cookOf(restaurant1,cook1).
tableOf(restaurant1,table1).
waiterOf(restaurant1,waiter1).
kitchenDoorOf(restaurant1,kitchenDoor1).
billOf(restaurant1,bill1).
%; prune

sort([ona,onb]).
fluent(on(ona,onb)).
event(placeOn(agent,ona,onb)).
event(takeOffOf(agent,ona,onb)).
sort([ordera,orderb,orderc]).
event(order(ordera,orderb,orderc)).
fluent(knowOrder(orderb,ordera,orderc)).
sort([requesta,requestb,requestc]).
event(request(requesta,requestb,requestc)).
fluent(knowRequest(requestb,requesta,requestc)).
sort([holda,holdb,holdc]).
event(takeOffOf(holda,holdb,holdc)).
event(pickUp(holda,holdb)).
event(letGoOf(holda,holdb)).
event(hold(holda,holdb)).
fluent(holding(holda,holdb)).
sort([sita,sitb]).
event(lieOn(sita,sitb)).
event(sitOn(sita,sitb)).
event(riseFrom(sita,sitb)).
fluent(lyingOn(sita,sitb)).
fluent(sittingOn(sita,sitb)).
sort([greeta,greetb]).
event(greet(greeta,greetb)).
ona(menu1).
ona(food1).
ona(bill1).
onb(table1).
ordera(customer1).
ordera(waiter1).
orderb(waiter1).
orderb(cook1).
orderc(food1).
requesta(customer1).
requestb(waiter1).
requestc(bill1).
holda(customer1).
holda(waiter1).
holdb(menu1).
holdb(food1).
holdb(bill1).
holdc(table1).
sita(customer1).
sitb(chair1).
greeta(customer1).
greeta(waiter1).
greetb(customer1).
greetb(waiter1).
%; initial situation

holds_at(at(waiter1,diningRoom1),0).
holds_at(at(cook1,kitchen1),0).
holds_at(at(table1,diningRoom1),0).
neg(holds_at(on(bill1,table1),0)).
holds_at(at(bill1,diningRoom1),0).
holds_at(standing(Agent),0).
neg(holds_at(holding(Agent,Object),0)).
neg(holds_at(knowOrder(Agent1,Agent2,Physobj),0)).
neg(holds_at(knowRequest(Agent1,Agent2,Physobj),0)).
holds_at(beWaiter0(waiter1),0).
holds_at(beCook0(cook1),0).
neg(holds_at(foodPrepared(Food),0)).
neg(holds_at(hungry(cook1),0)).
neg(holds_at(hungry(waiter1),0)).
happens(walkThroughDoor12(customer1,mainEntrance1),0).
happens(greet(waiter1,customer1),1).
happens(sitOn(customer1,chair1),2).
happens(takeOffOf(customer1,menu1,table1),3).
happens(order(customer1,waiter1,food1),4).
happens(placeOn(customer1,menu1,table1),5).
happens(eat(customer1,food1),11).
happens(request(customer1,waiter1,bill1),12).
happens(pay(customer1,waiter1),15).
happens(tip(customer1,waiter1),15).
happens(riseFrom(customer1,chair1),16).
happens(sayGoodbye(customer1,waiter1),17).
happens(walkThroughDoor21(customer1,mainEntrance1),18).
range(time,0,19).
range(offset,0,0).
range(diameter,0,0).
completion(happens).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: ecnet/RepRest.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%; @article{Mueller:InPress,
%;   author = "Erik T. Mueller",
%;   year = "in press",
%;   title = "Modelling space and time in narratives about restaurants",
%;   journal = "Literary and Linguistic Computing",
%; }
%;
%;sort boolean
%;sort integer
%;reified sort predicate
%;reified sort function
%;
%;sort time: integer
%;sort offset: integer
%;
%;reified sort fluent
%;reified sort event
%;
%;predicate Happens(event,time)
%;predicate HoldsAt(fluent,time)
%;predicate ReleasedAt(fluent,time)
%;predicate Initiates(event,fluent,time)
%;predicate Terminates(event,fluent,time)
%;predicate Releases(event,fluent,time)
%;
%;sort diameter: integer
%;
%;sort object
%;
%;sort agent: object
%;
%;sort physobj: object
%;sort bed: physobj
%;sort snowflake: physobj
%;sort sky: physobj
%;
%;sort stuff: physobj
%;
%;sort surface: physobj
%;sort ground: surface
%;
%;sort snow: stuff
%;sort ball
%;
%;sort food: physobj
%;sort fruit: food
%;sort orange: fruit
%;sort salad: food
%;
%;sort clothing: physobj
%;sort scarf: clothing
%;sort hat: clothing
%;
%;sort vegetablematter: physobj
%;sort coal: vegetablematter
%;
%;sort bodypart: physobj
%;sort hand: bodypart
%;
%;sort papertowels: physobj
%;sort device: physobj
%;sort electronicdevice: device
%;sort lamp: electronicdevice
%;
%;sort cat: physobj
%;
%;sort weapon: physobj
%;sort gun: weapon
%;sort bomb: weapon
%;sort bullet: weapon
%;
%;sort location
%;sort room: location, outside: location
%;
%;sort portal
%;sort door: portal, staircase: portal
%;sort street: portal
%;
%;sort building
%;
%;sort fire: object
%;
%;sort furniture: physobj
%;sort chair: furniture
%;sort table: furniture
%;
%;sort menu: physobj
%;sort bill: physobj
%;
%;sort script
%;

fluent(holding(agent,physobj)).
event(pickUp(agent,physobj)).
event(letGoOf(agent,physobj)).
initiates(pickUp(Agent,Physobj),holding(Agent,Physobj),Time).
happens(pickUp(Agent,Physobj),Time)->holds_at(at(Agent,Location),Time),holds_at(at(Physobj,Location),Time).
terminates(letGoOf(Agent,Physobj),holding(Agent,Physobj),Time).
happens(letGoOf(Agent,Physobj),Time)->holds_at(holding(Agent,Physobj),Time).
releases(pickUp(Agent,Physobj),at(Physobj,Location),Time).
holds_at(holding(Agent,Physobj),Time),holds_at(at(Agent,Location),Time)->holds_at(at(Physobj,Location),Time).
%;[agent,physobj,location1,location2,time]
%;HoldsAt(At(agent,location1),time) &
%;location1!=location2 ->
%;Terminates(LetGoOf(agent,physobj),At(physobj,location2),time).

holds_at(at(Agent,Location),Time)->initiates(letGoOf(Agent,Physobj),at(Physobj,Location),Time).
fluent(on(Physobj,Physobj)).
event(placeOn(Agent,Physobj,Physobj)).
event(takeOffOf(Agent,Physobj,Physobj)).
holds_at(on(Physobj1,Physobj2),Time)->Physobj1\=Physobj2.
holds_at(on(Physobj1,Physobj2),Time)->neg(holds_at(on(Physobj2,Physobj1),Time)).
initiates(placeOn(Agent,Physobj1,Physobj2),on(Physobj1,Physobj2),Time).
terminates(placeOn(Agent,Physobj1,Physobj2),holding(Agent,Physobj1),Time).
happens(placeOn(Agent,Physobj1,Physobj2),Time)->holds_at(holding(Agent,Physobj1),Time),holds_at(at(Agent,Location),Time),holds_at(at(Physobj2,Location),Time).
terminates(takeOffOf(Agent,Physobj1,Physobj2),on(Physobj1,Physobj2),Time).
initiates(takeOffOf(Agent,Physobj1,Physobj2),holding(Agent,Physobj1),Time).
releases(takeOffOf(Agent,Physobj1,Physobj2),at(Physobj1,Location),Time).
happens(takeOffOf(Agent,Physobj1,Physobj2),Time)->holds_at(on(Physobj1,Physobj2),Time),holds_at(at(Agent,Location),Time),holds_at(at(Physobj1,Location),Time),holds_at(at(Physobj2,Location),Time).
releases(placeOn(Agent,Physobj1,Physobj2),at(Physobj1,Location),Time).
holds_at(on(Physobj1,Physobj2),Time),holds_at(at(Physobj2,Location),Time)->holds_at(at(Physobj1,Location),Time).
fluent(at(object,Location)).
holds_at(at(Object,Location),Time).
holds_at(at(Object,Location1),Time),holds_at(at(Object,Location2),Time)->Location1=Location2.
function(side1(portal),location).
function(side2(portal),location).
fluent(nearPortal(Object,portal)).
noninertial(nearPortal).
<->(holds_at(nearPortal(Object,Portal),Time),((side1(Portal,Location);side2(Portal,Location)),holds_at(at(Object,Location),Time))).
event(walkThroughDoor12(Agent,door)).
event(walkThroughDoor21(Agent,door)).
happens(walkThroughDoor12(Agent,Door),Time)->holds_at(standing(Agent),Time),holds_at(at(Agent,side1(Door)),Time).
happens(walkThroughDoor21(Agent,Door),Time)->holds_at(standing(Agent),Time),holds_at(at(Agent,side2(Door)),Time).
side2(Door,Location)->initiates(walkThroughDoor12(Agent,Door),at(Agent,Location),Time).
side1(Door,Location)->initiates(walkThroughDoor21(Agent,Door),at(Agent,Location),Time).
side1(Door,Location)->terminates(walkThroughDoor12(Agent,Door),at(Agent,Location),Time).
side2(Door,Location)->terminates(walkThroughDoor21(Agent,Door),at(Agent,Location),Time).
fluent(hungry(Agent)).
fluent(satiated(Agent)).
noninertial(satiated).
<->(holds_at(hungry(Agent),Time),neg(holds_at(satiated(Agent),Time))).
event(eat(Agent,food)).
happens(eat(Agent,Food),Time)->holds_at(at(Agent,Location),Time),holds_at(at(Food,Location),Time).
terminates(eat(Agent,Food),hungry(Agent),Time).
% gleaned(subsort(restaurant,script)).

subsort(restaurant,script).
% gleaned(subsort(waiter,agent)).

subsort(waiter,agent).
% gleaned(subsort(cook,agent)).

subsort(cook,agent).
function(billOf(restaurant),bill).
function(cookOf(restaurant),cook).
function(tableOf(restaurant),table).
function(waiterOf(restaurant),waiter).
function(kitchenDoorOf(restaurant),door).
fluent(beWaiter0(waiter)).
fluent(beWaiter1(waiter)).
fluent(beWaiter2(waiter)).
fluent(beWaiter3(waiter)).
fluent(beWaiter4(waiter)).
fluent(beWaiter5(waiter)).
fluent(beWaiter6(waiter)).
fluent(beWaiter7(waiter)).
fluent(beWaiter8(waiter)).
fluent(beWaiter9(waiter)).
xor([beWaiter0,beWaiter1,beWaiter2,beWaiter3,beWaiter4,beWaiter5,beWaiter6,beWaiter7,beWaiter8,beWaiter9]).
holds_at(beWaiter0(Waiter),Time)->terminates(greet(Waiter,Agent),beWaiter0(Waiter),Time).
holds_at(beWaiter0(Waiter),Time)->initiates(greet(Waiter,Agent),beWaiter1(Waiter),Time).
holds_at(beWaiter1(Waiter),Time)->terminates(order(Agent,Waiter,Food),beWaiter1(Waiter),Time).
holds_at(beWaiter1(Waiter),Time)->initiates(order(Agent,Waiter,Food),beWaiter2(Waiter),Time).
waiterOf(Restaurant,Waiter),holds_at(beWaiter2(Waiter),Time)->happens(walkThroughDoor12(Waiter,kitchenDoorOf(Restaurant)),Time).
holds_at(beWaiter2(Waiter),Time),waiterOf(Restaurant,Waiter),kitchenDoorOf(Restaurant,Door)->terminates(walkThroughDoor12(Waiter,Door),beWaiter2(Waiter),Time).
holds_at(beWaiter2(Waiter),Time),waiterOf(Restaurant,Waiter),kitchenDoorOf(Restaurant,Door)->initiates(walkThroughDoor12(Waiter,Door),beWaiter3(Waiter),Time).
exists([Agent],(holds_at(beWaiter3(waiterOf(Restaurant)),Time),holds_at(knowOrder(waiterOf(Restaurant),Agent,Food),Time)->happens(order(waiterOf(Restaurant),cookOf(Restaurant),Food),Time))).
waiterOf(Restaurant,Waiter),cookOf(Restaurant,Cook),holds_at(beWaiter3(Waiter),Time)->terminates(order(Waiter,Cook,Food),beWaiter3(Waiter),Time).
waiterOf(Restaurant,Waiter),cookOf(Restaurant,Cook),holds_at(beWaiter3(Waiter),Time)->initiates(order(Waiter,Cook,Food),beWaiter4(Waiter),Time).
exists([Agent],(holds_at(beWaiter4(Waiter),Time),holds_at(knowOrder(Waiter,Agent,Food),Time),holds_at(foodPrepared(Food),Time)->happens(pickUp(Waiter,Food),Time))).
exists([Agent],(holds_at(beWaiter4(Waiter),Time),holds_at(knowOrder(Waiter,Agent,Food),Time)->terminates(pickUp(Waiter,Food),beWaiter4(Waiter),Time))).
exists([Agent],(holds_at(beWaiter4(Waiter),Time),holds_at(knowOrder(Waiter,Agent,Food),Time)->initiates(pickUp(Waiter,Food),beWaiter5(Waiter),Time))).
waiterOf(Restaurant,Waiter),holds_at(beWaiter5(Waiter),Time)->happens(walkThroughDoor21(Waiter,kitchenDoorOf(Restaurant)),Time).
holds_at(beWaiter5(Waiter),Time),waiterOf(Restaurant,Waiter),kitchenDoorOf(Restaurant,Door)->terminates(walkThroughDoor21(Waiter,Door),beWaiter5(Waiter),Time).
holds_at(beWaiter5(Waiter),Time),waiterOf(Restaurant,Waiter),kitchenDoorOf(Restaurant,Door)->initiates(walkThroughDoor21(Waiter,Door),beWaiter6(Waiter),Time).
waiterOf(Restaurant,Waiter),tableOf(Restaurant,Table),holds_at(beWaiter6(Waiter),Time),holds_at(holding(Waiter,Food),Time)->happens(placeOn(Waiter,Food,Table),Time).
holds_at(beWaiter6(Waiter),Time)->terminates(placeOn(Waiter,Food,Table),beWaiter6(Waiter),Time).
holds_at(beWaiter6(Waiter),Time)->initiates(placeOn(Waiter,Food,Table),beWaiter7(Waiter),Time).
holds_at(beWaiter7(Waiter),Time)->terminates(request(Agent,Waiter,Bill),beWaiter7(Waiter),Time).
holds_at(beWaiter7(Waiter),Time)->initiates(request(Agent,Waiter,Bill),beWaiter8(Waiter),Time).
waiterOf(Restaurant,Waiter),billOf(Restaurant,Bill),holds_at(beWaiter8(Waiter),Time)->happens(pickUp(Waiter,Bill),Time).
holds_at(beWaiter8(Waiter),Time)->terminates(pickUp(Waiter,Bill),beWaiter8(Waiter),Time).
holds_at(beWaiter8(Waiter),Time)->initiates(pickUp(Waiter,Bill),beWaiter9(Waiter),Time).
waiterOf(Restaurant,Waiter),billOf(Restaurant,Bill),tableOf(Restaurant,Table),holds_at(beWaiter9(Waiter),Time)->happens(placeOn(Waiter,Bill,Table),Time).
holds_at(beWaiter9(Waiter),Time)->terminates(placeOn(Waiter,Bill,Table),beWaiter9(Waiter),Time).
holds_at(beWaiter9(Waiter),Time)->initiates(placeOn(Waiter,Bill,Table),beWaiter0(Waiter),Time).
fluent(beCook0(Cook)).
fluent(beCook1(Cook)).
xor([beCook0,beCook1]).
holds_at(beCook0(Cook),Time)->terminates(order(Agent,Cook,Food),beCook0(Cook),Time).
holds_at(beCook0(Cook),Time)->initiates(order(Agent,Cook,Food),beCook1(Cook),Time).
event(foodPrepare(Agent,Food)).
fluent(foodPrepared(Food)).
initiates(foodPrepare(Agent,Food),foodPrepared(Food),Time).
happens(foodPrepare(Agent,Food),Time)->holds_at(at(Agent,Location),Time),holds_at(at(Food,Location),Time).
holds_at(beCook1(Cook),Time),holds_at(knowOrder(Cook,Agent,Food),Time)->happens(foodPrepare(Cook,Food),Time).
holds_at(beCook1(Cook),Time)->terminates(foodPrepare(Cook,Food),beCook1(Cook),Time).
holds_at(beCook1(Cook),Time)->initiates(foodPrepare(Cook,Food),beCook0(Cook),Time).
event(pay(Agent,Agent)).
event(tip(Agent,Agent)).
happens(lieOn(Agent,Physobj),Time)->holds_at(at(Agent,Room),Time),holds_at(at(Physobj,Room),Time).
happens(sitOn(Agent,Physobj),Time)->holds_at(at(Agent,Room),Time),holds_at(at(Physobj,Room),Time).
event(lieOn(Agent,Physobj)).
event(sitOn(Agent,Physobj)).
event(riseFrom(Agent,Physobj)).
fluent(lyingOn(Agent,Physobj)).
fluent(sittingOn(Agent,Physobj)).
fluent(standing(Agent)).
fluent(lying(Agent)).
fluent(sitting(Agent)).
noninertial(lying).
noninertial(sitting).
xor([lying,sitting,standing]).
holds_at(lyingOn(Agent,Physobj),Time)->holds_at(lying(Agent),Time).
holds_at(sittingOn(Agent,Physobj),Time)->holds_at(sitting(Agent),Time).
holds_at(lyingOn(Agent,Physobj1),Time),holds_at(lyingOn(Agent,Physobj2),Time)->Physobj1=Physobj2.
holds_at(sittingOn(Agent,Physobj1),Time),holds_at(sittingOn(Agent,Physobj2),Time)->Physobj1=Physobj2.
holds_at(standing(Agent),Time)->initiates(lieOn(Agent,Physobj),lyingOn(Agent,Physobj),Time).
terminates(lieOn(Agent,Physobj),standing(Agent),Time).
holds_at(standing(Agent),Time)->initiates(sitOn(Agent,Physobj),sittingOn(Agent,Physobj),Time).
terminates(sitOn(Agent,Physobj),standing(Agent),Time).
(holds_at(sittingOn(Agent,Physobj),Time);holds_at(lyingOn(Agent,Physobj),Time))->initiates(riseFrom(Agent,Physobj),standing(Agent),Time).
holds_at(lyingOn(Agent,Physobj),Time)->terminates(riseFrom(Agent,Physobj),lyingOn(Agent,Physobj),Time).
holds_at(sittingOn(Agent,Physobj),Time)->terminates(riseFrom(Agent,Physobj),sittingOn(Agent,Physobj),Time).
event(greet(Agent,Agent)).
event(sayGoodbye(Agent,Agent)).
happens(greet(Agent1,Agent2),Time)->holds_at(at(Agent1,Location),Time),holds_at(at(Agent2,Location),Time).
happens(sayGoodbye(Agent1,Agent2),Time)->holds_at(at(Agent1,Location),Time),holds_at(at(Agent2,Location),Time).
event(order(Agent,Agent,Physobj)).
fluent(knowOrder(Agent,Agent,Physobj)).
initiates(order(Agent1,Agent2,Physobj),knowOrder(Agent2,Agent1,Physobj),Time).
happens(order(Agent1,Agent2,Physobj),Time)->holds_at(at(Agent1,Location),Time),holds_at(at(Agent2,Location),Time).
event(request(Agent,Agent,Physobj)).
fluent(knowRequest(Agent,Agent,Physobj)).
initiates(request(Agent1,Agent2,Physobj),knowRequest(Agent2,Agent1,Physobj),Time).
happens(request(Agent1,Agent2,Physobj),Time)->holds_at(at(Agent1,Location),Time),holds_at(at(Agent2,Location),Time).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: ecnet/Diving.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%; scuba diving
%;

sort(object).
% gleaned(subsort(agent,object)).

subsort(agent,object).
% gleaned(subsort(diver,agent)).

subsort(diver,agent).
% gleaned(subsort(depth,integer)).

subsort(depth,integer).
% gleaned(subsort(boat,object)).

subsort(boat,object).
%; reference line, anchor line, shotline, SMB line, ...

% gleaned(subsort(line,object)).

subsort(line,object).
% gleaned(subsort(equipment,object)).

subsort(equipment,object).
% gleaned(subsort(weight,equipment)).

subsort(weight,equipment).
% gleaned(subsort(fin,equipment)).

subsort(fin,equipment).
% gleaned(subsort(airtank,equipment)).

subsort(airtank,equipment).
%; buoyancy compensator (BC)
%; buoyancy control device (BCD)

% gleaned(subsort(computer,equipment)).

subsort(computer,equipment).
% gleaned(subsort(bc,equipment)).

subsort(bc,equipment).
fluent(atDepth(object,depth)).
holds_at(atDepth(Object,Depth1),Time),holds_at(atDepth(Object,Depth2),Time)->Depth1=Depth2.
event(ascend(diver,depth)).
event(descend(diver,depth)).
holds_at(atDepth(Diver,Depth1),Time),happens(descend(Diver,Depth2),Time)->Depth2>Depth1.
holds_at(atDepth(Diver,Depth1),Time),happens(ascend(Diver,Depth2),Time)->Depth2<Depth1.
initiates(descend(Diver,Depth),atDepth(Diver,Depth),Time).
holds_at(atDepth(Diver,Depth1),Time)->terminates(descend(Diver,Depth2),atDepth(Diver,Depth1),Time).
initiates(ascend(Diver,Depth),atDepth(Diver,Depth),Time).
holds_at(atDepth(Diver,Depth1),Time)->terminates(ascend(Diver,Depth2),atDepth(Diver,Depth1),Time).
fluent(wearing(Diver,equipment)).
event(putOn(Diver,equipment)).
event(takeOff(Diver,equipment)).
event(lose(Diver,equipment)).
releases(putOn(Diver,Equipment),atDepth(Equipment,Depth),Time).
releases(putOn(Diver,Equipment),underWater(Equipment),Time).
exists([Diver1],(happens(putOn(Diver,Equipment),Time)->neg(holds_at(wearing(Diver1,Equipment),Time)))).
holds_at(wearing(Diver,Equipment),Time)-> <->(holds_at(atDepth(Diver,Depth),Time),holds_at(atDepth(Equipment,Depth),Time)).
holds_at(holding(Diver,Object),Time)-> <->(holds_at(atDepth(Diver,Depth),Time),holds_at(atDepth(Object,Depth),Time)).
holds_at(wearing(Diver,Equipment),Time)-> <->(holds_at(underWater(Diver),Time),holds_at(underWater(Equipment),Time)).
holds_at(holding(Diver,Object),Time)-> <->(holds_at(underWater(Diver),Time),holds_at(underWater(Object),Time)).
holds_at(atDepth(Diver,Depth),Time),holds_at(wearing(Diver,Equipment),Time)->initiates(takeOff(Diver,Equipment),atDepth(Equipment,Depth),Time).
neg(holds_at(atDepth(Diver,Depth),Time)),holds_at(wearing(Diver,Equipment),Time)->terminates(takeOff(Diver,Equipment),atDepth(Equipment,Depth),Time).
holds_at(underWater(Diver),Time)->initiates(takeOff(Diver,Equipment),underWater(Equipment),Time).
neg(holds_at(underWater(Diver),Time))->terminates(takeOff(Diver,Equipment),underWater(Equipment),Time).
holds_at(atDepth(Diver,Depth),Time),holds_at(wearing(Diver,Equipment),Time)->initiates(lose(Diver,Equipment),atDepth(Equipment,Depth),Time).
neg(holds_at(atDepth(Diver,Depth),Time)),holds_at(wearing(Diver,Equipment),Time)->terminates(lose(Diver,Equipment),atDepth(Equipment,Depth),Time).
holds_at(underWater(Diver),Time)->initiates(lose(Diver,Equipment),underWater(Equipment),Time).
neg(holds_at(underWater(Diver),Time))->terminates(lose(Diver,Equipment),underWater(Equipment),Time).
fluent(holding(Diver,Object)).
holds_at(holding(Diver1,Diver2),Time)->neg(holds_at(holding(Diver2,Diver1),Time)).
event(grab(Diver,Object)).
event(letGoOf(Diver,Object)).
initiates(grab(Diver,Object),holding(Diver,Object),Time).
terminates(letGoOf(Diver,Object),holding(Diver,Object),Time).
releases(grab(Diver,Object),atDepth(Object,Depth),Time).
releases(grab(Diver,Object),underWater(Object),Time).
holds_at(atDepth(Diver,Depth),Time),holds_at(holding(Diver,Object),Time)->initiates(letGoOf(Diver,Object),atDepth(Object,Depth),Time).
neg(holds_at(atDepth(Diver,Depth),Time)),holds_at(holding(Diver,Object),Time)->terminates(letGoOf(Diver,Object),atDepth(Object,Depth),Time).
holds_at(underWater(Diver),Time)->initiates(letGoOf(Diver,Object),underWater(Object),Time).
neg(holds_at(underWater(Diver),Time))->terminates(letGoOf(Diver,Object),underWater(Object),Time).
initiates(putOn(Diver,Equipment),wearing(Diver,Equipment),Time).
happens(putOn(Diver,Equipment),Time)->neg(holds_at(underWater(Diver),Time)).
terminates(takeOff(Diver,Equipment),wearing(Diver,Equipment),Time).
terminates(lose(Diver,Equipment),wearing(Diver,Equipment),Time).
fluent(vertical(Diver)).
fluent(horizontalDown(Diver)).
fluent(inverted(Diver)).
fluent(horizontalUp(Diver)).
xor([vertical,horizontalDown,inverted,horizontalUp]).
event(rotatePitch(Diver)).
holds_at(vertical(Diver),Time)->initiates(rotatePitch(Diver),horizontalDown(Diver),Time).
holds_at(horizontalDown(Diver),Time)->initiates(rotatePitch(Diver),inverted(Diver),Time).
holds_at(horizontalDown(Diver),Time)->terminates(rotatePitch(Diver),horizontalDown(Diver),Time).
holds_at(inverted(Diver),Time)->initiates(rotatePitch(Diver),horizontalUp(Diver),Time).
holds_at(inverted(Diver),Time)->terminates(rotatePitch(Diver),inverted(Diver),Time).
holds_at(horizontalUp(Diver),Time)->initiates(rotatePitch(Diver),vertical(Diver),Time).
holds_at(horizontalUp(Diver),Time)->terminates(rotatePitch(Diver),horizontalUp(Diver),Time).
event(rotateYaw(Diver)).
%; try taking out Holding condition here

exists([Diver1],(happens(ascend1(Diver),Time),neg(happens(rapidAscendToSurface(Diver),Time)),neg(holds_at(holding(Diver,Diver1),Time))->happens(rotateYaw(Diver),Time))).
fluent(underWater(object)).
Depth>0,holds_at(atDepth(Object,Depth),Time)->holds_at(underWater(Object),Time).
event(enterWater(Object)).
event(surface(Object)).
initiates(enterWater(Object),underWater(Object),Time).
exists([Diver1],(happens(enterWater(Diver),Time)->neg(holds_at(holding(Diver1,Diver),Time)))).
Depth=0->initiates(enterWater(Object),atDepth(Object,Depth),Time).
terminates(surface(Object),underWater(Object),Time).
terminates(surface(Diver),positivelyBuoyant(Diver),Time).
terminates(surface(Diver),negativelyBuoyant(Diver),Time).
terminates(surface(Diver),neutrallyBuoyant(Diver),Time).
terminates(surface(Object),atDepth(Object,Depth),Time).
happens(enterWater(Diver),Time)->holds_at(vertical(Diver),Time).
fluent(standingOn(Diver,boat)).
event(standOn(Diver,boat)).
terminates(enterWater(Diver),standingOn(Diver,Boat),Time).
initiates(standOn(Diver,Boat),standingOn(Diver,Boat),Time).
fluent(positivelyBuoyant(Diver)).
fluent(neutrallyBuoyant(Diver)).
fluent(negativelyBuoyant(Diver)).
mutex(positivelyBuoyant).
mutex(neutrallyBuoyant).
mutex(negativelyBuoyant).
holds_at(positivelyBuoyant(Diver),Time)->holds_at(underWater(Diver),Time).
holds_at(neutrallyBuoyant(Diver),Time)->holds_at(underWater(Diver),Time).
holds_at(negativelyBuoyant(Diver),Time)->holds_at(underWater(Diver),Time).
event(pressDeflateButton(Diver,bc)).
event(pressDumpButton(Diver,bc)).
event(pressInflateButton(Diver,bc)).
happens(pressDeflateButton(Diver,Bc),Time)->holds_at(vertical(Diver),Time),holds_at(underWater(Bc),Time).
happens(pressDumpButton(Diver,Bc),Time)->holds_at(vertical(Diver),Time),holds_at(underWater(Bc),Time).
happens(pressDumpButton(Diver,Bc),Time)->holds_at(uncontrolledBuoyancy(Diver),Time).
holds_at(wearing(Diver,Bc),Time)->initiates(pressDeflateButton(Diver,Bc),negativelyBuoyant(Diver),Time).
holds_at(wearing(Diver,Bc),Time)->terminates(pressDeflateButton(Diver,Bc),neutrallyBuoyant(Diver),Time).
holds_at(wearing(Diver,Bc),Time)->terminates(pressDeflateButton(Diver,Bc),positivelyBuoyant(Diver),Time).
holds_at(wearing(Diver,Bc),Time)->initiates(pressDumpButton(Diver,Bc),negativelyBuoyant(Diver),Time).
holds_at(wearing(Diver,Bc),Time)->terminates(pressDumpButton(Diver,Bc),neutrallyBuoyant(Diver),Time).
holds_at(wearing(Diver,Bc),Time)->terminates(pressDumpButton(Diver,Bc),positivelyBuoyant(Diver),Time).
holds_at(wearing(Diver,Bc),Time)->initiates(pressInflateButton(Diver,Bc),neutrallyBuoyant(Diver),Time).
holds_at(wearing(Diver,Bc),Time)->terminates(pressInflateButton(Diver,Bc),positivelyBuoyant(Diver),Time).
holds_at(wearing(Diver,Bc),Time)->terminates(pressInflateButton(Diver,Bc),negativelyBuoyant(Diver),Time).
holds_at(wearing(Diver,Weight),Time)->initiates(takeOff(Diver,Weight),positivelyBuoyant(Diver),Time).
holds_at(wearing(Diver,Weight),Time)->terminates(takeOff(Diver,Weight),negativelyBuoyant(Diver),Time).
holds_at(wearing(Diver,Weight),Time)->terminates(takeOff(Diver,Weight),neutrallyBuoyant(Diver),Time).
fluent(uncontrolledBuoyancy(Diver)).
event(loseBuoyancyControl(Diver)).
predicate(isInexperiencedDiver(Diver)).
happens(loseBuoyancyControl(Diver),Time)->isInexperiencedDiver(Diver).
initiates(loseBuoyancyControl(Diver),uncontrolledBuoyancy(Diver),Time).
initiates(loseBuoyancyControl(Diver),positivelyBuoyant(Diver),Time).
terminates(loseBuoyancyControl(Diver),negativelyBuoyant(Diver),Time).
terminates(loseBuoyancyControl(Diver),neutrallyBuoyant(Diver),Time).
%; determining fluent

fluent(ascendDescendAmount(diver,depth)).
noninertial(ascendDescendAmount).
holds_at(ascendDescendAmount(Diver,Depth1),Time),holds_at(ascendDescendAmount(Diver,Depth2),Time)->Depth1=Depth2.
exists([Depth1],(happens(descend(Diver,Depth),Time)->holds_at(negativelyBuoyant(Diver),Time),holds_at(ascendDescendAmount(Diver,Depth1),Time),holds_at(atDepth(Diver,Depth-Depth1),Time))).
event(kickUp(Diver)).
exists([Depth1],(happens(ascend(Diver,Depth),Time)->(holds_at(positivelyBuoyant(Diver),Time);holds_at(neutrallyBuoyant(Diver),Time),happens(kickUp(Diver),Time)),holds_at(ascendDescendAmount(Diver,Depth1),Time),holds_at(atDepth(Diver,Depth+Depth1),Time))).
happens(kickUp(Diver),Time)->holds_at(vertical(Diver),Time).
event(swimAround(Diver)).
happens(swimAround(Diver),Time)->holds_at(horizontalDown(Diver),Time).
%; signaling

event(signalDescend(diver,diver)).
event(signalOutOfTime(diver,diver)).
event(signalAscend(diver,diver)).
%;[diver1,diver2,time]
%;Happens(SignalAscend(diver1,diver2),time) ->
%;Happens(SignalOutOfTime(diver1,diver2),time-1).
%;[diver1,diver2,time]
%;Happens(SignalDescend(diver1,diver2),time) ->
%;HoldsAt(See(diver1,diver2),time) &
%;HoldsAt(See(diver2,diver1),time).
%;[diver1,diver2,time]
%;Happens(SignalOutOfTime(diver1,diver2),time) ->
%;HoldsAt(See(diver1,diver2),time) &
%;HoldsAt(See(diver2,diver1),time).
%;[diver1,diver2,time]
%;Happens(SignalAscend(diver1,diver2),time) ->
%;HoldsAt(See(diver1,diver2),time) &
%;HoldsAt(See(diver2,diver1),time).
%;event LookAt(agent,object)
%;fluent See(agent,object)
%;[agent,object,time]
%;Initiates(LookAt(agent,object),See(agent,object),time).
%;[agent,object1,object2,time]
%;object1!=object2 ->
%;Terminates(LookAt(agent,object1),
%;           See(agent,object2),
%;           time).

event(descend1(diver)).
event(ascend1(diver)).
%;[diver,object,time]
%;Terminates(Descend1(diver),See(diver,object),time).
%;[diver,object,time]
%;Terminates(Ascend1(diver),See(diver,object),time).
%;[diver,object,time]
%;Terminates(RotateYaw(diver),See(diver,object),time).

event(rapidAscendToSurface(diver)).
exists([Depth],<->(happens(descend1(Diver),Time),happens(descend(Diver,Depth),Time))).
exists([Depth],<->(happens(ascend1(Diver),Time),happens(ascend(Diver,Depth),Time))).
happens(rapidAscendToSurface(Diver),Time)->happens(ascend(Diver,0),Time).
event(ascendLine(Diver,line)).
happens(ascendLine(Diver,Line),Time)->happens(ascend1(Diver),Time).
fluent(disoriented(Diver)).
event(becomeDisoriented(Diver)).
event(becomeReoriented(Diver)).
initiates(becomeDisoriented(Diver),disoriented(Diver),Time).
terminates(becomeReoriented(Diver),disoriented(Diver),Time).
fluent(disturbedSilt()).
event(disturbSilt(Diver)).
initiates(disturbSilt(Diver),disturbedSilt(),Time).
happens(becomeDisoriented(Diver),Time)->neg(holds_at(disturbedSilt(),Time-1)),holds_at(disturbedSilt(),Time).
event(panic(Diver)).
exists([Equipment],(happens(panic(Diver),Time)->holds_at(disoriented(Diver),Time);holds_at(uncontrolledBuoyancy(Diver),Time);happens(lose(Diver,Equipment),Time-1);happens(vomit(Diver),Time-1))).
event(vomit(Diver)).
%; conditions

fluent(unconscious(diver)).
event(goUnconscious(diver)).
event(regainConsciousness(diver)).
initiates(goUnconscious(Diver),unconscious(Diver),Time).
terminates(regainConsciousness(Diver),unconscious(Diver),Time).
happens(goUnconscious(Diver),Time)->happens(rapidAscendToSurface(Diver),Time).
fluent(hasEarPain(Diver)).
event(startEarPain(Diver)).
initiates(startEarPain(Diver),hasEarPain(Diver),Time).
fluent(hasRupturedEardrum(Diver)).
event(ruptureEardrum(Diver)).
initiates(ruptureEardrum(Diver),hasRupturedEardrum(Diver),Time).
fluent(conditionOK(Diver)).
fluent(hasDecompressionIllness(Diver)).
event(startDecompressionIllness(Diver)).
initiates(startDecompressionIllness(Diver),hasDecompressionIllness(Diver),Time).
fluent(signalingDecompress(computer,Diver)).
fluent(signalingLowOnAir(computer,airtank,Diver)).
holds_at(signalingLowOnAir(Computer,Airtank,Diver),Time)->holds_at(lowOnAir(Airtank),Time).
exists([Time1],(holds_at(signalingDecompress(Computer,Diver),Time)->neg(Time1<Time),happens(decompress(Diver),Time1))).
event(decompress(Diver)).
event(equalizeEars(Diver)).
(happens(descend1(Diver),Time);happens(ascend1(Diver),Time)),neg(happens(equalizeEars(Diver),Time))->happens(startEarPain(Diver),Time),happens(ruptureEardrum(Diver),Time).
happens(ascend1(Diver),Time),neg(happens(decompress(Diver),Time))->happens(startDecompressionIllness(Diver),Time).
holds_at(holding(Diver1,Diver2),Time),happens(ascend1(Diver1),Time),neg(happens(decompress(Diver2),Time))->happens(startDecompressionIllness(Diver2),Time).
exists([Depth],(happens(decompress(Diver),Time)->(Depth>0,holds_at(atDepth(Diver,Depth),Time)),neg(holds_at(uncontrolledBuoyancy(Diver),Time)))).
fluent(hasHeadache(Diver)).
holds_at(conditionOK(Diver),Time)->neg(holds_at(unconscious(Diver),Time)),neg(holds_at(hasEarPain(Diver),Time)),neg(holds_at(hasRupturedEardrum(Diver),Time)),neg(holds_at(hasDecompressionIllness(Diver),Time)),neg(holds_at(hasHeadache(Diver),Time)).
event(beAirlifted(Diver)).
event(takeInWater(Diver)).
fluent(lowOnAir(Airtank)).
event(becomeLowOnAir(Airtank)).
initiates(becomeLowOnAir(Airtank),lowOnAir(Airtank),Time).
%; initial state

holds_at(conditionOK(Diver),0).
holds_at(vertical(Diver),0).
neg(holds_at(disturbedSilt(),0)).
neg(holds_at(uncontrolledBuoyancy(Diver),0)).
neg(holds_at(disoriented(Diver),0)).
neg(holds_at(positivelyBuoyant(Diver),0)),neg(holds_at(neutrallyBuoyant(Diver),0)),neg(holds_at(negativelyBuoyant(Diver),0)).
neg(holds_at(wearing(Diver,Object),0)).
neg(holds_at(holding(Diver,Object),0)).
neg(holds_at(separated(Diver1,Diver2),0)).
%;[agent,object] !HoldsAt(See(agent,object),0).

fluent(separated(diver,diver)).
holds_at(separated(Diver1,Diver2),Time)->holds_at(separated(Diver2,Diver1),Time).
event(becomeSeparated(diver,diver)).
event(beReunitedWith(diver,diver)).
initiates(becomeSeparated(Diver1,Diver2),separated(Diver1,Diver2),Time).
initiates(becomeSeparated(Diver1,Diver2),separated(Diver2,Diver1),Time).
terminates(beReunitedWith(Diver1,Diver2),separated(Diver1,Diver2),Time).
terminates(beReunitedWith(Diver1,Diver2),separated(Diver2,Diver1),Time).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: ecnet/Dress.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%; Dress
%; (cf Sleep)
%;

event(putOn(agent,clothing)).
event(takeOff(agent,clothing)).
fluent(wearing(agent,clothing)).
initiates(putOn(Agent,Clothing),wearing(Agent,Clothing),Time).
happens(putOn(Agent,Clothing),Time)->neg(holds_at(wearing(Agent,Clothing),Time)),holds_at(at(Agent,Location),Time),holds_at(at(Clothing,Location),Time).
terminates(takeOff(Agent,Clothing),wearing(Agent,Clothing),Time).
happens(takeOff(Agent,Clothing),Time)->holds_at(wearing(Agent,Clothing),Time).
releases(putOn(Agent,Clothing),at(Clothing,Location),Time).
holds_at(wearing(Agent,Clothing),Time),holds_at(at(Agent,Location),Time)->holds_at(at(Clothing,Location),Time).
%;[agent,clothing,location1,location2,time]
%;HoldsAt(At(agent,location1),time) &
%;location1 != location2 ->
%;Terminates(TakeOff(agent,clothing),At(clothing,location2),time).

holds_at(at(Agent,Location),Time)->initiates(takeOff(Agent,Clothing),at(Clothing,Location),Time).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: ecnet/HungerNeed.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
%; hunger need
%;

fluent(hungry(agent)).
fluent(satiated(agent)).
noninertial(satiated).
<->(holds_at(hungry(Agent),Time),neg(holds_at(satiated(Agent),Time))).
event(eat(Agent,food)).
happens(eat(Agent,Food),Time)->holds_at(at(Agent,Location),Time),holds_at(at(Food,Location),Time).
terminates(eat(Agent,Food),hungry(Agent),Time).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: ecnet/Restaurant.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

% gleaned(subsort(restaurant,script)).

subsort(restaurant,script).
% gleaned(subsort(waiter,agent)).

subsort(waiter,agent).
% gleaned(subsort(cook,agent)).

subsort(cook,agent).
function(billOf(restaurant),bill).
function(cookOf(restaurant),cook).
function(tableOf(restaurant),table).
function(waiterOf(restaurant),waiter).
function(kitchenDoorOf(restaurant),door).
%; awaiting customer/waiter has set down bill on customer's table

fluent(beWaiter0(waiter)).
%; awaiting customer order

fluent(beWaiter1(waiter)).
%; has customer order

fluent(beWaiter2(waiter)).
%; in kitchen

fluent(beWaiter3(waiter)).
%; awaiting preparation of order

fluent(beWaiter4(waiter)).
%; has order

fluent(beWaiter5(waiter)).
%; back in dining room

fluent(beWaiter6(waiter)).
%; order delivered to customer (can ask if all is OK)

fluent(beWaiter7(waiter)).
%; customer has requested bill

fluent(beWaiter8(waiter)).
%; waiter is holding bill

fluent(beWaiter9(waiter)).
xor([beWaiter0,beWaiter1,beWaiter2,beWaiter3,beWaiter4,beWaiter5,beWaiter6,beWaiter7,beWaiter8,beWaiter9]).
holds_at(beWaiter0(Waiter),Time)->terminates(greet(Waiter,Agent),beWaiter0(Waiter),Time).
holds_at(beWaiter0(Waiter),Time)->initiates(greet(Waiter,Agent),beWaiter1(Waiter),Time).
holds_at(beWaiter1(Waiter),Time)->terminates(order(Agent,Waiter,Food),beWaiter1(Waiter),Time).
holds_at(beWaiter1(Waiter),Time)->initiates(order(Agent,Waiter,Food),beWaiter2(Waiter),Time).
waiterOf(Restaurant,Waiter),holds_at(beWaiter2(Waiter),Time)->happens(walkThroughDoor12(Waiter,kitchenDoorOf(Restaurant)),Time).
holds_at(beWaiter2(Waiter),Time),waiterOf(Restaurant,Waiter),kitchenDoorOf(Restaurant,Door)->terminates(walkThroughDoor12(Waiter,Door),beWaiter2(Waiter),Time).
holds_at(beWaiter2(Waiter),Time),waiterOf(Restaurant,Waiter),kitchenDoorOf(Restaurant,Door)->initiates(walkThroughDoor12(Waiter,Door),beWaiter3(Waiter),Time).
exists([Agent],(holds_at(beWaiter3(waiterOf(Restaurant)),Time),holds_at(knowOrder(waiterOf(Restaurant),Agent,Food),Time)->happens(order(waiterOf(Restaurant),cookOf(Restaurant),Food),Time))).
waiterOf(Restaurant,Waiter),cookOf(Restaurant,Cook),holds_at(beWaiter3(Waiter),Time)->terminates(order(Waiter,Cook,Food),beWaiter3(Waiter),Time).
waiterOf(Restaurant,Waiter),cookOf(Restaurant,Cook),holds_at(beWaiter3(Waiter),Time)->initiates(order(Waiter,Cook,Food),beWaiter4(Waiter),Time).
exists([Agent],(holds_at(beWaiter4(Waiter),Time),holds_at(knowOrder(Waiter,Agent,Food),Time),holds_at(foodPrepared(Food),Time)->happens(pickUp(Waiter,Food),Time))).
exists([Agent],(holds_at(beWaiter4(Waiter),Time),holds_at(knowOrder(Waiter,Agent,Food),Time)->terminates(pickUp(Waiter,Food),beWaiter4(Waiter),Time))).
exists([Agent],(holds_at(beWaiter4(Waiter),Time),holds_at(knowOrder(Waiter,Agent,Food),Time)->initiates(pickUp(Waiter,Food),beWaiter5(Waiter),Time))).
waiterOf(Restaurant,Waiter),holds_at(beWaiter5(Waiter),Time)->happens(walkThroughDoor21(Waiter,kitchenDoorOf(Restaurant)),Time).
holds_at(beWaiter5(Waiter),Time),waiterOf(Restaurant,Waiter),kitchenDoorOf(Restaurant,Door)->terminates(walkThroughDoor21(Waiter,Door),beWaiter5(Waiter),Time).
holds_at(beWaiter5(Waiter),Time),waiterOf(Restaurant,Waiter),kitchenDoorOf(Restaurant,Door)->initiates(walkThroughDoor21(Waiter,Door),beWaiter6(Waiter),Time).
waiterOf(Restaurant,Waiter),tableOf(Restaurant,Table),holds_at(beWaiter6(Waiter),Time),holds_at(holding(Waiter,Food),Time)->happens(placeOn(Waiter,Food,Table),Time).
holds_at(beWaiter6(Waiter),Time)->terminates(placeOn(Waiter,Food,Table),beWaiter6(Waiter),Time).
holds_at(beWaiter6(Waiter),Time)->initiates(placeOn(Waiter,Food,Table),beWaiter7(Waiter),Time).
holds_at(beWaiter7(Waiter),Time)->terminates(request(Agent,Waiter,Bill),beWaiter7(Waiter),Time).
holds_at(beWaiter7(Waiter),Time)->initiates(request(Agent,Waiter,Bill),beWaiter8(Waiter),Time).
waiterOf(Restaurant,Waiter),billOf(Restaurant,Bill),holds_at(beWaiter8(Waiter),Time)->happens(pickUp(Waiter,Bill),Time).
holds_at(beWaiter8(Waiter),Time)->terminates(pickUp(Waiter,Bill),beWaiter8(Waiter),Time).
holds_at(beWaiter8(Waiter),Time)->initiates(pickUp(Waiter,Bill),beWaiter9(Waiter),Time).
waiterOf(Restaurant,Waiter),billOf(Restaurant,Bill),tableOf(Restaurant,Table),holds_at(beWaiter9(Waiter),Time)->happens(placeOn(Waiter,Bill,Table),Time).
holds_at(beWaiter9(Waiter),Time)->terminates(placeOn(Waiter,Bill,Table),beWaiter9(Waiter),Time).
holds_at(beWaiter9(Waiter),Time)->initiates(placeOn(Waiter,Bill,Table),beWaiter0(Waiter),Time).
%; awaiting next waiter order

fluent(beCook0(cook)).
%; waiter order received

fluent(beCook1(cook)).
xor([beCook0,beCook1]).
holds_at(beCook0(Cook),Time)->terminates(order(Agent,Cook,Food),beCook0(Cook),Time).
holds_at(beCook0(Cook),Time)->initiates(order(Agent,Cook,Food),beCook1(Cook),Time).
event(foodPrepare(Agent,Food)).
fluent(foodPrepared(Food)).
initiates(foodPrepare(Agent,Food),foodPrepared(Food),Time).
happens(foodPrepare(Agent,Food),Time)->holds_at(at(Agent,Location),Time),holds_at(at(Food,Location),Time).
holds_at(beCook1(Cook),Time),holds_at(knowOrder(Cook,Agent,Food),Time)->happens(foodPrepare(Cook,Food),Time).
holds_at(beCook1(Cook),Time)->terminates(foodPrepare(Cook,Food),beCook1(Cook),Time).
holds_at(beCook1(Cook),Time)->initiates(foodPrepare(Cook,Food),beCook0(Cook),Time).


%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: ecnet/EatingInAHouse.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

option(modeldiff,on).
option(encoding,3).
option(renaming,off).
ignore([love,threatenedBy]).
ignore([lookOutOnto,floor,buildingOf,skyOf,groundOf]).
ignore(inside).
ignore([near,walkFrom,walkFromTo,runFromTo]).
ignore([billOf,cookOf,tableOf,waiterOf,kitchenDoorOf]).
ignore([beWaiter0,beWaiter1,beWaiter2,beWaiter3,beWaiter4]).
ignore([beWaiter5,beWaiter6,beWaiter7,beWaiter8,beWaiter9]).
ignore([beCook0,beCook1]).
ignore([inviteIn,invitedIn,intendToWalkIn,intentionToWalkIn]).
ignore([actOnIntentionToWalkIn,greet,sayGoodbye,cryForJoy]).
ignore([threaten,releaseFromThreat,threatenedBy]).
ignore([order,knowOrder,request,knowRequest]).
ignore([putInside,takeOutOf]).
ignore([sayPleaseToMeet,move]).
load('foundations/Root.e').
load('foundations/EC.e').
load('answers/Mueller2003/Ontology.e').
load('answers/Mueller2004c/RTSpaceM.e').
load('answers/Mueller2004c/OTSpaceM.e').
load('answers/Mueller2004c/HungerNeed.e').
load('answers/Mueller2004c/Restaurant.e').
load('answers/Mueller2003/Sleep.e').
load('answers/Mueller2003/SpeechAct.e').
load('answers/Mueller2004c/Dress.e').
room(upstairs1).
staircase(staircase1).
room(hallway1).
side1(staircase1,hallway1).
side2(staircase1,upstairs1).
door(diningRoomDoor1).
room(diningRoom1).
side1(diningRoomDoor1,hallway1).
side2(diningRoomDoor1,diningRoom1).
door(kitchenDoor1).
room(kitchen1).
side1(kitchenDoor1,diningRoom1).
side2(kitchenDoor1,kitchen1).
agent(eater1).
agent(eater2).
clothing(clothing1).
clothing(clothing2).
chair(chair1).
chair(chair2).
food(food1).
agent(cook1).
table(table1).
content(content1).
content(content2).
outside(dummyOutside1).
%; prune

sort([ona,onb]).
fluent(on(ona,onb)).
event(placeOn(agent,ona,onb)).
event(takeOffOf(agent,ona,onb)).
sort([ordera,orderb,orderc]).
event(order(ordera,orderb,orderc)).
fluent(knowOrder(orderb,ordera,orderc)).
sort([requesta,requestb,requestc]).
event(request(requesta,requestb,requestc)).
fluent(knowRequest(requestb,requesta,requestc)).
sort([holda,holdb,holdc]).
event(takeOffOf(holda,holdb,holdc)).
event(pickUp(holda,holdb)).
event(letGoOf(holda,holdb)).
event(hold(holda,holdb)).
fluent(holding(holda,holdb)).
sort([sita,sitb]).
event(lieOn(sita,sitb)).
event(sitOn(sita,sitb)).
event(riseFrom(sita,sitb)).
fluent(lyingOn(sita,sitb)).
fluent(sittingOn(sita,sitb)).
ona(food1).
onb(table1).
holda(cook1).
holdb(food1).
holdc(table1).
sita(eater1).
sitb(chair1).
%; initial situation

holds_at(dressed(Agent),0).
holds_at(awake(Agent),0).
holds_at(sleep3(Agent),0).
holds_at(standing(Agent),0).
holds_at(standing(Agent),0).
neg(holds_at(holding(Agent,Object),0)).
holds_at(at(Food,kitchen1),0).
neg(holds_at(foodPrepared(Food),0)).
holds_at(hungry(Agent),0).
holds_at(doorIsOpen(Door),0).
holds_at(at(Clothing,upstairs1),0).
holds_at(at(Chair,diningRoom1),0).
holds_at(at(cook1,kitchen1),0).
holds_at(at(table1,diningRoom1),0).
neg(holds_at(wearing(Agent,Clothing),0)).
%; narrative

holds_at(at(cook1,kitchen1),0).
holds_at(at(eater1,upstairs1),0).
holds_at(at(eater2,upstairs1),0).
happens(foodPrepare(cook1,food1),0).
happens(putOn(eater1,clothing1),1).
happens(putOn(eater2,clothing2),2).
happens(walkDownStaircase(eater1,staircase1),3).
happens(walkDownStaircase(eater2,staircase1),4).
happens(walkThroughDoor12(eater1,diningRoomDoor1),5).
happens(walkThroughDoor12(eater2,diningRoomDoor1),6).
happens(sitOn(eater1,chair1),7).
happens(sitOn(eater2,chair2),8).
happens(pickUp(cook1,food1),9).
happens(walkThroughDoor21(cook1,kitchenDoor1),10).
happens(placeOn(cook1,food1,table1),11).
happens(walkThroughDoor12(cook1,kitchenDoor1),12).
happens(eat(eater1,food1),13).
happens(eat(eater2,food1),14).
happens(converse(eater1,eater2),15).
happens(talkAbout(eater1,content1),16).
happens(talkAbout(eater2,content2),17).
happens(riseFrom(eater1,chair1),18).
happens(riseFrom(eater2,chair2),19).
range(time,0,20).
range(offset,0,0).
range(diameter,0,0).
completion(happens).
