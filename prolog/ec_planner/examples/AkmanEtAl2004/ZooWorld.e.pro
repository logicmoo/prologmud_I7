% examples/AkmanEtAl2004/ZooWorld.e:1
% ;
% ; Copyright (c) 2005 IBM Corporation and others.
% ; All rights reserved. This program and the accompanying materials
% ; are made available under the terms of the Common Public License v1.0
% ; which accompanies this distribution, and is available at
% ; http://www.eclipse.org/legal/cpl-v10.html
% ;
% ; Contributors:
% ; IBM - Initial implementation
% ;
% ; @article{Akman:2004,
% ;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
% ;   year = "2004",
% ;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
% ;   journal = "Artificial Intelligence",
% ;   volume = "153",
% ;   pages = "105--140",
% ; }
% ;
% examples/AkmanEtAl2004/ZooWorld.e:20
% 
% sort position: integer
subsort(position, integer).


% sort location
sort(location).


% sort cage: location
subsort(cage, location).


% sort gate
sort(gate).


% sort animal
sort(animal).


% examples/AkmanEtAl2004/ZooWorld.e:26
% sort elephant: animal
subsort(elephant, animal).


% sort horse: animal
subsort(horse, animal).


% sort dog: animal
subsort(dog, animal).


% sort human: animal
subsort(human, animal).


% sort species
sort(species).


% 
% examples/AkmanEtAl2004/ZooWorld.e:32
% function Loc(position): location
function(loc(position), location).


% function Side1(gate): position
function(side1(gate), position).


% function Side2(gate): position
function(side2(gate), position).


% function Species(animal): species
function(species(animal), species).


% 
% predicate Accessible(position,position,time)
predicate(accessible(position, position, time)).


% examples/AkmanEtAl2004/ZooWorld.e:38
% predicate Adult(animal)
predicate(adult(animal)).


% predicate Large(animal)
predicate(large(animal)).


% predicate LargeSpecies(species)
predicate(largeSpecies(species)).


% predicate Neighbor(position,position)
predicate(neighbor(position, position)).


% predicate Sides(position,position,gate)
predicate(sides(position, position, gate)).


% 
% examples/AkmanEtAl2004/ZooWorld.e:44
% event Close(human,gate)
event(close(human, gate)).


% event GetOff(human,animal)
event(getOff(human, animal)).


% event Mount(human,animal)
event(mount(human, animal)).


% event Move(animal,position)
event(move(animal, position)).


% event Open(human,gate)
event(open(human, gate)).


% event ThrowOff(animal,human)
event(throwOff(animal, human)).


% examples/AkmanEtAl2004/ZooWorld.e:50
% 
% fluent AbnormalEncroachment(human)
fluent(abnormalEncroachment(human)).


% noninertial AbnormalEncroachment
noninertial(abnormalEncroachment).


% fluent DoneBy(event,animal)
fluent(doneBy(event, animal)).


% noninertial DoneBy
noninertial(doneBy).


% fluent Mounted(human,animal)
fluent(mounted(human, animal)).


% examples/AkmanEtAl2004/ZooWorld.e:56
% fluent MountFails(human)
fluent(mountFails(human)).


% noninertial MountFails
noninertial(mountFails).


% fluent Moves(animal)
fluent(moves(animal)).


% noninertial Moves
noninertial(moves).


% fluent Opened(gate)
fluent(opened(gate)).


% fluent Pos(animal,position)
fluent(pos(animal, position)).


% examples/AkmanEtAl2004/ZooWorld.e:62
% fluent PosDeterminingFluent(human,position)
fluent(posDeterminingFluent(human, position)).


% noninertial PosDeterminingFluent
noninertial(posDeterminingFluent).


% fluent ThrowOffFails(animal,human)
fluent(throwOffFails(animal, human)).


% noninertial ThrowOffFails
noninertial(throwOffFails).


% 
% species HumanSpecies, ElephantSpecies, HorseSpecies, DogSpecies
t(species, humanSpecies).


t(species, elephantSpecies).


t(species, horseSpecies).


t(species, dogSpecies).


% examples/AkmanEtAl2004/ZooWorld.e:68
% location Outside
t(location, outside).


% 
% LargeSpecies(HumanSpecies).
largeSpecies(humanSpecies).


% 
% LargeSpecies(ElephantSpecies).
largeSpecies(elephantSpecies).


% 
% LargeSpecies(HorseSpecies).
largeSpecies(horseSpecies).


% 
% !LargeSpecies(DogSpecies).
not(largeSpecies(dogSpecies)).


% 
% examples/AkmanEtAl2004/ZooWorld.e:74
% 
% examples/AkmanEtAl2004/ZooWorld.e:75
% [event,animal,time]% 
% HoldsAt(DoneBy(event,animal),time) <->
% (Happens(event,time) &
%  (({gate} event=Close(animal,gate)) |
%   ({animal1} event=GetOff(animal,animal1))|
%   ({animal1} event=Mount(animal,animal1))|
%   ({position} event=Move(animal,position))|
%   ({gate} event=Open(animal,gate)) |
%   ({human1} event=ThrowOff(animal,human1)))).
holds_at(doneBy(Event, Animal), Time) <->
	happens(Event, Time),
	(   exists([Gate], Event=close(Animal, Gate))
	;   exists([Animal1],
		   Event=getOff(Animal, Animal1))
	;   exists([Animal15],
		   Event=mount(Animal, Animal15))
	;   exists([Position],
		   Event=move(Animal, Position))
	;   exists([Gate7], Event=open(Animal, Gate7))
	;   exists([Human1],
		   Event=throwOff(Animal, Human1))
	).


% examples/AkmanEtAl2004/ZooWorld.e:83
% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:85
% [event1,event2,animal,time]% 
% HoldsAt(DoneBy(event1,animal),time) &
% HoldsAt(DoneBy(event2,animal),time) ->
% event1=event2.
holds_at(doneBy(Event1, Animal), Time), holds_at(doneBy(Event2, Animal), Time) ->
	Event1=Event2.


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:90
% [animal] % Large(animal) <-> (Adult(animal) & LargeSpecies(Species(animal))).
large(Animal) <->
	adult(Animal),
	largeSpecies(species(Animal)).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:92
% [position] 
% examples/AkmanEtAl2004/ZooWorld.e:92
% {position1} % position1!=% position & Neighbor(position,position1).
exists([Position1],  (Position1\=Position, neighbor(Position, Position1))).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:94
% [position] % !Neighbor(position,position).
not(neighbor(Position, Position)).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:96
% [position1,position2]% 
% Neighbor(position1,position2) ->
% Neighbor(position2,position1).
neighbor(Position1, Position2) ->
	neighbor(Position2, Position1).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:100
% [cage] % cage!=% Outside.
Cage\=outside.


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:102
% [position1,position2,gate]% 
% Sides(position1,position2,gate) <->
% ((Side1(gate)=position1 &
%   Side2(gate)=position2) |
%  (Side2(gate)=position1 &
%   Side1(gate)=position2)).
sides(Position1, Position2, Gate) <->
	(   side1(Gate)=Position1,
	    side2(Gate)=Position2
	;   side2(Gate)=Position1,
	    side1(Gate)=Position2
	).


% 
% examples/AkmanEtAl2004/ZooWorld.e:108
% 
% examples/AkmanEtAl2004/ZooWorld.e:109
% [gate] % Loc(Side1(gate))!=Loc(Side2(gate)).
loc(side1(Gate))\=loc(side2(Gate)).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:111
% [position1,position2,gate1,gate2]% 
% Sides(position1,position2,gate1) &
% Sides(position1,position2,gate2) ->
% gate1=gate2.
sides(Position1, Position2, Gate1), sides(Position1, Position2, Gate2) ->
	Gate1=Gate2.


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:116
% [position1,position2,gate]% 
% Sides(position1,position2,gate) ->
% Neighbor(position1,position2).
sides(Position1, Position2, Gate) ->
	neighbor(Position1, Position2).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:120
% [position1,position2]% 
% Loc(position1) != Loc(position2) &
% Neighbor(position1,position2) ->
% examples/AkmanEtAl2004/ZooWorld.e:123
% {gate}%  Sides(position1,position2,gate).
exists([Gate],  (loc(Position1)\=loc(Position2), neighbor(Position1, Position2)->sides(Position1, Position2, Gate))).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:125
% [animal,position1,position2,time]% 
% HoldsAt(Pos(animal,position1),time) &
% HoldsAt(Pos(animal,position2),time) ->
% position1=position2.
holds_at(pos(Animal, Position1), Time), holds_at(pos(Animal, Position2), Time) ->
	Position1=Position2.


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:130
% [animal,time]% 
% examples/AkmanEtAl2004/ZooWorld.e:131
% {position} % HoldsAt(Pos(animal,position),time).
exists([Position], holds_at(pos(Animal, Position), Time)).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:133
% [animal1,animal2,position,time]% 
% (animal1!=animal2 &
%  Large(animal1) &
%  Large(animal2) &
%  HoldsAt(Pos(animal1,position),time) &
%  HoldsAt(Pos(animal2,position),time)) ->
% (({human} human=animal1 & HoldsAt(Mounted(human,animal2),time)) |
%  ({human} human=animal2 & HoldsAt(Mounted(human,animal1),time))).
Animal1\=Animal2, large(Animal1), large(Animal2), holds_at(pos(Animal1, Position), Time), holds_at(pos(Animal2, Position), Time) ->
	(   exists([Human],
		   (Human=Animal1, holds_at(mounted(Human, Animal2), Time)))
	;   exists([Human5],
		   (Human5=Animal2, holds_at(mounted(Human5, Animal1), Time)))
	).


% examples/AkmanEtAl2004/ZooWorld.e:140
% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:142
% [human,position1,position2,time]% 
% HoldsAt(PosDeterminingFluent(human,position1),time) &
% HoldsAt(PosDeterminingFluent(human,position2),time) ->
% position1=position2.
holds_at(posDeterminingFluent(Human, Position1), Time), holds_at(posDeterminingFluent(Human, Position2), Time) ->
	Position1=Position2.


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:147
% [animal,position,time]% 
% Initiates(Move(animal,position),Pos(animal,position),time).
initiates(move(Animal, Position), pos(Animal, Position), Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:150
% [animal,position1,position2,time]% 
% HoldsAt(Pos(animal,position1),time) ->
% Terminates(Move(animal,position2),Pos(animal,position1),time).
holds_at(pos(Animal, Position1), Time) ->
	terminates(move(Animal, Position2),
		   pos(Animal, Position1),
		   Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:154
% [animal,position,time]% 
% Happens(Move(animal,position),time) ->
% !HoldsAt(Pos(animal,position),time).
happens(move(Animal, Position), Time) ->
	not(holds_at(pos(Animal, Position), Time)).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:158
% [human,position,time]% 
% Happens(Move(human,position),time) ->
% !{animal} HoldsAt(Mounted(human,animal),time).
happens(move(Human, Position), Time) ->
	not(exists([Animal],
		   holds_at(mounted(Human, Animal), Time))).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:162
% [human,gate,time]% 
% Initiates(Open(human,gate),Opened(gate),time).
initiates(open(Human, Gate), opened(Gate), Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:165
% [human,gate,time]% 
% Happens(Open(human,gate),time) ->
% !HoldsAt(Opened(gate),time) &
% (!{animal} HoldsAt(Mounted(human,animal),time)) &
% ({position}
%  (Side1(gate)=position | Side2(gate)=position) &
%  HoldsAt(Pos(human,position),time)).
happens(open(Human, Gate), Time) ->
	not(holds_at(opened(Gate), Time)),
	not(exists([Animal],
		   holds_at(mounted(Human, Animal), Time))),
	exists([Position],
	       ((side1(Gate)=Position;side2(Gate)=Position), holds_at(pos(Human, Position), Time))).


% examples/AkmanEtAl2004/ZooWorld.e:171
% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:173
% [human,gate,time]% 
% Terminates(Close(human,gate),Opened(gate),time).
terminates(close(Human, Gate), opened(Gate), Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:176
% [human,gate,time]% 
% Happens(Close(human,gate),time) ->
% HoldsAt(Opened(gate),time) &
% (!{animal} HoldsAt(Mounted(human,animal),time)) &
% examples/AkmanEtAl2004/ZooWorld.e:180
% {position}% 
% (Side1(gate)=position | Side2(gate)=position) &
% HoldsAt(Pos(human,position),time).
exists([Position],  (happens(close(Human, Gate), Time)->holds_at(opened(Gate), Time), not(exists([Animal], holds_at(mounted(Human, Animal), Time))), (side1(Gate)=Position;side2(Gate)=Position), holds_at(pos(Human, Position), Time))).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:184
% [human,animal,position,time]% 
% HoldsAt(Mounted(human,animal),time) &
% HoldsAt(Pos(animal,position),time) ->
% HoldsAt(Pos(human,position),time).
holds_at(mounted(Human, Animal), Time), holds_at(pos(Animal, Position), Time) ->
	holds_at(pos(Human, Position), Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:189
% [animal,time]% 
% HoldsAt(Moves(animal),time) <->
% ({position}
%  HoldsAt(Pos(animal,position),time) &
%  !HoldsAt(Pos(animal,position),time+1)).
holds_at(moves(Animal), Time) <->
	exists([Position],
	       (holds_at(pos(Animal, Position), Time), not(holds_at(pos(Animal, Position), Time+1)))).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:195
% [human,time]% 
% HoldsAt(MountFails(human),time) <->
% ({animal}
%   Happens(Mount(human,animal),time) &
%   HoldsAt(Moves(animal),time)).
holds_at(mountFails(Human), Time) <->
	exists([Animal],
	       (happens(mount(Human, Animal), Time), holds_at(moves(Animal), Time))).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:201
% [human,animal,position,time]% 
% !HoldsAt(Moves(animal),time) ->
% Releases(Mount(human,animal),Pos(human,position),time).
not(holds_at(moves(Animal), Time)) ->
	releases(mount(Human, Animal),
		 pos(Human, Position),
		 Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:205
% [human,animal,time]% 
% !HoldsAt(Moves(animal),time) ->
% Initiates(Mount(human,animal),Mounted(human,animal),time).
not(holds_at(moves(Animal), Time)) ->
	initiates(mount(Human, Animal),
		  mounted(Human, Animal),
		  Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:209
% [human,animal,position,time]% 
% HoldsAt(Pos(animal,position),time) &
% HoldsAt(Moves(animal),time) ->
% Initiates(Mount(human,animal),Pos(human,position),time).
holds_at(pos(Animal, Position), Time), holds_at(moves(Animal), Time) ->
	initiates(mount(Human, Animal),
		  pos(Human, Position),
		  Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:214
% [human,animal,position,time]% 
% HoldsAt(Pos(human,position),time) &
% HoldsAt(Moves(animal),time) ->
% Terminates(Mount(human,animal),Pos(human,position),time).
holds_at(pos(Human, Position), Time), holds_at(moves(Animal), Time) ->
	terminates(mount(Human, Animal),
		   pos(Human, Position),
		   Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:219
% [human,animal,time]% 
% Happens(Mount(human,animal),time) ->
% Large(animal).
happens(mount(Human, Animal), Time) ->
	large(Animal).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:223
% [human,animal,time]% 
% HoldsAt(Mounted(human,animal),time) ->
% Large(animal).
holds_at(mounted(Human, Animal), Time) ->
	large(Animal).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:227
% [human1,human2,time]% 
% Happens(Mount(human1,human2),time) ->
% !Large(human1).
happens(mount(Human1, Human2), Time) ->
	not(large(Human1)).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:231
% [human1,human2,time]% 
% HoldsAt(Mounted(human1,human2),time) ->
% !Large(human1).
holds_at(mounted(Human1, Human2), Time) ->
	not(large(Human1)).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:235
% [human,animal,time]% 
% Happens(Mount(human,animal),time) ->
% !{human1} human1!=human & HoldsAt(Mounted(human1,animal),time).
happens(mount(Human, Animal), Time) ->
	not(exists([Human1],
		   (Human1\=Human, holds_at(mounted(Human1, Animal), Time)))).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:239
% [human1,human2,animal,time]% 
% HoldsAt(Mounted(human1,animal),time) &
% HoldsAt(Mounted(human2,animal),time) ->
% human1=human2.
holds_at(mounted(Human1, Animal), Time), holds_at(mounted(Human2, Animal), Time) ->
	Human1=Human2.


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:244
% [human,animal,time]% 
% Happens(Mount(human,animal),time) ->
% !{human1} human1!=human & HoldsAt(Mounted(human1,human),time).
happens(mount(Human, Animal), Time) ->
	not(exists([Human1],
		   (Human1\=Human, holds_at(mounted(Human1, Human), Time)))).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:248
% [human1,human2,time]% 
% Happens(Mount(human1,human2),time) ->
% examples/AkmanEtAl2004/ZooWorld.e:250
% {animal}%  HoldsAt(Mounted(human2,animal),time).
exists([Animal],  (happens(mount(Human1, Human2), Time)->holds_at(mounted(Human2, Animal), Time))).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:252
% [human1,human2,time]% 
% HoldsAt(Mounted(human1,human2),time) ->
% !{animal} HoldsAt(Mounted(human2,animal),time).
holds_at(mounted(Human1, Human2), Time) ->
	not(exists([Animal],
		   holds_at(mounted(Human2, Animal), Time))).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:256
% [human,animal,time]% 
% Happens(Mount(human,animal),time) ->
% !{animal1} HoldsAt(Mounted(human,animal1),time).
happens(mount(Human, Animal), Time) ->
	not(exists([Animal1],
		   holds_at(mounted(Human, Animal1), Time))).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:260
% [human,animal,time]% 
% !HoldsAt(Moves(animal),time) ->
% Terminates(GetOff(human,animal),Mounted(human,animal),time).
not(holds_at(moves(Animal), Time)) ->
	terminates(getOff(Human, Animal),
		   mounted(Human, Animal),
		   Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:264
% [human,animal,position,time]% 
% !HoldsAt(Moves(animal),time) &
% HoldsAt(PosDeterminingFluent(human,position),time) ->
% Initiates(GetOff(human,animal),Pos(human,position),time).
not(holds_at(moves(Animal), Time)), holds_at(posDeterminingFluent(Human, Position), Time) ->
	initiates(getOff(Human, Animal),
		  pos(Human, Position),
		  Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:269
% [human,animal,position,time]% 
% !HoldsAt(Moves(animal),time) &
% HoldsAt(Pos(human,position),time) ->
% Terminates(GetOff(human,animal),Pos(human,position),time).
not(holds_at(moves(Animal), Time)), holds_at(pos(Human, Position), Time) ->
	terminates(getOff(Human, Animal),
		   pos(Human, Position),
		   Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:274
% [human,animal,position1,position2,time]% 
% !HoldsAt(Moves(animal),time) &
% HoldsAt(Pos(human,position1),time) &
% position1!=position2 ->
% Terminates(GetOff(human,animal),Pos(human,position2),time).
not(holds_at(moves(Animal), Time)), holds_at(pos(Human, Position1), Time), Position1\=Position2 ->
	terminates(getOff(Human, Animal),
		   pos(Human, Position2),
		   Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:280
% [human,animal,time]% 
% Happens(GetOff(human,animal),time) ->
% HoldsAt(Mounted(human,animal),time).
happens(getOff(Human, Animal), Time) ->
	holds_at(mounted(Human, Animal), Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:284
% [animal1,human,time]% 
% HoldsAt(ThrowOffFails(animal1,human),time) <->
% ({position,animal2}
%  animal2!=human &
%  HoldsAt(PosDeterminingFluent(human,position),time) &
%  Large(animal2) &
%  HoldsAt(Pos(animal2,position),time+1)).
holds_at(throwOffFails(Animal1, Human), Time) <->
	exists([Position, Animal2],
	       (Animal2\=Human, holds_at(posDeterminingFluent(Human, Position), Time), large(Animal2), holds_at(pos(Animal2, Position), Time+1))).


% examples/AkmanEtAl2004/ZooWorld.e:290
% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:292
% [animal,human,position,time]% 
% HoldsAt(PosDeterminingFluent(human,position),time) &
% !HoldsAt(ThrowOffFails(animal,human),time) ->
% Initiates(ThrowOff(animal,human),Pos(human,position),time).
holds_at(posDeterminingFluent(Human, Position), Time), not(holds_at(throwOffFails(Animal, Human), Time)) ->
	initiates(throwOff(Animal, Human),
		  pos(Human, Position),
		  Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:297
% [animal,human,position,time]% 
% HoldsAt(Pos(human,position),time) &
% !HoldsAt(ThrowOffFails(animal,human),time) ->
% Terminates(ThrowOff(animal,human),Pos(human,position),time).
holds_at(pos(Human, Position), Time), not(holds_at(throwOffFails(Animal, Human), Time)) ->
	terminates(throwOff(Animal, Human),
		   pos(Human, Position),
		   Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:302
% [animal,human,position1,position2,time]% 
% !HoldsAt(ThrowOffFails(animal,human),time) &
% HoldsAt(Pos(human,position1),time) &
% !HoldsAt(PosDeterminingFluent(human,position2),time) &
% position1!=position2 ->
% Terminates(ThrowOff(animal,human),Pos(human,position2),time).
not(holds_at(throwOffFails(Animal, Human), Time)), holds_at(pos(Human, Position1), Time), not(holds_at(posDeterminingFluent(Human, Position2), Time)), Position1\=Position2 ->
	terminates(throwOff(Animal, Human),
		   pos(Human, Position2),
		   Time).


% 
% examples/AkmanEtAl2004/ZooWorld.e:308
% 
% examples/AkmanEtAl2004/ZooWorld.e:309
% [human,time]% 
% (!{animal} Happens(ThrowOff(animal,human),time) |
%            Happens(GetOff(human,animal),time)) ->
% HoldsAt(PosDeterminingFluent(human,1),time).
not(exists([Animal], happens(throwOff(Animal, Human), Time)));happens(getOff(Human, animal), Time) ->
	holds_at(posDeterminingFluent(Human, 1), Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:314
% [human,position,animal1,animal2,time]% 
% HoldsAt(PosDeterminingFluent(human,position),time) &
% HoldsAt(ThrowOffFails(animal1,human),time) &
% HoldsAt(Pos(animal2,position),time) ->
% Initiates(ThrowOff(animal1,human),Mounted(human,animal2),time).
holds_at(posDeterminingFluent(Human, Position), Time), holds_at(throwOffFails(Animal1, Human), Time), holds_at(pos(Animal2, Position), Time) ->
	initiates(throwOff(Animal1, Human),
		  mounted(Human, Animal2),
		  Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:320
% [human,animal,time]% 
% !HoldsAt(ThrowOffFails(animal,human),time) ->
% Terminates(ThrowOff(animal,human),Mounted(human,animal),time).
not(holds_at(throwOffFails(Animal, Human), Time)) ->
	terminates(throwOff(Animal, Human),
		   mounted(Human, Animal),
		   Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:324
% [animal,human,time]% 
% Happens(ThrowOff(animal,human),time) ->
% HoldsAt(Mounted(human,animal),time).
happens(throwOff(Animal, Human), Time) ->
	holds_at(mounted(Human, Animal), Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:328
% [animal,human,time]% 
% Happens(ThrowOff(animal,human),time) ->
% !Happens(GetOff(human,animal),time).
happens(throwOff(Animal, Human), Time) ->
	not(happens(getOff(Human, Animal), Time)).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:332
% [animal,human,time]% 
% Happens(GetOff(human,animal),time) ->
% !Happens(ThrowOff(animal,human),time).
happens(getOff(Human, Animal), Time) ->
	not(happens(throwOff(Animal, Human), Time)).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:336
% [position1,position2,time]% 
% Accessible(position1,position2,time) <->
% (Neighbor(position1,position2) &
%  !{gate} Sides(position1,position2,gate) &
%          !HoldsAt(Opened(gate),time)).
accessible(Position1, Position2, Time) <->
	thereExists((neighbor(Position1, Position2), not([gate])),
		    (sides(Position1, Position2, gate), not(holds_at(opened(gate), Time)))).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:342
% [animal,position1,position2,time]% 
% (position1!=position2 &
%  HoldsAt(Pos(animal,position1),time) &
%  HoldsAt(Pos(animal,position2),time+1)) ->
% Accessible(position1,position2,time).
Position1\=Position2, holds_at(pos(Animal, Position1), Time), holds_at(pos(Animal, Position2), Time+1) ->
	accessible(Position1, Position2, Time).


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:348
% [human,time]% 
% HoldsAt(AbnormalEncroachment(human),time) <->
% (HoldsAt(MountFails(human),time) |
%  ({position,animal1,animal2}
%    HoldsAt(PosDeterminingFluent(human,position),time) &
%    !HoldsAt(ThrowOffFails(animal2,human),time) &
%    Happens(ThrowOff(animal2,human),time) &
%    animal1!=human &
%    Large(animal1) &
%    HoldsAt(Pos(animal1,position),time) &
%    !HoldsAt(Pos(animal1,position),time+1))).
holds_at(abnormalEncroachment(Human), Time) <->
	(   holds_at(mountFails(Human), Time)
	;   exists([Position, Animal1, Animal2],
		   (holds_at(posDeterminingFluent(Human, Position), Time), not(holds_at(throwOffFails(Animal2, Human), Time)), happens(throwOff(Animal2, Human), Time), Animal1\=Human, large(Animal1), holds_at(pos(Animal1, Position), Time), not(holds_at(pos(Animal1, Position), Time+1))))
	).


% examples/AkmanEtAl2004/ZooWorld.e:358
% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:360
% [animal1,animal2,position,time]% 
% HoldsAt(Pos(animal1,position),time) &
% !HoldsAt(Pos(animal1,position),time+1) &
% !HoldsAt(Pos(animal2,position),time) &
% HoldsAt(Pos(animal2,position),time+1) ->
% (!Large(animal1) |
%  !Large(animal2) |
%  ({human} human=animal2 & HoldsAt(AbnormalEncroachment(human),time))).
holds_at(pos(Animal1, Position), Time), not(holds_at(pos(Animal1, Position), Time+1)), not(holds_at(pos(Animal2, Position), Time)), holds_at(pos(Animal2, Position), Time+1) ->
	(   not(large(Animal1))
	;   not(large(Animal2))
	;   exists([Human],
		   (Human=Animal2, holds_at(abnormalEncroachment(Human), Time)))
	).


% examples/AkmanEtAl2004/ZooWorld.e:367
% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:369
% [animal1,animal2,position1,position2,time]% 
% animal1!=% animal2 &
% Large(animal1) & Large(animal2) &
% HoldsAt(Pos(animal1,position1),time) &
% HoldsAt(Pos(animal1,position2),time+1) &
% HoldsAt(Pos(animal2,position1),time) &
% HoldsAt(Pos(animal2,position2),time+1) ->
% !{gate} Sides(position1,position2,gate).
Animal1\=Animal2, large(Animal1), large(Animal2), holds_at(pos(Animal1, Position1), Time), holds_at(pos(Animal1, Position2), Time+1), holds_at(pos(Animal2, Position1), Time), holds_at(pos(Animal2, Position2), Time+1) ->
	not(exists([Gate],
		   sides(Position1, Position2, Gate))).


% examples/AkmanEtAl2004/ZooWorld.e:376
% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:378
% [animal1,animal2,position1,position2,time]% 
% animal1!=% animal2 &
% Large(animal1) & Large(animal2) &
% HoldsAt(Pos(animal1,position1),time) &
% HoldsAt(Pos(animal1,position2),time+1) &
% HoldsAt(Pos(animal2,position2),time) &
% HoldsAt(Pos(animal2,position1),time+1) ->
% !{gate} Sides(position1,position2,gate).
Animal1\=Animal2, large(Animal1), large(Animal2), holds_at(pos(Animal1, Position1), Time), holds_at(pos(Animal1, Position2), Time+1), holds_at(pos(Animal2, Position2), Time), holds_at(pos(Animal2, Position1), Time+1) ->
	not(exists([Gate],
		   sides(Position1, Position2, Gate))).


% examples/AkmanEtAl2004/ZooWorld.e:385
% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:387
% [gate,position1,position2,time]% 
% HoldsAt(Opened(gate),time) &
% !HoldsAt(Opened(gate),time+1) &
% Sides(position1,position2,gate) ->
% !{animal}
% HoldsAt(Pos(animal,position1),time) &
% HoldsAt(Pos(animal,position2),time+1).
holds_at(opened(Gate), Time), not(holds_at(opened(Gate), Time+1)), sides(Position1, Position2, Gate) ->
	not(exists([Animal],
		   (holds_at(pos(Animal, Position1), Time), holds_at(pos(Animal, Position2), Time+1)))).


% examples/AkmanEtAl2004/ZooWorld.e:393
% 
% 
% gate GateAO
t(gate, gateAO).


% cage CageA
t(cage, cageA).


% 
% Loc(1)=CageA.
loc(1)=cageA.


% 
% examples/AkmanEtAl2004/ZooWorld.e:399
% Loc(2)=CageA.
loc(2)=cageA.


% 
% Loc(3)=CageA.
loc(3)=cageA.


% 
% Loc(4)=CageA.
loc(4)=cageA.


% 
% Loc(5)=Outside.
loc(5)=outside.


% 
% Loc(6)=Outside.
loc(6)=outside.


% 
% Loc(7)=Outside.
loc(7)=outside.


% 
% examples/AkmanEtAl2004/ZooWorld.e:405
% Loc(8)=Outside.
loc(8)=outside.


% 
% 
% examples/AkmanEtAl2004/ZooWorld.e:407
% [position1,position2]% 
% Neighbor(position1,position2) <->
% ((position1=1 & position2=2) |
%  (position1=1 & position2=3) |
%  (position1=1 & position2=4) |
%  (position1=2 & position2=3) |
%  (position1=2 & position2=4) |
%  (position1=3 & position2=4) |
%  (position1=5 & position2=6) |
%  (position1=5 & position2=7) |
%  (position1=5 & position2=8) |
%  (position1=6 & position2=7) |
%  (position1=6 & position2=8) |
%  (position1=7 & position2=8) |
%  (position2=1 & position1=2) |
%  (position2=1 & position1=3) |
%  (position2=1 & position1=4) |
%  (position2=2 & position1=3) |
%  (position2=2 & position1=4) |
%  (position2=3 & position1=4) |
%  (position2=5 & position1=6) |
%  (position2=5 & position1=7) |
%  (position2=5 & position1=8) |
%  (position2=6 & position1=7) |
%  (position2=6 & position1=8) |
%  (position2=7 & position1=8) |
%  (position1=4 & position2=7) |
%  (position2=4 & position1=7)).
neighbor(Position1, Position2) <->
	(   Position1=1,
	    Position2=2
	;   Position1=1,
	    Position2=3
	;   Position1=1,
	    Position2=4
	;   Position1=2,
	    Position2=3
	;   Position1=2,
	    Position2=4
	;   Position1=3,
	    Position2=4
	;   Position1=5,
	    Position2=6
	;   Position1=5,
	    Position2=7
	;   Position1=5,
	    Position2=8
	;   Position1=6,
	    Position2=7
	;   Position1=6,
	    Position2=8
	;   Position1=7,
	    Position2=8
	;   Position2=1,
	    Position1=2
	;   Position2=1,
	    Position1=3
	;   Position2=1,
	    Position1=4
	;   Position2=2,
	    Position1=3
	;   Position2=2,
	    Position1=4
	;   Position2=3,
	    Position1=4
	;   Position2=5,
	    Position1=6
	;   Position2=5,
	    Position1=7
	;   Position2=5,
	    Position1=8
	;   Position2=6,
	    Position1=7
	;   Position2=6,
	    Position1=8
	;   Position2=7,
	    Position1=8
	;   Position1=4,
	    Position2=7
	;   Position2=4,
	    Position1=7
	).


% examples/AkmanEtAl2004/ZooWorld.e:434
% 
% 
% Side1(GateAO)=4.
side1(gateAO)=4.


% 
% Side2(GateAO)=7.
side2(gateAO)=7.


% 
% 
% ; End of file.
