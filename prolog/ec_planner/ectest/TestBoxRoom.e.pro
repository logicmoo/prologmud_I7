% ectest/TestBoxRoom.e:1
% % 
% % 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter10/MovingNewspaperAndBox.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ;
% ; Copyright (c) 2005 IBM Corporation and others.
% ; All rights reserved. This program and the accompanying materials
% ; are made available under the terms of the Common Public License v1.0
% ; which accompanies this distribution, and is available in
% ; http://www.eclipse.org/legal/cpl-v10.html
% ;
% ; Contributors:
% ; IBM - Initial implementation
% ;
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
 
% ectest/TestBoxRoom.e:24
% load foundations/Root.e
% ectest/TestBoxRoom.e:25
% load('foundations/Root.e').


% ectest/TestBoxRoom.e:25
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
% sort boolean
sort(boolean).


% sort integer
sort(integer).


% reified sort predicate
reified_sort(predicate).


% reified sort function
reified_sort(function).


% 
% ; End of file.
% load foundations/EC.e
% ectest/TestBoxRoom.e:26
% load('foundations/EC.e').


% foundations/EC.e:1
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
% ; Event Calculus (EC)
% ;
% ; @incollection{MillerShanahan:2002,
% ;   author = "Rob Miller and Murray Shanahan",
% ;   year = "2002",
% ;   title = "Some alternative formulations of the event calculus",
% ;   editor = "Antonis C. Kakas and Fariba Sadri",
% ;   booktitle = "Computational Logic: Logic Programming and Beyond: Essays in Honour of \uppercase{R}obert \uppercase{A}. \uppercase{K}owalski, Part \uppercase{II}",
% ;   series = "Lecture Notes in Computer Science",
% ;   volume = "2408",
% ;   pages = "452--490",
% ;   address = "Berlin",
% ;   publisher = "Springer",
% ; }
% ;
% foundations/EC.e:26
% 
% sort time: integer
subsort(time, integer).


% sort offset: integer
subsort(offset, integer).


% 
% reified sort fluent
reified_sort(fluent).


% reified sort event
reified_sort(event).


% foundations/EC.e:32
% 
% predicate Happens(event,time)
predicate(happens(event, time)).


% predicate HoldsAt(fluent,time)
predicate(holds_at(fluent, time)).


% predicate ReleasedAt(fluent,time)
predicate(releasedAt(fluent, time)).


% predicate Initiates(event,fluent,time)
predicate(initiates(event, fluent, time)).


% predicate Terminates(event,fluent,time)
predicate(terminates(event, fluent, time)).


% foundations/EC.e:38
% predicate Releases(event,fluent,time)
predicate(releases(event, fluent, time)).


% predicate Trajectory(fluent,time,fluent,offset)
predicate(trajectory(fluent, time, fluent, offset)).


% 
% ; End of file.
% % 
% sort object
sort(object).


% sort agent: object
subsort(agent, object).


% sort physobj: object
subsort(physobj, object).


% sort room: object
subsort(room, object).


% % 
% ectest/TestBoxRoom.e:32
% fluent directlyIn(object,object)fluent(directlyIn(object, object)).


% 
% fluent inRoom(object,room)fluent(inRoom(object, room)).


% 
% noninertial inRoom
noninertial(inRoom).


% % 
% ;; executable(move(agent,object,object,object))
% % 
% ectest/TestBoxRoom.e:38
% agent Lisa
t(agent, lisa).


% physobj Box, Newspaper
t(physobj, box).


t(physobj, newspaper).


% room Kitchen, LivingRoom
t(room, kitchen).


t(room, livingRoom).


% % 
% ; Sigma
% % 
% ; RS10
% ectest/TestBoxRoom.e:45
% [agent,physobj1,physobj2,room,time]% % 
% HoldsAt(directlyIn(agent,room),time) &% 
% HoldsAt(directlyIn(physobj1,room),time) &% 
% HoldsAt(inRoom(physobj2,room),time) ->% 
% Initiates(move(agent,physobj1,room,physobj2),directlyIn(physobj1,physobj2),time).
holds_at(directlyIn(Agent, Room), Time), holds_at(directlyIn(Physobj1, Room), Time), holds_at(inRoom(Physobj2, Room), Time) ->
	initiates(move(Agent, Physobj1, Room, Physobj2),
		  directlyIn(Physobj1, Physobj2),
		  Time).


% % 
% % 
% ; RS11
% ectest/TestBoxRoom.e:52
% [agent,physobj1,physobj2,room,time]% % 
% HoldsAt(directlyIn(agent,room),time) &% 
% HoldsAt(directlyIn(physobj1,room),time) &% 
% HoldsAt(inRoom(physobj2,room),time) ->% 
% Terminates(move(agent,physobj1,room,physobj2),directlyIn(physobj1,room),time).
holds_at(directlyIn(Agent, Room), Time), holds_at(directlyIn(Physobj1, Room), Time), holds_at(inRoom(Physobj2, Room), Time) ->
	terminates(move(Agent, Physobj1, Room, Physobj2),
		   directlyIn(Physobj1, Room),
		   Time).


% % 
% % 
% ; RS12
% ectest/TestBoxRoom.e:59
% [agent,physobj1,physobj2,room,time]% % 
% HoldsAt(directlyIn(agent,room),time) ->% 
% Initiates(move(agent,physobj1,physobj2,room),directlyIn(physobj1,room),time).
holds_at(directlyIn(Agent, Room), Time) ->
	initiates(move(Agent, Physobj1, Physobj2, Room),
		  directlyIn(Physobj1, Room),
		  Time).


% % 
% % 
% ; RS13
% ectest/TestBoxRoom.e:64
% [agent,physobj1,physobj2,room,time]% % 
% HoldsAt(directlyIn(agent,room),time) ->% 
% Terminates(move(agent,physobj1,physobj2,room),directlyIn(physobj1,physobj2),time).
holds_at(directlyIn(Agent, Room), Time) ->
	terminates(move(Agent, Physobj1, Physobj2, Room),
		   directlyIn(Physobj1, Physobj2),
		   Time).


% % 
% % 
% ; RS14
% ectest/TestBoxRoom.e:69
% [agent,room1,room2,time]% % 
% HoldsAt(directlyIn(agent,room1),time) ->% 
% Initiates(move(agent,agent,room1,room2),directlyIn(agent,room2),time).
holds_at(directlyIn(Agent, Room1), Time) ->
	initiates(move(Agent, Agent, Room1, Room2),
		  directlyIn(Agent, Room2),
		  Time).


% % 
% % 
% ; RS15
% ectest/TestBoxRoom.e:74
% [agent,room1,room2,time]% % 
% HoldsAt(directlyIn(agent,room1),time) ->% 
% Terminates(move(agent,agent,room1,room2),directlyIn(agent,room1),time).
holds_at(directlyIn(Agent, Room1), Time) ->
	terminates(move(Agent, Agent, Room1, Room2),
		   directlyIn(Agent, Room1),
		   Time).


% % 
% % 
% ; RS16
% ectest/TestBoxRoom.e:79
% [agent,physobj,room,time]% % 
% HoldsAt(directlyIn(agent,room),time) &% 
% HoldsAt(directlyIn(physobj,room),time) ->% 
% Initiates(move(agent,physobj,room,agent),directlyIn(physobj,agent),time).
holds_at(directlyIn(Agent, Room), Time), holds_at(directlyIn(Physobj, Room), Time) ->
	initiates(move(Agent, Physobj, Room, Agent),
		  directlyIn(Physobj, Agent),
		  Time).


% % 
% % 
% ; RS17
% ectest/TestBoxRoom.e:85
% [agent,physobj,room,time]% % 
% HoldsAt(directlyIn(agent,room),time) &% 
% HoldsAt(directlyIn(physobj,room),time) ->% 
% Terminates(move(agent,physobj,room,agent),directlyIn(physobj,room),time).
holds_at(directlyIn(Agent, Room), Time), holds_at(directlyIn(Physobj, Room), Time) ->
	terminates(move(Agent, Physobj, Room, Agent),
		   directlyIn(Physobj, Room),
		   Time).


% % 
% % 
% ; RS18
% ectest/TestBoxRoom.e:91
% [agent,physobj,room,time]% % 
% HoldsAt(directlyIn(physobj,agent),time) &% 
% HoldsAt(directlyIn(agent,room),time) ->% 
% Initiates(move(agent,physobj,agent,room),directlyIn(physobj,room),time).
holds_at(directlyIn(Physobj, Agent), Time), holds_at(directlyIn(Agent, Room), Time) ->
	initiates(move(Agent, Physobj, Agent, Room),
		  directlyIn(Physobj, Room),
		  Time).


% % 
% % 
% ; RS19
% ectest/TestBoxRoom.e:97
% [agent,physobj,room,time]% % 
% HoldsAt(directlyIn(physobj,agent),time) &% 
% HoldsAt(directlyIn(agent,room),time) ->% 
% Terminates(move(agent,physobj,agent,room),directlyIn(physobj,agent),time).
holds_at(directlyIn(Physobj, Agent), Time), holds_at(directlyIn(Agent, Room), Time) ->
	terminates(move(Agent, Physobj, Agent, Room),
		   directlyIn(Physobj, Agent),
		   Time).


% % 
% % 
% ; Delta
% ectest/TestBoxRoom.e:103
% % 
% Happens(move(Lisa,Newspaper,LivingRoom,Box),0).
happens(move(lisa, newspaper, livingRoom, box), 0).


% % 
% Happens(move(Lisa,Box,LivingRoom,Lisa),1).
happens(move(lisa, box, livingRoom, lisa), 1).


% % 
% Happens(move(Lisa,Lisa,LivingRoom,Kitchen),2).
happens(move(lisa, lisa, livingRoom, kitchen), 2).


% % 
% Happens(move(Lisa,Box,Lisa,Kitchen),3).
happens(move(lisa, box, lisa, kitchen), 3).


% % 
% Happens(move(Lisa,Lisa,Kitchen,LivingRoom),4).
happens(move(lisa, lisa, kitchen, livingRoom), 4).


% % 
% ectest/TestBoxRoom.e:109
% % 
% ; Psi
% % 
% ; RS1
% ectest/TestBoxRoom.e:113
% [object,time] % !HoldsAt(directlyIn(object,object),time).
not(holds_at(directlyIn(Object, Object), Time)).


% % 
% % 
% ; RS2
% ectest/TestBoxRoom.e:116
% [object1,object2,time]% % 
% HoldsAt(directlyIn(object1,object2),time) ->% 
% !HoldsAt(directlyIn(object2,object1),time).
holds_at(directlyIn(Object1, Object2), Time) ->
	not(holds_at(directlyIn(Object2, Object1), Time)).


% % 
% % 
% ; RS3
% ectest/TestBoxRoom.e:121
% [object1,object2,object3,time]% % 
% HoldsAt(directlyIn(object1,object2),time) &% 
% HoldsAt(directlyIn(object2,object3),time) ->% 
% !HoldsAt(directlyIn(object1,object3),time).
holds_at(directlyIn(Object1, Object2), Time), holds_at(directlyIn(Object2, Object3), Time) ->
	not(holds_at(directlyIn(Object1, Object3), Time)).


% % 
% % 
% ; RS4
% ectest/TestBoxRoom.e:127
% [object,object1,object2,time]% % 
% HoldsAt(directlyIn(object,object1),time) &% 
% HoldsAt(directlyIn(object,object2),time) ->% 
% object1=object2.
holds_at(directlyIn(Object, Object1), Time), holds_at(directlyIn(Object, Object2), Time) ->
	Object1=Object2.


% % 
% % 
% ; RS7
% ectest/TestBoxRoom.e:133
% [object,room,time]% % 
% HoldsAt(directlyIn(object,room),time) ->% 
% HoldsAt(inRoom(object,room),time).
holds_at(directlyIn(Object, Room), Time) ->
	holds_at(inRoom(Object, Room), Time).


% % 
% % 
% ; RS8
% ectest/TestBoxRoom.e:138
% [object1,object2,room,time]% % 
% HoldsAt(directlyIn(object1,object2),time) &% 
% HoldsAt(inRoom(object2,room),time) ->% 
% HoldsAt(inRoom(object1,room),time).
holds_at(directlyIn(Object1, Object2), Time), holds_at(inRoom(Object2, Room), Time) ->
	holds_at(inRoom(Object1, Room), Time).


% % 
% % 
% ; RS9
% ectest/TestBoxRoom.e:144
% [object,room1,room2,time]% % 
% HoldsAt(inRoom(object,room1),time) &% 
% HoldsAt(inRoom(object,room2),time) ->% 
% room1=room2.
holds_at(inRoom(Object, Room1), Time), holds_at(inRoom(Object, Room2), Time) ->
	Room1=Room2.


% % 
% % 
% ; Gamma
% ectest/TestBoxRoom.e:150
% % 
% HoldsAt(directlyIn(Lisa,LivingRoom),0).
holds_at(directlyIn(lisa, livingRoom), 0).


% % 
% HoldsAt(directlyIn(Newspaper,LivingRoom),0).
holds_at(directlyIn(newspaper, livingRoom), 0).


% % 
% HoldsAt(directlyIn(Box,LivingRoom),0).
holds_at(directlyIn(box, livingRoom), 0).


% % 
% % 
% ; added:                                                 
% ectest/TestBoxRoom.e:156
% [room1,room2,time] % !HoldsAt(inRoom(room1,room2),time).
not(holds_at(inRoom(Room1, Room2), Time)).


% % 
% ectest/TestBoxRoom.e:157
% [room,object,time] % !HoldsAt(directlyIn(room,object),time).
not(holds_at(directlyIn(Room, Object), Time)).


% % 
% % 
% ; entailed:
% ; HoldsAt(directlyIn(Lisa,LivingRoom),5).
% ; HoldsAt(directlyIn(Box,Kitchen),5).
% ; HoldsAt(inRoom(Newspaper,Kitchen),5).
% ectest/TestBoxRoom.e:163
% % 
% completion Happens
completion(happens).


% % 
% range time 0 5
range(time, 0, 5).


% range offset 1 1
range(offset, 1, 1).


% % 
% ; End of file.
% ectest/TestBoxRoom.e:170
% % 
% % 
