% ectest/ec_reader_test_foundations.e:1
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: foundations/Root.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ectest/ec_reader_test_foundations.e:7
% 
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
% ectest/ec_reader_test_foundations.e:18
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
% ectest/ec_reader_test_foundations.e:24
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: foundations/EC.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ectest/ec_reader_test_foundations.e:30
% 
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
% ectest/ec_reader_test_foundations.e:56
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


% ectest/ec_reader_test_foundations.e:62
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


% ectest/ec_reader_test_foundations.e:68
% predicate Releases(event,fluent,time)
predicate(releases(event, fluent, time)).


% predicate Trajectory(fluent,time,fluent,offset)
predicate(trajectory(fluent, time, fluent, offset)).


% 
% ; End of file.
% 
% 
% ectest/ec_reader_test_foundations.e:74
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: foundations/DEC.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% 
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
% ; Discrete Event Calculus (DEC)
% ;
% ; @article{Mueller:2004a,
% ;   author = "Erik T. Mueller",
% ;   year = "2004",
% ;   title = "Event calculus reasoning through satisfiability",
% ;   journal = "Journal of Logic and Computation",
% ;   volume = "14",
% ;   number = "5",
% ;   pages = "703--730",
% ; }
% ;
% ectest/ec_reader_test_foundations.e:101
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


% ectest/ec_reader_test_foundations.e:107
% 
% predicate Happens(event,time)
predicate(happens(event, time)).


% predicate HoldsAt(fluent,time)
predicate(holds_at(fluent, time)).


% predicate ReleasedAt(fluent,time)
predicate(releasedAt(fluent, time)).


% 
% predicate Initiates(event,fluent,time)
predicate(initiates(event, fluent, time)).


% ectest/ec_reader_test_foundations.e:113
% predicate Terminates(event,fluent,time)
predicate(terminates(event, fluent, time)).


% predicate Releases(event,fluent,time)
predicate(releases(event, fluent, time)).


% 
% ectest/ec_reader_test_foundations.e:116
% [fluent,time]% 
% (HoldsAt(fluent,time) &
%  !ReleasedAt(fluent,time+1) &
%  !({event} Happens(event,time) & Terminates(event,fluent,time))) ->
% HoldsAt(fluent,time+1).
holds_at(Fluent, Time), not(releasedAt(Fluent, Time+1)), not(exists([Event],  (happens(Event, Time), terminates(Event, Fluent, Time)))) ->
	holds_at(Fluent, Time+1).


% 
% 
% ectest/ec_reader_test_foundations.e:122
% [fluent,time]% 
% (!HoldsAt(fluent,time) &
%  !ReleasedAt(fluent,time+1) &
%  !({event} Happens(event,time) & Initiates(event,fluent,time))) ->
% !HoldsAt(fluent,time+1).
not(holds_at(Fluent, Time)), not(releasedAt(Fluent, Time+1)), not(exists([Event],  (happens(Event, Time), initiates(Event, Fluent, Time)))) ->
	not(holds_at(Fluent, Time+1)).


% 
% 
% ectest/ec_reader_test_foundations.e:128
% [fluent,time]% 
% (!ReleasedAt(fluent,time) &
%  !({event} Happens(event,time) & Releases(event,fluent,time))) ->
% !ReleasedAt(fluent,time+1).
not(releasedAt(Fluent, Time)), not(exists([Event],  (happens(Event, Time), releases(Event, Fluent, Time)))) ->
	not(releasedAt(Fluent, Time+1)).


% 
% 
% ectest/ec_reader_test_foundations.e:133
% [fluent,time]% 
% (ReleasedAt(fluent,time) &
%  !({event} Happens(event,time) &
%    (Initiates(event,fluent,time) |
%     Terminates(event,fluent,time)))) ->
% ReleasedAt(fluent,time+1).
releasedAt(Fluent, Time), not(exists([Event],  (happens(Event, Time), (initiates(Event, Fluent, Time);terminates(Event, Fluent, Time))))) ->
	releasedAt(Fluent, Time+1).


% 
% ectest/ec_reader_test_foundations.e:139
% 
% ectest/ec_reader_test_foundations.e:140
% [event,fluent,time]% 
% (Happens(event,time) & Initiates(event,fluent,time)) ->
% (HoldsAt(fluent,time+1) & !ReleasedAt(fluent,time+1)).
happens(Event, Time), initiates(Event, Fluent, Time) ->
	holds_at(Fluent, Time+1),
	not(releasedAt(Fluent, Time+1)).


% 
% 
% ectest/ec_reader_test_foundations.e:144
% [event,fluent,time]% 
% (Happens(event,time) & Terminates(event,fluent,time)) ->
% (!HoldsAt(fluent,time+1) & !ReleasedAt(fluent,time+1)).
happens(Event, Time), terminates(Event, Fluent, Time) ->
	not(holds_at(Fluent, Time+1)),
	not(releasedAt(Fluent, Time+1)).


% 
% 
% ectest/ec_reader_test_foundations.e:148
% [event,fluent,time]% 
% (Happens(event,time) & Releases(event,fluent,time)) ->
% ReleasedAt(fluent,time+1).
happens(Event, Time), releases(Event, Fluent, Time) ->
	releasedAt(Fluent, Time+1).


% 
% 
% ; End of file.
% 
% ectest/ec_reader_test_foundations.e:154
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: foundations/ECCausal.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% 
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
% ; Causal Constraints
% ;
% ; @inproceedings{Shanahan:1999a,
% ;   author = "Murray Shanahan",
% ;   year = "1999",
% ;   title = "The ramification problem in the event calculus",
% ;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}ixteenth \uppercase{I}nternational \uppercase{J}oint \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
% ;   pages = "140--146",
% ;   address = "San Mateo, CA",
% ;   publisher = "Morgan Kaufmann",
% ; }
% ;
% ectest/ec_reader_test_foundations.e:182
% 
% predicate Started(fluent,time)
predicate(started(fluent, time)).


% predicate Stopped(fluent,time)
predicate(stopped(fluent, time)).


% 
% ectest/ec_reader_test_foundations.e:186
% [fluent,time]% 
% Started(fluent,time) <->
% (HoldsAt(fluent,time) |
%  ({event} Happens(event,time) & Initiates(event,fluent,time))).
started(Fluent, Time) <->
	(   holds_at(Fluent, Time)
	;   exists([Event],
		   (happens(Event, Time), initiates(Event, Fluent, Time)))
	).


% 
% 
% ectest/ec_reader_test_foundations.e:191
% [fluent,time]% 
% Stopped(fluent,time) <->
% (!HoldsAt(fluent,time) |
%  ({event} Happens(event,time) & Terminates(event,fluent,time))).
stopped(Fluent, Time) <->
	(   not(holds_at(Fluent, Time))
	;   exists([Event],
		   (happens(Event, Time), terminates(Event, Fluent, Time)))
	).


% 
% 
% predicate Initiated(fluent,time)
predicate(initiated(fluent, time)).


% ectest/ec_reader_test_foundations.e:197
% predicate Terminated(fluent,time)
predicate(terminated(fluent, time)).


% 
% ectest/ec_reader_test_foundations.e:199
% [fluent,time]% 
% Initiated(fluent,time) <->
% (Started(fluent,time) &
%  !({event} Happens(event,time) & Terminates(event,fluent,time))).
initiated(Fluent, Time) <->
	started(Fluent, Time),
	not(exists([Event],
		   (happens(Event, Time), terminates(Event, Fluent, Time)))).


% 
% 
% ectest/ec_reader_test_foundations.e:204
% [fluent,time]% 
% Terminated(fluent,time) <->
% (Stopped(fluent,time) &
%  !({event} Happens(event,time) & Initiates(event,fluent,time))).
terminated(Fluent, Time) <->
	stopped(Fluent, Time),
	not(exists([Event],
		   (happens(Event, Time), initiates(Event, Fluent, Time)))).


% 
% 
% ; End of file.
% ectest/ec_reader_test_foundations.e:210
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: foundations/ECTraj.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ectest/ec_reader_test_foundations.e:216
% 
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
% ectest/ec_reader_test_foundations.e:240
% 
% predicate Clipped(time,fluent,time)
predicate(clipped(time, fluent, time)).


% predicate Declipped(time,fluent,time)
predicate(declipped(time, fluent, time)).


% 
% predicate Trajectory(fluent,time,fluent,offset)
predicate(trajectory(fluent, time, fluent, offset)).


% predicate AntiTrajectory(fluent,time,fluent,offset)
predicate(antiTrajectory(fluent, time, fluent, offset)).


% ectest/ec_reader_test_foundations.e:246
% 
% ectest/ec_reader_test_foundations.e:247
% [event,fluent,fluent2,offset,time]% 
% Happens(event,time) &
% Initiates(event,fluent,time) &
% 0 < offset &
% Trajectory(fluent,time,fluent2,offset) &
% !Clipped(time,fluent,time+offset) ->
% HoldsAt(fluent2,time+offset).
happens(Event, Time), initiates(Event, Fluent, Time), 0<Offset, trajectory(Fluent, Time, Fluent2, Offset), not(clipped(Time, Fluent, Time+Offset)) ->
	holds_at(Fluent2, Time+Offset).


% ectest/ec_reader_test_foundations.e:253
% 
% 
% ectest/ec_reader_test_foundations.e:255
% [event,fluent,fluent2,offset,time]% 
% Happens(event,time) &
% Terminates(event,fluent,time) &
% 0 < offset &
% AntiTrajectory(fluent,time,fluent2,offset) &
% !Declipped(time,fluent,time+offset) ->
% HoldsAt(fluent2,time+offset).
happens(Event, Time), terminates(Event, Fluent, Time), 0<Offset, antiTrajectory(Fluent, Time, Fluent2, Offset), not(declipped(Time, Fluent, Time+Offset)) ->
	holds_at(Fluent2, Time+Offset).


% ectest/ec_reader_test_foundations.e:261
% 
% 
% ; End of file.
