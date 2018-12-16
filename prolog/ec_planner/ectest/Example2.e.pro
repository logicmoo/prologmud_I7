% ectest/Example2.e:1
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
% ectest/Example2.e:11
% load foundations/Root.e
% ectest/Example2.e:12
'foundations/Root.e'.


% foundations/Root.e:1
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
% foundations/Root.e:11
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
% ectest/Example2.e:13
'foundations/EC.e'.


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
% 
% sort agent
sort(agent).


% 
% fluent Awake(agent)
fluent(awake(agent)).


% event WakeUp(agent)
event(wakeUp(agent)).


% 
% ectest/Example2.e:19
% [agent,time] % Initiates(WakeUp(agent),Awake(agent),time).
initiates(wakeUp(Agent), awake(Agent), Time).


% 
% 
% agent James
t(agent, james).


% !HoldsAt(Awake(James),0).
not(holds_at(awake(james), 0)).


% 
% HoldsAt(Awake(James),1).
holds_at(awake(james), 1).


% 
% 
% ectest/Example2.e:25
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


