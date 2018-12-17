% ectest/Example1.e:1
% translate: begining  File: ectest/Example1.e.pro 
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
% ectest/Example1.e:11
% 
% ; deduction
% 
% load foundations/Root.e
% loading('foundations/Root.e').


% foundations/Root.e:1
% translate: unskipped  File: on_load_ele 
% foundations/Root.e:17
% translate: ready  File: on_load_ele 
% load foundations/EC.e
% loading('foundations/EC.e').


% foundations/EC.e:1
% translate: unskipped  File: on_load_ele 
% foundations/EC.e:42
% translate: ready  File: on_load_ele 
% 
% ectest/Example1.e:17
% sort agent
sort(agent).


% 
% fluent Awake(agent)
fluent(awake(agent)).


% event WakeUp(agent)
event(wakeUp(agent)).


% 
% ectest/Example1.e:22
% [agent,time] % Initiates(WakeUp(agent),Awake(agent),time).
initiates(wakeUp(Agent), awake(Agent), Time).


% 
% 
% agent James
t(agent, james).


% !HoldsAt(Awake(James),0).
not(holds_at(awake(james), 0)).


% 
% Delta:
directive(delta).


 % Happens(WakeUp(James),0).
happens(wakeUp(james), 0).


% 
% 
% ectest/Example1.e:28
% completion Delta Happens
completion([delta, happens]).


% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% ectest/Example1.e:32
% translate: ending  File: ectest/Example1.e.pro 
