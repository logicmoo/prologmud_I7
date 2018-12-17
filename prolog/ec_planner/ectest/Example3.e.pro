% ectest/Example3.e:1
% translate: begining  File: ectest/Example3.e.pro 
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
% ectest/Example3.e:11
% load foundations/Root.e
% loading('foundations/Root.e').


% foundations/Root.e:1
% translate: unskipped  File: on_load_ele 
% foundations/Root.e:17
% translate: ready  File: on_load_ele 
% load foundations/EC.e
% loading('foundations/EC.e').


% ectest/Example3.e:13
% translate: unskipped  File: on_load_ele 
% ectest/Example3.e:13
% translate: ready  File: on_load_ele 
% 
% sort agent
sort(agent).


% 
% fluent Awake(agent)
fluent(awake(agent)).


% event WakeUp(agent)
event(wakeUp(agent)).


% 
% ectest/Example3.e:19
% [agent,time] % Initiates(WakeUp(agent),Awake(agent),time).
initiates(wakeUp(Agent), awake(Agent), Time).


% 
% 
% agent James, Jessie
t(agent, james).


t(agent, jessie).


% !HoldsAt(Awake(James),0).
not(holds_at(awake(james), 0)).


% 
% HoldsAt(Awake(James),1).
holds_at(awake(james), 1).


% 
% 
% ectest/Example3.e:25
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% ectest/Example3.e:27
% translate: ending  File: ectest/Example3.e.pro 
