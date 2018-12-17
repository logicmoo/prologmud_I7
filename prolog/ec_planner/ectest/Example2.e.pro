% ectest/Example2.e:1
% translate: begining  File: ectest/Example2.e.pro 
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
load('foundations/Root.e').

% load foundations/EC.e
load('foundations/EC.e').

% 
% sort agent
sort(agent).

% 
% fluent Awake(agent)
fluent(awake(agent)).

% ectest/Example2.e:17
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

% ectest/Example2.e:27
% translate: ending  File: ectest/Example2.e.pro 
