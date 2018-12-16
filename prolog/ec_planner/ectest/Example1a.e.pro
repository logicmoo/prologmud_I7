% ectest/Example1a.e:1
% translate: begining  File: ectest/Example1a.e.pro 
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
% ; deduction
% ectest/Example1a.e:12
% 
% option timediff off
option(timediff, off).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% ectest/Example1a.e:18
% sort agent
sort(agent).


% 
% fluent Awake(agent)
fluent(awake(agent)).


% ;;   executable(wake_up(_X)).
% event WakeUp(agent)
event(wakeUp(agent)).


% 
% ;;   axiom(initiates(wake_up(X),awake(X),T),[]).
% ectest/Example1a.e:25
% [agent,time] % Initiates(WakeUp(agent),Awake(agent),time).
initiates(wakeUp(Agent), awake(Agent), Time).


% 
% 
% agent James
t(agent, james).


% ;; axiom(initially(neg(awake(nathan))),[]). 
% !HoldsAt(Awake(James),0).
not(holds_at(awake(james), 0)).


% 
% 
% ectest/Example1a.e:31
% Delta:
directive(delta).


 % Happens(WakeUp(James),0).
happens(wakeUp(james), 0).


% 
% 
% completion Delta Happens
completion([delta, happens]).


% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% ectest/Example1a.e:37
% 
% ;;   axiom(terminates(fall_asleep(X),awake(Y),T),[]). 
% ;;  
% ;;   abducible(dummy).
% ;; executable(fall_asleep(_X)).
% 
% ectest/Example1a.e:43
% translate: ending  File: ectest/Example1a.e.pro 
