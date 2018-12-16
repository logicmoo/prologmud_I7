% ectest/Story1.e:1
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
% ; @article{FrankEtAl:2003,
% ;   author = "Stefan L. Frank and Mathieu Koppen and Leo G. M. Noordman and Wietske Vonk",
% ;   year = "2003",
% ;   title = "Modeling knowledge-based inferences in story comprehension",
% ;   journal = "Cognitive Science",
% ;   volume = "27",
% ;   pages = "875--910",
% ; }
% ;
% ectest/Story1.e:20
% 
% option modeldiff on
option(modeldiff, on).


% 
% load foundations/Root.e
% ectest/Story1.e:24
'foundations/Root.e'.


% ectest/Story1.e:24
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
% ectest/Story1.e:25
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
% load examples/FrankEtAl2003/FrankEtAl.e
% ectest/Story1.e:29
'examples/FrankEtAl2003/FrankEtAl.e'.


% examples/FrankEtAl2003/FrankEtAl.e:1
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
% ; @article{FrankEtAl:2003,
% ;   author = "Stefan L. Frank and Mathieu Koppen and Leo G. M. Noordman and Wietske Vonk",
% ;   year = "2003",
% ;   title = "Modeling knowledge-based inferences in story comprehension",
% ;   journal = "Cognitive Science",
% ;   volume = "27",
% ;   pages = "875--910",
% ; }
% ;
% examples/FrankEtAl2003/FrankEtAl.e:20
% 
% fluent SunShining()
fluent(sunShining()).


% fluent Raining()
fluent(raining()).


% fluent Outside(agent)
fluent(outside(agent)).


% fluent PlaySoccer(agent)
fluent(playSoccer(agent)).


% fluent PlayHideAndSeek(agent)
fluent(playHideAndSeek(agent)).


% examples/FrankEtAl2003/FrankEtAl.e:26
% fluent PlayComputerGame(agent)
fluent(playComputerGame(agent)).


% fluent PlayWithDog(agent)
fluent(playWithDog(agent)).


% fluent Win(agent)
fluent(win(agent)).


% 
% noninertial Outside, PlaySoccer, PlayHideAndSeek, PlayComputerGame
noninertial([outside, playSoccer, playHideAndSeek, playComputerGame]).


% noninertial PlayWithDog, Win
noninertial([playWithDog, win]).


% examples/FrankEtAl2003/FrankEtAl.e:32
% 
% xor PlaySoccer, PlayHideAndSeek, PlayComputerGame, PlayWithDog
xor([playSoccer, playHideAndSeek, playComputerGame, playWithDog]).


% 
% examples/FrankEtAl2003/FrankEtAl.e:35
% [agent,time]% 
% HoldsAt(PlaySoccer(agent),time) ->
% HoldsAt(Outside(agent),time).
holds_at(playSoccer(Agent), Time) ->
	holds_at(outside(Agent), Time).


% 
% 
% examples/FrankEtAl2003/FrankEtAl.e:39
% [agent,time]% 
% HoldsAt(PlaySoccer(agent),time) ->
% ({agent1} agent1!=agent & HoldsAt(PlaySoccer(agent1),time)).
holds_at(playSoccer(Agent), Time) ->
	exists([Agent1],
	       (Agent1\=Agent, holds_at(playSoccer(Agent1), Time))).


% 
% 
% examples/FrankEtAl2003/FrankEtAl.e:43
% [agent,time]% 
% HoldsAt(PlayHideAndSeek(agent),time) ->
% ({agent1} agent1!=agent & HoldsAt(PlayHideAndSeek(agent1),time)).
holds_at(playHideAndSeek(Agent), Time) ->
	exists([Agent1],
	       (Agent1\=Agent, holds_at(playHideAndSeek(Agent1), Time))).


% 
% 
% examples/FrankEtAl2003/FrankEtAl.e:47
% [agent,time]% 
% HoldsAt(PlayComputerGame(agent),time) ->
% !HoldsAt(Outside(agent),time).
holds_at(playComputerGame(Agent), Time) ->
	not(holds_at(outside(Agent), Time)).


% 
% 
% examples/FrankEtAl2003/FrankEtAl.e:51
% [agent,time]% 
% HoldsAt(Win(agent),time) ->
% (HoldsAt(PlaySoccer(agent),time) |
%  HoldsAt(PlayHideAndSeek(agent),time) |
%  (HoldsAt(PlayComputerGame(agent),time) &
%   ({agent1} agent1!=agent & HoldsAt(PlayComputerGame(agent1),time)))).
holds_at(win(Agent), Time) ->
	(   holds_at(playSoccer(Agent), Time)
	;   holds_at(playHideAndSeek(Agent), Time)
	;   holds_at(playComputerGame(Agent), Time),
	    exists([Agent1],
		   (Agent1\=Agent, holds_at(playComputerGame(Agent1), Time)))
	).


% 
% examples/FrankEtAl2003/FrankEtAl.e:57
% 
% examples/FrankEtAl2003/FrankEtAl.e:58
% [agent,time]% 
% HoldsAt(PlaySoccer(agent),time) &
% HoldsAt(Win(agent),time) ->
% !HoldsAt(PlaySoccer(agent),time+1).
holds_at(playSoccer(Agent), Time), holds_at(win(Agent), Time) ->
	not(holds_at(playSoccer(Agent), Time+1)).


% 
% 
% examples/FrankEtAl2003/FrankEtAl.e:63
% [agent,time]% 
% HoldsAt(PlayHideAndSeek(agent),time) &
% HoldsAt(Win(agent),time) ->
% !HoldsAt(PlayHideAndSeek(agent),time+1).
holds_at(playHideAndSeek(Agent), Time), holds_at(win(Agent), Time) ->
	not(holds_at(playHideAndSeek(Agent), Time+1)).


% 
% 
% examples/FrankEtAl2003/FrankEtAl.e:68
% [agent,time]% 
% HoldsAt(PlayComputerGame(agent),time) &
% HoldsAt(Win(agent),time) ->
% !HoldsAt(PlayComputerGame(agent),time+1).
holds_at(playComputerGame(Agent), Time), holds_at(win(Agent), Time) ->
	not(holds_at(playComputerGame(Agent), Time+1)).


% 
% 
% examples/FrankEtAl2003/FrankEtAl.e:73
% [agent,time]% 
% HoldsAt(Win(agent),time) ->
% HoldsAt(PlaySoccer(agent),time-1) |
% HoldsAt(PlayHideAndSeek(agent),time-1) |
% HoldsAt(PlayComputerGame(agent),time-1).
(   ( holds_at(win(Agent), Time)->holds_at(playSoccer(Agent), Time-1)
    )
;   holds_at(playHideAndSeek(Agent), Time-1)
;   holds_at(playComputerGame(Agent), Time-1)
).


% 
% 
% examples/FrankEtAl2003/FrankEtAl.e:79
% [agent,time]% 
% HoldsAt(PlaySoccer(agent),time) ->
% !HoldsAt(Raining(),time).
holds_at(playSoccer(Agent), Time) ->
	not(holds_at(raining(), Time)).


% 
% 
% examples/FrankEtAl2003/FrankEtAl.e:83
% [agent,time]% 
% HoldsAt(Win(agent),time) ->
% !({agent1} agent1!=agent & HoldsAt(Win(agent1),time)).
holds_at(win(Agent), Time) ->
	not(exists([Agent1],
		   (Agent1\=Agent, holds_at(win(Agent1), Time)))).


% 
% 
% examples/FrankEtAl2003/FrankEtAl.e:87
% [agent1,agent2,time]% 
% HoldsAt(PlayHideAndSeek(agent1),time) &
% HoldsAt(PlayHideAndSeek(agent2),time) ->
% ((HoldsAt(Outside(agent1),time) & HoldsAt(Outside(agent2),time)) |
%  (!HoldsAt(Outside(agent1),time) & !HoldsAt(Outside(agent2),time))).
holds_at(playHideAndSeek(Agent1), Time), holds_at(playHideAndSeek(Agent2), Time) ->
	(   holds_at(outside(Agent1), Time),
	    holds_at(outside(Agent2), Time)
	;   not(holds_at(outside(Agent1), Time)),
	    not(holds_at(outside(Agent2), Time))
	).


% 
% 
% ; End of file.
% 
% agent Bob, Jilly
t(agent, bob).


t(agent, jilly).


% 
% !HoldsAt(Raining(),0).
not(holds_at(raining(), 0)).


% 
% !HoldsAt(SunShining(),0).
not(holds_at(sunShining(), 0)).


% 
% 
% ectest/Story1.e:35
% (HoldsAt(PlaySoccer(Bob),1) & HoldsAt(PlaySoccer(Jilly),1)) |
% (HoldsAt(PlayHideAndSeek(Bob),1) & HoldsAt(PlayHideAndSeek(Jilly),1)) |
% (HoldsAt(PlayComputerGame(Bob),1) & HoldsAt(PlayComputerGame(Jilly),1)).
(   holds_at(playSoccer(bob), 1),
    holds_at(playSoccer(jilly), 1)
;   holds_at(playHideAndSeek(bob), 1),
    holds_at(playHideAndSeek(jilly), 1)
;   holds_at(playComputerGame(bob), 1),
    holds_at(playComputerGame(jilly), 1)
).


% 
% 
% HoldsAt(Win(Bob),1) | HoldsAt(Win(Jilly),1).
(   holds_at(win(bob), 1)
;   holds_at(win(jilly), 1)
).


% 
% 
% ectest/Story1.e:41
% range time 0 1
range(time, 0, 1).


% range offset 0 0
range(offset, 0, 0).


% 
% ; End of file.
