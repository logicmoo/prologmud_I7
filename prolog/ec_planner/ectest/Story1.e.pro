% ectest/Story1.e:1
% translate: begining  File: ectest/Story1.e.pro 
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
% loading('foundations/Root.e').


% ectest/Story1.e:24
% translate: unskipped  File: on_load_ele 
% ectest/Story1.e:24
% translate: ready  File: on_load_ele 
% load foundations/EC.e
% loading('foundations/EC.e').


% foundations/EC.e:1
% translate: unskipped  File: on_load_ele 
% foundations/EC.e:42
% translate: ready  File: on_load_ele 
% 
% sort agent
sort(agent).


% 
% include examples/FrankEtAl2003/FrankEtAl.e
t(include, 'examples/FrankEtAl2003/FrankEtAl.e').


% 
% ectest/Story1.e:30
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


% ectest/Story1.e:37
% 
% 
% HoldsAt(Win(Bob),1) | HoldsAt(Win(Jilly),1).
(   holds_at(win(bob), 1)
;   holds_at(win(jilly), 1)
).


% 
% 
% range time 0 1
range(time, 0, 1).


% range offset 0 0
range(offset, 0, 0).


% ectest/Story1.e:43
% 
% ; End of file.
% ectest/Story1.e:45
% translate: ending  File: ectest/Story1.e.pro 
