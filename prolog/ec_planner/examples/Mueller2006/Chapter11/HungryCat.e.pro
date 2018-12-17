% examples/Mueller2006/Chapter11/HungryCat.e:1
% translate: begining  File: examples/Mueller2006/Chapter11/HungryCat.e.pro 
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
% ; @inproceedings{WinikoffEtAl:2002,
% ;   author = "Michael Winikoff and Lin Padgham and James Harland and John Thangarajah",
% ;   year = "2002",
% ;   title = "Declarative \& procedural goals in intelligent agent systems",
% ;   editor = "Dieter Fensel and Fausto Giunchiglia and Deborah McGuinness and Mary-Anne Williams",
% ;   booktitle = "\uppercase{P}roceedings of the \uppercase{E}ighth \uppercase{I}nternational \uppercase{C}onference on \uppercase{P}rinciples of \uppercase{K}nowledge \uppercase{R}epresentation and \uppercase{R}easoning",
% ;   pages = "470--481",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann",
% ; }
% ;
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% examples/Mueller2006/Chapter11/HungryCat.e:30
% 
% load foundations/Root.e
% loading('foundations/Root.e').


% examples/Mueller2006/Chapter11/HungryCat.e:32
% translate: unskipped  File: on_load_ele 
% examples/Mueller2006/Chapter11/HungryCat.e:32
% translate: ready  File: on_load_ele 
% load foundations/EC.e
% loading('foundations/EC.e').


% foundations/EC.e:1
% translate: unskipped  File: on_load_ele 
% foundations/EC.e:42
% translate: ready  File: on_load_ele 
% 
% sort object
sort(object).


% sort agent: object
subsort(agent, object).


% sort food: object
subsort(food, object).


% sort surface
sort(surface).


% examples/Mueller2006/Chapter11/HungryCat.e:38
% sort plan
sort(plan).


% 
% reified sort belief
reified_sort(belief).


% 
% agent Cat
t(agent, cat).


% surface Floor, Chair, Shelf, Table
t(surface, floor).


t(surface, chair).


t(surface, shelf).


t(surface, table).


% examples/Mueller2006/Chapter11/HungryCat.e:44
% food Food1, Food2
t(food, food1).


t(food, food2).


% plan P1, P1a, P1b, P2, P2a
t(plan, p1).


t(plan, p1a).


t(plan, p1b).


t(plan, p2).


t(plan, p2a).


% 
% predicate SelectedPlan(agent,belief,plan,time)
predicate(selectedPlan(agent, belief, plan, time)).


% predicate SoundPlan(agent,belief,plan,time)
predicate(soundPlan(agent, belief, plan, time)).


% 
% examples/Mueller2006/Chapter11/HungryCat.e:50
% fluent On(object,surface)
fluent(on(object, surface)).


% fluent Goal(agent,belief)
fluent(goal(agent, belief)).


% fluent CanJump(surface,surface)
fluent(canJump(surface, surface)).


% fluent Plan(agent,belief,plan)
fluent(plan(agent, belief, plan)).


% fluent Satiated(agent)
fluent(satiated(agent)).


% fluent Believe(agent,belief)
fluent(believe(agent, belief)).


% examples/Mueller2006/Chapter11/HungryCat.e:56
% 
% event AddPlan(agent,belief,plan)
event(addPlan(agent, belief, plan)).


% event DropPlan(agent,belief,plan)
event(dropPlan(agent, belief, plan)).


% event Jump(agent,surface,surface)
event(jump(agent, surface, surface)).


% event Move(surface,surface,surface)
event(move(surface, surface, surface)).


% event Eat(agent,food)
event(eat(agent, food)).


% examples/Mueller2006/Chapter11/HungryCat.e:62
% event Wait(agent)
event(wait(agent)).


% 
% belief BSatiated(agent)
t(belief, 'bSatiated(agent)').


% belief BCanJump(surface,surface)
t(belief, 'bCanJump(surface').


t(belief, 'surface)').


% belief BOn(object,surface)
t(belief, 'bOn(object').


t(belief, 'surface)').


% 
% ; Sigma
% examples/Mueller2006/Chapter11/HungryCat.e:69
% 
% ; A5
% examples/Mueller2006/Chapter11/HungryCat.e:71
% [agent,belief,plan,time]% 
% Initiates(AddPlan(agent,belief,plan),Plan(agent,belief,plan),time).
initiates(addPlan(Agent, Belief, Plan), plan(Agent, Belief, Plan), Time).


% 
% 
% ; A6
% examples/Mueller2006/Chapter11/HungryCat.e:75
% [agent,belief,plan,time]% 
% Terminates(DropPlan(agent,belief,plan),Plan(agent,belief,plan),time).
terminates(dropPlan(Agent, Belief, Plan), plan(Agent, Belief, Plan), Time).


% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:78
% [agent,surface1,surface2,time]% 
% HoldsAt(On(agent,surface1),time) &
% HoldsAt(CanJump(surface1,surface2),time) ->
% Initiates(Jump(agent,surface1,surface2),On(agent,surface2),time).
holds_at(on(Agent, Surface1), Time), holds_at(canJump(Surface1, Surface2), Time) ->
	initiates(jump(Agent, Surface1, Surface2),
		  on(Agent, Surface2),
		  Time).


% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:83
% [agent,surface1,surface2,time]% 
% HoldsAt(On(agent,surface1),time) &
% HoldsAt(CanJump(surface1,surface2),time) ->
% Terminates(Jump(agent,surface1,surface2),On(agent,surface1),time).
holds_at(on(Agent, Surface1), Time), holds_at(canJump(Surface1, Surface2), Time) ->
	terminates(jump(Agent, Surface1, Surface2),
		   on(Agent, Surface1),
		   Time).


% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:88
% [surface1,surface2,surface3,time]% 
% Initiates(Move(surface1,surface2,surface3),CanJump(surface1,surface3),time).
initiates(move(Surface1, Surface2, Surface3), canJump(Surface1, Surface3), Time).


% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:91
% [surface1,surface2,surface3,time]% 
% Terminates(Move(surface1,surface2,surface3),CanJump(surface1,surface2),time).
terminates(move(Surface1, Surface2, Surface3), canJump(Surface1, Surface2), Time).


% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:94
% [agent,food,surface,time]% 
% HoldsAt(On(agent,surface),time) &
% HoldsAt(On(food,surface),time) ->
% Initiates(Eat(agent,food),Satiated(agent),time).
holds_at(on(Agent, Surface), Time), holds_at(on(Food, Surface), Time) ->
	initiates(eat(Agent, Food),
		  satiated(Agent),
		  Time).


% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:99
% [agent,food,surface,time]% 
% HoldsAt(On(agent,surface),time) &
% HoldsAt(On(food,surface),time) ->
% Terminates(Eat(agent,food),On(food,surface),time).
holds_at(on(Agent, Surface), Time), holds_at(on(Food, Surface), Time) ->
	terminates(eat(Agent, Food),
		   on(Food, Surface),
		   Time).


% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:104
% [agent,surface1,surface2,belief,time]% 
% HoldsAt(Believe(agent,BOn(agent,surface1)),time) &
% HoldsAt(Believe(agent,BCanJump(surface1,surface2)),time) &
% (belief = BOn(agent,surface2)) ->
% Initiates(Jump(agent,surface1,surface2),
%           Believe(agent,belief),
%           time).
holds_at(believe(Agent, bOn(Agent, Surface1)), Time), holds_at(believe(Agent, bCanJump(Surface1, Surface2)), Time), Belief=bOn(Agent, Surface2) ->
	initiates(jump(Agent, Surface1, Surface2),
		  believe(Agent, Belief),
		  Time).


% examples/Mueller2006/Chapter11/HungryCat.e:110
% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:112
% [agent,surface1,surface2,belief,time]% 
% HoldsAt(Believe(agent,BOn(agent,surface1)),time) &
% HoldsAt(Believe(agent,BCanJump(surface1,surface2)),time) &
% (belief = BOn(agent,surface1)) ->
% Terminates(Jump(agent,surface1,surface2),
%            Believe(agent,belief),
%            time).
holds_at(believe(Agent, bOn(Agent, Surface1)), Time), holds_at(believe(Agent, bCanJump(Surface1, Surface2)), Time), Belief=bOn(Agent, Surface1) ->
	terminates(jump(Agent, Surface1, Surface2),
		   believe(Agent, Belief),
		   Time).


% examples/Mueller2006/Chapter11/HungryCat.e:118
% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:120
% [agent,surface1,surface2,surface3,belief,time]% 
% (belief = BCanJump(surface1,surface3)) ->
% Initiates(Move(surface1,surface2,surface3),
%           Believe(agent,belief),
%           time).
Belief=bCanJump(Surface1, Surface3) ->
	initiates(move(Surface1, Surface2, Surface3),
		  believe(Agent, Belief),
		  Time).


% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:126
% [agent,surface1,surface2,surface3,belief,time]% 
% (belief = BCanJump(surface1,surface2)) ->
% Terminates(Move(surface1,surface2,surface3),
%            Believe(agent,belief),
%            time).
Belief=bCanJump(Surface1, Surface2) ->
	terminates(move(Surface1, Surface2, Surface3),
		   believe(Agent, Belief),
		   Time).


% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:132
% [agent,food,surface,belief,time]% 
% HoldsAt(Believe(agent,BOn(agent,surface)),time) &
% HoldsAt(Believe(agent,BOn(food,surface)),time) &
% (belief = BSatiated(agent)) ->
% Initiates(Eat(agent,food),Believe(agent,belief),time).
holds_at(believe(Agent, bOn(Agent, Surface)), Time), holds_at(believe(Agent, bOn(Food, Surface)), Time), Belief=bSatiated(Agent) ->
	initiates(eat(Agent, Food),
		  believe(Agent, Belief),
		  Time).


% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:138
% [agent,food,surface,belief,time]% 
% HoldsAt(Believe(agent,BOn(agent,surface)),time) &
% HoldsAt(Believe(agent,BOn(food,surface)),time) &
% (belief = BOn(food,surface)) ->
% Terminates(Eat(agent,food),Believe(agent,belief),time).
holds_at(believe(Agent, bOn(Agent, Surface)), Time), holds_at(believe(Agent, bOn(Food, Surface)), Time), Belief=bOn(Food, Surface) ->
	terminates(eat(Agent, Food),
		   believe(Agent, Belief),
		   Time).


% 
% 
% ; Delta
% examples/Mueller2006/Chapter11/HungryCat.e:145
% 
% ; A7
% examples/Mueller2006/Chapter11/HungryCat.e:147
% [agent,belief,plan,time]% 
% HoldsAt(Goal(agent,belief),time) &
% !HoldsAt(Believe(agent,belief),time) &
% SelectedPlan(agent,belief,plan,time) &
% (!{plan1} HoldsAt(Plan(agent,belief,plan1),time)) ->
% Happens(AddPlan(agent,belief,plan),time).
holds_at(goal(Agent, Belief), Time), not(holds_at(believe(Agent, Belief), Time)), selectedPlan(Agent, Belief, Plan, Time), not(exists([Plan1], holds_at(plan(Agent, Belief, Plan1), Time))) ->
	happens(addPlan(Agent, Belief, Plan), Time).


% 
% examples/Mueller2006/Chapter11/HungryCat.e:153
% 
% ; A8
% examples/Mueller2006/Chapter11/HungryCat.e:155
% [agent,belief,time]% 
% HoldsAt(Plan(agent,belief,P1),time) &
% !HoldsAt(Believe(agent,belief),time) &
% SoundPlan(agent,belief,P1,time) ->
% Happens(Jump(Cat,Floor,Chair),time).
holds_at(plan(Agent, Belief, p1), Time), not(holds_at(believe(Agent, Belief), Time)), soundPlan(Agent, Belief, p1, Time) ->
	happens(jump(cat, floor, chair), Time).


% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:161
% [agent,belief,time]% 
% HoldsAt(Plan(agent,belief,P1a),time) &
% !HoldsAt(Believe(agent,belief),time) &
% SoundPlan(agent,belief,P1a,time) ->
% Happens(Wait(Cat),time).
holds_at(plan(Agent, Belief, p1a), Time), not(holds_at(believe(Agent, Belief), Time)), soundPlan(Agent, Belief, p1a, Time) ->
	happens(wait(cat), Time).


% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:167
% [agent,belief,time]% 
% HoldsAt(Plan(agent,belief,P2),time) &
% !HoldsAt(Believe(agent,belief),time) &
% SoundPlan(agent,belief,P2,time) ->
% Happens(Jump(Cat,Chair,Shelf),time).
holds_at(plan(Agent, Belief, p2), Time), not(holds_at(believe(Agent, Belief), Time)), soundPlan(Agent, Belief, p2, Time) ->
	happens(jump(cat, chair, shelf), Time).


% 
% 
% ; A9
% examples/Mueller2006/Chapter11/HungryCat.e:174
% [agent,belief,plan,time]% 
% HoldsAt(Plan(agent,belief,plan),time) ->
% Happens(DropPlan(agent,belief,plan),time).
holds_at(plan(Agent, Belief, Plan), Time) ->
	happens(dropPlan(Agent, Belief, Plan), Time).


% 
% 
% ; A10
% examples/Mueller2006/Chapter11/HungryCat.e:179
% [agent,belief,time]% 
% HoldsAt(Plan(agent,belief,P1),time) &
% !HoldsAt(Believe(agent,belief),time) &
% SoundPlan(agent,belief,P1,time) ->
% Happens(AddPlan(agent,belief,P1a),time).
holds_at(plan(Agent, Belief, p1), Time), not(holds_at(believe(Agent, Belief), Time)), soundPlan(Agent, Belief, p1, Time) ->
	happens(addPlan(Agent, Belief, p1a), Time).


% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:185
% [agent,belief,time]% 
% HoldsAt(Plan(agent,belief,P1a),time) &
% !HoldsAt(Believe(agent,belief),time) &
% SoundPlan(agent,belief,P1a,time) ->
% Happens(AddPlan(agent,belief,P1b),time).
holds_at(plan(Agent, Belief, p1a), Time), not(holds_at(believe(Agent, Belief), Time)), soundPlan(Agent, Belief, p1a, Time) ->
	happens(addPlan(Agent, Belief, p1b), Time).


% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:191
% [agent,belief,time]% 
% HoldsAt(Plan(agent,belief,P2),time) &
% !HoldsAt(Believe(agent,belief),time) &
% SoundPlan(agent,belief,P2,time) ->
% Happens(AddPlan(agent,belief,P2a),time).
holds_at(plan(Agent, Belief, p2), Time), not(holds_at(believe(Agent, Belief), Time)), soundPlan(Agent, Belief, p2, Time) ->
	happens(addPlan(Agent, Belief, p2a), Time).


% 
% 
% ; reactive behavior
% examples/Mueller2006/Chapter11/HungryCat.e:198
% [agent,food,surface,time]% 
% !HoldsAt(Satiated(agent),time) &
% HoldsAt(On(agent,surface),time) &
% HoldsAt(On(food,surface),time) ->
% Happens(Eat(agent,food),time).
not(holds_at(satiated(Agent), Time)), holds_at(on(Agent, Surface), Time), holds_at(on(Food, Surface), Time) ->
	happens(eat(Agent, Food), Time).


% 
% 
% ; narrative
% examples/Mueller2006/Chapter11/HungryCat.e:205
% 
% Happens(Move(Chair,Table,Shelf),2).
happens(move(chair, table, shelf), 2).


% 
% 
% ; SelectedPlan - plan library
% 
% ;[agent,belief,plan,time]
% ;SelectedPlan(agent,belief,plan,time) <->
% ;(agent=Cat & belief=BSatiated(Cat) & plan=P1 & time=0) |
% ;(agent=Cat & belief=BSatiated(Cat) & plan=P2 & time=4).
% examples/Mueller2006/Chapter11/HungryCat.e:214
% 
% examples/Mueller2006/Chapter11/HungryCat.e:215
% [agent,belief,plan,time]% 
% SelectedPlan(agent,belief,plan,time) <->
% ({surface1,surface2,surface3,food}
%  HoldsAt(Believe(agent,BOn(agent,surface1)),time) &
%  HoldsAt(Believe(agent,BCanJump(surface1,surface2)),time) &
%  HoldsAt(Believe(agent,BCanJump(surface2,surface3)),time) &
%  HoldsAt(Believe(agent,BOn(food,surface3)),time) &
%  belief=BSatiated(agent) &
%  plan=P1 &
%  time=0) |
% ({surface1,surface2,surface3,food}
%  HoldsAt(Believe(agent,BOn(agent,surface1)),time) &
%  HoldsAt(Believe(agent,BCanJump(surface1,surface2)),time) &
%  HoldsAt(Believe(agent,BCanJump(surface2,surface3)),time) &
%  HoldsAt(Believe(agent,BOn(food,surface3)),time) &
%  belief=BSatiated(agent) &
%  plan=P2 &
%  time=4).
selectedPlan(Agent, Belief, Plan, Time) <->
	(   exists([Surface1, Surface2, Surface3, Food],
		   (holds_at(believe(Agent, bOn(Agent, Surface1)), Time), holds_at(believe(Agent, bCanJump(Surface1, Surface2)), Time), holds_at(believe(Agent, bCanJump(Surface2, Surface3)), Time), holds_at(believe(Agent, bOn(Food, Surface3)), Time), Belief=bSatiated(Agent), Plan=p1, Time=0))
	;   exists(
		   [ Surface18,
		     Surface29,
		     Surface310,
		     Food11
		   ],
		   (holds_at(believe(Agent, bOn(Agent, Surface18)), Time), holds_at(believe(Agent, bCanJump(Surface18, Surface29)), Time), holds_at(believe(Agent, bCanJump(Surface29, Surface310)), Time), holds_at(believe(Agent, bOn(Food11, Surface310)), Time), Belief=bSatiated(Agent), Plan=p2, Time=4))
	).


% examples/Mueller2006/Chapter11/HungryCat.e:232
% 
% 
% 
% ; SoundPlan
% 
% examples/Mueller2006/Chapter11/HungryCat.e:237
% [agent,belief,plan,time]% 
% SoundPlan(agent,belief,plan,time) <->
% (plan=P1 ->
%  HoldsAt(Believe(agent,BCanJump(Floor,Chair)),time) &
%  HoldsAt(Believe(agent,BCanJump(Chair,Table)),time)) &
% ((plan=P1a | plan=P1b) ->
%   HoldsAt(Believe(agent,BCanJump(Chair,Table)),time)).
soundPlan(Agent, Belief, Plan, Time) <->
	( Plan=p1->holds_at(believe(Agent, bCanJump(floor, chair)), Time), holds_at(believe(Agent, bCanJump(chair, table)), Time)
	),
	( Plan=p1a;Plan=p1b->holds_at(believe(Agent, bCanJump(chair, table)), Time)
	).


% examples/Mueller2006/Chapter11/HungryCat.e:243
% 
% 
% ; Gamma
% 
% examples/Mueller2006/Chapter11/HungryCat.e:247
% [agent,belief]% 
% HoldsAt(Goal(agent,belief),0) <->
% (agent=Cat & belief=BSatiated(Cat)).
holds_at(goal(Agent, Belief), 0) <->
	Agent=cat,
	Belief=bSatiated(cat).


% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:251
% [agent,belief,plan] % !HoldsAt(Plan(agent,belief,plan),0).
not(holds_at(plan(Agent, Belief, Plan), 0)).


% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:253
% [object,surface] % HoldsAt(On(object,surface),0) <->
% (object=Cat & surface=Floor) |
% (object=Food1 & surface=Table) |
% (object=Food2 & surface=Shelf).
holds_at(on(Object, Surface), 0) <->
	(   Object=cat,
	    Surface=floor
	;   Object=food1,
	    Surface=table
	;   Object=food2,
	    Surface=shelf
	).


% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:258
% [surface1,surface2] % HoldsAt(CanJump(surface1,surface2),0) <->
% (surface1=Floor & surface2=Chair) |
% (surface1=Chair & surface2=Table) |
% (surface1=Shelf & surface2=Table).
holds_at(canJump(Surface1, Surface2), 0) <->
	(   Surface1=floor,
	    Surface2=chair
	;   Surface1=chair,
	    Surface2=table
	;   Surface1=shelf,
	    Surface2=table
	).


% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:263
% [agent,object,surface]% 
% HoldsAt(Believe(agent,BOn(object,surface)),0) <->
% (agent=Cat & object=Cat & surface=Floor) |
% (agent=Cat & object=Food1 & surface=Table).
holds_at(believe(Agent, bOn(Object, Surface)), 0) <->
	(   Agent=cat,
	    Object=cat,
	    Surface=floor
	;   Agent=cat,
	    Object=food1,
	    Surface=table
	).


% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:268
% [agent,surface1,surface2]% 
% HoldsAt(Believe(agent,BCanJump(surface1,surface2)),0) <->
% (agent=Cat & surface1=Floor & surface2=Chair) |
% (agent=Cat & surface1=Chair & surface2=Table) |
% (agent=Cat & surface1=Shelf & surface2=Table).
holds_at(believe(Agent, bCanJump(Surface1, Surface2)), 0) <->
	(   Agent=cat,
	    Surface1=floor,
	    Surface2=chair
	;   Agent=cat,
	    Surface1=chair,
	    Surface2=table
	;   Agent=cat,
	    Surface1=shelf,
	    Surface2=table
	).


% 
% 
% examples/Mueller2006/Chapter11/HungryCat.e:274
% !HoldsAt(Believe(Cat,BSatiated(Cat)),0).
not(holds_at(believe(cat, bSatiated(cat)), 0)).


% 
% 
% ; ADDED:
% !HoldsAt(Satiated(Cat),0).
not(holds_at(satiated(cat), 0)).


% 
% 
% completion Happens
completion(happens).


% examples/Mueller2006/Chapter11/HungryCat.e:280
% 
% range time 0 7
range(time, 0, 7).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% examples/Mueller2006/Chapter11/HungryCat.e:285
% translate: ending  File: examples/Mueller2006/Chapter11/HungryCat.e.pro 
