:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(pfc).
 %  loading(always,'examples/Mueller2006/Chapter3/Telephone1.e').
%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; @book{Mueller:2006,
%;   author = "Erik T. Mueller",
%;   year = "2006",
%;   title = "Commonsense Reasoning",
%;   address = "San Francisco",
%;   publisher = "Morgan Kaufmann/Elsevier",
%; }
%;

% load foundations/Root.e

% load foundations/EC.e

% sort agent
==> sort(agent).

% sort phone
==> sort(phone).

% agent Agent1, Agent2
==> t(agent,agent1).
==> t(agent,agent2).

% phone Phone1, Phone2
==> t(phone,phone1).
==> t(phone,phone2).

% fluent Ringing(phone,phone)
 %  fluent(ringing(phone,phone)).
==> mpred_prop(ringing(phone,phone),fluent).
==> meta_argtypes(ringing(phone,phone)).

% fluent DialTone(phone)
 %  fluent(dialTone(phone)).
==> mpred_prop(dialTone(phone),fluent).
==> meta_argtypes(dialTone(phone)).

% fluent BusySignal(phone)
 %  fluent(busySignal(phone)).
==> mpred_prop(busySignal(phone),fluent).
==> meta_argtypes(busySignal(phone)).

% fluent Idle(phone)
 %  fluent(idle(phone)).
==> mpred_prop(idle(phone),fluent).
==> meta_argtypes(idle(phone)).

% fluent Connected(phone,phone)
 %  fluent(connected(phone,phone)).
==> mpred_prop(connected(phone,phone),fluent).
==> meta_argtypes(connected(phone,phone)).

% fluent Disconnected(phone)
 %  fluent(disconnected(phone)).
==> mpred_prop(disconnected(phone),fluent).
==> meta_argtypes(disconnected(phone)).

% event PickUp(agent,phone)
 %  event(pickUp(agent,phone)).
==> mpred_prop(pickUp(agent,phone),event).
==> meta_argtypes(pickUp(agent,phone)).

% event SetDown(agent,phone)
 %  event(setDown(agent,phone)).
==> mpred_prop(setDown(agent,phone),event).
==> meta_argtypes(setDown(agent,phone)).

% event Dial(agent,phone,phone)
 %  event(dial(agent,phone,phone)).
==> mpred_prop(dial(agent,phone,phone),event).
==> meta_argtypes(dial(agent,phone,phone)).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:39
%; Sigma
% [agent,phone,time]
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:42
% HoldsAt(Idle(phone),time) ->
% Initiates(PickUp(agent,phone),DialTone(phone),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:43
axiom(initiates(pickUp(Agent, Phone), dialTone(Phone), Time),
    [holds_at(idle(Phone), Time)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:45
% [agent,phone,time]
% HoldsAt(Idle(phone),time) ->
% Terminates(PickUp(agent,phone),Idle(phone),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:47
axiom(terminates(pickUp(Agent, Phone), idle(Phone), Time),
    [holds_at(idle(Phone), Time)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:49
% [agent,phone,time]
% HoldsAt(DialTone(phone),time) ->
% Initiates(SetDown(agent,phone),Idle(phone),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:51
axiom(initiates(setDown(Agent, Phone), idle(Phone), Time),
    [holds_at(dialTone(Phone), Time)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:53
% [agent,phone,time]
% HoldsAt(DialTone(phone),time) ->
% Terminates(SetDown(agent,phone),DialTone(phone),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:55
axiom(terminates(setDown(Agent, Phone), dialTone(Phone), Time),
    [holds_at(dialTone(Phone), Time)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:57
% [agent,phone1,phone2,time]
% HoldsAt(DialTone(phone1),time) &
% HoldsAt(Idle(phone2),time) ->
% Initiates(Dial(agent,phone1,phone2),Ringing(phone1,phone2),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:60
axiom(initiates(dial(Agent, Phone1, Phone2), ringing(Phone1, Phone2), Time),
   
    [ holds_at(dialTone(Phone1), Time),
      holds_at(idle(Phone2), Time)
    ]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:62
% [agent,phone1,phone2,time]
% HoldsAt(DialTone(phone1),time) &
% HoldsAt(Idle(phone2),time) ->
% Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:65
axiom(terminates(dial(Agent, Phone1, Phone2), dialTone(Phone1), Time),
   
    [ holds_at(dialTone(Phone1), Time),
      holds_at(idle(Phone2), Time)
    ]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:67
% [agent,phone1,phone2,time]
% HoldsAt(DialTone(phone1),time) &
% HoldsAt(Idle(phone2),time) ->
% Terminates(Dial(agent,phone1,phone2),Idle(phone2),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:70
axiom(terminates(dial(Agent, Phone1, Phone2), idle(Phone2), Time),
   
    [ holds_at(dialTone(Phone1), Time),
      holds_at(idle(Phone2), Time)
    ]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:72
% [agent,phone1,phone2,time]
% HoldsAt(DialTone(phone1),time) &
% !HoldsAt(Idle(phone2),time) ->
% Initiates(Dial(agent,phone1,phone2),BusySignal(phone1),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:75
axiom(initiates(dial(Agent, Phone1, Phone2), busySignal(Phone1), Time),
   
    [ holds_at(dialTone(Phone1), Time),
      not(holds_at(idle(Phone2), Time))
    ]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:77
% [agent,phone1,phone2,time]
% HoldsAt(DialTone(phone1),time) &
% !HoldsAt(Idle(phone2),time) ->
% Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:80
axiom(terminates(dial(Agent, Phone1, Phone2), dialTone(Phone1), Time),
   
    [ holds_at(dialTone(Phone1), Time),
      not(holds_at(idle(Phone2), Time))
    ]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:82
% [agent,phone,time]
% HoldsAt(BusySignal(phone),time) ->
% Initiates(SetDown(agent,phone),Idle(phone),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:84
axiom(initiates(setDown(Agent, Phone), idle(Phone), Time),
    [holds_at(busySignal(Phone), Time)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:86
% [agent,phone,time]
% HoldsAt(BusySignal(phone),time) ->
% Terminates(SetDown(agent,phone),BusySignal(phone),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:88
axiom(terminates(setDown(Agent, Phone), busySignal(Phone), Time),
    [holds_at(busySignal(Phone), Time)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:90
% [agent,phone1,phone2,time]
% HoldsAt(Ringing(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone1),Idle(phone1),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:92
axiom(initiates(setDown(Agent, Phone1), idle(Phone1), Time),
    [holds_at(ringing(Phone1, Phone2), Time)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:94
% [agent,phone1,phone2,time]
% HoldsAt(Ringing(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone1),Idle(phone2),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:96
axiom(initiates(setDown(Agent, Phone1), idle(Phone2), Time),
    [holds_at(ringing(Phone1, Phone2), Time)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:98
% [agent,phone1,phone2,time]
% HoldsAt(Ringing(phone1,phone2),time) ->
% Terminates(SetDown(agent,phone1),Ringing(phone1,phone2),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:100
axiom(terminates(setDown(Agent, Phone1), ringing(Phone1, Phone2), Time),
    [holds_at(ringing(Phone1, Phone2), Time)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:102
% [agent,phone1,phone2,time]
% HoldsAt(Ringing(phone1,phone2),time) ->
% Initiates(PickUp(agent,phone2),Connected(phone1,phone2),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:104
axiom(initiates(pickUp(Agent, Phone2), connected(Phone1, Phone2), Time),
    [holds_at(ringing(Phone1, Phone2), Time)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:106
% [agent,phone1,phone2,time]
% HoldsAt(Ringing(phone1,phone2),time) ->
% Terminates(PickUp(agent,phone2),Ringing(phone1,phone2),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:108
axiom(terminates(pickUp(Agent, Phone2), ringing(Phone1, Phone2), Time),
    [holds_at(ringing(Phone1, Phone2), Time)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:110
% [agent,phone1,phone2,time]
% HoldsAt(Connected(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone1),Idle(phone1),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:112
axiom(initiates(setDown(Agent, Phone1), idle(Phone1), Time),
    [holds_at(connected(Phone1, Phone2), Time)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:114
% [agent,phone1,phone2,time]
% HoldsAt(Connected(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone1),Disconnected(phone2),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:116
axiom(initiates(setDown(Agent, Phone1), disconnected(Phone2), Time),
    [holds_at(connected(Phone1, Phone2), Time)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:118
% [agent,phone1,phone2,time]
% HoldsAt(Connected(phone1,phone2),time) ->
% Terminates(SetDown(agent,phone1),Connected(phone1,phone2),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:120
axiom(terminates(setDown(Agent, Phone1), connected(Phone1, Phone2), Time),
    [holds_at(connected(Phone1, Phone2), Time)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:122
% [agent,phone1,phone2,time]
% HoldsAt(Connected(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone2),Idle(phone2),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:124
axiom(initiates(setDown(Agent, Phone2), idle(Phone2), Time),
    [holds_at(connected(Phone1, Phone2), Time)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:126
% [agent,phone1,phone2,time]
% HoldsAt(Connected(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone2),Disconnected(phone1),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:128
axiom(initiates(setDown(Agent, Phone2), disconnected(Phone1), Time),
    [holds_at(connected(Phone1, Phone2), Time)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:130
% [agent,phone1,phone2,time]
% HoldsAt(Connected(phone1,phone2),time) ->
% Terminates(SetDown(agent,phone2),Connected(phone1,phone2),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:132
axiom(terminates(setDown(Agent, Phone2), connected(Phone1, Phone2), Time),
    [holds_at(connected(Phone1, Phone2), Time)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:134
% [agent,phone,time]
% HoldsAt(Disconnected(phone),time) ->
% Initiates(SetDown(agent,phone),Idle(phone),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:136
axiom(initiates(setDown(Agent, Phone), idle(Phone), Time),
    [holds_at(disconnected(Phone), Time)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:138
% [agent,phone,time]
% HoldsAt(Disconnected(phone),time) ->
% Terminates(SetDown(agent,phone),Disconnected(phone),time).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:140
axiom(terminates(setDown(Agent, Phone), disconnected(Phone), Time),
    [holds_at(disconnected(Phone), Time)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:142
%; Delta

% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:144
% Delta: 
next_axiom_uses(delta).
 


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:144
% Happens(PickUp(Agent1,Phone1),0).
axiom(happens(pickUp(agent1, phone1), t),
    [is_time(0)]).

% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:145
% Delta: 
next_axiom_uses(delta).
 


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:145
% Happens(Dial(Agent1,Phone1,Phone2),1).
axiom(happens(dial(agent1, phone1, phone2), start),
    [is_time(1), b(t, start), ignore(t+1=start)]).

% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:146
% Delta: 
next_axiom_uses(delta).
 


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:146
% Happens(PickUp(Agent2,Phone2),2).
axiom(happens(pickUp(agent2, phone2), t2),
    [is_time(2), b(t, t2), ignore(t+2=t2)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:148
%; Gamma
% [phone]
 % HoldsAt(Idle(phone),0).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:150
axiom(initially(idle(Phone)),
    []).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:151
% [phone]
 % !HoldsAt(DialTone(phone),0).
 %  not(initially(dialTone(Phone))).
axiom(not(initially(dialTone(DialTone_Ret))),
    []).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:152
% [phone]
 % !HoldsAt(BusySignal(phone),0).
 %  not(initially(busySignal(Phone))).
axiom(not(initially(busySignal(BusySignal_Ret))),
    []).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:153
% [phone1,phone2]
 % !HoldsAt(Ringing(phone1,phone2),0).
 %  not(initially(ringing(Phone1,Phone2))).
axiom(not(initially(ringing(Ringing_Param, Ringing_Ret))),
    []).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:154
% [phone1,phone2]
 % !HoldsAt(Connected(phone1,phone2),0).
 %  not(initially(connected(Phone1,Phone2))).
axiom(not(initially(connected(Connected_Param, Connected_Ret))),
    []).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:155
% [phone]
 % !HoldsAt(Disconnected(phone),0).
 %  not(initially(disconnected(Phone))).
axiom(not(initially(disconnected(Disconnected_Ret))),
    []).

% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:157
% completion Delta Happens
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:158
==> completion(delta).
==> completion(happens).

% range time 0 3
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:160
==> range(time,0,3).

% range offset 1 1
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/examples/Mueller2006/Chapter3/Telephone1.e:161
==> range(offset,1,1).
%; End of file.