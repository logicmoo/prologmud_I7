
:-include(library('ec_planner/ec_test_incl')).
% 
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
%; The SpeechAct representation deals with a few speech acts
%; \fullcite{Searle:1969}.
%;
%; @book{Searle:1969,
%;   author = "John R. Searle",
%;   year = "1969",
%;   title = "Speech Acts: An Essay in the Philosophy of Language",
%;   address = "Cambridge",
%;   publisher = "Cambridge University Press",
%; }
%;
%; We handle
%; the illocutionary acts of
%; inviting someone into one's house (a form of request) and
%; greeting someone,
%; and the expressive speech act of crying for joy.
%;
% ecnet/SpeechAct.e:28
% 
%; inviting in
% 
%; agent1 invites agent2 into room.
% event InviteIn(agent,agent,room)
 /*
.
*/
.

%; agent1 is invited into room by agent2.
% ecnet/SpeechAct.e:34
% fluent InvitedIn(agent,room,agent)
 /*
.
*/
.

% 
%; A precondition axiom states that for
%; an agent to invite another agent into a room,
%; the first agent must be in the room and
%; there must be an outside area such that
%; the second agent is at the outside area and
%; the outside area is adjacent to the room:
% ecnet/SpeechAct.e:42
% [agent1,agent2,room,time]% 
% Happens(InviteIn(agent1,agent2,room),time) ->
% HoldsAt(At(agent1,room),time) &
% ecnet/SpeechAct.e:45
% {outside}% 
% HoldsAt(At(agent2,outside),time) &
% Adjacent(room,outside).
 /*
exists([Outside],  (happens(inviteIn(Agent1, Agent2, Room), Time)->holds_at(at(Agent1, Room), Time), holds_at(at(Agent2, Outside), Time), adjacent(Room, Outside))).
*/
.

% 
% 
%; An effect axiom states that if
%; an agent invites another agent into a room,
%; the second agent will be invited into the room by the first agent:
% ecnet/SpeechAct.e:52
% [agent1,agent2,room,time]% 
% Initiates(InviteIn(agent1,agent2,room),
%           InvitedIn(agent2,room,agent1),
%           time).
 /*
.
*/
.

% 
% 
%; agent intends to walk into room.
% ecnet/SpeechAct.e:58
% event IntendToWalkIn(agent,room)
 /*
.
*/
.

%; agent has the intention to walk into room.
% fluent IntentionToWalkIn(agent,room)
 /*
.
*/
.

%; agent acts on the intention to walk into room.
% fluent ActOnIntentionToWalkIn(agent,room)
 /*
.
*/
.

% noninertial ActOnIntentionToWalkIn
 /*
.
*/
.

% ecnet/SpeechAct.e:64
% 
%; A trigger axiom states that
%; if an agent is invited into a room by another agent,
%; the first agent likes the second agent, and
%; the first agent does not already have the intention to
%; walk into the room,
%; the first agent intends to walk into the room:
% ecnet/SpeechAct.e:71
% [agent1,agent2,room,time]% 
% HoldsAt(InvitedIn(agent1,room,agent2),time) &
% HoldsAt(Like(agent1,agent2),time) &
% !HoldsAt(IntentionToWalkIn(agent1,room),time) ->
% Happens(IntendToWalkIn(agent1,room),time).
 /*
holds_at(invitedIn(Agent1, Room, Agent2), Time), holds_at(like(Agent1, Agent2), Time), not(holds_at(intentionToWalkIn(Agent1, Room), Time)) ->
    happens(intendToWalkIn(Agent1, Room), Time).
*/
.

% 
% 
%; An effect axiom states that
%; if an agent intends to walk into a room,
%; the agent will have the intention to walk into the room:
% ecnet/SpeechAct.e:80
% [agent,room,time]% 
% Initiates(IntendToWalkIn(agent,room),
%           IntentionToWalkIn(agent,room),
%           time).
 /*
.
*/
.

% 
% 
%; Two trigger axioms state that
%; if an agent has the intention to walk into a room,
%; the agent acts on the intention to walk into the room,
%; the agent is at a location,
%; side one (two) of a door is the room,
%; side two (one) of the door is the location,
%; agent will walk through side two (one) of the door:
% ecnet/SpeechAct.e:92
% [agent,room,location,door,time]% 
% HoldsAt(IntentionToWalkIn(agent,room),time) &
% HoldsAt(ActOnIntentionToWalkIn(agent,room),time) &
% HoldsAt(At(agent,location),time) &
% Side1(door)=room &
% Side2(door)=location ->
% Happens(WalkThroughDoor21(agent,door),time).
 /*
holds_at(intentionToWalkIn(Agent, Room), Time), holds_at(actOnIntentionToWalkIn(Agent, Room), Time), holds_at(at(Agent, Location), Time), side1(Door)=Room, side2(Door)=Location ->
    happens(walkThroughDoor21(Agent, Door), Time).
*/
.

% ecnet/SpeechAct.e:98
% 
% 
% ecnet/SpeechAct.e:100
% [agent,room,location,door,time]% 
% HoldsAt(IntentionToWalkIn(agent,room),time) &
% HoldsAt(ActOnIntentionToWalkIn(agent,room),time) &
% HoldsAt(At(agent,location),time) &
% Side2(door)=room &
% Side1(door)=location ->
% Happens(WalkThroughDoor12(agent,door),time).
 /*
holds_at(intentionToWalkIn(Agent, Room), Time), holds_at(actOnIntentionToWalkIn(Agent, Room), Time), holds_at(at(Agent, Location), Time), side2(Door)=Room, side1(Door)=Location ->
    happens(walkThroughDoor12(Agent, Door), Time).
*/
.

% ecnet/SpeechAct.e:106
% 
% 
%; Two effect axioms state that
%; if side one (two) of a door is a room and
%; an agent walks through side two (one) of the door,
%; the agent will no longer have the intention to
%; walk into the room:
% ecnet/SpeechAct.e:113
% [agent,room,door,time]% 
% Side1(door)=room ->
% Terminates(WalkThroughDoor21(agent,door),
%            IntentionToWalkIn(agent,room),
%            time).
 /*
side1(Door)=Room ->
    terminates(walkThroughDoor21(Agent, Door),
               intentionToWalkIn(Agent, Room),
               Time).
*/
.

% 
% 
% ecnet/SpeechAct.e:119
% [agent,room,door,time]% 
% Side2(door)=room ->
% Terminates(WalkThroughDoor12(agent,door),
%            IntentionToWalkIn(agent,room),
%            time).
 /*
side2(Door)=Room ->
    terminates(walkThroughDoor12(Agent, Door),
               intentionToWalkIn(Agent, Room),
               Time).
*/
.

% 
% 
%; agent greets object.
% ecnet/SpeechAct.e:126
% event Greet(agent,object)
 /*
.
*/
.

% 
% event SayPleasedToMeet(agent,agent)
 /*
.
*/
.

% 
%; agent says goodbye to object.
% event SayGoodbye(agent,object)
 /*
.
*/
.

% ecnet/SpeechAct.e:132
% 
% event TalkAbout(agent,content)
 /*
.
*/
.

% 
% event Converse(agent,agent)
 /*
.
*/
.

% 
% ecnet/SpeechAct.e:137
% [agent1,agent2,time]% 
% Happens(Converse(agent1,agent2),time) ->
% ecnet/SpeechAct.e:139
% {location}% 
% HoldsAt(At(agent1,location),time) &
% HoldsAt(At(agent2,location),time).
 /*
exists([Location],  (happens(converse(Agent1, Agent2), Time)->holds_at(at(Agent1, Location), Time), holds_at(at(Agent2, Location), Time))).
*/
.

% 
% 
%; A precondition axiom states that for
%; an agent to greet an object,
%; there must be a location such that
%; the agent is at the location and
%; the object is at the location:
% ecnet/SpeechAct.e:148
% [agent,object,time]% 
% Happens(Greet(agent,object),time) ->
% ecnet/SpeechAct.e:150
% {location}% 
% HoldsAt(At(agent,location),time) &
% HoldsAt(At(object,location),time).
 /*
exists([Location],  (happens(greet(Agent, Object), Time)->holds_at(at(Agent, Location), Time), holds_at(at(Object, Location), Time))).
*/
.

% 
% 
% ecnet/SpeechAct.e:154
% [agent,object,time]% 
% Happens(SayGoodbye(agent,object),time) ->
% ecnet/SpeechAct.e:156
% {location}% 
% HoldsAt(At(agent,location),time) &
% HoldsAt(At(object,location),time).
 /*
exists([Location],  (happens(sayGoodbye(Agent, Object), Time)->holds_at(at(Agent, Location), Time), holds_at(at(Object, Location), Time))).
*/
.

% 
% 
%; speech: expression of emotions
% 
%; agent cries for joy.
% ecnet/SpeechAct.e:163
% event CryForJoy(agent)
 /*
.
*/
.

% 
%; A precondition axiom states that for
%; an agent to cry for joy,
%; the agent must be happy:
% ecnet/SpeechAct.e:168
% [agent,time]% 
% Happens(CryForJoy(agent),time) ->
% HoldsAt(Happy(agent),time).
 /*
happens(cryForJoy(Agent), Time) ->
    holds_at(happy(Agent), Time).
*/
.

% 
% 
% event Threaten(agent,agent,weapon)
 /*
.
*/
.

% 
% ecnet/SpeechAct.e:174
% event ReleaseFromThreat(agent,agent)
 /*
.
*/
.

% 
% fluent ThreatenedBy(agent,agent)
 /*
.
*/
.

% 
% ecnet/SpeechAct.e:178
% [agent1,agent2,weapon,time]% 
% Happens(Threaten(agent1,agent2,weapon), time) ->
% HoldsAt(Holding(agent1,weapon),time) &
% ecnet/SpeechAct.e:181
% {location}% 
% HoldsAt(At(agent1,location),time) &
% HoldsAt(At(agent2,location),time).
 /*
exists([Location],  (happens(threaten(Agent1, Agent2, Weapon), Time)->holds_at(holding(Agent1, Weapon), Time), holds_at(at(Agent1, Location), Time), holds_at(at(Agent2, Location), Time))).
*/
.

% 
% 
% ecnet/SpeechAct.e:185
% [agent1,agent2,weapon,time]% 
% Happens(Threaten(agent1,agent2,weapon), time) ->
% Happens(BecomeAngryAt(agent2,agent1),time).
 /*
happens(threaten(Agent1, Agent2, Weapon), Time) ->
    happens(becomeAngryAt(Agent2, Agent1), Time).
*/
.

% 
% 
% ecnet/SpeechAct.e:189
% [agent1,agent2,weapon,time]% 
% Initiates(Threaten(agent1,agent2,weapon),
%           ThreatenedBy(agent2,agent1),
%           time).
 /*
.
*/
.

% 
% 
% ecnet/SpeechAct.e:194
% [agent1,agent2,time]% 
% Terminates(ReleaseFromThreat(agent1,agent2),
%            ThreatenedBy(agent2,agent1),
%            time).
 /*
.
*/
.

% 
% 
% event Order(agent,agent,physobj)
 /*
.
*/
.

% ecnet/SpeechAct.e:200
% 
% fluent KnowOrder(agent,agent,physobj)
 /*
.
*/
.

% 
% ecnet/SpeechAct.e:203
% [agent1,agent2,physobj,time]% 
% Initiates(Order(agent1,agent2,physobj),
%           KnowOrder(agent2,agent1,physobj),
%           time).
 /*
.
*/
.

% 
% 
% ecnet/SpeechAct.e:208
% [agent1,agent2,physobj,time]% 
% Happens(Order(agent1,agent2,physobj),time) ->
% ecnet/SpeechAct.e:210
% {location}% 
% HoldsAt(At(agent1,location),time) &
% HoldsAt(At(agent2,location),time).
 /*
exists([Location],  (happens(order(Agent1, Agent2, Physobj), Time)->holds_at(at(Agent1, Location), Time), holds_at(at(Agent2, Location), Time))).
*/
.

% 
% 
% event Request(agent,agent,physobj)
 /*
.
*/
.

% 
% ecnet/SpeechAct.e:216
% fluent KnowRequest(agent,agent,physobj)
 /*
.
*/
.

% 
% ecnet/SpeechAct.e:218
% [agent1,agent2,physobj,time]% 
% Initiates(Request(agent1,agent2,physobj),
%           KnowRequest(agent2,agent1,physobj),
%           time).
 /*
.
*/
.

% 
% 
% ecnet/SpeechAct.e:223
% [agent1,agent2,physobj,time]% 
% Happens(Request(agent1,agent2,physobj),time) ->
% ecnet/SpeechAct.e:225
% {location}% 
% HoldsAt(At(agent1,location),time) &
% HoldsAt(At(agent2,location),time).
 /*
exists([Location],  (happens(request(Agent1, Agent2, Physobj), Time)->holds_at(at(Agent1, Location), Time), holds_at(at(Agent2, Location), Time))).
*/
.

% 
% 
%; End of file.
