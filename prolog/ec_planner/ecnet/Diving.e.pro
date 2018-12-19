% ecnet/Diving.e:1
% translate: begining  File: ecnet/Diving.e.pro 
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
%; scuba diving
%;
% ecnet/Diving.e:13
% 
% sort object
sort(object).
% sort agent: object
subsort(agent,object).
% sort diver: agent
subsort(diver,agent).
% sort depth: integer
subsort(depth,integer).
% sort boat: object
subsort(boat,object).
% ecnet/Diving.e:19
% 
%; reference line, anchor line, shotline, SMB line, ...
% sort line: object
subsort(line,object).
% 
% sort equipment: object
subsort(equipment,object).
% sort weight: equipment
subsort(weight,equipment).
% ecnet/Diving.e:25
% sort fin: equipment
subsort(fin,equipment).
% sort airtank: equipment
subsort(airtank,equipment).
% 
%; buoyancy compensator (BC)
%; buoyancy control device (BCD)
% sort computer: equipment
subsort(computer,equipment).
% ecnet/Diving.e:31
% sort bc: equipment
subsort(bc,equipment).
% 
% fluent AtDepth(object,depth)
fluent(atDepth(object,depth)).
% 
% ecnet/Diving.e:35
% [object,depth1,depth2,time]% 
% HoldsAt(AtDepth(object,depth1),time) &
% HoldsAt(AtDepth(object,depth2),time) ->
% depth1 = depth2.
holds_at(atDepth(Object, Depth1), Time), holds_at(atDepth(Object, Depth2), Time) ->
	Depth1=Depth2.
% 
% 
% event Ascend(diver,depth)
event(ascend(diver,depth)).
% ecnet/Diving.e:41
% 
% event Descend(diver,depth)
event(descend(diver,depth)).
% 
% ecnet/Diving.e:44
% [diver,depth1,depth2,time]% 
% HoldsAt(AtDepth(diver,depth1),time) &
% Happens(Descend(diver,depth2),time) ->
% depth2>depth1.
holds_at(atDepth(Diver, Depth1), Time), happens(descend(Diver, Depth2), Time) ->
	Depth2>Depth1.
% 
% 
% ecnet/Diving.e:49
% [diver,depth1,depth2,time]% 
% HoldsAt(AtDepth(diver,depth1),time) &
% Happens(Ascend(diver,depth2),time) ->
% depth2<depth1.
holds_at(atDepth(Diver, Depth1), Time), happens(ascend(Diver, Depth2), Time) ->
	Depth2<Depth1.
% 
% 
% ecnet/Diving.e:54
% [diver,depth,time]% 
% Initiates(Descend(diver,depth),AtDepth(diver,depth),time).
initiates(descend(Diver,Depth),
	  atDepth(Diver,Depth),
	  Time).
% 
% 
% ecnet/Diving.e:57
% [diver,depth1,depth2,time]% 
% HoldsAt(AtDepth(diver,depth1),time) ->
% Terminates(Descend(diver,depth2),AtDepth(diver,depth1),time).
holds_at(atDepth(Diver, Depth1), Time) ->
	terminates(descend(Diver, Depth2),
		   atDepth(Diver, Depth1),
		   Time).
% 
% 
% ecnet/Diving.e:61
% [diver,depth,time]% 
% Initiates(Ascend(diver,depth),AtDepth(diver,depth),time).
initiates(ascend(Diver,Depth),
	  atDepth(Diver,Depth),
	  Time).
% 
% 
% ecnet/Diving.e:64
% [diver,depth1,depth2,time]% 
% HoldsAt(AtDepth(diver,depth1),time) ->
% Terminates(Ascend(diver,depth2),AtDepth(diver,depth1),time).
holds_at(atDepth(Diver, Depth1), Time) ->
	terminates(ascend(Diver, Depth2),
		   atDepth(Diver, Depth1),
		   Time).
% 
% 
% fluent Wearing(diver,equipment)
fluent(wearing(diver,equipment)).
% 
% ecnet/Diving.e:70
% event PutOn(diver,equipment)
event(putOn(diver,equipment)).
% 
% event TakeOff(diver,equipment)
event(takeOff(diver,equipment)).
% 
% event Lose(diver,equipment)
event(lose(diver,equipment)).
% 
% ecnet/Diving.e:76
% [diver,equipment,depth,time]% 
% Releases(PutOn(diver,equipment),AtDepth(equipment,depth),time).
releases(putOn(Diver,Equipment),
	 atDepth(Equipment,Depth),
	 Time).
% 
% 
% ecnet/Diving.e:79
% [diver,equipment,time]% 
% Releases(PutOn(diver,equipment),UnderWater(equipment),time).
releases(putOn(Diver,Equipment),
	 underWater(Equipment),
	 Time).
% 
% 
% ecnet/Diving.e:82
% [diver,equipment,time]% 
% Happens(PutOn(diver,equipment),time) ->
% !{diver1} HoldsAt(Wearing(diver1,equipment),time).
happens(putOn(Diver, Equipment), Time) ->
	not(exists([Diver1],
		   holds_at(wearing(Diver1, Equipment), Time))).
% 
% 
% ecnet/Diving.e:86
% [diver,depth,equipment,time]% 
% HoldsAt(Wearing(diver,equipment),time) ->
% (HoldsAt(AtDepth(diver,depth),time) <->
%  HoldsAt(AtDepth(equipment,depth),time)).
holds_at(wearing(Diver, Equipment), Time) ->
	( holds_at(atDepth(Diver, Depth), Time)<->holds_at(atDepth(Equipment, Depth), Time)
	).
% 
% 
% ecnet/Diving.e:91
% [diver,depth,object,time]% 
% HoldsAt(Holding(diver,object),time) ->
% (HoldsAt(AtDepth(diver,depth),time) <->
%  HoldsAt(AtDepth(object,depth),time)).
holds_at(holding(Diver, Object), Time) ->
	( holds_at(atDepth(Diver, Depth), Time)<->holds_at(atDepth(Object, Depth), Time)
	).
% 
% 
% ecnet/Diving.e:96
% [diver,equipment,time]% 
% HoldsAt(Wearing(diver,equipment),time) ->
% (HoldsAt(UnderWater(diver),time) <->
%  HoldsAt(UnderWater(equipment),time)).
holds_at(wearing(Diver, Equipment), Time) ->
	( holds_at(underWater(Diver), Time)<->holds_at(underWater(Equipment), Time)
	).
% 
% 
% ecnet/Diving.e:101
% [diver,object,time]% 
% HoldsAt(Holding(diver,object),time) ->
% (HoldsAt(UnderWater(diver),time) <->
%  HoldsAt(UnderWater(object),time)).
holds_at(holding(Diver, Object), Time) ->
	( holds_at(underWater(Diver), Time)<->holds_at(underWater(Object), Time)
	).
% 
% 
% ecnet/Diving.e:106
% [diver,depth,equipment,time]% 
% HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Wearing(diver,equipment),time) ->
% Initiates(TakeOff(diver,equipment),AtDepth(equipment,depth),time).
holds_at(atDepth(Diver, Depth), Time), holds_at(wearing(Diver, Equipment), Time) ->
	initiates(takeOff(Diver, Equipment),
		  atDepth(Equipment, Depth),
		  Time).
% 
% 
% ecnet/Diving.e:111
% [diver,depth,equipment,time]% 
% !HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Wearing(diver,equipment),time) ->
% Terminates(TakeOff(diver,equipment),AtDepth(equipment,depth),time).
not(holds_at(atDepth(Diver, Depth), Time)), holds_at(wearing(Diver, Equipment), Time) ->
	terminates(takeOff(Diver, Equipment),
		   atDepth(Equipment, Depth),
		   Time).
% 
% 
% ecnet/Diving.e:116
% [diver,equipment,time]% 
% HoldsAt(UnderWater(diver),time) ->
% Initiates(TakeOff(diver,equipment),UnderWater(equipment),time).
holds_at(underWater(Diver), Time) ->
	initiates(takeOff(Diver, Equipment),
		  underWater(Equipment),
		  Time).
% 
% 
% ecnet/Diving.e:120
% [diver,equipment,time]% 
% !HoldsAt(UnderWater(diver),time) ->
% Terminates(TakeOff(diver,equipment),UnderWater(equipment),time).
not(holds_at(underWater(Diver), Time)) ->
	terminates(takeOff(Diver, Equipment),
		   underWater(Equipment),
		   Time).
% 
% 
% ecnet/Diving.e:124
% [diver,equipment,depth,time]% 
% HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Wearing(diver,equipment),time) ->
% Initiates(Lose(diver,equipment),AtDepth(equipment,depth),time).
holds_at(atDepth(Diver, Depth), Time), holds_at(wearing(Diver, Equipment), Time) ->
	initiates(lose(Diver, Equipment),
		  atDepth(Equipment, Depth),
		  Time).
% 
% 
% ecnet/Diving.e:129
% [diver,equipment,depth,time]% 
% !HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Wearing(diver,equipment),time) ->
% Terminates(Lose(diver,equipment),AtDepth(equipment,depth),time).
not(holds_at(atDepth(Diver, Depth), Time)), holds_at(wearing(Diver, Equipment), Time) ->
	terminates(lose(Diver, Equipment),
		   atDepth(Equipment, Depth),
		   Time).
% 
% 
% ecnet/Diving.e:134
% [diver,equipment,time]% 
% HoldsAt(UnderWater(diver),time) ->
% Initiates(Lose(diver,equipment),UnderWater(equipment),time).
holds_at(underWater(Diver), Time) ->
	initiates(lose(Diver, Equipment),
		  underWater(Equipment),
		  Time).
% 
% 
% ecnet/Diving.e:138
% [diver,equipment,time]% 
% !HoldsAt(UnderWater(diver),time) ->
% Terminates(Lose(diver,equipment),UnderWater(equipment),time).
not(holds_at(underWater(Diver), Time)) ->
	terminates(lose(Diver, Equipment),
		   underWater(Equipment),
		   Time).
% 
% 
% fluent Holding(diver,object)
fluent(holding(diver,object)).
% 
% ecnet/Diving.e:144
% [diver1,diver2,time]% 
% HoldsAt(Holding(diver1,diver2),time) ->
% !HoldsAt(Holding(diver2,diver1),time).
holds_at(holding(Diver1, Diver2), Time) ->
	not(holds_at(holding(Diver2, Diver1), Time)).
% 
% 
% event Grab(diver,object)
event(grab(diver,object)).
% 
% ecnet/Diving.e:150
% event LetGoOf(diver,object)
event(letGoOf(diver,object)).
% 
% ecnet/Diving.e:152
% [diver,object,time]% 
% Initiates(Grab(diver,object),Holding(diver,object),time).
initiates(grab(Diver,Object),
	  holding(Diver,Object),
	  Time).
% 
% 
% ecnet/Diving.e:155
% [diver,object,time]% 
% Terminates(LetGoOf(diver,object),Holding(diver,object),time).
terminates(letGoOf(Diver,Object),
	   holding(Diver,Object),
	   Time).
% 
% 
% ecnet/Diving.e:158
% [diver,object,depth,time]% 
% Releases(Grab(diver,object),AtDepth(object,depth),time).
releases(grab(Diver,Object),
	 atDepth(Object,Depth),
	 Time).
% 
% 
% ecnet/Diving.e:161
% [diver,object,time]% 
% Releases(Grab(diver,object),UnderWater(object),time).
releases(grab(Diver,Object),
	 underWater(Object),
	 Time).
% 
% 
% ecnet/Diving.e:164
% [diver,object,depth,time]% 
% HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Holding(diver,object),time) ->
% Initiates(LetGoOf(diver,object),AtDepth(object,depth),time).
holds_at(atDepth(Diver, Depth), Time), holds_at(holding(Diver, Object), Time) ->
	initiates(letGoOf(Diver, Object),
		  atDepth(Object, Depth),
		  Time).
% 
% 
% ecnet/Diving.e:169
% [diver,object,depth,time]% 
% !HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Holding(diver,object),time) ->
% Terminates(LetGoOf(diver,object),AtDepth(object,depth),time).
not(holds_at(atDepth(Diver, Depth), Time)), holds_at(holding(Diver, Object), Time) ->
	terminates(letGoOf(Diver, Object),
		   atDepth(Object, Depth),
		   Time).
% 
% 
% ecnet/Diving.e:174
% [diver,object,time]% 
% HoldsAt(UnderWater(diver),time) ->
% Initiates(LetGoOf(diver,object),UnderWater(object),time).
holds_at(underWater(Diver), Time) ->
	initiates(letGoOf(Diver, Object),
		  underWater(Object),
		  Time).
% 
% 
% ecnet/Diving.e:178
% [diver,object,time]% 
% !HoldsAt(UnderWater(diver),time) ->
% Terminates(LetGoOf(diver,object),UnderWater(object),time).
not(holds_at(underWater(Diver), Time)) ->
	terminates(letGoOf(Diver, Object),
		   underWater(Object),
		   Time).
% 
% 
% ecnet/Diving.e:182
% [diver,equipment,time]% 
% Initiates(PutOn(diver,equipment),Wearing(diver,equipment),time).
initiates(putOn(Diver,Equipment),
	  wearing(Diver,Equipment),
	  Time).
% 
% 
% ecnet/Diving.e:185
% [diver,equipment,time]% 
% Happens(PutOn(diver,equipment),time) ->
% !HoldsAt(UnderWater(diver),time).
happens(putOn(Diver, Equipment), Time) ->
	not(holds_at(underWater(Diver), Time)).
% 
% 
% ecnet/Diving.e:189
% [diver,equipment,time]% 
% Terminates(TakeOff(diver,equipment),Wearing(diver,equipment),time).
terminates(takeOff(Diver,Equipment),
	   wearing(Diver,Equipment),
	   Time).
% 
% 
% ecnet/Diving.e:192
% [diver,equipment,time]% 
% Terminates(Lose(diver,equipment),Wearing(diver,equipment),time).
terminates(lose(Diver,Equipment),
	   wearing(Diver,Equipment),
	   Time).
% 
% 
% fluent Vertical(diver)
fluent(vertical(diver)).
% 
% fluent HorizontalDown(diver)
fluent(horizontalDown(diver)).
% ecnet/Diving.e:198
% 
% fluent Inverted(diver)
fluent(inverted(diver)).
% 
% fluent HorizontalUp(diver)
fluent(horizontalUp(diver)).
% 
% xor Vertical, HorizontalDown, Inverted, HorizontalUp
xor([vertical,horizontalDown,inverted,horizontalUp]).
% ecnet/Diving.e:204
% 
% event RotatePitch(diver)
event(rotatePitch(diver)).
% 
% ecnet/Diving.e:207
% [diver,time]% 
% HoldsAt(Vertical(diver),time) ->
% Initiates(RotatePitch(diver),HorizontalDown(diver),time).
holds_at(vertical(Diver), Time) ->
	initiates(rotatePitch(Diver),
		  horizontalDown(Diver),
		  Time).
% 
% 
% ecnet/Diving.e:211
% [diver,time]% 
% HoldsAt(HorizontalDown(diver),time) ->
% Initiates(RotatePitch(diver),Inverted(diver),time).
holds_at(horizontalDown(Diver), Time) ->
	initiates(rotatePitch(Diver), inverted(Diver), Time).
% 
% 
% ecnet/Diving.e:215
% [diver,time]% 
% HoldsAt(HorizontalDown(diver),time) ->
% Terminates(RotatePitch(diver),HorizontalDown(diver),time).
holds_at(horizontalDown(Diver), Time) ->
	terminates(rotatePitch(Diver),
		   horizontalDown(Diver),
		   Time).
% 
% 
% ecnet/Diving.e:219
% [diver,time]% 
% HoldsAt(Inverted(diver),time) ->
% Initiates(RotatePitch(diver),HorizontalUp(diver),time).
holds_at(inverted(Diver), Time) ->
	initiates(rotatePitch(Diver),
		  horizontalUp(Diver),
		  Time).
% 
% 
% ecnet/Diving.e:223
% [diver,time]% 
% HoldsAt(Inverted(diver),time) ->
% Terminates(RotatePitch(diver),Inverted(diver),time).
holds_at(inverted(Diver), Time) ->
	terminates(rotatePitch(Diver), inverted(Diver), Time).
% 
% 
% ecnet/Diving.e:227
% [diver,time]% 
% HoldsAt(HorizontalUp(diver),time) ->
% Initiates(RotatePitch(diver),Vertical(diver),time).
holds_at(horizontalUp(Diver), Time) ->
	initiates(rotatePitch(Diver), vertical(Diver), Time).
% 
% 
% ecnet/Diving.e:231
% [diver,time]% 
% HoldsAt(HorizontalUp(diver),time) ->
% Terminates(RotatePitch(diver),HorizontalUp(diver),time).
holds_at(horizontalUp(Diver), Time) ->
	terminates(rotatePitch(Diver),
		   horizontalUp(Diver),
		   Time).
% 
% 
% event RotateYaw(diver)
event(rotateYaw(diver)).
% 
%; try taking out Holding condition here
% ecnet/Diving.e:238
% [diver,time]% 
% Happens(Ascend1(diver),time) &
% !Happens(RapidAscendToSurface(diver),time) &
% !({diver1} HoldsAt(Holding(diver,diver1),time)) ->
% Happens(RotateYaw(diver),time).
happens(ascend1(Diver), Time), not(happens(rapidAscendToSurface(Diver), Time)), not(exists([Diver1], holds_at(holding(Diver, Diver1), Time))) ->
	happens(rotateYaw(Diver), Time).
% 
% 
% ecnet/Diving.e:244
% fluent UnderWater(object)
fluent(underWater(object)).
% 
% ecnet/Diving.e:246
% [object,depth,time]% 
% depth>% 0 &
% HoldsAt(AtDepth(object,depth),time) ->
% HoldsAt(UnderWater(object),time).
Depth>0, holds_at(atDepth(Object, Depth), Time) ->
	holds_at(underWater(Object), Time).
% 
% 
% event EnterWater(object)
event(enterWater(object)).
% ecnet/Diving.e:252
% 
% event Surface(object)
event(surface(object)).
% 
% ecnet/Diving.e:255
% [object,time]% 
% Initiates(EnterWater(object),UnderWater(object),time).
initiates(enterWater(Object),underWater(Object),Time).
% 
% 
% ecnet/Diving.e:258
% [diver,time]% 
% Happens(EnterWater(diver),time) ->
% !{diver1} HoldsAt(Holding(diver1,diver),time).
happens(enterWater(Diver), Time) ->
	not(exists([Diver1],
		   holds_at(holding(Diver1, Diver), Time))).
% 
% 
% ecnet/Diving.e:262
% [object,depth,time]% 
% depth=% 0 ->
% Initiates(EnterWater(object),AtDepth(object,depth),time).
Depth=0 ->
	initiates(enterWater(Object),
		  atDepth(Object, Depth),
		  Time).
% 
% 
% ecnet/Diving.e:266
% [object,time]% 
% Terminates(Surface(object),UnderWater(object),time).
terminates(surface(Object),underWater(Object),Time).
% 
% 
% ecnet/Diving.e:269
% [diver,time]% 
% Terminates(Surface(diver),PositivelyBuoyant(diver),time).
terminates(surface(Diver),
	   positivelyBuoyant(Diver),
	   Time).
% 
% 
% ecnet/Diving.e:272
% [diver,time]% 
% Terminates(Surface(diver),NegativelyBuoyant(diver),time).
terminates(surface(Diver),
	   negativelyBuoyant(Diver),
	   Time).
% 
% 
% ecnet/Diving.e:275
% [diver,time]% 
% Terminates(Surface(diver),NeutrallyBuoyant(diver),time).
terminates(surface(Diver),
	   neutrallyBuoyant(Diver),
	   Time).
% 
% 
% ecnet/Diving.e:278
% [object,depth,time]% 
% Terminates(Surface(object),AtDepth(object,depth),time).
terminates(surface(Object),
	   atDepth(Object,Depth),
	   Time).
% 
% 
% ecnet/Diving.e:281
% [diver,time] % Happens(EnterWater(diver),time) ->
% HoldsAt(Vertical(diver),time).
happens(enterWater(Diver), Time) ->
	holds_at(vertical(Diver), Time).
% 
% 
% fluent StandingOn(diver,boat)
fluent(standingOn(diver,boat)).
% 
% event StandOn(diver,boat)
event(standOn(diver,boat)).
% ecnet/Diving.e:287
% 
% ecnet/Diving.e:288
% [diver,boat,time]% 
% Terminates(EnterWater(diver),StandingOn(diver,boat),time).
terminates(enterWater(Diver),
	   standingOn(Diver,Boat),
	   Time).
% 
% 
% ecnet/Diving.e:291
% [diver,boat,time]% 
% Initiates(StandOn(diver,boat),StandingOn(diver,boat),time).
initiates(standOn(Diver,Boat),
	  standingOn(Diver,Boat),
	  Time).
% 
% 
% fluent PositivelyBuoyant(diver)
fluent(positivelyBuoyant(diver)).
% 
% fluent NeutrallyBuoyant(diver)
fluent(neutrallyBuoyant(diver)).
% ecnet/Diving.e:297
% 
% fluent NegativelyBuoyant(diver)
fluent(negativelyBuoyant(diver)).
% 
% mutex PositivelyBuoyant, NeutrallyBuoyant, NegativelyBuoyant
mutex(positivelyBuoyant).
mutex(neutrallyBuoyant).
mutex(negativelyBuoyant).
% 
% ecnet/Diving.e:302
% [diver,time]% 
% HoldsAt(PositivelyBuoyant(diver),time) ->
% HoldsAt(UnderWater(diver),time).
holds_at(positivelyBuoyant(Diver), Time) ->
	holds_at(underWater(Diver), Time).
% 
% 
% ecnet/Diving.e:306
% [diver,time]% 
% HoldsAt(NeutrallyBuoyant(diver),time) ->
% HoldsAt(UnderWater(diver),time).
holds_at(neutrallyBuoyant(Diver), Time) ->
	holds_at(underWater(Diver), Time).
% 
% 
% ecnet/Diving.e:310
% [diver,time]% 
% HoldsAt(NegativelyBuoyant(diver),time) ->
% HoldsAt(UnderWater(diver),time).
holds_at(negativelyBuoyant(Diver), Time) ->
	holds_at(underWater(Diver), Time).
% 
% 
% event PressDeflateButton(diver,bc)
event(pressDeflateButton(diver,bc)).
% 
% ecnet/Diving.e:316
% event PressDumpButton(diver,bc)
event(pressDumpButton(diver,bc)).
% 
% event PressInflateButton(diver,bc)
event(pressInflateButton(diver,bc)).
% 
% ecnet/Diving.e:320
% [diver,bc,time]% 
% Happens(PressDeflateButton(diver,bc),time) ->
% HoldsAt(Vertical(diver),time) &
% HoldsAt(UnderWater(bc),time).
happens(pressDeflateButton(Diver, Bc), Time) ->
	holds_at(vertical(Diver), Time),
	holds_at(underWater(Bc), Time).
% 
% 
% ecnet/Diving.e:325
% [diver,bc,time]% 
% Happens(PressDumpButton(diver,bc),time) ->
% HoldsAt(Vertical(diver),time) &
% HoldsAt(UnderWater(bc),time).
happens(pressDumpButton(Diver, Bc), Time) ->
	holds_at(vertical(Diver), Time),
	holds_at(underWater(Bc), Time).
% 
% 
% ecnet/Diving.e:330
% [diver,bc,time] % Happens(PressDumpButton(diver,bc),time) ->
% HoldsAt(UncontrolledBuoyancy(diver),time).
happens(pressDumpButton(Diver, Bc), Time) ->
	holds_at(uncontrolledBuoyancy(Diver), Time).
% 
% 
% ecnet/Diving.e:333
% [diver,bc,time]% 
% HoldsAt(Wearing(diver,bc),time) ->
% Initiates(PressDeflateButton(diver,bc),NegativelyBuoyant(diver),time).
holds_at(wearing(Diver, Bc), Time) ->
	initiates(pressDeflateButton(Diver, Bc),
		  negativelyBuoyant(Diver),
		  Time).
% 
% 
% ecnet/Diving.e:337
% [diver,bc,time]% 
% HoldsAt(Wearing(diver,bc),time) ->
% Terminates(PressDeflateButton(diver,bc),NeutrallyBuoyant(diver),time).
holds_at(wearing(Diver, Bc), Time) ->
	terminates(pressDeflateButton(Diver, Bc),
		   neutrallyBuoyant(Diver),
		   Time).
% 
% 
% ecnet/Diving.e:341
% [diver,bc,time]% 
% HoldsAt(Wearing(diver,bc),time) ->
% Terminates(PressDeflateButton(diver,bc),PositivelyBuoyant(diver),time).
holds_at(wearing(Diver, Bc), Time) ->
	terminates(pressDeflateButton(Diver, Bc),
		   positivelyBuoyant(Diver),
		   Time).
% 
% 
% ecnet/Diving.e:345
% [diver,bc,time]% 
% HoldsAt(Wearing(diver,bc),time) ->
% Initiates(PressDumpButton(diver,bc),NegativelyBuoyant(diver),time).
holds_at(wearing(Diver, Bc), Time) ->
	initiates(pressDumpButton(Diver, Bc),
		  negativelyBuoyant(Diver),
		  Time).
% 
% 
% ecnet/Diving.e:349
% [diver,bc,time]% 
% HoldsAt(Wearing(diver,bc),time) ->
% Terminates(PressDumpButton(diver,bc),NeutrallyBuoyant(diver),time).
holds_at(wearing(Diver, Bc), Time) ->
	terminates(pressDumpButton(Diver, Bc),
		   neutrallyBuoyant(Diver),
		   Time).
% 
% 
% ecnet/Diving.e:353
% [diver,bc,time]% 
% HoldsAt(Wearing(diver,bc),time) ->
% Terminates(PressDumpButton(diver,bc),PositivelyBuoyant(diver),time).
holds_at(wearing(Diver, Bc), Time) ->
	terminates(pressDumpButton(Diver, Bc),
		   positivelyBuoyant(Diver),
		   Time).
% 
% 
% ecnet/Diving.e:357
% [diver,bc,time]% 
% HoldsAt(Wearing(diver,bc),time) ->
% Initiates(PressInflateButton(diver,bc),NeutrallyBuoyant(diver),time).
holds_at(wearing(Diver, Bc), Time) ->
	initiates(pressInflateButton(Diver, Bc),
		  neutrallyBuoyant(Diver),
		  Time).
% 
% 
% ecnet/Diving.e:361
% [diver,bc,time]% 
% HoldsAt(Wearing(diver,bc),time) ->
% Terminates(PressInflateButton(diver,bc),PositivelyBuoyant(diver),time).
holds_at(wearing(Diver, Bc), Time) ->
	terminates(pressInflateButton(Diver, Bc),
		   positivelyBuoyant(Diver),
		   Time).
% 
% 
% ecnet/Diving.e:365
% [diver,bc,time]% 
% HoldsAt(Wearing(diver,bc),time) ->
% Terminates(PressInflateButton(diver,bc),NegativelyBuoyant(diver),time).
holds_at(wearing(Diver, Bc), Time) ->
	terminates(pressInflateButton(Diver, Bc),
		   negativelyBuoyant(Diver),
		   Time).
% 
% 
% ecnet/Diving.e:369
% [diver,weight,time]% 
% HoldsAt(Wearing(diver,weight),time) ->
% Initiates(TakeOff(diver,weight),PositivelyBuoyant(diver),time).
holds_at(wearing(Diver, Weight), Time) ->
	initiates(takeOff(Diver, Weight),
		  positivelyBuoyant(Diver),
		  Time).
% 
% 
% ecnet/Diving.e:373
% [diver,weight,time]% 
% HoldsAt(Wearing(diver,weight),time) ->
% Terminates(TakeOff(diver,weight),NegativelyBuoyant(diver),time).
holds_at(wearing(Diver, Weight), Time) ->
	terminates(takeOff(Diver, Weight),
		   negativelyBuoyant(Diver),
		   Time).
% 
% 
% ecnet/Diving.e:377
% [diver,weight,time]% 
% HoldsAt(Wearing(diver,weight),time) ->
% Terminates(TakeOff(diver,weight),NeutrallyBuoyant(diver),time).
holds_at(wearing(Diver, Weight), Time) ->
	terminates(takeOff(Diver, Weight),
		   neutrallyBuoyant(Diver),
		   Time).
% 
% 
% fluent UncontrolledBuoyancy(diver)
fluent(uncontrolledBuoyancy(diver)).
% 
% ecnet/Diving.e:383
% event LoseBuoyancyControl(diver)
event(loseBuoyancyControl(diver)).
% 
% predicate IsInexperiencedDiver(diver)
predicate(isInexperiencedDiver(diver)).
% 
% ecnet/Diving.e:387
% [diver,time]% 
% Happens(LoseBuoyancyControl(diver),time) ->
% IsInexperiencedDiver(diver).
happens(loseBuoyancyControl(Diver), Time) ->
	isInexperiencedDiver(Diver).
% 
% 
% ecnet/Diving.e:391
% [diver,time]% 
% Initiates(LoseBuoyancyControl(diver),UncontrolledBuoyancy(diver),time).
initiates(loseBuoyancyControl(Diver),
	  uncontrolledBuoyancy(Diver),
	  Time).
% 
% 
% ecnet/Diving.e:394
% [diver,time]% 
% Initiates(LoseBuoyancyControl(diver),PositivelyBuoyant(diver),time).
initiates(loseBuoyancyControl(Diver),
	  positivelyBuoyant(Diver),
	  Time).
% 
% 
% ecnet/Diving.e:397
% [diver,time]% 
% Terminates(LoseBuoyancyControl(diver),NegativelyBuoyant(diver),time).
terminates(loseBuoyancyControl(Diver),
	   negativelyBuoyant(Diver),
	   Time).
% 
% 
% ecnet/Diving.e:400
% [diver,time]% 
% Terminates(LoseBuoyancyControl(diver),NeutrallyBuoyant(diver),time).
terminates(loseBuoyancyControl(Diver),
	   neutrallyBuoyant(Diver),
	   Time).
% 
% 
%; determining fluent
% fluent AscendDescendAmount(diver,depth)
fluent(ascendDescendAmount(diver,depth)).
% noninertial AscendDescendAmount
noninertial(ascendDescendAmount).
% ecnet/Diving.e:406
% 
% ecnet/Diving.e:407
% [diver,depth1,depth2,time]% 
% HoldsAt(AscendDescendAmount(diver,depth1),time) &
% HoldsAt(AscendDescendAmount(diver,depth2),time) ->
% depth1=depth2.
holds_at(ascendDescendAmount(Diver, Depth1), Time), holds_at(ascendDescendAmount(Diver, Depth2), Time) ->
	Depth1=Depth2.
% 
% 
% ecnet/Diving.e:412
% [diver,depth,time]% 
% Happens(Descend(diver,depth),time) ->
% HoldsAt(NegativelyBuoyant(diver),time) &
% ({depth1}
%  HoldsAt(AscendDescendAmount(diver,depth1),time) &
%  HoldsAt(AtDepth(diver,depth-depth1),time)).
happens(descend(Diver, Depth), Time) ->
	holds_at(negativelyBuoyant(Diver), Time),
	exists([Depth1],
	        (holds_at(ascendDescendAmount(Diver, Depth1), Time), holds_at(atDepth(Diver, Depth-Depth1), Time))).
% 
% ecnet/Diving.e:418
% 
% event KickUp(diver)
event(kickUp(diver)).
% 
% ecnet/Diving.e:421
% [diver,depth,time]% 
% Happens(Ascend(diver,depth),time) ->
% (HoldsAt(PositivelyBuoyant(diver),time) |
%  (HoldsAt(NeutrallyBuoyant(diver),time) & Happens(KickUp(diver),time))) &
% ({depth1}
%  HoldsAt(AscendDescendAmount(diver,depth1),time) &
%  HoldsAt(AtDepth(diver,depth+depth1),time)).
happens(ascend(Diver, Depth), Time) ->
	(   holds_at(positivelyBuoyant(Diver), Time)
	;   holds_at(neutrallyBuoyant(Diver), Time),
	    happens(kickUp(Diver), Time)
	),
	exists([Depth1],
	        (holds_at(ascendDescendAmount(Diver, Depth1), Time), holds_at(atDepth(Diver, Depth+Depth1), Time))).
% ecnet/Diving.e:427
% 
% 
% ecnet/Diving.e:429
% [diver,time]% 
% Happens(KickUp(diver),time) ->
% HoldsAt(Vertical(diver),time).
happens(kickUp(Diver), Time) ->
	holds_at(vertical(Diver), Time).
% 
% 
% event SwimAround(diver)
event(swimAround(diver)).
% 
% ecnet/Diving.e:435
% [diver,time]% 
% Happens(SwimAround(diver),time) ->
% HoldsAt(HorizontalDown(diver),time).
happens(swimAround(Diver), Time) ->
	holds_at(horizontalDown(Diver), Time).
% 
% 
%; signaling
% 
% ecnet/Diving.e:441
% event SignalDescend(diver,diver)
event(signalDescend(diver,diver)).
% 
% event SignalOutOfTime(diver,diver)
event(signalOutOfTime(diver,diver)).
% 
% event SignalAscend(diver,diver)
event(signalAscend(diver,diver)).
% 
%;[diver1,diver2,time]
%;Happens(SignalAscend(diver1,diver2),time) ->
%;Happens(SignalOutOfTime(diver1,diver2),time-1).
% ecnet/Diving.e:450
% 
%;[diver1,diver2,time]
%;Happens(SignalDescend(diver1,diver2),time) ->
%;HoldsAt(See(diver1,diver2),time) &
%;HoldsAt(See(diver2,diver1),time).
% 
%;[diver1,diver2,time]
%;Happens(SignalOutOfTime(diver1,diver2),time) ->
%;HoldsAt(See(diver1,diver2),time) &
%;HoldsAt(See(diver2,diver1),time).
% ecnet/Diving.e:460
% 
%;[diver1,diver2,time]
%;Happens(SignalAscend(diver1,diver2),time) ->
%;HoldsAt(See(diver1,diver2),time) &
%;HoldsAt(See(diver2,diver1),time).
% 
%;event LookAt(agent,object)
% ecnet/Diving.e:467
% 
%;fluent See(agent,object)
% 
%;[agent,object,time]
%;Initiates(LookAt(agent,object),See(agent,object),time).
% 
%;[agent,object1,object2,time]
%;object1!=object2 ->
%;Terminates(LookAt(agent,object1),
%;           See(agent,object2),
%;           time).
% ecnet/Diving.e:478
% 
% event Descend1(diver)
event(descend1(diver)).
% 
% event Ascend1(diver)
event(ascend1(diver)).
% 
%;[diver,object,time]
%;Terminates(Descend1(diver),See(diver,object),time).
% ecnet/Diving.e:485
% 
%;[diver,object,time]
%;Terminates(Ascend1(diver),See(diver,object),time).
% 
%;[diver,object,time]
%;Terminates(RotateYaw(diver),See(diver,object),time).
% ecnet/Diving.e:491
% 
% event RapidAscendToSurface(diver)
event(rapidAscendToSurface(diver)).
% 
% ecnet/Diving.e:494
% [diver,time]% 
% Happens(Descend1(diver),time) <->
% ({depth} Happens(Descend(diver,depth),time)).
happens(descend1(Diver), Time) <->
	exists([Depth],
	       happens(descend(Diver, Depth), Time)).
% 
% 
% ecnet/Diving.e:498
% [diver,time]% 
% Happens(Ascend1(diver),time) <->
% ({depth} Happens(Ascend(diver,depth),time)).
happens(ascend1(Diver), Time) <->
	exists([Depth],
	       happens(ascend(Diver, Depth), Time)).
% 
% 
% ecnet/Diving.e:502
% [diver,time]% 
% Happens(RapidAscendToSurface(diver),time) ->
% Happens(Ascend(diver,0),time).
happens(rapidAscendToSurface(Diver), Time) ->
	happens(ascend(Diver, 0), Time).
% 
% 
% event AscendLine(diver,line)
event(ascendLine(diver,line)).
% 
% ecnet/Diving.e:508
% [diver,line,time]% 
% Happens(AscendLine(diver,line),time) ->
% Happens(Ascend1(diver),time).
happens(ascendLine(Diver, Line), Time) ->
	happens(ascend1(Diver), Time).
% 
% 
% fluent Disoriented(diver)
fluent(disoriented(diver)).
% 
% ecnet/Diving.e:514
% event BecomeDisoriented(diver)
event(becomeDisoriented(diver)).
% 
% event BecomeReoriented(diver)
event(becomeReoriented(diver)).
% 
% ecnet/Diving.e:518
% [diver,time]% 
% Initiates(BecomeDisoriented(diver),Disoriented(diver),time).
initiates(becomeDisoriented(Diver),
	  disoriented(Diver),
	  Time).
% 
% 
% ecnet/Diving.e:521
% [diver,time]% 
% Terminates(BecomeReoriented(diver),Disoriented(diver),time).
terminates(becomeReoriented(Diver),
	   disoriented(Diver),
	   Time).
% 
% 
% fluent DisturbedSilt()
fluent(disturbedSilt()).
% 
% event DisturbSilt(diver)
event(disturbSilt(diver)).
% ecnet/Diving.e:527
% 
% ecnet/Diving.e:528
% [diver,time]% 
% Initiates(DisturbSilt(diver),DisturbedSilt(),time).
initiates(disturbSilt(Diver),disturbedSilt(),Time).
% 
% 
% ecnet/Diving.e:531
% [diver,time]% 
% Happens(BecomeDisoriented(diver),time) ->
% (!HoldsAt(DisturbedSilt(),time-1) &
%  HoldsAt(DisturbedSilt(),time)).
happens(becomeDisoriented(Diver), Time) ->
	not(holds_at(disturbedSilt(), Time-1)),
	holds_at(disturbedSilt(), Time).
% 
% 
% event Panic(diver)
event(panic(diver)).
% ecnet/Diving.e:537
% 
% ecnet/Diving.e:538
% [diver,time] % Happens(Panic(diver),time) ->
% HoldsAt(Disoriented(diver),time) |
% HoldsAt(UncontrolledBuoyancy(diver),time) |
% ({equipment} Happens(Lose(diver,equipment),time-1)) |
% Happens(Vomit(diver),time-1).
(   ( happens(panic(Diver), Time)->holds_at(disoriented(Diver), Time)
    )
;   holds_at(uncontrolledBuoyancy(Diver), Time)
;   exists([Equipment],
	   happens(lose(Diver, Equipment), Time-1))
;   happens(vomit(Diver), Time-1)
).
% 
% 
% ecnet/Diving.e:544
% event Vomit(diver)
event(vomit(diver)).
% 
%; conditions
% 
% fluent Unconscious(diver)
fluent(unconscious(diver)).
% 
% ecnet/Diving.e:550
% event GoUnconscious(diver)
event(goUnconscious(diver)).
% 
% event RegainConsciousness(diver)
event(regainConsciousness(diver)).
% 
% ecnet/Diving.e:554
% [diver,time]% 
% Initiates(GoUnconscious(diver),Unconscious(diver),time).
initiates(goUnconscious(Diver),
	  unconscious(Diver),
	  Time).
% 
% 
% ecnet/Diving.e:557
% [diver,time]% 
% Terminates(RegainConsciousness(diver),Unconscious(diver),time).
terminates(regainConsciousness(Diver),
	   unconscious(Diver),
	   Time).
% 
% 
% ecnet/Diving.e:560
% [diver,time]% 
% Happens(GoUnconscious(diver),time) ->
% Happens(RapidAscendToSurface(diver),time).
happens(goUnconscious(Diver), Time) ->
	happens(rapidAscendToSurface(Diver), Time).
% 
% 
% fluent HasEarPain(diver)
fluent(hasEarPain(diver)).
% 
% ecnet/Diving.e:566
% event StartEarPain(diver)
event(startEarPain(diver)).
% 
% ecnet/Diving.e:568
% [diver,time] % Initiates(StartEarPain(diver),HasEarPain(diver),time).
initiates(startEarPain(Diver),hasEarPain(Diver),Time).
% 
% 
% fluent HasRupturedEardrum(diver)
fluent(hasRupturedEardrum(diver)).
% 
% event RuptureEardrum(diver)
event(ruptureEardrum(diver)).
% 
% ecnet/Diving.e:574
% [diver,time]% 
% Initiates(RuptureEardrum(diver),HasRupturedEardrum(diver),time).
initiates(ruptureEardrum(Diver),
	  hasRupturedEardrum(Diver),
	  Time).
% 
% fluent ConditionOK(diver)
fluent(conditionOK(diver)).
% 
% fluent HasDecompressionIllness(diver)
fluent(hasDecompressionIllness(diver)).
% 
% ecnet/Diving.e:580
% event StartDecompressionIllness(diver)
event(startDecompressionIllness(diver)).
% 
% ecnet/Diving.e:582
% [diver,time]% 
% Initiates(StartDecompressionIllness(diver),
%           HasDecompressionIllness(diver),
%           time).
initiates(startDecompressionIllness(Diver),
	  hasDecompressionIllness(Diver),
	  Time).
% 
% 
% fluent SignalingDecompress(computer,diver)
fluent(signalingDecompress(computer,diver)).
% ecnet/Diving.e:588
% 
% fluent SignalingLowOnAir(computer,airtank,diver)
fluent(signalingLowOnAir(computer,airtank,diver)).
% 
% ecnet/Diving.e:591
% [computer,airtank,diver,time]% 
% HoldsAt(SignalingLowOnAir(computer,airtank,diver),time) ->
% HoldsAt(LowOnAir(airtank),time).
holds_at(signalingLowOnAir(Computer, Airtank, Diver), Time) ->
	holds_at(lowOnAir(Airtank), Time).
% 
% 
% ecnet/Diving.e:595
% [computer,diver,time]% 
% HoldsAt(SignalingDecompress(computer,diver),time) ->
% !{time1} time1<time & Happens(Decompress(diver),time1).
holds_at(signalingDecompress(Computer, Diver), Time) ->
	not(exists([Time1],
		    (Time1<Time, happens(decompress(Diver), Time1)))).
% 
% 
% event Decompress(diver)
event(decompress(diver)).
% 
% ecnet/Diving.e:601
% event EqualizeEars(diver)
event(equalizeEars(diver)).
% 
% ecnet/Diving.e:603
% [diver,time]% 
% (Happens(Descend1(diver),time) | Happens(Ascend1(diver),time)) &
% !Happens(EqualizeEars(diver),time) ->
% Happens(StartEarPain(diver),time) &
% Happens(RuptureEardrum(diver),time).
(happens(descend1(Diver), Time);happens(ascend1(Diver), Time)), not(happens(equalizeEars(Diver), Time)) ->
	happens(startEarPain(Diver), Time),
	happens(ruptureEardrum(Diver), Time).
% 
% 
% ecnet/Diving.e:609
% [diver,time]% 
% Happens(Ascend1(diver),time) &
% !Happens(Decompress(diver),time) ->
% Happens(StartDecompressionIllness(diver),time).
happens(ascend1(Diver), Time), not(happens(decompress(Diver), Time)) ->
	happens(startDecompressionIllness(Diver), Time).
% 
% 
% ecnet/Diving.e:614
% [diver1,diver2,time]% 
% HoldsAt(Holding(diver1,diver2),time) &
% Happens(Ascend1(diver1),time) &
% !Happens(Decompress(diver2),time) ->
% Happens(StartDecompressionIllness(diver2),time).
holds_at(holding(Diver1, Diver2), Time), happens(ascend1(Diver1), Time), not(happens(decompress(Diver2), Time)) ->
	happens(startDecompressionIllness(Diver2), Time).
% 
% 
% ecnet/Diving.e:620
% [diver,time]% 
% Happens(Decompress(diver),time) ->
% ({depth} depth>0 & HoldsAt(AtDepth(diver,depth),time)) &
% !HoldsAt(UncontrolledBuoyancy(diver),time).
happens(decompress(Diver), Time) ->
	exists([Depth],
	        (Depth>0, holds_at(atDepth(Diver, Depth), Time))),
	not(holds_at(uncontrolledBuoyancy(Diver), Time)).
% 
% 
% fluent HasHeadache(diver)
fluent(hasHeadache(diver)).
% ecnet/Diving.e:626
% 
% ecnet/Diving.e:627
% [diver,time]% 
% HoldsAt(ConditionOK(diver),time) ->
% !HoldsAt(Unconscious(diver),time) &
% !HoldsAt(HasEarPain(diver),time) &
% !HoldsAt(HasRupturedEardrum(diver),time) &
% !HoldsAt(HasDecompressionIllness(diver),time) &
% !HoldsAt(HasHeadache(diver),time).
holds_at(conditionOK(Diver), Time) ->
	not(holds_at(unconscious(Diver), Time)),
	not(holds_at(hasEarPain(Diver), Time)),
	not(holds_at(hasRupturedEardrum(Diver), Time)),
	not(holds_at(hasDecompressionIllness(Diver), Time)),
	not(holds_at(hasHeadache(Diver), Time)).
% ecnet/Diving.e:633
% 
% 
% event BeAirlifted(diver)
event(beAirlifted(diver)).
% 
% event TakeInWater(diver)
event(takeInWater(diver)).
% 
% ecnet/Diving.e:639
% fluent LowOnAir(airtank)
fluent(lowOnAir(airtank)).
% 
% event BecomeLowOnAir(airtank)
event(becomeLowOnAir(airtank)).
% 
% ecnet/Diving.e:643
% [airtank,time]% 
% Initiates(BecomeLowOnAir(airtank),LowOnAir(airtank),time).
initiates(becomeLowOnAir(Airtank),
	  lowOnAir(Airtank),
	  Time).
% 
% 
%; initial state
% ecnet/Diving.e:647
% [diver] % HoldsAt(ConditionOK(diver),0).
holds_at(conditionOK(Diver),0).
% 
% ecnet/Diving.e:648
% [diver] % HoldsAt(Vertical(diver),0).
holds_at(vertical(Diver),0).
% 
% !HoldsAt(DisturbedSilt(),0).
not(holds_at(disturbedSilt(),0)).
% 
% ecnet/Diving.e:650
% [diver] % !HoldsAt(UncontrolledBuoyancy(diver),0).
not(holds_at(uncontrolledBuoyancy(Diver),0)).
% 
% ecnet/Diving.e:651
% [diver] % !HoldsAt(Disoriented(diver),0).
not(holds_at(disoriented(Diver),0)).
% 
% ecnet/Diving.e:652
% [diver] % !HoldsAt(PositivelyBuoyant(diver),0) &
%         !HoldsAt(NeutrallyBuoyant(diver),0) &
%         !HoldsAt(NegativelyBuoyant(diver),0).
not(holds_at(positivelyBuoyant(Diver),0)) ',' not(holds_at(neutrallyBuoyant(Diver),0)) ',' not(holds_at(negativelyBuoyant(Diver),0)).
% 
% ecnet/Diving.e:655
% [diver,object] % !HoldsAt(Wearing(diver,object),0).
not(holds_at(wearing(Diver,Object),0)).
% 
% ecnet/Diving.e:656
% [diver,object] % !HoldsAt(Holding(diver,object),0).
not(holds_at(holding(Diver,Object),0)).
% 
% ecnet/Diving.e:657
% [diver1,diver2] % !HoldsAt(Separated(diver1,diver2),0).
not(holds_at(separated(Diver1,Diver2),0)).
% 
%;[agent,object] !HoldsAt(See(agent,object),0).
% 
% fluent Separated(diver,diver)
fluent(separated(diver,diver)).
% 
% ecnet/Diving.e:662
% [diver1,diver2,time]% 
% HoldsAt(Separated(diver1,diver2),time) ->
% HoldsAt(Separated(diver2,diver1),time).
holds_at(separated(Diver1, Diver2), Time) ->
	holds_at(separated(Diver2, Diver1), Time).
% 
% 
% event BecomeSeparated(diver,diver)
event(becomeSeparated(diver,diver)).
% 
% ecnet/Diving.e:668
% event BeReunitedWith(diver,diver)
event(beReunitedWith(diver,diver)).
% 
% ecnet/Diving.e:670
% [diver1,diver2,time]% 
% Initiates(BecomeSeparated(diver1,diver2),Separated(diver1,diver2),time).
initiates(becomeSeparated(Diver1,Diver2),
	  separated(Diver1,Diver2),
	  Time).
% 
% 
% ecnet/Diving.e:673
% [diver1,diver2,time]% 
% Initiates(BecomeSeparated(diver1,diver2),Separated(diver2,diver1),time).
initiates(becomeSeparated(Diver1,Diver2),
	  separated(Diver2,Diver1),
	  Time).
% 
% 
% ecnet/Diving.e:676
% [diver1,diver2,time]% 
% Terminates(BeReunitedWith(diver1,diver2),Separated(diver1,diver2),time).
terminates(beReunitedWith(Diver1,Diver2),
	   separated(Diver1,Diver2),
	   Time).
% 
% 
% ecnet/Diving.e:679
% [diver1,diver2,time]% 
% Terminates(BeReunitedWith(diver1,diver2),Separated(diver2,diver1),time).
terminates(beReunitedWith(Diver1,Diver2),
	   separated(Diver2,Diver1),
	   Time).
% 
% 
%; End of file.
% ecnet/Diving.e:683
% translate: ending  File: ecnet/Diving.e.pro 
