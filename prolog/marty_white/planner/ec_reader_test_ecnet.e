


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/WritingABook.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Mueller:2004c,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Understanding script-based stories using commonsense reasoning",
;   journal = "Cognitive Systems Research",
;   volume = "5",
;   number = "4",
;   pages = "307--340",
; }
;

option modeldiff on

ignore Love, ThreatenedBy
ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
ignore Inside
ignore Near, WalkFrom, WalkFromTo, RunFromTo
ignore See

option renaming off

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/HandTo.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2004c/Cognition.e

door Door1

room Room0

room Room1

Side1(Door1)=Room0.
Side2(Door1)=Room1.

agent Writer1

paper Paper1

pen Pen1

chair Chair1

physobj Desk1

; initial state
[agent,object] !HoldsAt(Holding(agent,object),0).
[agent,physobj] !HoldsAt(SittingOn(agent,physobj),0).
[agent,physobj] !HoldsAt(LyingOn(agent,physobj),0).
[physobj1,physobj2]
!(physobj1=Pen1 & physobj2=Desk1) &
!(physobj1=Paper1 & physobj2=Desk1) ->
!HoldsAt(On(physobj1, physobj2),0).
HoldsAt(On(Paper1,Desk1),0).
HoldsAt(On(Pen1,Desk1),0).
HoldsAt(Dressed(Writer1),0).
HoldsAt(Awake(Writer1),0).
HoldsAt(Sleep3(Writer1),0).
HoldsAt(Standing(Writer1),0).
HoldsAt(DoorUnlocked(Door1),0).
HoldsAt(DoorIsOpen(Door1),0).
HoldsAt(At(Writer1,Room0),0).
HoldsAt(At(Chair1,Room1),0).
HoldsAt(At(Desk1,Room1),0).

; narrative
Happens(WalkThroughDoor12(Writer1,Door1),0).
Happens(SitOn(Writer1,Chair1),1).
Happens(TakeOffOf(Writer1,Pen1,Desk1),2).
Happens(Think(Writer1),3).
Happens(WriteOn(Writer1,Paper1,Pen1),4).
Happens(WriteOn(Writer1,Paper1,Pen1),5).
Happens(PlaceOn(Writer1,Pen1,Desk1),6).
Happens(RiseFrom(Writer1,Chair1),7).
Happens(WalkThroughDoor21(Writer1,Door1),8).

range time 0 9
range offset 0 0
range diameter 0 0

completion Happens

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/SmallFire.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; SmallFire: matches, lighters, cigarettes, etc.
;

event Light(agent,physobj)
event LightWith(agent,physobj,physobj)
event PutOut(agent,physobj)
event BlowOut(agent,physobj)
fluent IsBurning(physobj)

[agent,physobj1,physobj2,time]
HoldsAt(IsBurning(physobj2),time) ->
Initiates(LightWith(agent,physobj1,physobj2),
          IsBurning(physobj1),
          time).

[agent,physobj1,physobj2,time]
Happens(LightWith(agent,physobj1,physobj2),time) ->
HoldsAt(Holding(agent,physobj1),time) &
HoldsAt(Holding(agent,physobj2),time) &
!HoldsAt(IsBurning(physobj1),time).

[agent,physobj,time]
Initiates(Light(agent,physobj),
          IsBurning(physobj),
          time).

[agent,physobj,time]
Happens(Light(agent,physobj),time) ->
HoldsAt(Holding(agent,physobj),time) &
!HoldsAt(IsBurning(physobj),time).

[agent,physobj,time]
Terminates(PutOut(agent,physobj),
           IsBurning(physobj),
           time).

[agent,physobj,time]
Happens(PutOut(agent,physobj),time) ->
HoldsAt(Holding(agent,physobj),time) &
HoldsAt(IsBurning(physobj),time).

[agent,physobj,time]
Terminates(BlowOut(agent,physobj),
           IsBurning(physobj),
           time).

[agent,physobj,time]
Happens(BlowOut(agent,physobj),time) ->
HoldsAt(Holding(agent,physobj),time) &
HoldsAt(IsBurning(physobj),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/PolySpace.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @phdthesis{Cassimatis:2002,
;   author = "Nicholas L. Cassimatis",
;   year = "2002",
;   title = "Polyscheme: A Cognitive Architecture for Integrating Multiple Representation and Inference Schemes",
;   address = "Cambridge, MA",
;   school = "Program in Media Arts and Sciences, School of Architecture and Planning, Massachusetts Institute of Technology",
; }
;

; sorts
sort object
sort xcoord: integer
sort ycoord: integer
sort grid
sort shape
sort color

; constants
shape Round,Square
color Red,Green

; predicates, fluents, and events
predicate Equal(object,object)
predicate Shape(object,shape)
predicate Color(object,color)
fluent Location(grid,object,xcoord,ycoord)
event Move(grid,object,xcoord,ycoord,xcoord,ycoord)

; axioms

[object1,object2] Equal(object1,object2) -> Equal(object2,object1).

; objects have unique shape
[object,shape1,shape2]
Shape(object,shape1) & Shape(object,shape2) ->
shape1=shape2.

; objects have unique color
[object,color1,color2]
Color(object,color1) & Color(object,color2) ->
color1=color2.

; if objects are the same, they have the same shape
[object1,object2]
Equal(object1,object2) ->
({shape} Shape(object1,shape) & Shape(object2,shape)).

; if objects are the same, they have the same color
[object1,object2]
Equal(object1,object2) ->
({color} Color(object1,color) & Color(object2,color)).

; if objects are the same, they have the same location
[grid,object1,object2,xcoord1,ycoord1,xcoord2,ycoord2,time]
Equal(object1,object2) ->
(HoldsAt(Location(grid,object1,xcoord1,ycoord1),time) &
 HoldsAt(Location(grid,object2,xcoord2,ycoord2),time) ->
 xcoord1=xcoord2 & ycoord1=ycoord2).

; object in one location at a time
[grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
HoldsAt(Location(grid,object,xcoord1,ycoord1),time) &
HoldsAt(Location(grid,object,xcoord2,ycoord2),time) ->
xcoord1=xcoord2 & ycoord1=ycoord2.

; objects have locations
[grid,object,time]
({xcoord,ycoord} HoldsAt(Location(grid,object,xcoord,ycoord),time)).

; different objects are not at same location
[grid,object1,object2,xcoord1,ycoord1,time]
HoldsAt(Location(grid,object1,xcoord1,ycoord1),time) &
HoldsAt(Location(grid,object2,xcoord1,ycoord1),time) ->
Equal(object1,object2).

; moving to a location causes an object to be at that location
[grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
Initiates(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),
          Location(grid,object,xcoord2,ycoord2),
          time).

; moving to a location causes the object no longer to be at its previous
; location
[grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
Terminates(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),
           Location(grid,object,xcoord1,ycoord1),
           time).

;; allow diagonal movements
;[grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
;Happens(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),time) ->
;HoldsAt(Location(grid,object,xcoord1,ycoord1),time) &
;(xcoord1=xcoord2 |
; xcoord1=xcoord2+1 |
; xcoord1=xcoord2-1) &
;(ycoord1=ycoord2 |
; ycoord1=ycoord2+1 |
; ycoord1=ycoord2-1).

; only allow right angle movements
[grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
Happens(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),time) ->
HoldsAt(Location(grid,object,xcoord1,ycoord1),time) &
((xcoord1=xcoord2 & (ycoord1=ycoord2+1 | ycoord1=ycoord2-1)) |
 (ycoord1=ycoord2 & (xcoord1=xcoord2+1 | xcoord1=xcoord2-1))).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Rain.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; Rain
;

; It starts raining at location outside.
event StartRaining(outside)

; It stops raining at location outside.
event StopRaining(outside)

; It is raining at location outside.
fluent Raining(outside)

event GetWet(object)

event Dry(object)

fluent Wet(object)

[agent,outside,time]
HoldsAt(At(agent,outside),time) &
HoldsAt(Raining(outside),time) &
!HoldsAt(Wet(agent),time) &
(!{umbrella} HoldsAt(Holding(agent,umbrella),time)) ->
Happens(GetWet(agent),time).

[object,time]
Initiates(GetWet(object),Wet(object),time).

[object,time]
Terminates(Dry(object),Wet(object),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/OMSpace.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; OMSpace: object-scale metric space
;
; The OMSpace representation deals with metric space at
; the scale of objects.
;
; @article{Morgenstern:2001,
;   author = "Morgenstern, Leora",
;   year = "2001",
;   title = "Mid-sized axiomatizations of commonsense problems: A case study in egg cracking",
;   journal = "Studia Logica",
;   volume = "67",
;   pages = "333--384",
; }
;
; @article{Shanahan:2003,
;   author = "Shanahan, Murray",
;   year = "2004",
;   title = "An attempt to formalise a non-trivial benchmark problem in common sense reasoning",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "141--165",
; }
;

sort height: integer
sort distance: integer

; Height

; The height of object is height.
fluent Height(object,height)

; State constraint represent the fact that each
; object has a unique height:
[object,height1,height2,time]
HoldsAt(Height(object,height1),time) &
HoldsAt(Height(object,height2),time) ->
height1=height2.

[object,time]
{height}
HoldsAt(Height(object,height),time).

; falling

; physobj1 is falling from physobj2 to physobj3.
fluent FallingFromTo(physobj,physobj,physobj)
; physobj1 starts falling from physobj2 to physobj3.
event StartFallingFromTo(physobj,physobj,physobj)
; physobj1 collides with physobj2.
event CollideWith(physobj,physobj)

; An effect axiom states that if a first physical object starts
; falling from a second physical object to a third physical
; object, the first physical object will be falling from the
; second physical object to the third physical object:
[physobj1,physobj2,physobj3,time]
Initiates(StartFallingFromTo(physobj1,physobj2,physobj3),
          FallingFromTo(physobj1,physobj2,physobj3),
          time).

; A precondition axiom states that for
; a first physical object to start
; falling from a second physical object to a third physical
; object,
; the height of the first physical object and the
; second physical object must be the same.
[physobj1,physobj2,physobj3,height1,height2,time]
Happens(StartFallingFromTo(physobj1,physobj2,physobj3),time) &
HoldsAt(Height(physobj1,height1),time) &
HoldsAt(Height(physobj2,height2),time) ->
height1=height2.

; A state constraint says that a physical object
; cannot fall from itself, cannot fall to itself,
; and cannot fall from and to the same physical object:
[physobj1,physobj2,physobj3,time]
HoldsAt(FallingFromTo(physobj1,physobj2,physobj3),time) ->
physobj1!=physobj2 &
physobj1!=physobj3 &
physobj2!=physobj3.

; A state constraint says that the sky cannot fall:
[sky,physobj1,physobj2,time]
!HoldsAt(FallingFromTo(sky,physobj1,physobj2),time).

; A releases axiom states that if
; if a first physical object starts
; falling from a second physical object to a third physical
; object, the height of the first physical object
; will be released from inertia:
[physobj1,physobj2,physobj3,height,time]
Releases(StartFallingFromTo(physobj1,physobj2,physobj3),
         Height(physobj1,height),
         time).

; A trajectory axiom states that
; if a first physical object starts falling
; from a second physical object
; to a third physical object
; at a time and
; the first physical object has a height at the time,
; then the first physical object will have a height
; equal to the height minus an offset
; at a time equal to the time plus the offset:
[physobj1,physobj2,physobj3,height1,height2,offset,time]
HoldsAt(Height(physobj1,height1),time) &
height2=height1-offset ->
Trajectory(FallingFromTo(physobj1,physobj2,physobj3),time,
           Height(physobj1,height2),offset).

; A trigger axiom states that
; if a first physical object is falling
; from a second physical object
; to a third physical object and
; the height of the first physical object
; is the same as the height of the third physical object,
; the first physical object collides with the
; third physical object:
[physobj1,physobj2,physobj3,height,time]
HoldsAt(FallingFromTo(physobj1,physobj2,physobj3),time) &
HoldsAt(Height(physobj1,height),time) &
HoldsAt(Height(physobj3,height),time) ->
Happens(CollideWith(physobj1,physobj3),time).

; An effect axiom states that
; if a first physical object is falling
; from a second physical object
; to a third physical object and
; the first physical object collides with
; the third physical object,
; the first physical object will be on the third physical object:
[physobj1,physobj2,physobj3,time]
HoldsAt(FallingFromTo(physobj1,physobj2,physobj3),time) ->
Initiates(CollideWith(physobj1,physobj3),
          On(physobj1,physobj3),
          time).

; An effect axiom states that
; if a physical object collides with another
; physical object,
; the height of the first physical object will
; be the height of the second physical object:
[physobj1,physobj2,height,time]
HoldsAt(Height(physobj2,height),time) ->
Initiates(CollideWith(physobj1,physobj2),
          Height(physobj1,height),
          time).

;[physobj1,physobj2,height1,height2,time]
;HoldsAt(Height(physobj2,height1),time) &
;height1 != height2 ->
;Terminates(CollideWith(physobj1,physobj2),
;           Height(physobj1,height2),
;           time).

; An effect axiom states that
; if a first physical object is falling
; from a second physical object
; to a third physical object and
; the first physical object collides with
; the third physical object,
; the first physical object will no longer be
; falling from the second physical object to the
; third physical object:
[physobj1,physobj2,physobj3,time]
HoldsAt(FallingFromTo(physobj1,physobj2,physobj3),time) ->
Terminates(CollideWith(physobj1,physobj3),
           FallingFromTo(physobj1,physobj2,physobj3),
           time).

; flying

; agent is flying from physobj1 to physobj2.
fluent FlyingFromTo(agent,physobj,physobj)
; agent starts flying from physobj1 to physobj2.
event StartFlyingFromTo(agent,physobj,physobj)
; agent reaches physobj.
event Reach(agent,physobj)

; An effect axiom states that if an agent starts
; flying from a physical object to another physical object,
; the agent will be flying from the first physical object
; to the second physical object:
[agent,physobj1,physobj2,time]
Initiates(StartFlyingFromTo(agent,physobj1,physobj2),
          FlyingFromTo(agent,physobj1,physobj2),
          time).

; A precondition axiom states that for
; an agent to start flying from a physical object to
; another physical object,
; the height of the agent and
; the first physical object must be the same:
[agent,physobj1,physobj2,height1,height2,time]
Happens(StartFlyingFromTo(agent,physobj1,physobj2),time) &
HoldsAt(Height(agent,height1),time) &
HoldsAt(Height(physobj1,height2),time) ->
height1=height2.

; A state constraint says that an agent
; cannot fly from and to the same physical object:
[agent,physobj1,physobj2,time]
HoldsAt(FlyingFromTo(agent,physobj1,physobj2),time) ->
physobj1!=physobj2.

; A releases axiom states that if an agent
; starts flying from a physical object to another
; physical object, the height of the agent will
; be released from inertia:
[agent,physobj1,physobj2,height,time]
Releases(StartFlyingFromTo(agent,physobj1,physobj2),
         Height(agent,height),
         time).

; A trajectory axiom states that
; if an agent starts flying from
; from a physical object
; to another physical object
; at a time and
; the agent has a height at the time,
; then the agent will have a height
; equal to the height plus an offset
; at a time equal to the time plus the offset:
[agent,physobj1,physobj2,height1,height2,offset,time]
HoldsAt(Height(agent,height1),time) &
height2=height1+offset ->
Trajectory(FlyingFromTo(agent,physobj1,physobj2),time,
           Height(agent,height2),offset).

; A trigger axiom states that
; if an agent is flying
; from a physical object
; to another physical object and
; the height of the agent
; is the same as the height of the second physical object,
; the agent reaches the second physical object:
[agent,physobj1,physobj2,height,time]
HoldsAt(FlyingFromTo(agent,physobj1,physobj2),time) &
HoldsAt(Height(agent,height),time) &
HoldsAt(Height(physobj2,height),time) ->
Happens(Reach(agent,physobj2),time).

; An effect axiom states that
; if an agent reaches a physical object,
; the height of the agent will be the
; height of the physical object:
[agent,physobj,height,time]
HoldsAt(Height(physobj,height),time) ->
Initiates(Reach(agent,physobj),Height(agent,height),time).

;[agent,physobj,height1,height2,time]
;HoldsAt(Height(physobj,height1),time) &
;height1!=height2 ->
;Terminates(Reach(agent,physobj),Height(agent,height2),time).

; An effect axiom states that
; if an agent is flying
; from a physical object
; to another physical object and
; the agent reaches the second physical object,
; the agent will no longer be
; flying from the first physical object
; to the second physical object:
[agent,physobj1,physobj2,time]
HoldsAt(FlyingFromTo(agent,physobj1,physobj2),time) ->
Terminates(Reach(agent,physobj2),
           FlyingFromTo(agent,physobj1,physobj2),
           time).

; A releases axiom states that
; if an agent holds a physical object,
; the height of the physical object is released from inertia:
[agent,physobj,height,time]
Releases(Hold(agent,physobj),Height(physobj,height),time).

;[agent,physobj,height1,height2,time]
;(!{object} PartOf(physobj,object)) &
;HoldsAt(Height(physobj,height1),time) &
;height1 != height2 ->
;Terminates(LetGoOf(agent,physobj),Height(physobj,height2),time).

[agent,physobj,height,time]
(!{object} PartOf(physobj,object)) &
HoldsAt(Height(physobj,height),time) ->
Initiates(LetGoOf(agent,physobj),Height(physobj,height),time).

; A state constraint says that
; if an agent is holding a physical object and
; the height of the agent is height,
; the height of the physical object is height:
[agent,physobj,height,time]
HoldsAt(Holding(agent,physobj),time) &
HoldsAt(Height(agent,height),time) ->
HoldsAt(Height(physobj,height),time).

; A state constraint says that if a physical object
; is part of an object,
; the height of the physical object
; is the same as the height of the object:
[physobj,object,height,time]
PartOf(physobj,object) &
HoldsAt(Height(object,height),time) ->
HoldsAt(Height(physobj,height),time).

;event Catch(agent,physobj)
;event HitFromTo(agent,physobj,object,object)
;fluent Distance(physobj,physobj,distance)
;fluent FlyingAcrossFromTo(physobj,object,object)

;[agent,physobj1,physobj2,physobj3,time]
;Initiates(HitFromTo(agent,physobj1,physobj2,physobj3),
;          FlyingAcrossFromTo(physobj1,physobj2,physobj3),
;          time).

;[agent,physobj1,physobj2,physobj3,distance,time]
;Releases(HitFromTo(agent,physobj1,physobj2,physobj3),
;         Distance(physobj1,physobj2,distance),
;         time).

;[agent,physobj1,physobj2,physobj3,distance,time]
;Releases(HitFromTo(agent,physobj1,physobj2,physobj3),
;         Distance(physobj1,physobj3,distance),
;         time).

;[physobj1,physobj2,physobj3,offset,time]
;Trajectory(FlyingAcrossFromTo(physobj1,physobj2,physobj3),time,
;           Distance(physobj1,physobj2,offset),offset).

;[physobj1,physobj2,physobj3,distance1,distance2,offset,time]
;HoldsAt(Distance(physobj2,physobj3,distance1),time) &
;distance2 = distance1 - time ->
;Trajectory(FlyingAcrossFromTo(physobj1,physobj2,physobj3),time,
;           Distance(physobj1,physobj3,distance2),offset).

;[agent,physobj1,physobj2,physobj3,time]
;HoldsAt(FlyingAcrossFromTo(physobj1,physobj2,physobj3),time) ->
;Initiates(Catch(agent,physobj1),
;          Holding(agent,physobj1),
;          time).

;[agent,physobj1,physobj2,physobj3,time]
;HoldsAt(FlyingAcrossFromTo(physobj1,physobj2,physobj3),time) ->
;Terminates(Catch(agent,physobj1),
;           FlyingAcrossFromTo(physobj1,physobj2,physobj3),
;           time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Feeling.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; feeling = emotion, attitude, ...
;
; The Feeling representation includes simple positive, neutral, and
; negative emotions, and positive, neutral, and negative attitudes
; toward objects.
;

; emotions

; agent is happy.
fluent Happy(agent)

; agent is emotionally neutral or calm.
fluent Calm(agent)

; agent is unhappy.
fluent Unhappy(agent)

; At any moment, an agent is in one of three emotional states:
xor Happy, Calm, Unhappy

; agent becomes happy.
event BecomeHappy(agent)

; agent becomes calm.
event BecomeCalm(agent)

; agent becomes unhappy.
event BecomeUnhappy(agent)

; A number of effect and precondition axioms deal with the transitions
; from one emotional state to another:
[agent,time]
Initiates(BecomeHappy(agent),Happy(agent),time).

[agent,time]
HoldsAt(Calm(agent),time) ->
Terminates(BecomeHappy(agent),Calm(agent),time).

[agent,time]
HoldsAt(Unhappy(agent),time) ->
Terminates(BecomeHappy(agent),Unhappy(agent),time).

[agent,time]
Happens(BecomeHappy(agent),time) ->
!HoldsAt(Happy(agent),time).

[agent,time]
Initiates(BecomeCalm(agent),Calm(agent),time).

[agent,time]
HoldsAt(Happy(agent),time) ->
Terminates(BecomeCalm(agent),Happy(agent),time).

[agent,time]
HoldsAt(Unhappy(agent),time) ->
Terminates(BecomeCalm(agent),Unhappy(agent),time).

[agent,time]
Happens(BecomeCalm(agent),time) -> !HoldsAt(Calm(agent),time).

[agent,time]
Initiates(BecomeUnhappy(agent),Unhappy(agent),time).

[agent,time]
HoldsAt(Happy(agent),time) ->
Terminates(BecomeUnhappy(agent),Happy(agent),time).

[agent,time]
HoldsAt(Calm(agent),time) ->
Terminates(BecomeUnhappy(agent),Calm(agent),time).

[agent,time]
Happens(BecomeUnhappy(agent),time) -> !HoldsAt(Unhappy(agent),time).

; anger

fluent AngryAt(agent,agent)

event BecomeAngryAt(agent,agent)

[agent1,agent2,time]
Initiates(BecomeAngryAt(agent1,agent2),AngryAt(agent1,agent2),time).

[agent1,agent2,time]
Terminates(BecomeHappy(agent1),AngryAt(agent1,agent2),time).

[agent1,agent2,time]
Happens(BecomeAngryAt(agent1,agent2),time) ->
Happens(BecomeUnhappy(agent1),time).

; attitudes

; agent likes object.
fluent Like(agent,object)
; agent loves object.
fluent Love(agent,object)
; agent dislikes object.
fluent Dislike(agent,object)

; agent likes snow.
fluent LikeSnow(agent)

; A trigger axiom states that
; if an agent is awake, likes snow, and is in a room that
; looks out onto a location where it is snowing, that agent
; becomes happy:
[agent,room,outside,time]
!HoldsAt(Happy(agent),time) &
HoldsAt(Awake(agent),time) &
HoldsAt(LikeSnow(agent),time) &
HoldsAt(At(agent,room),time) &
LookOutOnto(room)=outside &
HoldsAt(Snowing(outside),time) ->
Happens(BecomeHappy(agent),time).

; We introduced LikeSnow above since Like
; can only be used to represent that an agent likes a
; particular object, not snow in general.

event Smile(agent)

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Condition.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; human health

fluent Alive(agent)

fluent Dead(agent)
noninertial Dead

fluent Injured(agent)

event Kill(object,agent)
event Injure(object,agent)
event HealInjured(agent)

[agent,time] HoldsAt(Alive(agent),time) <-> !HoldsAt(Dead(agent),time).
[agent,time] HoldsAt(Injured(agent),time) -> HoldsAt(Alive(agent),time).

[object,agent,time]
Terminates(Kill(object,agent),Alive(agent),time).

[object,agent,time]
Initiates(Injure(object,agent),Injured(agent),time).

[agent,time]
Terminates(HealInjured(agent),Injured(agent),time).

fluent Intact(physobj)

fluent Damaged(physobj)

fluent Destroyed(physobj)

; At any time, a physical object is either intact, damaged, or destroyed:
xor Intact, Damaged, Destroyed

event Damage(object,physobj)

event Destroy(object,physobj)

event Repair(object,physobj)

[object,physobj,time]
Happens(Damage(object,physobj),time) ->
HoldsAt(Intact(physobj),time).

[object,physobj,time]
Initiates(Damage(object,physobj),Damaged(physobj),time).

[object,physobj,time]
Terminates(Damage(object,physobj),Intact(physobj),time).

[object,physobj,time]
Happens(Destroy(object,physobj),time) ->
(HoldsAt(Intact(physobj),time)|
 HoldsAt(Damaged(physobj),time)).

[object,physobj,time]
Initiates(Destroy(object,physobj),Destroyed(physobj),time).

[object,physobj,time]
Terminates(Destroy(object,physobj),Intact(physobj),time).

[object,physobj,time]
Terminates(Destroy(object,physobj),Damaged(physobj),time).

[object,physobj,time]
Initiates(Repair(object,physobj),Intact(physobj),time).

; end of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/GSpace.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; GSpace: grid space
;
; @book{Mueller:1998,
;   author = "Erik T. Mueller",
;   year = "1998",
;   title = "Natural Language Processing with \uppercase{T}hought\uppercase{T}reasure",
;   address = "New York",
;   publisher = "Signiform",
; }
;

sort coord: integer
sort grid

; object is at (coord1, coord2) in grid.
fluent GridAt(grid,object,coord,coord)

; agent walks from (coord1, coord2)
; to (coord3, coord4) in grid.
event GridWalk(grid,agent,coord,coord,coord,coord)

; A state constraint says that for a given grid an
; object is at one cell in that grid at a time:
[grid,object,coord1,coord2,coord3,coord4,time]
HoldsAt(GridAt(grid,object,coord1,coord2),time) &
HoldsAt(GridAt(grid,object,coord3,coord4),time) ->
coord1=coord3 & coord2=coord4.

; An effect axiom states that
; if an agent walks from one cell in a grid to another cell,
; the agent will be at second cell:
[grid,agent,coord1,coord2,coord3,coord4,time]
Initiates(GridWalk(grid,agent,coord1,coord2,coord3,coord4),
          GridAt(grid,agent,coord3,coord4),
          time).

; An effect axiom states that
; if an agent walks from one cell in a grid to another cell,
; the agent will no longer be at the first cell:
[grid,agent,coord1,coord2,coord3,coord4,time]
Terminates(GridWalk(grid,agent,coord1,coord2,coord3,coord4),
           GridAt(grid,agent,coord1,coord2),
           time).

; A precondition axiom states that for an agent to walk
; from one cell in a grid to another cell, the agent
; must be at the first cell, the second cell must not
; be occupied, and the first cell must be adjacent to
; the second cell:
[grid,agent,coord1,coord2,coord3,coord4,time]
Happens(GridWalk(grid,agent,coord1,coord2,coord3,coord4),time) ->
HoldsAt(GridAt(grid,agent,coord1,coord2),time) &
(!{object} HoldsAt(GridAt(grid,object,coord3,coord4),time)) &
(coord1=coord3 |
 coord1=coord3+1 |
 coord1=coord3-1) &
(coord2=coord4 |
 coord2=coord4+1 |
 coord2=coord4-1).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Bomb.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; bomb

; agent is nondeterministically killed.
fluent KilledDeterminingFluent(agent)
noninertial KilledDeterminingFluent

; agent is nondeterministically injured.
fluent InjuredDeterminingFluent(agent)
noninertial InjuredDeterminingFluent

; physobj is nondeterministically destroyed.
fluent DestroyedDeterminingFluent(physobj)
noninertial DestroyedDeterminingFluent

; physobj is nondeterministically damaged.
fluent DamagedDeterminingFluent(physobj)
noninertial DamagedDeterminingFluent

; agent activates bomb.
event BombActivate(agent,bomb)

; agent deactivates bomb.
event BombDeactivate(agent,bomb)

; bomb explodes.
event BombExplode(bomb)

; bomb is activated.
fluent BombActivated(bomb)

; The timer value of bomb is offset.
fluent BombTimerValue(bomb,offset)

; The timer value of bomb is decremented.
event BombDecrementTimer(bomb)

; The time delay of bomb is offset.
function BombTimeDelay(bomb): offset

; A state constraint says that a bomb has one timer
; value at a time:
[bomb,offset1,offset2,time]
HoldsAt(BombTimerValue(bomb,offset1),time) &
HoldsAt(BombTimerValue(bomb,offset2),time) ->
offset1=offset2.

; An effect axiom states that if a bomb is intact and
; an agent activates the bomb,
; the bomb will be activated:
[agent,bomb,time]
HoldsAt(Intact(bomb),time) ->
Initiates(BombActivate(agent,bomb),
          BombActivated(bomb),
          time).

; A precondition axiom states that
; for an agent to activate a bomb,
; the agent must be holding the bomb:
[agent,bomb,time]
Happens(BombActivate(agent,bomb),time) ->
HoldsAt(Holding(agent,bomb),time).

; An effect axiom states that if a bomb is intact and
; an agent deactivates the bomb,
; the bomb will no longer be activated:
[agent,bomb,time]
HoldsAt(Intact(bomb),time) ->
Terminates(BombDeactivate(agent,bomb),
           BombActivated(bomb),
           time).

; An axiom states that if a bomb explodes, the
; bomb destroys the bomb:
[bomb,time]
Happens(BombExplode(bomb),time) ->
Happens(Destroy(bomb,bomb),time).

; An effect axiom states that if a bomb explodes,
; the bomb is no longer activated:
[bomb,time]
Terminates(BombExplode(bomb),BombActivated(bomb),time).

; A trigger axiom states that
; if a bomb is activated,
; the timer value of the bomb is a timer value, and
; the timer value is greater than zero,
; the timer value of the bomb will be decremented:
[bomb,offset,time]
HoldsAt(BombActivated(bomb),time) &
HoldsAt(BombTimerValue(bomb,offset),time) &
(offset > 0) ->
Happens(BombDecrementTimer(bomb),time).

; An effect axiom states that
; if the timer value of the bomb is a timer value and
; the timer value of the bomb is decremented,
; the timer value of the bomb will be the timer value minus one:
[bomb,offset1,offset2,time]
HoldsAt(BombTimerValue(bomb,offset1),time) &
offset2 = offset1-1 ->
Initiates(BombDecrementTimer(bomb),
          BombTimerValue(bomb,offset2),
          time).

; An effect axiom states that
; if the timer value of the bomb is a timer value and
; the timer value of the bomb is decremented,
; the timer value of the bomb will no longer be the timer value:
[bomb,offset,time]
HoldsAt(BombTimerValue(bomb,offset),time) ->
Terminates(BombDecrementTimer(bomb),
           BombTimerValue(bomb,offset),
           time).

; An effect axiom states that if a bomb explodes,
; the bomb will no longer be activated:
[bomb,time]
Terminates(BombExplode(bomb),BombActivated(bomb),time).

; A trigger axiom states that if the timer value
; of a bomb is zero, the bomb will explode:
[bomb,time]
HoldsAt(BombTimerValue(bomb,0),time) ->
Happens(BombExplode(bomb),time).

; An axiom states that if an agent is at a location,
; a bomb is at the location,
; the agent is nondeterministically injured, and
; the bomb explodes, then
; the bomb will injure the agent:
[agent,location,bomb,time]
HoldsAt(At(agent,location),time) &
HoldsAt(At(bomb,location),time) &
HoldsAt(InjuredDeterminingFluent(agent),time) &
Happens(BombExplode(bomb),time) ->
Happens(Injure(bomb,agent),time).

; An axiom states that if an agent is at a location,
; a bomb is at the location,
; the agent is nondeterministically killed, and
; the bomb explodes, then
; the bomb will kill the agent:
[agent,location,bomb,time]
HoldsAt(At(agent,location),time) &
HoldsAt(At(bomb,location),time) &
HoldsAt(KilledDeterminingFluent(agent),time) &
Happens(BombExplode(bomb),time) ->
Happens(Kill(bomb,agent),time).

; An axiom states that if an physical object is at a location,
; a bomb is at the location,
; the physical object is nondeterministically damaged, and
; the bomb explodes, then
; the bomb will damage the physical object:
[physobj,location,bomb,time]
HoldsAt(At(physobj,location),time) &
HoldsAt(At(bomb,location),time) &
HoldsAt(DamagedDeterminingFluent(physobj),time) &
Happens(BombExplode(bomb),time) ->
Happens(Damage(bomb,physobj),time).

; An axiom states that if an physical object is at a location,
; a bomb is at the location,
; the physical object is nondeterministically destroyed, and
; the bomb explodes, then
; the bomb will destroy the physical object:
[physobj,location,bomb,time]
HoldsAt(At(physobj,location),time) &
HoldsAt(At(bomb,location),time) &
HoldsAt(DestroyedDeterminingFluent(physobj),time) &
Happens(BombExplode(bomb),time) ->
Happens(Destroy(bomb,physobj),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/IPRel.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; IPRel: interpersonal relations
;

fluent FriendOf(agent,agent)

fluent NeutralOf(agent,agent)

fluent EnemyOf(agent,agent)

event BecomeFriends(agent,agent)
event BecomeNeutral(agent,agent)
event BecomeEnemies(agent,agent)

[agent1,agent2,time]
HoldsAt(FriendOf(agent1,agent2),time) ->
!Holds(EnemyOf(agent1,agent2),time).

[agent1,agent2,time]
HoldsAt(NeutralOf(agent1,agent2),time) ->
!Holds(EnemyOf(agent1,agent2),time).

[agent1,agent2,time]
HoldsAt(FriendOf(agent1,agent2),time) ->
HoldsAt(FriendOf(agent2,agent1),time).

[agent1,agent2,time]
HoldsAt(NeutralOf(agent1,agent2),time) ->
HoldsAt(NeutralOf(agent2,agent1),time).

[agent1,agent2,time]
HoldsAt(EnemyOf(agent1,agent2),time) ->
HoldsAt(EnemyOf(agent2,agent1),time).

[agent1,agent2,time]
Initiates(BecomeFriends(agent1,agent2),FriendOf(agent1,agent2),time).

[agent1,agent2,time]
Initiates(BecomeFriends(agent1,agent2),FriendOf(agent2,agent1),time).

[agent1,agent2,time]
Terminates(BecomeFriends(agent1,agent2),NeutralOf(agent1,agent2),time).

[agent1,agent2,time]
Terminates(BecomeFriends(agent1,agent2),NeutralOf(agent2,agent1),time).

[agent1,agent2,time]
Terminates(BecomeFriends(agent1,agent2),EnemyOf(agent1,agent2),time).

[agent1,agent2,time]
Terminates(BecomeFriends(agent1,agent2),EnemyOf(agent2,agent1),time).

[agent1,agent2,time]
Initiates(BecomeEnemies(agent1,agent2),EnemyOf(agent1,agent2),time).

[agent1,agent2,time]
Initiates(BecomeEnemies(agent1,agent2),EnemyOf(agent2,agent1),time).

[agent1,agent2,time]
Terminates(BecomeEnemies(agent1,agent2),NeutralOf(agent1,agent2),time).

[agent1,agent2,time]
Terminates(BecomeEnemies(agent1,agent2),NeutralOf(agent2,agent1),time).

[agent1,agent2,time]
Terminates(BecomeEnemies(agent1,agent2),FriendOf(agent1,agent2),time).

[agent1,agent2,time]
Terminates(BecomeEnemies(agent1,agent2),FriendOf(agent2,agent1),time).

[agent1,agent2,time]
Initiates(BecomeNeutral(agent1,agent2),NeutralOf(agent1,agent2),time).

[agent1,agent2,time]
Initiates(BecomeNeutral(agent1,agent2),NeutralOf(agent2,agent1),time).

[agent1,agent2,time]
Terminates(BecomeNeutral(agent1,agent2),FriendOf(agent1,agent2),time).

[agent1,agent2,time]
Terminates(BecomeNeutral(agent1,agent2),FriendOf(agent2,agent1),time).

[agent1,agent2,time]
Terminates(BecomeNeutral(agent1,agent2),EnemyOf(agent1,agent2),time).

[agent1,agent2,time]
Terminates(BecomeNeutral(agent1,agent2),EnemyOf(agent2,agent1),time).

[agent1,agent2,time]
HoldsAt(FriendOf(agent1,agent2),time) ->
HoldsAt(Like(agent1,agent2),time).

[agent1,agent2,time]
HoldsAt(EnemyOf(agent1,agent2),time) ->
HoldsAt(Dislike(agent1,agent2),time).

fluent AcquaintanceOf(agent,agent)

[agent,time] HoldsAt(AcquaintanceOf(agent,agent),time).

[agent1,agent2,time]
HoldsAt(AcquaintanceOf(agent1,agent2),time) ->
HoldsAt(AcquaintanceOf(agent2,agent1),time).

event Introduce(agent,agent)

[agent1,agent2,time]
Initiates(Introduce(agent1,agent2),
          AcquaintanceOf(agent1,agent2),
          time).

[agent1,agent2,time]
Initiates(Introduce(agent1,agent2),
          AcquaintanceOf(agent2,agent1),
          time).

event IntroduceMutual(agent,agent,agent)

[agent1,agent2,agent3,time]
Initiates(IntroduceMutual(agent1,agent2,agent3),
          AcquaintanceOf(agent2,agent3),
          time).

[agent1,agent2,agent3,time]
Initiates(IntroduceMutual(agent1,agent2,agent3),
          AcquaintanceOf(agent3,agent2),
          time).

[agent1,agent2,agent3,time]
Happens(IntroduceMutual(agent1,agent2,agent3),time) ->
HoldsAt(AcquaintanceOf(agent1,agent2),time) &
HoldsAt(AcquaintanceOf(agent1,agent3),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Money.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;

event Pay(agent,agent)
event Tip(agent,agent)



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Container.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
;
; Container: container
;

; linkage to OTSpace(M):
[agent,container1,container2,time]
Happens(TakeOutOf(agent,container1,container2),time) ->
HoldsAt(ContainerIsOpen(container2),time).

[agent,container1,container2,time]
Happens(PutInside(agent,container1,container2),time) ->
HoldsAt(ContainerIsOpen(container2),time).

; agent opens container.
event ContainerOpen(agent,container)

; agent closes container.
event ContainerClose(agent,container)

; container is open.
fluent ContainerIsOpen(container)

fluent ContainerClosed(container)
noninertial ContainerClosed

[container,time]
HoldsAt(ContainerClosed(container),time) <->
!HoldsAt(ContainerIsOpen(container),time).

; A precondition axiom states that
; for an agent to open a container,
; the agent must be awake,
; the container must not already be open, and
; the agent must be holding the container.
[agent,container,time]
Happens(ContainerOpen(agent,container),time) ->
HoldsAt(Awake(agent),time) &
!HoldsAt(ContainerIsOpen(container),time) &
HoldsAt(Holding(agent,container),time).

; An effect axiom states that
; if an agent opens a container,
; the container will be open:
[agent,container,time]
Initiates(ContainerOpen(agent,container),ContainerIsOpen(container),time).

; A precondition axiom states that
; for an agent to close a container,
; the agent must be awake,
; the container must be open, and
; the agent must be holding the container.
[agent,container,time]
Happens(ContainerClose(agent,container),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(ContainerIsOpen(container),time) &
HoldsAt(Holding(agent,container),time).

; An effect axiom states that
; if an agent closes a container,
; the container will no longer be open:
[agent,container,time]
Terminates(ContainerClose(agent,container),ContainerIsOpen(container),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/PlayNeed.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; A complete story understanding program will require representations
; of common human needs \fullcite{SchankAbelson:1977}.
;
; @book{SchankAbelson:1977,
;   author = "Schank, Roger C. and Abelson, Robert P.",
;   year = "1977",
;   title = "Scripts, Plans, Goals, and Understanding: An Inquiry into Human Knowledge Structures",
;   address = "Hillsdale, NJ",
;   publisher = "Lawrence Erlbaum",
; }
;
; The PlayNeed representation deals with one type of need, the need
; to play.

; Our underlying theory of human needs consists of the following sequence:
; (1) A need is unsatisfied.
; (2) Given certain stimuli and an unsatisfied need, an intention
; to satisfy the need is activated.
; (3) The intention is acted upon.
; (4) The need is satisfied.

; agent has an unsatisfied need to play.
fluent HungryToPlay(agent)
; agent has the intention to play outside.
fluent IntentionToPlay(agent,outside)
; agent has a satisfied need to play.
fluent SatiatedFromPlay(agent)

; At any time, an agent is in one of three states with respect
; to the need to play:
xor HungryToPlay, IntentionToPlay, SatiatedFromPlay

; agent intends to play at location outside.
event IntendToPlay(agent,outside)
; agent plays at location outside.
event Play(agent,outside)

; agent acts on the intention to play outside.
fluent ActOnIntentionToPlay(agent,outside)
noninertial ActOnIntentionToPlay

; A trigger axiom activates an intention for an agent to play when
; the agent has an unsatisfied need for play, the agent likes snow,
; the agent is awake, and
; the agent is in a room that looks out onto an outside area where it
; is snowing:
[agent,room,outside,time]
HoldsAt(HungryToPlay(agent),time) &
HoldsAt(LikeSnow(agent),time) &
HoldsAt(At(agent,room),time) &
LookOutOnto(room)=outside &
HoldsAt(Awake(agent),time) &
HoldsAt(Snowing(outside),time) ->
Happens(IntendToPlay(agent,outside),time).

; A story understanding program will need a detailed representation
; of intention \fullcite{CohenLevesque:1990}.
;
; @article{CohenLevesque:1990,
;   author = "Philip R. Cohen and Hector J. Levesque",
;   year = "1990",
;   title = "Intention is choice with commitment",
;   journal = "Artificial Intelligence",
;   volume = "42",
;   pages = "213--261",
; }
;
; In our simplified representation, once an intention to
; perform $e$ is activated, it persists until it is acted
; upon. Intentions are represented by inertial fluents.
; If an intention to perform $e$ is active at time point $t$,
; the agent may or may not perform $e$ at time point $t$.
; That is, we do not know exactly when the agent will act on the
; intention.
; This is a case of nondeterminism,
; which we handle by introducing a noninertial fluent corresponding
; to each intention fluent that
; indicates whether the agent does or does not in fact act
; on an intention at a given time.
; Since each ground term of the new noninertial fluent multiplies the
; number of models by $2^{n}$ where $n$ is the number of time points,
; in practice we may constrain the truth value of the fluent
; at various time points.
; In the case of the need to play,
; HoldsAt(ActOnIntentionToPlay(agent, outside), time)
; represents that
; HoldsAt(IntentionToPlay(agent, outside), time) is acted
; upon at time.

; Effect axioms state that
; if an agent intends to play in an outside area,
; the agent will have an intention to play in the outside area
; and will no longer be in the hungry-to-play state:
[agent,outside,time]
Initiates(IntendToPlay(agent,outside),IntentionToPlay(agent,outside),time).

[agent,outside,time]
Terminates(IntendToPlay(agent,outside),HungryToPlay(agent),time).

; A trigger axiom states that if an agent has the intention
; to play in an outside area,
; the agent acts on the intention to play in the outside area, and
; the agent is at the outside area,
; the agent plays in the outside area:
[agent,outside,time]
HoldsAt(IntentionToPlay(agent,outside),time) &
HoldsAt(ActOnIntentionToPlay(agent,outside),time) &
HoldsAt(At(agent,outside),time) ->
Happens(Play(agent,outside),time).

; Effect axioms state that if an agent plays in an
; outside area, the agent will be satiated from play
; and will no longer have an intention to play in
; the outside area:
[agent,outside,time]
Initiates(Play(agent,outside),SatiatedFromPlay(agent),time).

[agent,outside,time]
Terminates(Play(agent,outside),IntentionToPlay(agent,outside),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/TimeDelayBombing.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; DEV-MUC3-0008
; TimeDelayBombing
;
; @article{Mueller:2004c,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Understanding script-based stories using commonsense reasoning",
;   journal = "Cognitive Systems Research",
;   volume = "5",
;   number = "4",
;   pages = "307--340",
; }
;

ignore SkyOf, GroundOf, Near, Inside, WalkFromTo, RunFromTo
ignore RollAlong, Diameter, Move, HoldSome, PutInside
ignore TakeOutOf, On, DoorUnlock, DoorLock, WalkThroughDoor12
ignore WalkThroughDoor21, WalkDownStaircase, WalkUpStaircase

ignore ThreatenedBy

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/Condition.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2004c/Bomb.e

bomb Bomb1
BombTimeDelay(Bomb1)=3.
[time] !HoldsAt(DestroyedDeterminingFluent(Bomb1),time).
[time] !HoldsAt(DamagedDeterminingFluent(Bomb1),time).

agent Perp1
[time] !HoldsAt(InjuredDeterminingFluent(Perp1),time).
[time] !HoldsAt(KilledDeterminingFluent(Perp1),time).

agent HumanTarget1
HoldsAt(Alive(HumanTarget1),0).
HoldsAt(Awake(HumanTarget1),0).
HoldsAt(Standing(HumanTarget1),0).
HoldsAt(Sleep2(HumanTarget1),0).
!HoldsAt(Injured(HumanTarget1),0).
[object] !HoldsAt(Holding(HumanTarget1,object),0).
HoldsAt(At(HumanTarget1,Outside1),0).

[time] HoldsAt(KilledDeterminingFluent(HumanTarget1),time).
[time] !HoldsAt(InjuredDeterminingFluent(HumanTarget1),time).

agent HumanTarget2
HoldsAt(Alive(HumanTarget2),0).
HoldsAt(Awake(HumanTarget2),0).
HoldsAt(Standing(HumanTarget2),0).
HoldsAt(Sleep2(HumanTarget2),0).
!HoldsAt(Injured(HumanTarget2),0).
[object] !HoldsAt(Holding(HumanTarget2,object),0).
HoldsAt(At(HumanTarget2,Outside1),0).

[time] HoldsAt(KilledDeterminingFluent(HumanTarget2),time).
[time] !HoldsAt(InjuredDeterminingFluent(HumanTarget2),time).

agent HumanTarget3
HoldsAt(Alive(HumanTarget3),0).
HoldsAt(Awake(HumanTarget3),0).
HoldsAt(Standing(HumanTarget3),0).
HoldsAt(Sleep2(HumanTarget3),0).
!HoldsAt(Injured(HumanTarget3),0).
[object] !HoldsAt(Holding(HumanTarget3,object),0).
HoldsAt(At(HumanTarget3,Outside1),0).

[time] HoldsAt(KilledDeterminingFluent(HumanTarget3),time).
[time] !HoldsAt(InjuredDeterminingFluent(HumanTarget3),time).

agent HumanTarget4
HoldsAt(Alive(HumanTarget4),0).
HoldsAt(Awake(HumanTarget4),0).
HoldsAt(Standing(HumanTarget4),0).
HoldsAt(Sleep2(HumanTarget4),0).
!HoldsAt(Injured(HumanTarget4),0).
[object] !HoldsAt(Holding(HumanTarget4,object),0).
HoldsAt(At(HumanTarget4,Outside1),0).

[time] HoldsAt(KilledDeterminingFluent(HumanTarget4),time).
[time] !HoldsAt(InjuredDeterminingFluent(HumanTarget4),time).

physobj PhysTarget1
HoldsAt(Intact(PhysTarget1),0).
HoldsAt(At(PhysTarget1,Outside1),0).

[time] HoldsAt(DestroyedDeterminingFluent(PhysTarget1),time).
[time] !HoldsAt(DamagedDeterminingFluent(PhysTarget1),time).

; room-scale topological space
outside Outside1
outside Outside2
room Inside1
door Door1
building Building1
street Street1
Side1(Door1)=Outside1.
Side2(Door1)=Inside1.
LookOutOnto(Inside1)=Outside1.
Floor(Inside1)=1.
BuildingOf(Inside1)=Building1.
Side1(Street1)=Outside1.
Side2(Street1)=Outside2.

HoldsAt(Alive(Perp1),0).
HoldsAt(Awake(Perp1),0).
HoldsAt(Standing(Perp1),0).
HoldsAt(Sleep2(Perp1),0).
!HoldsAt(Injured(Perp1),0).
[object] !HoldsAt(Holding(Perp1,object),0).
HoldsAt(At(Bomb1,Outside2),0).
HoldsAt(At(Perp1,Outside2),0).
HoldsAt(Intact(Bomb1),0).
!HoldsAt(BombActivated(Bomb1),0).
!HoldsAt(DoorIsOpen(Door1),0).
HoldsAt(DoorUnlocked(Door1),0).
HoldsAt(BombTimerValue(Bomb1, BombTimeDelay(Bomb1)),0).

; narrative
Happens(PickUp(Perp1,Bomb1),0).
Happens(WalkStreet21(Perp1,Street1),1).
Happens(BombActivate(Perp1,Bomb1),2).
Happens(LetGoOf(Perp1,Bomb1),3).
Happens(WalkStreet12(Perp1,Street1),4).

range time 0 7
range offset 0 3
range diameter 0 0

completion Happens

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Book.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; Book: book (a sort of device)
;

sort page: integer

; agent opens book to page.
event BookOpenTo(agent,book,page)

; agent closes book.
event BookClose(agent,book)

; book is open to page.
fluent BookIsOpenTo(book,page)

fluent BookClosed(book)
noninertial BookClosed

; agent turns page of book to page.
event BookTurnPageTo(agent,book,page)

[book,page1,page2,time]
HoldsAt(BookIsOpenTo(book,page1),time) &
HoldsAt(BookIsOpenTo(book,page2),time) ->
page1=page2.

[book,time]
HoldsAt(BookClosed(book),time) <->
!{page} HoldsAt(BookIsOpenTo(book,page),time).

; A precondition axiom states that
; for an agent to open a book to a page,
; the agent must be awake,
; the book must be closed, and
; the agent must be holding the book.
[agent,book,page,time]
Happens(BookOpenTo(agent,book,page),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(BookClosed(book),time) &
HoldsAt(Holding(agent,book),time).

; An effect axiom states that
; if an agent opens a book to a page,
; the book will be open to the page:
[agent,book,page,time]
Initiates(BookOpenTo(agent,book,page),BookIsOpenTo(book,page),time).

; A precondition axiom states that
; for an agent to close a book,
; the agent must be awake,
; the book must not already be closed, and
; the agent must be holding the book.
[agent,book,time]
Happens(BookClose(agent,book),time) ->
HoldsAt(Awake(agent),time) &
!HoldsAt(BookClosed(book),time) &
HoldsAt(Holding(agent,book),time).

; An effect axiom states that
; if an agent closes a book,
; the book will no longer be open:
[agent,book,page,time]
Terminates(BookClose(agent,book),BookIsOpenTo(book,page),time).

[agent,book,page,time]
Happens(BookTurnPageTo(agent,book,page),time) ->
HoldsAt(Awake(agent),time) &
({page1} page1 != page & HoldsAt(BookIsOpenTo(book,page1),time)) &
HoldsAt(Holding(agent,book),time).

[agent,book,page,time]
Initiates(BookTurnPageTo(agent,book,page),BookIsOpenTo(book,page),time).

[agent,book,page1,page2,time]
HoldsAt(BookIsOpenTo(book,page1),time) &
page1 != page2 ->
Terminates(BookTurnPageTo(agent,book,page2),BookIsOpenTo(book,page1),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Sleep.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; The Sleep representation deals with the activity of sleeping and
; body posture.
; It is similar to the finite automaton representation of sleep
; used in ThoughtTreasure \fullcite[chap. 7]{Mueller:1998}.
;
; @book{Mueller:1998,
;   author = "Erik T. Mueller",
;   year = "1998",
;   title = "Natural Language Processing with \uppercase{T}hought\uppercase{T}reasure",
;   address = "New York",
;   publisher = "Signiform",
; }
;

; sleep

; agent wakes up.
event WakeUp(agent)

; agent gets tired.
event GetTired(agent)

; agent falls asleep.
event FallAsleep(agent)

; agent is asleep.
fluent Sleep0(agent)
; agent is awake and in bed.
fluent Sleep1(agent)
; agent is awake, out of bed, and undressed.
fluent Sleep2(agent)
; agent is awake and dressed.
fluent Sleep3(agent)
; agent is tired and dressed.
fluent Sleep4(agent)
; agent is tired and undressed.
fluent Sleep5(agent)
; agent is in bed, waiting to fall asleep.
fluent Sleep6(agent)

; At any time, an agent is in one of seven sleep states:
xor Sleep0, Sleep1, Sleep2, Sleep3, Sleep4, Sleep5, Sleep6

; constraints

; agent is asleep.
fluent Asleep(agent)
; agent is awake.
fluent Awake(agent)
noninertial Asleep
noninertial Awake

; Sleep0 indicates that the agent is asleep:
[agent,time] HoldsAt(Asleep(agent),time) <-> HoldsAt(Sleep0(agent),time).

; In all other sleep states, the agent is awake:
[agent,time]
HoldsAt(Awake(agent),time) <->
HoldsAt(Sleep1(agent),time) |
HoldsAt(Sleep2(agent),time) |
HoldsAt(Sleep3(agent),time) |
HoldsAt(Sleep4(agent),time) |
HoldsAt(Sleep5(agent),time) |
HoldsAt(Sleep6(agent),time).

; A number of axioms are used to specify the transitions of
; a finite automaton.
;--

; Waking up causes a transition from Sleep0
; to Sleep1:
[agent,time] Terminates(WakeUp(agent),Sleep0(agent),time).

[agent,time] Initiates(WakeUp(agent),Sleep1(agent),time).

[agent,time] Happens(WakeUp(agent),time) -> HoldsAt(Sleep0(agent),time).

;--

; Getting out of bed causes a transition from Sleep1
; to Sleep2:
[agent,bed,time] Terminates(RiseFrom(agent,bed),Sleep1(agent),time).

[agent,bed,time] Initiates(RiseFrom(agent,bed),Sleep2(agent),time).

[agent,bed,time]
Happens(RiseFrom(agent,bed),time) -> HoldsAt(Sleep1(agent),time).

;--

; Getting dressed causes a transition from Sleep2
; to Sleep3, the normal state of awakeness:
[agent,time] Terminates(GetDressed(agent),Sleep2(agent),time).

[agent,time] Initiates(GetDressed(agent),Sleep3(agent),time).

[agent,time] Happens(GetDressed(agent),time) -> HoldsAt(Sleep2(agent),time).

;--

; Getting tired causes a transition from Sleep3
; to Sleep4:
[agent,time] Terminates(GetTired(agent),Sleep3(agent),time).

[agent,time] Initiates(GetTired(agent),Sleep4(agent),time).

[agent,time] Happens(GetTired(agent),time) -> HoldsAt(Sleep3(agent),time).

;--

; Getting undressed causes a transition from Sleep4
; to Sleep5:
[agent,time] Terminates(GetUndressed(agent),Sleep4(agent),time).

[agent,time] Initiates(GetUndressed(agent),Sleep5(agent),time).

[agent,time] Happens(GetUndressed(agent),time) -> HoldsAt(Sleep4(agent),time).

;--

; Lying on a bed causes a transition from Sleep5
; to Sleep6:
[agent,bed,time] Terminates(LieOn(agent,bed),Sleep5(agent),time).

[agent,bed,time] Initiates(LieOn(agent,bed),Sleep6(agent),time).

[agent,bed,time] Happens(LieOn(agent,bed),time) -> HoldsAt(Sleep5(agent),time).

;--

; Falling asleep causes a transition from Sleep6
; to Sleep0:
[agent,time] Terminates(FallAsleep(agent),Sleep6(agent),time).

[agent,time] Initiates(FallAsleep(agent),Sleep0(agent),time).

[agent,time] Happens(FallAsleep(agent),time) -> HoldsAt(Sleep6(agent),time).

;--

; agent acts on being in state Sleep5.
fluent ActOnSleep5(agent)
noninertial ActOnSleep5

; We reduce the number of models by asserting that
; an agent only acts on being in state Sleep5 while in
; that state:
[agent,time]
!HoldsAt(Sleep5(agent),time) ->
!HoldsAt(ActOnSleep5(agent),time).

; Undressed is like IntentionToPlay
; ActOnSleep5 is like ActOnIntentionToPlay

; A trigger axiom states that if an agent is in state Sleep5,
; the agent acts on this state, the agent is in a room, and
; a bed is at the room, the agent lies on the bed:
[agent,room,bed,time]
HoldsAt(Sleep5(agent),time) &
HoldsAt(ActOnSleep5(agent),time) &
HoldsAt(At(agent,room),time) &
HoldsAt(At(bed,room),time) ->
Happens(LieOn(agent,bed),time).

; A precondition axiom states that for
; an agent to lie on a bed,
; the agent must be in state Sleep5,
; the agent must act on this state, and
; there must be a room such that
; the agent is in the room and the bed is in the room:
[agent,bed,time]
Happens(LieOn(agent,bed),time) ->
HoldsAt(Sleep5(agent),time) &
HoldsAt(ActOnSleep5(agent),time) &
{room}
 HoldsAt(At(agent,room),time) &
 HoldsAt(At(bed,room),time).

; (body) posture

; agent lies on physobj.
event LieOn(agent,physobj)

; agent sits on physobj.
event SitOn(agent,physobj)

[agent,physobj,time]
Happens(SitOn(agent,physobj),time) ->
{location}
 HoldsAt(At(agent,location),time) &
 HoldsAt(At(physobj,location),time).

; agent rises from physobj.
event RiseFrom(agent,physobj)

; agent is lying on physobj.
fluent LyingOn(agent,physobj)
; agent is sitting on physobj.
fluent SittingOn(agent,physobj)
; agent is standing.
fluent Standing(agent)

; agent is lying down.
fluent Lying(agent)
; agent is sitting.
fluent Sitting(agent)
noninertial Lying
noninertial Sitting

; At any time, an agent is either lying, sitting, or standing:
xor Lying, Sitting, Standing

[agent,physobj,time]
HoldsAt(LyingOn(agent,physobj),time) ->
HoldsAt(Lying(agent),time).

[agent,physobj,time]
HoldsAt(SittingOn(agent,physobj),time) ->
HoldsAt(Sitting(agent),time).

; State constraints represent that an agent can lie or sit
; on at most one object at a time:
[agent,physobj1,physobj2,time]
HoldsAt(LyingOn(agent,physobj1),time) &
HoldsAt(LyingOn(agent,physobj2),time) ->
physobj1=physobj2.

[agent,physobj1,physobj2,time]
HoldsAt(SittingOn(agent,physobj1),time) &
HoldsAt(SittingOn(agent,physobj2),time) ->
physobj1=physobj2.

; An effect axiom states that if an agent is standing and
; lies on a physical object, the agent will be lying on
; the physical object:
[agent,physobj,time]
HoldsAt(Standing(agent),time) ->
Initiates(LieOn(agent,physobj),
          LyingOn(agent,physobj),
          time).

; An effect axiom states that if an agent
; lies on a physical object, the agent will no longer
; be standing:
[agent,physobj,time]
Terminates(LieOn(agent,physobj),
           Standing(agent),
           time).

; An effect axiom states that if an agent is standing and
; sits on a physical object, the agent will be sitting on
; the physical object:
[agent,physobj,time]
HoldsAt(Standing(agent),time) ->
Initiates(SitOn(agent,physobj),
          SittingOn(agent,physobj),
          time).

; An effect axiom states that if an agent
; sits on a physical object, the agent will no longer
; be standing:
[agent,physobj,time]
Terminates(SitOn(agent,physobj),
           Standing(agent),
           time).

; An effect axiom states that if an agent
; is sitting or lying on a physical object and
; the agent rises from the physical object,
; the agent will be standing:
[agent,physobj,time]
(HoldsAt(SittingOn(agent,physobj),time) |
 HoldsAt(LyingOn(agent,physobj),time)) ->
Initiates(RiseFrom(agent,physobj),
          Standing(agent),
          time).

; An effect axiom states that if an agent is sitting on
; a physical object and the agent rises from the physical
; object, the agent will no longer be sitting on the
; physical object:
[agent,physobj,time]
HoldsAt(SittingOn(agent,physobj),time) ->
Terminates(RiseFrom(agent,physobj),
           SittingOn(agent,physobj),
           time).

; An effect axiom states that if an agent is lying on
; a physical object and the agent rises from the physical
; object, the agent will no longer be lying on the
; physical object:
[agent,physobj,time]
HoldsAt(LyingOn(agent,physobj),time) ->
Terminates(RiseFrom(agent,physobj),
           LyingOn(agent,physobj),
           time).

; dressing

; agent gets undressed.
event GetDressed(agent)
; agent gets dressed.
event GetUndressed(agent)
; agent is dressed.
fluent Dressed(agent)

; Effect axioms deal with getting dressed and undressed:
[agent,time] Initiates(GetDressed(agent),Dressed(agent),time).
[agent,time] Terminates(GetUndressed(agent),Dressed(agent),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Kidnapping.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; DEV-MUC3-0008
; Kidnapping
;
; @article{Mueller:2004c,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Understanding script-based stories using commonsense reasoning",
;   journal = "Cognitive Systems Research",
;   volume = "5",
;   number = "4",
;   pages = "307--340",
; }
;

ignore SkyOf, GroundOf, Near, WalkFromTo, RunFromTo
ignore RollAlong, Diameter, Move, HoldSome
ignore On, DoorUnlock, DoorLock
ignore WalkDownStaircase, WalkUpStaircase

ignore Request, KnowRequest, Order, KnowOrder, SayGoodbye
ignore IntentionToWalkIn, InvitedIn
ignore Snowing
ignore Like, Dislike, LikeSnow

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2003/Feeling.e
load answers/Mueller2004c/Condition.e
load answers/Mueller2004c/Gun.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2003/SpeechAct.e

gun Gun1
bullet Bullet1
HoldsAt(Intact(Gun1),0).
HoldsAt(Intact(Bullet1),0).

agent Perp1

agent HumanTarget1
HoldsAt(Calm(HumanTarget1),0).
HoldsAt(Alive(HumanTarget1),0).
HoldsAt(Awake(HumanTarget1),0).
HoldsAt(Standing(HumanTarget1),0).
HoldsAt(Sleep2(HumanTarget1),0).
!HoldsAt(Injured(HumanTarget1),0).
[object] !HoldsAt(Holding(HumanTarget1,object),0).
HoldsAt(At(HumanTarget1,Outside1),0).
[physobj1,physobj2] !HoldsAt(Inside(physobj1,physobj2),0).

; prune
sort shoota, shootb, shooto, shooth, shootp
event! Shoot(shoota,shootb,shooto)
event! ShootInjure(shoota,shootb,shooth)
event! ShootKill(shoota,shootb,shooth)
event! ShootDamage(shoota,shootb,shootp)
event! ShootDestroy(shoota,shootb,shootp)
shoota! Perp1
shootb! Gun1
shooto! HumanTarget1
shooth! HumanTarget1

; room-scale topological space
outside Outside1
outside Outside2
room Inside1
door Door1
building Building1
street Street1
Side1(Door1)=Outside2.
Side2(Door1)=Inside1.
LookOutOnto(Inside1)=Outside1.
Floor(Inside1)=1.
BuildingOf(Inside1)=Building1.
Side1(Street1)=Outside1.
Side2(Street1)=Outside2.

HoldsAt(Calm(Perp1),0).
HoldsAt(Alive(Perp1),0).
HoldsAt(Awake(Perp1),0).
HoldsAt(Standing(Perp1),0).
HoldsAt(Sleep2(Perp1),0).
!HoldsAt(Injured(Perp1),0).
[object] !HoldsAt(Holding(Perp1,object),0).
HoldsAt(At(Gun1,Outside2),0).
HoldsAt(At(Perp1,Outside2),0).
HoldsAt(At(Bullet1,Outside2),0).
HoldsAt(DoorIsOpen(Door1),0).
HoldsAt(DoorUnlocked(Door1),0).
[agent1,agent2] !HoldsAt(ThreatenedBy(agent1,agent2),0).
[agent1,agent2] !HoldsAt(AngryAt(agent1,agent2),0).
[physobj1,physobj2] !HoldsAt(Inside(physobj1,physobj2),0).
[agent,object] !HoldsAt(Love(agent,object),0).

; narrative
Happens(PickUp(Perp1,Gun1),0).
Happens(PickUp(Perp1,Bullet1),1).
Happens(PutInside(Perp1,Bullet1,Gun1),2).
Happens(WalkStreet21(Perp1,Street1),3).
Happens(Threaten(Perp1,HumanTarget1,Gun1),4).
Happens(Grab(Perp1,HumanTarget1),5).
Happens(WalkStreet12(Perp1,Street1),6).
Happens(WalkThroughDoor12(Perp1,Door1),7).
Happens(LetGoOf(Perp1,HumanTarget1),8).
Happens(Shoot(Perp1,Gun1,HumanTarget1),9).
Happens(ShootKill(Perp1,Gun1,HumanTarget1),9).

range time 0 10
range offset 0 3
range diameter 0 0

completion Happens

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Smoke.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; Smoking: smoking cigarettes and cigars
;

fluent CraveNicotine(agent)

fluent NicotineCravingSatisfied(agent)
noninertial NicotineCravingSatisfied

[agent,time]
HoldsAt(CraveNicotine(agent),time) <->
!HoldsAt(NicotineCravingSatisfied(agent),time).

event Smoke(agent,cigarette)

[agent,cigarette,time]
Happens(Smoke(agent,cigarette),time) ->
HoldsAt(Holding(agent,cigarette),time).

[agent,cigarette,time]
Terminates(Smoke(agent,cigarette),CraveNicotine(agent),time).

event Puff(agent,cigarette)

[agent,cigarette,time]
Happens(Puff(agent,cigarette),time) ->
Happens(Smoke(agent,cigarette),time).

event BlowOutSmoke(agent,smoke)

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/OTSpace.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; OTSpace: object-scale topological space
;
; The OTSpace representation deals with topological space at
; the scale of objects such as agents (humans and animals)
; and physical objects.
;

; PartOf

; physobj is a part of object.
predicate PartOf(physobj,object)

; A state constraint says that if a physical object
; is part of an object, the location of the
; physical object is the same as the location of the object:
[physobj,object,location,time]
PartOf(physobj,object) &
HoldsAt(At(object,location),time) ->
HoldsAt(At(physobj,location),time).

; rolling a snowball bigger

; agent rolls stuff1 along stuff2.
event RollAlong(agent,stuff,stuff)
; The diameter of ball is diameter.
fluent Diameter(ball,diameter)

; A state constraint says that a ball has a unique diameter:
[ball,diameter1,diameter2,time]
HoldsAt(Diameter(ball,diameter1),time) &
HoldsAt(Diameter(ball,diameter2),time) ->
diameter1=diameter2.

; Effect axiom state that if an agent rolls some snow along
; some other snow, the diameter of the first snow will increase:
[agent,snow1,snow2,diameter1,diameter2,time]
HoldsAt(Diameter(snow1,diameter1),time) &
diameter2 = diameter1+1 ->
Initiates(RollAlong(agent,snow1,snow2),
          Diameter(snow1,diameter2),
          time).

[agent,snow1,snow2,diameter1,time]
HoldsAt(Diameter(snow1,diameter1),time) ->
Terminates(RollAlong(agent,snow1,snow2),
           Diameter(snow1,diameter1),
           time).

; A precondition axiom states that
; for an agent to roll some snow along some other snow,
; there must be a location such that
; the agent is at the location,
; the first snow is at the location, and
; the second snow is at the location:
;[agent,snow1,snow2,time]
;Happens(RollAlong(agent,snow1,snow2),time) ->
;{location}
;HoldsAt(At(agent,location),time) &
;HoldsAt(At(snow1,location),time) &
;HoldsAt(At(snow2,location),time).

; motion

; object moves (in place).
event Move(object)

; Holding

; agent is holding physobj.
fluent Holding(agent,physobj)
; agent holds or picks up physobj.
event Hold(agent,physobj)
; agent picks up some stuff1 from stuff2.
event HoldSome(agent,stuff,stuff)
; agent releases or lets go of physobj.
event LetGoOf(agent,physobj)

; An effect axiom states that if an agent holds
; a physical object, the agent will be holding the
; physical object:
[agent,physobj,time]
Initiates(Hold(agent,physobj),Holding(agent,physobj),time).

; A precondition axiom states that
; for an agent to hold a physical object,
; there must be a location such that
; the agent is at the location and
; the physical object is at the location:
;[agent,physobj,time]
;Happens(Hold(agent,physobj),time) ->
;{location}
;  HoldsAt(At(agent,location),time) &
;  HoldsAt(At(physobj,location),time).

; An effect axiom states that if an agent
; lets go of a physical object, the agent is no longer holding
; the physical object:
[agent,physobj,time]
Terminates(LetGoOf(agent,physobj),Holding(agent,physobj),time).

; A precondition axiom states that
; for an agent to let go of a physical object,
; the agent must be holding the physical object:
[agent,physobj,time]
Happens(LetGoOf(agent,physobj),time) ->
HoldsAt(Holding(agent,physobj),time).

; A releases axiom states that if an agent holds
; a physical object,
; the physical object's location will be released
; from inertia:
[agent,physobj,location,time]
Releases(Hold(agent,physobj),At(physobj,location),time).

; A state constraint says that if an agent is holding
; a physical object and the agent is at a location,
; the physical object is also at the location:
[agent,physobj,location,time]
HoldsAt(Holding(agent,physobj),time) &
HoldsAt(At(agent,location),time) ->
HoldsAt(At(physobj,location),time).

; A releases axiom states that if an agent holds
; a physical object,
; the locations of the parts of the physical object
; will be released from inertia:
[agent,physobj1,physobj2,location,time]
PartOf(physobj1,physobj2) ->
Releases(Hold(agent,physobj2),At(physobj1,location),time).

; Further, if an agent holds a physical object,
; the locations of the physical objects of which
; the physical object is a part
; will be released from inertia:
[agent,physobj1,physobj2,location,time]
PartOf(physobj1,physobj2) ->
Releases(Hold(agent,physobj1),At(physobj2,location),time).

;[agent,physobj,location1,location2,time]
;(!{object} PartOf(physobj,object)) &
;HoldsAt(At(agent,location1),time) &
;location1 != location2 ->
;Terminates(LetGoOf(agent,physobj),At(physobj,location2),time).

[agent,physobj,location,time]
(!{object} PartOf(physobj,object)) &
HoldsAt(At(agent,location),time) ->
Initiates(LetGoOf(agent,physobj),At(physobj,location),time).

;[agent,physobj1,physobj2,location1,location2,time]
;PartOf(physobj1,physobj2) &
;(!{object} PartOf(physobj2,object)) &
;HoldsAt(At(agent,location1),time) &
;location1 != location2 ->
;Terminates(LetGoOf(agent,physobj1),At(physobj2,location2),time).

[agent,physobj1,physobj2,location,time]
PartOf(physobj1,physobj2) &
(!{object} PartOf(physobj2,object)) &
HoldsAt(At(agent,location),time) ->
Initiates(LetGoOf(agent,physobj1),At(physobj2,location),time).

; An effect axiom states that if an agent is at a location
; and lets go of a physical object, the physical object
; will be at the location:
[agent,physobj,location,time]
HoldsAt(At(agent,location),time) ->
Initiates(LetGoOf(agent,physobj),At(physobj,location),time).

; An effect axiom states that if an agent picks up
; some stuff out of some other stuff, the agent will
; be holding the first stuff:
[agent,stuff1,stuff2,time]
Initiates(HoldSome(agent,stuff1,stuff2),
          Holding(agent,stuff1),
          time).

; A precondition axiom states that
; for an agent to pick up some stuff out of some other stuff,
; the first stuff must be a part of the second stuff and
; there must be a location such that the agent is at the location,
; the first stuff is at the location, and the second stuff is
; at the location:
[agent,stuff1,stuff2,time]
Happens(HoldSome(agent,stuff1,stuff2),time) ->
PartOf(stuff1,stuff2) &
{location}
  HoldsAt(At(agent,location),time) &
  HoldsAt(At(stuff1,location),time) &
  HoldsAt(At(stuff2,location),time).

; A releases axiom states that if an agent picks up some
; stuff out of some other stuff,
; the first stuff's location will be released
; from inertia:
[agent,stuff1,stuff2,location,time]
Releases(HoldSome(agent,stuff1,stuff2),At(stuff1,location),time).

; Inside

; physobj1 is inside physobj2.
fluent Inside(physobj,physobj)
; agent puts physobj1 inside physobj2.
event PutInside(agent,physobj,physobj)
; agent takes physobj1 out of physobj2.
event TakeOutOf(agent,physobj,physobj)

; A state constraint says that a physical object cannot
; be inside itself:
[physobj1,physobj2,time]
HoldsAt(Inside(physobj1,physobj2),time) ->
physobj1!=physobj2.

; A state constraint says that if a physical object is
; inside another physical object, the second physical object
; is not inside the first physical object:
[physobj1,physobj2,time]
HoldsAt(Inside(physobj1,physobj2),time) ->
!HoldsAt(Inside(physobj2,physobj1),time).

; An effect axiom states that if an agent puts a physical
; object inside another physical object, the first
; physical object will be inside the second physical object:
[agent,physobj1,physobj2,time]
Initiates(PutInside(agent,physobj1,physobj2),
          Inside(physobj1,physobj2),time).

; An effect axiom states that if an agent puts a physical
; object inside another physical object, the agent will
; no longer be holding the first physical object:
[agent,physobj1,physobj2,time]
Terminates(PutInside(agent,physobj1,physobj2),
           Holding(agent,physobj1),time).

; A precondition axiom states that
; for an agent to put a physical object inside another
; physical object,
; the agent must be holding the first physical object
; and there must be a location such that
; the agent is at the location and
; the second physical object is at the location:
;[agent,physobj1,physobj2,time]
;Happens(PutInside(agent,physobj1,physobj2),time) ->
;HoldsAt(Holding(agent,physobj1),time) &
;{location}
; HoldsAt(At(agent,location),time) &
; HoldsAt(At(physobj2,location),time).

; An effect axiom states that
; if an agent takes a physical object out of another
; physical object, the first physical object
; will no longer be inside the second physical object:
[agent,physobj1,physobj2,time]
Terminates(TakeOutOf(agent,physobj1,physobj2),
           Inside(physobj1,physobj2),time).

; A precondition axiom states that
; for an agent to take a physical object out of another
; physical object,
; the first physical object must be inside the second physical object
; and there must be a location such that
; the agent is at the location,
; the first physical object is at the location, and
; the second physical object is at the location:
[agent,physobj1,physobj2,time]
Happens(TakeOutOf(agent,physobj1,physobj2),time) ->
HoldsAt(Inside(physobj1,physobj2),time) &
{location}
 HoldsAt(At(agent,location),time) &
 HoldsAt(At(physobj1,location),time) &
 HoldsAt(At(physobj2,location),time).

; A releases axiom states that if an agent puts a physical
; object inside another physical object,
; the first physical object's location will be released
; from inertia:
[agent,physobj1,physobj2,location,time]
Releases(PutInside(agent,physobj1,physobj2),
         At(physobj1,location),time).

; A state constraint says that if a physical object is inside
; another physical object and the second physical object is
; at a location, the first physical object is also at the location:
[physobj1,physobj2,location,time]
HoldsAt(Inside(physobj1,physobj2),time) &
HoldsAt(At(physobj2,location),time) ->
HoldsAt(At(physobj1,location),time).

; An effect axiom states that if an agent takes a physical
; object out of another physical object,
; the agent will be holding the first physical object:
[agent,physobj1,physobj2,time]
Initiates(TakeOutOf(agent,physobj1,physobj2),
          Holding(agent,physobj1),
          time).

; On

; physobj1 is on physobj2.
fluent On(physobj,physobj)

; agent places physobj1 on physobj2.
event PlaceOn(agent,physobj,physobj)
; agent takes physobj1 off of physobj2.
event TakeOffOf(agent,physobj,physobj)

; A state constraint says that a physical object cannot
; be on itself:
[physobj1,physobj2,time]
HoldsAt(On(physobj1,physobj2),time) ->
physobj1!=physobj2.

; A state constraint says that if a physical object is
; on another physical object, the second physical object
; is not on the first physical object:
[physobj1,physobj2,time]
HoldsAt(On(physobj1,physobj2),time) ->
!HoldsAt(On(physobj2,physobj1),time).

; An effect axiom states that if an agent places a physical
; object on another physical object, the first
; physical object will be on the second physical object:
[agent,physobj1,physobj2,time]
Initiates(PlaceOn(agent,physobj1,physobj2),
          On(physobj1,physobj2),time).

; An effect axiom states that if an agent places a physical
; object on another physical object, the agent will
; no longer be holding the first physical object:
[agent,physobj1,physobj2,time]
Terminates(PlaceOn(agent,physobj1,physobj2),
           Holding(agent,physobj1),time).

; A precondition axiom states that
; for an agent to place a physical object on another
; physical object,
; the agent must be holding the first physical object
; and there must be a location such that
; the agent is at the location and
; the second physical object is at the location:
;[agent,physobj1,physobj2,time]
;Happens(PlaceOn(agent,physobj1,physobj2),time) ->
;HoldsAt(Holding(agent,physobj1),time) &
;{location}
; HoldsAt(At(agent,location),time) &
; HoldsAt(At(physobj2,location),time).

; An effect axiom states that
; if an agent takes a physical object off of another
; physical object, the first physical object
; will no longer be on the second physical object:
[agent,physobj1,physobj2,time]
Terminates(TakeOffOf(agent,physobj1,physobj2),
           On(physobj1,physobj2),time).

; An effect axiom states that if an agent takes a physical
; object off of another physical object,
; the agent will be holding the first physical object:
[agent,physobj1,physobj2,time]
Initiates(TakeOffOf(agent,physobj1,physobj2),
          Holding(agent,physobj1),time).

; A precondition axiom states that
; for an agent to take a physical object off of another
; physical object,
; the first physical object must be on the second physical object
; and there must be a location such that
; the agent is at the location and
; the first physical object is at the location:
; the second physical object is at the location:
[agent,physobj1,physobj2,time]
Happens(TakeOffOf(agent,physobj1,physobj2),time) ->
HoldsAt(On(physobj1,physobj2),time) &
{location}
 HoldsAt(At(agent,location),time) &
 HoldsAt(At(physobj1,location),time) &
 HoldsAt(At(physobj2,location),time).

; A releases axiom states that if an agent places a physical
; object on another physical object,
; the first physical object's location will be released
; from inertia:
[agent,physobj1,physobj2,location,time]
Releases(PlaceOn(agent,physobj1,physobj2),
         At(physobj1,location),
         time).

; A state constraint says that if a physical object is on
; another physical object and the second physical object is
; at a location, the first physical object is also at the location:
[physobj1,physobj2,location,time]
HoldsAt(On(physobj1,physobj2),time) &
HoldsAt(At(physobj2,location),time) ->
HoldsAt(At(physobj1,location),time).

fluent Near(agent,object)
event WalkFromTo(agent,object,object)
event WalkFrom(agent,object)
event RunFromTo(agent,object,object)

[agent,object1,object2,time]
Initiates(WalkFromTo(agent,object1,object2),
          Near(agent,object2),
          time).

[agent,object1,object2,time]
Terminates(WalkFromTo(agent,object1,object2),
           Near(agent,object1),
           time).

[agent,object1,object2,time]
Happens(WalkFromTo(agent,object1,object2),time) ->
{location}
HoldsAt(At(agent,location),time) &
HoldsAt(At(object1,location),time) &
HoldsAt(At(object2,location),time).

[agent,object1,object2,time]
Initiates(RunFromTo(agent,object1,object2),
          Near(agent,object2),
          time).

[agent,object1,object2,time]
Terminates(RunFromTo(agent,object1,object2),
           Near(agent,object1),
           time).

[agent,object1,object2,time]
Happens(RunFromTo(agent,object1,object2),time) ->
{location}
HoldsAt(At(agent,location),time) &
HoldsAt(At(object1,location),time) &
HoldsAt(At(object2,location),time).

[agent,object,time]
Terminates(WalkFrom(agent,object),
           Near(agent,object),
           time).

[agent,object,location,door,time]
HoldsAt(Near(agent,object),time) &
HoldsAt(At(agent,location),time) &
HoldsAt(At(object,location),time) &
Side1(door)=location &
Happens(WalkThroughDoor12(agent,door),time) ->
Happens(WalkFrom(agent,object),time).

[agent,object,location,door,time]
HoldsAt(Near(agent,object),time) &
HoldsAt(At(agent,location),time) &
HoldsAt(At(object,location),time) &
Side2(door)=location &
Happens(WalkThroughDoor21(agent,door),time) ->
Happens(WalkFrom(agent,object),time).

[agent,object,room,staircase,time]
HoldsAt(Near(agent,object),time) &
HoldsAt(At(agent,room),time) &
HoldsAt(At(object,room),time) &
Side1(staircase)=room &
Happens(WalkUpStaircase(agent,staircase),time) ->
Happens(WalkFrom(agent,object),time).

[agent,object,room,staircase,time]
HoldsAt(Near(agent,object),time) &
HoldsAt(At(agent,room),time) &
HoldsAt(At(object,room),time) &
Side2(staircase)=room &
Happens(WalkDownStaircase(agent,staircase),time) ->
Happens(WalkFrom(agent,object),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/TakingAWalk.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Mueller:2004c,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Understanding script-based stories using commonsense reasoning",
;   journal = "Cognitive Systems Research",
;   volume = "5",
;   number = "4",
;   pages = "307--340",
; }
;

option modeldiff on
option renaming off

ignore Love, ThreatenedBy
ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
ignore Inside, PutInside, On, PlaceOn
ignore Like, Happy, BecomeAngryAt
ignore Near, WalkFrom, WalkFromTo, RunFromTo
ignore ActOnIntentionToWalkIn, IntentionToWalkIn, InvitedIn, InviteIn
ignore TakeOffOf, TakeOutOf, LetGoOf
ignore Greet, SayGoodbye, Order, KnowOrder
ignore LieOn, SitOn, RiseFrom, LyingOn, SittingOn

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2004c/Dress.e
load answers/Mueller2004c/Rain.e

room Origin1

door OriginDoor1

outside OriginOutside1

street StreetFromOriginToMiddle1

outside Middle1

street StreetFromMiddleToDestination1

outside DestinationOutside1

door DestinationDoor1

room Destination1

Side1(OriginDoor1)=OriginOutside1.
Side2(OriginDoor1)=Origin1.

Side1(StreetFromOriginToMiddle1)=OriginOutside1.
Side2(StreetFromOriginToMiddle1)=Middle1.

Side1(StreetFromMiddleToDestination1)=Middle1.
Side2(StreetFromMiddleToDestination1)=DestinationOutside1.

Side1(DestinationDoor1)=DestinationOutside1.
Side2(DestinationDoor1)=Destination1.

agent Walker1

clothing Clothes1

umbrella Umbrella1

; initial state
[agent,object] !HoldsAt(Holding(agent,object),0).
[agent] HoldsAt(Dressed(agent),0).
[agent] HoldsAt(Awake(agent),0).
[agent] HoldsAt(Sleep3(agent),0).
[agent] HoldsAt(Standing(agent),0).
HoldsAt(DoorUnlocked(OriginDoor1),0).
!HoldsAt(DoorIsOpen(OriginDoor1),0).
HoldsAt(DoorUnlocked(DestinationDoor1),0).
!HoldsAt(DoorIsOpen(DestinationDoor1),0).
HoldsAt(At(Walker1,Origin1),0).
HoldsAt(At(Clothes1,Origin1),0).
HoldsAt(At(Umbrella1,Origin1),0).
[outside,time] HoldsAt(Raining(outside),time).
[object] !HoldsAt(Wet(object),0).

; narrative
Happens(PutOn(Walker1,Clothes1),0).
Happens(PickUp(Walker1,Umbrella1),1).
Happens(DoorOpen(Walker1,OriginDoor1),2).
Happens(WalkThroughDoor21(Walker1,OriginDoor1),3).
Happens(DoorClose(Walker1,OriginDoor1),4).
Happens(WalkStreet12(Walker1,StreetFromOriginToMiddle1),5).
Happens(WalkStreet12(Walker1,StreetFromMiddleToDestination1),6).
Happens(DoorOpen(Walker1,DestinationDoor1),7).
Happens(WalkThroughDoor12(Walker1,DestinationDoor1),8).
Happens(DoorClose(Walker1,DestinationDoor1),9).
Happens(TakeOff(Walker1,Clothes1),10).

range time 0 11
range offset 0 0
range diameter 0 0

completion Happens

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Vision.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; The Vision representation deals with some simple aspects
; of vision.
;

; agent looks at object.
event LookAt(agent,object)

; agent sees object.
fluent See(agent,object)

; An effect axiom states that if an agent looks at
; an object, the agent will see the object:
[agent,object,time]
Initiates(LookAt(agent,object),
          See(agent,object),
          time).

; A precondition axiom states that for
; an agent to look at an object,
; there must be a location such that
; the agent is at the location and
; the object is at the location, or
; there must be a door such that
; the agent is near the door,
; the object is near the door, and
; the door is open:
;[agent,object,time]
;Happens(LookAt(agent,object),time) ->
;({location}
; HoldsAt(At(agent,location),time) &
; HoldsAt(At(object,location),time))|
;({door}
; HoldsAt(NearPortal(agent,door),time) &
; HoldsAt(NearPortal(object,door),time) &
; HoldsAt(DoorIsOpen(door),time)).

; An effect axiom states that if an agent
; looks at an object, the agent will no longer
; see other objects:
[agent,object1,object2,time]
object1!=object2 ->
Terminates(LookAt(agent,object1),
           See(agent,object2),
           time).

; Several effect axioms state that if an
; agent walks through a door, up a staircase, or down a staircase,
; the agent no longer sees an object:
[agent,door,object,time]
Terminates(WalkThroughDoor12(agent,door),
           See(agent,object),
           time).
[agent,door,object,time]
Terminates(WalkThroughDoor21(agent,door),
           See(agent,object),
           time).
[agent,door,object,time]
Terminates(WalkUpStaircase(agent,door),
           See(agent,object),
           time).
[agent,door,object,time]
Terminates(WalkDownStaircase(agent,door),
           See(agent,object),
           time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Sleeping.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Mueller:2004c,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Understanding script-based stories using commonsense reasoning",
;   journal = "Cognitive Systems Research",
;   volume = "5",
;   number = "4",
;   pages = "307--340",
; }
;

option modeldiff on

ignore Love, ThreatenedBy
ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
ignore Inside, Near
ignore See

ignore ActOnSleep5

option renaming off

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/Cognition.e
load answers/Mueller2003/Sleep.e

door Door1

room Room0

room Room1

Side1(Door1)=Room0.
Side2(Door1)=Room1.

agent Sleeper1

bed Bed1

outside Outside1

; initial state
[agent,object] !HoldsAt(Holding(agent,object),0).
[agent,physobj] !HoldsAt(SittingOn(agent,physobj),0).
[agent,physobj] !HoldsAt(LyingOn(agent,physobj),0).
HoldsAt(Dressed(Sleeper1),0).
HoldsAt(Awake(Sleeper1),0).
HoldsAt(Sleep3(Sleeper1),0).
HoldsAt(Standing(Sleeper1),0).
HoldsAt(DoorUnlocked(Door1),0).
HoldsAt(DoorIsOpen(Door1),0).
HoldsAt(At(Sleeper1,Room0),0).
HoldsAt(At(Bed1,Room1),0).

; narrative
Happens(GetTired(Sleeper1),0).
Happens(WalkThroughDoor12(Sleeper1,Door1),1).
Happens(GetUndressed(Sleeper1),2).
Happens(LieOn(Sleeper1,Bed1),3).
Happens(FallAsleep(Sleeper1),4).
Happens(Dream(Sleeper1),5).
Happens(WakeUp(Sleeper1),6).
Happens(RiseFrom(Sleeper1,Bed1),7).
Happens(GetDressed(Sleeper1),8).
Happens(WalkThroughDoor21(Sleeper1,Door1),9).

range time 0 10
range offset 0 0
range diameter 0 0

completion Happens

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Smoking.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Mueller:2004c,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Understanding script-based stories using commonsense reasoning",
;   journal = "Cognitive Systems Research",
;   volume = "5",
;   number = "4",
;   pages = "307--340",
; }
;

option modeldiff on

ignore Love, ThreatenedBy
ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
ignore Near, WalkFrom, WalkFromTo, RunFromTo
ignore Side1, Side2

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/Container.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2004c/SmallFire.e
load answers/Mueller2004c/Smoke.e

location Location1

portal DummyPortal1

agent Smoker1

cigarette Cigarette1

container Package1

physobj Surface1

physobj LightingDevice1

ashtray AshTray1

physobj Trash1

smoke Smoke1

; prune
sort ona, onb
fluent! On(ona,onb)
event! PlaceOn(agent,ona,onb)
event! TakeOffOf(agent,ona,onb)
ona! LightingDevice1, Package1, Cigarette1
onb! Surface1, AshTray1

sort insidea, insideb
fluent! Inside(insidea,insideb)
event! PutInside(agent,insidea,insideb)
event! TakeOutOf(agent,insidea,insideb)
insidea! Cigarette1
insideb! Package1, Trash1

sort lighta, lightb, lightc
event! LightWith(lighta,lightb,lightc)
lighta! Smoker1
lightb! Cigarette1
lightc! LightingDevice1

; initial state
[agent,object] !HoldsAt(Holding(agent,object),0).
[agent,physobj] !HoldsAt(SittingOn(agent,physobj),0).
[agent,physobj] !HoldsAt(LyingOn(agent,physobj),0).
HoldsAt(On(Package1,Surface1),0).
[physobj1,physobj2]
!(physobj1=Package1 & physobj2=Surface1) ->
!HoldsAt(On(physobj1, physobj2),0).
HoldsAt(Dressed(Smoker1),0).
HoldsAt(Awake(Smoker1),0).
HoldsAt(Sleep3(Smoker1),0).
HoldsAt(Standing(Smoker1),0).
HoldsAt(CraveNicotine(Smoker1),0).
HoldsAt(ContainerClosed(Package1),0).
[physobj] !HoldsAt(IsBurning(physobj),0).
HoldsAt(Inside(Cigarette1,Package1),0).
[physobj1,physobj2]
!(physobj1=Cigarette1 & physobj2=Package1) ->
!HoldsAt(Inside(physobj1, physobj2),0).

; narrative
Happens(TakeOffOf(Smoker1,Package1,Surface1),0).
Happens(ContainerOpen(Smoker1,Package1),1).
Happens(TakeOutOf(Smoker1,Cigarette1,Package1),2).
Happens(PickUp(Smoker1,LightingDevice1),3).
Happens(Light(Smoker1,LightingDevice1),4).
Happens(LightWith(Smoker1,Cigarette1,LightingDevice1),5).
Happens(BlowOut(Smoker1,LightingDevice1),6).
Happens(PlaceOn(Smoker1,LightingDevice1,Surface1),7).
Happens(PlaceOn(Smoker1,Package1,Surface1),8).
Happens(Puff(Smoker1,Cigarette1),9).
Happens(BlowOutSmoke(Smoker1,Smoke1),10).
Happens(PlaceOn(Smoker1,Cigarette1,AshTray1),11).
Happens(TakeOffOf(Smoker1,Cigarette1,AshTray1),12).
Happens(Puff(Smoker1,Cigarette1),13).
Happens(PutOut(Smoker1,Cigarette1),14).
Happens(PutInside(Smoker1,Cigarette1,Trash1),15).

range time 0 16
range offset 0 0
range diameter 0 0

completion Happens

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/CTime.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; clock time

; The CTime representation maps the time points of ECTime to clock time.

; part of the day

; time is in the daytime.
predicate Daytime(time)
; time is in the nighttime.
predicate Nighttime(time)
xor Daytime, Nighttime

; time is in the morning.
predicate Morning(time)
; time is in the afternoon.
predicate Afternoon(time)
; time is in the evening.
predicate Evening(time)
; time is in the night.
predicate Night(time)
; time is in the late night.
predicate LateNight(time)
xor Morning, Afternoon, Evening, Night, LateNight

[time] Daytime(time) <-> Morning(time)|Afternoon(time)|Evening(time).
[time] Nighttime(time) <-> Night(time)|LateNight(time).

; dreams

; time is part of a dream sequence.
predicate DreamSequence(time)

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/XWalk.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; XWalk: WALK x-schema representation of walking
;
; @phdthesis{Narayanan:1997,
;   author = "Srinivas S. Narayanan",
;   year = "1997",
;   title = "Knowledge-based Action Representations for Metaphor and Aspect (\uppercase{KARMA})",
;   address = "Berkeley, CA",
;   school = "University of California, Berkeley",
; }
;

option trajectory on

sort xschema

; parameters

predicate XWalkAgent(xschema,agent)
function XWalkRate(xschema): offset ; step duration
function XWalkSize(xschema): offset ; step size

; TTL input lines

fluent XWalkEnabled(xschema)
fluent XWalkGroundStable(xschema)
fluent XWalkPosture(xschema)
fluent XWalkFootingOK(xschema)
noninertial XWalkEnabled, XWalkGroundStable, XWalkPosture, XWalkFootingOK

; fluents

fluent XWalkDistance(xschema,distance)

[xschema,distance1,distance2,time]
HoldsAt(XWalkDistance(xschema,distance1),time) &
HoldsAt(XWalkDistance(xschema,distance2),time) ->
distance1=distance2.

; logic gate behavior

fluent XWalkVision(xschema)
fluent XWalkVisionOK(xschema)
fluent XWalkAtDestination(xschema)
fluent XWalkDone(xschema)
noninertial XWalkVision, XWalkVisionOK, XWalkAtDestination, XWalkDone

[xschema,time]
HoldsAt(XWalkGroundStable(xschema),time) <->
HoldsAt(XWalkVision(xschema),time).

[xschema,time]
HoldsAt(XWalkEnabled(xschema),time) &
HoldsAt(XWalkVision(xschema),time) &
HoldsAt(XWalkPosture(xschema),time) <->
HoldsAt(XWalkVisionOK(xschema),time).

[xschema,time]
HoldsAt(XWalkDistance(xschema,0),time) <->
HoldsAt(XWalkAtDestination(xschema),time).

[xschema,time]
HoldsAt(XWalkAtDestination(xschema),time) <->
HoldsAt(XWalkDone(xschema),time).

; durative events

fluent XWalkStepping(xschema,distance) ; distance is the goal
event XWalkSteppingOn(xschema)
event XWalkSteppingOff(xschema)

[xschema,distance1,distance2,time]
HoldsAt(XWalkDistance(xschema,distance1),time) &
distance2 = distance1 - XWalkSize(xschema) ->
Initiates(XWalkSteppingOn(xschema),XWalkStepping(xschema,distance2),time).

[xschema,distance,time]
Terminates(XWalkSteppingOff(xschema),XWalkStepping(xschema,distance),time).

[xschema,distance,time]
Releases(XWalkSteppingOn(xschema),XWalkDistance(xschema,distance),time).

[xschema,distance1,distance2,time]
HoldsAt(XWalkDistance(xschema,distance1),time) &
distance1 != distance2 ->
Terminates(XWalkSteppingOff(xschema),XWalkDistance(xschema,distance2),time).

[xschema,distance,time]
HoldsAt(XWalkDistance(xschema,distance),time) ->
Initiates(XWalkSteppingOff(xschema),XWalkDistance(xschema,distance),time).

[xschema,distance01,distance02,distance03,offset,time]
HoldsAt(XWalkDistance(xschema,distance01),time) &
(distance03=(distance01-(offset*(XWalkSize(xschema)/XWalkRate(xschema))))) ->
Trajectory(XWalkStepping(xschema,distance02),
           time,
           XWalkDistance(xschema,distance03),
           offset).

[xschema,distance,time]
HoldsAt(XWalkStepping(xschema,distance),time) &
HoldsAt(XWalkDistance(xschema,distance),time) ->
Happens(XWalkSteppingOff(xschema),time).

; punctual events

event XWalkTestFooting(xschema)
event XWalkMoveFoot(xschema)

[xschema,time]
Happens(XWalkTestFooting(xschema),time) &
!HoldsAt(XWalkFootingOK(xschema),time) ->
Happens(XWalkMoveFoot(xschema),time+1).

[xschema,time]
Happens(XWalkMoveFoot(xschema),time) ->
Happens(XWalkReadyOn(xschema),time+1).

; Petri net behavior

fluent XWalkReady(xschema)
event XWalkReadyOn(xschema)
event XWalkReadyOff(xschema)

[xschema,time]
HoldsAt(XWalkEnabled(xschema),time) &
HoldsAt(XWalkVision(xschema),time) &
HoldsAt(XWalkPosture(xschema),time) &
!({distance} HoldsAt(XWalkStepping(xschema,distance),time)) & ; !!! pulse
!HoldsAt(XWalkReady(xschema),time) ->
Happens(XWalkReadyOn(xschema),time).

[xschema,time]
Initiates(XWalkReadyOn(xschema),XWalkReady(xschema),time).

[xschema,time]
Terminates(XWalkReadyOff(xschema),XWalkReady(xschema),time).

; bypass_ok
[xschema,time]
!({distance} HoldsAt(XWalkStepping(xschema,distance),time)) &
HoldsAt(XWalkVisionOK(xschema),time) &
HoldsAt(XWalkReady(xschema),time) ->
Happens(XWalkSteppingOn(xschema),time) &
Happens(XWalkReadyOff(xschema),time).

; !bypass_ok
[xschema,time]
!({distance} HoldsAt(XWalkStepping(xschema,distance),time)) &
!HoldsAt(XWalkVisionOK(xschema),time) &
HoldsAt(XWalkReady(xschema),time) ->
Happens(XWalkTestFooting(xschema),time) &
Happens(XWalkReadyOff(xschema),time).

[xschema,distance,time]
HoldsAt(XWalkStepping(xschema,distance),time) &
HoldsAt(XWalkDistance(xschema,distance),time) &
(distance > 0) ->
Happens(XWalkReadyOn(xschema),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/ReadingABook.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Mueller:2004c,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Understanding script-based stories using commonsense reasoning",
;   journal = "Cognitive Systems Research",
;   volume = "5",
;   number = "4",
;   pages = "307--340",
; }
;

option modeldiff on

ignore Love, ThreatenedBy
ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
ignore Inside
ignore Near, WalkFrom, WalkFromTo, RunFromTo

option renaming off

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/Book.e
load answers/Mueller2004c/Cognition.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2003/Vision.e

door Door1

room Room0

room Room1

Side1(Door1)=Room0.
Side2(Door1)=Room1.

agent Reader1

book Book1

chair Chair1

physobj BookSupport1

content Content1

; initial state
[agent,object] !HoldsAt(Holding(agent,object),0).
[agent,physobj] !HoldsAt(SittingOn(agent,physobj),0).
[agent,physobj] !HoldsAt(LyingOn(agent,physobj),0).
!{page} HoldsAt(BookIsOpenTo(Book1,page),0).
[physobj1,physobj2]
!(physobj1=Book1 & physobj2=BookSupport1) ->
!HoldsAt(On(physobj1, physobj2),0).
HoldsAt(Dressed(Reader1),0).
HoldsAt(Awake(Reader1),0).
HoldsAt(Sleep3(Reader1),0).
HoldsAt(Standing(Reader1),0).
HoldsAt(DoorUnlocked(Door1),0).
HoldsAt(DoorIsOpen(Door1),0).
HoldsAt(At(Reader1,Room0),0).
HoldsAt(At(Chair1,Room1),0).
HoldsAt(At(Book1,Room1),0).
HoldsAt(On(Book1,BookSupport1),0).
[object] !HoldsAt(See(Reader1,object),0).

; narrative
Happens(WalkThroughDoor12(Reader1,Door1),0).
Happens(TakeOffOf(Reader1,Book1,BookSupport1),1).
Happens(SitOn(Reader1,Chair1),2).
Happens(BookOpenTo(Reader1,Book1,1),3).
Happens(LookAt(Reader1,Book1),4).
Happens(Read(Reader1,Book1,Content1),5).
Happens(ThinkAbout(Reader1,Content1),6).
Happens(Understand(Reader1,Content1),7).
Happens(BookTurnPageTo(Reader1,Book1,2),8).
Happens(BookTurnPageTo(Reader1,Book1,3),9).
Happens(BookClose(Reader1,Book1),10).
Happens(RiseFrom(Reader1,Chair1),11).
Happens(PlaceOn(Reader1,Book1,BookSupport1),12).
Happens(WalkThroughDoor21(Reader1,Door1),13).

range time 0 14
range page 1 3
range offset 0 0
range diameter 0 0

completion Happens

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/SpeechAct.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; The SpeechAct representation deals with a few speech acts
; \fullcite{Searle:1969}.
;
; @book{Searle:1969,
;   author = "John R. Searle",
;   year = "1969",
;   title = "Speech Acts: An Essay in the Philosophy of Language",
;   address = "Cambridge",
;   publisher = "Cambridge University Press",
; }
;
; We handle
; the illocutionary acts of
; inviting someone into one's house (a form of request) and
; greeting someone,
; and the expressive speech act of crying for joy.
;

; inviting in

; agent1 invites agent2 into room.
event InviteIn(agent,agent,room)
; agent1 is invited into room by agent2.
fluent InvitedIn(agent,room,agent)

; A precondition axiom states that for
; an agent to invite another agent into a room,
; the first agent must be in the room and
; there must be an outside area such that
; the second agent is at the outside area and
; the outside area is adjacent to the room:
[agent1,agent2,room,time]
Happens(InviteIn(agent1,agent2,room),time) ->
HoldsAt(At(agent1,room),time) &
{outside}
HoldsAt(At(agent2,outside),time) &
Adjacent(room,outside).

; An effect axiom states that if
; an agent invites another agent into a room,
; the second agent will be invited into the room by the first agent:
[agent1,agent2,room,time]
Initiates(InviteIn(agent1,agent2,room),
          InvitedIn(agent2,room,agent1),
          time).

; agent intends to walk into room.
event IntendToWalkIn(agent,room)
; agent has the intention to walk into room.
fluent IntentionToWalkIn(agent,room)
; agent acts on the intention to walk into room.
fluent ActOnIntentionToWalkIn(agent,room)
noninertial ActOnIntentionToWalkIn

; A trigger axiom states that
; if an agent is invited into a room by another agent,
; the first agent likes the second agent, and
; the first agent does not already have the intention to
; walk into the room,
; the first agent intends to walk into the room:
[agent1,agent2,room,time]
HoldsAt(InvitedIn(agent1,room,agent2),time) &
HoldsAt(Like(agent1,agent2),time) &
!HoldsAt(IntentionToWalkIn(agent1,room),time) ->
Happens(IntendToWalkIn(agent1,room),time).

; An effect axiom states that
; if an agent intends to walk into a room,
; the agent will have the intention to walk into the room:
[agent,room,time]
Initiates(IntendToWalkIn(agent,room),
          IntentionToWalkIn(agent,room),
          time).

; Two trigger axioms state that
; if an agent has the intention to walk into a room,
; the agent acts on the intention to walk into the room,
; the agent is at a location,
; side one (two) of a door is the room,
; side two (one) of the door is the location,
; agent will walk through side two (one) of the door:
[agent,room,location,door,time]
HoldsAt(IntentionToWalkIn(agent,room),time) &
HoldsAt(ActOnIntentionToWalkIn(agent,room),time) &
HoldsAt(At(agent,location),time) &
Side1(door)=room &
Side2(door)=location ->
Happens(WalkThroughDoor21(agent,door),time).

[agent,room,location,door,time]
HoldsAt(IntentionToWalkIn(agent,room),time) &
HoldsAt(ActOnIntentionToWalkIn(agent,room),time) &
HoldsAt(At(agent,location),time) &
Side2(door)=room &
Side1(door)=location ->
Happens(WalkThroughDoor12(agent,door),time).

; Two effect axioms state that
; if side one (two) of a door is a room and
; an agent walks through side two (one) of the door,
; the agent will no longer have the intention to
; walk into the room:
[agent,room,door,time]
Side1(door)=room ->
Terminates(WalkThroughDoor21(agent,door),
           IntentionToWalkIn(agent,room),
           time).

[agent,room,door,time]
Side2(door)=room ->
Terminates(WalkThroughDoor12(agent,door),
           IntentionToWalkIn(agent,room),
           time).

; agent greets object.
event Greet(agent,object)

event SayPleasedToMeet(agent,agent)

; agent says goodbye to object.
event SayGoodbye(agent,object)

event TalkAbout(agent,content)

event Converse(agent,agent)

[agent1,agent2,time]
Happens(Converse(agent1,agent2),time) ->
{location}
HoldsAt(At(agent1,location),time) &
HoldsAt(At(agent2,location),time).

; A precondition axiom states that for
; an agent to greet an object,
; there must be a location such that
; the agent is at the location and
; the object is at the location:
[agent,object,time]
Happens(Greet(agent,object),time) ->
{location}
HoldsAt(At(agent,location),time) &
HoldsAt(At(object,location),time).

[agent,object,time]
Happens(SayGoodbye(agent,object),time) ->
{location}
HoldsAt(At(agent,location),time) &
HoldsAt(At(object,location),time).

; speech: expression of emotions

; agent cries for joy.
event CryForJoy(agent)

; A precondition axiom states that for
; an agent to cry for joy,
; the agent must be happy:
[agent,time]
Happens(CryForJoy(agent),time) ->
HoldsAt(Happy(agent),time).

event Threaten(agent,agent,weapon)

event ReleaseFromThreat(agent,agent)

fluent ThreatenedBy(agent,agent)

[agent1,agent2,weapon,time]
Happens(Threaten(agent1,agent2,weapon), time) ->
HoldsAt(Holding(agent1,weapon),time) &
{location}
HoldsAt(At(agent1,location),time) &
HoldsAt(At(agent2,location),time).

[agent1,agent2,weapon,time]
Happens(Threaten(agent1,agent2,weapon), time) ->
Happens(BecomeAngryAt(agent2,agent1),time).

[agent1,agent2,weapon,time]
Initiates(Threaten(agent1,agent2,weapon),
          ThreatenedBy(agent2,agent1),
          time).

[agent1,agent2,time]
Terminates(ReleaseFromThreat(agent1,agent2),
           ThreatenedBy(agent2,agent1),
           time).

event Order(agent,agent,physobj)

fluent KnowOrder(agent,agent,physobj)

[agent1,agent2,physobj,time]
Initiates(Order(agent1,agent2,physobj),
          KnowOrder(agent2,agent1,physobj),
          time).

[agent1,agent2,physobj,time]
Happens(Order(agent1,agent2,physobj),time) ->
{location}
HoldsAt(At(agent1,location),time) &
HoldsAt(At(agent2,location),time).

event Request(agent,agent,physobj)

fluent KnowRequest(agent,agent,physobj)

[agent1,agent2,physobj,time]
Initiates(Request(agent1,agent2,physobj),
          KnowRequest(agent2,agent1,physobj),
          time).

[agent1,agent2,physobj,time]
Happens(Request(agent1,agent2,physobj),time) ->
{location}
HoldsAt(At(agent1,location),time) &
HoldsAt(At(agent2,location),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Dress.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; Dress
; (cf Sleep)
;

event PutOn(agent,clothing)

event TakeOff(agent,clothing)

fluent Wearing(agent,clothing)

[agent,clothing,time]
Initiates(PutOn(agent,clothing),
          Wearing(agent,clothing),
          time).

[agent,clothing,time]
Happens(PutOn(agent,clothing),time) ->
!HoldsAt(Wearing(agent,clothing),time) &
{location} HoldsAt(At(agent,location),time) &
           HoldsAt(At(clothing,location),time).

[agent,clothing,time]
Terminates(TakeOff(agent,clothing),
           Wearing(agent,clothing),
           time).

[agent,clothing,time]
Happens(TakeOff(agent,clothing),time) ->
HoldsAt(Wearing(agent,clothing),time).

[agent,clothing,location,time]
Releases(PutOn(agent,clothing),At(clothing,location),time).

[agent,clothing,location,time]
HoldsAt(Wearing(agent,clothing),time) &
HoldsAt(At(agent,location),time) ->
HoldsAt(At(clothing,location),time).

;[agent,clothing,location1,location2,time]
;HoldsAt(At(agent,location1),time) &
;location1 != location2 ->
;Terminates(TakeOff(agent,clothing),At(clothing,location2),time).

[agent,clothing,location,time]
HoldsAt(At(agent,location),time) ->
Initiates(TakeOff(agent,clothing),At(clothing,location),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/ReadingALetter.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Mueller:2004c,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Understanding script-based stories using commonsense reasoning",
;   journal = "Cognitive Systems Research",
;   volume = "5",
;   number = "4",
;   pages = "307--340",
; }
;

option modeldiff on

ignore Love, ThreatenedBy
ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
ignore Near, WalkFrom, WalkFromTo, RunFromTo
ignore Side1, Side2

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/Container.e
load answers/Mueller2004c/Cognition.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2003/Vision.e
load answers/Mueller2004c/HandTo.e

location Location1

portal DummyPortal1

agent Recipient1

letter Letter1

container Envelope1

physobj Surface1

chair Chair1

content Content1

agent Carrier1

; prune
sort ona, onb
fluent! On(ona,onb)
event! PlaceOn(agent,ona,onb)
event! TakeOffOf(agent,ona,onb)
ona! Envelope1, Letter1
onb! Surface1

sort insidea, insideb
fluent! Inside(insidea,insideb)
event! PutInside(agent,insidea,insideb)
event! TakeOutOf(agent,insidea,insideb)
insidea! Letter1
insideb! Envelope1

; initial state
[agent,physobj] !HoldsAt(SittingOn(agent,physobj),0).
[agent,physobj] !HoldsAt(LyingOn(agent,physobj),0).
HoldsAt(Dressed(Recipient1),0).
HoldsAt(Dressed(Carrier1),0).
HoldsAt(Awake(Recipient1),0).
HoldsAt(Awake(Carrier1),0).
HoldsAt(Sleep3(Recipient1),0).
HoldsAt(Sleep3(Carrier1),0).
HoldsAt(Standing(Recipient1),0).
HoldsAt(Standing(Carrier1),0).
HoldsAt(ContainerClosed(Envelope1),0).
HoldsAt(Inside(Letter1,Envelope1),0).
[physobj1,physobj2]
!(physobj1=Letter1 & physobj2=Envelope1) ->
!HoldsAt(Inside(physobj1, physobj2),0).
[agent,object] !HoldsAt(See(agent,object),0).
[agent,object]
!(agent=Carrier1 & object=Envelope1) ->
!HoldsAt(Holding(agent,object),0).
HoldsAt(Holding(Carrier1,Envelope1),0).
[physobj1,physobj2] !HoldsAt(On(physobj1, physobj2),0).

; narrative
Happens(PlaceOn(Carrier1,Envelope1,Surface1),0).
Happens(TakeOffOf(Recipient1,Envelope1,Surface1),1).
;Happens(HandTo(Carrier1,Recipient1,Envelope1),0).
Happens(SitOn(Recipient1,Chair1),2).
Happens(ContainerOpen(Recipient1,Envelope1),3).
Happens(TakeOutOf(Recipient1,Letter1,Envelope1),4).
Happens(LookAt(Recipient1,Letter1),5).
Happens(Read(Recipient1,Letter1,Content1),6).
Happens(ThinkAbout(Recipient1,Content1),7).
Happens(Understand(Recipient1,Content1),8).
Happens(PutInside(Recipient1,Letter1,Envelope1),9).
Happens(RiseFrom(Recipient1,Chair1),10).
Happens(PlaceOn(Recipient1,Envelope1,Surface1),11).

range time 0 12
range offset 0 0
range diameter 0 0

completion Happens

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Vehicle.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; Vehicle: transportation vehicles
;

sort vehicle: physobj
sort vehiclein: vehicle
sort vehicleon: vehicle
sort train: vehicleon
sort carriage: vehiclein

sort vehicledoor

; RideTrack

event RideTrack12(train,track)

event RideTrack21(train,track)

[train,track,time]
Happens(RideTrack12(train,track),time) ->
HoldsAt(At(train,Side1(track)),time).

[train,track,time]
Happens(RideTrack21(train,track),time) ->
HoldsAt(At(train,Side2(track)),time).

[train,track,location,time]
Side2(track)=location ->
Initiates(RideTrack12(train,track),At(train,location),time).

[train,track,location,time]
Side1(track)=location ->
Initiates(RideTrack21(train,track),At(train,location),time).

[train,track,location,time]
Side1(track)=location ->
Terminates(RideTrack12(train,track),At(train,location),time).

[train,track,location,time]
Side2(track)=location ->
Terminates(RideTrack21(train,track),At(train,location),time).

; DriveStreet

event DriveStreet12(vehicle,street)

event DriveStreet21(vehicle,street)

[vehicle,street,time]
Happens(DriveStreet12(vehicle,street),time) ->
HoldsAt(At(vehicle,Side1(street)),time).

[vehicle,street,time]
Happens(DriveStreet21(vehicle,street),time) ->
HoldsAt(At(vehicle,Side2(street)),time).

[vehicle,street,location,time]
Side2(street)=location ->
Initiates(DriveStreet12(vehicle,street),At(vehicle,location),time).

[vehicle,street,location,time]
Side1(street)=location ->
Initiates(DriveStreet21(vehicle,street),At(vehicle,location),time).

[vehicle,street,location,time]
Side1(street)=location ->
Terminates(DriveStreet12(vehicle,street),At(vehicle,location),time).

[vehicle,street,location,time]
Side2(street)=location ->
Terminates(DriveStreet21(vehicle,street),At(vehicle,location),time).

; Pulling

event PointToward(agent,horse,street)

fluent PointedToward(horse,street)

[horse,street1,street2,time]
HoldsAt(PointedToward(horse,street1),time) &
HoldsAt(PointedToward(horse,street2),time) ->
street1=street2.

[agent,horse,street,time]
Initiates(PointToward(agent,horse,street),
          PointedToward(horse,street),
          time).

[agent,horse,street1,street2,time]
HoldsAt(PointedToward(horse,street1),time) ->
Terminates(PointToward(agent,horse,street2),
           PointedToward(horse,street1),
           time).

[horse,vehicle,street,time]
Terminates(PullStreet12(horse,vehicle,street),
           PointedToward(horse,street),
           time).

[horse,vehicle,street,time]
Terminates(PullStreet21(horse,vehicle,street),
           PointedToward(horse,street),
           time).

[horse,street,time]
HoldsAt(PointedToward(horse,street),time) ->
HoldsAt(NearPortal(horse,street),time).

event Lash(agent,horse)

fluent HitchedTo(horse,vehicle)

[horse,vehicle,location,time]
HoldsAt(HitchedTo(horse,vehicle),time) &
HoldsAt(At(vehicle,location),time) ->
HoldsAt(At(horse,location),time).

[agent,horse,vehicle,street,time]
Happens(Lash(agent,horse),time) &
HoldsAt(PointedToward(horse,street),time) &
HoldsAt(HitchedTo(horse,vehicle),time) &
HoldsAt(At(horse,Side1(street)),time) ->
Happens(PullStreet12(horse,vehicle,street),time).

[agent,horse,vehicle,street,time]
Happens(Lash(agent,horse),time) &
HoldsAt(PointedToward(horse,street),time) &
HoldsAt(HitchedTo(horse,vehicle),time) &
HoldsAt(At(horse,Side2(street)),time) ->
Happens(PullStreet21(horse,vehicle,street),time).

event PullStreet12(horse,vehicle,street)

event PullStreet21(horse,vehicle,street)

[horse,vehicle,street,time]
Happens(PullStreet12(horse,vehicle,street),time) ->
Happens(DriveStreet12(vehicle,street),time).

[horse,vehicle,street,time]
Happens(PullStreet21(horse,vehicle,street),time) ->
Happens(DriveStreet21(vehicle,street),time).

[horse,vehicle,street,time]
Happens(PullStreet12(horse,vehicle,street),time) ->
HoldsAt(At(horse,Side1(street)),time).

[horse,vehicle,street,time]
Happens(PullStreet21(horse,vehicle,street),time) ->
HoldsAt(At(horse,Side2(street)),time).

[horse,vehicle,street,location,time]
Side2(street)=location ->
Initiates(PullStreet12(horse,vehicle,street),At(horse,location),time).

[horse,vehicle,street,location,time]
Side1(street)=location ->
Initiates(PullStreet21(horse,vehicle,street),At(horse,location),time).

[horse,vehicle,street,location,time]
Side1(street)=location ->
Terminates(PullStreet12(horse,vehicle,street),At(horse,location),time).

[horse,vehicle,street,location,time]
Side2(street)=location ->
Terminates(PullStreet21(horse,vehicle,street),At(horse,location),time).

; OnVehicle

fluent OnVehicle(object,vehicleon)

event GetOnVehicle(agent,vehicleon)

event GetOffVehicle(agent,vehicleon)

[vehicleon1,vehicleon2,time]
HoldsAt(OnVehicle(vehicleon1,vehicleon2),time) ->
vehicleon1!=vehicleon2.

[vehicleon1,vehicleon2,time]
HoldsAt(OnVehicle(vehicleon1,vehicleon2),time) ->
!HoldsAt(OnVehicle(vehicleon2,vehicleon1),time).

[agent,vehicleon,time]
Initiates(GetOnVehicle(agent,vehicleon),OnVehicle(agent,vehicleon),time).

[agent,vehicleon,time]
Happens(GetOnVehicle(agent,vehicleon),time) ->
{location}
 HoldsAt(At(agent,location),time) &
 HoldsAt(At(vehicleon,location),time).

[agent,vehicleon,time]
Terminates(GetOffVehicle(agent,vehicleon),OnVehicle(agent,vehicleon),time).

[agent,vehicleon,time]
Happens(GetOffVehicle(agent,vehicleon),time) ->
HoldsAt(OnVehicle(agent,vehicleon),time).

[agent,vehicleon,location,time]
Releases(GetOnVehicle(agent,vehicleon),
         At(agent,location),
         time).

;[agent,vehicleon,location1,location2,time]
;HoldsAt(At(vehicleon,location1),time) &
;location1 != location2 ->
;Terminates(GetOffVehicle(agent,vehicleon),
;           At(agent,location2),
;           time).

[agent,vehicleon,location,time]
HoldsAt(At(vehicleon,location),time) ->
Initiates(GetOffVehicle(agent,vehicleon),
          At(agent,location),
          time).

[object,vehicleon,location,time]
HoldsAt(OnVehicle(object,vehicleon),time) &
HoldsAt(At(vehicleon,location),time) ->
HoldsAt(At(object,location),time).

; InVehicle

fluent InVehicle(object,vehiclein)

event GetInVehicle(agent,vehiclein)

event GetOutOfVehicle(agent,vehiclein)

[vehiclein1,vehiclein2,time]
HoldsAt(InVehicle(vehiclein1,vehiclein2),time) ->
vehiclein1!=vehiclein2.

[vehiclein1,vehiclein2,time]
HoldsAt(InVehicle(vehiclein1,vehiclein2),time) ->
!HoldsAt(InVehicle(vehiclein2,vehiclein1),time).

[agent,vehiclein,time]
Initiates(GetInVehicle(agent,vehiclein),InVehicle(agent,vehiclein),time).

[agent,vehiclein,time]
Happens(GetInVehicle(agent,vehiclein),time) ->
{location}
 HoldsAt(At(agent,location),time) &
 HoldsAt(At(vehiclein,location),time).

[agent,vehiclein,time]
Terminates(GetOutOfVehicle(agent,vehiclein),InVehicle(agent,vehiclein),time).

[agent,vehiclein,time]
Happens(GetOutOfVehicle(agent,vehiclein),time) ->
HoldsAt(InVehicle(agent,vehiclein),time).

[agent,vehiclein,location,time]
Releases(GetInVehicle(agent,vehiclein),
         At(agent,location),
         time).

;[agent,vehiclein,location1,location2,time]
;HoldsAt(At(vehiclein,location1),time) &
;location1 != location2 ->
;Terminates(GetOutOfVehicle(agent,vehiclein),
;           At(agent,location2),
;           time).

[agent,vehiclein,location,time]
HoldsAt(At(vehiclein,location),time) ->
Initiates(GetOutOfVehicle(agent,vehiclein),
          At(agent,location),
          time).

[object,vehiclein,location,time]
HoldsAt(InVehicle(object,vehiclein),time) &
HoldsAt(At(vehiclein,location),time) ->
HoldsAt(At(object,location),time).

; vehicle door
; door does not have to be open for entry; passenger can jump in

event VehicleDoorOpen(agent,vehicledoor)

event VehicleDoorClose(agent,vehicledoor)

fluent VehicleDoorIsOpen(vehicledoor)

[agent,vehicledoor,time]
Happens(VehicleDoorOpen(agent,vehicledoor),time) ->
HoldsAt(Awake(agent),time) &
!HoldsAt(VehicleDoorIsOpen(vehicledoor),time).

[agent,vehicledoor,time]
Initiates(VehicleDoorOpen(agent,vehicledoor),
          VehicleDoorIsOpen(vehicledoor),
          time).

[agent,vehicledoor,time]
Happens(VehicleDoorClose(agent,vehicledoor),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(VehicleDoorIsOpen(vehicledoor),time).

[agent,vehicledoor,time]
Terminates(VehicleDoorClose(agent,vehicledoor),
           VehicleDoorIsOpen(vehicledoor),
           time).

; ticketagent

sort ticketagent: agent

fluent BeTicketAgent0(ticketagent)
fluent BeTicketAgent1(ticketagent)
fluent BeTicketAgent2(ticketagent)

xor BeTicketAgent0, BeTicketAgent1, BeTicketAgent2

[ticketagent,agent,ticket,time]
HoldsAt(BeTicketAgent0(ticketagent),time) ->
Terminates(Request(agent,ticketagent,ticket),
           BeTicketAgent0(ticketagent),
           time).

[ticketagent,agent,ticket,time]
HoldsAt(BeTicketAgent0(ticketagent),time) ->
Initiates(Request(agent,ticketagent,ticket),
          BeTicketAgent1(ticketagent),
          time).

[ticketagent,agent,ticket,time]
HoldsAt(BeTicketAgent1(ticketagent),time) &
HoldsAt(KnowRequest(ticketagent,agent,ticket),time) ->
Happens(PickUp(ticketagent,ticket),time).

[ticketagent,ticket,time]
HoldsAt(BeTicketAgent1(ticketagent),time) ->
Terminates(PickUp(ticketagent,ticket),
           BeTicketAgent1(ticketagent),
           time).

[ticketagent,ticket,time]
HoldsAt(BeTicketAgent1(ticketagent),time) ->
Initiates(PickUp(ticketagent,ticket),
          BeTicketAgent2(ticketagent),
          time).

[ticketagent,agent,ticket,time]
HoldsAt(BeTicketAgent2(ticketagent),time) &
HoldsAt(KnowRequest(ticketagent,agent,ticket),time) ->
Happens(HandTo(ticketagent,agent,ticket),time).

[ticketagent,ticket,agent,time]
HoldsAt(BeTicketAgent2(ticketagent),time) ->
Terminates(HandTo(ticketagent,agent,ticket),
           BeTicketAgent2(ticketagent),
           time).

[ticketagent,ticket,agent,time]
HoldsAt(BeTicketAgent2(ticketagent),time) ->
Initiates(HandTo(ticketagent,agent,ticket),
          BeTicketAgent0(ticketagent),
          time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Ontology.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; integer
;

sort diameter: integer

; object

sort object

sort agent: object

sort physobj: object
sort bed: physobj
sort snowflake: physobj
sort sky: physobj

sort stuff: physobj

sort surface: physobj
sort ground: surface

sort snow: stuff
sort ball

sort food: physobj
sort fruit: food
sort orange: fruit
sort salad: food

sort clothing: physobj
sort scarf: clothing
sort hat: clothing

sort vegetablematter: physobj
sort coal: vegetablematter

sort bodypart: physobj
sort hand: bodypart

sort papertowels: physobj
sort device: physobj
sort electronicdevice: device
sort lamp: electronicdevice

sort cat: physobj
sort horse: physobj

sort weapon: physobj
sort gun: weapon
sort bomb: weapon
sort bullet: weapon

; location

sort location
sort room: location, outside: location

; portal

sort portal
sort door: portal, staircase: portal
sort street: portal
sort track: portal

sort building

sort fire: object
sort smoke: physobj

sort furniture: physobj
sort chair: furniture
sort table: furniture

sort bill: physobj
sort ticket: physobj
sort envelope: physobj

sort text: physobj
sort book: text
sort letter: text
sort menu: text

sort paper: physobj

sort content
sort script

sort container: physobj
sort cigarette: physobj
sort ashtray: physobj
sort umbrella: physobj

sort pen: physobj

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Rest.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Mueller:InPress,
;   author = "Erik T. Mueller",
;   year = "in press",
;   title = "Modelling space and time in narratives about restaurants",
;   journal = "Literary and Linguistic Computing",
; }
;

option renaming off
option encoding 3

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/MuellerInPress/RepRest.e

door MainEntrance1

; room-scale topological space
outside Street1
room DiningRoom1
door KitchenDoor1
room Kitchen1
Side1(MainEntrance1)=Street1.
Side2(MainEntrance1)=DiningRoom1.
Side1(KitchenDoor1)=DiningRoom1.
Side2(KitchenDoor1)=Kitchen1.

agent Customer1
menu Menu1
chair Chair1
food Food1
HoldsAt(At(Customer1,Street1),0).
HoldsAt(Hungry(Customer1),0).
HoldsAt(At(Chair1,DiningRoom1),0).
HoldsAt(At(Menu1,DiningRoom1),0).
HoldsAt(On(Menu1,Table1),0).
HoldsAt(At(Food1,Kitchen1),0).

waiter Waiter1
cook Cook1

; props
table Table1
bill Bill1

; restaurant
restaurant Restaurant1
CookOf(Restaurant1)=Cook1.
TableOf(Restaurant1)=Table1.
WaiterOf(Restaurant1)=Waiter1.
KitchenDoorOf(Restaurant1)=KitchenDoor1.
BillOf(Restaurant1)=Bill1.

; prune
sort ona, onb
fluent! On(ona,onb)
event! PlaceOn(agent,ona,onb)
event! TakeOffOf(agent,ona,onb)

sort ordera, orderb, orderc
event! Order(ordera,orderb,orderc)
fluent! KnowOrder(orderb,ordera,orderc)

sort requesta, requestb, requestc
event! Request(requesta,requestb,requestc)
fluent! KnowRequest(requestb,requesta,requestc)

sort holda, holdb, holdc
event! TakeOffOf(holda,holdb,holdc)
event! PickUp(holda,holdb)
event! LetGoOf(holda,holdb)
event! Hold(holda,holdb)
fluent! Holding(holda,holdb)

sort sita, sitb
event! LieOn(sita,sitb)
event! SitOn(sita,sitb)
event! RiseFrom(sita,sitb)
fluent! LyingOn(sita,sitb)
fluent! SittingOn(sita,sitb)

sort greeta, greetb
event! Greet(greeta,greetb)

ona! Menu1, Food1, Bill1
onb! Table1
ordera! Customer1, Waiter1
orderb! Waiter1, Cook1
orderc! Food1
requesta! Customer1
requestb! Waiter1
requestc! Bill1
holda! Customer1, Waiter1
holdb! Menu1, Food1, Bill1
holdc! Table1
sita! Customer1
sitb! Chair1
greeta! Customer1, Waiter1
greetb! Customer1, Waiter1

; initial situation
HoldsAt(At(Waiter1,DiningRoom1),0).
HoldsAt(At(Cook1,Kitchen1),0).
HoldsAt(At(Table1,DiningRoom1),0).
!HoldsAt(On(Bill1,Table1),0).
HoldsAt(At(Bill1,DiningRoom1),0).
[agent] HoldsAt(Standing(agent),0).
[agent,object] !HoldsAt(Holding(agent,object),0).
[agent1,agent2,physobj] !HoldsAt(KnowOrder(agent1,agent2,physobj),0).
[agent1,agent2,physobj] !HoldsAt(KnowRequest(agent1,agent2,physobj),0).
HoldsAt(BeWaiter0(Waiter1),0).
HoldsAt(BeCook0(Cook1),0).
[food] !HoldsAt(FoodPrepared(food),0).
!HoldsAt(Hungry(Cook1),0).
!HoldsAt(Hungry(Waiter1),0).

Happens(WalkThroughDoor12(Customer1,MainEntrance1),0).
Happens(Greet(Waiter1,Customer1),1).
Happens(SitOn(Customer1,Chair1),2).
Happens(TakeOffOf(Customer1,Menu1,Table1),3).
Happens(Order(Customer1,Waiter1,Food1),4).
Happens(PlaceOn(Customer1,Menu1,Table1),5).
Happens(Eat(Customer1,Food1),11).
Happens(Request(Customer1,Waiter1,Bill1),12).
Happens(Pay(Customer1,Waiter1),15).
Happens(Tip(Customer1,Waiter1),15).
Happens(RiseFrom(Customer1,Chair1),16).
Happens(SayGoodbye(Customer1,Waiter1),17).
Happens(WalkThroughDoor21(Customer1,MainEntrance1),18).

range time 0 19
range offset 0 0
range diameter 0 0

completion Happens

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/EatingInAHouse.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Mueller:2004c,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Understanding script-based stories using commonsense reasoning",
;   journal = "Cognitive Systems Research",
;   volume = "5",
;   number = "4",
;   pages = "307--340",
; }
;

option modeldiff on
option encoding 3
option renaming off

ignore Love, ThreatenedBy
ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
ignore Inside
ignore Near, WalkFrom, WalkFromTo, RunFromTo
ignore BillOf, CookOf, TableOf, WaiterOf, KitchenDoorOf
ignore BeWaiter0, BeWaiter1, BeWaiter2, BeWaiter3, BeWaiter4
ignore BeWaiter5, BeWaiter6, BeWaiter7, BeWaiter8, BeWaiter9
ignore BeCook0, BeCook1
ignore InviteIn, InvitedIn, IntendToWalkIn, IntentionToWalkIn
ignore ActOnIntentionToWalkIn, Greet, SayGoodbye, CryForJoy
ignore Threaten, ReleaseFromThreat, ThreatenedBy
ignore Order, KnowOrder, Request, KnowRequest
ignore PutInside, TakeOutOf
ignore SayPleaseToMeet, Move

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/HungerNeed.e
load answers/Mueller2004c/Restaurant.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2003/SpeechAct.e
load answers/Mueller2004c/Dress.e

room Upstairs1

staircase Staircase1

room Hallway1

Side1(Staircase1)=Hallway1.
Side2(Staircase1)=Upstairs1.

door DiningRoomDoor1

room DiningRoom1

Side1(DiningRoomDoor1)=Hallway1.
Side2(DiningRoomDoor1)=DiningRoom1.

door KitchenDoor1

room Kitchen1

Side1(KitchenDoor1)=DiningRoom1.
Side2(KitchenDoor1)=Kitchen1.

agent Eater1

agent Eater2

clothing Clothing1

clothing Clothing2

chair Chair1

chair Chair2

food Food1

agent Cook1

table Table1

content Content1

content Content2

outside DummyOutside1

; prune
sort ona, onb
fluent! On(ona,onb)
event! PlaceOn(agent,ona,onb)
event! TakeOffOf(agent,ona,onb)

sort ordera, orderb, orderc
event! Order(ordera,orderb,orderc)
fluent! KnowOrder(orderb,ordera,orderc)

sort requesta, requestb, requestc
event! Request(requesta,requestb,requestc)
fluent! KnowRequest(requestb,requesta,requestc)

sort holda, holdb, holdc
event! TakeOffOf(holda,holdb,holdc)
event! PickUp(holda,holdb)
event! LetGoOf(holda,holdb)
event! Hold(holda,holdb)
fluent! Holding(holda,holdb)

sort sita, sitb
event! LieOn(sita,sitb)
event! SitOn(sita,sitb)
event! RiseFrom(sita,sitb)
fluent! LyingOn(sita,sitb)
fluent! SittingOn(sita,sitb)

ona! Food1
onb! Table1
holda! Cook1
holdb! Food1
holdc! Table1
sita! Eater1
sitb! Chair1

; initial situation
[agent] HoldsAt(Dressed(agent),0).
[agent] HoldsAt(Awake(agent),0).
[agent] HoldsAt(Sleep3(agent),0).
[agent] HoldsAt(Standing(agent),0).
[agent] HoldsAt(Standing(agent),0).
[agent,object] !HoldsAt(Holding(agent,object),0).
[food] HoldsAt(At(food,Kitchen1),0).
[food] !HoldsAt(FoodPrepared(food),0).
[agent] HoldsAt(Hungry(agent),0).
[door] HoldsAt(DoorIsOpen(door),0).
[clothing] HoldsAt(At(clothing,Upstairs1),0).
[chair] HoldsAt(At(chair,DiningRoom1),0).
HoldsAt(At(Cook1,Kitchen1),0).
HoldsAt(At(Table1,DiningRoom1),0).
[agent,clothing] !HoldsAt(Wearing(agent,clothing),0).

; narrative
HoldsAt(At(Cook1,Kitchen1),0).
HoldsAt(At(Eater1,Upstairs1),0).
HoldsAt(At(Eater2,Upstairs1),0).
Happens(FoodPrepare(Cook1,Food1),0).
Happens(PutOn(Eater1,Clothing1),1).
Happens(PutOn(Eater2,Clothing2),2).
Happens(WalkDownStaircase(Eater1,Staircase1),3).
Happens(WalkDownStaircase(Eater2,Staircase1),4).
Happens(WalkThroughDoor12(Eater1,DiningRoomDoor1),5).
Happens(WalkThroughDoor12(Eater2,DiningRoomDoor1),6).
Happens(SitOn(Eater1,Chair1),7).
Happens(SitOn(Eater2,Chair2),8).
Happens(PickUp(Cook1, Food1),9).
Happens(WalkThroughDoor21(Cook1, KitchenDoor1),10).
Happens(PlaceOn(Cook1, Food1, Table1),11).
Happens(WalkThroughDoor12(Cook1, KitchenDoor1),12).
Happens(Eat(Eater1,Food1),13).
Happens(Eat(Eater2,Food1),14).
Happens(Converse(Eater1,Eater2),15).
Happens(TalkAbout(Eater1,Content1),16).
Happens(TalkAbout(Eater2,Content2),17).
Happens(RiseFrom(Eater1,Chair1),18).
Happens(RiseFrom(Eater2,Chair2),19).

range time 0 20
range offset 0 0
range diameter 0 0

completion Happens

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/ShootingAttack.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; DEV-MUC3-0147
; ShootingAttack
;
; @article{Mueller:2004c,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Understanding script-based stories using commonsense reasoning",
;   journal = "Cognitive Systems Research",
;   volume = "5",
;   number = "4",
;   pages = "307--340",
; }
;

ignore SkyOf, GroundOf, Near, WalkFromTo, RunFromTo
ignore RollAlong, Diameter, Move, HoldSome
ignore TakeOutOf, On, DoorUnlock, DoorLock, WalkThroughDoor12
ignore WalkThroughDoor21, WalkDownStaircase, WalkUpStaircase

ignore Love, ThreatenedBy

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/Condition.e
load answers/Mueller2004c/Gun.e
load answers/Mueller2003/Sleep.e

gun Gun1
bullet Bullet1
HoldsAt(Intact(Gun1),0).
HoldsAt(Intact(Bullet1),0).

agent Perp1

physobj PhysTarget1
HoldsAt(Intact(PhysTarget1),0).
HoldsAt(At(PhysTarget1,Outside1),0).

; prune
sort shoota, shootb, shooto, shooth, shootp
event! Shoot(shoota,shootb,shooto)
event! ShootInjure(shoota,shootb,shooth)
event! ShootKill(shoota,shootb,shooth)
event! ShootDamage(shoota,shootb,shootp)
event! ShootDestroy(shoota,shootb,shootp)
shoota! Perp1
shootb! Gun1
shooto! PhysTarget1
shootp! PhysTarget1

; room-scale topological space
outside Outside1
outside Outside2
room Inside1
door Door1
building Building1
street Street1
Side1(Door1)=Outside1.
Side2(Door1)=Inside1.
LookOutOnto(Inside1)=Outside1.
Floor(Inside1)=1.
BuildingOf(Inside1)=Building1.
Side1(Street1)=Outside1.
Side2(Street1)=Outside2.

HoldsAt(Alive(Perp1),0).
HoldsAt(Awake(Perp1),0).
HoldsAt(Standing(Perp1),0).
HoldsAt(Sleep2(Perp1),0).
!HoldsAt(Injured(Perp1),0).
[object] !HoldsAt(Holding(Perp1,object),0).
HoldsAt(At(Gun1,Outside2),0).
HoldsAt(At(Perp1,Outside2),0).
HoldsAt(At(Bullet1,Outside2),0).
HoldsAt(DoorIsOpen(Door1),0).
HoldsAt(DoorUnlocked(Door1),0).
[physobj1,physobj2] !HoldsAt(Inside(physobj1,physobj2),0).

; narrative
Happens(PickUp(Perp1,Gun1),0).
Happens(PickUp(Perp1,Bullet1),1).
Happens(PutInside(Perp1,Bullet1,Gun1),2).
Happens(WalkStreet21(Perp1,Street1),3).
Happens(Shoot(Perp1,Gun1,PhysTarget1),4).
Happens(ShootDestroy(Perp1,Gun1,PhysTarget1),4).
Happens(WalkStreet12(Perp1,Street1),5).

range time 0 6
range offset 0 3
range diameter 0 0

completion Happens

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/RidingInACarriage.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Mueller:2004c,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Understanding script-based stories using commonsense reasoning",
;   journal = "Cognitive Systems Research",
;   volume = "5",
;   number = "4",
;   pages = "307--340",
; }
;

option modeldiff on
option renaming off

ignore Love, ThreatenedBy
ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
ignore Inside, PutInside, On, PlaceOn
ignore Like, Happy, BecomeAngryAt
ignore Near, WalkFrom, WalkFromTo, RunFromTo
ignore ActOnIntentionToWalkIn, IntentionToWalkIn, InvitedIn, InviteIn
ignore TakeOffOf, TakeOutOf, LetGoOf
ignore Greet, SayGoodbye, Order, KnowOrder
ignore Request, KnowRequest
ignore TakeOffOf, PickUp, LetGoOf, Hold, Holding, HandTo, Grab
ignore Move

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/HandTo.e
load answers/Mueller2004c/Money.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2003/SpeechAct.e
load answers/Mueller2004c/Vehicle.e

outside NearLocation1

street Street1

outside Location1

Side1(Street1)=NearLocation1.
Side2(Street1)=Location1.

outside BeforeLocation1

street StreetToLocation1

Side1(StreetToLocation1)=BeforeLocation1.
Side2(StreetToLocation1)=Location1.

outside BetweenLocation1And2

street StreetToBetweenLocation1And2

Side1(StreetToBetweenLocation1And2)=Location1.
Side2(StreetToBetweenLocation1And2)=BetweenLocation1And2.

outside Location2
street StreetToLocation2

Side1(StreetToLocation2)=BetweenLocation1And2.
Side2(StreetToLocation2)=Location2.

outside NearLocation2

street Street2

Side1(Street2)=Location2.
Side2(Street2)=NearLocation2.

agent Passenger1

agent Driver1

chair CarriageSeat1

carriage Carriage1

vehicledoor CarriageDoor1

horse Horse1

; prune
sort sita, sitb
event! LieOn(sita,sitb)
event! SitOn(sita,sitb)
event! RiseFrom(sita,sitb)
fluent! LyingOn(sita,sitb)
fluent! SittingOn(sita,sitb)
sita! Passenger1
sitb! CarriageSeat1

; release
option manualrelease on
[ball, diameter] !ReleasedAt(Diameter(ball, diameter), 0).
[agent, object] !ReleasedAt(Holding(agent, object), 0).
[door] !ReleasedAt(DoorUnlocked(door), 0).
[door] !ReleasedAt(DoorIsOpen(door), 0).
[agent] !ReleasedAt(Sleep0(agent), 0).
[agent] !ReleasedAt(Sleep1(agent), 0).
[agent] !ReleasedAt(Sleep2(agent), 0).
[agent] !ReleasedAt(Sleep3(agent), 0).
[agent] !ReleasedAt(Sleep4(agent), 0).
[agent] !ReleasedAt(Sleep5(agent), 0).
[agent] !ReleasedAt(Sleep6(agent), 0).
[agent, physobj] !ReleasedAt(LyingOn(agent, physobj), 0).
[agent, physobj] !ReleasedAt(SittingOn(agent, physobj), 0).
[agent] !ReleasedAt(Standing(agent), 0).
[agent] !ReleasedAt(Dressed(agent), 0).
[agent1, agent2, physobj] !ReleasedAt(KnowRequest(agent1, agent2, physobj), 0).
[horse, street] !ReleasedAt(PointedToward(horse, street), 0).
[horse, vehicle] !ReleasedAt(HitchedTo(horse, vehicle), 0).
[object, vehicleon] !ReleasedAt(OnVehicle(object, vehicleon), 0).
[object, vehiclein] !ReleasedAt(InVehicle(object, vehiclein), 0).
[vehicledoor] !ReleasedAt(VehicleDoorIsOpen(vehicledoor), 0).
[ticketagent] !ReleasedAt(BeTicketAgent0(ticketagent), 0).
[ticketagent] !ReleasedAt(BeTicketAgent1(ticketagent), 0).
[ticketagent] !ReleasedAt(BeTicketAgent2(ticketagent), 0).
; special cases
[location] ReleasedAt(At(CarriageSeat1,location),0).
[location] ReleasedAt(At(Driver1,location),0).
[object, location]
object!=CarriageSeat1 & object!=Driver1 ->
!ReleasedAt(At(object, location), 0).

; initial state
[agent,object] !HoldsAt(Holding(agent,object),0).
[agent,physobj] !HoldsAt(SittingOn(agent,physobj),0).
[agent,physobj] !HoldsAt(LyingOn(agent,physobj),0).
[physobj1,physobj2] !HoldsAt(On(physobj1, physobj2),0).
[agent] HoldsAt(Dressed(agent),0).
[agent] HoldsAt(Awake(agent),0).
[agent] HoldsAt(Sleep3(agent),0).
[agent] HoldsAt(Standing(agent),0).
HoldsAt(At(Passenger1,NearLocation1),0).
HoldsAt(At(Horse1,BeforeLocation1),0).;;;;;
HoldsAt(At(Carriage1,BeforeLocation1),0).
HoldsAt(InVehicle(CarriageSeat1,Carriage1),0).
HoldsAt(InVehicle(Driver1,Carriage1),0).
[object]
object!=CarriageSeat1 & object!=Driver1 ->
!HoldsAt(InVehicle(object,Carriage1),0).
[agent1,agent2,physobj] !HoldsAt(KnowOrder(agent1,agent2,physobj),0).
[agent1,agent2,physobj] !HoldsAt(KnowRequest(agent1,agent2,physobj),0).
!HoldsAt(VehicleDoorIsOpen(CarriageDoor1),0).
HoldsAt(HitchedTo(Horse1,Carriage1),0).
HoldsAt(PointedToward(Horse1,StreetToLocation1),0).

; narrative
Happens(WalkStreet12(Passenger1,Street1),0).
Happens(Lash(Driver1,Horse1),1).
Happens(VehicleDoorOpen(Passenger1,CarriageDoor1),2).
Happens(GetInVehicle(Passenger1,Carriage1),3).
Happens(VehicleDoorClose(Passenger1,CarriageDoor1),4).
Happens(SitOn(Passenger1,CarriageSeat1),5).
Happens(PointToward(Driver1,Horse1,StreetToBetweenLocation1And2),6).
Happens(Lash(Driver1,Horse1),7).
Happens(PointToward(Driver1,Horse1,StreetToLocation2),8).
Happens(Lash(Driver1,Horse1),9).
Happens(RiseFrom(Passenger1,CarriageSeat1),10).
Happens(VehicleDoorOpen(Passenger1,CarriageDoor1),11).
Happens(GetOutOfVehicle(Passenger1,Carriage1),12).
Happens(VehicleDoorClose(Passenger1,CarriageDoor1),13).
Happens(WalkStreet12(Passenger1,Street2),14).

range time 0 15
range offset 0 0
range diameter 0 0

completion Happens

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/HandTo.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;

event HandTo(agent,agent,physobj)

[agent1,agent2,physobj,time]
Initiates(HandTo(agent1,agent2,physobj),
          Holding(agent2,physobj),
          time).

[agent1,agent2,physobj,time]
Terminates(HandTo(agent1,agent2,physobj),
           Holding(agent1,physobj),
           time).

[agent1,agent2,physobj,time]
Happens(HandTo(agent1,agent2,physobj),time) ->
HoldsAt(Holding(agent1,physobj),time).

event ShakeHands(agent,agent)

event WriteOn(agent,paper,pen)



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Baseball.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{EthanAllen:1982,
;   author = "Ethan Allen",
;   year = "1982",
;   title = "Baseball Play and Strategy",
;   address = "Robert E. Krieger",
;   publisher = "Malabar, FL",
;   edition = "Third",
; }
;
; @book{Coombs:1967,
;   author = "Jack Coombs",
;   year = "1967",
;   title = "Baseball",
;   address = "Englewood Cliffs, NJ",
;   publisher = "Prentice-Hall",
;   edition = "4th",
;   howpublished = "revised by Danny Litwhiler",
; }
;

sort ballgame

sort hardball: ball

sort base: physobj
sort firstbase: base
;sort secondbase: base
;sort thirdbase: base
sort homeplate: base

sort mound: physobj
sort pitchermound: mound

;sort furniture: physobj
;sort bench: furniture
;sort playerbench: bench

sort field: physobj

;sort shortstoparea: field
;sort catcherarea: field

sort outfield: field
;sort leftfield: outfield
;sort centerfield: outfield
;sort rightfield: outfield

function BallOf(ballgame): hardball
function FirstBaseOf(ballgame): firstbase
;function SecondBaseOf(ballgame): secondbase
;function ThirdBaseOf(ballgame): thirdbase
function HomeplateOf(ballgame): homeplate
function OutfieldOf(ballgame): outfield
function PitchermoundOf(ballgame): pitchermound
function PlayerbenchOf(ballgame): playerbench

predicate HomeTeamPlayer(ballgame,agent)
predicate VisitingTeamPlayer(ballgame,agent)
predicate Player(ballgame,agent)
predicate OnOppositeTeams(ballgame,agent,agent)

event Pitch(ballgame,agent,hardball,agent)
event PitchInStrikeZone(ballgame,agent,hardball,agent)
event PitchOutOfStrikeZone(ballgame,agent,hardball,agent)
event Swing(ballgame,agent,hardball)
event SwingMiss(ballgame,agent,hardball)
event SwingHit(ballgame,agent,hardball)
event SwingHitFair(ballgame,agent,hardball)
event SwingHitFoul(ballgame,agent,hardball)
event SwingHitFairFly(ballgame,agent,hardball)
event SwingHitFairGround(ballgame,agent,hardball)

[ballgame,agent]
HomeTeamPlayer(ballgame,agent) ->
!VisitingTeamPlayer(ballgame,agent).

[ballgame,agent] HomeTeamPlayer(ballgame,agent) -> Player(ballgame,agent).

[ballgame,agent] VisitingTeamPlayer(ballgame,agent) -> Player(ballgame,agent).

[ballgame,agent1,agent2]
OnOppositeTeams(ballgame,agent1,agent2) <->
(HomeTeamPlayer(ballgame,agent1) &
 VisitingTeamPlayer(ballgame,agent2)) |
(HomeTeamPlayer(ballgame,agent2) &
 VisitingTeamPlayer(ballgame,agent1)).

[ballgame,agent1,hardball,agent2,pitchermound,homeplate]
Happens(Pitch(ballgame,agent1,hardball,agent2),time) &
PitchermoundOf(ballgame) = pitchermound &
HomeplateOf(ballgame) = homeplate ->
HoldsAt(Near(agent1,pitchermound),time) &
HoldsAt(Near(agent2,homeplate),time) &
OnOppositeTeams(ballgame,agent1,agent2).

[ballgame,agent1,agent2,hardball,time]
Happens(Pitch(ballgame,agent1,hardball,agent2),time) ->
Happens(PitchInStrikeZone(ballgame,agent1,hardball,agent2),time) |
Happens(PitchOutOfStrikeZone(ballgame,agent1,hardball,agent2),time).

[ballgame,agent1,agent2,hardball,time]
Happens(PitchInStrikeZone(ballgame,agent1,hardball,agent2),time) ->
!Happens(PitchOutOfStrikeZone(ballgame,agent1,hardball,agent2),time).

[ballgame,agent1,agent2,hardball,time]
Happens(PitchInStrikeZone(ballgame,agent1,hardball,agent2),time) ->
Happens(Swing(ballgame,agent2,hardball),time+1).

[ballgame,agent,hardball,time]
Happens(Swing(ballgame,agent,hardball),time) ->
Happens(SwingHit(ballgame,agent,hardball),time) |
Happens(SwingMiss(ballgame,agent,hardball),time).

[ballgame,agent,hardball,time]
Happens(SwingHit(ballgame,agent,hardball),time) ->
!Happens(SwingMiss(ballgame,agent,hardball),time).

[ballgame,agent,hardball,time]
Happens(SwingHit(ballgame,agent,hardball),time) ->
Happens(SwingHitFair(ballgame,agent,hardball),time) |
Happens(SwingHitFoul(ballgame,agent,hardball),time).

[ballgame,agent,hardball,time]
Happens(SwingHitFair(ballgame,agent,hardball),time) ->
!Happens(SwingHitFoul(ballgame,agent,hardball),time).

[ballgame,agent,hardball,time]
Happens(SwingHitFair(ballgame,agent,hardball),time) ->
Happens(SwingHitFairFly(ballgame,agent,hardball),time) |
Happens(SwingHitFairGround(ballgame,agent,hardball),time).

[ballgame,agent,hardball,time]
Happens(SwingHitFairFly(ballgame,agent,hardball),time) ->
Happens(SwingHitFairGround(ballgame,agent,hardball),time).

[ballgame,agent,hardball,homeplate,firstbase,time]
Happens(SwingHit(ballgame,agent,hardball),time) &
HomeplateOf(ballgame) = homeplate &
FirstBaseOf(ballgame) = firstbase ->
Happens(RunFromTo(agent,homeplate,firstbase),time).

[ballgame,agent,hardball,homeplate,outfield,time]
HomeplateOf(ballgame) = homeplate &
OutfieldOf(ballgame) = outfield &
Happens(SwingHitFairFly(ballgame,agent,hardball),time) ->
Happens(HitFromTo(agent,hardball,homeplate,outfield),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Cognition.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
event Read(agent,text,content)
event ThinkAbout(agent,content)
event Think(agent)
event Understand(agent,content)
event Dream(agent)

[agent,text,content,time]
Happens(Read(agent,text,content),time) ->
HoldsAt(See(agent,text),time).



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/RepRest.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Mueller:InPress,
;   author = "Erik T. Mueller",
;   year = "in press",
;   title = "Modelling space and time in narratives about restaurants",
;   journal = "Literary and Linguistic Computing",
; }
;

;sort boolean
;sort integer
;reified sort predicate
;reified sort function
;
;sort time: integer
;sort offset: integer
;
;reified sort fluent
;reified sort event
;
;predicate Happens(event,time)
;predicate HoldsAt(fluent,time)
;predicate ReleasedAt(fluent,time)
;predicate Initiates(event,fluent,time)
;predicate Terminates(event,fluent,time)
;predicate Releases(event,fluent,time)
;
;sort diameter: integer
;
;sort object
;
;sort agent: object
;
;sort physobj: object
;sort bed: physobj
;sort snowflake: physobj
;sort sky: physobj
;
;sort stuff: physobj
;
;sort surface: physobj
;sort ground: surface
;
;sort snow: stuff
;sort ball
;
;sort food: physobj
;sort fruit: food
;sort orange: fruit
;sort salad: food
;
;sort clothing: physobj
;sort scarf: clothing
;sort hat: clothing
;
;sort vegetablematter: physobj
;sort coal: vegetablematter
;
;sort bodypart: physobj
;sort hand: bodypart
;
;sort papertowels: physobj
;sort device: physobj
;sort electronicdevice: device
;sort lamp: electronicdevice
;
;sort cat: physobj
;
;sort weapon: physobj
;sort gun: weapon
;sort bomb: weapon
;sort bullet: weapon
;
;sort location
;sort room: location, outside: location
;
;sort portal
;sort door: portal, staircase: portal
;sort street: portal
;
;sort building
;
;sort fire: object
;
;sort furniture: physobj
;sort chair: furniture
;sort table: furniture
;
;sort menu: physobj
;sort bill: physobj
;
;sort script
;
fluent Holding(agent,physobj)
event PickUp(agent,physobj)
event LetGoOf(agent,physobj)

[agent,physobj,time]
Initiates(PickUp(agent,physobj),Holding(agent,physobj),time).

[agent,physobj,time]
Happens(PickUp(agent,physobj),time) ->
{location}
  HoldsAt(At(agent,location),time) &
  HoldsAt(At(physobj,location),time).

[agent,physobj,time]
Terminates(LetGoOf(agent,physobj),Holding(agent,physobj),time).

[agent,physobj,time]
Happens(LetGoOf(agent,physobj),time) ->
HoldsAt(Holding(agent,physobj),time).

[agent,physobj,location,time]
Releases(PickUp(agent,physobj),At(physobj,location),time).

[agent,physobj,location,time]
HoldsAt(Holding(agent,physobj),time) &
HoldsAt(At(agent,location),time) ->
HoldsAt(At(physobj,location),time).

;[agent,physobj,location1,location2,time]
;HoldsAt(At(agent,location1),time) &
;location1!=location2 ->
;Terminates(LetGoOf(agent,physobj),At(physobj,location2),time).

[agent,physobj,location,time]
HoldsAt(At(agent,location),time) ->
Initiates(LetGoOf(agent,physobj),At(physobj,location),time).

fluent On(physobj,physobj)

event PlaceOn(agent,physobj,physobj)

event TakeOffOf(agent,physobj,physobj)

[physobj1,physobj2,time]
HoldsAt(On(physobj1,physobj2),time) ->
physobj1!=physobj2.

[physobj1,physobj2,time]
HoldsAt(On(physobj1,physobj2),time) ->
!HoldsAt(On(physobj2,physobj1),time).

[agent,physobj1,physobj2,time]
Initiates(PlaceOn(agent,physobj1,physobj2),
          On(physobj1,physobj2),time).

[agent,physobj1,physobj2,time]
Terminates(PlaceOn(agent,physobj1,physobj2),
           Holding(agent,physobj1),time).

[agent,physobj1,physobj2,time]
Happens(PlaceOn(agent,physobj1,physobj2),time) ->
HoldsAt(Holding(agent,physobj1),time) &
{location}
 HoldsAt(At(agent,location),time) &
 HoldsAt(At(physobj2,location),time).

[agent,physobj1,physobj2,time]
Terminates(TakeOffOf(agent,physobj1,physobj2),
           On(physobj1,physobj2),time).

[agent,physobj1,physobj2,time]
Initiates(TakeOffOf(agent,physobj1,physobj2),
          Holding(agent,physobj1),time).

[agent,physobj1,physobj2,location,time]
Releases(TakeOffOf(agent,physobj1,physobj2),
         At(physobj1,location),
         time).

[agent,physobj1,physobj2,time]
Happens(TakeOffOf(agent,physobj1,physobj2),time) ->
HoldsAt(On(physobj1,physobj2),time) &
{location}
 HoldsAt(At(agent,location),time) &
 HoldsAt(At(physobj1,location),time) &
 HoldsAt(At(physobj2,location),time).

[agent,physobj1,physobj2,location,time]
Releases(PlaceOn(agent,physobj1,physobj2),
         At(physobj1,location),
         time).

[physobj1,physobj2,location,time]
HoldsAt(On(physobj1,physobj2),time) &
HoldsAt(At(physobj2,location),time) ->
HoldsAt(At(physobj1,location),time).

fluent At(object,location)

[object,time]
{location} HoldsAt(At(object,location),time).

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
HoldsAt(At(object,location2),time) ->
location1=location2.

function Side1(portal): location
function Side2(portal): location

fluent NearPortal(object,portal)
noninertial NearPortal

[object,portal,time]
HoldsAt(NearPortal(object,portal),time) <->
{location}
 (Side1(portal)=location|
  Side2(portal)=location) &
 HoldsAt(At(object,location),time).

event WalkThroughDoor12(agent,door)
event WalkThroughDoor21(agent,door)

[agent,door,time]
Happens(WalkThroughDoor12(agent,door),time) ->
HoldsAt(Standing(agent),time) &
HoldsAt(At(agent,Side1(door)),time).

[agent,door,time]
Happens(WalkThroughDoor21(agent,door),time) ->
HoldsAt(Standing(agent),time) &
HoldsAt(At(agent,Side2(door)),time).

[agent,door,location,time]
Side2(door)=location ->
Initiates(WalkThroughDoor12(agent,door),At(agent,location),time).

[agent,door,location,time]
Side1(door)=location ->
Initiates(WalkThroughDoor21(agent,door),At(agent,location),time).

[agent,door,location,time]
Side1(door)=location ->
Terminates(WalkThroughDoor12(agent,door),At(agent,location),time).

[agent,door,location,time]
Side2(door)=location ->
Terminates(WalkThroughDoor21(agent,door),At(agent,location),time).

fluent Hungry(agent)

fluent Satiated(agent)
noninertial Satiated

[agent,time] HoldsAt(Hungry(agent),time) <-> !HoldsAt(Satiated(agent),time).

event Eat(agent,food)

[agent,food,time]
Happens(Eat(agent,food),time) ->
{location}
HoldsAt(At(agent,location),time) &
HoldsAt(At(food,location),time).

[agent,food,time]
Terminates(Eat(agent,food),Hungry(agent),time).

sort restaurant: script
sort waiter: agent
sort cook: agent

function BillOf(restaurant): bill
function CookOf(restaurant): cook
function TableOf(restaurant): table
function WaiterOf(restaurant): waiter
function KitchenDoorOf(restaurant): door

fluent BeWaiter0(waiter)

fluent BeWaiter1(waiter)

fluent BeWaiter2(waiter)

fluent BeWaiter3(waiter)

fluent BeWaiter4(waiter)

fluent BeWaiter5(waiter)

fluent BeWaiter6(waiter)

fluent BeWaiter7(waiter)

fluent BeWaiter8(waiter)

fluent BeWaiter9(waiter)

xor BeWaiter0, BeWaiter1, BeWaiter2, BeWaiter3, BeWaiter4, BeWaiter5, BeWaiter6, BeWaiter7, BeWaiter8, BeWaiter9

[waiter,agent,time]
HoldsAt(BeWaiter0(waiter),time) ->
Terminates(Greet(waiter,agent),
           BeWaiter0(waiter),
           time).

[waiter,agent,time]
HoldsAt(BeWaiter0(waiter),time) ->
Initiates(Greet(waiter,agent),
          BeWaiter1(waiter),
          time).

[waiter,agent,food,time]
HoldsAt(BeWaiter1(waiter),time) ->
Terminates(Order(agent,waiter,food),
           BeWaiter1(waiter),
           time).

[waiter,agent,food,time]
HoldsAt(BeWaiter1(waiter),time) ->
Initiates(Order(agent,waiter,food),
          BeWaiter2(waiter),
          time).

[restaurant,waiter,time]
WaiterOf(restaurant)=waiter &
HoldsAt(BeWaiter2(waiter),time) ->
Happens(WalkThroughDoor12(waiter,KitchenDoorOf(restaurant)),time).

[restaurant,waiter,door,time]
HoldsAt(BeWaiter2(waiter),time) &
WaiterOf(restaurant)=waiter &
KitchenDoorOf(restaurant)=door ->
Terminates(WalkThroughDoor12(waiter,door),
           BeWaiter2(waiter),
           time).

[restaurant,waiter,door,time]
HoldsAt(BeWaiter2(waiter),time) &
WaiterOf(restaurant)=waiter &
KitchenDoorOf(restaurant)=door ->
Initiates(WalkThroughDoor12(waiter,door),
          BeWaiter3(waiter),
          time).

[restaurant,food,time]
HoldsAt(BeWaiter3(WaiterOf(restaurant)),time) &
({agent} HoldsAt(KnowOrder(WaiterOf(restaurant),agent,food),time)) ->
Happens(Order(WaiterOf(restaurant),CookOf(restaurant),food),time).

[restaurant,waiter,cook,food,time]
WaiterOf(restaurant)=waiter &
CookOf(restaurant)=cook &
HoldsAt(BeWaiter3(waiter),time) ->
Terminates(Order(waiter,cook,food),
           BeWaiter3(waiter),
           time).

[restaurant,waiter,cook,food,time]
WaiterOf(restaurant)=waiter &
CookOf(restaurant)=cook &
HoldsAt(BeWaiter3(waiter),time) ->
Initiates(Order(waiter,cook,food),
          BeWaiter4(waiter),
          time).

[waiter,food,time]
HoldsAt(BeWaiter4(waiter),time) &
({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) &
HoldsAt(FoodPrepared(food),time) ->
Happens(PickUp(waiter,food),time).

[waiter,food,time]
HoldsAt(BeWaiter4(waiter),time) &
({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) ->
Terminates(PickUp(waiter,food),
           BeWaiter4(waiter),
           time).

[waiter,food,time]
HoldsAt(BeWaiter4(waiter),time) &
({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) ->
Initiates(PickUp(waiter,food),
          BeWaiter5(waiter),
          time).

[restaurant,waiter,time]
WaiterOf(restaurant)=waiter &
HoldsAt(BeWaiter5(waiter),time) ->
Happens(WalkThroughDoor21(waiter,KitchenDoorOf(restaurant)),time).

[restaurant,waiter,door,time]
HoldsAt(BeWaiter5(waiter),time) &
WaiterOf(restaurant)=waiter &
KitchenDoorOf(restaurant)=door ->
Terminates(WalkThroughDoor21(waiter,door),
           BeWaiter5(waiter),
           time).

[restaurant,waiter,door,time]
HoldsAt(BeWaiter5(waiter),time) &
WaiterOf(restaurant)=waiter &
KitchenDoorOf(restaurant)=door ->
Initiates(WalkThroughDoor21(waiter,door),
          BeWaiter6(waiter),
          time).

[restaurant,waiter,table,food,time]
WaiterOf(restaurant)=waiter &
TableOf(restaurant)=table &
HoldsAt(BeWaiter6(waiter),time) &
HoldsAt(Holding(waiter,food),time) ->
Happens(PlaceOn(waiter,food,table),time).

[waiter,food,table,time]
HoldsAt(BeWaiter6(waiter),time) ->
Terminates(PlaceOn(waiter,food,table),
           BeWaiter6(waiter),
           time).

[waiter,food,table,time]
HoldsAt(BeWaiter6(waiter),time) ->
Initiates(PlaceOn(waiter,food,table),
          BeWaiter7(waiter),
          time).

[waiter,agent,bill,time]
HoldsAt(BeWaiter7(waiter),time) ->
Terminates(Request(agent,waiter,bill),
           BeWaiter7(waiter),
           time).

[waiter,agent,bill,time]
HoldsAt(BeWaiter7(waiter),time) ->
Initiates(Request(agent,waiter,bill),
          BeWaiter8(waiter),
          time).

[restaurant,waiter,bill,time]
WaiterOf(restaurant)=waiter &
BillOf(restaurant)=bill &
HoldsAt(BeWaiter8(waiter),time) ->
Happens(PickUp(waiter,bill),time).

[waiter,bill,time]
HoldsAt(BeWaiter8(waiter),time) ->
Terminates(PickUp(waiter,bill),
           BeWaiter8(waiter),
           time).

[waiter,bill,time]
HoldsAt(BeWaiter8(waiter),time) ->
Initiates(PickUp(waiter,bill),
          BeWaiter9(waiter),
          time).

[restaurant,waiter,bill,table,time]
WaiterOf(restaurant)=waiter &
BillOf(restaurant)=bill &
TableOf(restaurant)=table &
HoldsAt(BeWaiter9(waiter),time) ->
Happens(PlaceOn(waiter,bill,table),time).

[waiter,bill,table,time]
HoldsAt(BeWaiter9(waiter),time) ->
Terminates(PlaceOn(waiter,bill,table),
           BeWaiter9(waiter),
           time).

[waiter,bill,table,time]
HoldsAt(BeWaiter9(waiter),time) ->
Initiates(PlaceOn(waiter,bill,table),
          BeWaiter0(waiter),
          time).

fluent BeCook0(cook)

fluent BeCook1(cook)

xor BeCook0, BeCook1

[cook,agent,food,time]
HoldsAt(BeCook0(cook),time) ->
Terminates(Order(agent,cook,food),
           BeCook0(cook),
           time).

[cook,agent,food,time]
HoldsAt(BeCook0(cook),time) ->
Initiates(Order(agent,cook,food),
          BeCook1(cook),
          time).

event FoodPrepare(agent,food)

fluent FoodPrepared(food)

[agent,food,time]
Initiates(FoodPrepare(agent,food),
          FoodPrepared(food),
          time).

[agent,food,time]
Happens(FoodPrepare(agent,food),time) ->
{location}
HoldsAt(At(agent,location),time) &
HoldsAt(At(food,location),time).

[cook,agent,food,time]
HoldsAt(BeCook1(cook),time) &
HoldsAt(KnowOrder(cook,agent,food),time) ->
Happens(FoodPrepare(cook,food),time).

[cook,food,time]
HoldsAt(BeCook1(cook),time) ->
Terminates(FoodPrepare(cook,food),
           BeCook1(cook),
           time).

[cook,food,time]
HoldsAt(BeCook1(cook),time) ->
Initiates(FoodPrepare(cook,food),
          BeCook0(cook),
          time).

event Pay(agent,agent)

event Tip(agent,agent)

[agent,physobj,time]
Happens(LieOn(agent,physobj),time) ->
{room}
 HoldsAt(At(agent,room),time) &
 HoldsAt(At(physobj,room),time).

[agent,physobj,time]
Happens(SitOn(agent,physobj),time) ->
{room}
 HoldsAt(At(agent,room),time) &
 HoldsAt(At(physobj,room),time).

event LieOn(agent,physobj)

event SitOn(agent,physobj)

event RiseFrom(agent,physobj)

fluent LyingOn(agent,physobj)
fluent SittingOn(agent,physobj)
fluent Standing(agent)

fluent Lying(agent)
fluent Sitting(agent)
noninertial Lying
noninertial Sitting

xor Lying, Sitting, Standing

[agent,physobj,time]
HoldsAt(LyingOn(agent,physobj),time) ->
HoldsAt(Lying(agent),time).

[agent,physobj,time]
HoldsAt(SittingOn(agent,physobj),time) ->
HoldsAt(Sitting(agent),time).

[agent,physobj1,physobj2,time]
HoldsAt(LyingOn(agent,physobj1),time) &
HoldsAt(LyingOn(agent,physobj2),time) ->
physobj1=physobj2.

[agent,physobj1,physobj2,time]
HoldsAt(SittingOn(agent,physobj1),time) &
HoldsAt(SittingOn(agent,physobj2),time) ->
physobj1=physobj2.

[agent,physobj,time]
HoldsAt(Standing(agent),time) ->
Initiates(LieOn(agent,physobj),
          LyingOn(agent,physobj),
          time).

[agent,physobj,time]
Terminates(LieOn(agent,physobj),
           Standing(agent),
           time).

[agent,physobj,time]
HoldsAt(Standing(agent),time) ->
Initiates(SitOn(agent,physobj),
          SittingOn(agent,physobj),
          time).

[agent,physobj,time]
Terminates(SitOn(agent,physobj),
           Standing(agent),
           time).

[agent,physobj,time]
(HoldsAt(SittingOn(agent,physobj),time) |
 HoldsAt(LyingOn(agent,physobj),time)) ->
Initiates(RiseFrom(agent,physobj),
          Standing(agent),
          time).

[agent,physobj,time]
HoldsAt(LyingOn(agent,physobj),time) ->
Terminates(RiseFrom(agent,physobj),
           LyingOn(agent,physobj),
           time).

[agent,physobj,time]
HoldsAt(SittingOn(agent,physobj),time) ->
Terminates(RiseFrom(agent,physobj),
           SittingOn(agent,physobj),
           time).

event Greet(agent,agent)

event SayGoodbye(agent,agent)

[agent1,agent2,time]
Happens(Greet(agent1,agent2),time) ->
{location}
HoldsAt(At(agent1,location),time) &
HoldsAt(At(agent2,location),time).

[agent1,agent2,time]
Happens(SayGoodbye(agent1,agent2),time) ->
{location}
HoldsAt(At(agent1,location),time) &
HoldsAt(At(agent2,location),time).

event Order(agent,agent,physobj)

fluent KnowOrder(agent,agent,physobj)

[agent1,agent2,physobj,time]
Initiates(Order(agent1,agent2,physobj),
          KnowOrder(agent2,agent1,physobj),
          time).

[agent1,agent2,physobj,time]
Happens(Order(agent1,agent2,physobj),time) ->
{location}
HoldsAt(At(agent1,location),time) &
HoldsAt(At(agent2,location),time).

event Request(agent,agent,physobj)

fluent KnowRequest(agent,agent,physobj)

[agent1,agent2,physobj,time]
Initiates(Request(agent1,agent2,physobj),
          KnowRequest(agent2,agent1,physobj),
          time).

[agent1,agent2,physobj,time]
Happens(Request(agent1,agent2,physobj),time) ->
{location}
HoldsAt(At(agent1,location),time) &
HoldsAt(At(agent2,location),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/TakingATrain.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Mueller:2004c,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Understanding script-based stories using commonsense reasoning",
;   journal = "Cognitive Systems Research",
;   volume = "5",
;   number = "4",
;   pages = "307--340",
; }
;

option modeldiff on
option renaming off

ignore Love, ThreatenedBy
ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
ignore Inside, PutInside, On, PlaceOn
ignore Like, Happy, BecomeAngryAt
ignore Near, WalkFrom, WalkFromTo, RunFromTo
ignore ActOnIntentionToWalkIn, IntentionToWalkIn, InvitedIn, InviteIn
ignore TakeOffOf, TakeOutOf, LetGoOf
ignore Greet, SayGoodbye, Order, KnowOrder

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/HandTo.e
load answers/Mueller2004c/Money.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2003/SpeechAct.e
load answers/Mueller2004c/Vehicle.e

outside NearStation1

street Street1

outside Station1

Side1(Street1)=NearStation1.
Side2(Street1)=Station1.

room WaitingRoom1

door Door1

Side1(Door1)=Station1.
Side2(Door1)=WaitingRoom1.

outside BeforeStation1

track TrackToStation1

Side1(TrackToStation1)=BeforeStation1.
Side2(TrackToStation1)=Station1.

outside BetweenStation1And2

track TrackToBetweenStation1And2

Side1(TrackToBetweenStation1And2)=Station1.
Side2(TrackToBetweenStation1And2)=BetweenStation1And2.

outside Station2
track TrackToStation2

Side1(TrackToStation2)=BetweenStation1And2.
Side2(TrackToStation2)=Station2.

outside NearStation2

street Street2

Side1(Street2)=Station2.
Side2(Street2)=NearStation2.

agent Passenger1

agent Conductor1

chair TrainSeat1

train Train1

chair WaitingRoomSeat1

ticketagent TicketAgent1

ticket Ticket1

; prune
sort requesta, requestb, requestc
event! Request(requesta,requestb,requestc)
fluent! KnowRequest(requestb,requesta,requestc)
requesta! Passenger1
requestb! TicketAgent1
requestc! Ticket1

sort holda, holdb
event! TakeOffOf(holda,holdb,holdc)
event! PickUp(holda,holdb)
event! LetGoOf(holda,holdb)
event! Hold(holda,holdb)
fluent! Holding(holda,holdb)
event! HandTo(holda,holda,holdb)
holda! TicketAgent1, Passenger1, Conductor1
holdb! Ticket1

sort sita, sitb
event! LieOn(sita,sitb)
event! SitOn(sita,sitb)
event! RiseFrom(sita,sitb)
fluent! LyingOn(sita,sitb)
fluent! SittingOn(sita,sitb)
sita! Passenger1
sitb! WaitingRoomSeat1, TrainSeat1

; release
option manualrelease on
[ball, diameter] !ReleasedAt(Diameter(ball, diameter), 0).
[agent, object] !ReleasedAt(Holding(agent, object), 0).
[door] !ReleasedAt(DoorUnlocked(door), 0).
[door] !ReleasedAt(DoorIsOpen(door), 0).
[agent] !ReleasedAt(Sleep0(agent), 0).
[agent] !ReleasedAt(Sleep1(agent), 0).
[agent] !ReleasedAt(Sleep2(agent), 0).
[agent] !ReleasedAt(Sleep3(agent), 0).
[agent] !ReleasedAt(Sleep4(agent), 0).
[agent] !ReleasedAt(Sleep5(agent), 0).
[agent] !ReleasedAt(Sleep6(agent), 0).
[agent, physobj] !ReleasedAt(LyingOn(agent, physobj), 0).
[agent, physobj] !ReleasedAt(SittingOn(agent, physobj), 0).
[agent] !ReleasedAt(Standing(agent), 0).
[agent] !ReleasedAt(Dressed(agent), 0).
[agent1, agent2, physobj] !ReleasedAt(KnowOrder(agent1, agent2, physobj), 0).
[agent1, agent2, physobj] !ReleasedAt(KnowRequest(agent1, agent2, physobj), 0).
[object, vehicleon] !ReleasedAt(OnVehicle(object, vehicleon), 0).
[ticketagent] !ReleasedAt(BeTicketAgent0(ticketagent), 0).
[ticketagent] !ReleasedAt(BeTicketAgent1(ticketagent), 0).
[ticketagent] !ReleasedAt(BeTicketAgent2(ticketagent), 0).
; special cases
[location] ReleasedAt(At(TrainSeat1,location),0).
[location] ReleasedAt(At(Conductor1,location),0).
[object, location]
object!=TrainSeat1 & object!=Conductor1 ->
!ReleasedAt(At(object, location), 0).

; initial state
[agent,object] !HoldsAt(Holding(agent,object),0).
[agent,physobj] !HoldsAt(SittingOn(agent,physobj),0).
[agent,physobj] !HoldsAt(LyingOn(agent,physobj),0).
[physobj1,physobj2]
!HoldsAt(On(physobj1, physobj2),0).
[agent] HoldsAt(Dressed(agent),0).
[agent] HoldsAt(Awake(agent),0).
[agent] HoldsAt(Sleep3(agent),0).
[agent] HoldsAt(Standing(agent),0).
HoldsAt(DoorUnlocked(Door1),0).
HoldsAt(DoorIsOpen(Door1),0).
HoldsAt(At(WaitingRoomSeat1,WaitingRoom1),0).
HoldsAt(At(Passenger1,NearStation1),0).
HoldsAt(At(Train1,BeforeStation1),0).
HoldsAt(OnVehicle(TrainSeat1,Train1),0).
HoldsAt(OnVehicle(Conductor1,Train1),0).
[object]
object!=TrainSeat1 & object!=Conductor1 ->
!HoldsAt(OnVehicle(object,Train1),0).
HoldsAt(At(Ticket1,WaitingRoom1),0).
HoldsAt(At(TicketAgent1,WaitingRoom1),0).
HoldsAt(BeTicketAgent0(TicketAgent1),0).
[agent1,agent2,physobj] !HoldsAt(KnowOrder(agent1,agent2,physobj),0).
[agent1,agent2,physobj] !HoldsAt(KnowRequest(agent1,agent2,physobj),0).

; narrative
Happens(WalkStreet12(Passenger1,Street1),0).
Happens(WalkThroughDoor12(Passenger1,Door1),1).
Happens(Request(Passenger1,TicketAgent1,Ticket1),2).
Happens(Pay(Passenger1,TicketAgent1),3).
;TRIGGERED Happens(PickUp(TicketAgent1,Ticket1),3).
;TRIGGERED Happens(HandTo(TicketAgent1,Passenger1,Ticket1),4).
Happens(SitOn(Passenger1,WaitingRoomSeat1),5).
Happens(RideTrack12(Train1,TrackToStation1),6).
Happens(RiseFrom(Passenger1,WaitingRoomSeat1),7).
Happens(WalkThroughDoor21(Passenger1,Door1),8).
Happens(GetOnVehicle(Passenger1,Train1),9).
Happens(SitOn(Passenger1,TrainSeat1),10).
Happens(RideTrack12(Train1,TrackToBetweenStation1And2),11).
Happens(HandTo(Passenger1,Conductor1,Ticket1),12).
;OR PAY
Happens(RideTrack12(Train1,TrackToStation2),13).
Happens(RiseFrom(Passenger1,TrainSeat1),14).
Happens(GetOffVehicle(Passenger1,Train1),15).
Happens(WalkStreet12(Passenger1,Street2),16).

range time 0 17
range offset 0 0
range diameter 0 0

completion Happens

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/MakingAnAcquaintance.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @article{Mueller:2004c,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Understanding script-based stories using commonsense reasoning",
;   journal = "Cognitive Systems Research",
;   volume = "5",
;   number = "4",
;   pages = "307--340",
; }
;

option modeldiff on

ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
ignore Near, WalkFrom, WalkFromTo, RunFromTo
ignore Side1, Side2
ignore FriendOf, NeutralOf, EnemyOf,
ignore BecomeFriends, BecomeNeutral, BecomeEnemies
ignore Happy, Calm, Unhappy
ignore BecomeHappy, BecomeCalm, BecomeUnhappy
ignore AngryAt, BecomeAngryAt
ignore Like, Love, Dislike, LikeSnow
ignore HandTo
ignore InviteIn, InvitedIn, IntendToWalkIn, IntentionToWalkIn
ignore ActOnIntentionToWalkIn, Greet, SayGoodbye, CryForJoy
ignore Threaten, ReleaseFromThreat, ThreatenedBy
ignore Order, KnowOrder, Request, KnowRequest

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2003/Feeling.e
load answers/Mueller2004c/HandTo.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2003/SpeechAct.e
load answers/Mueller2004c/IPRel.e

location Location1

portal DummyPortal1

agent Introducer1

agent Introducee1

agent Introduced1

; initial state
[agent,physobj] !HoldsAt(SittingOn(agent,physobj),0).
[agent,physobj] !HoldsAt(LyingOn(agent,physobj),0).
[agent] HoldsAt(Dressed(agent),0).
[agent] HoldsAt(Awake(agent),0).
[agent] HoldsAt(Sleep3(agent),0).
[agent] HoldsAt(Standing(agent),0).
HoldsAt(AcquaintanceOf(Introducer1,Introducee1),0).
HoldsAt(AcquaintanceOf(Introducer1,Introduced1),0).
!HoldsAt(AcquaintanceOf(Introducee1,Introduced1),0).

; narrative
;Happens(IntroduceMutual(Introducer1,Introducee1,Introduced1),0).
Happens(Introduce(Introducee1,Introduced1),0).
;Happens(Introduce(Introduced1,Introducee1),0).
;Happens(Smile(Introducer1),1).
Happens(Smile(Introducee1),1).
Happens(Smile(Introduced1),2).
Happens(SayPleasedToMeet(Introducee1,Introduced1),3).
Happens(SayPleasedToMeet(Introduced1,Introducee1),4).
Happens(ShakeHands(Introducee1,Introduced1),5).

range time 0 6
range offset 0 0
range diameter 0 0

completion Happens

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Arson.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; DEV-MUC3-0060
; Arson
;
; @article{Mueller:2004c,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Understanding script-based stories using commonsense reasoning",
;   journal = "Cognitive Systems Research",
;   volume = "5",
;   number = "4",
;   pages = "307--340",
; }
;

ignore SkyOf, GroundOf, Near, Inside, WalkFromTo, RunFromTo
ignore RollAlong, Diameter, Move, HoldSome, PutInside
ignore TakeOutOf, On, DoorUnlock, DoorLock, WalkThroughDoor12
ignore WalkThroughDoor21, WalkDownStaircase, WalkUpStaircase

ignore ThreatenedBy

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/Fire.e
load answers/Mueller2004c/Condition.e
load answers/Mueller2003/Sleep.e

fire Fire1
HoldsAt(At(Fire1,Outside1),0).

agent Perp1

physobj PhysTarget1
HoldsAt(Intact(PhysTarget1),0).
HoldsAt(At(PhysTarget1,Outside1),0).

; room-scale topological space
outside Outside1
outside Outside2
room Inside1
door Door1
building Building1
street Street1
Side1(Door1)=Outside1.
Side2(Door1)=Inside1.
LookOutOnto(Inside1)=Outside1.
Floor(Inside1)=1.
BuildingOf(Inside1)=Building1.
Side1(Street1)=Outside1.
Side2(Street1)=Outside2.

HoldsAt(Alive(Perp1),0).
HoldsAt(Awake(Perp1),0).
HoldsAt(Standing(Perp1),0).
HoldsAt(Sleep2(Perp1),0).
!HoldsAt(Injured(Perp1),0).
[object] !HoldsAt(Holding(Perp1,object),0).
HoldsAt(At(Perp1,Outside2),0).
!HoldsAt(DoorIsOpen(Door1),0).
HoldsAt(DoorUnlocked(Door1),0).
[physobj,fire,offset] !HoldsAt(Burning(physobj,fire,offset),0).

; narrative
Happens(WalkStreet21(Perp1,Street1),0).
Happens(SetFireTo(Perp1,PhysTarget1,Fire1,3),1).
Happens(WalkStreet12(Perp1,Street1),2).

range time 0 6
range offset 0 3
range diameter 0 0

completion Happens

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Fire.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; fire
;

; agent sets fire to physobj with burn time offset.
event SetFireTo(agent,physobj,fire,offset)

; An effect axioms states that
; if an agent sets a fire to a physical object with a burn time,
; the physical object will be burning with the fire and burn time:
[agent,physobj,fire,offset,time]
Initiates(SetFireTo(agent,physobj,fire,offset),
          Burning(physobj,fire,offset),
          time).

; agent puts out fire on physobj.
event PutOutFire(agent,physobj,fire)

; An effect axiom states that
; if an agent puts out a fire on a physical object,
; the physical object will no longer be burning:
[agent,physobj,fire,offset,time]
Terminates(PutOutFire(agent,physobj,fire),
           Burning(physobj,fire,offset),
           time).

; A precondition axiom states that
; for an agent to set fire to a physical object,
; there must be a location such that
; the agent is at the location and
; the physical object is at the location:
[agent,fire,physobj,offset,time]
Happens(SetFireTo(agent,physobj,fire,offset),time) ->
{location} HoldsAt(At(agent,location),time) &
           HoldsAt(At(physobj,location),time).

; A precondition axiom states that
; for an agent to put out a fire on a physical object,
; there must be a location such that
; the agent is at the location and
; the physical object is at the location:
[agent,fire,physobj,time]
Happens(PutOutFire(agent,physobj,fire),time) ->
{location} HoldsAt(At(agent,location),time) &
           HoldsAt(At(physobj,location),time).

; physobj is burning with fire and burn time offset.
fluent Burning(physobj,fire,offset)

; A state constraint says that a physical object burning with
; a fire has at most one burn time at a time:
[physobj,fire,offset1,offset2,time]
HoldsAt(Burning(physobj,fire,offset1),time) &
HoldsAt(Burning(physobj,fire,offset2),time) ->
offset1=offset2.

; The burn time of physobj is decremented.
event DecrementBurning(physobj)

; A trigger axiom states that
; if a physical object is burning with a fire and a burn time and
; the burn time is greater than zero,
; the burn time of the physical object is decremented:
[physobj,fire,offset,time]
HoldsAt(Burning(physobj,fire,offset),time) &
(offset > 0) ->
Happens(DecrementBurning(physobj),time).

; An effect axiom states that if a physical object is
; burning with a fire and a burn time, and the burn time of a physical
; object is decremented, the burn time of the physical
; object will be the burn time minus one:
[physobj,fire,offset1,offset2,time]
HoldsAt(Burning(physobj,fire,offset1),time) &
offset2 = offset1-1 ->
Initiates(DecrementBurning(physobj),
          Burning(physobj,fire,offset2),
          time).

; An effect axiom states that if a physical object is
; burning with a fire and a burn time, and the burn time of a physical
; object is decremented, the burn time of the physical
; object will no longer be the burn time:
[physobj,fire,offset,time]
HoldsAt(Burning(physobj,fire,offset),time) ->
Terminates(DecrementBurning(physobj),
           Burning(physobj,fire,offset),
           time).

; A trigger axiom states that
; if a physical object is burning with a fire and a burn time
; that is not equal to zero, the fire will damage the
; physical object:
[physobj,fire,offset,time]
offset!=0 &
HoldsAt(Burning(physobj,fire,offset),time) &
HoldsAt(Intact(physobj),time) ->
Happens(Damage(fire,physobj),time).

; A trigger axiom states that
; if a physical object is burning with a fire and a burn time
; that is equal to zero, the fire will destroy the
; physical object:
[physobj,fire,time]
HoldsAt(Burning(physobj,fire,0),time) &
!HoldsAt(Destroyed(physobj),time) ->
Happens(Destroy(fire,physobj),time).

; An effect axiom states that if a fire destroys a physical
; object, the physical object will no longer be burning:
[physobj,fire,offset,time]
Terminates(Destroy(fire,physobj),
           Burning(physobj,fire,offset),
           time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Restaurant.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
sort restaurant: script
sort waiter: agent
sort cook: agent

function BillOf(restaurant): bill
function CookOf(restaurant): cook
function TableOf(restaurant): table
function WaiterOf(restaurant): waiter
function KitchenDoorOf(restaurant): door

; awaiting customer/waiter has set down bill on customer's table
fluent BeWaiter0(waiter)

; awaiting customer order
fluent BeWaiter1(waiter)

; has customer order
fluent BeWaiter2(waiter)

; in kitchen
fluent BeWaiter3(waiter)

; awaiting preparation of order
fluent BeWaiter4(waiter)

; has order
fluent BeWaiter5(waiter)

; back in dining room
fluent BeWaiter6(waiter)

; order delivered to customer (can ask if all is OK)
fluent BeWaiter7(waiter)

; customer has requested bill
fluent BeWaiter8(waiter)

; waiter is holding bill
fluent BeWaiter9(waiter)

xor BeWaiter0, BeWaiter1, BeWaiter2, BeWaiter3, BeWaiter4, BeWaiter5, BeWaiter6, BeWaiter7, BeWaiter8, BeWaiter9

[waiter,agent,time]
HoldsAt(BeWaiter0(waiter),time) ->
Terminates(Greet(waiter,agent),
           BeWaiter0(waiter),
           time).

[waiter,agent,time]
HoldsAt(BeWaiter0(waiter),time) ->
Initiates(Greet(waiter,agent),
          BeWaiter1(waiter),
          time).

[waiter,agent,food,time]
HoldsAt(BeWaiter1(waiter),time) ->
Terminates(Order(agent,waiter,food),
           BeWaiter1(waiter),
           time).

[waiter,agent,food,time]
HoldsAt(BeWaiter1(waiter),time) ->
Initiates(Order(agent,waiter,food),
          BeWaiter2(waiter),
          time).

[restaurant,waiter,time]
WaiterOf(restaurant)=waiter &
HoldsAt(BeWaiter2(waiter),time) ->
Happens(WalkThroughDoor12(waiter,KitchenDoorOf(restaurant)),time).

[restaurant,waiter,door,time]
HoldsAt(BeWaiter2(waiter),time) &
WaiterOf(restaurant)=waiter &
KitchenDoorOf(restaurant)=door ->
Terminates(WalkThroughDoor12(waiter,door),
           BeWaiter2(waiter),
           time).

[restaurant,waiter,door,time]
HoldsAt(BeWaiter2(waiter),time) &
WaiterOf(restaurant)=waiter &
KitchenDoorOf(restaurant)=door ->
Initiates(WalkThroughDoor12(waiter,door),
          BeWaiter3(waiter),
          time).

[restaurant,food,time]
HoldsAt(BeWaiter3(WaiterOf(restaurant)),time) &
({agent} HoldsAt(KnowOrder(WaiterOf(restaurant),agent,food),time)) ->
Happens(Order(WaiterOf(restaurant),CookOf(restaurant),food),time).

[restaurant,waiter,cook,food,time]
WaiterOf(restaurant)=waiter &
CookOf(restaurant)=cook &
HoldsAt(BeWaiter3(waiter),time) ->
Terminates(Order(waiter,cook,food),
           BeWaiter3(waiter),
           time).

[restaurant,waiter,cook,food,time]
WaiterOf(restaurant)=waiter &
CookOf(restaurant)=cook &
HoldsAt(BeWaiter3(waiter),time) ->
Initiates(Order(waiter,cook,food),
          BeWaiter4(waiter),
          time).

[waiter,food,time]
HoldsAt(BeWaiter4(waiter),time) &
({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) &
HoldsAt(FoodPrepared(food),time) ->
Happens(PickUp(waiter,food),time).

[waiter,food,time]
HoldsAt(BeWaiter4(waiter),time) &
({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) ->
Terminates(PickUp(waiter,food),
           BeWaiter4(waiter),
           time).

[waiter,food,time]
HoldsAt(BeWaiter4(waiter),time) &
({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) ->
Initiates(PickUp(waiter,food),
          BeWaiter5(waiter),
          time).

[restaurant,waiter,time]
WaiterOf(restaurant)=waiter &
HoldsAt(BeWaiter5(waiter),time) ->
Happens(WalkThroughDoor21(waiter,KitchenDoorOf(restaurant)),time).

[restaurant,waiter,door,time]
HoldsAt(BeWaiter5(waiter),time) &
WaiterOf(restaurant)=waiter &
KitchenDoorOf(restaurant)=door ->
Terminates(WalkThroughDoor21(waiter,door),
           BeWaiter5(waiter),
           time).

[restaurant,waiter,door,time]
HoldsAt(BeWaiter5(waiter),time) &
WaiterOf(restaurant)=waiter &
KitchenDoorOf(restaurant)=door ->
Initiates(WalkThroughDoor21(waiter,door),
          BeWaiter6(waiter),
          time).

[restaurant,waiter,table,food,time]
WaiterOf(restaurant)=waiter &
TableOf(restaurant)=table &
HoldsAt(BeWaiter6(waiter),time) &
HoldsAt(Holding(waiter,food),time) ->
Happens(PlaceOn(waiter,food,table),time).

[waiter,food,table,time]
HoldsAt(BeWaiter6(waiter),time) ->
Terminates(PlaceOn(waiter,food,table),
           BeWaiter6(waiter),
           time).

[waiter,food,table,time]
HoldsAt(BeWaiter6(waiter),time) ->
Initiates(PlaceOn(waiter,food,table),
          BeWaiter7(waiter),
          time).

[waiter,agent,bill,time]
HoldsAt(BeWaiter7(waiter),time) ->
Terminates(Request(agent,waiter,bill),
           BeWaiter7(waiter),
           time).

[waiter,agent,bill,time]
HoldsAt(BeWaiter7(waiter),time) ->
Initiates(Request(agent,waiter,bill),
          BeWaiter8(waiter),
          time).

[restaurant,waiter,bill,time]
WaiterOf(restaurant)=waiter &
BillOf(restaurant)=bill &
HoldsAt(BeWaiter8(waiter),time) ->
Happens(PickUp(waiter,bill),time).

[waiter,bill,time]
HoldsAt(BeWaiter8(waiter),time) ->
Terminates(PickUp(waiter,bill),
           BeWaiter8(waiter),
           time).

[waiter,bill,time]
HoldsAt(BeWaiter8(waiter),time) ->
Initiates(PickUp(waiter,bill),
          BeWaiter9(waiter),
          time).

[restaurant,waiter,bill,table,time]
WaiterOf(restaurant)=waiter &
BillOf(restaurant)=bill &
TableOf(restaurant)=table &
HoldsAt(BeWaiter9(waiter),time) ->
Happens(PlaceOn(waiter,bill,table),time).

[waiter,bill,table,time]
HoldsAt(BeWaiter9(waiter),time) ->
Terminates(PlaceOn(waiter,bill,table),
           BeWaiter9(waiter),
           time).

[waiter,bill,table,time]
HoldsAt(BeWaiter9(waiter),time) ->
Initiates(PlaceOn(waiter,bill,table),
          BeWaiter0(waiter),
          time).

; awaiting next waiter order
fluent BeCook0(cook)

; waiter order received
fluent BeCook1(cook)

xor BeCook0, BeCook1

[cook,agent,food,time]
HoldsAt(BeCook0(cook),time) ->
Terminates(Order(agent,cook,food),
           BeCook0(cook),
           time).

[cook,agent,food,time]
HoldsAt(BeCook0(cook),time) ->
Initiates(Order(agent,cook,food),
          BeCook1(cook),
          time).

event FoodPrepare(agent,food)

fluent FoodPrepared(food)

[agent,food,time]
Initiates(FoodPrepare(agent,food),
          FoodPrepared(food),
          time).

[agent,food,time]
Happens(FoodPrepare(agent,food),time) ->
{location}
HoldsAt(At(agent,location),time) &
HoldsAt(At(food,location),time).

[cook,agent,food,time]
HoldsAt(BeCook1(cook),time) &
HoldsAt(KnowOrder(cook,agent,food),time) ->
Happens(FoodPrepare(cook,food),time).

[cook,food,time]
HoldsAt(BeCook1(cook),time) ->
Terminates(FoodPrepare(cook,food),
           BeCook1(cook),
           time).

[cook,food,time]
HoldsAt(BeCook1(cook),time) ->
Initiates(FoodPrepare(cook,food),
          BeCook0(cook),
          time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Diving.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; scuba diving
;

sort object
sort agent: object
sort diver: agent
sort depth: integer
sort boat: object

; reference line, anchor line, shotline, SMB line, ...
sort line: object

sort equipment: object
sort weight: equipment
sort fin: equipment
sort airtank: equipment

; buoyancy compensator (BC)
; buoyancy control device (BCD)
sort computer: equipment
sort bc: equipment

fluent AtDepth(object,depth)

[object,depth1,depth2,time]
HoldsAt(AtDepth(object,depth1),time) &
HoldsAt(AtDepth(object,depth2),time) ->
depth1 = depth2.

event Ascend(diver,depth)

event Descend(diver,depth)

[diver,depth1,depth2,time]
HoldsAt(AtDepth(diver,depth1),time) &
Happens(Descend(diver,depth2),time) ->
depth2>depth1.

[diver,depth1,depth2,time]
HoldsAt(AtDepth(diver,depth1),time) &
Happens(Ascend(diver,depth2),time) ->
depth2<depth1.

[diver,depth,time]
Initiates(Descend(diver,depth),AtDepth(diver,depth),time).

[diver,depth1,depth2,time]
HoldsAt(AtDepth(diver,depth1),time) ->
Terminates(Descend(diver,depth2),AtDepth(diver,depth1),time).

[diver,depth,time]
Initiates(Ascend(diver,depth),AtDepth(diver,depth),time).

[diver,depth1,depth2,time]
HoldsAt(AtDepth(diver,depth1),time) ->
Terminates(Ascend(diver,depth2),AtDepth(diver,depth1),time).

fluent Wearing(diver,equipment)

event PutOn(diver,equipment)

event TakeOff(diver,equipment)

event Lose(diver,equipment)

[diver,equipment,depth,time]
Releases(PutOn(diver,equipment),AtDepth(equipment,depth),time).

[diver,equipment,time]
Releases(PutOn(diver,equipment),UnderWater(equipment),time).

[diver,equipment,time]
Happens(PutOn(diver,equipment),time) ->
!{diver1} HoldsAt(Wearing(diver1,equipment),time).

[diver,depth,equipment,time]
HoldsAt(Wearing(diver,equipment),time) ->
(HoldsAt(AtDepth(diver,depth),time) <->
 HoldsAt(AtDepth(equipment,depth),time)).

[diver,depth,object,time]
HoldsAt(Holding(diver,object),time) ->
(HoldsAt(AtDepth(diver,depth),time) <->
 HoldsAt(AtDepth(object,depth),time)).

[diver,equipment,time]
HoldsAt(Wearing(diver,equipment),time) ->
(HoldsAt(UnderWater(diver),time) <->
 HoldsAt(UnderWater(equipment),time)).

[diver,object,time]
HoldsAt(Holding(diver,object),time) ->
(HoldsAt(UnderWater(diver),time) <->
 HoldsAt(UnderWater(object),time)).

[diver,depth,equipment,time]
HoldsAt(AtDepth(diver,depth),time) &
HoldsAt(Wearing(diver,equipment),time) ->
Initiates(TakeOff(diver,equipment),AtDepth(equipment,depth),time).

[diver,depth,equipment,time]
!HoldsAt(AtDepth(diver,depth),time) &
HoldsAt(Wearing(diver,equipment),time) ->
Terminates(TakeOff(diver,equipment),AtDepth(equipment,depth),time).

[diver,equipment,time]
HoldsAt(UnderWater(diver),time) ->
Initiates(TakeOff(diver,equipment),UnderWater(equipment),time).

[diver,equipment,time]
!HoldsAt(UnderWater(diver),time) ->
Terminates(TakeOff(diver,equipment),UnderWater(equipment),time).

[diver,equipment,depth,time]
HoldsAt(AtDepth(diver,depth),time) &
HoldsAt(Wearing(diver,equipment),time) ->
Initiates(Lose(diver,equipment),AtDepth(equipment,depth),time).

[diver,equipment,depth,time]
!HoldsAt(AtDepth(diver,depth),time) &
HoldsAt(Wearing(diver,equipment),time) ->
Terminates(Lose(diver,equipment),AtDepth(equipment,depth),time).

[diver,equipment,time]
HoldsAt(UnderWater(diver),time) ->
Initiates(Lose(diver,equipment),UnderWater(equipment),time).

[diver,equipment,time]
!HoldsAt(UnderWater(diver),time) ->
Terminates(Lose(diver,equipment),UnderWater(equipment),time).

fluent Holding(diver,object)

[diver1,diver2,time]
HoldsAt(Holding(diver1,diver2),time) ->
!HoldsAt(Holding(diver2,diver1),time).

event Grab(diver,object)

event LetGoOf(diver,object)

[diver,object,time]
Initiates(Grab(diver,object),Holding(diver,object),time).

[diver,object,time]
Terminates(LetGoOf(diver,object),Holding(diver,object),time).

[diver,object,depth,time]
Releases(Grab(diver,object),AtDepth(object,depth),time).

[diver,object,time]
Releases(Grab(diver,object),UnderWater(object),time).

[diver,object,depth,time]
HoldsAt(AtDepth(diver,depth),time) &
HoldsAt(Holding(diver,object),time) ->
Initiates(LetGoOf(diver,object),AtDepth(object,depth),time).

[diver,object,depth,time]
!HoldsAt(AtDepth(diver,depth),time) &
HoldsAt(Holding(diver,object),time) ->
Terminates(LetGoOf(diver,object),AtDepth(object,depth),time).

[diver,object,time]
HoldsAt(UnderWater(diver),time) ->
Initiates(LetGoOf(diver,object),UnderWater(object),time).

[diver,object,time]
!HoldsAt(UnderWater(diver),time) ->
Terminates(LetGoOf(diver,object),UnderWater(object),time).

[diver,equipment,time]
Initiates(PutOn(diver,equipment),Wearing(diver,equipment),time).

[diver,equipment,time]
Happens(PutOn(diver,equipment),time) ->
!HoldsAt(UnderWater(diver),time).

[diver,equipment,time]
Terminates(TakeOff(diver,equipment),Wearing(diver,equipment),time).

[diver,equipment,time]
Terminates(Lose(diver,equipment),Wearing(diver,equipment),time).

fluent Vertical(diver)

fluent HorizontalDown(diver)

fluent Inverted(diver)

fluent HorizontalUp(diver)

xor Vertical, HorizontalDown, Inverted, HorizontalUp

event RotatePitch(diver)

[diver,time]
HoldsAt(Vertical(diver),time) ->
Initiates(RotatePitch(diver),HorizontalDown(diver),time).

[diver,time]
HoldsAt(HorizontalDown(diver),time) ->
Initiates(RotatePitch(diver),Inverted(diver),time).

[diver,time]
HoldsAt(HorizontalDown(diver),time) ->
Terminates(RotatePitch(diver),HorizontalDown(diver),time).

[diver,time]
HoldsAt(Inverted(diver),time) ->
Initiates(RotatePitch(diver),HorizontalUp(diver),time).

[diver,time]
HoldsAt(Inverted(diver),time) ->
Terminates(RotatePitch(diver),Inverted(diver),time).

[diver,time]
HoldsAt(HorizontalUp(diver),time) ->
Initiates(RotatePitch(diver),Vertical(diver),time).

[diver,time]
HoldsAt(HorizontalUp(diver),time) ->
Terminates(RotatePitch(diver),HorizontalUp(diver),time).

event RotateYaw(diver)

; try taking out Holding condition here
[diver,time]
Happens(Ascend1(diver),time) &
!Happens(RapidAscendToSurface(diver),time) &
!({diver1} HoldsAt(Holding(diver,diver1),time)) ->
Happens(RotateYaw(diver),time).

fluent UnderWater(object)

[object,depth,time]
depth>0 &
HoldsAt(AtDepth(object,depth),time) ->
HoldsAt(UnderWater(object),time).

event EnterWater(object)

event Surface(object)

[object,time]
Initiates(EnterWater(object),UnderWater(object),time).

[diver,time]
Happens(EnterWater(diver),time) ->
!{diver1} HoldsAt(Holding(diver1,diver),time).

[object,depth,time]
depth=0 ->
Initiates(EnterWater(object),AtDepth(object,depth),time).

[object,time]
Terminates(Surface(object),UnderWater(object),time).

[diver,time]
Terminates(Surface(diver),PositivelyBuoyant(diver),time).

[diver,time]
Terminates(Surface(diver),NegativelyBuoyant(diver),time).

[diver,time]
Terminates(Surface(diver),NeutrallyBuoyant(diver),time).

[object,depth,time]
Terminates(Surface(object),AtDepth(object,depth),time).

[diver,time] Happens(EnterWater(diver),time) ->
HoldsAt(Vertical(diver),time).

fluent StandingOn(diver,boat)

event StandOn(diver,boat)

[diver,boat,time]
Terminates(EnterWater(diver),StandingOn(diver,boat),time).

[diver,boat,time]
Initiates(StandOn(diver,boat),StandingOn(diver,boat),time).

fluent PositivelyBuoyant(diver)

fluent NeutrallyBuoyant(diver)

fluent NegativelyBuoyant(diver)

mutex PositivelyBuoyant, NeutrallyBuoyant, NegativelyBuoyant

[diver,time]
HoldsAt(PositivelyBuoyant(diver),time) ->
HoldsAt(UnderWater(diver),time).

[diver,time]
HoldsAt(NeutrallyBuoyant(diver),time) ->
HoldsAt(UnderWater(diver),time).

[diver,time]
HoldsAt(NegativelyBuoyant(diver),time) ->
HoldsAt(UnderWater(diver),time).

event PressDeflateButton(diver,bc)

event PressDumpButton(diver,bc)

event PressInflateButton(diver,bc)

[diver,bc,time]
Happens(PressDeflateButton(diver,bc),time) ->
HoldsAt(Vertical(diver),time) &
HoldsAt(UnderWater(bc),time).

[diver,bc,time]
Happens(PressDumpButton(diver,bc),time) ->
HoldsAt(Vertical(diver),time) &
HoldsAt(UnderWater(bc),time).

[diver,bc,time] Happens(PressDumpButton(diver,bc),time) ->
HoldsAt(UncontrolledBuoyancy(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Initiates(PressDeflateButton(diver,bc),NegativelyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Terminates(PressDeflateButton(diver,bc),NeutrallyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Terminates(PressDeflateButton(diver,bc),PositivelyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Initiates(PressDumpButton(diver,bc),NegativelyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Terminates(PressDumpButton(diver,bc),NeutrallyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Terminates(PressDumpButton(diver,bc),PositivelyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Initiates(PressInflateButton(diver,bc),NeutrallyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Terminates(PressInflateButton(diver,bc),PositivelyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Terminates(PressInflateButton(diver,bc),NegativelyBuoyant(diver),time).

[diver,weight,time]
HoldsAt(Wearing(diver,weight),time) ->
Initiates(TakeOff(diver,weight),PositivelyBuoyant(diver),time).

[diver,weight,time]
HoldsAt(Wearing(diver,weight),time) ->
Terminates(TakeOff(diver,weight),NegativelyBuoyant(diver),time).

[diver,weight,time]
HoldsAt(Wearing(diver,weight),time) ->
Terminates(TakeOff(diver,weight),NeutrallyBuoyant(diver),time).

fluent UncontrolledBuoyancy(diver)

event LoseBuoyancyControl(diver)

predicate IsInexperiencedDiver(diver)

[diver,time]
Happens(LoseBuoyancyControl(diver),time) ->
IsInexperiencedDiver(diver).

[diver,time]
Initiates(LoseBuoyancyControl(diver),UncontrolledBuoyancy(diver),time).

[diver,time]
Initiates(LoseBuoyancyControl(diver),PositivelyBuoyant(diver),time).

[diver,time]
Terminates(LoseBuoyancyControl(diver),NegativelyBuoyant(diver),time).

[diver,time]
Terminates(LoseBuoyancyControl(diver),NeutrallyBuoyant(diver),time).

; determining fluent
fluent AscendDescendAmount(diver,depth)
noninertial AscendDescendAmount

[diver,depth1,depth2,time]
HoldsAt(AscendDescendAmount(diver,depth1),time) &
HoldsAt(AscendDescendAmount(diver,depth2),time) ->
depth1=depth2.

[diver,depth,time]
Happens(Descend(diver,depth),time) ->
HoldsAt(NegativelyBuoyant(diver),time) &
({depth1}
 HoldsAt(AscendDescendAmount(diver,depth1),time) &
 HoldsAt(AtDepth(diver,depth-depth1),time)).

event KickUp(diver)

[diver,depth,time]
Happens(Ascend(diver,depth),time) ->
(HoldsAt(PositivelyBuoyant(diver),time) |
 (HoldsAt(NeutrallyBuoyant(diver),time) & Happens(KickUp(diver),time))) &
({depth1}
 HoldsAt(AscendDescendAmount(diver,depth1),time) &
 HoldsAt(AtDepth(diver,depth+depth1),time)).

[diver,time]
Happens(KickUp(diver),time) ->
HoldsAt(Vertical(diver),time).

event SwimAround(diver)

[diver,time]
Happens(SwimAround(diver),time) ->
HoldsAt(HorizontalDown(diver),time).

; signaling

event SignalDescend(diver,diver)

event SignalOutOfTime(diver,diver)

event SignalAscend(diver,diver)

;[diver1,diver2,time]
;Happens(SignalAscend(diver1,diver2),time) ->
;Happens(SignalOutOfTime(diver1,diver2),time-1).

;[diver1,diver2,time]
;Happens(SignalDescend(diver1,diver2),time) ->
;HoldsAt(See(diver1,diver2),time) &
;HoldsAt(See(diver2,diver1),time).

;[diver1,diver2,time]
;Happens(SignalOutOfTime(diver1,diver2),time) ->
;HoldsAt(See(diver1,diver2),time) &
;HoldsAt(See(diver2,diver1),time).

;[diver1,diver2,time]
;Happens(SignalAscend(diver1,diver2),time) ->
;HoldsAt(See(diver1,diver2),time) &
;HoldsAt(See(diver2,diver1),time).

;event LookAt(agent,object)

;fluent See(agent,object)

;[agent,object,time]
;Initiates(LookAt(agent,object),See(agent,object),time).

;[agent,object1,object2,time]
;object1!=object2 ->
;Terminates(LookAt(agent,object1),
;           See(agent,object2),
;           time).

event Descend1(diver)

event Ascend1(diver)

;[diver,object,time]
;Terminates(Descend1(diver),See(diver,object),time).

;[diver,object,time]
;Terminates(Ascend1(diver),See(diver,object),time).

;[diver,object,time]
;Terminates(RotateYaw(diver),See(diver,object),time).

event RapidAscendToSurface(diver)

[diver,time]
Happens(Descend1(diver),time) <->
({depth} Happens(Descend(diver,depth),time)).

[diver,time]
Happens(Ascend1(diver),time) <->
({depth} Happens(Ascend(diver,depth),time)).

[diver,time]
Happens(RapidAscendToSurface(diver),time) ->
Happens(Ascend(diver,0),time).

event AscendLine(diver,line)

[diver,line,time]
Happens(AscendLine(diver,line),time) ->
Happens(Ascend1(diver),time).

fluent Disoriented(diver)

event BecomeDisoriented(diver)

event BecomeReoriented(diver)

[diver,time]
Initiates(BecomeDisoriented(diver),Disoriented(diver),time).

[diver,time]
Terminates(BecomeReoriented(diver),Disoriented(diver),time).

fluent DisturbedSilt()

event DisturbSilt(diver)

[diver,time]
Initiates(DisturbSilt(diver),DisturbedSilt(),time).

[diver,time]
Happens(BecomeDisoriented(diver),time) ->
(!HoldsAt(DisturbedSilt(),time-1) &
 HoldsAt(DisturbedSilt(),time)).

event Panic(diver)

[diver,time] Happens(Panic(diver),time) ->
HoldsAt(Disoriented(diver),time) |
HoldsAt(UncontrolledBuoyancy(diver),time) |
({equipment} Happens(Lose(diver,equipment),time-1)) |
Happens(Vomit(diver),time-1).

event Vomit(diver)

; conditions

fluent Unconscious(diver)

event GoUnconscious(diver)

event RegainConsciousness(diver)

[diver,time]
Initiates(GoUnconscious(diver),Unconscious(diver),time).

[diver,time]
Terminates(RegainConsciousness(diver),Unconscious(diver),time).

[diver,time]
Happens(GoUnconscious(diver),time) ->
Happens(RapidAscendToSurface(diver),time).

fluent HasEarPain(diver)

event StartEarPain(diver)

[diver,time] Initiates(StartEarPain(diver),HasEarPain(diver),time).

fluent HasRupturedEardrum(diver)

event RuptureEardrum(diver)

[diver,time]
Initiates(RuptureEardrum(diver),HasRupturedEardrum(diver),time).
fluent ConditionOK(diver)

fluent HasDecompressionIllness(diver)

event StartDecompressionIllness(diver)

[diver,time]
Initiates(StartDecompressionIllness(diver),
          HasDecompressionIllness(diver),
          time).

fluent SignalingDecompress(computer,diver)

fluent SignalingLowOnAir(computer,airtank,diver)

[computer,airtank,diver,time]
HoldsAt(SignalingLowOnAir(computer,airtank,diver),time) ->
HoldsAt(LowOnAir(airtank),time).

[computer,diver,time]
HoldsAt(SignalingDecompress(computer,diver),time) ->
!{time1} time1<time & Happens(Decompress(diver),time1).

event Decompress(diver)

event EqualizeEars(diver)

[diver,time]
(Happens(Descend1(diver),time) | Happens(Ascend1(diver),time)) &
!Happens(EqualizeEars(diver),time) ->
Happens(StartEarPain(diver),time) &
Happens(RuptureEardrum(diver),time).

[diver,time]
Happens(Ascend1(diver),time) &
!Happens(Decompress(diver),time) ->
Happens(StartDecompressionIllness(diver),time).

[diver1,diver2,time]
HoldsAt(Holding(diver1,diver2),time) &
Happens(Ascend1(diver1),time) &
!Happens(Decompress(diver2),time) ->
Happens(StartDecompressionIllness(diver2),time).

[diver,time]
Happens(Decompress(diver),time) ->
({depth} depth>0 & HoldsAt(AtDepth(diver,depth),time)) &
!HoldsAt(UncontrolledBuoyancy(diver),time).

fluent HasHeadache(diver)

[diver,time]
HoldsAt(ConditionOK(diver),time) ->
!HoldsAt(Unconscious(diver),time) &
!HoldsAt(HasEarPain(diver),time) &
!HoldsAt(HasRupturedEardrum(diver),time) &
!HoldsAt(HasDecompressionIllness(diver),time) &
!HoldsAt(HasHeadache(diver),time).

event BeAirlifted(diver)

event TakeInWater(diver)

fluent LowOnAir(airtank)

event BecomeLowOnAir(airtank)

[airtank,time]
Initiates(BecomeLowOnAir(airtank),LowOnAir(airtank),time).

; initial state
[diver] HoldsAt(ConditionOK(diver),0).
[diver] HoldsAt(Vertical(diver),0).
!HoldsAt(DisturbedSilt(),0).
[diver] !HoldsAt(UncontrolledBuoyancy(diver),0).
[diver] !HoldsAt(Disoriented(diver),0).
[diver] !HoldsAt(PositivelyBuoyant(diver),0) &
        !HoldsAt(NeutrallyBuoyant(diver),0) &
        !HoldsAt(NegativelyBuoyant(diver),0).
[diver,object] !HoldsAt(Wearing(diver,object),0).
[diver,object] !HoldsAt(Holding(diver,object),0).
[diver1,diver2] !HoldsAt(Separated(diver1,diver2),0).
;[agent,object] !HoldsAt(See(agent,object),0).

fluent Separated(diver,diver)

[diver1,diver2,time]
HoldsAt(Separated(diver1,diver2),time) ->
HoldsAt(Separated(diver2,diver1),time).

event BecomeSeparated(diver,diver)

event BeReunitedWith(diver,diver)

[diver1,diver2,time]
Initiates(BecomeSeparated(diver1,diver2),Separated(diver1,diver2),time).

[diver1,diver2,time]
Initiates(BecomeSeparated(diver1,diver2),Separated(diver2,diver1),time).

[diver1,diver2,time]
Terminates(BeReunitedWith(diver1,diver2),Separated(diver1,diver2),time).

[diver1,diver2,time]
Terminates(BeReunitedWith(diver1,diver2),Separated(diver2,diver1),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/HungerNeed.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; hunger need
;

fluent Hungry(agent)

fluent Satiated(agent)
noninertial Satiated

[agent,time] HoldsAt(Hungry(agent),time) <-> !HoldsAt(Satiated(agent),time).

event Eat(agent,food)

[agent,food,time]
Happens(Eat(agent,food),time) ->
{location}
HoldsAt(At(agent,location),time) &
HoldsAt(At(food,location),time).

[agent,food,time]
Terminates(Eat(agent,food),Hungry(agent),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Gun.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;

fluent Loaded(gun,bullet)
noninertial Loaded

[gun,bullet,time]
HoldsAt(Inside(bullet,gun),time) <->
HoldsAt(Loaded(gun,bullet),time).

event Shoot(agent,gun,object)

event ShootInjure(agent,gun,agent)

event ShootKill(agent,gun,agent)

event ShootDamage(agent,gun,physobj)

event ShootDestroy(agent,gun,physobj)

[agent,gun,bullet,object,time]
HoldsAt(Inside(bullet,gun),time) ->
Terminates(Shoot(agent,gun,object),
           Inside(bullet,gun),
           time).

[agent,gun,bullet,object,location1,location2,time]
HoldsAt(Inside(bullet,gun),time) &
HoldsAt(At(gun,location1),time) &
location1 != location2 ->
Terminates(Shoot(agent,gun,object),At(bullet,location2),time).

[agent,gun,bullet,object,location,time]
HoldsAt(At(object,location),time) &
HoldsAt(Inside(bullet,gun),time) ->
Initiates(Shoot(agent,gun,object),At(bullet,location),time).

[agent,gun,object,time]
Happens(Shoot(agent,gun,object),time) ->
HoldsAt(Holding(agent,gun),time) &
({bullet} HoldsAt(Loaded(gun,bullet),time)) &
({location} HoldsAt(At(agent,location),time) &
            HoldsAt(At(object,location),time)).

[agent1,gun,agent2,time]
Happens(Shoot(agent1,gun,agent2),time) ->
Happens(ShootInjure(agent1,gun,agent2),time) |
Happens(ShootKill(agent1,gun,agent2),time).

[agent1,gun,bullet,agent2,time]
HoldsAt(Inside(bullet,gun),time) &
Happens(ShootKill(agent1,gun,agent2),time) ->
Happens(Kill(bullet,agent2),time).

[agent1,gun,bullet,agent2,time]
HoldsAt(Inside(bullet,gun),time) &
Happens(ShootInjure(agent1,gun,agent2),time) ->
Happens(Injure(bullet,agent2),time).

[agent,gun,physobj,time]
Happens(Shoot(agent,gun,physobj),time) ->
Happens(ShootDamage(agent,gun,physobj),time) |
Happens(ShootDestroy(agent,gun,physobj),time).

[agent,gun,bullet,physobj,time]
HoldsAt(Inside(bullet,gun),time) &
Happens(ShootDamage(agent,gun,physobj),time) ->
Happens(Damage(bullet,physobj),time).

[agent,gun,bullet,physobj,time]
HoldsAt(Inside(bullet,gun),time) &
Happens(ShootDestroy(agent,gun,physobj),time) ->
Happens(Destroy(bullet,physobj),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/RTSpace.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; RTSpace: room-scale topological space
;
; We use topological and metric representations of space,
; at two levels of granularity---room-scale and object-scale.
; The RTSpace representation deals with topological space at
; the scale of rooms and outdoor locations.
; This representation of space consists of locations, which
; are connected by portals. There are two types of locations:
; rooms and outside areas (outsides).
;

; object is at location.
fluent At(object,location)
manualrelease At

[object1,location,time]
({object2} PartOf(object1,object2)) ->
ReleasedAt(At(object1,location),time).

; A state constraint says that an object
; is at one location at a time:
[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
HoldsAt(At(object,location2),time) ->
location1=location2.

; connectivity

; Side one of portal is location.
function Side1(portal): location
; Side two of portal is location.
function Side2(portal): location

; The building of room is building.
function BuildingOf(room): building

; object is at a location that has portal.
fluent NearPortal(object,portal)
noninertial NearPortal

; A state constraint says that an object is near
; a portal if and only if there is a location such that
; the object is at the location and one of the sides
; of the portal is the location:
[object,portal,time]
HoldsAt(NearPortal(object,portal),time) <->
{location}
 (Side1(portal)=location|
  Side2(portal)=location) &
 HoldsAt(At(object,location),time).

; locking and unlocking doors

; agent unlocks door.
event DoorUnlock(agent,door)
; agent locks door.
event DoorLock(agent,door)
; door is unlocked.
fluent DoorUnlocked(door)

; A precondition axiom states that
; for an agent to unlock a door,
; the agent must be awake,
; the door must not already be unlocked, and
; the agent must be near the door:
[agent,door,time]
Happens(DoorUnlock(agent,door),time) ->
HoldsAt(Awake(agent),time) &
!HoldsAt(DoorUnlocked(door),time) &
HoldsAt(NearPortal(agent,door),time).

; An effect axiom states that
; if an agent unlocks a door,
; the door will be unlocked:
[agent,door,time]
Initiates(DoorUnlock(agent,door),DoorUnlocked(door),time).

; A precondition axiom states that
; for an agent to lock a door,
; the agent must be awake,
; the door must be unlocked, and
; the agent must be near the door:
[agent,door,time]
Happens(DoorLock(agent,door),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(DoorUnlocked(door),time) &
HoldsAt(NearPortal(agent,door),time).

; An effect axiom states that
; if an agent locks a door,
; the door will no longer be unlocked.
[agent,door,time]
Terminates(DoorLock(agent,door),DoorUnlocked(door),time).

; A state constraint says that if a door is open,
; it is unlocked:
[door,time]
HoldsAt(DoorIsOpen(door),time) -> HoldsAt(DoorUnlocked(door),time).

; opening and closing doors

; agent opens door.
event DoorOpen(agent,door)
; agent closes door.
event DoorClose(agent,door)
; door is open.
fluent DoorIsOpen(door)

; A precondition axiom states that
; for an agent to open a door,
; the agent must be awake,
; the door must not already be open,
; the door must be unlocked, and
; the agent must be near the door:
[agent,door,time]
Happens(DoorOpen(agent,door),time) ->
HoldsAt(Awake(agent),time) &
!HoldsAt(DoorIsOpen(door),time) &
HoldsAt(DoorUnlocked(door),time) &
HoldsAt(NearPortal(agent,door),time).

; An effect axiom states that
; if an agent opens a door,
; the door will be open:
[agent,door,time]
Initiates(DoorOpen(agent,door),DoorIsOpen(door),time).

; A precondition axiom states that
; for an agent to close a door,
; the agent must be awake,
; the door must be open,
; the door must be unlocked, and
; the agent must be near the door:
[agent,door,time]
Happens(DoorClose(agent,door),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(DoorIsOpen(door),time) &
HoldsAt(DoorUnlocked(door),time) &
HoldsAt(NearPortal(agent,door),time).

; An effect axiom states that
; if an agent closes a door,
; the door will no longer be open:
[agent,door,time]
Terminates(DoorClose(agent,door),DoorIsOpen(door),time).

; passing through doors

; agent walks through side one of door.
event WalkThroughDoor12(agent,door)
; agent walks through side two of door.
event WalkThroughDoor21(agent,door)

; Precondition axioms state that
; for an agent to walk through a side of a door,
; the agent must be awake and standing,
; the door must be open, and
; the agent must be at the side of the door that
; the agent walks through:
[agent,door,time]
Happens(WalkThroughDoor12(agent,door),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(Standing(agent),time) &
HoldsAt(DoorIsOpen(door),time) &
HoldsAt(At(agent,Side1(door)),time).

[agent,door,time]
Happens(WalkThroughDoor21(agent,door),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(Standing(agent),time) &
HoldsAt(DoorIsOpen(door),time) &
HoldsAt(At(agent,Side2(door)),time).

; Effect axioms state that
; if an agent walks through one side of a door,
; the agent will be at the other side of the door:
[agent,door,location,time]
Side2(door)=location ->
Initiates(WalkThroughDoor12(agent,door),At(agent,location),time).

[agent,door,location,time]
Side1(door)=location ->
Initiates(WalkThroughDoor21(agent,door),At(agent,location),time).

[agent,door,location,time]
Side1(door)=location ->
Terminates(WalkThroughDoor12(agent,door),At(agent,location),time).

[agent,door,location,time]
Side2(door)=location ->
Terminates(WalkThroughDoor21(agent,door),At(agent,location),time).

; walking from one end of a street to another

; agent walks from the first end of street to the second end.
event WalkStreet12(agent,street)
; agent walks from the second end of street to the first end.
event WalkStreet21(agent,street)

; Precondition axioms state that
; for an agent to walk from one end of a street to another,
; the agent must be awake,
; the agent must be standing, and
; the agent must be at the first end of the street:
[agent,street,time]
Happens(WalkStreet12(agent,street),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(Standing(agent),time) &
HoldsAt(At(agent,Side1(street)),time).

[agent,street,time]
Happens(WalkStreet21(agent,street),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(Standing(agent),time) &
HoldsAt(At(agent,Side2(street)),time).

; Effect axioms state that
; if an agent walks from one end of a street to another,
; the agent will be at the other end of the street:
[agent,street,location,time]
Side2(street)=location ->
Initiates(WalkStreet12(agent,street),At(agent,location),time).

[agent,street,location,time]
Side1(street)=location ->
Initiates(WalkStreet21(agent,street),At(agent,location),time).

[agent,street,location,time]
Side1(street)=location ->
Terminates(WalkStreet12(agent,street),At(agent,location),time).

[agent,street,location,time]
Side2(street)=location ->
Terminates(WalkStreet21(agent,street),At(agent,location),time).

; floors

; The floor of room is integer.
function Floor(room): integer

; walking up and down staircases

; agent walks down staircase.
event WalkDownStaircase(agent,staircase)
; agent walks up staircase.
event WalkUpStaircase(agent,staircase)

; Precondition axioms state that
; for an agent to walk down (up) a staircase,
; the agent must be awake, standing, and
; at the top (bottom) of the staircase:
[agent,staircase,time]
Happens(WalkDownStaircase(agent,staircase),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(Standing(agent),time) &
HoldsAt(At(agent,Side2(staircase)),time).

[agent,staircase,time]
Happens(WalkUpStaircase(agent,staircase),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(Standing(agent),time) &
HoldsAt(At(agent,Side1(staircase)),time).

; Effect axioms state that
; if an agent walks down (up) a staircase,
; the agent will be at the bottom (top) of the staircase:
[agent,staircase,room,time]
Side1(staircase)=room ->
Initiates(WalkDownStaircase(agent,staircase),At(agent,room),time).

[agent,staircase,room,time]
Side2(staircase)=room ->
Terminates(WalkDownStaircase(agent,staircase),At(agent,room),time).

[agent,staircase,room,time]
Side2(staircase)=room ->
Initiates(WalkUpStaircase(agent,staircase),At(agent,room),time).

[agent,staircase,room,time]
Side1(staircase)=room ->
Terminates(WalkUpStaircase(agent,staircase),At(agent,room),time).

; A state constraint says that if an agent is outside,
; the agent is dressed.
[agent,outside,time]
HoldsAt(At(agent,outside),time) ->
HoldsAt(Dressed(agent),time).

; room looks out onto outside.
function LookOutOnto(room): outside

; location1 is adjacent to location2.
predicate Adjacent(location,location)

; A state constraint says that
; two locations are adjacent if and only if
; they have a portal in common:
[location1,location2] Adjacent(location1,location2) <->
{portal}
(Side1(portal)=location1 &
 Side2(portal)=location2) |
(Side2(portal)=location1 &
 Side1(portal)=location2).

; The ground of outside is ground.
function GroundOf(outside): ground
; The sky of outside is sky.
function SkyOf(outside): sky

; State constraints fix the location of ground and sky:
[outside,ground,time]
GroundOf(outside) = ground ->
HoldsAt(At(ground,outside),time).

[outside,sky,time]
SkyOf(outside) = sky ->
HoldsAt(At(sky,outside),time).

; End of file.
