


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: foundations/Root.e
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
sort boolean
sort integer
reified sort predicate
reified sort function

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: foundations/ECTraj.e
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
; @incollection{MillerShanahan:2002,
;   author = "Rob Miller and Murray Shanahan",
;   year = "2002",
;   title = "Some alternative formulations of the event calculus",
;   editor = "Antonis C. Kakas and Fariba Sadri",
;   booktitle = "Computational Logic: Logic Programming and Beyond: Essays in Honour of \uppercase{R}obert \uppercase{A}. \uppercase{K}owalski, Part \uppercase{II}",
;   series = "Lecture Notes in Computer Science",
;   volume = "2408",
;   pages = "452--490",
;   address = "Berlin",
;   publisher = "Springer",
; }
;

predicate Clipped(time,fluent,time)
predicate Declipped(time,fluent,time)

predicate Trajectory(fluent,time,fluent,offset)
predicate AntiTrajectory(fluent,time,fluent,offset)

[event,fluent,fluent2,offset,time]
Happens(event,time) &
Initiates(event,fluent,time) &
0 < offset &
Trajectory(fluent,time,fluent2,offset) &
!Clipped(time,fluent,time+offset) ->
HoldsAt(fluent2,time+offset).

[event,fluent,fluent2,offset,time]
Happens(event,time) &
Terminates(event,fluent,time) &
0 < offset &
AntiTrajectory(fluent,time,fluent2,offset) &
!Declipped(time,fluent,time+offset) ->
HoldsAt(fluent2,time+offset).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: foundations/DEC.e
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
; Discrete Event Calculus (DEC)
;
; @article{Mueller:2004a,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Event calculus reasoning through satisfiability",
;   journal = "Journal of Logic and Computation",
;   volume = "14",
;   number = "5",
;   pages = "703--730",
; }
;

sort time: integer
sort offset: integer

reified sort fluent
reified sort event

predicate Happens(event,time)
predicate HoldsAt(fluent,time)
predicate ReleasedAt(fluent,time)

predicate Initiates(event,fluent,time)
predicate Terminates(event,fluent,time)
predicate Releases(event,fluent,time)

[fluent,time]
(HoldsAt(fluent,time) &
 !ReleasedAt(fluent,time+1) &
 !({event} Happens(event,time) & Terminates(event,fluent,time))) ->
HoldsAt(fluent,time+1).

[fluent,time]
(!HoldsAt(fluent,time) &
 !ReleasedAt(fluent,time+1) &
 !({event} Happens(event,time) & Initiates(event,fluent,time))) ->
!HoldsAt(fluent,time+1).

[fluent,time]
(!ReleasedAt(fluent,time) &
 !({event} Happens(event,time) & Releases(event,fluent,time))) ->
!ReleasedAt(fluent,time+1).

[fluent,time]
(ReleasedAt(fluent,time) &
 !({event} Happens(event,time) &
   (Initiates(event,fluent,time) |
    Terminates(event,fluent,time)))) ->
ReleasedAt(fluent,time+1).

[event,fluent,time]
(Happens(event,time) & Initiates(event,fluent,time)) ->
(HoldsAt(fluent,time+1) & !ReleasedAt(fluent,time+1)).

[event,fluent,time]
(Happens(event,time) & Terminates(event,fluent,time)) ->
(!HoldsAt(fluent,time+1) & !ReleasedAt(fluent,time+1)).

[event,fluent,time]
(Happens(event,time) & Releases(event,fluent,time)) ->
ReleasedAt(fluent,time+1).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: foundations/EC.e
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
; Event Calculus (EC)
;
; @incollection{MillerShanahan:2002,
;   author = "Rob Miller and Murray Shanahan",
;   year = "2002",
;   title = "Some alternative formulations of the event calculus",
;   editor = "Antonis C. Kakas and Fariba Sadri",
;   booktitle = "Computational Logic: Logic Programming and Beyond: Essays in Honour of \uppercase{R}obert \uppercase{A}. \uppercase{K}owalski, Part \uppercase{II}",
;   series = "Lecture Notes in Computer Science",
;   volume = "2408",
;   pages = "452--490",
;   address = "Berlin",
;   publisher = "Springer",
; }
;

sort time: integer
sort offset: integer

reified sort fluent
reified sort event

predicate Happens(event,time)
predicate HoldsAt(fluent,time)
predicate ReleasedAt(fluent,time)
predicate Initiates(event,fluent,time)
predicate Terminates(event,fluent,time)
predicate Releases(event,fluent,time)
predicate Trajectory(fluent,time,fluent,offset)

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: foundations/ECCausal.e
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
; Causal Constraints
;
; @inproceedings{Shanahan:1999a,
;   author = "Murray Shanahan",
;   year = "1999",
;   title = "The ramification problem in the event calculus",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}ixteenth \uppercase{I}nternational \uppercase{J}oint \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
;   pages = "140--146",
;   address = "San Mateo, CA",
;   publisher = "Morgan Kaufmann",
; }
;

predicate Started(fluent,time)
predicate Stopped(fluent,time)

[fluent,time]
Started(fluent,time) <->
(HoldsAt(fluent,time) |
 ({event} Happens(event,time) & Initiates(event,fluent,time))).

[fluent,time]
Stopped(fluent,time) <->
(!HoldsAt(fluent,time) |
 ({event} Happens(event,time) & Terminates(event,fluent,time))).

predicate Initiated(fluent,time)
predicate Terminated(fluent,time)

[fluent,time]
Initiated(fluent,time) <->
(Started(fluent,time) &
 !({event} Happens(event,time) & Terminates(event,fluent,time))).

[fluent,time]
Terminated(fluent,time) <->
(Stopped(fluent,time) &
 !({event} Happens(event,time) & Initiates(event,fluent,time))).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: includes/SaaSPatterns/sorts.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sort rule,subject,object,action,ruleeffect,policy,policyset














; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: includes/SaaSPatterns/RulesPatterns/ruleOutput.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fluent F_RuleDenied(rule)
fluent F_RulePermitted(rule)

event Epermit(rule)
event EDeny(rule)

[rule,time] Initiates(EDeny(rule),F_RuleDenied(rule),time).
[rule,time] Initiates(Epermit(rule),F_RulePermitted(rule),time).


[rule] !HoldsAt(F_RulePermitted(rule),0).
[rule] !HoldsAt(F_RuleDenied(rule),0).












;[rule] HoldsAt(F_RulePermitted(rule),3) | HoldsAt(F_RuleDenied(rule),3).
;[rule,time] Happens(RuleDeny(rule), time) |  Happens(RulePermit(rule), time) -> time=2.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: includes/SaaSPatterns/RulesPatterns/targetHolds.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fluent F_TargetHolds(rule)
fluent F_TargetDoesntHolds(rule)

event E_MatchRuleParametters(rule)
event E_DontMatchRuleParametters(rule)

[rule,time] Initiates(E_MatchRuleParametters(rule),F_TargetHolds(rule),time).
[rule,time] Initiates(E_DontMatchRuleParametters(rule),F_TargetDoesntHolds(rule),time).

[rule,time] Happens(E_MatchRuleParametters(rule), time) -> !HoldsAt(F_TargetHolds(rule),time).
[rule,time] Happens(E_DontMatchRuleParametters(rule), time) -> !HoldsAt(F_TargetDoesntHolds(rule),time).


[rule] !HoldsAt(F_TargetHolds(rule),0).
[rule] !HoldsAt(F_TargetDoesntHolds(rule),0).



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: includes/SaaSPatterns/RulesPatterns/ConditionsVerification.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fluent F_ConditionSatisfied(rule)
;event E_ConditionSatisfied(rule)


;[rule,time] Initiates(E_ConditionSatisfied(rule),F_ConditionSatisfied(rule),time).

;[rule,time] Happens(E_ConditionSatisfied(rule),time) -> HoldsAt(F_TargetHolds(rule),time).

;[rule,time] Happens(E_ConditionSatisfied(rule), time) -> !HoldsAt(F_ConditionSatisfied(rule),time).


[rule] HoldsAt(F_ConditionSatisfied(rule),0).



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: includes/SaaSPatterns/RulesPatterns/ruleModel.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fluent F_RuleEffectPermitted(rule); prédéfinies
fluent F_RuleEffectNOTpermitted(rule) ;prédéfinies


fluent F_RuleDenied(rule)
fluent F_RulePermitted(rule)
fluent F_RuleNotApplicable(rule)



event Epermit(rule)
event EDeny(rule)
event ERuleDoesNotApply(rule)


[rule,time] Initiates(EDeny(rule),F_RuleDenied(rule),time).
[rule,time] Initiates(Epermit(rule),F_RulePermitted(rule),time).
[rule,time] Initiates(ERuleDoesNotApply(rule),F_RuleNotApplicable(rule),time).


[rule,time] Happens(EDeny(rule),time) -> HoldsAt(F_TargetHolds(rule),time)
                                            & HoldsAt(F_ConditionSatisfied(rule),time)
                                            & HoldsAt(F_RuleEffectNOTpermitted(rule),time).


[rule,time] Happens(Epermit(rule),time) -> HoldsAt(F_TargetHolds(rule),time)
                                            & HoldsAt(F_ConditionSatisfied(rule),time)
                                            & HoldsAt(F_RuleEffectPermitted(rule),time).

[rule,time] Happens(ERuleDoesNotApply(rule),time) -> HoldsAt(F_TargetDoesntHolds(rule),time).



[rule] !HoldsAt(F_RulePermitted(rule),0).
[rule] !HoldsAt(F_RuleDenied(rule),0).
[rule] !HoldsAt(F_RuleNotApplicable(rule),0).




;********************************************************************************************************************
;--------------------------------------------------------------------------------------------------------------------
;********************************************************************************************************************

;[rule] HoldsAt(F_RulePermitted(rule),3) | HoldsAt(F_RuleDenied(rule),3).




;[rule,time] Happens(RuleDeny(rule), time) |  Happens(RulePermit(rule), time) -> time=2.

;[rule,time,ruleeffect] Happens(EDeny(rule),time) -> HoldsAt(F_TargetHolds(rule),time)
                                                ;    & HoldsAt(F_ConditionSatisfied(rule),time)
                                                ;    & ruleeffect=Deny.


;[rule,time,ruleeffect] Happens(Epermit(rule),time) -> HoldsAt(F_TargetHolds(rule),time)
                                               ;    & HoldsAt(F_ConditionSatisfied(rule),time)
                                               ;    & ruleeffect=Permit.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: includes/SaaSPatterns/ordering.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[rule,time] Happens(E_MatchRuleParametters(rule), time) | Happens(E_DontMatchRuleParametters(rule), time) -> time = 0.

[rule,time] Happens(EDeny(rule), time) | Happens(Epermit(rule), time) | Happens(ERuleDoesNotApply(rule), time) -> time = 1.

;[policy,time] Happens(E_policyPermit(policy), time) | Happens(E_policyDeny(policy), time) | Happens(E_PolicyDoesNotApply(policy),time) -> time = 2.


;[policyset,time] Happens(E_policysetPermit(policyset), time) | Happens(E_policysetDeny(policyset), time) | Happens(E_policysetDontApply(policyset),time) -> time = 3.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: includes/SaaSPatterns/PolicySetPatterns/policySetModel.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
event E_policysetPermit(policyset)
event E_policysetDeny(policyset)
event E_policysetDontApply(policyset)

fluent F_policysetPermitted(policyset)
fluent F_policysetDenied(policyset)
fluent F_policySetNotApplicable(policyset)

predicate PolicysetHaspolicies(policyset,policy)


[policyset,time] Initiates(E_policysetPermit(policyset),F_policysetPermitted(policyset),time).
[policyset,time] Initiates(E_policysetDeny(policyset),F_policysetDenied(policyset),time).
[policyset,time] Initiates(E_policysetDontApply(policyset),F_policySetNotApplicable(policyset),time).



; 'policies combaning algorithm (stategy) : All Permit'
[policyset,policy,time] Happens(E_policysetPermit(policyset),time) & PolicysetHaspolicies(policyset,policy) -> HoldsAt(F_policyPermitted(policy),time).


; 'policies combaning algorithm (stategy) : Deny override'
[policyset,time] Happens(E_policysetDeny(policyset),time) -> {policy}  PolicysetHaspolicies(policyset,policy) & HoldsAt(F_policyDenied(policy),time).


; 'policies combaning algorithm (stategy) : All Permit'
[policyset,policy,time] Happens(E_policysetDontApply(policyset),time) & PolicysetHaspolicies(policyset,policy) -> HoldsAt(F_policyNotApplicable(policy),time).


[policyset]!HoldsAt(F_policysetPermitted(policyset),0).
[policyset]!HoldsAt(F_policysetDenied(policyset),0).
[policyset]!HoldsAt(F_policySetNotApplicable(policyset),0).



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: includes/SaaSPatterns/PolicyPatterns/policyModel.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
event E_policyPermit(policy)
event E_policyDeny(policy)
event E_PolicyDoesNotApply(policy)

fluent F_policyPermitted(policy)
fluent F_policyDenied(policy)
fluent F_policyNotApplicable(policy)

predicate PolicyHasRules(policy,rule)


[policy,time] Initiates(E_policyPermit(policy),F_policyPermitted(policy),time).
[policy,time] Initiates(E_policyDeny(policy),F_policyDenied(policy),time).
[policy,time] Initiates(E_PolicyDoesNotApply(policy),F_policyNotApplicable(policy),time).



; 'Rule combaning algorithm (stategy) : All Permit'
[policy,rule,time] Happens(E_policyPermit(policy),time) & PolicyHasRules(policy,rule) -> HoldsAt(F_RulePermitted(rule),time).


; 'Rule combaning algorithm (stategy) : Deny override (s il existe au moin une règle satisfaite)'
[policy,time] Happens(E_policyDeny(policy),time) -> {rule}  PolicyHasRules(policy,rule) & HoldsAt(F_RuleDenied(rule),time).


; 'Rule combaning algorithm (stategy) : All not Applicable'
[policy,time,rule] Happens(E_PolicyDoesNotApply(policy),time) & PolicyHasRules(policy,rule) -> HoldsAt(F_RuleNotApplicable(rule),time).


[policy]!HoldsAt(F_policyPermitted(policy),0).
[policy]!HoldsAt(F_policyDenied(policy),0).
[policy]!HoldsAt(F_policyNotApplicable(policy),0).



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: includes/SaaSPatterns/input.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
subject Navas
object Gloves
action Get




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



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004a/Holding.e
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
; @article{Mueller:2004a,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Event calculus reasoning through satisfiability",
;   journal = "Journal of Logic and Computation",
;   volume = "14",
;   number = "5",
;   pages = "703--730",
; }
;

option encoding 3

load foundations/Root.e
load foundations/EC.e

sort person
sort object

event Hold(person,object)
fluent Holding(person,object)

person P1
object O1

Happens(Hold(P1,O1),0).

[person,object,time]
Initiates(Hold(person,object),Holding(person,object),time).

!HoldsAt(Holding(P1,O1),0).
;;; AUTO !ReleasedAt(Holding(P1,O1),0).

completion Happens

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004a/Leaf.e
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
; @article{Mueller:2004a,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Event calculus reasoning through satisfiability",
;   journal = "Journal of Logic and Computation",
;   volume = "14",
;   number = "5",
;   pages = "703--730",
; }
;

option trajectory on

load foundations/Root.e
load foundations/EC.e

sort object
sort height: integer

fluent Height(object,height)
fluent Falling(object)
event StartFalling(object)
event HitsGround(object)

[object,height1,height2,time]
HoldsAt(Height(object,height1),time) &
HoldsAt(Height(object,height2),time) ->
height1=height2.

[object,time]
Initiates(StartFalling(object),Falling(object),time).

[object,height,time]
Releases(StartFalling(object),Height(object,height),time).

[object,height1,height2,offset,time]
HoldsAt(Height(object,height1),time) &
height2=height1-offset*offset ->
Trajectory(Falling(object),time,Height(object,height2),offset).

[object,time]
HoldsAt(Falling(object),time) &
HoldsAt(Height(object,0),time) ->
Happens(HitsGround(object),time).

;[object,height1,height2,time]
;HoldsAt(Height(object,height1),time) &
;height1 != height2 ->
;Terminates(HitsGround(object),Height(object,height2),time).

[object,height,time]
HoldsAt(Height(object,height),time) ->
Initiates(HitsGround(object),Height(object,height),time).

[object,time]
Terminates(HitsGround(object),Falling(object),time).

object Leaf

!HoldsAt(Falling(Leaf),0).
HoldsAt(Height(Leaf,9),0).
Happens(StartFalling(Leaf),0).

completion Happens

range time 0 4
range offset 1 9
range height 0 9

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Cassimatis2002/PolySpace.e
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
; FILE: examples/Cassimatis2002/TwoScreens.e
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

load foundations/Root.e
load foundations/EC.e
load examples/Cassimatis2002/PolySpace.e

grid G1
object X,Y,Screen1,Screen2

; perceptions:
Shape(X,Round).
Color(X,Red).
Shape(Y,Round).
Color(Y,Red).
Shape(Screen1,Square).
Color(Screen1,Green).
Shape(Screen2,Square).
Color(Screen2,Green).
[time] HoldsAt(Location(G1,Screen1,2,0),time).
[time] HoldsAt(Location(G1,Screen2,4,0),time).
HoldsAt(Location(G1,X,1,1),0).
HoldsAt(Location(G1,Y,5,1),4).

[xcoord,ycoord,time]
xcoord!=2 & xcoord!=4 & !(xcoord=1 & ycoord=1 & time=0) ->
!HoldsAt(Location(G1,X,xcoord,ycoord),time) |
xcoord=5 & ycoord=1 & time=4 & Equal(X,Y).

[xcoord,ycoord,time]
xcoord!=2 & xcoord!=4 & !(xcoord=5 & ycoord=1 & time=4) ->
!HoldsAt(Location(G1,Y,xcoord,ycoord),time) |
xcoord=1 & ycoord=1 & time=0 & Equal(X,Y).

range time 0 4
range xcoord 0 5
range ycoord 0 1
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Cassimatis2002/OneScreen.e
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

load foundations/Root.e
load foundations/EC.e
load examples/Cassimatis2002/PolySpace.e

grid G1
object X,Y,Screen

; perceptions:
Shape(X,Round).
Color(X,Red).
Shape(Y,Round).
Color(Y,Red).
Shape(Screen,Square).
Color(Screen,Green).
[time] HoldsAt(Location(G1,Screen,2,0),time).
HoldsAt(Location(G1,X,1,1),0).
HoldsAt(Location(G1,Y,3,1),2).

[xcoord,ycoord,time]
xcoord!=2 & !(xcoord=1 & ycoord=1 & time=0) ->
!HoldsAt(Location(G1,X,xcoord,ycoord),time) |
xcoord=3 & ycoord=1 & time=2 & Equal(X,Y).

[xcoord,ycoord,time]
xcoord!=2 & !(xcoord=3 & ycoord=1 & time=2) ->
!HoldsAt(Location(G1,Y,xcoord,ycoord),time) |
xcoord=1 & ycoord=1 & time=0 & Equal(X,Y).

range time 0 2
range xcoord 0 4
range ycoord 0 2
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/BrewkaDixKonolige1997/Wine.e
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
; reasoning by cases
; \fullciteA[p. 45]{BrewkaDixKonolige:1997}
;
; @book{BrewkaDixKonolige:1997,
;   author = "Gerhard Brewka and J{\"{u}}rgen Dix and Kurt Konolige",
;   year = "1997",
;   title = "Nonmonotonic Reasoning: An Overview",
;   address = "Stanford, CA",
;   publisher = "CSLI",
; }
;

load foundations/Root.e
load foundations/EC.e

sort x
x Person

predicate LikesWine(x)
predicate Italian(x)
predicate French(x)
predicate Ab1(x)
predicate Ab2(x)

[x] Italian(x) & !Ab1(x) -> LikesWine(x).
[x] French(x) & !Ab2(x) -> LikesWine(x).
[x] Italian(x) -> !French(x).

Italian(Person) | French(Person).

range time 0 0
range offset 1 1

completion Theta Ab1
completion Theta Ab2

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1997/Yale.e
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
; @article{HanksMcDermott:1987,
;   author = "Steve Hanks and Drew V. McDermott",
;   year = "1987",
;   title = "Nonmonotonic logic and temporal projection",
;   journal = "Artificial Intelligence",
;   volume = "33",
;   number = "3",
;   pages = "379--412",
; }
;
; \fullciteA[pp. 322--323]{Shanahan:1997}
;
; @book{Shanahan:1997,
;   author = "Murray Shanahan",
;   year = "1997",
;   title = "Solving the Frame Problem",
;   address = "Cambridge, MA",
;   publisher = "MIT Press",
; }
;
; deduction
;
; modifications from Shanahan's formulation:
; InitiallyP -> HoldsAt
; timestamps
; added [time] Terminates(Shoot(),Loaded(),time).
;

option showpred off

load foundations/Root.e
load foundations/EC.e

event Load()
event Shoot()
event Sneeze()
fluent Loaded()
fluent Alive()

[time] Initiates(Load(),Loaded(),time).
[time] HoldsAt(Loaded(),time) -> Terminates(Shoot(),Alive(),time).
[time] Terminates(Shoot(),Loaded(),time).

HoldsAt(Alive(),0).
!HoldsAt(Loaded(),0).
Happens(Load(),0).
Happens(Sneeze(),1).
Happens(Shoot(),2).

completion Happens

range time 0 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1997/StuffyRoom.e
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
; @article{GinsbergSmith:1988a,
;   author = "Matthew L. Ginsberg and David E. Smith",
;   year = "1988",
;   title = "Reasoning about action \uppercase{I}: \uppercase{A} possible worlds approach",
;   journal = "Artificial Intelligence",
;   volume = "35",
;   number = "2",
;   pages = "165--195",
; }
;
; \fullciteA[pp. 288--289]{Shanahan:1997}
;
; @book{Shanahan:1997,
;   author = "Murray Shanahan",
;   year = "1997",
;   title = "Solving the Frame Problem",
;   address = "Cambridge, MA",
;   publisher = "MIT Press",
; }
;
; deduction
;
; modifications from Shanahan's formulation:
; timestamps
; added:
; !HoldsAt(Blocked1(),0).
; !HoldsAt(Blocked2(),0).
;

load foundations/Root.e
load foundations/EC.e

event Close1()
event Close2()
event Start()
fluent Blocked1()
fluent Blocked2()
fluent Stuffy()
noninertial Stuffy

[time] Initiates(Close1(),Blocked1(),time).
[time] Initiates(Close2(),Blocked2(),time).

[time]
HoldsAt(Stuffy(),time) <->
HoldsAt(Blocked1(),time)&HoldsAt(Blocked2(),time).

[time] Initiates(Start(),Blocked1(),time).
[time] Terminates(Start(),Blocked2(),time).

!HoldsAt(Blocked1(),0).
!HoldsAt(Blocked2(),0).
Happens(Start(),0).
Happens(Close2(),1).

completion Happens

range time 0 2
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1997/BusRide.e
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
; @article{Kartha:1994,
;   author = "G. Neelakantan Kartha",
;   year = "1994",
;   title = "Two counterexamples related to \uppercase{B}aker's approach to the frame problem",
;   journal = "Artificial Intelligence",
;   volume = "69",
;   number = "1--2",
;   pages = "379--391",
; }
;
; \fullciteA[pp. 359--361]{Shanahan:1997}
;
; @book{Shanahan:1997,
;   author = "Murray Shanahan",
;   year = "1997",
;   title = "Solving the Frame Problem",
;   address = "Cambridge, MA",
;   publisher = "MIT Press",
; }
;
; modifications from Shanahan's formulation:
; InitiallyN -> !HoldsAt
; timestamps
;

load foundations/Root.e
load foundations/EC.e

fluent HasTicket()
fluent OnRed()
fluent OnYellow()
event Buy()
event Board()
event BoardRed()
event BoardYellow()

[time] Happens(Board(),time) -> Happens(BoardRed(),time) | Happens(BoardYellow(),time).

[time] Initiates(Buy(),HasTicket(),time).
[time] HoldsAt(HasTicket(),time) -> Initiates(BoardRed(),OnRed(),time).
[time] HoldsAt(HasTicket(),time) -> Initiates(BoardYellow(),OnYellow(),time).

[time] !(HoldsAt(OnRed(),time) & HoldsAt(OnYellow(),time)).
[time] HoldsAt(OnRed(),time) -> HoldsAt(HasTicket(),time).
[time] HoldsAt(OnYellow(),time) -> HoldsAt(HasTicket(),time).

HoldsAt(OnRed(),2).

!HoldsAt(HasTicket(),0).
Happens(Buy(),0).
Happens(Board(),1).
; ABDUCED Happens(BoardRed(), 1).

completion Happens

range time 0 2
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1997/DeadOrAlive.e
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
; \fullciteA[p. 324]{Shanahan:1997}
;
; @book{Shanahan:1997,
;   author = "Murray Shanahan",
;   year = "1997",
;   title = "Solving the Frame Problem",
;   address = "Cambridge, MA",
;   publisher = "MIT Press",
; }
;
; deduction
;
; modifications from Shanahan's formulation:
; InitiallyP -> HoldsAt
; timestamps
; added [time] Terminates(Shoot(),Loaded(),time).
;

load foundations/Root.e
load foundations/EC.e

event Load()
event Shoot()
event Sneeze()
fluent Loaded()
fluent Alive()
fluent Dead()
noninertial Dead

[time] Initiates(Load(),Loaded(),time).
[time] HoldsAt(Loaded(),time) -> Terminates(Shoot(),Alive(),time).
[time] Terminates(Shoot(),Loaded(),time).
[time] HoldsAt(Dead(),time) <-> !HoldsAt(Alive(),time).

HoldsAt(Alive(),0).
!HoldsAt(Loaded(),0).
Happens(Load(),0).
Happens(Sneeze(),1).
Happens(Shoot(),2).

completion Happens

range time 0 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1997/Supermarket.e
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
; \fullciteA[pp. 302--304]{Shanahan:1997}
;
; @book{Shanahan:1997,
;   author = "Murray Shanahan",
;   year = "1997",
;   title = "Solving the Frame Problem",
;   address = "Cambridge, MA",
;   publisher = "MIT Press",
; }
;
; deduction
;
; modifications from Shanahan's formulation:
; reformulated using the method of \fullciteA[pp. 460--461]{MillerShanahan:2002}
;
; @incollection{MillerShanahan:2002,
;   author = "Rob Miller and Murray Shanahan",
;   year = "2002",
;   title = "Some alternative formulations of the event calculus",
;   editor = "Antonis C. Kakas and Fariba Sadri",
;   booktitle = "Computational Logic: Logic Programming and Beyond: Essays in Honour of \uppercase{R}obert \uppercase{A}. \uppercase{K}owalski, Part \uppercase{II}",
;   series = "Lecture Notes in Computer Science",
;   volume = "2408",
;   pages = "452--490",
;   address = "Berlin",
;   publisher = "Springer",
; }
;
; added:
; !HoldsAt(Forwards(), 0).
; !HoldsAt(Backwards(), 0).
; !HoldsAt(Spinning(), 0).
;

load foundations/Root.e
load foundations/EC.e

event Push()
event Pull()
fluent Forwards()
fluent Backwards()
fluent Spinning()

[time]
!Happens(Pull(), time) ->
Initiates(Push(), Forwards(), time).

[time]
!Happens(Pull(), time) ->
Terminates(Push(), Backwards(), time).

[time]
!Happens(Push(), time) ->
Initiates(Pull(), Backwards(), time).

[time]
!Happens(Push(), time) ->
Terminates(Pull(), Forwards(), time).

[time]
Happens(Push(), time) ->
Initiates(Pull(), Spinning(), time).

[time]
Happens(Push(), time) ->
Terminates(Pull(), Forwards(), time).

[time]
Happens(Push(), time) ->
Terminates(Pull(), Backwards(), time).

[time]
!Happens(Pull(), time) ->
Terminates(Push(), Spinning(), time).

[time]
!Happens(Push(), time) ->
Terminates(Pull(), Spinning(), time).

!HoldsAt(Forwards(), 0).
!HoldsAt(Backwards(), 0).
!HoldsAt(Spinning(), 0).

Happens(Push(), 5).
Happens(Pull(), 5).
Happens(Pull(), 10).
Happens(Push(), 10).

completion Happens

range time 0 12
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1997/StolenCar.e
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
; @inproceedings{Kautz:1986,
;   author = "Henry A. Kautz",
;   year = "1986",
;   title = "The Logic of Persistence",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{F}ifth \uppercase{N}ational \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
;   pages = "401--405",
;   address = "Los Altos, CA",
;   publisher = "Morgan Kaufmann",
; }
;
; \fullciteA[p. 359]{Shanahan:1997}
;
; @book{Shanahan:1997,
;   author = "Murray Shanahan",
;   year = "1997",
;   title = "Solving the Frame Problem",
;   address = "Cambridge, MA",
;   publisher = "MIT Press",
; }
;
; abduction
;
; modifications from Shanahan's formulation:
; timestamps
; added !HoldsAt(CarParked(),0).
;

load foundations/Root.e
load foundations/EC.e

event Park()
event Steal()
fluent CarParked()

[time] Initiates(Park(),CarParked(),time).
[time] Terminates(Steal(),CarParked(),time).

!HoldsAt(CarParked(),0).
Happens(Park(),0).
; ABDUCED Happens(Steal(), 1).
!HoldsAt(CarParked(),2).

range time 0 2
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/MillerShanahan2002/Bowl.e
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
; \fullciteA[p. 461]{MillerShanahan:2002}
;
; @incollection{MillerShanahan:2002,
;   author = "Rob Miller and Murray Shanahan",
;   year = "2002",
;   title = "Some alternative formulations of the event calculus",
;   editor = "Antonis C. Kakas and Fariba Sadri",
;   booktitle = "Computational Logic: Logic Programming and Beyond: Essays in Honour of \uppercase{R}obert \uppercase{A}. \uppercase{K}owalski, Part \uppercase{II}",
;   series = "Lecture Notes in Computer Science",
;   volume = "2408",
;   pages = "452--490",
;   address = "Berlin",
;   publisher = "Springer",
; }
;

load foundations/Root.e
load foundations/EC.e

event LiftLeft()
event LiftRight()
fluent Spilt()
fluent Raised()

[time]
!Happens(LiftRight(), time) ->
Initiates(LiftLeft(), Spilt(), time).

[time]
!Happens(LiftLeft(), time) ->
Initiates(LiftRight(), Spilt(), time).

[time]
Happens(LiftLeft(), time) ->
Initiates(LiftRight(), Raised(), time).

!HoldsAt(Spilt(), 0).
!HoldsAt(Raised(), 0).
Happens(LiftLeft(), 2).
Happens(LiftRight(), 2).

completion Happens

range time 0 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/ReiterCriscuolo1981/NixonDiamond1.e
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
; conflicting defaults: showing that inconsistency results
; without a cancellation rule
; \fullciteA[p. 274]{ReiterCriscuolo:1981}
; \fullciteA[pp. 98--99]{McCarthy:1986}
;
; @inproceedings{ReiterCriscuolo:1981,
;   author = "Raymond Reiter and Giovanni Criscuolo",
;   year = "1981",
;   title = "On interacting defaults",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventh \uppercase{I}nternational \uppercase{J}oint \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
;   volume = "1",
;   pages = "270--276",
;   address = "Los Altos, CA",
;   publisher = "William Kaufmann",
; }
;
; @article{McCarthy:1986,
;   author = "John McCarthy",
;   year = "1986",
;   title = "Applications of circumscription to formalizing common-sense knowledge",
;   journal = "Artificial Intelligence",
;   volume = "28",
;   pages = "89--116".
; }
;

load foundations/Root.e
load foundations/EC.e

sort x

predicate Republican(x)
predicate Quaker(x)
predicate Pacifist(x)
predicate Ab1(x)
predicate Ab2(x)

x John

Republican(John).
Quaker(John).

[x] Republican(x) & !Ab1(x) -> !Pacifist(x).
[x] Quaker(x) & !Ab2(x) -> Pacifist(x).

range time 0 0
range offset 1 1

completion Theta Ab1
completion Theta Ab2

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/ReiterCriscuolo1981/NixonDiamond2.e
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
; conflicting defaults: method (D)
; \fullciteA[p. 274]{ReiterCriscuolo:1981}
; \fullciteA[pp. 98--99]{McCarthy:1986}
; \fullciteA[p. 18]{BrewkaDixKonolige:1997}
;
; @inproceedings{ReiterCriscuolo:1981,
;   author = "Raymond Reiter and Giovanni Criscuolo",
;   year = "1981",
;   title = "On interacting defaults",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventh \uppercase{I}nternational \uppercase{J}oint \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
;   volume = "1",
;   pages = "270--276",
;   address = "Los Altos, CA",
;   publisher = "William Kaufmann",
; }
;
; @article{McCarthy:1986,
;   author = "John McCarthy",
;   year = "1986",
;   title = "Applications of circumscription to formalizing common-sense knowledge",
;   journal = "Artificial Intelligence",
;   volume = "28",
;   pages = "89--116".
; }
;
; @book{BrewkaDixKonolige:1997,
;   author = "Gerhard Brewka and J{\"{u}}rgen Dix and Kurt Konolige",
;   year = "1997",
;   title = "Nonmonotonic Reasoning: An Overview",
;   address = "Stanford, CA",
;   publisher = "CSLI",
; }
;

load foundations/Root.e
load foundations/EC.e

sort x

predicate Republican(x)
predicate Quaker(x)
predicate Pacifist(x)
predicate Ab1(x)
predicate Ab2(x)

x John

Republican(John).
Quaker(John).

[x] Republican(x) & !Ab1(x) -> !Pacifist(x).
[x] Quaker(x) & !Ab2(x) -> Pacifist(x).
Theta: [x] Republican(x) -> Ab2(x).

range time 0 0
range offset 1 1

completion Theta Ab1
completion Theta Ab2

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter2/Sleep2.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

agent Nathan

fluent Awake(agent)

event WakeUp(agent)
event FallAsleep(agent)

; Sigma

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).
[agent,time] Terminates(FallAsleep(agent),Awake(agent),time).

; Gamma

!HoldsAt(Awake(Nathan),0).
HoldsAt(Awake(Nathan),1).

; abduced:
; Happens(WakeUp(Nathan),0).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter2/Sleep1.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

agent Nathan

fluent Awake(agent)

event WakeUp(agent)
event FallAsleep(agent)

; Sigma

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).
[agent,time] Terminates(FallAsleep(agent),Awake(agent),time).

; Delta

Happens(WakeUp(Nathan),1).

; Gamma

!HoldsAt(Awake(Nathan),0).

; entailed:
; HoldsAt(Awake(Nathan),3).

completion Happens

range time 0 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter2/Sleep3.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

agent Nathan

fluent Awake(agent)

event WakeUp(agent)
event FallAsleep(agent)

; Sigma

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).
[agent,time] Terminates(FallAsleep(agent),Awake(agent),time).

; Delta

[agent,time]
Happens(WakeUp(agent),time) ->
!HoldsAt(Awake(agent),time).

Happens(WakeUp(Nathan),0).

; Gamma

HoldsAt(Awake(Nathan),1).

; inferred:
; !HoldsAt(Awake(Nathan),0).

completion Happens

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter2/Inconsistency3.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
object O1

fluent F(object)

event E(object)

[object,time] Releases(E(object),F(object),time).
[object,time] Terminates(E(object),F(object),time).

Happens(E(O1),0).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter2/Sleep4.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

agent Nathan

fluent Awake(agent)

event WakeUp(agent)
event FallAsleep(agent)

; Sigma

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).
[agent,time] Terminates(FallAsleep(agent),Awake(agent),time).

; Delta

Happens(WakeUp(Nathan),1).

; entailed:
; HoldsAt(Awake(Nathan),3).

completion Happens

range time 0 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter2/Inconsistency4.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
object O1

event E(object)

fluent F1(object)
fluent F2(object)

[object,time]
Initiates(E(object),F1(object),time).

[object,time]
HoldsAt(F1(object),time) <-> HoldsAt(F2(object),time).

!HoldsAt(F2(O1),0).
Happens(E(O1),0).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter2/Inconsistency1.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
object O1

fluent F(object)

event E(object)

[object,time] Initiates(E(object),F(object),time).
[object,time] Terminates(E(object),F(object),time).

Happens(E(O1),0).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter2/Inconsistency2.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
object O1

fluent F(object)

event E(object)

[object,time] Releases(E(object),F(object),time).
[object,time] Initiates(E(object),F(object),time).

Happens(E(O1),0).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter8/CameraWithFlash.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort camera

camera Camera1

fluent ProperlyExposedPicture(camera)
fluent ImproperlyExposedPicture(camera)

event ReleaseShutter(camera)
event TriggerFlash(camera)

; Sigma

[camera,time]
Happens(TriggerFlash(camera),time) ->
Initiates(ReleaseShutter(camera),ProperlyExposedPicture(camera),time).

[camera,time]
!Happens(TriggerFlash(camera),time) ->
Initiates(ReleaseShutter(camera),ImproperlyExposedPicture(camera),time).

; Delta

Delta: Happens(ReleaseShutter(Camera1),0).
Delta: Happens(TriggerFlash(Camera1),1).
Delta: Happens(ReleaseShutter(Camera1),1).

; added:
[camera] !HoldsAt(ImproperlyExposedPicture(camera),0).
[camera] !HoldsAt(ProperlyExposedPicture(camera),0).

completion Delta Happens

range time 0 2
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter8/MovingRobot.e
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
; @inproceedings{Shanahan:1996,
;   author = "Murray Shanahan",
;   year = "1996",
;   title = "Robotics and the common sense informatic situation",
;   editor = "Wolfgang Wahlster",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{T}welfth \uppercase{E}uropean \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
;   pages = "684--688",
;   address = "Chichester, UK",
;   publisher = "John Wiley",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option renaming off

load foundations/Root.e
load foundations/EC.e

sort coord: integer

sort direction: integer
; 0 -> 0, 1 -> 90, 2 -> 180, 3 -> 370

sort robot

robot Robot1

function Sin(direction): coord
function Cos(direction): coord

Sin(0)=0.
Sin(1)=1.
Sin(2)=2.
Sin(3)=3.

Cos(0)=1.
Cos(1)=2.
Cos(2)=3.
Cos(3)=4.

fluent Direction(robot,direction)
fluent Location(robot,coord,coord)

event MoveLeftWheel(robot)
event MoveRightWheel(robot)

; Sigma

[robot,direction1,direction2,time]
!Happens(MoveRightWheel(robot),time) &
HoldsAt(Direction(robot,direction1),time) &
direction2 = (direction1-1)->
Initiates(MoveLeftWheel(robot),Direction(robot,direction2),time).

[robot,direction,time]
!Happens(MoveRightWheel(robot),time) &
HoldsAt(Direction(robot,direction),time) ->
Terminates(MoveLeftWheel(robot),Direction(robot,direction),time).

[robot,direction1,direction2,time]
!Happens(MoveLeftWheel(robot),time) &
HoldsAt(Direction(robot,direction1),time) &
direction2 = (direction1+1)->
Initiates(MoveRightWheel(robot),Direction(robot,direction2),time).

[robot,direction,time]
!Happens(MoveLeftWheel(robot),time) &
HoldsAt(Direction(robot,direction),time) ->
Terminates(MoveRightWheel(robot),Direction(robot,direction),time).

[robot,direction,coord1,coord2,coord3,coord4,time]
Happens(MoveLeftWheel(robot),time) &
HoldsAt(Location(robot,coord1,coord2),time) &
HoldsAt(Direction(robot,direction),time) &
coord3 = coord1+Cos(direction) &
coord4 = coord2+Sin(direction) ->
Initiates(MoveRightWheel(robot),
          Location(robot,coord3,coord4),
          time).

[robot,coord1,coord2,time]
Happens(MoveLeftWheel(robot),time) &
HoldsAt(Location(robot,coord1,coord2),time) ->
; FIX: Direction not needed!!
; HoldsAt(Direction(robot,direction),time) ->
Terminates(MoveRightWheel(robot),Location(robot,coord1,coord2),time).

; Delta

Happens(MoveRightWheel(Robot1),0).
Happens(MoveLeftWheel(Robot1),1).
Happens(MoveRightWheel(Robot1),1).

; Psi


[robot,coord1,coord2,coord3,coord4,time]
HoldsAt(Location(robot,coord1,coord2),time) &
HoldsAt(Location(robot,coord3,coord4),time) ->
coord1=coord3 &
coord2=coord4.

[robot,direction1,direction2,time]
HoldsAt(Direction(robot,direction1),time) &
HoldsAt(Direction(robot,direction2),time) ->
direction1=direction2.

; Gamma

HoldsAt(Location(Robot1,0,0),0).
HoldsAt(Direction(Robot1,0),0).

completion Happens

range time 0 3
range coord 0 3
range direction 0 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter8/PatHeadRubStomach.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

event PatHead(agent)
event RubStomach(agent)

agent Nathan

; Delta

[agent,time]
Happens(PatHead(agent),time) ->
!Happens(RubStomach(agent),time).

Happens(PatHead(Nathan),0) & Happens(RubStomach(Nathan),0).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter10/MovingNewspaperAndBox.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
sort agent: object
sort physobj: object
sort room: object

fluent IN(object,object)
fluent INROOM(object,room)
noninertial INROOM

event MOVE(agent,object,object,object)

agent Lisa
physobj Box, Newspaper
room Kitchen, LivingRoom

; Sigma

; RS10
[agent,physobj1,physobj2,room,time]
HoldsAt(IN(agent,room),time) &
HoldsAt(IN(physobj1,room),time) &
HoldsAt(INROOM(physobj2,room),time) ->
Initiates(MOVE(agent,physobj1,room,physobj2),IN(physobj1,physobj2),time).

; RS11
[agent,physobj1,physobj2,room,time]
HoldsAt(IN(agent,room),time) &
HoldsAt(IN(physobj1,room),time) &
HoldsAt(INROOM(physobj2,room),time) ->
Terminates(MOVE(agent,physobj1,room,physobj2),IN(physobj1,room),time).

; RS12
[agent,physobj1,physobj2,room,time]
HoldsAt(IN(agent,room),time) ->
Initiates(MOVE(agent,physobj1,physobj2,room),IN(physobj1,room),time).

; RS13
[agent,physobj1,physobj2,room,time]
HoldsAt(IN(agent,room),time) ->
Terminates(MOVE(agent,physobj1,physobj2,room),IN(physobj1,physobj2),time).

; RS14
[agent,room1,room2,time]
HoldsAt(IN(agent,room1),time) ->
Initiates(MOVE(agent,agent,room1,room2),IN(agent,room2),time).

; RS15
[agent,room1,room2,time]
HoldsAt(IN(agent,room1),time) ->
Terminates(MOVE(agent,agent,room1,room2),IN(agent,room1),time).

; RS16
[agent,physobj,room,time]
HoldsAt(IN(agent,room),time) &
HoldsAt(IN(physobj,room),time) ->
Initiates(MOVE(agent,physobj,room,agent),IN(physobj,agent),time).

; RS17
[agent,physobj,room,time]
HoldsAt(IN(agent,room),time) &
HoldsAt(IN(physobj,room),time) ->
Terminates(MOVE(agent,physobj,room,agent),IN(physobj,room),time).

; RS18
[agent,physobj,room,time]
HoldsAt(IN(physobj,agent),time) &
HoldsAt(IN(agent,room),time) ->
Initiates(MOVE(agent,physobj,agent,room),IN(physobj,room),time).

; RS19
[agent,physobj,room,time]
HoldsAt(IN(physobj,agent),time) &
HoldsAt(IN(agent,room),time) ->
Terminates(MOVE(agent,physobj,agent,room),IN(physobj,agent),time).

; Delta

Happens(MOVE(Lisa,Newspaper,LivingRoom,Box),0).
Happens(MOVE(Lisa,Box,LivingRoom,Lisa),1).
Happens(MOVE(Lisa,Lisa,LivingRoom,Kitchen),2).
Happens(MOVE(Lisa,Box,Lisa,Kitchen),3).
Happens(MOVE(Lisa,Lisa,Kitchen,LivingRoom),4).

; Psi

; RS1
[object,time] !HoldsAt(IN(object,object),time).

; RS2
[object1,object2,time]
HoldsAt(IN(object1,object2),time) ->
!HoldsAt(IN(object2,object1),time).

; RS3
[object1,object2,object3,time]
HoldsAt(IN(object1,object2),time) &
HoldsAt(IN(object2,object3),time) ->
!HoldsAt(IN(object1,object3),time).

; RS4
[object,object1,object2,time]
HoldsAt(IN(object,object1),time) &
HoldsAt(IN(object,object2),time) ->
object1=object2.

; RS7
[object,room,time]
HoldsAt(IN(object,room),time) ->
HoldsAt(INROOM(object,room),time).

; RS8
[object1,object2,room,time]
HoldsAt(IN(object1,object2),time) &
HoldsAt(INROOM(object2,room),time) ->
HoldsAt(INROOM(object1,room),time).

; RS9
[object,room1,room2,time]
HoldsAt(INROOM(object,room1),time) &
HoldsAt(INROOM(object,room2),time) ->
room1=room2.

; Gamma

HoldsAt(IN(Lisa,LivingRoom),0).
HoldsAt(IN(Newspaper,LivingRoom),0).
HoldsAt(IN(Box,LivingRoom),0).

; added:
[room1,room2,time] !HoldsAt(INROOM(room1,room2),time).
[room,object,time] !HoldsAt(IN(room,object),time).

; entailed:
; HoldsAt(IN(Lisa,LivingRoom),5).
; HoldsAt(IN(Box,Kitchen),5).
; HoldsAt(INROOM(Newspaper,Kitchen),5).

completion Happens

range time 0 5
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter10/TwoScreens.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
sort location

object O1, O2
location L1, L2, L3, L4, L5

predicate Adjacent(location,location)
predicate Equal(object,object)

fluent At(object,location)
event Move(object,location,location)

; Sigma

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
Adjacent(location1,location2) ->
Initiates(Move(object,location1,location2),At(object,location2),time).

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
Adjacent(location1,location2) ->
Terminates(Move(object,location1,location2),At(object,location1),time).

; Psi

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
HoldsAt(At(object,location2),time) ->
location1=location2.

[object,time]
{location} HoldsAt(At(object,location),time).

[object1,object2,location,time]
HoldsAt(At(object1,location),time) &
HoldsAt(At(object2,location),time) ->
Equal(object1,object2).

[location1, location2]
Adjacent(location1,location2) <->
Adjacent(location2,location1).

[object1,object2]
Equal(object1,object2) <->
Equal(object2,object1).

; Gamma

[location1,location2]
Adjacent(location1,location2) <->
(location1=L1 & location2=L2) |
(location1=L2 & location2=L1) |
(location1=L2 & location2=L3) |
(location1=L3 & location2=L2) |
(location1=L3 & location2=L4) |
(location1=L4 & location2=L3) |
(location1=L4 & location2=L5) |
(location1=L5 & location2=L4).

HoldsAt(At(O1,L1),0).
[object] !HoldsAt(At(object,L5),0).

HoldsAt(At(O2,L5),4).
[object] !HoldsAt(At(object,L1),4).

[object,time] !HoldsAt(At(object,L3),time).

; ADDED:
[object,location1,location2,time]
Happens(Move(object,location1,location2),time) ->
HoldsAt(At(object,location1),time) &
Adjacent(location1,location2).

[object1,object2,location1,location2,time]
Equal(object1,object2) &
Happens(Move(object1,location1,location2),time) ->
Happens(Move(object2,location1,location2),time).

; entailed: !Equal(O1,O2).

range time 0 4
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter10/OneScreen.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
sort location

object O1, O2
location L1, L2, L3

predicate Adjacent(location,location)
predicate Equal(object,object)

fluent At(object,location)
event Move(object,location,location)

; Sigma

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
Adjacent(location1,location2) ->
Initiates(Move(object,location1,location2),At(object,location2),time).

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
Adjacent(location1,location2) ->
Terminates(Move(object,location1,location2),At(object,location1),time).

; Psi

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
HoldsAt(At(object,location2),time) ->
location1=location2.

[object,time]
{location} HoldsAt(At(object,location),time).

[object1,object2,location,time]
HoldsAt(At(object1,location),time) &
HoldsAt(At(object2,location),time) ->
Equal(object1,object2).

[location1, location2]
Adjacent(location1,location2) <->
Adjacent(location2,location1).

[object1,object2]
Equal(object1,object2) <->
Equal(object2,object1).

; Gamma

[location1,location2]
Adjacent(location1,location2) <->
(location1=L1 & location2=L2) |
(location1=L2 & location2=L1) |
(location1=L2 & location2=L3) |
(location1=L3 & location2=L2).

HoldsAt(At(O1,L1),0).
[object] !HoldsAt(At(object,L3),0).

[object] !HoldsAt(At(object,L1),1).
[object] !HoldsAt(At(object,L3),1).

HoldsAt(At(O2,L3),2).
[object] !HoldsAt(At(object,L1),2).

; ADDED:
[object,location1,location2,time]
Happens(Move(object,location1,location2),time) ->
HoldsAt(At(object,location1),time) &
Adjacent(location1,location2).

[object1,object2,location1,location2,time]
Equal(object1,object2) &
Happens(Move(object1,location1,location2),time) ->
Happens(Move(object2,location1,location2),time).

range time 0 2
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter9/RunningAndDriving.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort location

agent James
location Bookstore

fluent Tired(agent)

event Go(agent,location)
event Run(agent,location)
event Drive(agent,location)

[agent,location,time]
Happens(Go(agent,location),time) ->
Happens(Run(agent,location),time) | Happens(Drive(agent,location),time).

xor Run, Drive

[agent,location,time] Initiates(Run(agent,location),Tired(agent),time).

!HoldsAt(Tired(James),0).
Happens(Go(James,Bookstore),0).
HoldsAt(Tired(James),1).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter9/RouletteWheel.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort dealer
sort wheel
sort value: integer

wheel Wheel1
dealer Dealer1

fluent WheelNumberDeterminer(wheel,value)
fluent WheelNumber(wheel,value)
noninertial WheelNumberDeterminer

event Spin(dealer,wheel)
event Reset(dealer,wheel)

[wheel,time]
{value}
HoldsAt(WheelNumberDeterminer(wheel,value),time).

[wheel,value1,value2,time]
HoldsAt(WheelNumberDeterminer(wheel,value1),time) &
HoldsAt(WheelNumberDeterminer(wheel,value2),time) ->
value1=value2.

[dealer,wheel,value,time]
HoldsAt(WheelNumberDeterminer(wheel,value),time) ->
Initiates(Spin(dealer,wheel),WheelNumber(wheel,value),time).

[dealer,wheel,value1,value2,time]
HoldsAt(WheelNumber(wheel,value1),time) &
HoldsAt(WheelNumberDeterminer(wheel,value2),time) &
value1!=value2 ->
Terminates(Spin(dealer,wheel),WheelNumber(wheel,value1),time).

[dealer,wheel,value,time]
Terminates(Reset(dealer,wheel),WheelNumber(wheel,value),time).

[wheel,value1,value2,time]
HoldsAt(WheelNumber(wheel,value1),time) &
HoldsAt(WheelNumber(wheel,value2),time) ->
value1=value2.

[value] !HoldsAt(WheelNumber(Wheel1,value),0).

Happens(Spin(Dealer1,Wheel1),0).
;Happens(Reset(Dealer1,Wheel1),1).

; added to prune models
HoldsAt(WheelNumberDeterminer(Wheel1, 1),1).

completion Happens

range value 1 3
range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter14/NetBill1.e
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
; @inproceedings{SirbuTygar:1995,
;   author = "Marvin A. Sirbu and J. D. Tygar",
;   year = "1995",
;   title = "Net\uppercase{B}ill: An \uppercase{I}nternet commerce system optimized for network delivered services",
;   editor = "
;   booktitle = "40th \uppercase{IEEE} \uppercase{C}omputer \uppercase{S}ociety \uppercase{I}nternational \uppercase{C}onference",
;   pages = "20--25",
;   publisher = "
;   address = "
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort agent
agent MusicStore, Jen

sort product
product BritneyCD

sort f
f PurchaseRequestedJenMusicStoreBritneyCD1
f DeliveredMusicStoreJenBritneyCD
f EPOSentJenMusicStore1

sort amount: integer

fluent C(agent,agent,f)
fluent CC(agent,agent,f,f)

event CreateC(agent,agent,f)
event CreateCC(agent,agent,f,f)
event DischargeC(agent,agent,f)
event DischargeCC(agent,agent,f,f)

fluent QuoteSent(agent,agent,product,amount)
fluent PurchaseRequested(agent,agent,product,amount)
fluent Delivered(agent,agent,product)
fluent EPOSent(agent,agent,amount)

event SendQuote(agent,agent,product,amount)
event RequestPurchase(agent,agent,product,amount)
event Deliver(agent,agent,product)
event SendEPO(agent,agent,amount)

; Sigma

[agent1,agent2,f,time]
Initiates(CreateC(agent1,agent2,f),C(agent1,agent2,f),time).

[agent1,agent2,f1,f2,time]
Initiates(CreateCC(agent1,agent2,f1,f2),CC(agent1,agent2,f1,f2),time).

[agent1,agent2,f,time]
Terminates(DischargeC(agent1,agent2,f),C(agent1,agent2,f),time).

[agent1,agent2,f1,f2,time]
Terminates(DischargeCC(agent1,agent2,f1,f2),CC(agent1,agent2,f1,f2),time).

[agent1,agent2,product,amount,time]
Initiates(SendQuote(agent1,agent2,product,amount),
          QuoteSent(agent1,agent2,product,amount),
          time).

[agent1,agent2,product,amount,time]
Initiates(RequestPurchase(agent1,agent2,product,amount),
          PurchaseRequested(agent1,agent2,product,amount),
          time).

[agent1,agent2,product,time]
Initiates(Deliver(agent1,agent2,product),
          Delivered(agent1,agent2,product),
          time).

[agent1,agent2,amount,time]
Initiates(SendEPO(agent1,agent2,amount),
          EPOSent(agent1,agent2,amount),
          time).

[agent1,agent2,product,amount,f1,f2,time]
agent1=MusicStore &
agent2=Jen &
product=BritneyCD &
amount=1 &
f1=PurchaseRequestedJenMusicStoreBritneyCD1 &
f2=DeliveredMusicStoreJenBritneyCD ->
Initiates(SendQuote(agent1,agent2,product,amount),
          CC(agent1,agent2,f1,f2),
          time).

[agent1,agent2,product,amount,f1,f2,time]
agent1=Jen &
agent2=MusicStore &
product=BritneyCD &
amount=1 &
f1=DeliveredMusicStoreJenBritneyCD &
f2=EPOSentJenMusicStore1 &
!HoldsAt(Delivered(agent2,agent1,product),time) ->
Initiates(RequestPurchase(agent1,agent2,product,amount),
          CC(agent1,agent2,f1,f2),
          time).

; Delta

Delta: [time]
HoldsAt(CC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(PurchaseRequested(Jen,MusicStore,BritneyCD,1),time) ->
Happens(CreateC(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(CC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(PurchaseRequested(Jen, MusicStore, BritneyCD, 1),time) ->
Happens(DischargeCC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(CC(Jen, MusicStore, DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(CreateC(Jen,MusicStore,EPOSentJenMusicStore1),time).

Delta: [time]
HoldsAt(CC(Jen, MusicStore, DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(DischargeCC(Jen,MusicStore,DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time).

Delta: [time]
HoldsAt(C(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(DischargeC(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(C(Jen,MusicStore,EPOSentJenMusicStore1),time) &
HoldsAt(EPOSent(Jen,MusicStore,1),time) ->
Happens(DischargeC(Jen,MusicStore,EPOSentJenMusicStore1),time).

Delta: Happens(SendQuote(MusicStore,Jen,BritneyCD,1),0).
Delta: Happens(RequestPurchase(Jen,MusicStore,BritneyCD,1),1).
Delta: Happens(Deliver(MusicStore,Jen,BritneyCD),3).
Delta: Happens(SendEPO(Jen,MusicStore,1),5).

; Gamma

[agent1,agent2,product,amount]
!HoldsAt(QuoteSent(agent1,agent2,product,amount),0).

[agent1,agent2,product,amount]
!HoldsAt(PurchaseRequested(agent1,agent2,product,amount),0).

[agent1,agent2,product]
!HoldsAt(Delivered(agent1,agent2,product),0).

[agent1,agent2,f]
!HoldsAt(C(agent1,agent2,f),0).

[agent1,agent2,f1,f2]
!HoldsAt(CC(agent1,agent2,f1,f2),0).

[agent1,agent2,amount]
!HoldsAt(EPOSent(agent1,agent2,amount),0).

completion Delta Happens

range time 0 7
range offset 1 1
range amount 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter14/NetBill3.e
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
; @inproceedings{SirbuTygar:1995,
;   author = "Marvin A. Sirbu and J. D. Tygar",
;   year = "1995",
;   title = "Net\uppercase{B}ill: An \uppercase{I}nternet commerce system optimized for network delivered services",
;   editor = "
;   booktitle = "40th \uppercase{IEEE} \uppercase{C}omputer \uppercase{S}ociety \uppercase{I}nternational \uppercase{C}onference",
;   pages = "20--25",
;   publisher = "
;   address = "
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort agent
agent MusicStore, Jen

sort product
product BritneyCD

sort f
f PurchaseRequestedJenMusicStoreBritneyCD1
f DeliveredMusicStoreJenBritneyCD
f EPOSentJenMusicStore1

sort amount: integer

fluent C(agent,agent,f)
fluent CC(agent,agent,f,f)

event CreateC(agent,agent,f)
event CreateCC(agent,agent,f,f)
event DischargeC(agent,agent,f)
event DischargeCC(agent,agent,f,f)

fluent QuoteSent(agent,agent,product,amount)
fluent PurchaseRequested(agent,agent,product,amount)
fluent Delivered(agent,agent,product)
fluent EPOSent(agent,agent,amount)

event SendQuote(agent,agent,product,amount)
event RequestPurchase(agent,agent,product,amount)
event Deliver(agent,agent,product)
event SendEPO(agent,agent,amount)

; Sigma

[agent1,agent2,f,time]
Initiates(CreateC(agent1,agent2,f),C(agent1,agent2,f),time).

[agent1,agent2,f1,f2,time]
Initiates(CreateCC(agent1,agent2,f1,f2),CC(agent1,agent2,f1,f2),time).

[agent1,agent2,f,time]
Terminates(DischargeC(agent1,agent2,f),C(agent1,agent2,f),time).

[agent1,agent2,f1,f2,time]
Terminates(DischargeCC(agent1,agent2,f1,f2),CC(agent1,agent2,f1,f2),time).

[agent1,agent2,product,amount,time]
Initiates(SendQuote(agent1,agent2,product,amount),
          QuoteSent(agent1,agent2,product,amount),
          time).

[agent1,agent2,product,amount,time]
Initiates(RequestPurchase(agent1,agent2,product,amount),
          PurchaseRequested(agent1,agent2,product,amount),
          time).

[agent1,agent2,product,time]
Initiates(Deliver(agent1,agent2,product),
          Delivered(agent1,agent2,product),
          time).

[agent1,agent2,amount,time]
Initiates(SendEPO(agent1,agent2,amount),
          EPOSent(agent1,agent2,amount),
          time).

[agent1,agent2,product,amount,f1,f2,time]
agent1=MusicStore &
agent2=Jen &
product=BritneyCD &
amount=1 &
f1=PurchaseRequestedJenMusicStoreBritneyCD1 &
f2=DeliveredMusicStoreJenBritneyCD ->
Initiates(SendQuote(agent1,agent2,product,amount),
          CC(agent1,agent2,f1,f2),
          time).

[agent1,agent2,product,amount,f1,f2,time]
agent1=Jen &
agent2=MusicStore &
product=BritneyCD &
amount=1 &
f1=DeliveredMusicStoreJenBritneyCD &
f2=EPOSentJenMusicStore1 &
!HoldsAt(Delivered(agent2,agent1,product),time) ->
Initiates(RequestPurchase(agent1,agent2,product,amount),
          CC(agent1,agent2,f1,f2),
          time).

; Delta

Delta: [time]
HoldsAt(CC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(PurchaseRequested(Jen,MusicStore,BritneyCD,1),time) ->
Happens(CreateC(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(CC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(PurchaseRequested(Jen, MusicStore, BritneyCD, 1),time) ->
Happens(DischargeCC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(CC(Jen, MusicStore, DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(CreateC(Jen,MusicStore,EPOSentJenMusicStore1),time).

Delta: [time]
HoldsAt(CC(Jen, MusicStore, DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(DischargeCC(Jen,MusicStore,DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time).

Delta: [time]
HoldsAt(C(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(DischargeC(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(C(Jen,MusicStore,EPOSentJenMusicStore1),time) &
HoldsAt(EPOSent(Jen,MusicStore,1),time) ->
Happens(DischargeC(Jen,MusicStore,EPOSentJenMusicStore1),time).

Delta: Happens(Deliver(MusicStore,Jen,BritneyCD),0).
Delta: Happens(SendEPO(Jen,MusicStore,1),2).

; Gamma

[agent1,agent2,product,amount]
!HoldsAt(QuoteSent(agent1,agent2,product,amount),0).

[agent1,agent2,product,amount]
!HoldsAt(PurchaseRequested(agent1,agent2,product,amount),0).

[agent1,agent2,product]
!HoldsAt(Delivered(agent1,agent2,product),0).

[agent1,agent2,f]
!HoldsAt(C(agent1,agent2,f),0).

[agent1,agent2,f1,f2]
!HoldsAt(CC(agent1,agent2,f1,f2),0).

[agent1,agent2,amount]
!HoldsAt(EPOSent(agent1,agent2,amount),0).

completion Delta Happens

range time 0 4
range offset 1 1
range amount 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter14/NetBill2.e
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
; @inproceedings{SirbuTygar:1995,
;   author = "Marvin A. Sirbu and J. D. Tygar",
;   year = "1995",
;   title = "Net\uppercase{B}ill: An \uppercase{I}nternet commerce system optimized for network delivered services",
;   editor = "
;   booktitle = "40th \uppercase{IEEE} \uppercase{C}omputer \uppercase{S}ociety \uppercase{I}nternational \uppercase{C}onference",
;   pages = "20--25",
;   publisher = "
;   address = "
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort agent
agent MusicStore, Jen

sort product
product BritneyCD

sort f
f PurchaseRequestedJenMusicStoreBritneyCD1
f DeliveredMusicStoreJenBritneyCD
f EPOSentJenMusicStore1

sort amount: integer

fluent C(agent,agent,f)
fluent CC(agent,agent,f,f)

event CreateC(agent,agent,f)
event CreateCC(agent,agent,f,f)
event DischargeC(agent,agent,f)
event DischargeCC(agent,agent,f,f)

fluent QuoteSent(agent,agent,product,amount)
fluent PurchaseRequested(agent,agent,product,amount)
fluent Delivered(agent,agent,product)
fluent EPOSent(agent,agent,amount)

event SendQuote(agent,agent,product,amount)
event RequestPurchase(agent,agent,product,amount)
event Deliver(agent,agent,product)
event SendEPO(agent,agent,amount)

; Sigma

[agent1,agent2,f,time]
Initiates(CreateC(agent1,agent2,f),C(agent1,agent2,f),time).

[agent1,agent2,f1,f2,time]
Initiates(CreateCC(agent1,agent2,f1,f2),CC(agent1,agent2,f1,f2),time).

[agent1,agent2,f,time]
Terminates(DischargeC(agent1,agent2,f),C(agent1,agent2,f),time).

[agent1,agent2,f1,f2,time]
Terminates(DischargeCC(agent1,agent2,f1,f2),CC(agent1,agent2,f1,f2),time).

[agent1,agent2,product,amount,time]
Initiates(SendQuote(agent1,agent2,product,amount),
          QuoteSent(agent1,agent2,product,amount),
          time).

[agent1,agent2,product,amount,time]
Initiates(RequestPurchase(agent1,agent2,product,amount),
          PurchaseRequested(agent1,agent2,product,amount),
          time).

[agent1,agent2,product,time]
Initiates(Deliver(agent1,agent2,product),
          Delivered(agent1,agent2,product),
          time).

[agent1,agent2,amount,time]
Initiates(SendEPO(agent1,agent2,amount),
          EPOSent(agent1,agent2,amount),
          time).

[agent1,agent2,product,amount,f1,f2,time]
agent1=MusicStore &
agent2=Jen &
product=BritneyCD &
amount=1 &
f1=PurchaseRequestedJenMusicStoreBritneyCD1 &
f2=DeliveredMusicStoreJenBritneyCD ->
Initiates(SendQuote(agent1,agent2,product,amount),
          CC(agent1,agent2,f1,f2),
          time).

[agent1,agent2,product,amount,f1,f2,time]
agent1=Jen &
agent2=MusicStore &
product=BritneyCD &
amount=1 &
f1=DeliveredMusicStoreJenBritneyCD &
f2=EPOSentJenMusicStore1 &
!HoldsAt(Delivered(agent2,agent1,product),time) ->
Initiates(RequestPurchase(agent1,agent2,product,amount),
          CC(agent1,agent2,f1,f2),
          time).

; Delta

Delta: [time]
HoldsAt(CC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(PurchaseRequested(Jen,MusicStore,BritneyCD,1),time) ->
Happens(CreateC(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(CC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(PurchaseRequested(Jen, MusicStore, BritneyCD, 1),time) ->
Happens(DischargeCC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(CC(Jen, MusicStore, DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(CreateC(Jen,MusicStore,EPOSentJenMusicStore1),time).

Delta: [time]
HoldsAt(CC(Jen, MusicStore, DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(DischargeCC(Jen,MusicStore,DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time).

Delta: [time]
HoldsAt(C(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time) &
HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
Happens(DischargeC(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time).

Delta: [time]
HoldsAt(C(Jen,MusicStore,EPOSentJenMusicStore1),time) &
HoldsAt(EPOSent(Jen,MusicStore,1),time) ->
Happens(DischargeC(Jen,MusicStore,EPOSentJenMusicStore1),time).

Delta: Happens(RequestPurchase(Jen,MusicStore,BritneyCD,1),0).
Delta: Happens(Deliver(MusicStore,Jen,BritneyCD),2).
Delta: Happens(SendEPO(Jen,MusicStore,1),4).

; Gamma

[agent1,agent2,product,amount]
!HoldsAt(QuoteSent(agent1,agent2,product,amount),0).

[agent1,agent2,product,amount]
!HoldsAt(PurchaseRequested(agent1,agent2,product,amount),0).

[agent1,agent2,product]
!HoldsAt(Delivered(agent1,agent2,product),0).

[agent1,agent2,f]
!HoldsAt(C(agent1,agent2,f),0).

[agent1,agent2,f1,f2]
!HoldsAt(CC(agent1,agent2,f1,f2),0).

[agent1,agent2,amount]
!HoldsAt(EPOSent(agent1,agent2,amount),0).

completion Delta Happens

range time 0 6
range offset 1 1
range amount 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter14/Vision.e
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
; @inproceedings{ShanahanRandell:2004,
;   author = "Murray Shanahan and David A. Randell",
;   year = "2004",
;   title = "A logic-based formulation of active visual perception",
;   editor = "Didier Dubois and Christopher A. Welty and Mary-Anne Williams",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{N}inth \uppercase{I}nternational \uppercase{C}onference on \uppercase{P}rinciples of \uppercase{K}nowledge \uppercase{R}epresentation and \uppercase{R}easoning",
;   pages = "64--72",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort object
sort shape
sort aspect

object Object1
aspect Aspect1, Aspect2, Aspect3
shape Shape1, Shape2

predicate Shape(object,shape)
predicate Arc(shape,aspect,aspect)
fluent Aspect(object,aspect)
event Change(object,aspect,aspect)

; Sigma

[object,aspect1,aspect2,shape,time]
HoldsAt(Aspect(object,aspect1),time) &
Shape(object,shape) &
(Arc(shape,aspect1,aspect2) |
 Arc(shape,aspect2,aspect1)) ->
Initiates(Change(object,aspect1,aspect2),Aspect(object,aspect2),time).

[object,aspect1,aspect2,shape,time]
HoldsAt(Aspect(object,aspect1),time) &
Shape(object,shape) &
(Arc(shape,aspect1,aspect2) |
 Arc(shape,aspect2,aspect1)) ->
Terminates(Change(object,aspect1,aspect2),Aspect(object,aspect1),time).

; preconditions (added)

[object,aspect1,aspect2,time]
Happens(Change(object,aspect1,aspect2),time) ->
HoldsAt(Aspect(object,aspect1),time).

[object,aspect1,aspect2,aspect3,time]
Happens(Change(object,aspect1,aspect2),time) &
Happens(Change(object,aspect1,aspect3),time) ->
aspect2=aspect3.

; Psi

[object,shape1,shape2]
Shape(object,shape1) &
Shape(object,shape2) ->
shape1=shape2.

[object,aspect1,aspect2,time]
HoldsAt(Aspect(object,aspect1),time) &
HoldsAt(Aspect(object,aspect2),time) ->
aspect1=aspect2.

[aspect1,aspect2]
Arc(Shape1,aspect1,aspect2) <->
(aspect1=Aspect1 & aspect2=Aspect2).

[aspect1,aspect2]
Arc(Shape2,aspect1,aspect2) <->
((aspect1=Aspect1 & aspect2=Aspect3) |
 (aspect1=Aspect3 & aspect2=Aspect2)).

; Gamma

HoldsAt(Aspect(Object1,Aspect1),0).
HoldsAt(Aspect(Object1,Aspect2),1).

;completion Delta Happens

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter14/Workflow.e
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
; @incollection{CicekliYildirim:2000,
;   author = "Nihan Kesim Cicekli and Yakup Yildirim",
;   year = "2000",
;   title = "Formalizing workflows using the event calculus",
;   editor = "Mohamed T. Ibrahim and Josef K{\"{u}}ng and Norman Revell",
;   booktitle = "Database and Expert Systems Applications",
;   series = "Lecture Notes in Computer Science",
;   volume = "1873",
;   pages = "222--231",
;   address = "Berlin",
;   publisher = "Springer",
; }
;
; @unpublished{WFMC:1999,
;   author = "{Workflow Management Coalition}",
;   year = "1999",
;   title = "\uppercase{W}orkflow \uppercase{M}anagement \uppercase{C}oalition Terminology \& Glossary",
;   howpublished = "Document Number WFMC-TC-1011, Document Status -- Issue 3.0, Workflow Management Coalition, Winchester, UK",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort activity
sort condition
activity A, B, C1, C2, C3, D, E1, E2, E3, F, G
condition E1C, E2C, E3C, FC

fluent Active(activity)
fluent Completed(activity)
fluent Condition(condition)
noninertial Condition

event Start(activity)
event End(activity)

; Sigma

[activity,time]
Initiates(Start(activity),Active(activity),time).

[activity,time]
Terminates(Start(activity),Completed(activity),time).

[activity,time]
Initiates(End(activity),Completed(activity),time).

[activity,time]
Terminates(End(activity),Active(activity),time).

; Delta

; A; B
Delta: [time]
!HoldsAt(Active(B),time) &
!HoldsAt(Completed(A),time-1) &
HoldsAt(Completed(A),time) ->
Happens(Start(B),time).

; B; AND-split C1, C2, C3
Delta: [time]
!HoldsAt(Active(C1),time) &
!HoldsAt(Completed(B),time-1) &
HoldsAt(Completed(B),time) ->
Happens(Start(C1),time).

Delta: [time]
!HoldsAt(Active(C2),time) &
!HoldsAt(Completed(B),time-1) &
HoldsAt(Completed(B),time) ->
Happens(Start(C2),time).

Delta: [time]
!HoldsAt(Active(C3),time) &
!HoldsAt(Completed(B),time-1) &
HoldsAt(Completed(B),time) ->
Happens(Start(C3),time).

; AND-join C1, C2, C3; D
Delta: [time]
!HoldsAt(Active(D),time) &
((!HoldsAt(Completed(C1),time-1) & HoldsAt(Completed(C1),time))|
 (!HoldsAt(Completed(C2),time-1) & HoldsAt(Completed(C2),time))|
 (!HoldsAt(Completed(C3),time-1) & HoldsAt(Completed(C3),time))) &
HoldsAt(Completed(C1),time) &
HoldsAt(Completed(C2),time) &
HoldsAt(Completed(C3),time) ->
Happens(Start(D),time).

; D; XOR-split E1, E2, E3
Delta: [time]
!HoldsAt(Active(E1),time) &
!HoldsAt(Completed(D),time-1) &
HoldsAt(Completed(D),time) &
HoldsAt(Condition(E1C),time) ->
Happens(Start(E1),time).

Delta: [time]
!HoldsAt(Active(E2),time) &
!HoldsAt(Completed(D),time-1) &
HoldsAt(Completed(D),time) &
HoldsAt(Condition(E2C),time) ->
Happens(Start(E2),time).

Delta: [time]
!HoldsAt(Active(E3),time) &
!HoldsAt(Completed(D),time-1) &
HoldsAt(Completed(D),time) &
HoldsAt(Condition(E3C),time) ->
Happens(Start(E3),time).

; XOR-join E1, E2, E3; F
Delta: [time]
!HoldsAt(Active(F),time) &
((!HoldsAt(Completed(E1),time-1) & HoldsAt(Completed(E1),time))|
 (!HoldsAt(Completed(E2),time-1) & HoldsAt(Completed(E2),time))|
 (!HoldsAt(Completed(E3),time-1) & HoldsAt(Completed(E3),time))) ->
Happens(Start(F),time).

; while (FC) F; G
Delta: [time]
!HoldsAt(Active(F),time) &
!HoldsAt(Completed(F),time-1) &
HoldsAt(Completed(F),time) &
HoldsAt(Condition(FC),time) ->
Happens(Start(F),time).

Delta: [time]
!HoldsAt(Active(G),time) &
!HoldsAt(Completed(F),time-1) &
HoldsAt(Completed(F),time) &
!HoldsAt(Condition(FC),time) ->
Happens(Start(G),time).

Delta: Happens(Start(A),0).
Delta: Happens(End(A),1).
Delta: Happens(End(B),3).
Delta: Happens(End(C1),5).
Delta: Happens(End(C2),6).
Delta: Happens(End(C3),7).
Delta: Happens(End(D),9).
Delta: Happens(End(E2),11).
Delta: Happens(End(F),13).
Delta: Happens(End(F),15).

; Gamma

[activity] !HoldsAt(Active(activity),0).
[activity] !HoldsAt(Completed(activity),0).
[time] time=14 <-> HoldsAt(Condition(FC),time).
[time] !HoldsAt(Condition(E1C),time).
[time] time=10 <-> HoldsAt(Condition(E2C),time).
[time] !HoldsAt(Condition(E3C),time).

completion Delta Happens

range time 0 18
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter6/ThielscherCircuit1.e
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
; @article{Thielscher:1997,
;   author = "Michael Thielscher",
;   year = "1997",
;   title = "Ramification and causality",
;   journal = "Artificial Intelligence",
;   volume = "89",
;   pages = "317--364",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e
load foundations/ECCausal.e

sort switch
sort relay
sort light

switch S1, S2, S3
relay R
light L

event Light(light)
event Close(switch)
event Open(switch)
event Activate(relay)

fluent Lit(light)
fluent Closed(switch)
fluent Activated(relay)

[time]
Stopped(Lit(L),time) &
Initiated(Closed(S1),time) &
Initiated(Closed(S2),time) ->
Happens(Light(L),time).

[time]
Started(Closed(S2),time) &
Initiated(Activated(R),time) ->
Happens(Open(S2),time).

[time]
Stopped(Activated(R),time) &
Initiated(Closed(S1),time) &
Initiated(Closed(S3),time) ->
Happens(Activate(R),time).

[switch,time] Initiates(Close(switch),Closed(switch),time).
[switch,time] Terminates(Open(switch),Closed(switch),time).
[relay,time] Initiates(Activate(relay),Activated(relay),time).
[light,time] Initiates(Light(light),Lit(light),time).

!HoldsAt(Closed(S1),0).
HoldsAt(Closed(S2),0).
HoldsAt(Closed(S3),0).
!HoldsAt(Activated(R),0).
!HoldsAt(Lit(L),0).

Happens(Close(S1),0).

completion Happens

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter6/CarryingABook1.e
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
; Example: Carrying a Book (Effect Axioms)
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort object
sort agent: object
sort room

object Book
agent Nathan
room LivingRoom, Kitchen

event LetGoOf(agent,object)
event PickUp(agent,object)
event Walk(agent,room,room)

fluent InRoom(object,room)
fluent Holding(agent,object)

; Sigma

[agent,room1,room2,time]
Initiates(Walk(agent,room1,room2),InRoom(agent,room2),time).

[agent,room1,room2,time]
room1!=room2 ->
Terminates(Walk(agent,room1,room2),InRoom(agent,room1),time).

[agent,object,room,time]
HoldsAt(InRoom(agent,room),time) &
HoldsAt(InRoom(object,room),time) ->
Initiates(PickUp(agent,object),Holding(agent,object),time).

[agent,object,time]
HoldsAt(Holding(agent,object),time) ->
Terminates(LetGoOf(agent,object),Holding(agent,object),time).

[agent,object,room1,room2,time]
HoldsAt(Holding(agent,object),time) ->
Initiates(Walk(agent,room1,room2),InRoom(object,room2),time).

[agent,object,room1,room2,time]
HoldsAt(Holding(agent,object),time) &
room1!=room2 ->
Terminates(Walk(agent,room1,room2),InRoom(object,room1),time).

; Delta

Happens(PickUp(Nathan,Book),0).
Happens(Walk(Nathan,LivingRoom,Kitchen),1).

; Psi

[object,room1,room2,time]
HoldsAt(InRoom(object,room1),time) &
HoldsAt(InRoom(object,room2),time) ->
room1=room2.

; Gamma

HoldsAt(InRoom(Nathan,LivingRoom),0).
HoldsAt(InRoom(Book,LivingRoom),0).

; added:
!HoldsAt(Holding(Nathan,Book),0).
[agent,time] !HoldsAt(Holding(agent,agent),time).

completion Happens

range time 0 2
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter6/ThielscherCircuit2.e
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
; @article{Thielscher:1997,
;   author = "Michael Thielscher",
;   year = "1997",
;   title = "Ramification and causality",
;   journal = "Artificial Intelligence",
;   volume = "89",
;   pages = "317--364",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort switch
sort relay
sort light

switch S1, S2, S3
relay R
light L

event Light(light)
event Unlight(light)
event Close(switch)
event Open(switch)
event Activate(relay)

fluent Lit(light)
fluent Closed(switch)
fluent Activated(relay)

[time]
!HoldsAt(Lit(L),time) &
HoldsAt(Closed(S1),time) &
HoldsAt(Closed(S2),time) ->
Happens(Light(L),time).

[time]
HoldsAt(Lit(L),time) &
(!HoldsAt(Closed(S1),time) | !HoldsAt(Closed(S2),time)) ->
Happens(Unlight(L),time).

[time]
HoldsAt(Closed(S2),time) &
HoldsAt(Activated(R),time) ->
Happens(Open(S2),time).

[time]
!HoldsAt(Activated(R),time) &
HoldsAt(Closed(S1),time) &
HoldsAt(Closed(S3),time) ->
Happens(Activate(R),time).

[switch,time] Initiates(Close(switch),Closed(switch),time).
[switch,time] Terminates(Open(switch),Closed(switch),time).
[relay,time] Initiates(Activate(relay),Activated(relay),time).
[light,time] Initiates(Light(light),Lit(light),time).
[light,time] Terminates(Unlight(light),Lit(light),time).

!HoldsAt(Closed(S1),0).
HoldsAt(Closed(S2),0).
HoldsAt(Closed(S3),0).
!HoldsAt(Activated(R),0).
!HoldsAt(Lit(L),0).

Happens(Close(S1),0).

completion Happens

range time 0 4
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter6/ShanahanCircuit.e
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
; @inproceedings{Shanahan:1999a,
;   author = "Murray Shanahan",
;   year = "1999",
;   title = "The ramification problem in the event calculus",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}ixteenth \uppercase{I}nternational \uppercase{J}oint \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
;   pages = "140--146",
;   address = "San Mateo, CA",
;   publisher = "Morgan Kaufmann",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort switch
sort relay
sort light

switch S1, S2, S3
relay R
light L

event Light(light)
event Unlight(light)
event Close(switch)
event Open(switch)
event Activate(relay)
event Deactivate(relay)

fluent Lit(light)
fluent Closed(switch)
fluent Activated(relay)

[time]
!HoldsAt(Lit(L),time) &
HoldsAt(Closed(S1),time) &
HoldsAt(Closed(S2),time) ->
Happens(Light(L),time).

[time]
HoldsAt(Lit(L),time) &
(!HoldsAt(Closed(S1),time) | !HoldsAt(Closed(S2),time)) ->
Happens(Unlight(L),time).

[time]
HoldsAt(Closed(S2),time) &
HoldsAt(Activated(R),time) ->
Happens(Open(S2),time).

[time]
!HoldsAt(Activated(R),time) &
HoldsAt(Closed(S1),time) &
HoldsAt(Closed(S2),time) &
HoldsAt(Closed(S3),time) ->
Happens(Activate(R),time).

[time]
HoldsAt(Activated(R),time) &
(!HoldsAt(Closed(S1),time) |
 !HoldsAt(Closed(S2),time) |
 !HoldsAt(Closed(S3),time)) ->
Happens(Deactivate(R),time).

[switch,time] Initiates(Close(switch),Closed(switch),time).
[switch,time] Terminates(Open(switch),Closed(switch),time).
[relay,time] Initiates(Activate(relay),Activated(relay),time).
[relay,time] Terminates(Deactivate(relay),Activated(relay),time).
[light,time] Initiates(Light(light),Lit(light),time).
[light,time] Terminates(Unlight(light),Lit(light),time).

!HoldsAt(Closed(S1),0).
HoldsAt(Closed(S2),0).
HoldsAt(Closed(S3),0).
!HoldsAt(Activated(R),0).
!HoldsAt(Lit(L),0).

Happens(Close(S1),0).

completion Happens

range time 0 4
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter6/CarryingABook2.e
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
; Example: Carrying a Book (Release Axioms and State Constraints)
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
sort agent: object
sort room

object Book
agent Nathan
room LivingRoom, Kitchen

event LetGoOf(agent,object)
event PickUp(agent,object)
event Walk(agent,room,room)

fluent InRoom(object,room)
fluent Holding(agent,object)

; Sigma

[agent,room1,room2,time]
Initiates(Walk(agent,room1,room2),InRoom(agent,room2),time).

[agent,room1,room2,time]
room1!=room2 ->
Terminates(Walk(agent,room1,room2),InRoom(agent,room1),time).

[agent,object,room,time]
HoldsAt(InRoom(agent,room),time) &
HoldsAt(InRoom(object,room),time) ->
Initiates(PickUp(agent,object),Holding(agent,object),time).

[agent,object,time]
HoldsAt(Holding(agent,object),time) ->
Terminates(LetGoOf(agent,object),Holding(agent,object),time).

[agent,object,room,time]
Releases(PickUp(agent,object),InRoom(object,room),time).

[agent,object,room,time]
HoldsAt(InRoom(agent,room),time) ->
Initiates(LetGoOf(agent,object),InRoom(object,room),time).

; Delta

Happens(PickUp(Nathan,Book),0).
Happens(Walk(Nathan,LivingRoom,Kitchen),1).

; Psi

[object,room1,room2,time]
HoldsAt(InRoom(object,room1),time) &
HoldsAt(InRoom(object,room2),time) ->
room1=room2.

[agent,object,room,time]
HoldsAt(Holding(agent,object),time) &
HoldsAt(InRoom(agent,room),time) ->
HoldsAt(InRoom(object,room),time).

; Gamma

HoldsAt(InRoom(Nathan,LivingRoom),0).
HoldsAt(InRoom(Book,LivingRoom),0).

; added:
!HoldsAt(Holding(Nathan,Book),0).
[agent,time] !HoldsAt(Holding(agent,agent),time).

completion Happens

range time 0 2
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter7/HotAirBalloon.e
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
; @article{MillerShanahan:1999,
;   author = "Rob Miller and Murray Shanahan",
;   year = "1999",
;   title = "The event calculus in classical logic---\uppercase{A}lternative axiomatisations",
;   journal = "Link{\"{o}}ping Electronic Articles in Computer and Information Science",
;   volume = "4",
;   number = "016",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option encoding 3
option trajectory on

load foundations/Root.e
load foundations/EC.e

sort balloon
sort agent
sort height: integer

agent Nathan
balloon Balloon

fluent HeaterOn(balloon)
fluent Height(balloon,height)
noninertial Height

event TurnOnHeater(agent,balloon)
event TurnOffHeater(agent,balloon)

; Sigma

[agent,balloon,time]
Initiates(TurnOnHeater(agent,balloon),HeaterOn(balloon),time).

[agent,balloon,time]
Terminates(TurnOffHeater(agent,balloon),HeaterOn(balloon),time).

; Delta

Delta: Happens(TurnOnHeater(Nathan,Balloon),0).
Delta: Happens(TurnOffHeater(Nathan,Balloon),2).

; Psi

[balloon,height1,height2,time]
HoldsAt(Height(balloon,height1),time) &
HoldsAt(Height(balloon,height2),time) ->
height1=height2.

; Pi

[balloon,height1,height2,offset,time]
HoldsAt(Height(balloon,height1),time) &
height2 = (height1 + offset) ->
Trajectory(HeaterOn(balloon),time,Height(balloon,height2),offset).

[balloon,height1,height2,offset,time]
HoldsAt(Height(balloon,height1),time) &
height2 = (height1 - offset) ->
AntiTrajectory(HeaterOn(balloon),time,Height(balloon,height2),offset).

; Gamma

HoldsAt(Height(Balloon,0),0).

; added:
!HoldsAt(HeaterOn(Balloon),0).

completion Delta Happens

range time 0 3
range height 0 2
range offset 1 2

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter7/FallingObjectWithEvents.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
sort agent
sort height: integer

agent Nathan
object Apple

fluent Falling(object)
fluent Height(object,height)

event Drop(agent,object)
event HitGround(object)

; Sigma

[agent,object,time]
Initiates(Drop(agent,object),Falling(object),time).

[agent,object,height,time]
Releases(Drop(agent,object),Height(object,height),time).

[object,time]
Terminates(HitGround(object),Falling(object),time).

[object,height,time]
HoldsAt(Height(object,height),time) ->
Initiates(HitGround(object),Height(object,height),time).

; Delta

Delta: [object,time]
HoldsAt(Falling(object),time) &
HoldsAt(Height(object,0),time) ->
Happens(HitGround(object),time).

Delta: Happens(Drop(Nathan,Apple),0).

; Psi

[object,height1,height2,time]
HoldsAt(Height(object,height1),time) &
HoldsAt(Height(object,height2),time) ->
height1=height2.

; Pi

[object,height1,height2,offset,time]
HoldsAt(Height(object,height1),time) &
height2 = (height1 - offset) ->
Trajectory(Falling(object),time,Height(object,height2),offset).

; Gamma

!HoldsAt(Falling(Apple),0).
HoldsAt(Height(Apple,3),0).

completion Delta Happens

range time 0 5
range height 0 3
range offset 1 3

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter7/FallingObjectWithAntiTrajectory.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option encoding 3
option trajectory on

load foundations/Root.e
load foundations/EC.e

sort object
sort agent
sort height: integer

agent Nathan
object Apple

fluent Falling(object)
fluent Height(object,height)
noninertial Height

event Drop(agent,object)
event HitGround(object)

; Sigma

[agent,object,time]
Initiates(Drop(agent,object),Falling(object),time).

[object,time]
Terminates(HitGround(object),Falling(object),time).

; Delta

Delta: [object,time]
HoldsAt(Falling(object),time) &
HoldsAt(Height(object,0),time) ->
Happens(HitGround(object),time).

Delta: Happens(Drop(Nathan,Apple),0).

; Psi

[object,height1,height2,time]
HoldsAt(Height(object,height1),time) &
HoldsAt(Height(object,height2),time) ->
height1=height2.

; Pi

[object,height1,height2,offset,time]
HoldsAt(Height(object,height1),time) &
height2 = (height1 - offset) ->
Trajectory(Falling(object),time,Height(object,height2),offset).

[object,height,offset,time]
HoldsAt(Height(object,height),time) ->
AntiTrajectory(Falling(object),time,Height(object,height),offset).

; Gamma

!HoldsAt(Falling(Apple),0).
HoldsAt(Height(Apple,3),0).

completion Delta Happens

range time 0 5
range height 0 3
range offset 1 3

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter3/Telephone2.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort phone

agent Agent1, Agent2
phone Phone1, Phone2

fluent Ringing(phone,phone)
fluent DialTone(phone)
fluent BusySignal(phone)
fluent Idle(phone)
fluent Connected(phone,phone)
fluent Disconnected(phone)

event PickUp(agent,phone)
event SetDown(agent,phone)
event Dial(agent,phone,phone)

; Sigma

[agent,phone,time]
HoldsAt(Idle(phone),time) ->
Initiates(PickUp(agent,phone),DialTone(phone),time).

[agent,phone,time]
HoldsAt(Idle(phone),time) ->
Terminates(PickUp(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(DialTone(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(DialTone(phone),time) ->
Terminates(SetDown(agent,phone),DialTone(phone),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Initiates(Dial(agent,phone1,phone2),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
!HoldsAt(Idle(phone2),time) ->
Initiates(Dial(agent,phone1,phone2),BusySignal(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
!HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).

[agent,phone,time]
HoldsAt(BusySignal(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(BusySignal(phone),time) ->
Terminates(SetDown(agent,phone),BusySignal(phone),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Terminates(SetDown(agent,phone1),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(PickUp(agent,phone2),Connected(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Terminates(PickUp(agent,phone2),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Disconnected(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Terminates(SetDown(agent,phone1),Connected(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone2),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone2),Disconnected(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Terminates(SetDown(agent,phone2),Connected(phone1,phone2),time).

[agent,phone,time]
HoldsAt(Disconnected(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(Disconnected(phone),time) ->
Terminates(SetDown(agent,phone),Disconnected(phone),time).

; Delta

Happens(PickUp(Agent1,Phone1),0).
Happens(Dial(Agent1,Phone1,Phone2),1).
Happens(PickUp(Agent2,Phone2),2).

; Psi

[phone,time]
!HoldsAt(Ringing(phone,phone),time).

[phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) &
phone1!=phone2 ->
!HoldsAt(Ringing(phone2,phone1),time).

[phone,time]
!HoldsAt(Connected(phone,phone),time).

[phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) &
phone1!=phone2 ->
!HoldsAt(Connected(phone2,phone1),time).

mutex Idle, DialTone, BusySignal, Disconnected

[phone1,phone2,time]
HoldsAt(Idle(phone1),time) ->
!HoldsAt(Ringing(phone1,phone2),time) &
!HoldsAt(Connected(phone1,phone2),time).

; etc.

; Gamma

[phone] HoldsAt(Idle(phone),0).

completion Happens

range time 0 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter3/Telephone1.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort phone

agent Agent1, Agent2
phone Phone1, Phone2

fluent Ringing(phone,phone)
fluent DialTone(phone)
fluent BusySignal(phone)
fluent Idle(phone)
fluent Connected(phone,phone)
fluent Disconnected(phone)

event PickUp(agent,phone)
event SetDown(agent,phone)
event Dial(agent,phone,phone)

; Sigma

[agent,phone,time]
HoldsAt(Idle(phone),time) ->
Initiates(PickUp(agent,phone),DialTone(phone),time).

[agent,phone,time]
HoldsAt(Idle(phone),time) ->
Terminates(PickUp(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(DialTone(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(DialTone(phone),time) ->
Terminates(SetDown(agent,phone),DialTone(phone),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Initiates(Dial(agent,phone1,phone2),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
!HoldsAt(Idle(phone2),time) ->
Initiates(Dial(agent,phone1,phone2),BusySignal(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
!HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).

[agent,phone,time]
HoldsAt(BusySignal(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(BusySignal(phone),time) ->
Terminates(SetDown(agent,phone),BusySignal(phone),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Terminates(SetDown(agent,phone1),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(PickUp(agent,phone2),Connected(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Terminates(PickUp(agent,phone2),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Disconnected(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Terminates(SetDown(agent,phone1),Connected(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone2),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone2),Disconnected(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Terminates(SetDown(agent,phone2),Connected(phone1,phone2),time).

[agent,phone,time]
HoldsAt(Disconnected(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(Disconnected(phone),time) ->
Terminates(SetDown(agent,phone),Disconnected(phone),time).

; Delta

Delta: Happens(PickUp(Agent1,Phone1),0).
Delta: Happens(Dial(Agent1,Phone1,Phone2),1).
Delta: Happens(PickUp(Agent2,Phone2),2).

; Gamma

[phone] HoldsAt(Idle(phone),0).
[phone] !HoldsAt(DialTone(phone),0).
[phone] !HoldsAt(BusySignal(phone),0).
[phone1,phone2] !HoldsAt(Ringing(phone1,phone2),0).
[phone1,phone2] !HoldsAt(Connected(phone1,phone2),0).
[phone] !HoldsAt(Disconnected(phone),0).

completion Delta Happens

range time 0 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter12/DefaultLocation.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
sort agent: object
sort device: object
sort tv: device
sort room

agent Nathan
tv TV
room LivingRoom, Kitchen

event TurnOn(agent,device)
event Walk(agent,room,room)

fluent InRoom(object,room)
fluent On(device)
fluent PluggedIn(device)
fluent BrokenSwitch(device)

predicate Ab1(device,time)
predicate Ab2(room,time)

; Sigma

[agent,room1,room2,time]
Initiates(Walk(agent,room1,room2),InRoom(agent,room2),time).

[agent,room1,room2,time]
room1!=room2 ->
Terminates(Walk(agent,room1,room2),InRoom(agent,room1),time).

[agent,device,time]
!Ab1(device,time) ->
Initiates(TurnOn(agent,device),On(device),time).

; Delta

[agent,room1,room2,time]
Happens(Walk(agent,room1,room2),time) ->
room1!=room2 &
HoldsAt(InRoom(agent,room1),time).

[agent,device,time]
Happens(TurnOn(agent,device),time) ->
{room} HoldsAt(InRoom(agent,room),time) &
       HoldsAt(InRoom(device,room),time).

[event1,event2,time]
Happens(event1,time) &
Happens(event2,time) ->
event1=event2.

; Theta

Theta: [device,time] HoldsAt(BrokenSwitch(device),time) -> Ab1(device,time).
Theta: [device,time] !HoldsAt(PluggedIn(device),time) -> Ab1(device,time).

; Psi

[object,room1,room2,time]
HoldsAt(InRoom(object,room1),time) &
HoldsAt(InRoom(object,room2),time) ->
room1=room2.

; Gamma

[tv] !HoldsAt(On(tv),0).
[tv] !HoldsAt(BrokenSwitch(tv),0).
[tv] HoldsAt(PluggedIn(tv),0).

HoldsAt(InRoom(Nathan,Kitchen),0).

[time]
!Ab2(LivingRoom,time) ->
{tv} HoldsAt(InRoom(tv,LivingRoom),time).

; goal

{tv} Happens(TurnOn(Nathan,tv),1).

; for two TVs:
;[tv,time] !HoldsAt(InRoom(tv,Kitchen),time).
;[tv,time] {room} HoldsAt(InRoom(tv,room),time).

completion Theta Ab1
completion Theta Ab2

range time 0 2
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter12/Device.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort device

agent Nathan
device Device1, AntiqueDevice1

predicate Ab1(device,time)

fluent On(device)
fluent PluggedIn(device)
fluent BrokenSwitch(device)

event TurnOn(agent,device)

; Sigma

[agent,device,time]
!Ab1(device,time) ->
Initiates(TurnOn(agent,device),On(device),time).

; Delta

Happens(TurnOn(Nathan,Device1),0).

; Theta

Theta: [device,time] HoldsAt(BrokenSwitch(device),time) -> Ab1(device,time).
Theta: [device,time] !HoldsAt(PluggedIn(device),time) -> Ab1(device,time).
Theta: [time] Ab1(AntiqueDevice1,time).

; Gamma

!HoldsAt(On(Device1),0).
!HoldsAt(BrokenSwitch(Device1),0).
HoldsAt(PluggedIn(Device1),0).

; added:
[time] !HoldsAt(On(AntiqueDevice1),time).
[time] HoldsAt(PluggedIn(AntiqueDevice1),time).

; entailed:
; HoldsAt(On(Device1),1).

completion Theta Ab1
completion Happens

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter12/ErraticDevice.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort device

agent Nathan
device Device1

predicate Ab1(device,time)

fluent On(device)
fluent PluggedIn(device)
fluent BrokenSwitch(device)
fluent Erratic(device)

fluent DeterminingFluent(device)
noninertial DeterminingFluent

event TurnOn(agent,device)

; Sigma

[agent,device,time]
!Ab1(device,time) ->
Initiates(TurnOn(agent,device),On(device),time).

; Delta

Happens(TurnOn(Nathan,Device1),0).

; Theta


Theta: [device,time] HoldsAt(BrokenSwitch(device),time) -> Ab1(device,time).
Theta: [device,time]
HoldsAt(Erratic(device),time) & HoldsAt(DeterminingFluent(device),time) ->
Ab1(device,time).

Theta: [device,time] !HoldsAt(PluggedIn(device),time) -> Ab1(device,time).

; Gamma

!HoldsAt(On(Device1),0).
!HoldsAt(BrokenSwitch(Device1),0).
HoldsAt(Erratic(Device1),0).
HoldsAt(PluggedIn(Device1),0).

; added:
HoldsAt(DeterminingFluent(Device1),1).

completion Theta Ab1
completion Happens

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter12/DefaultEvent.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort agent
sort clock

fluent Beeping(clock)
fluent AlarmTime(clock,time)
fluent AlarmOn(clock)

event SetAlarmTime(agent,clock,time)
event StartBeeping(clock)
event TurnOnAlarm(agent,clock)
event TurnOffAlarm(agent,clock)

predicate Ab1(clock,time)

agent Nathan
clock Clock

; Sigma

[agent,clock,time1,time2,time]
HoldsAt(AlarmTime(clock,time1),time) &
time1!=time2 ->
Initiates(SetAlarmTime(agent,clock,time2),AlarmTime(clock,time2),time).

[agent,clock,time1,time2,time]
HoldsAt(AlarmTime(clock,time1),time) &
time1!=time2 ->
Terminates(SetAlarmTime(agent,clock,time2),AlarmTime(clock,time1),time).

[agent,clock,time]
Initiates(TurnOnAlarm(agent,clock),AlarmOn(clock),time).

[agent,clock,time]
Terminates(TurnOffAlarm(agent,clock),AlarmOn(clock),time).

[clock,time]
Initiates(StartBeeping(clock),Beeping(clock),time).

[agent,clock,time]
Terminates(TurnOffAlarm(agent,clock),Beeping(clock),time).

; Delta

[clock,time]
HoldsAt(AlarmTime(clock,time),time) &
HoldsAt(AlarmOn(clock),time) &
!Ab1(clock,time) ->
Happens(StartBeeping(clock),time).

Happens(SetAlarmTime(Nathan,Clock,2),0).
Happens(TurnOnAlarm(Nathan,Clock),1).

; Psi

[clock,time1,time2,time]
HoldsAt(AlarmTime(clock,time1),time) &
HoldsAt(AlarmTime(clock,time2),time) ->
time1=time2.

; Gamma

!HoldsAt(AlarmOn(Clock),0).
!HoldsAt(Beeping(Clock),0).
HoldsAt(AlarmTime(Clock,3),0).

completion Happens
completion Theta Ab1

range time 0 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter12/MethodD.e
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
; Method (D)
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object

object A,B

fluent P(object)
fluent Q(object)
fluent R(object)

predicate Ab1(object,time)
predicate Ab2(object,time)

[object,time]
HoldsAt(P(object),time) & !Ab1(object,time) ->
HoldsAt(Q(object),time).

[object,time]
HoldsAt(R(object),time) & !Ab2(object,time) ->
!HoldsAt(Q(object),time).

[object,time]
HoldsAt(R(object),time) -> HoldsAt(P(object),time).

HoldsAt(R(A),0).
HoldsAt(P(B),0).
!HoldsAt(R(B),0).

Theta: 
[object,time]
HoldsAt(R(object),time) -> Ab1(object,time).

range time 0 0
range offset 1 1

completion Theta Ab1
completion Theta Ab2

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter12/BrokenDevice.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort device

agent Nathan
device Device1

predicate Ab1(device,time)

fluent On(device)
fluent PluggedIn(device)
fluent BrokenSwitch(device)

event TurnOn(agent,device)

; Sigma

[agent,device,time]
!Ab1(device,time) ->
Initiates(TurnOn(agent,device),On(device),time).

; Delta

Happens(TurnOn(Nathan,Device1),0).

; Theta

Theta: [device,time] HoldsAt(BrokenSwitch(device),time) -> Ab1(device,time).
Theta: [device,time] !HoldsAt(PluggedIn(device),time) -> Ab1(device,time).

; Gamma

!HoldsAt(On(Device1),0).
HoldsAt(BrokenSwitch(Device1),0).

; added:
HoldsAt(PluggedIn(Device1),0).

; entailed:
; !HoldsAt(On(Device1),1).

completion Theta Ab1
completion Happens

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter12/MethodB.e
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
; Method (D)
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object

object A,B

fluent P(object)
fluent Q(object)
predicate Ab(object,time)

[object,time]
HoldsAt(P(object),time) & !Ab(object,time) ->
HoldsAt(Q(object),time).

HoldsAt(P(A),0).
HoldsAt(P(B),0).

Theta: Ab(A,0).

range time 0 0
range offset 1 1

completion Theta Ab

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter13/ModelFinding.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).
[agent,time] Happens(WakeUp(agent),time) -> !HoldsAt(Awake(agent),time).

agent James

range time 0 1
range offset 1 1



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter13/Postdiction.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).
[agent,time] Happens(WakeUp(agent),time) -> !HoldsAt(Awake(agent),time).

agent James
Delta: Happens(WakeUp(James),0).
HoldsAt(Awake(James),1).

completion Delta Happens

range time 0 1
range offset 1 1



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter13/Deduction2.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option timediff off

load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).

agent James
!HoldsAt(Awake(James),0).
Delta: Happens(WakeUp(James),0).

completion Delta Happens

range time 0 1
range offset 1 1



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter13/Deduction1.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).

agent James
!HoldsAt(Awake(James),0).
Delta: Happens(WakeUp(James),0).

completion Delta Happens

range time 0 1
range offset 1 1



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter13/Abduction.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).

agent James
!HoldsAt(Awake(James),0).
HoldsAt(Awake(James),1).

range time 0 1
range offset 1 1



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter4/AlarmClock.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort clock

fluent Beeping(clock)
fluent AlarmTime(clock,time)
fluent AlarmOn(clock)

event SetAlarmTime(agent,clock,time)
event StartBeeping(clock)
event TurnOnAlarm(agent,clock)
event TurnOffAlarm(agent,clock)

agent Nathan
clock Clock

; Sigma

[agent,clock,time1,time2,time]
HoldsAt(AlarmTime(clock,time1),time) &
time1!=time2 ->
Initiates(SetAlarmTime(agent,clock,time2),AlarmTime(clock,time2),time).

[agent,clock,time1,time2,time]
HoldsAt(AlarmTime(clock,time1),time) &
time1!=time2 ->
Terminates(SetAlarmTime(agent,clock,time2),AlarmTime(clock,time1),time).

[agent,clock,time]
Initiates(TurnOnAlarm(agent,clock),AlarmOn(clock),time).

[agent,clock,time]
Terminates(TurnOffAlarm(agent,clock),AlarmOn(clock),time).

[clock,time]
Initiates(StartBeeping(clock),Beeping(clock),time).

[agent,clock,time]
Terminates(TurnOffAlarm(agent,clock),Beeping(clock),time).

; Delta

[clock,time]
HoldsAt(AlarmTime(clock,time),time) &
HoldsAt(AlarmOn(clock),time) ->
Happens(StartBeeping(clock),time).

Happens(SetAlarmTime(Nathan,Clock,2),0).
Happens(TurnOnAlarm(Nathan,Clock),1).

; Psi

[clock,time1,time2,time]
HoldsAt(AlarmTime(clock,time1),time) &
HoldsAt(AlarmTime(clock,time2),time) ->
time1=time2.

; Gamma

!HoldsAt(AlarmOn(Clock),0).
!HoldsAt(Beeping(Clock),0).
HoldsAt(AlarmTime(Clock,3),0).

completion Happens

range time 0 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter4/BankAccountServiceFee.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort account
sort value: integer

account Account1, Account2

predicate EndOfMonth(time)
function ServiceFee(account): value
function MinimumBalance(account): value

fluent ServiceFeeCharged(account)
fluent Balance(account,value)

event Transfer(account,account,value)
event MonthlyReset(account)
event ChargeServiceFee(account)

; Sigma

[account1,account2,value1,value2,value3,value4,time]
HoldsAt(Balance(account1,value1),time) &
HoldsAt(Balance(account2,value2),time) &
value3>0 &
value1>=value3 &
value4=(value2+value3) ->
Initiates(Transfer(account1,account2,value3),Balance(account2,value4),time).

[account1,account2,value1,value2,value3,time]
HoldsAt(Balance(account1,value1),time) &
HoldsAt(Balance(account2,value2),time) &
value3>0 &
value1>=value3 ->
Terminates(Transfer(account1,account2,value3),Balance(account2,value2),time).

[account1,account2,value1,value2,value3,value4,time]
HoldsAt(Balance(account1,value1),time) &
HoldsAt(Balance(account2,value2),time) &
value3>0 &
value1>=value3 &
value4=(value1-value3) ->
Initiates(Transfer(account1,account2,value3),Balance(account1,value4),time).

[account1,account2,value1,value2,value3,time]
HoldsAt(Balance(account1,value1),time) &
HoldsAt(Balance(account2,value2),time) &
value3>0 &
value1>=value3 ->
Terminates(Transfer(account1,account2,value3),Balance(account1,value1),time).

[account,time]
Initiates(ChargeServiceFee(account),ServiceFeeCharged(account),time).

[account,time]
Terminates(MonthlyReset(account),ServiceFeeCharged(account),time).

[account,value1,value2,time]
HoldsAt(Balance(account,value1),time) &
value2 = (value1-ServiceFee(account)) ->
Initiates(ChargeServiceFee(account),
          Balance(account,value2),
          time).

[account,value,time]
HoldsAt(Balance(account,value),time) ->
Terminates(ChargeServiceFee(account),Balance(account,value),time).

; Delta

[account,value,time]
HoldsAt(Balance(account,value),time) &
value<MinimumBalance(account) &
!HoldsAt(ServiceFeeCharged(account),time) ->
Happens(ChargeServiceFee(account),time).

[account,time]
EndOfMonth(time) ->
Happens(MonthlyReset(account),time).

Happens(Transfer(Account1,Account2,1),0).
Happens(Transfer(Account1,Account2,1),0).

; Psi

[account,value1,value2,time]
HoldsAt(Balance(account,value1),time) &
HoldsAt(Balance(account,value2),time) ->
value1=value2.

; Gamma

!HoldsAt(ServiceFeeCharged(Account1),0).
!HoldsAt(ServiceFeeCharged(Account2),0).
HoldsAt(Balance(Account1,3),0).
HoldsAt(Balance(Account2,1),0).
MinimumBalance(Account1)=3.
MinimumBalance(Account2)=1.
ServiceFee(Account1)=1.
ServiceFee(Account2)=1.
[time] !EndOfMonth(time).

completion Happens

range time 0 3
range value 1 3
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Exercises/Counter.e
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
; @article{DeneckerDupreBelleghem:1998,
;   author = "Marc Denecker and Daniele Theseider Dupr\'{e} and Kristof Van Belleghem",
;   year = "1998",
;   title = "An inductive definition approach to ramifications",
;   journal = "Link{\"{o}}ping Electronic Articles in Computer and Information Science",
;   volume = "3",
;   number = "007",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort counter
counter Counter1

event FalseToTrue(counter)
event TrueToFalse(counter)

fluent Count(counter,integer)
fluent True(counter)
fluent InputLine(counter)
noninertial InputLine

Delta: [counter,time]
!HoldsAt(True(counter),time) &
HoldsAt(InputLine(counter),time) ->
Happens(FalseToTrue(counter),time).

Delta: [counter,time]
HoldsAt(True(counter),time) &
!HoldsAt(InputLine(counter),time) ->
Happens(TrueToFalse(counter),time).

[counter,time] Initiates(FalseToTrue(counter),True(counter),time).

[counter,time] Terminates(TrueToFalse(counter),True(counter),time).

[counter,integer1,integer2,time]
HoldsAt(Count(counter,integer1),time) &
(integer2 = (integer1 + 1)) ->
Initiates(FalseToTrue(counter),Count(counter,integer2),time).

[counter,integer,time]
HoldsAt(Count(counter,integer),time) ->
Terminates(FalseToTrue(counter),Count(counter,integer),time).

[counter,integer1,integer2,time]
HoldsAt(Count(counter,integer1),time) &
HoldsAt(Count(counter,integer2),time) ->
integer1 = integer2.

!HoldsAt(True(Counter1),0).
!HoldsAt(InputLine(Counter1),0).
HoldsAt(InputLine(Counter1),1).
HoldsAt(InputLine(Counter1),2).
HoldsAt(InputLine(Counter1),3).
!HoldsAt(InputLine(Counter1),4).
!HoldsAt(InputLine(Counter1),5).
!HoldsAt(InputLine(Counter1),6).
HoldsAt(InputLine(Counter1),7).
HoldsAt(InputLine(Counter1),8).
HoldsAt(InputLine(Counter1),9).

HoldsAt(Count(Counter1,0),0).

completion Happens

range integer 0 6
range time 0 10
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Exercises/TeacherTells.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort agent
sort room
sort fact

agent Teacher, Student
room Kitchen, Classroom
fact Fact1, Fact2

fluent InRoom(agent,room)
fluent ListeningTo(agent,agent)
fluent Know(agent,fact)

event Tell(agent,agent,fact)

; Sigma

[agent1,agent2,fact,time]
({room} HoldsAt(InRoom(agent1,room),time) &
        HoldsAt(InRoom(agent2,room),time)) &
HoldsAt(ListeningTo(agent2,agent1),time) ->
Initiates(Tell(agent1,agent2,fact),Know(agent2,fact),time).

; Delta

Happens(Tell(Teacher,Student,Fact1),0).

; Psi

[agent,room1,room2,time]
HoldsAt(InRoom(agent,room1),time) &
HoldsAt(InRoom(agent,room2),time) ->
room1 = room2.

; Gamma

[agent,fact] !HoldsAt(Know(agent,fact),0).
[agent1,agent2] HoldsAt(ListeningTo(agent1,agent2),0).
[agent] HoldsAt(InRoom(agent,Classroom),0).

completion Happens

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Exercises/MixingPaints.e
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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort palette
sort color

palette Palette1
color Red, Yellow, Blue, Green

event PlaceOnPalette(palette,color)
fluent OnPalette(palette,color)

[palette,color,time]
!Happens(PlaceOnPalette(palette,Yellow),time) |
!Happens(PlaceOnPalette(palette,Blue),time) ->
Initiates(PlaceOnPalette(palette,color),OnPalette(palette,color),time).

[palette,color1,color2,time]
Happens(PlaceOnPalette(palette,Yellow),time) &
color1 = Blue &
color2 = Green ->
Initiates(PlaceOnPalette(palette,color1),OnPalette(palette,color2),time).

[palette,color1,color2,time]
!(Happens(PlaceOnPalette(palette,Yellow),time) &
  Happens(PlaceOnPalette(palette,Blue),time)) &
HoldsAt(OnPalette(palette,color1),time) &
color1 != color2 ->
Terminates(PlaceOnPalette(palette,color2),OnPalette(palette,color1),time).

[palette,color1,color2,time]
Happens(PlaceOnPalette(palette,Yellow),time) &
HoldsAt(OnPalette(palette,color2),time) &
color1 = Blue &
color2 != Green ->
Terminates(PlaceOnPalette(palette,color1),OnPalette(palette,color2),time).

; state constraint

[palette,color1,color2,time]
HoldsAt(OnPalette(palette,color1),time) &
HoldsAt(OnPalette(palette,color2),time) ->
color1 = color2.

; (1) place green over red
HoldsAt(OnPalette(Palette1,Red),0).
Delta: Happens(PlaceOnPalette(Palette1,Green),0).

; (2) place yellow+blue over green
Delta: Happens(PlaceOnPalette(Palette1,Yellow),1).
Delta: Happens(PlaceOnPalette(Palette1,Blue),1).

; (3) place yellow
Delta: Happens(PlaceOnPalette(Palette1,Yellow),2).

; (4) place blue
Delta: Happens(PlaceOnPalette(Palette1,Blue),3).

; (5) place green
Delta: Happens(PlaceOnPalette(Palette1,Yellow),4).
Delta: Happens(PlaceOnPalette(Palette1,Blue),4).

completion Delta Happens

range time 0 5
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Exercises/SnoozeAlarm.e
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
; Example: Alarm Clock with snooze alarm added
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort clock

fluent Beeping(clock)
fluent AlarmTime(clock,time)
fluent AlarmOn(clock)

event SetAlarmTime(agent,clock,time)
event StartBeeping(clock)
event TurnOnAlarm(agent,clock)
event TurnOffAlarm(agent,clock)

event PressSnooze(agent,clock)

agent Nathan
clock Clock

; Sigma

[agent,clock,time1,time2,time]
HoldsAt(AlarmTime(clock,time1),time) &
time1!=time2 ->
Initiates(SetAlarmTime(agent,clock,time2),AlarmTime(clock,time2),time).

[agent,clock,time1,time2,time]
HoldsAt(AlarmTime(clock,time1),time) &
time1!=time2 ->
Terminates(SetAlarmTime(agent,clock,time2),AlarmTime(clock,time1),time).

[agent,clock,time]
Initiates(TurnOnAlarm(agent,clock),AlarmOn(clock),time).

[agent,clock,time]
Terminates(TurnOffAlarm(agent,clock),AlarmOn(clock),time).

[clock,time]
Initiates(StartBeeping(clock),Beeping(clock),time).

[agent,clock,time]
Terminates(TurnOffAlarm(agent,clock),Beeping(clock),time).

; added axioms:

[agent,clock,time2,time]
HoldsAt(Beeping(clock),time) &
time2 = time+9 ->
Initiates(PressSnooze(agent,clock),AlarmTime(clock,time2),time).

[agent,clock,time1,time2,time]
HoldsAt(Beeping(clock),time) &
HoldsAt(AlarmTime(clock,time1),time) &
time2 = time+9 &
time1 != time2 ->
Terminates(PressSnooze(agent,clock),AlarmTime(clock,time1),time).

[agent,clock,time]
Terminates(PressSnooze(agent,clock),Beeping(clock),time).

; Delta

[clock,time]
HoldsAt(AlarmTime(clock,time),time) &
HoldsAt(AlarmOn(clock),time) ->
Happens(StartBeeping(clock),time).

Happens(SetAlarmTime(Nathan,Clock,2),0).
Happens(TurnOnAlarm(Nathan,Clock),1).
Happens(PressSnooze(Nathan,Clock),4).

; Psi

[clock,time1,time2,time]
HoldsAt(AlarmTime(clock,time1),time) &
HoldsAt(AlarmTime(clock,time2),time) ->
time1=time2.

; Gamma

!HoldsAt(AlarmOn(Clock),0).
!HoldsAt(Beeping(Clock),0).
HoldsAt(AlarmTime(Clock,3),0).

completion Happens

range time 0 15
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Exercises/TelephoneBugs.e
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
; Example: Telephone
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort phone

agent Agent1, Agent2
phone Phone1, Phone2

fluent Ringing(phone,phone)
fluent DialTone(phone)
fluent BusySignal(phone)
fluent Idle(phone)
fluent Connected(phone,phone)
fluent Disconnected(phone)

event PickUp(agent,phone)
event SetDown(agent,phone)
event Dial(agent,phone,phone)

; Sigma

[agent,phone,time]
HoldsAt(Idle(phone),time) ->
Initiates(PickUp(agent,phone),DialTone(phone),time).

[agent,phone,time]
HoldsAt(Idle(phone),time) ->
Terminates(PickUp(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(DialTone(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(DialTone(phone),time) ->
Terminates(SetDown(agent,phone),DialTone(phone),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Initiates(Dial(agent,phone1,phone2),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
!HoldsAt(Idle(phone2),time) ->
Initiates(Dial(agent,phone1,phone2),BusySignal(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
!HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).

[agent,phone,time]
HoldsAt(BusySignal(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(BusySignal(phone),time) ->
Terminates(SetDown(agent,phone),BusySignal(phone),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Terminates(SetDown(agent,phone1),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(PickUp(agent,phone2),Connected(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Terminates(PickUp(agent,phone2),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Disconnected(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Terminates(SetDown(agent,phone1),Connected(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone2),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone2),Disconnected(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Terminates(SetDown(agent,phone2),Connected(phone1,phone2),time).

[agent,phone,time]
HoldsAt(Disconnected(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(Disconnected(phone),time) ->
Terminates(SetDown(agent,phone),Disconnected(phone),time).

; Delta

; (1) Two agents dial each other simultaneously without first
; picking up phone.
Happens(Dial(Agent1,Phone1,Phone2),0).
Happens(Dial(Agent2,Phone2,Phone1),0).

; (2) Two agents dial each other simultaneously.
Happens(PickUp(Agent1,Phone1),1).
Happens(PickUp(Agent2,Phone2),1).
Happens(Dial(Agent1,Phone1,Phone2),2).
Happens(Dial(Agent2,Phone2,Phone1),2).
Happens(SetDown(Agent1,Phone1),3).
Happens(SetDown(Agent2,Phone2),3).

; (3) One agent dials another agent just as the other
; agent picks up the phone.
Happens(PickUp(Agent1,Phone1),4).
Happens(Dial(Agent1,Phone1,Phone2),5).
Happens(PickUp(Agent2,Phone2),5).

; Psi

[phone,time]
!HoldsAt(Ringing(phone,phone),time).

[phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) &
phone1!=phone2 ->
!HoldsAt(Ringing(phone2,phone1),time).

[phone,time]
!HoldsAt(Connected(phone,phone),time).

[phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) &
phone1!=phone2 ->
!HoldsAt(Connected(phone2,phone1),time).

mutex Idle, DialTone, BusySignal, Disconnected

[phone1,phone2,time]
HoldsAt(Idle(phone1),time) ->
!HoldsAt(Ringing(phone1,phone2),time) &
!HoldsAt(Connected(phone1,phone2),time).

; contradicts (3) above:
;[phone1,phone2,time]
;HoldsAt(DialTone(phone2),time) ->
;!HoldsAt(Ringing(phone1,phone2),time) &
;!HoldsAt(Connected(phone1,phone2),time).

; etc.

; Gamma

[phone] HoldsAt(Idle(phone),0).

completion Happens

range time 0 6
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter11/HungryCat.e
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
; @inproceedings{WinikoffEtAl:2002,
;   author = "Michael Winikoff and Lin Padgham and James Harland and John Thangarajah",
;   year = "2002",
;   title = "Declarative \& procedural goals in intelligent agent systems",
;   editor = "Dieter Fensel and Fausto Giunchiglia and Deborah McGuinness and Mary-Anne Williams",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{E}ighth \uppercase{I}nternational \uppercase{C}onference on \uppercase{P}rinciples of \uppercase{K}nowledge \uppercase{R}epresentation and \uppercase{R}easoning",
;   pages = "470--481",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
sort agent: object
sort food: object
sort surface
sort plan

reified sort belief

agent Cat
surface Floor, Chair, Shelf, Table
food Food1, Food2
plan P1, P1a, P1b, P2, P2a

predicate SelectedPlan(agent,belief,plan,time)
predicate SoundPlan(agent,belief,plan,time)

fluent On(object,surface)
fluent Goal(agent,belief)
fluent CanJump(surface,surface)
fluent Plan(agent,belief,plan)
fluent Satiated(agent)
fluent Believe(agent,belief)

event AddPlan(agent,belief,plan)
event DropPlan(agent,belief,plan)
event Jump(agent,surface,surface)
event Move(surface,surface,surface)
event Eat(agent,food)
event Wait(agent)

belief BSatiated(agent)
belief BCanJump(surface,surface)
belief BOn(object,surface)

; Sigma

; A5
[agent,belief,plan,time]
Initiates(AddPlan(agent,belief,plan),Plan(agent,belief,plan),time).

; A6
[agent,belief,plan,time]
Terminates(DropPlan(agent,belief,plan),Plan(agent,belief,plan),time).

[agent,surface1,surface2,time]
HoldsAt(On(agent,surface1),time) &
HoldsAt(CanJump(surface1,surface2),time) ->
Initiates(Jump(agent,surface1,surface2),On(agent,surface2),time).

[agent,surface1,surface2,time]
HoldsAt(On(agent,surface1),time) &
HoldsAt(CanJump(surface1,surface2),time) ->
Terminates(Jump(agent,surface1,surface2),On(agent,surface1),time).

[surface1,surface2,surface3,time]
Initiates(Move(surface1,surface2,surface3),CanJump(surface1,surface3),time).

[surface1,surface2,surface3,time]
Terminates(Move(surface1,surface2,surface3),CanJump(surface1,surface2),time).

[agent,food,surface,time]
HoldsAt(On(agent,surface),time) &
HoldsAt(On(food,surface),time) ->
Initiates(Eat(agent,food),Satiated(agent),time).

[agent,food,surface,time]
HoldsAt(On(agent,surface),time) &
HoldsAt(On(food,surface),time) ->
Terminates(Eat(agent,food),On(food,surface),time).

[agent,surface1,surface2,belief,time]
HoldsAt(Believe(agent,BOn(agent,surface1)),time) &
HoldsAt(Believe(agent,BCanJump(surface1,surface2)),time) &
(belief = BOn(agent,surface2)) ->
Initiates(Jump(agent,surface1,surface2),
          Believe(agent,belief),
          time).

[agent,surface1,surface2,belief,time]
HoldsAt(Believe(agent,BOn(agent,surface1)),time) &
HoldsAt(Believe(agent,BCanJump(surface1,surface2)),time) &
(belief = BOn(agent,surface1)) ->
Terminates(Jump(agent,surface1,surface2),
           Believe(agent,belief),
           time).

[agent,surface1,surface2,surface3,belief,time]
(belief = BCanJump(surface1,surface3)) ->
Initiates(Move(surface1,surface2,surface3),
          Believe(agent,belief),
          time).

[agent,surface1,surface2,surface3,belief,time]
(belief = BCanJump(surface1,surface2)) ->
Terminates(Move(surface1,surface2,surface3),
           Believe(agent,belief),
           time).

[agent,food,surface,belief,time]
HoldsAt(Believe(agent,BOn(agent,surface)),time) &
HoldsAt(Believe(agent,BOn(food,surface)),time) &
(belief = BSatiated(agent)) ->
Initiates(Eat(agent,food),Believe(agent,belief),time).

[agent,food,surface,belief,time]
HoldsAt(Believe(agent,BOn(agent,surface)),time) &
HoldsAt(Believe(agent,BOn(food,surface)),time) &
(belief = BOn(food,surface)) ->
Terminates(Eat(agent,food),Believe(agent,belief),time).

; Delta

; A7
[agent,belief,plan,time]
HoldsAt(Goal(agent,belief),time) &
!HoldsAt(Believe(agent,belief),time) &
SelectedPlan(agent,belief,plan,time) &
(!{plan1} HoldsAt(Plan(agent,belief,plan1),time)) ->
Happens(AddPlan(agent,belief,plan),time).

; A8
[agent,belief,time]
HoldsAt(Plan(agent,belief,P1),time) &
!HoldsAt(Believe(agent,belief),time) &
SoundPlan(agent,belief,P1,time) ->
Happens(Jump(Cat,Floor,Chair),time).

[agent,belief,time]
HoldsAt(Plan(agent,belief,P1a),time) &
!HoldsAt(Believe(agent,belief),time) &
SoundPlan(agent,belief,P1a,time) ->
Happens(Wait(Cat),time).

[agent,belief,time]
HoldsAt(Plan(agent,belief,P2),time) &
!HoldsAt(Believe(agent,belief),time) &
SoundPlan(agent,belief,P2,time) ->
Happens(Jump(Cat,Chair,Shelf),time).

; A9
[agent,belief,plan,time]
HoldsAt(Plan(agent,belief,plan),time) ->
Happens(DropPlan(agent,belief,plan),time).

; A10
[agent,belief,time]
HoldsAt(Plan(agent,belief,P1),time) &
!HoldsAt(Believe(agent,belief),time) &
SoundPlan(agent,belief,P1,time) ->
Happens(AddPlan(agent,belief,P1a),time).

[agent,belief,time]
HoldsAt(Plan(agent,belief,P1a),time) &
!HoldsAt(Believe(agent,belief),time) &
SoundPlan(agent,belief,P1a,time) ->
Happens(AddPlan(agent,belief,P1b),time).

[agent,belief,time]
HoldsAt(Plan(agent,belief,P2),time) &
!HoldsAt(Believe(agent,belief),time) &
SoundPlan(agent,belief,P2,time) ->
Happens(AddPlan(agent,belief,P2a),time).

; reactive behavior
[agent,food,surface,time]
!HoldsAt(Satiated(agent),time) &
HoldsAt(On(agent,surface),time) &
HoldsAt(On(food,surface),time) ->
Happens(Eat(agent,food),time).

; narrative

Happens(Move(Chair,Table,Shelf),2).

; SelectedPlan - plan library

;[agent,belief,plan,time]
;SelectedPlan(agent,belief,plan,time) <->
;(agent=Cat & belief=BSatiated(Cat) & plan=P1 & time=0) |
;(agent=Cat & belief=BSatiated(Cat) & plan=P2 & time=4).

[agent,belief,plan,time]
SelectedPlan(agent,belief,plan,time) <->
({surface1,surface2,surface3,food}
 HoldsAt(Believe(agent,BOn(agent,surface1)),time) &
 HoldsAt(Believe(agent,BCanJump(surface1,surface2)),time) &
 HoldsAt(Believe(agent,BCanJump(surface2,surface3)),time) &
 HoldsAt(Believe(agent,BOn(food,surface3)),time) &
 belief=BSatiated(agent) &
 plan=P1 &
 time=0) |
({surface1,surface2,surface3,food}
 HoldsAt(Believe(agent,BOn(agent,surface1)),time) &
 HoldsAt(Believe(agent,BCanJump(surface1,surface2)),time) &
 HoldsAt(Believe(agent,BCanJump(surface2,surface3)),time) &
 HoldsAt(Believe(agent,BOn(food,surface3)),time) &
 belief=BSatiated(agent) &
 plan=P2 &
 time=4).


; SoundPlan

[agent,belief,plan,time]
SoundPlan(agent,belief,plan,time) <->
(plan=P1 ->
 HoldsAt(Believe(agent,BCanJump(Floor,Chair)),time) &
 HoldsAt(Believe(agent,BCanJump(Chair,Table)),time)) &
((plan=P1a | plan=P1b) ->
  HoldsAt(Believe(agent,BCanJump(Chair,Table)),time)).

; Gamma

[agent,belief]
HoldsAt(Goal(agent,belief),0) <->
(agent=Cat & belief=BSatiated(Cat)).

[agent,belief,plan] !HoldsAt(Plan(agent,belief,plan),0).

[object,surface] HoldsAt(On(object,surface),0) <->
(object=Cat & surface=Floor) |
(object=Food1 & surface=Table) |
(object=Food2 & surface=Shelf).

[surface1,surface2] HoldsAt(CanJump(surface1,surface2),0) <->
(surface1=Floor & surface2=Chair) |
(surface1=Chair & surface2=Table) |
(surface1=Shelf & surface2=Table).

[agent,object,surface]
HoldsAt(Believe(agent,BOn(object,surface)),0) <->
(agent=Cat & object=Cat & surface=Floor) |
(agent=Cat & object=Food1 & surface=Table).

[agent,surface1,surface2]
HoldsAt(Believe(agent,BCanJump(surface1,surface2)),0) <->
(agent=Cat & surface1=Floor & surface2=Chair) |
(agent=Cat & surface1=Chair & surface2=Table) |
(agent=Cat & surface1=Shelf & surface2=Table).

!HoldsAt(Believe(Cat,BSatiated(Cat)),0).

; ADDED:
!HoldsAt(Satiated(Cat),0).

completion Happens

range time 0 7
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter11/Lottery.e
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
; @book{OrtonyCloreCollins:1988,
;   author = "Andrew Ortony and Gerald L. Clore and Allan M. Collins",
;   year = "1988",
;   title = "The Cognitive Structure of Emotions",
;   address = "Cambridge",
;   publisher = "Cambridge University Press",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort agent
sort aboutevent
sort desirability: integer

agent Kate, Lisa
aboutevent WinLotteryKate, WinLotteryLisa

fluent Joy(agent,aboutevent)
fluent Desirability(agent,agent,aboutevent,desirability)
fluent Believe(agent,aboutevent)
fluent Like(agent,agent)
fluent HappyFor(agent,agent,aboutevent)

event WinLottery(agent)
event AddJoy(agent,aboutevent)
event AddHappyFor(agent,agent,aboutevent)

; Sigma

[agent,aboutevent,time]
Initiates(AddJoy(agent,aboutevent),Joy(agent,aboutevent),time).

[agent1,agent2,aboutevent,time]
Initiates(AddHappyFor(agent1,agent2,aboutevent),
          HappyFor(agent1,agent2,aboutevent),
          time).

[agent1,agent2,aboutevent,time]
(agent1=Kate & aboutevent=WinLotteryKate) |
(agent1=Lisa & aboutevent=WinLotteryLisa) ->
Initiates(WinLottery(agent1),Believe(agent2,aboutevent),time).

; Delta

[agent,aboutevent,desirability,time]
!HoldsAt(Joy(agent,aboutevent),time) &
HoldsAt(Desirability(agent,agent,aboutevent,desirability),time) &
desirability=1 &
HoldsAt(Believe(agent,aboutevent),time) ->
Happens(AddJoy(agent,aboutevent),time).

[agent1,agent2,aboutevent,desirability1,desirability2,time]
!HoldsAt(HappyFor(agent1,agent2,aboutevent),time) &
HoldsAt(Desirability(agent1,agent2,aboutevent,desirability1),time) &
desirability1=1 &
HoldsAt(Desirability(agent1,agent1,aboutevent,desirability2),time) &
desirability2=1 &
HoldsAt(Like(agent1,agent2),time) &
HoldsAt(Believe(agent1,aboutevent),time) &
agent1 != agent2 ->
Happens(AddHappyFor(agent1,agent2,aboutevent),time).

Happens(WinLottery(Kate),0).

; Psi

[agent1,agent2,aboutevent,desirability1,desirability2,time]
HoldsAt(Desirability(agent1,agent2,aboutevent,desirability1),time) &
HoldsAt(Desirability(agent1,agent2,aboutevent,desirability2),time) ->
desirability1 = desirability2.

; Gamma

[agent,aboutevent] !HoldsAt(Joy(agent,aboutevent),0).
[agent1,agent2,aboutevent] !HoldsAt(HappyFor(agent1,agent2,aboutevent),0).
[aboutevent] !HoldsAt(Believe(Kate,aboutevent),0).
[aboutevent] !HoldsAt(Believe(Lisa,aboutevent),0).
[agent1,agent2,time] HoldsAt(Like(agent1,agent2),time).

[time] HoldsAt(Desirability(Lisa,Kate,WinLotteryKate,1),time).
[time] HoldsAt(Desirability(Kate,Kate,WinLotteryKate,1),time).
[time] HoldsAt(Desirability(Lisa,Lisa,WinLotteryKate,1),time).
[time] HoldsAt(Desirability(Kate,Kate,WinLotteryLisa,0),time).
[time] HoldsAt(Desirability(Kate,Lisa,WinLotteryLisa,0),time).
[time] HoldsAt(Desirability(Kate,Kate,WinLotteryLisa,0),time).
[time] HoldsAt(Desirability(Kate,Lisa,WinLotteryKate,0),time).
[time] HoldsAt(Desirability(Lisa,Lisa,WinLotteryLisa,0),time).
[time] HoldsAt(Desirability(Lisa,Kate,WinLotteryLisa,1),time).

completion Happens

range time 0 3
range desirability -1 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Manual/Example1a.e
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
; deduction

option timediff off

load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).

agent James
!HoldsAt(Awake(James),0).
Delta: Happens(WakeUp(James),0).

completion Delta Happens

range time 0 1
range offset 1 1



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Manual/Example1.e
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
; deduction

load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).

agent James
!HoldsAt(Awake(James),0).
Delta: Happens(WakeUp(James),0).

completion Delta Happens

range time 0 1
range offset 1 1



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Manual/Example4.e
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
load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).
[agent,time] Happens(WakeUp(agent),time) -> !HoldsAt(Awake(agent),time).

agent James, Jessie
!HoldsAt(Awake(James),0).
!HoldsAt(Awake(Jessie),0).
HoldsAt(Awake(James),1).

range time 0 1
range offset 1 1



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Manual/Example3.e
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
load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).

agent James, Jessie
!HoldsAt(Awake(James),0).
HoldsAt(Awake(James),1).

range time 0 1
range offset 1 1



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Manual/Example2.e
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
load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).

agent James
!HoldsAt(Awake(James),0).
HoldsAt(Awake(James),1).

range time 0 1
range offset 1 1



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004b/RunningAndDriving2.e
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
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

fluent Tired(agent)

event Move(agent)
event Run(agent)
event Drive(agent)

[agent,time]
Happens(Move(agent),time) ->
Happens(Run(agent),time) | Happens(Drive(agent),time).

xor Run, Drive

[agent,time] Initiates(Run(agent),Tired(agent),time).

agent James

!HoldsAt(Tired(James),0).
Happens(Move(James),0).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004b/OffOn.e
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
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort switch

fluent On(switch)
fluent Off(switch)
event TurnOn(agent,switch)
event TurnOff(agent,switch)

noninertial Off

[switch,time] HoldsAt(Off(switch),time) <-> !HoldsAt(On(switch),time).

[agent,switch,time] Initiates(TurnOn(agent,switch),On(switch),time).
[agent,switch,time] Terminates(TurnOff(agent,switch),On(switch),time).

agent James
switch Switch1

!HoldsAt(On(Switch1),0).
Happens(TurnOn(James,Switch1),0).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004b/TV2.e
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
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort switch
sort tv

function TVOf(switch): tv
fluent SwitchOn(switch)
fluent TVOn(tv)
fluent PluggedIn(tv)
event TurnOn(agent,switch)
event TurnOff(agent,switch)

[agent,switch,time] Initiates(TurnOn(agent,switch),SwitchOn(switch),time).

[agent,switch,tv,time]
TVOf(switch)=tv & HoldsAt(PluggedIn(tv),time) ->
Initiates(TurnOn(agent,switch),TVOn(tv),time).

agent James
switch Switch1
tv TV1

TVOf(Switch1)=TV1.
!HoldsAt(PluggedIn(TV1),0).
!HoldsAt(SwitchOn(Switch1),0).
!HoldsAt(TVOn(TV1),0).
Happens(TurnOn(James,Switch1),0).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004b/Approve.e
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
; example of concurrent events with cumulative or canceling effects
;
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

event ApproveOf(agent,agent)
event DisapproveOf(agent,agent)
fluent Happy(agent)
fluent Confused(agent)

[agent1,agent2,time]
!Happens(DisapproveOf(agent1,agent2),time) ->
Initiates(ApproveOf(agent1,agent2),Happy(agent2),time).

[agent1,agent2,time]
!Happens(ApproveOf(agent1,agent2),time) ->
Terminates(DisapproveOf(agent1,agent2),Happy(agent2),time).

[agent1,agent2,time]
Happens(DisapproveOf(agent1,agent2),time) ->
Initiates(ApproveOf(agent1,agent2),Confused(agent2),time).

agent James, Peter

[agent] !HoldsAt(Happy(agent),0) & !HoldsAt(Confused(agent),0).

Happens(ApproveOf(Peter,James),0).
Happens(DisapproveOf(Peter,James),0).

completion Happens

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004b/Leaf.e
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
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;

option trajectory on

load foundations/Root.e
load foundations/EC.e

sort object
sort height: integer

fluent Height(object,height)
fluent Falling(object)
event StartFalling(object)
event HitsGround(object)

[object,height1,height2,time]
HoldsAt(Height(object,height1),time) &
HoldsAt(Height(object,height2),time) ->
height1=height2.

[object,time]
Initiates(StartFalling(object),Falling(object),time).

[object,height,time]
Releases(StartFalling(object),Height(object,height),time).

[object,height1,height2,offset,time]
HoldsAt(Height(object,height1),time) &
height2=height1-offset ->
Trajectory(Falling(object),time,Height(object,height2),offset).

[object,time]
HoldsAt(Falling(object),time) &
HoldsAt(Height(object,0),time) ->
Happens(HitsGround(object),time).

;[object,height1,height2,time]
;HoldsAt(Height(object,height1),time) &
;height1 != height2 ->
;Terminates(HitsGround(object),Height(object,height2),time).

[object,height,time]
HoldsAt(Height(object,height),time) ->
Initiates(HitsGround(object),Height(object,height),time).

[object,time]
Terminates(HitsGround(object),Falling(object),time).

object Leaf

!HoldsAt(Falling(Leaf),0).
HoldsAt(Height(Leaf,4),0).
Happens(StartFalling(Leaf),2).

completion Happens

range time 0 7
range offset 1 4
range height 0 4

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004b/RunningAndDriving1.e
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
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

fluent Tired(agent)

event Move(agent)
event Run(agent)
event Drive(agent)

[agent,time]
Happens(Move(agent),time) ->
Happens(Run(agent),time) | Happens(Drive(agent),time).

xor Run, Drive

[agent,time] Initiates(Run(agent),Tired(agent),time).

agent James

!HoldsAt(Tired(James),0).
Happens(Move(James),0).
HoldsAt(Tired(James),1).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004b/TV1.e
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
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort switch
sort tv

function TVOf(switch): tv
fluent SwitchOn(switch)
fluent TVOn(tv)
fluent PluggedIn(tv)
event TurnOn(agent,switch)
event TurnOff(agent,switch)

[agent,switch,time] Initiates(TurnOn(agent,switch),SwitchOn(switch),time).

[agent,switch,tv,time]
TVOf(switch)=tv & HoldsAt(PluggedIn(tv),time) ->
Initiates(TurnOn(agent,switch),TVOn(tv),time).

agent James
switch Switch1
tv TV1

TVOf(Switch1)=TV1.
HoldsAt(PluggedIn(TV1),0).
!HoldsAt(SwitchOn(Switch1),0).
!HoldsAt(TVOn(TV1),0).
Happens(TurnOn(James,Switch1),0).

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004b/RouletteWheel.e
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
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort wheel
sort value: integer

fluent WheelValueDeterminingFluent(wheel,value)
fluent WheelValue(wheel,value)
noninertial WheelValueDeterminingFluent
event Spin(wheel)

[wheel,value1,value2,time]
HoldsAt(WheelValue(wheel,value1),time) &
HoldsAt(WheelValue(wheel,value2),time) ->
value1=value2.

[wheel,value1,value2,time]
HoldsAt(WheelValueDeterminingFluent(wheel,value1),time) &
HoldsAt(WheelValueDeterminingFluent(wheel,value2),time) ->
value1=value2.

[wheel,value,time]
HoldsAt(WheelValueDeterminingFluent(wheel,value),time) ->
Initiates(Spin(wheel),WheelValue(wheel,value),time).

[wheel,value1,value2,time]
HoldsAt(WheelValue(wheel,value1),time) &
HoldsAt(WheelValueDeterminingFluent(wheel,value2),time) &
value1!=value2 ->
Terminates(Spin(wheel),WheelValue(wheel,value1),time).

[wheel,time]
{value} HoldsAt(WheelValueDeterminingFluent(wheel,value),time).

wheel Wheel

HoldsAt(WheelValue(Wheel,7),0).
Happens(Spin(Wheel),0).
HoldsAt(WheelValueDeterminingFluent(Wheel,7),1).

completion Happens

range value 7 10
range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2004b/PickUp.e
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
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
sort agent: object
sort physobj: object
sort location

fluent At(object,location)
fluent Holding(agent,physobj)
event PickUp(agent,physobj)
event SetDown(agent,physobj)
event Move(agent,location,location)

; state constraints

[agent,location,physobj,time]
HoldsAt(At(agent,location),time) &
HoldsAt(Holding(agent,physobj),time) ->
HoldsAt(At(physobj,location),time).

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
HoldsAt(At(object,location2),time) ->
location1=location2.

; effect axioms

[agent,location1,location2,time]
Initiates(Move(agent,location1,location2),At(agent,location2),time).

[agent,location1,location2,time]
Terminates(Move(agent,location1,location2),At(agent,location1),time).

[agent,physobj,time]
Initiates(PickUp(agent,physobj),Holding(agent,physobj),time).

[agent,physobj,time]
Terminates(SetDown(agent,physobj),Holding(agent,physobj),time).

; preconditions

[agent,location1,location2,time]
Happens(Move(agent,location1,location2),time) ->
HoldsAt(At(agent,location1),time).

[agent,physobj,time]
Happens(PickUp(agent,physobj),time) ->
{location} HoldsAt(At(agent,location),time) &
           HoldsAt(At(physobj,location),time).

; releases

[agent,physobj,location,time]
Releases(PickUp(agent,physobj),At(physobj,location),time).

[agent,physobj,location,time]
HoldsAt(At(agent,location),time) ->
Initiates(SetDown(agent,physobj),At(physobj,location),time).

;[agent,physobj,location1,location2,time]
;HoldsAt(At(agent,location1),time) &
;location1 != location2 ->
;Terminates(SetDown(agent,physobj),At(physobj,location2),time).

agent James
physobj Coin
location L1, L2, L3, L4

!HoldsAt(Holding(James,Coin),0).
HoldsAt(At(Coin,L4),0).
HoldsAt(At(James,L1),0).
Happens(Move(James,L1,L2),0).
Happens(Move(James,L2,L3),1).
Happens(Move(James,L3,L4),2).
Happens(PickUp(James,Coin),3).
Happens(Move(James,L4,L3),4).
Happens(Move(James,L3,L2),5).
Happens(SetDown(James,Coin),6).
Happens(Move(James,L2,L3),7).
Happens(Move(James,L3,L4),8).

completion Happens

range time 0 9
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/FrankEtAl2003/Story1.e
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
; @article{FrankEtAl:2003,
;   author = "Stefan L. Frank and Mathieu Koppen and Leo G. M. Noordman and Wietske Vonk",
;   year = "2003",
;   title = "Modeling knowledge-based inferences in story comprehension",
;   journal = "Cognitive Science",
;   volume = "27",
;   pages = "875--910",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort agent

load examples/FrankEtAl2003/FrankEtAl.e

agent Bob, Jilly

!HoldsAt(Raining(),0).
!HoldsAt(SunShining(),0).

(HoldsAt(PlaySoccer(Bob),1) & HoldsAt(PlaySoccer(Jilly),1)) |
(HoldsAt(PlayHideAndSeek(Bob),1) & HoldsAt(PlayHideAndSeek(Jilly),1)) |
(HoldsAt(PlayComputerGame(Bob),1) & HoldsAt(PlayComputerGame(Jilly),1)).

HoldsAt(Win(Bob),1) | HoldsAt(Win(Jilly),1).

range time 0 1
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/FrankEtAl2003/FrankEtAl.e
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
; @article{FrankEtAl:2003,
;   author = "Stefan L. Frank and Mathieu Koppen and Leo G. M. Noordman and Wietske Vonk",
;   year = "2003",
;   title = "Modeling knowledge-based inferences in story comprehension",
;   journal = "Cognitive Science",
;   volume = "27",
;   pages = "875--910",
; }
;

fluent SunShining()
fluent Raining()
fluent Outside(agent)
fluent PlaySoccer(agent)
fluent PlayHideAndSeek(agent)
fluent PlayComputerGame(agent)
fluent PlayWithDog(agent)
fluent Win(agent)

noninertial Outside, PlaySoccer, PlayHideAndSeek, PlayComputerGame
noninertial PlayWithDog, Win

xor PlaySoccer, PlayHideAndSeek, PlayComputerGame, PlayWithDog

[agent,time]
HoldsAt(PlaySoccer(agent),time) ->
HoldsAt(Outside(agent),time).

[agent,time]
HoldsAt(PlaySoccer(agent),time) ->
({agent1} agent1!=agent & HoldsAt(PlaySoccer(agent1),time)).

[agent,time]
HoldsAt(PlayHideAndSeek(agent),time) ->
({agent1} agent1!=agent & HoldsAt(PlayHideAndSeek(agent1),time)).

[agent,time]
HoldsAt(PlayComputerGame(agent),time) ->
!HoldsAt(Outside(agent),time).

[agent,time]
HoldsAt(Win(agent),time) ->
(HoldsAt(PlaySoccer(agent),time) |
 HoldsAt(PlayHideAndSeek(agent),time) |
 (HoldsAt(PlayComputerGame(agent),time) &
  ({agent1} agent1!=agent & HoldsAt(PlayComputerGame(agent1),time)))).

[agent,time]
HoldsAt(PlaySoccer(agent),time) &
HoldsAt(Win(agent),time) ->
!HoldsAt(PlaySoccer(agent),time+1).

[agent,time]
HoldsAt(PlayHideAndSeek(agent),time) &
HoldsAt(Win(agent),time) ->
!HoldsAt(PlayHideAndSeek(agent),time+1).

[agent,time]
HoldsAt(PlayComputerGame(agent),time) &
HoldsAt(Win(agent),time) ->
!HoldsAt(PlayComputerGame(agent),time+1).

[agent,time]
HoldsAt(Win(agent),time) ->
HoldsAt(PlaySoccer(agent),time-1) |
HoldsAt(PlayHideAndSeek(agent),time-1) |
HoldsAt(PlayComputerGame(agent),time-1).

[agent,time]
HoldsAt(PlaySoccer(agent),time) ->
!HoldsAt(Raining(),time).

[agent,time]
HoldsAt(Win(agent),time) ->
!({agent1} agent1!=agent & HoldsAt(Win(agent1),time)).

[agent1,agent2,time]
HoldsAt(PlayHideAndSeek(agent1),time) &
HoldsAt(PlayHideAndSeek(agent2),time) ->
((HoldsAt(Outside(agent1),time) & HoldsAt(Outside(agent2),time)) |
 (!HoldsAt(Outside(agent1),time) & !HoldsAt(Outside(agent2),time))).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/GiunchigliaEtAl2004/MonkeyPrediction.e
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
; @article{Giunchiglia:2004,
;   author = "Enrico Giunchiglia and Joohyung Lee and Vladimir Lifschitz and Norman C. McCain and Hudson Turner",
;   year = "2004",
;   title = "Nonmonotonic causal theories",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "49--104",
; }
;

; deduction

load foundations/Root.e
load foundations/EC.e
load examples/GiunchigliaEtAl2004/MonkeyBananas.e

HoldsAt(At(Monkey,L1),0).
HoldsAt(At(Bananas,L2),0).
HoldsAt(At(Box,L3),0).
Happens(Walk(L3),0).
Happens(PushBox(L2),1).

completion Happens

range time 0 2
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/GiunchigliaEtAl2004/MonkeyPlanning.e
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
; @article{Giunchiglia:2004,
;   author = "Enrico Giunchiglia and Joohyung Lee and Vladimir Lifschitz and Norman C. McCain and Hudson Turner",
;   year = "2004",
;   title = "Nonmonotonic causal theories",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "49--104",
; }
;

; planning

load foundations/Root.e
load foundations/EC.e
load examples/GiunchigliaEtAl2004/MonkeyBananas.e

HoldsAt(At(Monkey,L1),0).
HoldsAt(At(Bananas,L2),0).
HoldsAt(At(Box,L3),0).
HoldsAt(HasBananas(),4).

; PLAN Happens(Walk(L3),0).
; PLAN Happens(PushBox(L2),1).
; PLAN Happens(ClimbOn(),2).
; PLAN Happens(GraspBananas(),3).

; one event at a time
[event1,event2,time] Happens(event1,time) & Happens(event2,time) ->
event1=event2.

range time 0 4
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/GiunchigliaEtAl2004/MonkeyPostdiction.e
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
; @article{Giunchiglia:2004,
;   author = "Enrico Giunchiglia and Joohyung Lee and Vladimir Lifschitz and Norman C. McCain and Hudson Turner",
;   year = "2004",
;   title = "Nonmonotonic causal theories",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "49--104",
; }
;

; postdiction

load foundations/Root.e
load foundations/EC.e
load examples/GiunchigliaEtAl2004/MonkeyBananas.e

HoldsAt(At(Monkey,L1),0).
HoldsAt(At(Bananas,L2),0).
Happens(Walk(L3),0).
Happens(PushBox(L2),1).

completion Happens

range time 0 2
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/GiunchigliaEtAl2004/MonkeyBananas.e
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
; @article{Giunchiglia:2004,
;   author = "Enrico Giunchiglia and Joohyung Lee and Vladimir Lifschitz and Norman C. McCain and Hudson Turner",
;   year = "2004",
;   title = "Nonmonotonic causal theories",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "49--104",
; }
;

sort object
sort location

object Monkey, Bananas, Box
location L1, L2, L3

fluent At(object,location)
fluent OnBox()
fluent HasBananas()

event Walk(location)
event PushBox(location)
event ClimbOn()
event ClimbOff()
event GraspBananas()

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
HoldsAt(At(object,location2),time) ->
location1=location2.

[object,location,time]
object=Monkey ->
Initiates(Walk(location),At(object,location),time).

[object,location1,location2,time]
object=Monkey &
HoldsAt(At(object,location1),time) ->
Terminates(Walk(location2),At(object,location1),time).

[location,time]
Happens(Walk(location),time) ->
!HoldsAt(At(Monkey,location),time) &
!HoldsAt(OnBox(),time).

[location,time]
HoldsAt(HasBananas(),time) &
HoldsAt(At(Monkey,location),time) ->
HoldsAt(At(Bananas,location),time).

[object,location,time]
object=Box | object=Monkey ->
Initiates(PushBox(location),At(object,location),time).

[object,location1,location2,time]
(object=Box | object=Monkey) &
HoldsAt(At(object,location1),time) ->
Terminates(PushBox(location2),At(object,location1),time).

[location,time]
Happens(PushBox(location),time) ->
({location1}
  HoldsAt(At(Box,location1),time) &
  HoldsAt(At(Monkey,location1),time)) &
!HoldsAt(At(Monkey,location),time) &
!HoldsAt(OnBox(),time).

[time] Initiates(ClimbOn(),OnBox(),time).

[time]
Happens(ClimbOn(),time) ->
!HoldsAt(OnBox(),time).

[time] Terminates(ClimbOff(),OnBox(),time).

[time]
Happens(ClimbOff(),time) ->
HoldsAt(OnBox(),time).

[time] Initiates(GraspBananas(),HasBananas(),time).

[object,location,time]
object=Bananas ->
Releases(GraspBananas(),At(object,location),time).

[time]
Happens(GraspBananas(),time) ->
({location1}
  HoldsAt(At(Bananas,location1),time) &
  HoldsAt(At(Monkey,location1),time)) &
HoldsAt(OnBox(),time).

[time]
HoldsAt(OnBox(),time) ->
{location1} HoldsAt(At(Box,location1),time) &
            HoldsAt(At(Monkey,location1),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Antoniou1997/Student.e
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
; conflicting defaults: method (D)
; \fullciteA[p. 157]{Antoniou:1997}
;
; @book{Antoniou:1997,
;   author = "Grigoris Antoniou",
;   year = "1997",
;   title = "Nonmonotonic Reasoning",
;   address = "Cambridge, MA",
;   publisher = "MIT Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort x

predicate Adult(x)
predicate Student(x)
predicate Employed(x)
predicate Ab1(x)
predicate Ab2(x)

x Mary

Student(Mary).

[x] Adult(x) & !Ab1(x) -> Employed(x).
[x] Student(x) & !Ab2(x) -> !Employed(x).
[x] Student(x) -> Adult(x).
Theta: [x] Student(x) -> Ab1(x).

range time 0 0
range offset 1 1

completion Theta Ab1
completion Theta Ab2

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Antoniou1997/Dropout.e
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
; dealing with conflicting defaults by adding conditions
; to one of the conflicting rules
; \fullciteA[p. 56]{Antoniou:1997}
;
; @book{Antoniou:1997,
;   author = "Grigoris Antoniou",
;   year = "1997",
;   title = "Nonmonotonic Reasoning",
;   address = "Cambridge, MA",
;   publisher = "MIT Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort x

predicate Dropout(x)
predicate Adult(x)
predicate Employed(x)
predicate Ab1(x)
predicate Ab2(x)

x Bill

Dropout(Bill).

[x] Dropout(x) & !Ab1(x) -> Adult(x).
[x] Adult(x) & !Dropout(x) & !Ab2(x) -> Employed(x).

range time 0 0
range offset 1 1

completion Theta Ab1
completion Theta Ab2

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1999/Happy.e
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
; @incollection{Shanahan:1999,
;   author = "Shanahan, Murray",
;   year = "1999",
;   title = "The Event Calculus explained",
;   editor = "Michael J. Wooldridge and Manuela M. Veloso",
;   booktitle = "Artificial Intelligence Today: Recent Trends and Developments",
;   series = "Lecture Notes in Computer Science",
;   volume = "1600",
;   pages = "409--430",
;   address = "Berlin",
;   publisher = "Springer",
; }
;
; deduction
;
; modifications from Shanahan's formulation:
; InitiallyN -> !HoldsAt
; InitiallyP -> HoldsAt
; timestamps
;

load foundations/Root.e
load foundations/EC.e

sort person
event Feed(person)
event Clothe(person)
fluent Happy(person)
fluent Hungry(person)
fluent Cold(person)
noninertial Happy

[person,time]
HoldsAt(Happy(person),time) <->
!HoldsAt(Hungry(person),time) &
!HoldsAt(Cold(person),time).

[person,time]
Terminates(Feed(person),Hungry(person),time).

[person,time]
Terminates(Clothe(person),Cold(person),time).

person Fred

HoldsAt(Hungry(Fred),0).
!HoldsAt(Cold(Fred),0).
Happens(Feed(Fred),1).

completion Happens

range time 0 2
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1999/ThielscherCircuit.e
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
; @article{Thielscher:1997,
;   author = "Michael Thielscher",
;   year = "1997",
;   title = "Ramification and causality",
;   journal = "Artificial Intelligence",
;   volume = "89",
;   pages = "317--364",
; }
;
; @incollection{Shanahan:1999,
;   author = "Shanahan, Murray",
;   year = "1999",
;   title = "The Event Calculus explained",
;   editor = "Michael J. Wooldridge and Manuela M. Veloso",
;   booktitle = "Artificial Intelligence Today: Recent Trends and Developments",
;   series = "Lecture Notes in Computer Science",
;   volume = "1600",
;   pages = "409--430",
;   address = "Berlin",
;   publisher = "Springer",
; }
;
; deduction
;
; modifications from Shanahan's formulation:
; timestamps
;

load foundations/Root.e
load foundations/EC.e
load foundations/ECCausal.e

event LightOn()
event Close1()
event Open2()
event CloseRelay()

fluent Light()
fluent Switch1()
fluent Switch2()
fluent Switch3()
fluent Relay()

[time]
Stopped(Light(),time) &
Initiated(Switch1(),time) &
Initiated(Switch2(),time) ->
Happens(LightOn(),time).

[time]
Started(Switch2(),time) &
Initiated(Relay(),time) ->
Happens(Open2(),time).

[time]
Stopped(Relay(),time) &
Initiated(Switch1(),time) &
Initiated(Switch3(),time) ->
Happens(CloseRelay(),time).

[time] Initiates(LightOn(),Light(),time).

[time] Terminates(Open2(),Switch2(),time).

[time] Initiates(CloseRelay(),Relay(),time).

[time] Initiates(Close1(),Switch1(),time).

!HoldsAt(Switch1(),0).
HoldsAt(Switch2(),0).
HoldsAt(Switch3(),0).
!HoldsAt(Relay(),0).
!HoldsAt(Light(),0).

Happens(Close1(),0).

completion Happens

range time 0 1
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1999/CoinToss.e
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
; @article{Kartha:1994,
;   author = "G. Neelakantan Kartha",
;   year = "1994",
;   title = "Two counterexamples related to \uppercase{B}aker's approach to the frame problem",
;   journal = "Artificial Intelligence",
;   volume = "69",
;   number = "1--2",
;   pages = "379--391",
; }
;
; @incollection{Shanahan:1999,
;   author = "Shanahan, Murray",
;   year = "1999",
;   title = "The Event Calculus explained",
;   editor = "Michael J. Wooldridge and Manuela M. Veloso",
;   booktitle = "Artificial Intelligence Today: Recent Trends and Developments",
;   series = "Lecture Notes in Computer Science",
;   volume = "1600",
;   pages = "409--430",
;   address = "Berlin",
;   publisher = "Springer",
; }
;
; model finding
;
; modifications from Shanahan's formulation:
; InitiallyP -> HoldsAt
; pruning of models irrelevant to example
; timestamps
;

load foundations/Root.e
load foundations/EC.e

event Toss()
fluent ItsHeads()
fluent Heads()
noninertial ItsHeads

[time] HoldsAt(ItsHeads(),time) -> Initiates(Toss(),Heads(),time).
[time] !HoldsAt(ItsHeads(),time) -> Terminates(Toss(),Heads(),time).

HoldsAt(Heads(),0).
Happens(Toss(),1).
Happens(Toss(),2).
Happens(Toss(),3).

; prune models irrelevant to example:
HoldsAt(ItsHeads(),0).
HoldsAt(ItsHeads(),4).

completion Happens

range time 0 4
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1999/ChessBoard.e
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
; due to Raymond Reiter
;
; @inproceedings{KarthaLifschitz:1994,
;   author = "G. Neelakantan Kartha and Vladimir Lifschitz",
;   year = "1994",
;   title = "Actions with indirect effects (preliminary report)",
;   editor = "Jon Doyle and Erik Sandewall and Pietro Torasso",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{F}ourth \uppercase{I}nternational \uppercase{C}onference on \uppercase{P}rinciples of \uppercase{K}nowledge \uppercase{R}epresentation and \uppercase{R}easoning",
;   pages = "341--350",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann",
; }
;
; @incollection{Shanahan:1999,
;   author = "Shanahan, Murray",
;   year = "1999",
;   title = "The Event Calculus explained",
;   editor = "Michael J. Wooldridge and Manuela M. Veloso",
;   booktitle = "Artificial Intelligence Today: Recent Trends and Developments",
;   series = "Lecture Notes in Computer Science",
;   volume = "1600",
;   pages = "409--430",
;   address = "Berlin",
;   publisher = "Springer",
; }
;
; model finding
;
; modifications from Shanahan's formulation:
; InitiallyN -> !HoldsAt
; pruning of models irrelevant to example
; timestamps
;

load foundations/Root.e
load foundations/EC.e

event Throw()
fluent ItsBlack()
fluent ItsWhite()
fluent OnBlack()
fluent OnWhite()
noninertial ItsBlack, ItsWhite

[time]
HoldsAt(ItsWhite(),time) ->
Initiates(Throw(),OnWhite(),time).

[time]
HoldsAt(ItsBlack(),time) ->
Initiates(Throw(),OnBlack(),time).

[time] HoldsAt(ItsWhite(),time) | HoldsAt(ItsBlack(),time).

!HoldsAt(OnWhite(),0).
!HoldsAt(OnBlack(),0).
Happens(Throw(),1).

; prune models irrelevant to example:
HoldsAt(ItsWhite(),0).
HoldsAt(ItsBlack(),0).
HoldsAt(ItsWhite(),2).
HoldsAt(ItsBlack(),2).

completion Happens

range time 0 2
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Shanahan1999/RussianTurkey.e
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
; @book{Sandewall:1994,
;   author = "Sandewall, Erik",
;   year = "1994",
;   title = "Features and Fluents: The Representation of Knowledge about Dynamical Systems",
;   volume = "I",
;   address = "Oxford",
;   publisher = "Oxford University Press",
; }
;
; @incollection{Shanahan:1999,
;   author = "Shanahan, Murray",
;   year = "1999",
;   title = "The Event Calculus explained",
;   editor = "Michael J. Wooldridge and Manuela M. Veloso",
;   booktitle = "Artificial Intelligence Today: Recent Trends and Developments",
;   series = "Lecture Notes in Computer Science",
;   volume = "1600",
;   pages = "409--430",
;   address = "Berlin",
;   publisher = "Springer",
; }
;
; model finding
;
; modifications from Shanahan's formulation:
; InitiallyP -> HoldsAt
; added [time] Terminates(Shoot(),Loaded(),time).
; added !HoldsAt(Loaded(),0) to prune models
; timestamps
;

load foundations/Root.e
load foundations/EC.e

event Load()
event Shoot()
event Spin()
fluent Loaded()
fluent Alive()

[time] Initiates(Load(),Loaded(),time).
[time] HoldsAt(Loaded(),time) -> Terminates(Shoot(),Alive(),time).
[time] Releases(Spin(),Loaded(),time).
[time] Terminates(Shoot(),Loaded(),time).

HoldsAt(Alive(),0).
!HoldsAt(Loaded(),0).
Happens(Load(),1).
Happens(Spin(),2).
Happens(Shoot(),3).

completion Happens

range time 0 4
range offset 1 1

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/AkmanEtAl2004/ZooTest4.2.e
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
; @article{Akman:2004,
;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
;   year = "2004",
;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "105--140",
; }
;

option encoding 3

load foundations/Root.e
load foundations/EC.e
load examples/AkmanEtAl2004/ZooWorld.e

human Homer
elephant Jumbo

Species(Homer)=HumanSpecies.
Adult(Homer).
Species(Jumbo)=ElephantSpecies.
Adult(Jumbo).

!HoldsAt(Opened(GateAO),0).
{position} HoldsAt(Pos(Homer,position),0) & Outside=Loc(position).
{position} HoldsAt(Pos(Jumbo,position),0) & CageA=Loc(position).

{position} HoldsAt(Pos(Homer,position),5) & CageA=Loc(position).
{position} HoldsAt(Pos(Jumbo,position),5) & Outside=Loc(position).

[animal,time] !HoldsAt(Mounted(Homer,animal),time).

[human] HoldsAt(PosDeterminingFluent(human,1),5).
[event,animal] !HoldsAt(DoneBy(event,animal),5).

;HoldsAt(Pos(Homer,7),0).
;HoldsAt(Pos(Jumbo,4),0).
;Happens(Move(Jumbo,3),0).
;Happens(Open(Homer,GateAO),0).
;Happens(Move(Homer,4),1).
;Happens(Move(Jumbo,1),1).
;Happens(Move(Jumbo,3),2).
;Happens(Mount(Homer,Jumbo),2).
;Happens(Move(Jumbo,4),3).
;!Happens(Move(Homer,2),3).
;Happens(Move(Jumbo,7),4).
;!Happens(Mount(Homer,Jumbo),3).
;!Happens(Mount(Homer,Jumbo),4).
;[position] !Happens(Move(Homer,position),4).

range time 0 5
range position 1 8
range offset 0 0

completion Happens

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/AkmanEtAl2004/ZooTest5.1.e
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
; @article{Akman:2004,
;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
;   year = "2004",
;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "105--140",
; }
;

option encoding 3

load foundations/Root.e
load foundations/EC.e
load examples/AkmanEtAl2004/ZooWorld.e

human Homer
elephant Jumbo
horse Silver

Species(Homer)=HumanSpecies.
Adult(Homer).
Species(Jumbo)=ElephantSpecies.
Adult(Jumbo).
Species(Silver)=HorseSpecies.
Adult(Silver).

{position}
!HoldsAt(Pos(Homer,position),0) &
HoldsAt(Pos(Jumbo,position),0) &
HoldsAt(Pos(Homer,position),1) &
!HoldsAt(Pos(Jumbo,position),1).
HoldsAt(Mounted(Homer,Silver),0).

option manualrelease on
[human, animal] !ReleasedAt(Mounted(human, animal),0).
[gate] !ReleasedAt(Opened(gate),0).
[position] ReleasedAt(Pos(Homer,position),0).
[position] !ReleasedAt(Pos(Jumbo,position),0).
[position] !ReleasedAt(Pos(Silver,position),0).

[human] HoldsAt(PosDeterminingFluent(human,1),1).
[event,animal] !HoldsAt(DoneBy(event,animal),1).

;HoldsAt(Opened(GateAO),0).
;HoldsAt(Pos(Homer,3),0).
;HoldsAt(Pos(Jumbo,2),0).
;HoldsAt(Pos(Silver,3),0).
;Happens(Move(Jumbo,4),0).
;Happens(ThrowOff(Silver,Homer),0).
;HoldsAt(PosDeterminingFluent(Homer,2),0).

range time 0 1
range position 1 8
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/AkmanEtAl2004/ZooTest3.e
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
; @article{Akman:2004,
;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
;   year = "2004",
;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "105--140",
; }
;

option encoding 3

load foundations/Root.e
load foundations/EC.e
load examples/AkmanEtAl2004/ZooWorld.e

human Homer
dog Snoopy

Species(Homer)=HumanSpecies.
Adult(Homer).
Species(Snoopy)=DogSpecies.
Adult(Snoopy).

!HoldsAt(Opened(GateAO),0).
{position} HoldsAt(Pos(Homer,position),0) & Outside=Loc(position).
{position} HoldsAt(Pos(Snoopy,position),0) & CageA=Loc(position).

{position} HoldsAt(Pos(Homer,position),2) & CageA=Loc(position).
{position} HoldsAt(Pos(Snoopy,position),2) & Outside=Loc(position).

[human] HoldsAt(PosDeterminingFluent(human,1),2).
[event,animal] !HoldsAt(DoneBy(event,animal),2).

range time 0 2
range position 1 8
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/AkmanEtAl2004/ZooWorld.e
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
; @article{Akman:2004,
;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
;   year = "2004",
;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "105--140",
; }
;

sort position: integer
sort location
sort cage: location
sort gate
sort animal
sort elephant: animal
sort horse: animal
sort dog: animal
sort human: animal
sort species

function Loc(position): location
function Side1(gate): position
function Side2(gate): position
function Species(animal): species

predicate Accessible(position,position,time)
predicate Adult(animal)
predicate Large(animal)
predicate LargeSpecies(species)
predicate Neighbor(position,position)
predicate Sides(position,position,gate)

event Close(human,gate)
event GetOff(human,animal)
event Mount(human,animal)
event Move(animal,position)
event Open(human,gate)
event ThrowOff(animal,human)

fluent AbnormalEncroachment(human)
noninertial AbnormalEncroachment
fluent DoneBy(event,animal)
noninertial DoneBy
fluent Mounted(human,animal)
fluent MountFails(human)
noninertial MountFails
fluent Moves(animal)
noninertial Moves
fluent Opened(gate)
fluent Pos(animal,position)
fluent PosDeterminingFluent(human,position)
noninertial PosDeterminingFluent
fluent ThrowOffFails(animal,human)
noninertial ThrowOffFails

species HumanSpecies, ElephantSpecies, HorseSpecies, DogSpecies
location Outside

LargeSpecies(HumanSpecies).
LargeSpecies(ElephantSpecies).
LargeSpecies(HorseSpecies).
!LargeSpecies(DogSpecies).

[event,animal,time]
HoldsAt(DoneBy(event,animal),time) <->
(Happens(event,time) &
 (({gate} event=Close(animal,gate)) |
  ({animal1} event=GetOff(animal,animal1))|
  ({animal1} event=Mount(animal,animal1))|
  ({position} event=Move(animal,position))|
  ({gate} event=Open(animal,gate)) |
  ({human1} event=ThrowOff(animal,human1)))).

[event1,event2,animal,time]
HoldsAt(DoneBy(event1,animal),time) &
HoldsAt(DoneBy(event2,animal),time) ->
event1=event2.

[animal] Large(animal) <-> (Adult(animal) & LargeSpecies(Species(animal))).

[position] {position1} position1!=position & Neighbor(position,position1).

[position] !Neighbor(position,position).

[position1,position2]
Neighbor(position1,position2) ->
Neighbor(position2,position1).

[cage] cage!=Outside.

[position1,position2,gate]
Sides(position1,position2,gate) <->
((Side1(gate)=position1 &
  Side2(gate)=position2) |
 (Side2(gate)=position1 &
  Side1(gate)=position2)).

[gate] Loc(Side1(gate))!=Loc(Side2(gate)).

[position1,position2,gate1,gate2]
Sides(position1,position2,gate1) &
Sides(position1,position2,gate2) ->
gate1=gate2.

[position1,position2,gate]
Sides(position1,position2,gate) ->
Neighbor(position1,position2).

[position1,position2]
Loc(position1) != Loc(position2) &
Neighbor(position1,position2) ->
{gate} Sides(position1,position2,gate).

[animal,position1,position2,time]
HoldsAt(Pos(animal,position1),time) &
HoldsAt(Pos(animal,position2),time) ->
position1=position2.

[animal,time]
{position} HoldsAt(Pos(animal,position),time).

[animal1,animal2,position,time]
(animal1!=animal2 &
 Large(animal1) &
 Large(animal2) &
 HoldsAt(Pos(animal1,position),time) &
 HoldsAt(Pos(animal2,position),time)) ->
(({human} human=animal1 & HoldsAt(Mounted(human,animal2),time)) |
 ({human} human=animal2 & HoldsAt(Mounted(human,animal1),time))).

[human,position1,position2,time]
HoldsAt(PosDeterminingFluent(human,position1),time) &
HoldsAt(PosDeterminingFluent(human,position2),time) ->
position1=position2.

[animal,position,time]
Initiates(Move(animal,position),Pos(animal,position),time).

[animal,position1,position2,time]
HoldsAt(Pos(animal,position1),time) ->
Terminates(Move(animal,position2),Pos(animal,position1),time).

[animal,position,time]
Happens(Move(animal,position),time) ->
!HoldsAt(Pos(animal,position),time).

[human,position,time]
Happens(Move(human,position),time) ->
!{animal} HoldsAt(Mounted(human,animal),time).

[human,gate,time]
Initiates(Open(human,gate),Opened(gate),time).

[human,gate,time]
Happens(Open(human,gate),time) ->
!HoldsAt(Opened(gate),time) &
(!{animal} HoldsAt(Mounted(human,animal),time)) &
({position}
 (Side1(gate)=position | Side2(gate)=position) &
 HoldsAt(Pos(human,position),time)).

[human,gate,time]
Terminates(Close(human,gate),Opened(gate),time).

[human,gate,time]
Happens(Close(human,gate),time) ->
HoldsAt(Opened(gate),time) &
(!{animal} HoldsAt(Mounted(human,animal),time)) &
{position}
(Side1(gate)=position | Side2(gate)=position) &
HoldsAt(Pos(human,position),time).

[human,animal,position,time]
HoldsAt(Mounted(human,animal),time) &
HoldsAt(Pos(animal,position),time) ->
HoldsAt(Pos(human,position),time).

[animal,time]
HoldsAt(Moves(animal),time) <->
({position}
 HoldsAt(Pos(animal,position),time) &
 !HoldsAt(Pos(animal,position),time+1)).

[human,time]
HoldsAt(MountFails(human),time) <->
({animal}
  Happens(Mount(human,animal),time) &
  HoldsAt(Moves(animal),time)).

[human,animal,position,time]
!HoldsAt(Moves(animal),time) ->
Releases(Mount(human,animal),Pos(human,position),time).

[human,animal,time]
!HoldsAt(Moves(animal),time) ->
Initiates(Mount(human,animal),Mounted(human,animal),time).

[human,animal,position,time]
HoldsAt(Pos(animal,position),time) &
HoldsAt(Moves(animal),time) ->
Initiates(Mount(human,animal),Pos(human,position),time).

[human,animal,position,time]
HoldsAt(Pos(human,position),time) &
HoldsAt(Moves(animal),time) ->
Terminates(Mount(human,animal),Pos(human,position),time).

[human,animal,time]
Happens(Mount(human,animal),time) ->
Large(animal).

[human,animal,time]
HoldsAt(Mounted(human,animal),time) ->
Large(animal).

[human1,human2,time]
Happens(Mount(human1,human2),time) ->
!Large(human1).

[human1,human2,time]
HoldsAt(Mounted(human1,human2),time) ->
!Large(human1).

[human,animal,time]
Happens(Mount(human,animal),time) ->
!{human1} human1!=human & HoldsAt(Mounted(human1,animal),time).

[human1,human2,animal,time]
HoldsAt(Mounted(human1,animal),time) &
HoldsAt(Mounted(human2,animal),time) ->
human1=human2.

[human,animal,time]
Happens(Mount(human,animal),time) ->
!{human1} human1!=human & HoldsAt(Mounted(human1,human),time).

[human1,human2,time]
Happens(Mount(human1,human2),time) ->
{animal} HoldsAt(Mounted(human2,animal),time).

[human1,human2,time]
HoldsAt(Mounted(human1,human2),time) ->
!{animal} HoldsAt(Mounted(human2,animal),time).

[human,animal,time]
Happens(Mount(human,animal),time) ->
!{animal1} HoldsAt(Mounted(human,animal1),time).

[human,animal,time]
!HoldsAt(Moves(animal),time) ->
Terminates(GetOff(human,animal),Mounted(human,animal),time).

[human,animal,position,time]
!HoldsAt(Moves(animal),time) &
HoldsAt(PosDeterminingFluent(human,position),time) ->
Initiates(GetOff(human,animal),Pos(human,position),time).

[human,animal,position,time]
!HoldsAt(Moves(animal),time) &
HoldsAt(Pos(human,position),time) ->
Terminates(GetOff(human,animal),Pos(human,position),time).

[human,animal,position1,position2,time]
!HoldsAt(Moves(animal),time) &
HoldsAt(Pos(human,position1),time) &
position1!=position2 ->
Terminates(GetOff(human,animal),Pos(human,position2),time).

[human,animal,time]
Happens(GetOff(human,animal),time) ->
HoldsAt(Mounted(human,animal),time).

[animal1,human,time]
HoldsAt(ThrowOffFails(animal1,human),time) <->
({position,animal2}
 animal2!=human &
 HoldsAt(PosDeterminingFluent(human,position),time) &
 Large(animal2) &
 HoldsAt(Pos(animal2,position),time+1)).

[animal,human,position,time]
HoldsAt(PosDeterminingFluent(human,position),time) &
!HoldsAt(ThrowOffFails(animal,human),time) ->
Initiates(ThrowOff(animal,human),Pos(human,position),time).

[animal,human,position,time]
HoldsAt(Pos(human,position),time) &
!HoldsAt(ThrowOffFails(animal,human),time) ->
Terminates(ThrowOff(animal,human),Pos(human,position),time).

[animal,human,position1,position2,time]
!HoldsAt(ThrowOffFails(animal,human),time) &
HoldsAt(Pos(human,position1),time) &
!HoldsAt(PosDeterminingFluent(human,position2),time) &
position1!=position2 ->
Terminates(ThrowOff(animal,human),Pos(human,position2),time).

[human,time]
(!{animal} Happens(ThrowOff(animal,human),time) |
           Happens(GetOff(human,animal),time)) ->
HoldsAt(PosDeterminingFluent(human,1),time).

[human,position,animal1,animal2,time]
HoldsAt(PosDeterminingFluent(human,position),time) &
HoldsAt(ThrowOffFails(animal1,human),time) &
HoldsAt(Pos(animal2,position),time) ->
Initiates(ThrowOff(animal1,human),Mounted(human,animal2),time).

[human,animal,time]
!HoldsAt(ThrowOffFails(animal,human),time) ->
Terminates(ThrowOff(animal,human),Mounted(human,animal),time).

[animal,human,time]
Happens(ThrowOff(animal,human),time) ->
HoldsAt(Mounted(human,animal),time).

[animal,human,time]
Happens(ThrowOff(animal,human),time) ->
!Happens(GetOff(human,animal),time).

[animal,human,time]
Happens(GetOff(human,animal),time) ->
!Happens(ThrowOff(animal,human),time).

[position1,position2,time]
Accessible(position1,position2,time) <->
(Neighbor(position1,position2) &
 !{gate} Sides(position1,position2,gate) &
         !HoldsAt(Opened(gate),time)).

[animal,position1,position2,time]
(position1!=position2 &
 HoldsAt(Pos(animal,position1),time) &
 HoldsAt(Pos(animal,position2),time+1)) ->
Accessible(position1,position2,time).

[human,time]
HoldsAt(AbnormalEncroachment(human),time) <->
(HoldsAt(MountFails(human),time) |
 ({position,animal1,animal2}
   HoldsAt(PosDeterminingFluent(human,position),time) &
   !HoldsAt(ThrowOffFails(animal2,human),time) &
   Happens(ThrowOff(animal2,human),time) &
   animal1!=human &
   Large(animal1) &
   HoldsAt(Pos(animal1,position),time) &
   !HoldsAt(Pos(animal1,position),time+1))).

[animal1,animal2,position,time]
HoldsAt(Pos(animal1,position),time) &
!HoldsAt(Pos(animal1,position),time+1) &
!HoldsAt(Pos(animal2,position),time) &
HoldsAt(Pos(animal2,position),time+1) ->
(!Large(animal1) |
 !Large(animal2) |
 ({human} human=animal2 & HoldsAt(AbnormalEncroachment(human),time))).

[animal1,animal2,position1,position2,time]
animal1!=animal2 &
Large(animal1) & Large(animal2) &
HoldsAt(Pos(animal1,position1),time) &
HoldsAt(Pos(animal1,position2),time+1) &
HoldsAt(Pos(animal2,position1),time) &
HoldsAt(Pos(animal2,position2),time+1) ->
!{gate} Sides(position1,position2,gate).

[animal1,animal2,position1,position2,time]
animal1!=animal2 &
Large(animal1) & Large(animal2) &
HoldsAt(Pos(animal1,position1),time) &
HoldsAt(Pos(animal1,position2),time+1) &
HoldsAt(Pos(animal2,position2),time) &
HoldsAt(Pos(animal2,position1),time+1) ->
!{gate} Sides(position1,position2,gate).

[gate,position1,position2,time]
HoldsAt(Opened(gate),time) &
!HoldsAt(Opened(gate),time+1) &
Sides(position1,position2,gate) ->
!{animal}
HoldsAt(Pos(animal,position1),time) &
HoldsAt(Pos(animal,position2),time+1).

gate GateAO
cage CageA

Loc(1)=CageA.
Loc(2)=CageA.
Loc(3)=CageA.
Loc(4)=CageA.
Loc(5)=Outside.
Loc(6)=Outside.
Loc(7)=Outside.
Loc(8)=Outside.

[position1,position2]
Neighbor(position1,position2) <->
((position1=1 & position2=2) |
 (position1=1 & position2=3) |
 (position1=1 & position2=4) |
 (position1=2 & position2=3) |
 (position1=2 & position2=4) |
 (position1=3 & position2=4) |
 (position1=5 & position2=6) |
 (position1=5 & position2=7) |
 (position1=5 & position2=8) |
 (position1=6 & position2=7) |
 (position1=6 & position2=8) |
 (position1=7 & position2=8) |
 (position2=1 & position1=2) |
 (position2=1 & position1=3) |
 (position2=1 & position1=4) |
 (position2=2 & position1=3) |
 (position2=2 & position1=4) |
 (position2=3 & position1=4) |
 (position2=5 & position1=6) |
 (position2=5 & position1=7) |
 (position2=5 & position1=8) |
 (position2=6 & position1=7) |
 (position2=6 & position1=8) |
 (position2=7 & position1=8) |
 (position1=4 & position2=7) |
 (position2=4 & position1=7)).

Side1(GateAO)=4.
Side2(GateAO)=7.

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/AkmanEtAl2004/ZooTest4.1.e
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
; @article{Akman:2004,
;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
;   year = "2004",
;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "105--140",
; }
;

option encoding 3

load foundations/Root.e
load foundations/EC.e
load examples/AkmanEtAl2004/ZooWorld.e

human Homer
elephant Jumbo

Species(Homer)=HumanSpecies.
Adult(Homer).
Species(Jumbo)=ElephantSpecies.
Adult(Jumbo).

!HoldsAt(Opened(GateAO),0).
{position} HoldsAt(Pos(Homer,position),0) & Outside=Loc(position).
{position} HoldsAt(Pos(Jumbo,position),0) & CageA=Loc(position).

{position} HoldsAt(Pos(Homer,position),4) & CageA=Loc(position).
{position} HoldsAt(Pos(Jumbo,position),4) & Outside=Loc(position).

[human] HoldsAt(PosDeterminingFluent(human,1),4).
[event,animal] !HoldsAt(DoneBy(event,animal),4).

; ccalc.2.0b.8.3 single model
;HoldsAt(Pos(Homer,7),0).
;HoldsAt(Pos(Jumbo,2),0).
;Happens(Move(Jumbo,4),0).
;Happens(Open(Homer,GateAO),0).
;Happens(Mount(Homer,Jumbo),1).
;Happens(ThrowOff(Jumbo,Homer),2).
;HoldsAt(PosDeterminingFluent(Homer,1),2).
;Happens(Move(Jumbo,7),3).
;Happens(Mount(Homer,Jumbo),3).

range time 0 4
range position 1 8
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/AkmanEtAl2004/ZooTest2.e
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
; @article{Akman:2004,
;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
;   year = "2004",
;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "105--140",
; }
;

option encoding 3

load foundations/Root.e
load foundations/EC.e
load examples/AkmanEtAl2004/ZooWorld.e

human Homer

Species(Homer)=HumanSpecies.
Adult(Homer).

!HoldsAt(Opened(GateAO),0).
{position} HoldsAt(Pos(Homer,position),0) & Outside=Loc(position).
{position} HoldsAt(Pos(Homer,position),2) & CageA=Loc(position).

[human] HoldsAt(PosDeterminingFluent(human,1),2).
[event,animal] !HoldsAt(DoneBy(event,animal),2).

range time 0 2
range position 1 8
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/AkmanEtAl2004/ZooTest6.e
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
; @article{Akman:2004,
;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
;   year = "2004",
;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "105--140",
; }
;

option encoding 3

load foundations/Root.e
load foundations/EC.e
load examples/AkmanEtAl2004/ZooWorld.e

human Homer
elephant Jumbo

Species(Homer)=HumanSpecies.
Adult(Homer).
Species(Jumbo)=ElephantSpecies.
Adult(Jumbo).

HoldsAt(Mounted(Homer,Jumbo),0).
HoldsAt(Pos(Jumbo,1),0).
Happens(ThrowOff(Jumbo,Homer),0).

option manualrelease on
[human, animal] !ReleasedAt(Mounted(human, animal),0).
[gate] !ReleasedAt(Opened(gate),0).
[position] ReleasedAt(Pos(Homer,position),0).
[position] !ReleasedAt(Pos(Jumbo,position),0).

[human] HoldsAt(PosDeterminingFluent(human,1),1).
[event,animal] !HoldsAt(DoneBy(event,animal),1).

range time 0 1
range position 1 8
range offset 0 0

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/AkmanEtAl2004/ZooTest1.e
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
; @article{Akman:2004,
;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
;   year = "2004",
;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "105--140",
; }
;

option encoding 3

load foundations/Root.e
load foundations/EC.e
load examples/AkmanEtAl2004/ZooWorld.e

human Homer
elephant Jumbo

Species(Homer)=HumanSpecies.
Adult(Homer).
Species(Jumbo)=ElephantSpecies.
Adult(Jumbo).

!HoldsAt(Opened(GateAO),0).
HoldsAt(Pos(Homer,6),0).
[time] HoldsAt(Pos(Jumbo,1),time).

; goal
HoldsAt(Mounted(Homer,Jumbo),4).

;ABDUCE
;Happens(Move(Homer,7),0).
;Happens(Open(Homer,GateAO),1).
;Happens(Move(Homer,4),2).
;Happens(Mount(Homer,Jumbo),3).

[human] HoldsAt(PosDeterminingFluent(human,1),4).
[event,animal] !HoldsAt(DoneBy(event,animal),4).

range time 0 4
range position 1 8
range offset 0 0

option timediff off
option modeldiff on

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/AkmanEtAl2004/ZooTest5.2.e
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
; @article{Akman:2004,
;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
;   year = "2004",
;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "105--140",
; }
;

option encoding 3

load foundations/Root.e
load foundations/EC.e
load examples/AkmanEtAl2004/ZooWorld.e

human Homer
elephant Jumbo
horse Silver

Species(Homer)=HumanSpecies.
Adult(Homer).
Species(Jumbo)=ElephantSpecies.
Adult(Jumbo).
Species(Silver)=HorseSpecies.
Adult(Silver).

{position}
!HoldsAt(Pos(Homer,position),0) &
HoldsAt(Pos(Jumbo,position),0) &
HoldsAt(Pos(Homer,position),1) &
!HoldsAt(Pos(Jumbo,position),1).
[animal,time] !Happens(ThrowOff(animal,Homer),time).

[human] HoldsAt(PosDeterminingFluent(human,1),1).
[event,animal] !HoldsAt(DoneBy(event,animal),1).

;HoldsAt(Opened(GateAO),0).
;HoldsAt(Pos(Homer,3),0).
;HoldsAt(Pos(Jumbo,2),0).
;HoldsAt(Pos(Silver,7),0).
;Happens(Move(Jumbo,4),0).
;Happens(Move(Silver,8),0).
;Happens(Mount(Homer,Jumbo),0).

range time 0 1
range position 1 8
range offset 0 0

; End of file.
