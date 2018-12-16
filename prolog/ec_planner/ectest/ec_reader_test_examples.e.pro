% ectest/ec_reader_test_examples.e:1
% translate: begining  File: ectest/ec_reader_test_examples.e.pro 
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2004a/Holding.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{Mueller:2004a,
% ;   author = "Erik T. Mueller",
% ;   year = "2004",
% ;   title = "Event calculus reasoning through satisfiability",
% ;   journal = "Journal of Logic and Computation",
% ;   volume = "14",
% ;   number = "5",
% ;   pages = "703--730",
% ; }
% ;
% ectest/ec_reader_test_examples.e:27
% 
% option encoding 3
option(encoding, 3).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% ectest/ec_reader_test_examples.e:33
% sort person
sort(person).


% sort object
sort(object).


% 
% event Hold(person,object)
event(hold(person, object)).


% fluent Holding(person,object)
fluent(holding(person, object)).


% 
% ectest/ec_reader_test_examples.e:39
% person P1
t(person, p1).


% object O1
t(object, o1).


% 
% Happens(Hold(P1,O1),0).
happens(hold(p1, o1), 0).


% 
% 
% ectest/ec_reader_test_examples.e:44
% [person,object,time]% 
% Initiates(Hold(person,object),Holding(person,object),time).
initiates(hold(Person, Object), holding(Person, Object), Time).


% 
% 
% !HoldsAt(Holding(P1,O1),0).
not(holds_at(holding(p1, o1), 0)).


% 
% ;;; AUTO !ReleasedAt(Holding(P1,O1),0).
% 
% ectest/ec_reader_test_examples.e:50
% completion Happens
completion(happens).


% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:56
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2004a/Leaf.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{Mueller:2004a,
% ;   author = "Erik T. Mueller",
% ;   year = "2004",
% ;   title = "Event calculus reasoning through satisfiability",
% ;   journal = "Journal of Logic and Computation",
% ;   volume = "14",
% ;   number = "5",
% ;   pages = "703--730",
% ; }
% ;
% ectest/ec_reader_test_examples.e:82
% 
% option trajectory on
option(trajectory, on).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% ectest/ec_reader_test_examples.e:88
% sort object
sort(object).


% sort height: integer
subsort(height, integer).


% 
% fluent Height(object,height)
fluent(height(object, height)).


% fluent Falling(object)
fluent(falling(object)).


% event StartFalling(object)
event(startFalling(object)).


% ectest/ec_reader_test_examples.e:94
% event HitsGround(object)
event(hitsGround(object)).


% 
% ectest/ec_reader_test_examples.e:96
% [object,height1,height2,time]% 
% HoldsAt(Height(object,height1),time) &
% HoldsAt(Height(object,height2),time) ->
% height1=height2.
holds_at(height(Object, Height1), Time), holds_at(height(Object, Height2), Time) ->
	Height1=Height2.


% 
% 
% ectest/ec_reader_test_examples.e:101
% [object,time]% 
% Initiates(StartFalling(object),Falling(object),time).
initiates(startFalling(Object), falling(Object), Time).


% 
% 
% ectest/ec_reader_test_examples.e:104
% [object,height,time]% 
% Releases(StartFalling(object),Height(object,height),time).
releases(startFalling(Object), height(Object, Height), Time).


% 
% 
% ectest/ec_reader_test_examples.e:107
% [object,height1,height2,offset,time]% 
% HoldsAt(Height(object,height1),time) &
% height2=height1-offset*offset ->
% Trajectory(Falling(object),time,Height(object,height2),offset).
holds_at(height(Object, Height1), Time), Height2=Height1-Offset*Offset ->
	trajectory(falling(Object),
		   Time,
		   height(Object, Height2),
		   Offset).


% 
% 
% ectest/ec_reader_test_examples.e:112
% [object,time]% 
% HoldsAt(Falling(object),time) &
% HoldsAt(Height(object,0),time) ->
% Happens(HitsGround(object),time).
holds_at(falling(Object), Time), holds_at(height(Object, 0), Time) ->
	happens(hitsGround(Object), Time).


% 
% 
% ;[object,height1,height2,time]
% ;HoldsAt(Height(object,height1),time) &
% ;height1 != height2 ->
% ;Terminates(HitsGround(object),Height(object,height2),time).
% ectest/ec_reader_test_examples.e:121
% 
% ectest/ec_reader_test_examples.e:122
% [object,height,time]% 
% HoldsAt(Height(object,height),time) ->
% Initiates(HitsGround(object),Height(object,height),time).
holds_at(height(Object, Height), Time) ->
	initiates(hitsGround(Object),
		  height(Object, Height),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:126
% [object,time]% 
% Terminates(HitsGround(object),Falling(object),time).
terminates(hitsGround(Object), falling(Object), Time).


% 
% 
% object Leaf
t(object, leaf).


% 
% !HoldsAt(Falling(Leaf),0).
not(holds_at(falling(leaf), 0)).


% 
% ectest/ec_reader_test_examples.e:132
% HoldsAt(Height(Leaf,9),0).
holds_at(height(leaf, 9), 0).


% 
% Happens(StartFalling(Leaf),0).
happens(startFalling(leaf), 0).


% 
% 
% completion Happens
completion(happens).


% 
% range time 0 4
range(time, 0, 4).


% ectest/ec_reader_test_examples.e:138
% range offset 1 9
range(offset, 1, 9).


% range height 0 9
range(height, 0, 9).


% 
% ; End of file.
% 
% 
% ectest/ec_reader_test_examples.e:144
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Cassimatis2002/PolySpace.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @phdthesis{Cassimatis:2002,
% ;   author = "Nicholas L. Cassimatis",
% ;   year = "2002",
% ;   title = "Polyscheme: A Cognitive Architecture for Integrating Multiple Representation and Inference Schemes",
% ;   address = "Cambridge, MA",
% ;   school = "Program in Media Arts and Sciences, School of Architecture and Planning, Massachusetts Institute of Technology",
% ; }
% ;
% ectest/ec_reader_test_examples.e:166
% 
% ; sorts
% sort object
sort(object).


% sort xcoord: integer
subsort(xcoord, integer).


% sort ycoord: integer
subsort(ycoord, integer).


% sort grid
sort(grid).


% ectest/ec_reader_test_examples.e:172
% sort shape
sort(shape).


% sort color
sort(color).


% 
% ; constants
% shape Round,Square
t(shape, round).


t(shape, square).


% color Red,Green
t(color, red).


t(color, green).


% ectest/ec_reader_test_examples.e:178
% 
% ; predicates, fluents, and events
% predicate Equal(object,object)
predicate(equal(object, object)).


% predicate Shape(object,shape)
predicate(shape(object, shape)).


% predicate Color(object,color)
predicate(color(object, color)).


% fluent Location(grid,object,xcoord,ycoord)
fluent(location(grid, object, xcoord, ycoord)).


% ectest/ec_reader_test_examples.e:184
% event Move(grid,object,xcoord,ycoord,xcoord,ycoord)
event(move(grid, object, xcoord, ycoord, xcoord, ycoord)).


% 
% ; axioms
% 
% ectest/ec_reader_test_examples.e:188
% [object1,object2] % Equal(object1,object2) -> Equal(object2,object1).
equal(Object1, Object2) ->
	equal(Object2, Object1).


% 
% 
% ; objects have unique shape
% ectest/ec_reader_test_examples.e:191
% [object,shape1,shape2]% 
% Shape(object,shape1) & Shape(object,shape2) ->
% shape1=shape2.
shape(Object, Shape1), shape(Object, Shape2) ->
	Shape1=Shape2.


% 
% 
% ; objects have unique color
% ectest/ec_reader_test_examples.e:196
% [object,color1,color2]% 
% Color(object,color1) & Color(object,color2) ->
% color1=color2.
color(Object, Color1), color(Object, Color2) ->
	Color1=Color2.


% 
% 
% ; if objects are the same, they have the same shape
% ectest/ec_reader_test_examples.e:201
% [object1,object2]% 
% Equal(object1,object2) ->
% ({shape} Shape(object1,shape) & Shape(object2,shape)).
equal(Object1, Object2) ->
	exists([Shape],
	       (shape(Object1, Shape), shape(Object2, Shape))).


% 
% 
% ; if objects are the same, they have the same color
% ectest/ec_reader_test_examples.e:206
% [object1,object2]% 
% Equal(object1,object2) ->
% ({color} Color(object1,color) & Color(object2,color)).
equal(Object1, Object2) ->
	exists([Color],
	       (color(Object1, Color), color(Object2, Color))).


% 
% 
% ; if objects are the same, they have the same location
% ectest/ec_reader_test_examples.e:211
% [grid,object1,object2,xcoord1,ycoord1,xcoord2,ycoord2,time]% 
% Equal(object1,object2) ->
% (HoldsAt(Location(grid,object1,xcoord1,ycoord1),time) &
%  HoldsAt(Location(grid,object2,xcoord2,ycoord2),time) ->
%  xcoord1=xcoord2 & ycoord1=ycoord2).
equal(Object1, Object2) ->
	( holds_at(location(Grid, Object1, Xcoord1, Ycoord1), Time), holds_at(location(Grid, Object2, Xcoord2, Ycoord2), Time)->Xcoord1=Xcoord2, Ycoord1=Ycoord2
	).


% 
% 
% ; object in one location at a time
% ectest/ec_reader_test_examples.e:218
% [grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]% 
% HoldsAt(Location(grid,object,xcoord1,ycoord1),time) &
% HoldsAt(Location(grid,object,xcoord2,ycoord2),time) ->
% xcoord1=xcoord2 & ycoord1=ycoord2.
holds_at(location(Grid, Object, Xcoord1, Ycoord1), Time), holds_at(location(Grid, Object, Xcoord2, Ycoord2), Time) ->
	Xcoord1=Xcoord2,
	Ycoord1=Ycoord2.


% 
% 
% ; objects have locations
% ectest/ec_reader_test_examples.e:224
% [grid,object,time]% 
% (
% ectest/ec_reader_test_examples.e:225
% {xcoord,ycoord} HoldsAt(Location(grid,object,xcoord,ycoord),time)).
exists([Xcoord, Ycoord], holds_at(location(Grid, Object, Xcoord, Ycoord), Time)).


% 
% 
% ; different objects are not at same location
% ectest/ec_reader_test_examples.e:228
% [grid,object1,object2,xcoord1,ycoord1,time]% 
% HoldsAt(Location(grid,object1,xcoord1,ycoord1),time) &
% HoldsAt(Location(grid,object2,xcoord1,ycoord1),time) ->
% Equal(object1,object2).
holds_at(location(Grid, Object1, Xcoord1, Ycoord1), Time), holds_at(location(Grid, Object2, Xcoord1, Ycoord1), Time) ->
	equal(Object1, Object2).


% 
% 
% ; moving to a location causes an object to be at that location
% ectest/ec_reader_test_examples.e:234
% [grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]% 
% Initiates(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),
%           Location(grid,object,xcoord2,ycoord2),
%           time).
initiates(move(Grid, Object, Xcoord1, Ycoord1, Xcoord2, Ycoord2), location(Grid, Object, Xcoord2, Ycoord2), Time).


% 
% 
% ; moving to a location causes the object no longer to be at its previous
% ; location
% ectest/ec_reader_test_examples.e:241
% [grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]% 
% Terminates(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),
%            Location(grid,object,xcoord1,ycoord1),
%            time).
terminates(move(Grid, Object, Xcoord1, Ycoord1, Xcoord2, Ycoord2), location(Grid, Object, Xcoord1, Ycoord1), Time).


% 
% 
% ;; allow diagonal movements
% ;[grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
% ;Happens(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),time) ->
% ;HoldsAt(Location(grid,object,xcoord1,ycoord1),time) &
% ;(xcoord1=xcoord2 |
% ; xcoord1=xcoord2+1 |
% ; xcoord1=xcoord2-1) &
% ;(ycoord1=ycoord2 |
% ; ycoord1=ycoord2+1 |
% ; ycoord1=ycoord2-1).
% ectest/ec_reader_test_examples.e:256
% 
% ; only allow right angle movements
% ectest/ec_reader_test_examples.e:258
% [grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]% 
% Happens(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),time) ->
% HoldsAt(Location(grid,object,xcoord1,ycoord1),time) &
% ((xcoord1=xcoord2 & (ycoord1=ycoord2+1 | ycoord1=ycoord2-1)) |
%  (ycoord1=ycoord2 & (xcoord1=xcoord2+1 | xcoord1=xcoord2-1))).
happens(move(Grid, Object, Xcoord1, Ycoord1, Xcoord2, Ycoord2), Time) ->
	holds_at(location(Grid, Object, Xcoord1, Ycoord1),
		 Time),
	(   Xcoord1=Xcoord2,
	    (   Ycoord1=Ycoord2+1
	    ;   Ycoord1=Ycoord2-1
	    )
	;   Ycoord1=Ycoord2,
	    (   Xcoord1=Xcoord2+1
	    ;   Xcoord1=Xcoord2-1
	    )
	).


% 
% 
% ; End of file.
% ectest/ec_reader_test_examples.e:265
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Cassimatis2002/TwoScreens.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @phdthesis{Cassimatis:2002,
% ;   author = "Nicholas L. Cassimatis",
% ;   year = "2002",
% ;   title = "Polyscheme: A Cognitive Architecture for Integrating Multiple Representation and Inference Schemes",
% ;   address = "Cambridge, MA",
% ;   school = "Program in Media Arts and Sciences, School of Architecture and Planning, Massachusetts Institute of Technology",
% ; }
% ;
% ectest/ec_reader_test_examples.e:289
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% load examples/Cassimatis2002/PolySpace.e
load('examples/Cassimatis2002/PolySpace.e').


% 
% grid G1
t(grid, g1).


% ectest/ec_reader_test_examples.e:295
% object X,Y,Screen1,Screen2
t(object, x).


t(object, y).


t(object, screen1).


t(object, screen2).


% 
% ; perceptions:
% Shape(X,Round).
shape(x, round).


% 
% Color(X,Red).
color(x, red).


% 
% Shape(Y,Round).
shape(y, round).


% 
% ectest/ec_reader_test_examples.e:301
% Color(Y,Red).
color(y, red).


% 
% Shape(Screen1,Square).
shape(screen1, square).


% 
% Color(Screen1,Green).
color(screen1, green).


% 
% Shape(Screen2,Square).
shape(screen2, square).


% 
% Color(Screen2,Green).
color(screen2, green).


% 
% ectest/ec_reader_test_examples.e:306
% [time] % HoldsAt(Location(G1,Screen1,2,0),time).
holds_at(location(g1, screen1, 2, 0), Time).


% 
% ectest/ec_reader_test_examples.e:307
% [time] % HoldsAt(Location(G1,Screen2,4,0),time).
holds_at(location(g1, screen2, 4, 0), Time).


% 
% HoldsAt(Location(G1,X,1,1),0).
holds_at(location(g1, x, 1, 1), 0).


% 
% HoldsAt(Location(G1,Y,5,1),4).
holds_at(location(g1, y, 5, 1), 4).


% 
% 
% ectest/ec_reader_test_examples.e:311
% [xcoord,ycoord,time]% 
% xcoord!=% 2 & xcoord!=4 & !(xcoord=1 & ycoord=1 & time=0) ->
% !HoldsAt(Location(G1,X,xcoord,ycoord),time) |
% xcoord=5 & ycoord=1 & time=4 & Equal(X,Y).
(   ( Xcoord\=2, Xcoord\=4, not((Xcoord=1, Ycoord=1, Time=0))->not(holds_at(location(g1, x, Xcoord, Ycoord), Time))
    )
;   Xcoord=5,
    Ycoord=1,
    Time=4,
    equal(x, y)
).


% 
% 
% ectest/ec_reader_test_examples.e:316
% [xcoord,ycoord,time]% 
% xcoord!=% 2 & xcoord!=4 & !(xcoord=5 & ycoord=1 & time=4) ->
% !HoldsAt(Location(G1,Y,xcoord,ycoord),time) |
% xcoord=1 & ycoord=1 & time=0 & Equal(X,Y).
(   ( Xcoord\=2, Xcoord\=4, not((Xcoord=5, Ycoord=1, Time=4))->not(holds_at(location(g1, y, Xcoord, Ycoord), Time))
    )
;   Xcoord=1,
    Ycoord=1,
    Time=0,
    equal(x, y)
).


% 
% 
% range time 0 4
range(time, 0, 4).


% ectest/ec_reader_test_examples.e:322
% range xcoord 0 5
range(xcoord, 0, 5).


% range ycoord 0 1
range(ycoord, 0, 1).


% range offset 0 0
range(offset, 0, 0).


% 
% ; End of file.
% 
% ectest/ec_reader_test_examples.e:328
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Cassimatis2002/OneScreen.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @phdthesis{Cassimatis:2002,
% ;   author = "Nicholas L. Cassimatis",
% ;   year = "2002",
% ;   title = "Polyscheme: A Cognitive Architecture for Integrating Multiple Representation and Inference Schemes",
% ;   address = "Cambridge, MA",
% ;   school = "Program in Media Arts and Sciences, School of Architecture and Planning, Massachusetts Institute of Technology",
% ; }
% ;
% ectest/ec_reader_test_examples.e:351
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% load examples/Cassimatis2002/PolySpace.e
load('examples/Cassimatis2002/PolySpace.e').


% 
% grid G1
t(grid, g1).


% ectest/ec_reader_test_examples.e:357
% object X,Y,Screen
t(object, x).


t(object, y).


t(object, screen).


% 
% ; perceptions:
% Shape(X,Round).
shape(x, round).


% 
% Color(X,Red).
color(x, red).


% 
% Shape(Y,Round).
shape(y, round).


% 
% ectest/ec_reader_test_examples.e:363
% Color(Y,Red).
color(y, red).


% 
% Shape(Screen,Square).
shape(screen, square).


% 
% Color(Screen,Green).
color(screen, green).


% 
% ectest/ec_reader_test_examples.e:366
% [time] % HoldsAt(Location(G1,Screen,2,0),time).
holds_at(location(g1, screen, 2, 0), Time).


% 
% HoldsAt(Location(G1,X,1,1),0).
holds_at(location(g1, x, 1, 1), 0).


% 
% HoldsAt(Location(G1,Y,3,1),2).
holds_at(location(g1, y, 3, 1), 2).


% 
% 
% ectest/ec_reader_test_examples.e:370
% [xcoord,ycoord,time]% 
% xcoord!=% 2 & !(xcoord=1 & ycoord=1 & time=0) ->
% !HoldsAt(Location(G1,X,xcoord,ycoord),time) |
% xcoord=3 & ycoord=1 & time=2 & Equal(X,Y).
(   ( Xcoord\=2, not((Xcoord=1, Ycoord=1, Time=0))->not(holds_at(location(g1, x, Xcoord, Ycoord), Time))
    )
;   Xcoord=3,
    Ycoord=1,
    Time=2,
    equal(x, y)
).


% 
% 
% ectest/ec_reader_test_examples.e:375
% [xcoord,ycoord,time]% 
% xcoord!=% 2 & !(xcoord=3 & ycoord=1 & time=2) ->
% !HoldsAt(Location(G1,Y,xcoord,ycoord),time) |
% xcoord=1 & ycoord=1 & time=0 & Equal(X,Y).
(   ( Xcoord\=2, not((Xcoord=3, Ycoord=1, Time=2))->not(holds_at(location(g1, y, Xcoord, Ycoord), Time))
    )
;   Xcoord=1,
    Ycoord=1,
    Time=0,
    equal(x, y)
).


% 
% 
% range time 0 2
range(time, 0, 2).


% ectest/ec_reader_test_examples.e:381
% range xcoord 0 4
range(xcoord, 0, 4).


% range ycoord 0 2
range(ycoord, 0, 2).


% range offset 0 0
range(offset, 0, 0).


% 
% ; End of file.
% 
% ectest/ec_reader_test_examples.e:387
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/BrewkaDixKonolige1997/Wine.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; reasoning by cases
% ; \fullciteA[p. 45]{BrewkaDixKonolige:1997}
% ;
% ; @book{BrewkaDixKonolige:1997,
% ;   author = "Gerhard Brewka and J{\"{u}}rgen Dix and Kurt Konolige",
% ;   year = "1997",
% ;   title = "Nonmonotonic Reasoning: An Overview",
% ;   address = "Stanford, CA",
% ;   publisher = "CSLI",
% ; }
% ;
% ectest/ec_reader_test_examples.e:413
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort x
sort(x).


% x Person
t(x, person).


% ectest/ec_reader_test_examples.e:419
% 
% predicate LikesWine(x)
predicate(likesWine(x)).


% predicate Italian(x)
predicate(italian(x)).


% predicate French(x)
predicate(french(x)).


% predicate Ab1(x)
predicate(ab1(x)).


% predicate Ab2(x)
predicate(ab2(x)).


% ectest/ec_reader_test_examples.e:425
% 
% ectest/ec_reader_test_examples.e:426
% [x] % Italian(x) & !Ab1(x) -> LikesWine(x).
italian(X), not(ab1(X)) ->
	likesWine(X).


% 
% ectest/ec_reader_test_examples.e:427
% [x] % French(x) & !Ab2(x) -> LikesWine(x).
french(X), not(ab2(X)) ->
	likesWine(X).


% 
% ectest/ec_reader_test_examples.e:428
% [x] % Italian(x) -> !French(x).
italian(X) ->
	not(french(X)).


% 
% 
% Italian(Person) | French(Person).
(   italian(person)
;   french(person)
).


% 
% 
% range time 0 0
range(time, 0, 0).


% range offset 1 1
range(offset, 1, 1).


% ectest/ec_reader_test_examples.e:434
% 
% completion Theta Ab1
completion([theta, ab1]).


% completion Theta Ab2
completion([theta, ab2]).


% 
% ; End of file.
% 
% ectest/ec_reader_test_examples.e:440
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Shanahan1997/Yale.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{HanksMcDermott:1987,
% ;   author = "Steve Hanks and Drew V. McDermott",
% ;   year = "1987",
% ;   title = "Nonmonotonic logic and temporal projection",
% ;   journal = "Artificial Intelligence",
% ;   volume = "33",
% ;   number = "3",
% ;   pages = "379--412",
% ; }
% ;
% ; \fullciteA[pp. 322--323]{Shanahan:1997}
% ;
% ; @book{Shanahan:1997,
% ;   author = "Murray Shanahan",
% ;   year = "1997",
% ;   title = "Solving the Frame Problem",
% ;   address = "Cambridge, MA",
% ;   publisher = "MIT Press",
% ; }
% ;
% ; deduction
% ;
% ; modifications from Shanahan's formulation:
% ; InitiallyP -> HoldsAt
% ; timestamps
% ; added [time] Terminates(Shoot(),Loaded(),time).
% ;
% ectest/ec_reader_test_examples.e:482
% 
% option showpred off
option(showpred, off).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% ectest/ec_reader_test_examples.e:488
% event Load()
event(load()).


% event Shoot()
event(shoot()).


% event Sneeze()
event(sneeze()).


% fluent Loaded()
fluent(loaded()).


% fluent Alive()
fluent(alive()).


% 
% ectest/ec_reader_test_examples.e:494
% [time] % Initiates(Load(),Loaded(),time).
initiates(load(), loaded(), Time).


% 
% ectest/ec_reader_test_examples.e:495
% [time] % HoldsAt(Loaded(),time) -> Terminates(Shoot(),Alive(),time).
holds_at(loaded(), Time) ->
	terminates(shoot(), alive(), Time).


% 
% ectest/ec_reader_test_examples.e:496
% [time] % Terminates(Shoot(),Loaded(),time).
terminates(shoot(), loaded(), Time).


% 
% 
% HoldsAt(Alive(),0).
holds_at(alive(), 0).


% 
% !HoldsAt(Loaded(),0).
not(holds_at(loaded(), 0)).


% 
% Happens(Load(),0).
happens(load(), 0).


% 
% Happens(Sneeze(),1).
happens(sneeze(), 1).


% 
% ectest/ec_reader_test_examples.e:502
% Happens(Shoot(),2).
happens(shoot(), 2).


% 
% 
% completion Happens
completion(happens).


% 
% range time 0 3
range(time, 0, 3).


% range offset 1 1
range(offset, 1, 1).


% ectest/ec_reader_test_examples.e:508
% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Shanahan1997/StuffyRoom.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{GinsbergSmith:1988a,
% ;   author = "Matthew L. Ginsberg and David E. Smith",
% ;   year = "1988",
% ;   title = "Reasoning about action \uppercase{I}: \uppercase{A} possible worlds approach",
% ;   journal = "Artificial Intelligence",
% ;   volume = "35",
% ;   number = "2",
% ;   pages = "165--195",
% ; }
% ;
% ; \fullciteA[pp. 288--289]{Shanahan:1997}
% ;
% ; @book{Shanahan:1997,
% ;   author = "Murray Shanahan",
% ;   year = "1997",
% ;   title = "Solving the Frame Problem",
% ;   address = "Cambridge, MA",
% ;   publisher = "MIT Press",
% ; }
% ;
% ; deduction
% ;
% ; modifications from Shanahan's formulation:
% ; timestamps
% ; added:
% ; !HoldsAt(Blocked1(),0).
% ; !HoldsAt(Blocked2(),0).
% ;
% ectest/ec_reader_test_examples.e:554
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% event Close1()
event(close1()).


% event Close2()
event(close2()).


% ectest/ec_reader_test_examples.e:560
% event Start()
event(start()).


% fluent Blocked1()
fluent(blocked1()).


% fluent Blocked2()
fluent(blocked2()).


% fluent Stuffy()
fluent(stuffy()).


% noninertial Stuffy
noninertial(stuffy).


% 
% ectest/ec_reader_test_examples.e:566
% [time] % Initiates(Close1(),Blocked1(),time).
initiates(close1(), blocked1(), Time).


% 
% ectest/ec_reader_test_examples.e:567
% [time] % Initiates(Close2(),Blocked2(),time).
initiates(close2(), blocked2(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:569
% [time]% 
% HoldsAt(Stuffy(),time) <->
% HoldsAt(Blocked1(),time)&HoldsAt(Blocked2(),time).
holds_at(stuffy(), Time) <->
	holds_at(blocked1(), Time),
	holds_at(blocked2(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:573
% [time] % Initiates(Start(),Blocked1(),time).
initiates(start(), blocked1(), Time).


% 
% ectest/ec_reader_test_examples.e:574
% [time] % Terminates(Start(),Blocked2(),time).
terminates(start(), blocked2(), Time).


% 
% 
% !HoldsAt(Blocked1(),0).
not(holds_at(blocked1(), 0)).


% 
% !HoldsAt(Blocked2(),0).
not(holds_at(blocked2(), 0)).


% 
% Happens(Start(),0).
happens(start(), 0).


% 
% Happens(Close2(),1).
happens(close2(), 1).


% 
% ectest/ec_reader_test_examples.e:580
% 
% completion Happens
completion(happens).


% 
% range time 0 2
range(time, 0, 2).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:587
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Shanahan1997/BusRide.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{Kartha:1994,
% ;   author = "G. Neelakantan Kartha",
% ;   year = "1994",
% ;   title = "Two counterexamples related to \uppercase{B}aker's approach to the frame problem",
% ;   journal = "Artificial Intelligence",
% ;   volume = "69",
% ;   number = "1--2",
% ;   pages = "379--391",
% ; }
% ;
% ; \fullciteA[pp. 359--361]{Shanahan:1997}
% ;
% ; @book{Shanahan:1997,
% ;   author = "Murray Shanahan",
% ;   year = "1997",
% ;   title = "Solving the Frame Problem",
% ;   address = "Cambridge, MA",
% ;   publisher = "MIT Press",
% ; }
% ;
% ; modifications from Shanahan's formulation:
% ; InitiallyN -> !HoldsAt
% ; timestamps
% ;
% ectest/ec_reader_test_examples.e:627
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% fluent HasTicket()
fluent(hasTicket()).


% fluent OnRed()
fluent(onRed()).


% ectest/ec_reader_test_examples.e:633
% fluent OnYellow()
fluent(onYellow()).


% event Buy()
event(buy()).


% event Board()
event(board()).


% event BoardRed()
event(boardRed()).


% event BoardYellow()
event(boardYellow()).


% 
% ectest/ec_reader_test_examples.e:639
% [time] % Happens(Board(),time) -> Happens(BoardRed(),time) | Happens(BoardYellow(),time).
(   ( happens(board(), Time)->happens(boardRed(), Time)
    )
;   happens(boardYellow(), Time)
).


% 
% 
% ectest/ec_reader_test_examples.e:641
% [time] % Initiates(Buy(),HasTicket(),time).
initiates(buy(), hasTicket(), Time).


% 
% ectest/ec_reader_test_examples.e:642
% [time] % HoldsAt(HasTicket(),time) -> Initiates(BoardRed(),OnRed(),time).
holds_at(hasTicket(), Time) ->
	initiates(boardRed(), onRed(), Time).


% 
% ectest/ec_reader_test_examples.e:643
% [time] % HoldsAt(HasTicket(),time) -> Initiates(BoardYellow(),OnYellow(),time).
holds_at(hasTicket(), Time) ->
	initiates(boardYellow(), onYellow(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:645
% [time] % !(HoldsAt(OnRed(),time) & HoldsAt(OnYellow(),time)).
not(( holds_at(onRed(), Time),
      holds_at(onYellow(), Time)
    )).


% 
% ectest/ec_reader_test_examples.e:646
% [time] % HoldsAt(OnRed(),time) -> HoldsAt(HasTicket(),time).
holds_at(onRed(), Time) ->
	holds_at(hasTicket(), Time).


% 
% ectest/ec_reader_test_examples.e:647
% [time] % HoldsAt(OnYellow(),time) -> HoldsAt(HasTicket(),time).
holds_at(onYellow(), Time) ->
	holds_at(hasTicket(), Time).


% 
% 
% HoldsAt(OnRed(),2).
holds_at(onRed(), 2).


% 
% 
% !HoldsAt(HasTicket(),0).
not(holds_at(hasTicket(), 0)).


% 
% Happens(Buy(),0).
happens(buy(), 0).


% 
% ectest/ec_reader_test_examples.e:653
% Happens(Board(),1).
happens(board(), 1).


% 
% ; ABDUCED Happens(BoardRed(), 1).
% 
% completion Happens
completion(happens).


% 
% range time 0 2
range(time, 0, 2).


% ectest/ec_reader_test_examples.e:659
% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Shanahan1997/DeadOrAlive.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; \fullciteA[p. 324]{Shanahan:1997}
% ;
% ; @book{Shanahan:1997,
% ;   author = "Murray Shanahan",
% ;   year = "1997",
% ;   title = "Solving the Frame Problem",
% ;   address = "Cambridge, MA",
% ;   publisher = "MIT Press",
% ; }
% ;
% ; deduction
% ;
% ; modifications from Shanahan's formulation:
% ; InitiallyP -> HoldsAt
% ; timestamps
% ; added [time] Terminates(Shoot(),Loaded(),time).
% ;
% ectest/ec_reader_test_examples.e:695
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% event Load()
event(load()).


% event Shoot()
event(shoot()).


% ectest/ec_reader_test_examples.e:701
% event Sneeze()
event(sneeze()).


% fluent Loaded()
fluent(loaded()).


% fluent Alive()
fluent(alive()).


% fluent Dead()
fluent(dead()).


% noninertial Dead
noninertial(dead).


% 
% ectest/ec_reader_test_examples.e:707
% [time] % Initiates(Load(),Loaded(),time).
initiates(load(), loaded(), Time).


% 
% ectest/ec_reader_test_examples.e:708
% [time] % HoldsAt(Loaded(),time) -> Terminates(Shoot(),Alive(),time).
holds_at(loaded(), Time) ->
	terminates(shoot(), alive(), Time).


% 
% ectest/ec_reader_test_examples.e:709
% [time] % Terminates(Shoot(),Loaded(),time).
terminates(shoot(), loaded(), Time).


% 
% ectest/ec_reader_test_examples.e:710
% [time] % HoldsAt(Dead(),time) <-> !HoldsAt(Alive(),time).
holds_at(dead(), Time) <->
	not(holds_at(alive(), Time)).


% 
% 
% HoldsAt(Alive(),0).
holds_at(alive(), 0).


% 
% !HoldsAt(Loaded(),0).
not(holds_at(loaded(), 0)).


% 
% Happens(Load(),0).
happens(load(), 0).


% 
% Happens(Sneeze(),1).
happens(sneeze(), 1).


% 
% ectest/ec_reader_test_examples.e:716
% Happens(Shoot(),2).
happens(shoot(), 2).


% 
% 
% completion Happens
completion(happens).


% 
% range time 0 3
range(time, 0, 3).


% range offset 1 1
range(offset, 1, 1).


% ectest/ec_reader_test_examples.e:722
% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Shanahan1997/Supermarket.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; \fullciteA[pp. 302--304]{Shanahan:1997}
% ;
% ; @book{Shanahan:1997,
% ;   author = "Murray Shanahan",
% ;   year = "1997",
% ;   title = "Solving the Frame Problem",
% ;   address = "Cambridge, MA",
% ;   publisher = "MIT Press",
% ; }
% ;
% ; deduction
% ;
% ; modifications from Shanahan's formulation:
% ; reformulated using the method of \fullciteA[pp. 460--461]{MillerShanahan:2002}
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
% ; added:
% ; !HoldsAt(Forwards(), 0).
% ; !HoldsAt(Backwards(), 0).
% ; !HoldsAt(Spinning(), 0).
% ;
% ectest/ec_reader_test_examples.e:773
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% event Push()
event(push()).


% event Pull()
event(pull()).


% ectest/ec_reader_test_examples.e:779
% fluent Forwards()
fluent(forwards()).


% fluent Backwards()
fluent(backwards()).


% fluent Spinning()
fluent(spinning()).


% 
% ectest/ec_reader_test_examples.e:783
% [time]% 
% !Happens(Pull(), time) ->
% Initiates(Push(), Forwards(), time).
not(happens(pull(), Time)) ->
	initiates(push(), forwards(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:787
% [time]% 
% !Happens(Pull(), time) ->
% Terminates(Push(), Backwards(), time).
not(happens(pull(), Time)) ->
	terminates(push(), backwards(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:791
% [time]% 
% !Happens(Push(), time) ->
% Initiates(Pull(), Backwards(), time).
not(happens(push(), Time)) ->
	initiates(pull(), backwards(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:795
% [time]% 
% !Happens(Push(), time) ->
% Terminates(Pull(), Forwards(), time).
not(happens(push(), Time)) ->
	terminates(pull(), forwards(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:799
% [time]% 
% Happens(Push(), time) ->
% Initiates(Pull(), Spinning(), time).
happens(push(), Time) ->
	initiates(pull(), spinning(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:803
% [time]% 
% Happens(Push(), time) ->
% Terminates(Pull(), Forwards(), time).
happens(push(), Time) ->
	terminates(pull(), forwards(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:807
% [time]% 
% Happens(Push(), time) ->
% Terminates(Pull(), Backwards(), time).
happens(push(), Time) ->
	terminates(pull(), backwards(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:811
% [time]% 
% !Happens(Pull(), time) ->
% Terminates(Push(), Spinning(), time).
not(happens(pull(), Time)) ->
	terminates(push(), spinning(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:815
% [time]% 
% !Happens(Push(), time) ->
% Terminates(Pull(), Spinning(), time).
not(happens(push(), Time)) ->
	terminates(pull(), spinning(), Time).


% 
% 
% !HoldsAt(Forwards(), 0).
not(holds_at(forwards(), 0)).


% 
% !HoldsAt(Backwards(), 0).
not(holds_at(backwards(), 0)).


% 
% ectest/ec_reader_test_examples.e:821
% !HoldsAt(Spinning(), 0).
not(holds_at(spinning(), 0)).


% 
% 
% Happens(Push(), 5).
happens(push(), 5).


% 
% Happens(Pull(), 5).
happens(pull(), 5).


% 
% Happens(Pull(), 10).
happens(pull(), 10).


% 
% Happens(Push(), 10).
happens(push(), 10).


% 
% ectest/ec_reader_test_examples.e:827
% 
% completion Happens
completion(happens).


% 
% range time 0 12
range(time, 0, 12).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:834
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Shanahan1997/StolenCar.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @inproceedings{Kautz:1986,
% ;   author = "Henry A. Kautz",
% ;   year = "1986",
% ;   title = "The Logic of Persistence",
% ;   booktitle = "\uppercase{P}roceedings of the \uppercase{F}ifth \uppercase{N}ational \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
% ;   pages = "401--405",
% ;   address = "Los Altos, CA",
% ;   publisher = "Morgan Kaufmann",
% ; }
% ;
% ; \fullciteA[p. 359]{Shanahan:1997}
% ;
% ; @book{Shanahan:1997,
% ;   author = "Murray Shanahan",
% ;   year = "1997",
% ;   title = "Solving the Frame Problem",
% ;   address = "Cambridge, MA",
% ;   publisher = "MIT Press",
% ; }
% ;
% ; abduction
% ;
% ; modifications from Shanahan's formulation:
% ; timestamps
% ; added !HoldsAt(CarParked(),0).
% ;
% ectest/ec_reader_test_examples.e:876
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% event Park()
event(park()).


% event Steal()
event(steal()).


% ectest/ec_reader_test_examples.e:882
% fluent CarParked()
fluent(carParked()).


% 
% ectest/ec_reader_test_examples.e:884
% [time] % Initiates(Park(),CarParked(),time).
initiates(park(), carParked(), Time).


% 
% ectest/ec_reader_test_examples.e:885
% [time] % Terminates(Steal(),CarParked(),time).
terminates(steal(), carParked(), Time).


% 
% 
% !HoldsAt(CarParked(),0).
not(holds_at(carParked(), 0)).


% 
% Happens(Park(),0).
happens(park(), 0).


% 
% ; ABDUCED Happens(Steal(), 1).
% !HoldsAt(CarParked(),2).
not(holds_at(carParked(), 2)).


% 
% ectest/ec_reader_test_examples.e:891
% 
% range time 0 2
range(time, 0, 2).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% ectest/ec_reader_test_examples.e:897
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/MillerShanahan2002/Bowl.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; \fullciteA[p. 461]{MillerShanahan:2002}
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
% ectest/ec_reader_test_examples.e:927
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% event LiftLeft()
event(liftLeft()).


% event LiftRight()
event(liftRight()).


% ectest/ec_reader_test_examples.e:933
% fluent Spilt()
fluent(spilt()).


% fluent Raised()
fluent(raised()).


% 
% ectest/ec_reader_test_examples.e:936
% [time]% 
% !Happens(LiftRight(), time) ->
% Initiates(LiftLeft(), Spilt(), time).
not(happens(liftRight(), Time)) ->
	initiates(liftLeft(), spilt(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:940
% [time]% 
% !Happens(LiftLeft(), time) ->
% Initiates(LiftRight(), Spilt(), time).
not(happens(liftLeft(), Time)) ->
	initiates(liftRight(), spilt(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:944
% [time]% 
% Happens(LiftLeft(), time) ->
% Initiates(LiftRight(), Raised(), time).
happens(liftLeft(), Time) ->
	initiates(liftRight(), raised(), Time).


% 
% 
% !HoldsAt(Spilt(), 0).
not(holds_at(spilt(), 0)).


% 
% !HoldsAt(Raised(), 0).
not(holds_at(raised(), 0)).


% 
% ectest/ec_reader_test_examples.e:950
% Happens(LiftLeft(), 2).
happens(liftLeft(), 2).


% 
% Happens(LiftRight(), 2).
happens(liftRight(), 2).


% 
% 
% completion Happens
completion(happens).


% 
% range time 0 3
range(time, 0, 3).


% ectest/ec_reader_test_examples.e:956
% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/ReiterCriscuolo1981/NixonDiamond1.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; conflicting defaults: showing that inconsistency results
% ; without a cancellation rule
% ; \fullciteA[p. 274]{ReiterCriscuolo:1981}
% ; \fullciteA[pp. 98--99]{McCarthy:1986}
% ;
% ; @inproceedings{ReiterCriscuolo:1981,
% ;   author = "Raymond Reiter and Giovanni Criscuolo",
% ;   year = "1981",
% ;   title = "On interacting defaults",
% ;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventh \uppercase{I}nternational \uppercase{J}oint \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
% ;   volume = "1",
% ;   pages = "270--276",
% ;   address = "Los Altos, CA",
% ;   publisher = "William Kaufmann",
% ; }
% ;
% ; @article{McCarthy:1986,
% ;   author = "John McCarthy",
% ;   year = "1986",
% ;   title = "Applications of circumscription to formalizing common-sense knowledge",
% ;   journal = "Artificial Intelligence",
% ;   volume = "28",
% ;   pages = "89--116".
% ; }
% ;
% ectest/ec_reader_test_examples.e:1000
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort x
sort(x).


% 
% ectest/ec_reader_test_examples.e:1006
% predicate Republican(x)
predicate(republican(x)).


% predicate Quaker(x)
predicate(quaker(x)).


% predicate Pacifist(x)
predicate(pacifist(x)).


% predicate Ab1(x)
predicate(ab1(x)).


% predicate Ab2(x)
predicate(ab2(x)).


% 
% ectest/ec_reader_test_examples.e:1012
% x John
t(x, john).


% 
% Republican(John).
republican(john).


% 
% Quaker(John).
quaker(john).


% 
% 
% ectest/ec_reader_test_examples.e:1017
% [x] % Republican(x) & !Ab1(x) -> !Pacifist(x).
republican(X), not(ab1(X)) ->
	not(pacifist(X)).


% 
% ectest/ec_reader_test_examples.e:1018
% [x] % Quaker(x) & !Ab2(x) -> Pacifist(x).
quaker(X), not(ab2(X)) ->
	pacifist(X).


% 
% 
% range time 0 0
range(time, 0, 0).


% range offset 1 1
range(offset, 1, 1).


% 
% completion Theta Ab1
completion([theta, ab1]).


% ectest/ec_reader_test_examples.e:1024
% completion Theta Ab2
completion([theta, ab2]).


% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/ReiterCriscuolo1981/NixonDiamond2.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; conflicting defaults: method (D)
% ; \fullciteA[p. 274]{ReiterCriscuolo:1981}
% ; \fullciteA[pp. 98--99]{McCarthy:1986}
% ; \fullciteA[p. 18]{BrewkaDixKonolige:1997}
% ;
% ; @inproceedings{ReiterCriscuolo:1981,
% ;   author = "Raymond Reiter and Giovanni Criscuolo",
% ;   year = "1981",
% ;   title = "On interacting defaults",
% ;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventh \uppercase{I}nternational \uppercase{J}oint \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
% ;   volume = "1",
% ;   pages = "270--276",
% ;   address = "Los Altos, CA",
% ;   publisher = "William Kaufmann",
% ; }
% ;
% ; @article{McCarthy:1986,
% ;   author = "John McCarthy",
% ;   year = "1986",
% ;   title = "Applications of circumscription to formalizing common-sense knowledge",
% ;   journal = "Artificial Intelligence",
% ;   volume = "28",
% ;   pages = "89--116".
% ; }
% ;
% ; @book{BrewkaDixKonolige:1997,
% ;   author = "Gerhard Brewka and J{\"{u}}rgen Dix and Kurt Konolige",
% ;   year = "1997",
% ;   title = "Nonmonotonic Reasoning: An Overview",
% ;   address = "Stanford, CA",
% ;   publisher = "CSLI",
% ; }
% ;
% ectest/ec_reader_test_examples.e:1076
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort x
sort(x).


% 
% ectest/ec_reader_test_examples.e:1082
% predicate Republican(x)
predicate(republican(x)).


% predicate Quaker(x)
predicate(quaker(x)).


% predicate Pacifist(x)
predicate(pacifist(x)).


% predicate Ab1(x)
predicate(ab1(x)).


% predicate Ab2(x)
predicate(ab2(x)).


% 
% ectest/ec_reader_test_examples.e:1088
% x John
t(x, john).


% 
% Republican(John).
republican(john).


% 
% Quaker(John).
quaker(john).


% 
% 
% ectest/ec_reader_test_examples.e:1093
% [x] % Republican(x) & !Ab1(x) -> !Pacifist(x).
republican(X), not(ab1(X)) ->
	not(pacifist(X)).


% 
% ectest/ec_reader_test_examples.e:1094
% [x] % Quaker(x) & !Ab2(x) -> Pacifist(x).
quaker(X), not(ab2(X)) ->
	pacifist(X).


% 
% Theta:
directive(theta).


 
% ectest/ec_reader_test_examples.e:1095
% [x] % Republican(x) -> Ab2(x).
republican(X) ->
	ab2(X).


% 
% 
% range time 0 0
range(time, 0, 0).


% range offset 1 1
range(offset, 1, 1).


% 
% completion Theta Ab1
completion([theta, ab1]).


% ectest/ec_reader_test_examples.e:1101
% completion Theta Ab2
completion([theta, ab2]).


% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter2/Sleep2.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:1128
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% 
% ectest/ec_reader_test_examples.e:1134
% agent Nathan
t(agent, nathan).


% 
% fluent Awake(agent)
fluent(awake(agent)).


% 
% event WakeUp(agent)
event(wakeUp(agent)).


% event FallAsleep(agent)
event(fallAsleep(agent)).


% ectest/ec_reader_test_examples.e:1140
% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:1143
% [agent,time] % Initiates(WakeUp(agent),Awake(agent),time).
initiates(wakeUp(Agent), awake(Agent), Time).


% 
% ectest/ec_reader_test_examples.e:1144
% [agent,time] % Terminates(FallAsleep(agent),Awake(agent),time).
terminates(fallAsleep(Agent), awake(Agent), Time).


% 
% 
% ; Gamma
% 
% !HoldsAt(Awake(Nathan),0).
not(holds_at(awake(nathan), 0)).


% 
% HoldsAt(Awake(Nathan),1).
holds_at(awake(nathan), 1).


% 
% ectest/ec_reader_test_examples.e:1150
% 
% ; abduced:
% ; Happens(WakeUp(Nathan),0).
% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% ectest/ec_reader_test_examples.e:1156
% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter2/Sleep1.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:1182
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% 
% ectest/ec_reader_test_examples.e:1188
% agent Nathan
t(agent, nathan).


% 
% fluent Awake(agent)
fluent(awake(agent)).


% 
% event WakeUp(agent)
event(wakeUp(agent)).


% event FallAsleep(agent)
event(fallAsleep(agent)).


% ectest/ec_reader_test_examples.e:1194
% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:1197
% [agent,time] % Initiates(WakeUp(agent),Awake(agent),time).
initiates(wakeUp(Agent), awake(Agent), Time).


% 
% ectest/ec_reader_test_examples.e:1198
% [agent,time] % Terminates(FallAsleep(agent),Awake(agent),time).
terminates(fallAsleep(Agent), awake(Agent), Time).


% 
% 
% ; Delta
% 
% Happens(WakeUp(Nathan),1).
happens(wakeUp(nathan), 1).


% 
% 
% ; Gamma
% ectest/ec_reader_test_examples.e:1205
% 
% !HoldsAt(Awake(Nathan),0).
not(holds_at(awake(nathan), 0)).


% 
% 
% ; entailed:
% ; HoldsAt(Awake(Nathan),3).
% 
% ectest/ec_reader_test_examples.e:1211
% completion Happens
completion(happens).


% 
% range time 0 3
range(time, 0, 3).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:1217
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter2/Sleep3.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:1241
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% 
% ectest/ec_reader_test_examples.e:1247
% agent Nathan
t(agent, nathan).


% 
% fluent Awake(agent)
fluent(awake(agent)).


% 
% event WakeUp(agent)
event(wakeUp(agent)).


% event FallAsleep(agent)
event(fallAsleep(agent)).


% ectest/ec_reader_test_examples.e:1253
% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:1256
% [agent,time] % Initiates(WakeUp(agent),Awake(agent),time).
initiates(wakeUp(Agent), awake(Agent), Time).


% 
% ectest/ec_reader_test_examples.e:1257
% [agent,time] % Terminates(FallAsleep(agent),Awake(agent),time).
terminates(fallAsleep(Agent), awake(Agent), Time).


% 
% 
% ; Delta
% 
% ectest/ec_reader_test_examples.e:1261
% [agent,time]% 
% Happens(WakeUp(agent),time) ->
% !HoldsAt(Awake(agent),time).
happens(wakeUp(Agent), Time) ->
	not(holds_at(awake(Agent), Time)).


% 
% 
% Happens(WakeUp(Nathan),0).
happens(wakeUp(nathan), 0).


% 
% 
% ; Gamma
% ectest/ec_reader_test_examples.e:1268
% 
% HoldsAt(Awake(Nathan),1).
holds_at(awake(nathan), 1).


% 
% 
% ; inferred:
% ; !HoldsAt(Awake(Nathan),0).
% 
% ectest/ec_reader_test_examples.e:1274
% completion Happens
completion(happens).


% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:1280
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter2/Inconsistency3.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:1304
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort object
sort(object).


% object O1
t(object, o1).


% ectest/ec_reader_test_examples.e:1310
% 
% fluent F(object)
fluent(f(object)).


% 
% event E(object)
event(e(object)).


% 
% ectest/ec_reader_test_examples.e:1315
% [object,time] % Releases(E(object),F(object),time).
releases(e(Object), f(Object), Time).


% 
% ectest/ec_reader_test_examples.e:1316
% [object,time] % Terminates(E(object),F(object),time).
terminates(e(Object), f(Object), Time).


% 
% 
% Happens(E(O1),0).
happens(e(o1), 0).


% 
% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% ectest/ec_reader_test_examples.e:1322
% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter2/Sleep4.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:1348
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% 
% ectest/ec_reader_test_examples.e:1354
% agent Nathan
t(agent, nathan).


% 
% fluent Awake(agent)
fluent(awake(agent)).


% 
% event WakeUp(agent)
event(wakeUp(agent)).


% event FallAsleep(agent)
event(fallAsleep(agent)).


% ectest/ec_reader_test_examples.e:1360
% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:1363
% [agent,time] % Initiates(WakeUp(agent),Awake(agent),time).
initiates(wakeUp(Agent), awake(Agent), Time).


% 
% ectest/ec_reader_test_examples.e:1364
% [agent,time] % Terminates(FallAsleep(agent),Awake(agent),time).
terminates(fallAsleep(Agent), awake(Agent), Time).


% 
% 
% ; Delta
% 
% Happens(WakeUp(Nathan),1).
happens(wakeUp(nathan), 1).


% 
% 
% ; entailed:
% ; HoldsAt(Awake(Nathan),3).
% ectest/ec_reader_test_examples.e:1372
% 
% completion Happens
completion(happens).


% 
% range time 0 3
range(time, 0, 3).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:1379
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter2/Inconsistency4.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:1403
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort object
sort(object).


% object O1
t(object, o1).


% ectest/ec_reader_test_examples.e:1409
% 
% event E(object)
event(e(object)).


% 
% fluent F1(object)
fluent(f1(object)).


% fluent F2(object)
fluent(f2(object)).


% 
% ectest/ec_reader_test_examples.e:1415
% [object,time]% 
% Initiates(E(object),F1(object),time).
initiates(e(Object), f1(Object), Time).


% 
% 
% ectest/ec_reader_test_examples.e:1418
% [object,time]% 
% HoldsAt(F1(object),time) <-> HoldsAt(F2(object),time).
holds_at(f1(Object), Time) <->
	holds_at(f2(Object), Time).


% 
% 
% !HoldsAt(F2(O1),0).
not(holds_at(f2(o1), 0)).


% 
% Happens(E(O1),0).
happens(e(o1), 0).


% 
% 
% ectest/ec_reader_test_examples.e:1424
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% 
% ectest/ec_reader_test_examples.e:1430
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter2/Inconsistency1.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:1452
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort object
sort(object).


% object O1
t(object, o1).


% ectest/ec_reader_test_examples.e:1458
% 
% fluent F(object)
fluent(f(object)).


% 
% event E(object)
event(e(object)).


% 
% ectest/ec_reader_test_examples.e:1463
% [object,time] % Initiates(E(object),F(object),time).
initiates(e(Object), f(Object), Time).


% 
% ectest/ec_reader_test_examples.e:1464
% [object,time] % Terminates(E(object),F(object),time).
terminates(e(Object), f(Object), Time).


% 
% 
% Happens(E(O1),0).
happens(e(o1), 0).


% 
% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% ectest/ec_reader_test_examples.e:1470
% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter2/Inconsistency2.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:1496
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort object
sort(object).


% object O1
t(object, o1).


% ectest/ec_reader_test_examples.e:1502
% 
% fluent F(object)
fluent(f(object)).


% 
% event E(object)
event(e(object)).


% 
% ectest/ec_reader_test_examples.e:1507
% [object,time] % Releases(E(object),F(object),time).
releases(e(Object), f(Object), Time).


% 
% ectest/ec_reader_test_examples.e:1508
% [object,time] % Initiates(E(object),F(object),time).
initiates(e(Object), f(Object), Time).


% 
% 
% Happens(E(O1),0).
happens(e(o1), 0).


% 
% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% ectest/ec_reader_test_examples.e:1514
% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter8/CameraWithFlash.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:1540
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort camera
sort(camera).


% 
% ectest/ec_reader_test_examples.e:1546
% camera Camera1
t(camera, camera1).


% 
% fluent ProperlyExposedPicture(camera)
fluent(properlyExposedPicture(camera)).


% fluent ImproperlyExposedPicture(camera)
fluent(improperlyExposedPicture(camera)).


% 
% event ReleaseShutter(camera)
event(releaseShutter(camera)).


% ectest/ec_reader_test_examples.e:1552
% event TriggerFlash(camera)
event(triggerFlash(camera)).


% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:1556
% [camera,time]% 
% Happens(TriggerFlash(camera),time) ->
% Initiates(ReleaseShutter(camera),ProperlyExposedPicture(camera),time).
happens(triggerFlash(Camera), Time) ->
	initiates(releaseShutter(Camera),
		  properlyExposedPicture(Camera),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:1560
% [camera,time]% 
% !Happens(TriggerFlash(camera),time) ->
% Initiates(ReleaseShutter(camera),ImproperlyExposedPicture(camera),time).
not(happens(triggerFlash(Camera), Time)) ->
	initiates(releaseShutter(Camera),
		  improperlyExposedPicture(Camera),
		  Time).


% 
% 
% ; Delta
% 
% ectest/ec_reader_test_examples.e:1566
% Delta:
directive(delta).


 % Happens(ReleaseShutter(Camera1),0).
happens(releaseShutter(camera1), 0).


% 
% Delta:
directive(delta).


 % Happens(TriggerFlash(Camera1),1).
happens(triggerFlash(camera1), 1).


% 
% Delta:
directive(delta).


 % Happens(ReleaseShutter(Camera1),1).
happens(releaseShutter(camera1), 1).


% 
% 
% ; added:
% ectest/ec_reader_test_examples.e:1571
% [camera] % !HoldsAt(ImproperlyExposedPicture(camera),0).
not(holds_at(improperlyExposedPicture(Camera), 0)).


% 
% ectest/ec_reader_test_examples.e:1572
% [camera] % !HoldsAt(ProperlyExposedPicture(camera),0).
not(holds_at(properlyExposedPicture(Camera), 0)).


% 
% 
% completion Delta Happens
completion([delta, happens]).


% 
% range time 0 2
range(time, 0, 2).


% range offset 1 1
range(offset, 1, 1).


% ectest/ec_reader_test_examples.e:1578
% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter8/MovingRobot.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @inproceedings{Shanahan:1996,
% ;   author = "Murray Shanahan",
% ;   year = "1996",
% ;   title = "Robotics and the common sense informatic situation",
% ;   editor = "Wolfgang Wahlster",
% ;   booktitle = "\uppercase{P}roceedings of the \uppercase{T}welfth \uppercase{E}uropean \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
% ;   pages = "684--688",
% ;   address = "Chichester, UK",
% ;   publisher = "John Wiley",
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
% ectest/ec_reader_test_examples.e:1615
% 
% option renaming off
option(renaming, off).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% ectest/ec_reader_test_examples.e:1621
% sort coord: integer
subsort(coord, integer).


% 
% sort direction: integer
subsort(direction, integer).


% ; 0 -> 0, 1 -> 90, 2 -> 180, 3 -> 370
% 
% sort robot
sort(robot).


% ectest/ec_reader_test_examples.e:1627
% 
% robot Robot1
t(robot, robot1).


% 
% function Sin(direction): coord
function(sin(direction), coord).


% function Cos(direction): coord
function(cos(direction), coord).


% 
% ectest/ec_reader_test_examples.e:1633
% Sin(0)=0.
sin(0)=0.


% 
% Sin(1)=1.
sin(1)=1.


% 
% Sin(2)=2.
sin(2)=2.


% 
% Sin(3)=3.
sin(3)=3.


% 
% 
% Cos(0)=1.
cos(0)=1.


% 
% ectest/ec_reader_test_examples.e:1639
% Cos(1)=2.
cos(1)=2.


% 
% Cos(2)=3.
cos(2)=3.


% 
% Cos(3)=4.
cos(3)=4.


% 
% 
% fluent Direction(robot,direction)
fluent(direction(robot, direction)).


% fluent Location(robot,coord,coord)
fluent(location(robot, coord, coord)).


% ectest/ec_reader_test_examples.e:1645
% 
% event MoveLeftWheel(robot)
event(moveLeftWheel(robot)).


% event MoveRightWheel(robot)
event(moveRightWheel(robot)).


% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:1651
% [robot,direction1,direction2,time]% 
% !Happens(MoveRightWheel(robot),time) &
% HoldsAt(Direction(robot,direction1),time) &
% direction2 = (direction1-1)->
% Initiates(MoveLeftWheel(robot),Direction(robot,direction2),time).
not(happens(moveRightWheel(Robot), Time)), holds_at(direction(Robot, Direction1), Time), Direction2=Direction1-1 ->
	initiates(moveLeftWheel(Robot),
		  direction(Robot, Direction2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:1657
% [robot,direction,time]% 
% !Happens(MoveRightWheel(robot),time) &
% HoldsAt(Direction(robot,direction),time) ->
% Terminates(MoveLeftWheel(robot),Direction(robot,direction),time).
not(happens(moveRightWheel(Robot), Time)), holds_at(direction(Robot, Direction), Time) ->
	terminates(moveLeftWheel(Robot),
		   direction(Robot, Direction),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:1662
% [robot,direction1,direction2,time]% 
% !Happens(MoveLeftWheel(robot),time) &
% HoldsAt(Direction(robot,direction1),time) &
% direction2 = (direction1+1)->
% Initiates(MoveRightWheel(robot),Direction(robot,direction2),time).
not(happens(moveLeftWheel(Robot), Time)), holds_at(direction(Robot, Direction1), Time), Direction2=Direction1+1 ->
	initiates(moveRightWheel(Robot),
		  direction(Robot, Direction2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:1668
% [robot,direction,time]% 
% !Happens(MoveLeftWheel(robot),time) &
% HoldsAt(Direction(robot,direction),time) ->
% Terminates(MoveRightWheel(robot),Direction(robot,direction),time).
not(happens(moveLeftWheel(Robot), Time)), holds_at(direction(Robot, Direction), Time) ->
	terminates(moveRightWheel(Robot),
		   direction(Robot, Direction),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:1673
% [robot,direction,coord1,coord2,coord3,coord4,time]% 
% Happens(MoveLeftWheel(robot),time) &
% HoldsAt(Location(robot,coord1,coord2),time) &
% HoldsAt(Direction(robot,direction),time) &
% coord3 = coord1+Cos(direction) &
% coord4 = coord2+Sin(direction) ->
% Initiates(MoveRightWheel(robot),
%           Location(robot,coord3,coord4),
%           time).
happens(moveLeftWheel(Robot), Time), holds_at(location(Robot, Coord1, Coord2), Time), holds_at(direction(Robot, Direction), Time), Coord3=Coord1+cos(Direction), Coord4=Coord2+sin(Direction) ->
	initiates(moveRightWheel(Robot),
		  location(Robot, Coord3, Coord4),
		  Time).


% ectest/ec_reader_test_examples.e:1681
% 
% 
% ectest/ec_reader_test_examples.e:1683
% [robot,coord1,coord2,time]% 
% Happens(MoveLeftWheel(robot),time) &
% HoldsAt(Location(robot,coord1,coord2),time) ->
% ; FIX: Direction not needed!!
% ; HoldsAt(Direction(robot,direction),time) ->
% Terminates(MoveRightWheel(robot),Location(robot,coord1,coord2),time).
happens(moveLeftWheel(Robot), Time), holds_at(location(Robot, Coord1, Coord2), Time) ->
	terminates(moveRightWheel(Robot),
		   location(Robot, Coord1, Coord2),
		   Time).


% 
% ectest/ec_reader_test_examples.e:1689
% 
% ; Delta
% 
% Happens(MoveRightWheel(Robot1),0).
happens(moveRightWheel(robot1), 0).


% 
% Happens(MoveLeftWheel(Robot1),1).
happens(moveLeftWheel(robot1), 1).


% 
% Happens(MoveRightWheel(Robot1),1).
happens(moveRightWheel(robot1), 1).


% 
% ectest/ec_reader_test_examples.e:1695
% 
% ; Psi
% 
% 
% ectest/ec_reader_test_examples.e:1699
% [robot,coord1,coord2,coord3,coord4,time]% 
% HoldsAt(Location(robot,coord1,coord2),time) &
% HoldsAt(Location(robot,coord3,coord4),time) ->
% coord1=coord3 &
% coord2=coord4.
holds_at(location(Robot, Coord1, Coord2), Time), holds_at(location(Robot, Coord3, Coord4), Time) ->
	Coord1=Coord3,
	Coord2=Coord4.


% 
% 
% ectest/ec_reader_test_examples.e:1705
% [robot,direction1,direction2,time]% 
% HoldsAt(Direction(robot,direction1),time) &
% HoldsAt(Direction(robot,direction2),time) ->
% direction1=direction2.
holds_at(direction(Robot, Direction1), Time), holds_at(direction(Robot, Direction2), Time) ->
	Direction1=Direction2.


% 
% 
% ; Gamma
% ectest/ec_reader_test_examples.e:1711
% 
% HoldsAt(Location(Robot1,0,0),0).
holds_at(location(robot1, 0, 0), 0).


% 
% HoldsAt(Direction(Robot1,0),0).
holds_at(direction(robot1, 0), 0).


% 
% 
% completion Happens
completion(happens).


% 
% ectest/ec_reader_test_examples.e:1717
% range time 0 3
range(time, 0, 3).


% range coord 0 3
range(coord, 0, 3).


% range direction 0 3
range(direction, 0, 3).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:1723
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter8/PatHeadRubStomach.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:1747
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% 
% ectest/ec_reader_test_examples.e:1753
% event PatHead(agent)
event(patHead(agent)).


% event RubStomach(agent)
event(rubStomach(agent)).


% 
% agent Nathan
t(agent, nathan).


% 
% ; Delta
% ectest/ec_reader_test_examples.e:1759
% 
% ectest/ec_reader_test_examples.e:1760
% [agent,time]% 
% Happens(PatHead(agent),time) ->
% !Happens(RubStomach(agent),time).
happens(patHead(Agent), Time) ->
	not(happens(rubStomach(Agent), Time)).


% 
% 
% Happens(PatHead(Nathan),0) & Happens(RubStomach(Nathan),0).
happens(patHead(nathan), 0),
happens(rubStomach(nathan), 0).


% 
% 
% ectest/ec_reader_test_examples.e:1766
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% 
% ectest/ec_reader_test_examples.e:1772
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter10/MovingNewspaperAndBox.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:1794
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort object
sort(object).


% sort agent: object
subsort(agent, object).


% ectest/ec_reader_test_examples.e:1800
% sort physobj: object
subsort(physobj, object).


% sort room: object
subsort(room, object).


% 
% fluent IN(object,object)
fluent(in(object, object)).


% fluent INROOM(object,room)
fluent(inroom(object, room)).


% noninertial INROOM
noninertial(inroom).


% ectest/ec_reader_test_examples.e:1806
% 
% event MOVE(agent,object,object,object)
event(move(agent, object, object, object)).


% 
% agent Lisa
t(agent, lisa).


% physobj Box, Newspaper
t(physobj, box).


t(physobj, newspaper).


% room Kitchen, LivingRoom
t(room, kitchen).


t(room, livingRoom).


% ectest/ec_reader_test_examples.e:1812
% 
% ; Sigma
% 
% ; RS10
% ectest/ec_reader_test_examples.e:1816
% [agent,physobj1,physobj2,room,time]% 
% HoldsAt(IN(agent,room),time) &
% HoldsAt(IN(physobj1,room),time) &
% HoldsAt(INROOM(physobj2,room),time) ->
% Initiates(MOVE(agent,physobj1,room,physobj2),IN(physobj1,physobj2),time).
holds_at(in(Agent, Room), Time), holds_at(in(Physobj1, Room), Time), holds_at(inroom(Physobj2, Room), Time) ->
	initiates(move(Agent, Physobj1, Room, Physobj2),
		  in(Physobj1, Physobj2),
		  Time).


% 
% 
% ; RS11
% ectest/ec_reader_test_examples.e:1823
% [agent,physobj1,physobj2,room,time]% 
% HoldsAt(IN(agent,room),time) &
% HoldsAt(IN(physobj1,room),time) &
% HoldsAt(INROOM(physobj2,room),time) ->
% Terminates(MOVE(agent,physobj1,room,physobj2),IN(physobj1,room),time).
holds_at(in(Agent, Room), Time), holds_at(in(Physobj1, Room), Time), holds_at(inroom(Physobj2, Room), Time) ->
	terminates(move(Agent, Physobj1, Room, Physobj2),
		   in(Physobj1, Room),
		   Time).


% 
% 
% ; RS12
% ectest/ec_reader_test_examples.e:1830
% [agent,physobj1,physobj2,room,time]% 
% HoldsAt(IN(agent,room),time) ->
% Initiates(MOVE(agent,physobj1,physobj2,room),IN(physobj1,room),time).
holds_at(in(Agent, Room), Time) ->
	initiates(move(Agent, Physobj1, Physobj2, Room),
		  in(Physobj1, Room),
		  Time).


% 
% 
% ; RS13
% ectest/ec_reader_test_examples.e:1835
% [agent,physobj1,physobj2,room,time]% 
% HoldsAt(IN(agent,room),time) ->
% Terminates(MOVE(agent,physobj1,physobj2,room),IN(physobj1,physobj2),time).
holds_at(in(Agent, Room), Time) ->
	terminates(move(Agent, Physobj1, Physobj2, Room),
		   in(Physobj1, Physobj2),
		   Time).


% 
% 
% ; RS14
% ectest/ec_reader_test_examples.e:1840
% [agent,room1,room2,time]% 
% HoldsAt(IN(agent,room1),time) ->
% Initiates(MOVE(agent,agent,room1,room2),IN(agent,room2),time).
holds_at(in(Agent, Room1), Time) ->
	initiates(move(Agent, Agent, Room1, Room2),
		  in(Agent, Room2),
		  Time).


% 
% 
% ; RS15
% ectest/ec_reader_test_examples.e:1845
% [agent,room1,room2,time]% 
% HoldsAt(IN(agent,room1),time) ->
% Terminates(MOVE(agent,agent,room1,room2),IN(agent,room1),time).
holds_at(in(Agent, Room1), Time) ->
	terminates(move(Agent, Agent, Room1, Room2),
		   in(Agent, Room1),
		   Time).


% 
% 
% ; RS16
% ectest/ec_reader_test_examples.e:1850
% [agent,physobj,room,time]% 
% HoldsAt(IN(agent,room),time) &
% HoldsAt(IN(physobj,room),time) ->
% Initiates(MOVE(agent,physobj,room,agent),IN(physobj,agent),time).
holds_at(in(Agent, Room), Time), holds_at(in(Physobj, Room), Time) ->
	initiates(move(Agent, Physobj, Room, Agent),
		  in(Physobj, Agent),
		  Time).


% 
% 
% ; RS17
% ectest/ec_reader_test_examples.e:1856
% [agent,physobj,room,time]% 
% HoldsAt(IN(agent,room),time) &
% HoldsAt(IN(physobj,room),time) ->
% Terminates(MOVE(agent,physobj,room,agent),IN(physobj,room),time).
holds_at(in(Agent, Room), Time), holds_at(in(Physobj, Room), Time) ->
	terminates(move(Agent, Physobj, Room, Agent),
		   in(Physobj, Room),
		   Time).


% 
% 
% ; RS18
% ectest/ec_reader_test_examples.e:1862
% [agent,physobj,room,time]% 
% HoldsAt(IN(physobj,agent),time) &
% HoldsAt(IN(agent,room),time) ->
% Initiates(MOVE(agent,physobj,agent,room),IN(physobj,room),time).
holds_at(in(Physobj, Agent), Time), holds_at(in(Agent, Room), Time) ->
	initiates(move(Agent, Physobj, Agent, Room),
		  in(Physobj, Room),
		  Time).


% 
% 
% ; RS19
% ectest/ec_reader_test_examples.e:1868
% [agent,physobj,room,time]% 
% HoldsAt(IN(physobj,agent),time) &
% HoldsAt(IN(agent,room),time) ->
% Terminates(MOVE(agent,physobj,agent,room),IN(physobj,agent),time).
holds_at(in(Physobj, Agent), Time), holds_at(in(Agent, Room), Time) ->
	terminates(move(Agent, Physobj, Agent, Room),
		   in(Physobj, Agent),
		   Time).


% 
% 
% ; Delta
% ectest/ec_reader_test_examples.e:1874
% 
% Happens(MOVE(Lisa,Newspaper,LivingRoom,Box),0).
happens(move(lisa, newspaper, livingRoom, box), 0).


% 
% Happens(MOVE(Lisa,Box,LivingRoom,Lisa),1).
happens(move(lisa, box, livingRoom, lisa), 1).


% 
% Happens(MOVE(Lisa,Lisa,LivingRoom,Kitchen),2).
happens(move(lisa, lisa, livingRoom, kitchen), 2).


% 
% Happens(MOVE(Lisa,Box,Lisa,Kitchen),3).
happens(move(lisa, box, lisa, kitchen), 3).


% 
% Happens(MOVE(Lisa,Lisa,Kitchen,LivingRoom),4).
happens(move(lisa, lisa, kitchen, livingRoom), 4).


% 
% ectest/ec_reader_test_examples.e:1880
% 
% ; Psi
% 
% ; RS1
% ectest/ec_reader_test_examples.e:1884
% [object,time] % !HoldsAt(IN(object,object),time).
not(holds_at(in(Object, Object), Time)).


% 
% 
% ; RS2
% ectest/ec_reader_test_examples.e:1887
% [object1,object2,time]% 
% HoldsAt(IN(object1,object2),time) ->
% !HoldsAt(IN(object2,object1),time).
holds_at(in(Object1, Object2), Time) ->
	not(holds_at(in(Object2, Object1), Time)).


% 
% 
% ; RS3
% ectest/ec_reader_test_examples.e:1892
% [object1,object2,object3,time]% 
% HoldsAt(IN(object1,object2),time) &
% HoldsAt(IN(object2,object3),time) ->
% !HoldsAt(IN(object1,object3),time).
holds_at(in(Object1, Object2), Time), holds_at(in(Object2, Object3), Time) ->
	not(holds_at(in(Object1, Object3), Time)).


% 
% 
% ; RS4
% ectest/ec_reader_test_examples.e:1898
% [object,object1,object2,time]% 
% HoldsAt(IN(object,object1),time) &
% HoldsAt(IN(object,object2),time) ->
% object1=object2.
holds_at(in(Object, Object1), Time), holds_at(in(Object, Object2), Time) ->
	Object1=Object2.


% 
% 
% ; RS7
% ectest/ec_reader_test_examples.e:1904
% [object,room,time]% 
% HoldsAt(IN(object,room),time) ->
% HoldsAt(INROOM(object,room),time).
holds_at(in(Object, Room), Time) ->
	holds_at(inroom(Object, Room), Time).


% 
% 
% ; RS8
% ectest/ec_reader_test_examples.e:1909
% [object1,object2,room,time]% 
% HoldsAt(IN(object1,object2),time) &
% HoldsAt(INROOM(object2,room),time) ->
% HoldsAt(INROOM(object1,room),time).
holds_at(in(Object1, Object2), Time), holds_at(inroom(Object2, Room), Time) ->
	holds_at(inroom(Object1, Room), Time).


% 
% 
% ; RS9
% ectest/ec_reader_test_examples.e:1915
% [object,room1,room2,time]% 
% HoldsAt(INROOM(object,room1),time) &
% HoldsAt(INROOM(object,room2),time) ->
% room1=room2.
holds_at(inroom(Object, Room1), Time), holds_at(inroom(Object, Room2), Time) ->
	Room1=Room2.


% 
% 
% ; Gamma
% ectest/ec_reader_test_examples.e:1921
% 
% HoldsAt(IN(Lisa,LivingRoom),0).
holds_at(in(lisa, livingRoom), 0).


% 
% HoldsAt(IN(Newspaper,LivingRoom),0).
holds_at(in(newspaper, livingRoom), 0).


% 
% HoldsAt(IN(Box,LivingRoom),0).
holds_at(in(box, livingRoom), 0).


% 
% 
% ; added:
% ectest/ec_reader_test_examples.e:1927
% [room1,room2,time] % !HoldsAt(INROOM(room1,room2),time).
not(holds_at(inroom(Room1, Room2), Time)).


% 
% ectest/ec_reader_test_examples.e:1928
% [room,object,time] % !HoldsAt(IN(room,object),time).
not(holds_at(in(Room, Object), Time)).


% 
% 
% ; entailed:
% ; HoldsAt(IN(Lisa,LivingRoom),5).
% ; HoldsAt(IN(Box,Kitchen),5).
% ; HoldsAt(INROOM(Newspaper,Kitchen),5).
% ectest/ec_reader_test_examples.e:1934
% 
% completion Happens
completion(happens).


% 
% range time 0 5
range(time, 0, 5).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:1941
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter10/TwoScreens.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @phdthesis{Cassimatis:2002,
% ;   author = "Nicholas L. Cassimatis",
% ;   year = "2002",
% ;   title = "Polyscheme: A Cognitive Architecture for Integrating Multiple Representation and Inference Schemes",
% ;   address = "Cambridge, MA",
% ;   school = "Program in Media Arts and Sciences, School of Architecture and Planning, Massachusetts Institute of Technology",
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
% ectest/ec_reader_test_examples.e:1973
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort object
sort(object).


% sort location
sort(location).


% ectest/ec_reader_test_examples.e:1979
% 
% object O1, O2
t(object, o1).


t(object, o2).


% location L1, L2, L3, L4, L5
t(location, l1).


t(location, l2).


t(location, l3).


t(location, l4).


t(location, l5).


% 
% predicate Adjacent(location,location)
predicate(adjacent(location, location)).


% predicate Equal(object,object)
predicate(equal(object, object)).


% ectest/ec_reader_test_examples.e:1985
% 
% fluent At(object,location)
fluent(at(object, location)).


% event Move(object,location,location)
event(move(object, location, location)).


% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:1991
% [object,location1,location2,time]% 
% HoldsAt(At(object,location1),time) &
% Adjacent(location1,location2) ->
% Initiates(Move(object,location1,location2),At(object,location2),time).
holds_at(at(Object, Location1), Time), adjacent(Location1, Location2) ->
	initiates(move(Object, Location1, Location2),
		  at(Object, Location2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:1996
% [object,location1,location2,time]% 
% HoldsAt(At(object,location1),time) &
% Adjacent(location1,location2) ->
% Terminates(Move(object,location1,location2),At(object,location1),time).
holds_at(at(Object, Location1), Time), adjacent(Location1, Location2) ->
	terminates(move(Object, Location1, Location2),
		   at(Object, Location1),
		   Time).


% 
% 
% ; Psi
% ectest/ec_reader_test_examples.e:2002
% 
% ectest/ec_reader_test_examples.e:2003
% [object,location1,location2,time]% 
% HoldsAt(At(object,location1),time) &
% HoldsAt(At(object,location2),time) ->
% location1=location2.
holds_at(at(Object, Location1), Time), holds_at(at(Object, Location2), Time) ->
	Location1=Location2.


% 
% 
% ectest/ec_reader_test_examples.e:2008
% [object,time]% 
% ectest/ec_reader_test_examples.e:2009
% {location} % HoldsAt(At(object,location),time).
exists([Location], holds_at(at(Object, Location), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:2011
% [object1,object2,location,time]% 
% HoldsAt(At(object1,location),time) &
% HoldsAt(At(object2,location),time) ->
% Equal(object1,object2).
holds_at(at(Object1, Location), Time), holds_at(at(Object2, Location), Time) ->
	equal(Object1, Object2).


% 
% 
% ectest/ec_reader_test_examples.e:2016
% [location1, location2]% 
% Adjacent(location1,location2) <->
% Adjacent(location2,location1).
adjacent(Location1, Location2) <->
	adjacent(Location2, Location1).


% 
% 
% ectest/ec_reader_test_examples.e:2020
% [object1,object2]% 
% Equal(object1,object2) <->
% Equal(object2,object1).
equal(Object1, Object2) <->
	equal(Object2, Object1).


% 
% 
% ; Gamma
% 
% ectest/ec_reader_test_examples.e:2026
% [location1,location2]% 
% Adjacent(location1,location2) <->
% (location1=L1 & location2=L2) |
% (location1=L2 & location2=L1) |
% (location1=L2 & location2=L3) |
% (location1=L3 & location2=L2) |
% (location1=L3 & location2=L4) |
% (location1=L4 & location2=L3) |
% (location1=L4 & location2=L5) |
% (location1=L5 & location2=L4).
adjacent(Location1, Location2) <->
	(   Location1=l1,
	    Location2=l2
	;   Location1=l2,
	    Location2=l1
	;   Location1=l2,
	    Location2=l3
	;   Location1=l3,
	    Location2=l2
	;   Location1=l3,
	    Location2=l4
	;   Location1=l4,
	    Location2=l3
	;   Location1=l4,
	    Location2=l5
	;   Location1=l5,
	    Location2=l4
	).


% ectest/ec_reader_test_examples.e:2035
% 
% 
% HoldsAt(At(O1,L1),0).
holds_at(at(o1, l1), 0).


% 
% ectest/ec_reader_test_examples.e:2038
% [object] % !HoldsAt(At(object,L5),0).
not(holds_at(at(Object, l5), 0)).


% 
% 
% HoldsAt(At(O2,L5),4).
holds_at(at(o2, l5), 4).


% 
% ectest/ec_reader_test_examples.e:2041
% [object] % !HoldsAt(At(object,L1),4).
not(holds_at(at(Object, l1), 4)).


% 
% 
% ectest/ec_reader_test_examples.e:2043
% [object,time] % !HoldsAt(At(object,L3),time).
not(holds_at(at(Object, l3), Time)).


% 
% 
% ; ADDED:
% ectest/ec_reader_test_examples.e:2046
% [object,location1,location2,time]% 
% Happens(Move(object,location1,location2),time) ->
% HoldsAt(At(object,location1),time) &
% Adjacent(location1,location2).
happens(move(Object, Location1, Location2), Time) ->
	holds_at(at(Object, Location1), Time),
	adjacent(Location1, Location2).


% 
% 
% ectest/ec_reader_test_examples.e:2051
% [object1,object2,location1,location2,time]% 
% Equal(object1,object2) &
% Happens(Move(object1,location1,location2),time) ->
% Happens(Move(object2,location1,location2),time).
equal(Object1, Object2), happens(move(Object1, Location1, Location2), Time) ->
	happens(move(Object2, Location1, Location2),
		Time).


% 
% 
% ; entailed: !Equal(O1,O2).
% ectest/ec_reader_test_examples.e:2057
% 
% range time 0 4
range(time, 0, 4).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% ectest/ec_reader_test_examples.e:2063
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter10/OneScreen.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @phdthesis{Cassimatis:2002,
% ;   author = "Nicholas L. Cassimatis",
% ;   year = "2002",
% ;   title = "Polyscheme: A Cognitive Architecture for Integrating Multiple Representation and Inference Schemes",
% ;   address = "Cambridge, MA",
% ;   school = "Program in Media Arts and Sciences, School of Architecture and Planning, Massachusetts Institute of Technology",
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
% ectest/ec_reader_test_examples.e:2094
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort object
sort(object).


% sort location
sort(location).


% ectest/ec_reader_test_examples.e:2100
% 
% object O1, O2
t(object, o1).


t(object, o2).


% location L1, L2, L3
t(location, l1).


t(location, l2).


t(location, l3).


% 
% predicate Adjacent(location,location)
predicate(adjacent(location, location)).


% predicate Equal(object,object)
predicate(equal(object, object)).


% ectest/ec_reader_test_examples.e:2106
% 
% fluent At(object,location)
fluent(at(object, location)).


% event Move(object,location,location)
event(move(object, location, location)).


% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:2112
% [object,location1,location2,time]% 
% HoldsAt(At(object,location1),time) &
% Adjacent(location1,location2) ->
% Initiates(Move(object,location1,location2),At(object,location2),time).
holds_at(at(Object, Location1), Time), adjacent(Location1, Location2) ->
	initiates(move(Object, Location1, Location2),
		  at(Object, Location2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:2117
% [object,location1,location2,time]% 
% HoldsAt(At(object,location1),time) &
% Adjacent(location1,location2) ->
% Terminates(Move(object,location1,location2),At(object,location1),time).
holds_at(at(Object, Location1), Time), adjacent(Location1, Location2) ->
	terminates(move(Object, Location1, Location2),
		   at(Object, Location1),
		   Time).


% 
% 
% ; Psi
% ectest/ec_reader_test_examples.e:2123
% 
% ectest/ec_reader_test_examples.e:2124
% [object,location1,location2,time]% 
% HoldsAt(At(object,location1),time) &
% HoldsAt(At(object,location2),time) ->
% location1=location2.
holds_at(at(Object, Location1), Time), holds_at(at(Object, Location2), Time) ->
	Location1=Location2.


% 
% 
% ectest/ec_reader_test_examples.e:2129
% [object,time]% 
% ectest/ec_reader_test_examples.e:2130
% {location} % HoldsAt(At(object,location),time).
exists([Location], holds_at(at(Object, Location), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:2132
% [object1,object2,location,time]% 
% HoldsAt(At(object1,location),time) &
% HoldsAt(At(object2,location),time) ->
% Equal(object1,object2).
holds_at(at(Object1, Location), Time), holds_at(at(Object2, Location), Time) ->
	equal(Object1, Object2).


% 
% 
% ectest/ec_reader_test_examples.e:2137
% [location1, location2]% 
% Adjacent(location1,location2) <->
% Adjacent(location2,location1).
adjacent(Location1, Location2) <->
	adjacent(Location2, Location1).


% 
% 
% ectest/ec_reader_test_examples.e:2141
% [object1,object2]% 
% Equal(object1,object2) <->
% Equal(object2,object1).
equal(Object1, Object2) <->
	equal(Object2, Object1).


% 
% 
% ; Gamma
% 
% ectest/ec_reader_test_examples.e:2147
% [location1,location2]% 
% Adjacent(location1,location2) <->
% (location1=L1 & location2=L2) |
% (location1=L2 & location2=L1) |
% (location1=L2 & location2=L3) |
% (location1=L3 & location2=L2).
adjacent(Location1, Location2) <->
	(   Location1=l1,
	    Location2=l2
	;   Location1=l2,
	    Location2=l1
	;   Location1=l2,
	    Location2=l3
	;   Location1=l3,
	    Location2=l2
	).


% 
% ectest/ec_reader_test_examples.e:2153
% 
% HoldsAt(At(O1,L1),0).
holds_at(at(o1, l1), 0).


% 
% ectest/ec_reader_test_examples.e:2155
% [object] % !HoldsAt(At(object,L3),0).
not(holds_at(at(Object, l3), 0)).


% 
% 
% ectest/ec_reader_test_examples.e:2157
% [object] % !HoldsAt(At(object,L1),1).
not(holds_at(at(Object, l1), 1)).


% 
% ectest/ec_reader_test_examples.e:2158
% [object] % !HoldsAt(At(object,L3),1).
not(holds_at(at(Object, l3), 1)).


% 
% 
% HoldsAt(At(O2,L3),2).
holds_at(at(o2, l3), 2).


% 
% ectest/ec_reader_test_examples.e:2161
% [object] % !HoldsAt(At(object,L1),2).
not(holds_at(at(Object, l1), 2)).


% 
% 
% ; ADDED:
% ectest/ec_reader_test_examples.e:2164
% [object,location1,location2,time]% 
% Happens(Move(object,location1,location2),time) ->
% HoldsAt(At(object,location1),time) &
% Adjacent(location1,location2).
happens(move(Object, Location1, Location2), Time) ->
	holds_at(at(Object, Location1), Time),
	adjacent(Location1, Location2).


% 
% 
% ectest/ec_reader_test_examples.e:2169
% [object1,object2,location1,location2,time]% 
% Equal(object1,object2) &
% Happens(Move(object1,location1,location2),time) ->
% Happens(Move(object2,location1,location2),time).
equal(Object1, Object2), happens(move(Object1, Location1, Location2), Time) ->
	happens(move(Object2, Location1, Location2),
		Time).


% 
% 
% range time 0 2
range(time, 0, 2).


% ectest/ec_reader_test_examples.e:2175
% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter9/RunningAndDriving.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:2202
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% sort location
sort(location).


% ectest/ec_reader_test_examples.e:2208
% 
% agent James
t(agent, james).


% location Bookstore
t(location, bookstore).


% 
% fluent Tired(agent)
fluent(tired(agent)).


% 
% ectest/ec_reader_test_examples.e:2214
% event Go(agent,location)
event(go(agent, location)).


% event Run(agent,location)
event(run(agent, location)).


% event Drive(agent,location)
event(drive(agent, location)).


% 
% ectest/ec_reader_test_examples.e:2218
% [agent,location,time]% 
% Happens(Go(agent,location),time) ->
% Happens(Run(agent,location),time) | Happens(Drive(agent,location),time).
(   ( happens(go(Agent, Location), Time)->happens(run(Agent, Location), Time)
    )
;   happens(drive(Agent, Location), Time)
).


% 
% 
% xor Run, Drive
xor([run, drive]).


% 
% ectest/ec_reader_test_examples.e:2224
% [agent,location,time] % Initiates(Run(agent,location),Tired(agent),time).
initiates(run(Agent, Location), tired(Agent), Time).


% 
% 
% !HoldsAt(Tired(James),0).
not(holds_at(tired(james), 0)).


% 
% Happens(Go(James,Bookstore),0).
happens(go(james, bookstore), 0).


% 
% HoldsAt(Tired(James),1).
holds_at(tired(james), 1).


% 
% 
% ectest/ec_reader_test_examples.e:2230
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% 
% ectest/ec_reader_test_examples.e:2236
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter9/RouletteWheel.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:2258
% 
% option modeldiff on
option(modeldiff, on).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% ectest/ec_reader_test_examples.e:2264
% sort dealer
sort(dealer).


% sort wheel
sort(wheel).


% sort value: integer
subsort(value, integer).


% 
% wheel Wheel1
t(wheel, wheel1).


% dealer Dealer1
t(dealer, dealer1).


% ectest/ec_reader_test_examples.e:2270
% 
% fluent WheelNumberDeterminer(wheel,value)
fluent(wheelNumberDeterminer(wheel, value)).


% fluent WheelNumber(wheel,value)
fluent(wheelNumber(wheel, value)).


% noninertial WheelNumberDeterminer
noninertial(wheelNumberDeterminer).


% 
% event Spin(dealer,wheel)
event(spin(dealer, wheel)).


% ectest/ec_reader_test_examples.e:2276
% event Reset(dealer,wheel)
event(reset(dealer, wheel)).


% 
% ectest/ec_reader_test_examples.e:2278
% [wheel,time]% 
% ectest/ec_reader_test_examples.e:2279
% {value}% 
% HoldsAt(WheelNumberDeterminer(wheel,value),time).
exists([Value], holds_at(wheelNumberDeterminer(Wheel, Value), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:2282
% [wheel,value1,value2,time]% 
% HoldsAt(WheelNumberDeterminer(wheel,value1),time) &
% HoldsAt(WheelNumberDeterminer(wheel,value2),time) ->
% value1=value2.
holds_at(wheelNumberDeterminer(Wheel, Value1), Time), holds_at(wheelNumberDeterminer(Wheel, Value2), Time) ->
	Value1=Value2.


% 
% 
% ectest/ec_reader_test_examples.e:2287
% [dealer,wheel,value,time]% 
% HoldsAt(WheelNumberDeterminer(wheel,value),time) ->
% Initiates(Spin(dealer,wheel),WheelNumber(wheel,value),time).
holds_at(wheelNumberDeterminer(Wheel, Value), Time) ->
	initiates(spin(Dealer, Wheel),
		  wheelNumber(Wheel, Value),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:2291
% [dealer,wheel,value1,value2,time]% 
% HoldsAt(WheelNumber(wheel,value1),time) &
% HoldsAt(WheelNumberDeterminer(wheel,value2),time) &
% value1!=value2 ->
% Terminates(Spin(dealer,wheel),WheelNumber(wheel,value1),time).
holds_at(wheelNumber(Wheel, Value1), Time), holds_at(wheelNumberDeterminer(Wheel, Value2), Time), Value1\=Value2 ->
	terminates(spin(Dealer, Wheel),
		   wheelNumber(Wheel, Value1),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:2297
% [dealer,wheel,value,time]% 
% Terminates(Reset(dealer,wheel),WheelNumber(wheel,value),time).
terminates(reset(Dealer, Wheel), wheelNumber(Wheel, Value), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2300
% [wheel,value1,value2,time]% 
% HoldsAt(WheelNumber(wheel,value1),time) &
% HoldsAt(WheelNumber(wheel,value2),time) ->
% value1=value2.
holds_at(wheelNumber(Wheel, Value1), Time), holds_at(wheelNumber(Wheel, Value2), Time) ->
	Value1=Value2.


% 
% 
% ectest/ec_reader_test_examples.e:2305
% [value] % !HoldsAt(WheelNumber(Wheel1,value),0).
not(holds_at(wheelNumber(wheel1, Value), 0)).


% 
% 
% Happens(Spin(Dealer1,Wheel1),0).
happens(spin(dealer1, wheel1), 0).


% 
% ;Happens(Reset(Dealer1,Wheel1),1).
% 
% ; added to prune models
% ectest/ec_reader_test_examples.e:2311
% HoldsAt(WheelNumberDeterminer(Wheel1, 1),1).
holds_at(wheelNumberDeterminer(wheel1, 1), 1).


% 
% 
% completion Happens
completion(happens).


% 
% range value 1 3
range(value, 1, 3).


% range time 0 1
range(time, 0, 1).


% ectest/ec_reader_test_examples.e:2317
% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter14/NetBill1.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @inproceedings{SirbuTygar:1995,
% ;   author = "Marvin A. Sirbu and J. D. Tygar",
% ;   year = "1995",
% ;   title = "Net\uppercase{B}ill: An \uppercase{I}nternet commerce system optimized for network delivered services",
% ;   editor = "
% ;   booktitle = "40th \uppercase{IEEE} \uppercase{C}omputer \uppercase{S}ociety \uppercase{I}nternational \uppercase{C}onference",
% ;   pages = "20--25",
% ;   publisher = "
% ;   address = "
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
% ectest/ec_reader_test_examples.e:2355
% 
% option modeldiff on
option(modeldiff, on).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% ectest/ec_reader_test_examples.e:2361
% sort agent
sort(agent).


% agent MusicStore, Jen
t(agent, musicStore).


t(agent, jen).


% 
% sort product
sort(product).


% product BritneyCD
t(product, britneyCD).


% 
% ectest/ec_reader_test_examples.e:2367
% sort f
sort(f).


% f PurchaseRequestedJenMusicStoreBritneyCD1
t(f, purchaseRequestedJenMusicStoreBritneyCD1).


% f DeliveredMusicStoreJenBritneyCD
t(f, deliveredMusicStoreJenBritneyCD).


% f EPOSentJenMusicStore1
t(f, ePOSentJenMusicStore1).


% 
% sort amount: integer
subsort(amount, integer).


% ectest/ec_reader_test_examples.e:2373
% 
% fluent C(agent,agent,f)
fluent(c(agent, agent, f)).


% fluent CC(agent,agent,f,f)
fluent(cc(agent, agent, f, f)).


% 
% event CreateC(agent,agent,f)
event(createC(agent, agent, f)).


% event CreateCC(agent,agent,f,f)
event(createCC(agent, agent, f, f)).


% ectest/ec_reader_test_examples.e:2379
% event DischargeC(agent,agent,f)
event(dischargeC(agent, agent, f)).


% event DischargeCC(agent,agent,f,f)
event(dischargeCC(agent, agent, f, f)).


% 
% fluent QuoteSent(agent,agent,product,amount)
fluent(quoteSent(agent, agent, product, amount)).


% fluent PurchaseRequested(agent,agent,product,amount)
fluent(purchaseRequested(agent, agent, product, amount)).


% fluent Delivered(agent,agent,product)
fluent(delivered(agent, agent, product)).


% ectest/ec_reader_test_examples.e:2385
% fluent EPOSent(agent,agent,amount)
fluent(ePOSent(agent, agent, amount)).


% 
% event SendQuote(agent,agent,product,amount)
event(sendQuote(agent, agent, product, amount)).


% event RequestPurchase(agent,agent,product,amount)
event(requestPurchase(agent, agent, product, amount)).


% event Deliver(agent,agent,product)
event(deliver(agent, agent, product)).


% event SendEPO(agent,agent,amount)
event(sendEPO(agent, agent, amount)).


% ectest/ec_reader_test_examples.e:2391
% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:2394
% [agent1,agent2,f,time]% 
% Initiates(CreateC(agent1,agent2,f),C(agent1,agent2,f),time).
initiates(createC(Agent1, Agent2, F), c(Agent1, Agent2, F), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2397
% [agent1,agent2,f1,f2,time]% 
% Initiates(CreateCC(agent1,agent2,f1,f2),CC(agent1,agent2,f1,f2),time).
initiates(createCC(Agent1, Agent2, F1, F2), cc(Agent1, Agent2, F1, F2), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2400
% [agent1,agent2,f,time]% 
% Terminates(DischargeC(agent1,agent2,f),C(agent1,agent2,f),time).
terminates(dischargeC(Agent1, Agent2, F), c(Agent1, Agent2, F), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2403
% [agent1,agent2,f1,f2,time]% 
% Terminates(DischargeCC(agent1,agent2,f1,f2),CC(agent1,agent2,f1,f2),time).
terminates(dischargeCC(Agent1, Agent2, F1, F2), cc(Agent1, Agent2, F1, F2), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2406
% [agent1,agent2,product,amount,time]% 
% Initiates(SendQuote(agent1,agent2,product,amount),
%           QuoteSent(agent1,agent2,product,amount),
%           time).
initiates(sendQuote(Agent1, Agent2, Product, Amount), quoteSent(Agent1, Agent2, Product, Amount), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2411
% [agent1,agent2,product,amount,time]% 
% Initiates(RequestPurchase(agent1,agent2,product,amount),
%           PurchaseRequested(agent1,agent2,product,amount),
%           time).
initiates(requestPurchase(Agent1, Agent2, Product, Amount), purchaseRequested(Agent1, Agent2, Product, Amount), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2416
% [agent1,agent2,product,time]% 
% Initiates(Deliver(agent1,agent2,product),
%           Delivered(agent1,agent2,product),
%           time).
initiates(deliver(Agent1, Agent2, Product), delivered(Agent1, Agent2, Product), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2421
% [agent1,agent2,amount,time]% 
% Initiates(SendEPO(agent1,agent2,amount),
%           EPOSent(agent1,agent2,amount),
%           time).
initiates(sendEPO(Agent1, Agent2, Amount), ePOSent(Agent1, Agent2, Amount), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2426
% [agent1,agent2,product,amount,f1,f2,time]% 
% agent1=% MusicStore &
% agent2=Jen &
% product=BritneyCD &
% amount=1 &
% f1=PurchaseRequestedJenMusicStoreBritneyCD1 &
% f2=DeliveredMusicStoreJenBritneyCD ->
% Initiates(SendQuote(agent1,agent2,product,amount),
%           CC(agent1,agent2,f1,f2),
%           time).
Agent1=musicStore, Agent2=jen, Product=britneyCD, Amount=1, F1=purchaseRequestedJenMusicStoreBritneyCD1, F2=deliveredMusicStoreJenBritneyCD ->
	initiates(sendQuote(Agent1,
			    Agent2,
			    Product,
			    Amount),
		  cc(Agent1, Agent2, F1, F2),
		  Time).


% ectest/ec_reader_test_examples.e:2435
% 
% 
% ectest/ec_reader_test_examples.e:2437
% [agent1,agent2,product,amount,f1,f2,time]% 
% agent1=% Jen &
% agent2=MusicStore &
% product=BritneyCD &
% amount=1 &
% f1=DeliveredMusicStoreJenBritneyCD &
% f2=EPOSentJenMusicStore1 &
% !HoldsAt(Delivered(agent2,agent1,product),time) ->
% Initiates(RequestPurchase(agent1,agent2,product,amount),
%           CC(agent1,agent2,f1,f2),
%           time).
Agent1=jen, Agent2=musicStore, Product=britneyCD, Amount=1, F1=deliveredMusicStoreJenBritneyCD, F2=ePOSentJenMusicStore1, not(holds_at(delivered(Agent2, Agent1, Product), Time)) ->
	initiates(requestPurchase(Agent1,
				  Agent2,
				  Product,
				  Amount),
		  cc(Agent1, Agent2, F1, F2),
		  Time).


% ectest/ec_reader_test_examples.e:2447
% 
% 
% ; Delta
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:2451
% [time]% 
% HoldsAt(CC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time) &
% HoldsAt(PurchaseRequested(Jen,MusicStore,BritneyCD,1),time) ->
% Happens(CreateC(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time).
holds_at(cc(musicStore, jen, purchaseRequestedJenMusicStoreBritneyCD1, deliveredMusicStoreJenBritneyCD), Time), holds_at(purchaseRequested(jen, musicStore, britneyCD, 1), Time) ->
	happens(createC(musicStore, jen, deliveredMusicStoreJenBritneyCD),
		Time).


% 
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:2456
% [time]% 
% HoldsAt(CC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time) &
% HoldsAt(PurchaseRequested(Jen, MusicStore, BritneyCD, 1),time) ->
% Happens(DischargeCC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time).
holds_at(cc(musicStore, jen, purchaseRequestedJenMusicStoreBritneyCD1, deliveredMusicStoreJenBritneyCD), Time), holds_at(purchaseRequested(jen, musicStore, britneyCD, 1), Time) ->
	happens(dischargeCC(musicStore,
			    jen,
			    purchaseRequestedJenMusicStoreBritneyCD1,
			    deliveredMusicStoreJenBritneyCD),
		Time).


% 
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:2461
% [time]% 
% HoldsAt(CC(Jen, MusicStore, DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time) &
% HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
% Happens(CreateC(Jen,MusicStore,EPOSentJenMusicStore1),time).
holds_at(cc(jen, musicStore, deliveredMusicStoreJenBritneyCD, ePOSentJenMusicStore1), Time), holds_at(delivered(musicStore, jen, britneyCD), Time) ->
	happens(createC(jen, musicStore, ePOSentJenMusicStore1), Time).


% 
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:2466
% [time]% 
% HoldsAt(CC(Jen, MusicStore, DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time) &
% HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
% Happens(DischargeCC(Jen,MusicStore,DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time).
holds_at(cc(jen, musicStore, deliveredMusicStoreJenBritneyCD, ePOSentJenMusicStore1), Time), holds_at(delivered(musicStore, jen, britneyCD), Time) ->
	happens(dischargeCC(jen,
			    musicStore,
			    deliveredMusicStoreJenBritneyCD,
			    ePOSentJenMusicStore1),
		Time).


% 
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:2471
% [time]% 
% HoldsAt(C(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time) &
% HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
% Happens(DischargeC(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time).
holds_at(c(musicStore, jen, deliveredMusicStoreJenBritneyCD), Time), holds_at(delivered(musicStore, jen, britneyCD), Time) ->
	happens(dischargeC(musicStore, jen, deliveredMusicStoreJenBritneyCD),
		Time).


% 
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:2476
% [time]% 
% HoldsAt(C(Jen,MusicStore,EPOSentJenMusicStore1),time) &
% HoldsAt(EPOSent(Jen,MusicStore,1),time) ->
% Happens(DischargeC(Jen,MusicStore,EPOSentJenMusicStore1),time).
holds_at(c(jen, musicStore, ePOSentJenMusicStore1), Time), holds_at(ePOSent(jen, musicStore, 1), Time) ->
	happens(dischargeC(jen, musicStore, ePOSentJenMusicStore1), Time).


% 
% 
% Delta:
directive(delta).


 % Happens(SendQuote(MusicStore,Jen,BritneyCD,1),0).
happens(sendQuote(musicStore, jen, britneyCD, 1), 0).


% 
% ectest/ec_reader_test_examples.e:2482
% Delta:
directive(delta).


 % Happens(RequestPurchase(Jen,MusicStore,BritneyCD,1),1).
happens(requestPurchase(jen, musicStore, britneyCD, 1), 1).


% 
% Delta:
directive(delta).


 % Happens(Deliver(MusicStore,Jen,BritneyCD),3).
happens(deliver(musicStore, jen, britneyCD), 3).


% 
% Delta:
directive(delta).


 % Happens(SendEPO(Jen,MusicStore,1),5).
happens(sendEPO(jen, musicStore, 1), 5).


% 
% 
% ; Gamma
% 
% ectest/ec_reader_test_examples.e:2488
% [agent1,agent2,product,amount]% 
% !HoldsAt(QuoteSent(agent1,agent2,product,amount),0).
not(holds_at(quoteSent(Agent1, Agent2, Product, Amount), 0)).


% 
% 
% ectest/ec_reader_test_examples.e:2491
% [agent1,agent2,product,amount]% 
% !HoldsAt(PurchaseRequested(agent1,agent2,product,amount),0).
not(holds_at(purchaseRequested(Agent1, Agent2, Product, Amount), 0)).


% 
% 
% ectest/ec_reader_test_examples.e:2494
% [agent1,agent2,product]% 
% !HoldsAt(Delivered(agent1,agent2,product),0).
not(holds_at(delivered(Agent1, Agent2, Product), 0)).


% 
% 
% ectest/ec_reader_test_examples.e:2497
% [agent1,agent2,f]% 
% !HoldsAt(C(agent1,agent2,f),0).
not(holds_at(c(Agent1, Agent2, F), 0)).


% 
% 
% ectest/ec_reader_test_examples.e:2500
% [agent1,agent2,f1,f2]% 
% !HoldsAt(CC(agent1,agent2,f1,f2),0).
not(holds_at(cc(Agent1, Agent2, F1, F2), 0)).


% 
% 
% ectest/ec_reader_test_examples.e:2503
% [agent1,agent2,amount]% 
% !HoldsAt(EPOSent(agent1,agent2,amount),0).
not(holds_at(ePOSent(Agent1, Agent2, Amount), 0)).


% 
% 
% completion Delta Happens
completion([delta, happens]).


% 
% range time 0 7
range(time, 0, 7).


% ectest/ec_reader_test_examples.e:2509
% range offset 1 1
range(offset, 1, 1).


% range amount 1 1
range(amount, 1, 1).


% 
% ; End of file.
% 
% 
% ectest/ec_reader_test_examples.e:2515
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter14/NetBill3.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @inproceedings{SirbuTygar:1995,
% ;   author = "Marvin A. Sirbu and J. D. Tygar",
% ;   year = "1995",
% ;   title = "Net\uppercase{B}ill: An \uppercase{I}nternet commerce system optimized for network delivered services",
% ;   editor = "
% ;   booktitle = "40th \uppercase{IEEE} \uppercase{C}omputer \uppercase{S}ociety \uppercase{I}nternational \uppercase{C}onference",
% ;   pages = "20--25",
% ;   publisher = "
% ;   address = "
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
% ectest/ec_reader_test_examples.e:2548
% 
% option modeldiff on
option(modeldiff, on).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% ectest/ec_reader_test_examples.e:2554
% sort agent
sort(agent).


% agent MusicStore, Jen
t(agent, musicStore).


t(agent, jen).


% 
% sort product
sort(product).


% product BritneyCD
t(product, britneyCD).


% 
% ectest/ec_reader_test_examples.e:2560
% sort f
sort(f).


% f PurchaseRequestedJenMusicStoreBritneyCD1
t(f, purchaseRequestedJenMusicStoreBritneyCD1).


% f DeliveredMusicStoreJenBritneyCD
t(f, deliveredMusicStoreJenBritneyCD).


% f EPOSentJenMusicStore1
t(f, ePOSentJenMusicStore1).


% 
% sort amount: integer
subsort(amount, integer).


% ectest/ec_reader_test_examples.e:2566
% 
% fluent C(agent,agent,f)
fluent(c(agent, agent, f)).


% fluent CC(agent,agent,f,f)
fluent(cc(agent, agent, f, f)).


% 
% event CreateC(agent,agent,f)
event(createC(agent, agent, f)).


% event CreateCC(agent,agent,f,f)
event(createCC(agent, agent, f, f)).


% ectest/ec_reader_test_examples.e:2572
% event DischargeC(agent,agent,f)
event(dischargeC(agent, agent, f)).


% event DischargeCC(agent,agent,f,f)
event(dischargeCC(agent, agent, f, f)).


% 
% fluent QuoteSent(agent,agent,product,amount)
fluent(quoteSent(agent, agent, product, amount)).


% fluent PurchaseRequested(agent,agent,product,amount)
fluent(purchaseRequested(agent, agent, product, amount)).


% fluent Delivered(agent,agent,product)
fluent(delivered(agent, agent, product)).


% ectest/ec_reader_test_examples.e:2578
% fluent EPOSent(agent,agent,amount)
fluent(ePOSent(agent, agent, amount)).


% 
% event SendQuote(agent,agent,product,amount)
event(sendQuote(agent, agent, product, amount)).


% event RequestPurchase(agent,agent,product,amount)
event(requestPurchase(agent, agent, product, amount)).


% event Deliver(agent,agent,product)
event(deliver(agent, agent, product)).


% event SendEPO(agent,agent,amount)
event(sendEPO(agent, agent, amount)).


% ectest/ec_reader_test_examples.e:2584
% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:2587
% [agent1,agent2,f,time]% 
% Initiates(CreateC(agent1,agent2,f),C(agent1,agent2,f),time).
initiates(createC(Agent1, Agent2, F), c(Agent1, Agent2, F), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2590
% [agent1,agent2,f1,f2,time]% 
% Initiates(CreateCC(agent1,agent2,f1,f2),CC(agent1,agent2,f1,f2),time).
initiates(createCC(Agent1, Agent2, F1, F2), cc(Agent1, Agent2, F1, F2), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2593
% [agent1,agent2,f,time]% 
% Terminates(DischargeC(agent1,agent2,f),C(agent1,agent2,f),time).
terminates(dischargeC(Agent1, Agent2, F), c(Agent1, Agent2, F), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2596
% [agent1,agent2,f1,f2,time]% 
% Terminates(DischargeCC(agent1,agent2,f1,f2),CC(agent1,agent2,f1,f2),time).
terminates(dischargeCC(Agent1, Agent2, F1, F2), cc(Agent1, Agent2, F1, F2), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2599
% [agent1,agent2,product,amount,time]% 
% Initiates(SendQuote(agent1,agent2,product,amount),
%           QuoteSent(agent1,agent2,product,amount),
%           time).
initiates(sendQuote(Agent1, Agent2, Product, Amount), quoteSent(Agent1, Agent2, Product, Amount), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2604
% [agent1,agent2,product,amount,time]% 
% Initiates(RequestPurchase(agent1,agent2,product,amount),
%           PurchaseRequested(agent1,agent2,product,amount),
%           time).
initiates(requestPurchase(Agent1, Agent2, Product, Amount), purchaseRequested(Agent1, Agent2, Product, Amount), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2609
% [agent1,agent2,product,time]% 
% Initiates(Deliver(agent1,agent2,product),
%           Delivered(agent1,agent2,product),
%           time).
initiates(deliver(Agent1, Agent2, Product), delivered(Agent1, Agent2, Product), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2614
% [agent1,agent2,amount,time]% 
% Initiates(SendEPO(agent1,agent2,amount),
%           EPOSent(agent1,agent2,amount),
%           time).
initiates(sendEPO(Agent1, Agent2, Amount), ePOSent(Agent1, Agent2, Amount), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2619
% [agent1,agent2,product,amount,f1,f2,time]% 
% agent1=% MusicStore &
% agent2=Jen &
% product=BritneyCD &
% amount=1 &
% f1=PurchaseRequestedJenMusicStoreBritneyCD1 &
% f2=DeliveredMusicStoreJenBritneyCD ->
% Initiates(SendQuote(agent1,agent2,product,amount),
%           CC(agent1,agent2,f1,f2),
%           time).
Agent1=musicStore, Agent2=jen, Product=britneyCD, Amount=1, F1=purchaseRequestedJenMusicStoreBritneyCD1, F2=deliveredMusicStoreJenBritneyCD ->
	initiates(sendQuote(Agent1,
			    Agent2,
			    Product,
			    Amount),
		  cc(Agent1, Agent2, F1, F2),
		  Time).


% ectest/ec_reader_test_examples.e:2628
% 
% 
% ectest/ec_reader_test_examples.e:2630
% [agent1,agent2,product,amount,f1,f2,time]% 
% agent1=% Jen &
% agent2=MusicStore &
% product=BritneyCD &
% amount=1 &
% f1=DeliveredMusicStoreJenBritneyCD &
% f2=EPOSentJenMusicStore1 &
% !HoldsAt(Delivered(agent2,agent1,product),time) ->
% Initiates(RequestPurchase(agent1,agent2,product,amount),
%           CC(agent1,agent2,f1,f2),
%           time).
Agent1=jen, Agent2=musicStore, Product=britneyCD, Amount=1, F1=deliveredMusicStoreJenBritneyCD, F2=ePOSentJenMusicStore1, not(holds_at(delivered(Agent2, Agent1, Product), Time)) ->
	initiates(requestPurchase(Agent1,
				  Agent2,
				  Product,
				  Amount),
		  cc(Agent1, Agent2, F1, F2),
		  Time).


% ectest/ec_reader_test_examples.e:2640
% 
% 
% ; Delta
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:2644
% [time]% 
% HoldsAt(CC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time) &
% HoldsAt(PurchaseRequested(Jen,MusicStore,BritneyCD,1),time) ->
% Happens(CreateC(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time).
holds_at(cc(musicStore, jen, purchaseRequestedJenMusicStoreBritneyCD1, deliveredMusicStoreJenBritneyCD), Time), holds_at(purchaseRequested(jen, musicStore, britneyCD, 1), Time) ->
	happens(createC(musicStore, jen, deliveredMusicStoreJenBritneyCD),
		Time).


% 
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:2649
% [time]% 
% HoldsAt(CC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time) &
% HoldsAt(PurchaseRequested(Jen, MusicStore, BritneyCD, 1),time) ->
% Happens(DischargeCC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time).
holds_at(cc(musicStore, jen, purchaseRequestedJenMusicStoreBritneyCD1, deliveredMusicStoreJenBritneyCD), Time), holds_at(purchaseRequested(jen, musicStore, britneyCD, 1), Time) ->
	happens(dischargeCC(musicStore,
			    jen,
			    purchaseRequestedJenMusicStoreBritneyCD1,
			    deliveredMusicStoreJenBritneyCD),
		Time).


% 
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:2654
% [time]% 
% HoldsAt(CC(Jen, MusicStore, DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time) &
% HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
% Happens(CreateC(Jen,MusicStore,EPOSentJenMusicStore1),time).
holds_at(cc(jen, musicStore, deliveredMusicStoreJenBritneyCD, ePOSentJenMusicStore1), Time), holds_at(delivered(musicStore, jen, britneyCD), Time) ->
	happens(createC(jen, musicStore, ePOSentJenMusicStore1), Time).


% 
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:2659
% [time]% 
% HoldsAt(CC(Jen, MusicStore, DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time) &
% HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
% Happens(DischargeCC(Jen,MusicStore,DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time).
holds_at(cc(jen, musicStore, deliveredMusicStoreJenBritneyCD, ePOSentJenMusicStore1), Time), holds_at(delivered(musicStore, jen, britneyCD), Time) ->
	happens(dischargeCC(jen,
			    musicStore,
			    deliveredMusicStoreJenBritneyCD,
			    ePOSentJenMusicStore1),
		Time).


% 
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:2664
% [time]% 
% HoldsAt(C(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time) &
% HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
% Happens(DischargeC(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time).
holds_at(c(musicStore, jen, deliveredMusicStoreJenBritneyCD), Time), holds_at(delivered(musicStore, jen, britneyCD), Time) ->
	happens(dischargeC(musicStore, jen, deliveredMusicStoreJenBritneyCD),
		Time).


% 
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:2669
% [time]% 
% HoldsAt(C(Jen,MusicStore,EPOSentJenMusicStore1),time) &
% HoldsAt(EPOSent(Jen,MusicStore,1),time) ->
% Happens(DischargeC(Jen,MusicStore,EPOSentJenMusicStore1),time).
holds_at(c(jen, musicStore, ePOSentJenMusicStore1), Time), holds_at(ePOSent(jen, musicStore, 1), Time) ->
	happens(dischargeC(jen, musicStore, ePOSentJenMusicStore1), Time).


% 
% 
% Delta:
directive(delta).


 % Happens(Deliver(MusicStore,Jen,BritneyCD),0).
happens(deliver(musicStore, jen, britneyCD), 0).


% 
% ectest/ec_reader_test_examples.e:2675
% Delta:
directive(delta).


 % Happens(SendEPO(Jen,MusicStore,1),2).
happens(sendEPO(jen, musicStore, 1), 2).


% 
% 
% ; Gamma
% 
% ectest/ec_reader_test_examples.e:2679
% [agent1,agent2,product,amount]% 
% !HoldsAt(QuoteSent(agent1,agent2,product,amount),0).
not(holds_at(quoteSent(Agent1, Agent2, Product, Amount), 0)).


% 
% 
% ectest/ec_reader_test_examples.e:2682
% [agent1,agent2,product,amount]% 
% !HoldsAt(PurchaseRequested(agent1,agent2,product,amount),0).
not(holds_at(purchaseRequested(Agent1, Agent2, Product, Amount), 0)).


% 
% 
% ectest/ec_reader_test_examples.e:2685
% [agent1,agent2,product]% 
% !HoldsAt(Delivered(agent1,agent2,product),0).
not(holds_at(delivered(Agent1, Agent2, Product), 0)).


% 
% 
% ectest/ec_reader_test_examples.e:2688
% [agent1,agent2,f]% 
% !HoldsAt(C(agent1,agent2,f),0).
not(holds_at(c(Agent1, Agent2, F), 0)).


% 
% 
% ectest/ec_reader_test_examples.e:2691
% [agent1,agent2,f1,f2]% 
% !HoldsAt(CC(agent1,agent2,f1,f2),0).
not(holds_at(cc(Agent1, Agent2, F1, F2), 0)).


% 
% 
% ectest/ec_reader_test_examples.e:2694
% [agent1,agent2,amount]% 
% !HoldsAt(EPOSent(agent1,agent2,amount),0).
not(holds_at(ePOSent(Agent1, Agent2, Amount), 0)).


% 
% 
% completion Delta Happens
completion([delta, happens]).


% 
% range time 0 4
range(time, 0, 4).


% ectest/ec_reader_test_examples.e:2700
% range offset 1 1
range(offset, 1, 1).


% range amount 1 1
range(amount, 1, 1).


% 
% ; End of file.
% 
% 
% ectest/ec_reader_test_examples.e:2706
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter14/NetBill2.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @inproceedings{SirbuTygar:1995,
% ;   author = "Marvin A. Sirbu and J. D. Tygar",
% ;   year = "1995",
% ;   title = "Net\uppercase{B}ill: An \uppercase{I}nternet commerce system optimized for network delivered services",
% ;   editor = "
% ;   booktitle = "40th \uppercase{IEEE} \uppercase{C}omputer \uppercase{S}ociety \uppercase{I}nternational \uppercase{C}onference",
% ;   pages = "20--25",
% ;   publisher = "
% ;   address = "
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
% ectest/ec_reader_test_examples.e:2739
% 
% option modeldiff on
option(modeldiff, on).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% ectest/ec_reader_test_examples.e:2745
% sort agent
sort(agent).


% agent MusicStore, Jen
t(agent, musicStore).


t(agent, jen).


% 
% sort product
sort(product).


% product BritneyCD
t(product, britneyCD).


% 
% ectest/ec_reader_test_examples.e:2751
% sort f
sort(f).


% f PurchaseRequestedJenMusicStoreBritneyCD1
t(f, purchaseRequestedJenMusicStoreBritneyCD1).


% f DeliveredMusicStoreJenBritneyCD
t(f, deliveredMusicStoreJenBritneyCD).


% f EPOSentJenMusicStore1
t(f, ePOSentJenMusicStore1).


% 
% sort amount: integer
subsort(amount, integer).


% ectest/ec_reader_test_examples.e:2757
% 
% fluent C(agent,agent,f)
fluent(c(agent, agent, f)).


% fluent CC(agent,agent,f,f)
fluent(cc(agent, agent, f, f)).


% 
% event CreateC(agent,agent,f)
event(createC(agent, agent, f)).


% event CreateCC(agent,agent,f,f)
event(createCC(agent, agent, f, f)).


% ectest/ec_reader_test_examples.e:2763
% event DischargeC(agent,agent,f)
event(dischargeC(agent, agent, f)).


% event DischargeCC(agent,agent,f,f)
event(dischargeCC(agent, agent, f, f)).


% 
% fluent QuoteSent(agent,agent,product,amount)
fluent(quoteSent(agent, agent, product, amount)).


% fluent PurchaseRequested(agent,agent,product,amount)
fluent(purchaseRequested(agent, agent, product, amount)).


% fluent Delivered(agent,agent,product)
fluent(delivered(agent, agent, product)).


% ectest/ec_reader_test_examples.e:2769
% fluent EPOSent(agent,agent,amount)
fluent(ePOSent(agent, agent, amount)).


% 
% event SendQuote(agent,agent,product,amount)
event(sendQuote(agent, agent, product, amount)).


% event RequestPurchase(agent,agent,product,amount)
event(requestPurchase(agent, agent, product, amount)).


% event Deliver(agent,agent,product)
event(deliver(agent, agent, product)).


% event SendEPO(agent,agent,amount)
event(sendEPO(agent, agent, amount)).


% ectest/ec_reader_test_examples.e:2775
% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:2778
% [agent1,agent2,f,time]% 
% Initiates(CreateC(agent1,agent2,f),C(agent1,agent2,f),time).
initiates(createC(Agent1, Agent2, F), c(Agent1, Agent2, F), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2781
% [agent1,agent2,f1,f2,time]% 
% Initiates(CreateCC(agent1,agent2,f1,f2),CC(agent1,agent2,f1,f2),time).
initiates(createCC(Agent1, Agent2, F1, F2), cc(Agent1, Agent2, F1, F2), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2784
% [agent1,agent2,f,time]% 
% Terminates(DischargeC(agent1,agent2,f),C(agent1,agent2,f),time).
terminates(dischargeC(Agent1, Agent2, F), c(Agent1, Agent2, F), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2787
% [agent1,agent2,f1,f2,time]% 
% Terminates(DischargeCC(agent1,agent2,f1,f2),CC(agent1,agent2,f1,f2),time).
terminates(dischargeCC(Agent1, Agent2, F1, F2), cc(Agent1, Agent2, F1, F2), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2790
% [agent1,agent2,product,amount,time]% 
% Initiates(SendQuote(agent1,agent2,product,amount),
%           QuoteSent(agent1,agent2,product,amount),
%           time).
initiates(sendQuote(Agent1, Agent2, Product, Amount), quoteSent(Agent1, Agent2, Product, Amount), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2795
% [agent1,agent2,product,amount,time]% 
% Initiates(RequestPurchase(agent1,agent2,product,amount),
%           PurchaseRequested(agent1,agent2,product,amount),
%           time).
initiates(requestPurchase(Agent1, Agent2, Product, Amount), purchaseRequested(Agent1, Agent2, Product, Amount), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2800
% [agent1,agent2,product,time]% 
% Initiates(Deliver(agent1,agent2,product),
%           Delivered(agent1,agent2,product),
%           time).
initiates(deliver(Agent1, Agent2, Product), delivered(Agent1, Agent2, Product), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2805
% [agent1,agent2,amount,time]% 
% Initiates(SendEPO(agent1,agent2,amount),
%           EPOSent(agent1,agent2,amount),
%           time).
initiates(sendEPO(Agent1, Agent2, Amount), ePOSent(Agent1, Agent2, Amount), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2810
% [agent1,agent2,product,amount,f1,f2,time]% 
% agent1=% MusicStore &
% agent2=Jen &
% product=BritneyCD &
% amount=1 &
% f1=PurchaseRequestedJenMusicStoreBritneyCD1 &
% f2=DeliveredMusicStoreJenBritneyCD ->
% Initiates(SendQuote(agent1,agent2,product,amount),
%           CC(agent1,agent2,f1,f2),
%           time).
Agent1=musicStore, Agent2=jen, Product=britneyCD, Amount=1, F1=purchaseRequestedJenMusicStoreBritneyCD1, F2=deliveredMusicStoreJenBritneyCD ->
	initiates(sendQuote(Agent1,
			    Agent2,
			    Product,
			    Amount),
		  cc(Agent1, Agent2, F1, F2),
		  Time).


% ectest/ec_reader_test_examples.e:2819
% 
% 
% ectest/ec_reader_test_examples.e:2821
% [agent1,agent2,product,amount,f1,f2,time]% 
% agent1=% Jen &
% agent2=MusicStore &
% product=BritneyCD &
% amount=1 &
% f1=DeliveredMusicStoreJenBritneyCD &
% f2=EPOSentJenMusicStore1 &
% !HoldsAt(Delivered(agent2,agent1,product),time) ->
% Initiates(RequestPurchase(agent1,agent2,product,amount),
%           CC(agent1,agent2,f1,f2),
%           time).
Agent1=jen, Agent2=musicStore, Product=britneyCD, Amount=1, F1=deliveredMusicStoreJenBritneyCD, F2=ePOSentJenMusicStore1, not(holds_at(delivered(Agent2, Agent1, Product), Time)) ->
	initiates(requestPurchase(Agent1,
				  Agent2,
				  Product,
				  Amount),
		  cc(Agent1, Agent2, F1, F2),
		  Time).


% ectest/ec_reader_test_examples.e:2831
% 
% 
% ; Delta
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:2835
% [time]% 
% HoldsAt(CC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time) &
% HoldsAt(PurchaseRequested(Jen,MusicStore,BritneyCD,1),time) ->
% Happens(CreateC(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time).
holds_at(cc(musicStore, jen, purchaseRequestedJenMusicStoreBritneyCD1, deliveredMusicStoreJenBritneyCD), Time), holds_at(purchaseRequested(jen, musicStore, britneyCD, 1), Time) ->
	happens(createC(musicStore, jen, deliveredMusicStoreJenBritneyCD),
		Time).


% 
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:2840
% [time]% 
% HoldsAt(CC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time) &
% HoldsAt(PurchaseRequested(Jen, MusicStore, BritneyCD, 1),time) ->
% Happens(DischargeCC(MusicStore,Jen,PurchaseRequestedJenMusicStoreBritneyCD1,DeliveredMusicStoreJenBritneyCD),time).
holds_at(cc(musicStore, jen, purchaseRequestedJenMusicStoreBritneyCD1, deliveredMusicStoreJenBritneyCD), Time), holds_at(purchaseRequested(jen, musicStore, britneyCD, 1), Time) ->
	happens(dischargeCC(musicStore,
			    jen,
			    purchaseRequestedJenMusicStoreBritneyCD1,
			    deliveredMusicStoreJenBritneyCD),
		Time).


% 
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:2845
% [time]% 
% HoldsAt(CC(Jen, MusicStore, DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time) &
% HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
% Happens(CreateC(Jen,MusicStore,EPOSentJenMusicStore1),time).
holds_at(cc(jen, musicStore, deliveredMusicStoreJenBritneyCD, ePOSentJenMusicStore1), Time), holds_at(delivered(musicStore, jen, britneyCD), Time) ->
	happens(createC(jen, musicStore, ePOSentJenMusicStore1), Time).


% 
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:2850
% [time]% 
% HoldsAt(CC(Jen, MusicStore, DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time) &
% HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
% Happens(DischargeCC(Jen,MusicStore,DeliveredMusicStoreJenBritneyCD, EPOSentJenMusicStore1),time).
holds_at(cc(jen, musicStore, deliveredMusicStoreJenBritneyCD, ePOSentJenMusicStore1), Time), holds_at(delivered(musicStore, jen, britneyCD), Time) ->
	happens(dischargeCC(jen,
			    musicStore,
			    deliveredMusicStoreJenBritneyCD,
			    ePOSentJenMusicStore1),
		Time).


% 
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:2855
% [time]% 
% HoldsAt(C(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time) &
% HoldsAt(Delivered(MusicStore,Jen,BritneyCD),time) ->
% Happens(DischargeC(MusicStore,Jen,DeliveredMusicStoreJenBritneyCD),time).
holds_at(c(musicStore, jen, deliveredMusicStoreJenBritneyCD), Time), holds_at(delivered(musicStore, jen, britneyCD), Time) ->
	happens(dischargeC(musicStore, jen, deliveredMusicStoreJenBritneyCD),
		Time).


% 
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:2860
% [time]% 
% HoldsAt(C(Jen,MusicStore,EPOSentJenMusicStore1),time) &
% HoldsAt(EPOSent(Jen,MusicStore,1),time) ->
% Happens(DischargeC(Jen,MusicStore,EPOSentJenMusicStore1),time).
holds_at(c(jen, musicStore, ePOSentJenMusicStore1), Time), holds_at(ePOSent(jen, musicStore, 1), Time) ->
	happens(dischargeC(jen, musicStore, ePOSentJenMusicStore1), Time).


% 
% 
% Delta:
directive(delta).


 % Happens(RequestPurchase(Jen,MusicStore,BritneyCD,1),0).
happens(requestPurchase(jen, musicStore, britneyCD, 1), 0).


% 
% ectest/ec_reader_test_examples.e:2866
% Delta:
directive(delta).


 % Happens(Deliver(MusicStore,Jen,BritneyCD),2).
happens(deliver(musicStore, jen, britneyCD), 2).


% 
% Delta:
directive(delta).


 % Happens(SendEPO(Jen,MusicStore,1),4).
happens(sendEPO(jen, musicStore, 1), 4).


% 
% 
% ; Gamma
% 
% ectest/ec_reader_test_examples.e:2871
% [agent1,agent2,product,amount]% 
% !HoldsAt(QuoteSent(agent1,agent2,product,amount),0).
not(holds_at(quoteSent(Agent1, Agent2, Product, Amount), 0)).


% 
% 
% ectest/ec_reader_test_examples.e:2874
% [agent1,agent2,product,amount]% 
% !HoldsAt(PurchaseRequested(agent1,agent2,product,amount),0).
not(holds_at(purchaseRequested(Agent1, Agent2, Product, Amount), 0)).


% 
% 
% ectest/ec_reader_test_examples.e:2877
% [agent1,agent2,product]% 
% !HoldsAt(Delivered(agent1,agent2,product),0).
not(holds_at(delivered(Agent1, Agent2, Product), 0)).


% 
% 
% ectest/ec_reader_test_examples.e:2880
% [agent1,agent2,f]% 
% !HoldsAt(C(agent1,agent2,f),0).
not(holds_at(c(Agent1, Agent2, F), 0)).


% 
% 
% ectest/ec_reader_test_examples.e:2883
% [agent1,agent2,f1,f2]% 
% !HoldsAt(CC(agent1,agent2,f1,f2),0).
not(holds_at(cc(Agent1, Agent2, F1, F2), 0)).


% 
% 
% ectest/ec_reader_test_examples.e:2886
% [agent1,agent2,amount]% 
% !HoldsAt(EPOSent(agent1,agent2,amount),0).
not(holds_at(ePOSent(Agent1, Agent2, Amount), 0)).


% 
% 
% completion Delta Happens
completion([delta, happens]).


% 
% range time 0 6
range(time, 0, 6).


% ectest/ec_reader_test_examples.e:2892
% range offset 1 1
range(offset, 1, 1).


% range amount 1 1
range(amount, 1, 1).


% 
% ; End of file.
% 
% 
% ectest/ec_reader_test_examples.e:2898
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter14/Vision.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @inproceedings{ShanahanRandell:2004,
% ;   author = "Murray Shanahan and David A. Randell",
% ;   year = "2004",
% ;   title = "A logic-based formulation of active visual perception",
% ;   editor = "Didier Dubois and Christopher A. Welty and Mary-Anne Williams",
% ;   booktitle = "\uppercase{P}roceedings of the \uppercase{N}inth \uppercase{I}nternational \uppercase{C}onference on \uppercase{P}rinciples of \uppercase{K}nowledge \uppercase{R}epresentation and \uppercase{R}easoning",
% ;   pages = "64--72",
% ;   address = "Menlo Park, CA",
% ;   publisher = "AAAI Press",
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
% ectest/ec_reader_test_examples.e:2931
% 
% option modeldiff on
option(modeldiff, on).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% ectest/ec_reader_test_examples.e:2937
% sort object
sort(object).


% sort shape
sort(shape).


% sort aspect
sort(aspect).


% 
% object Object1
t(object, object1).


% aspect Aspect1, Aspect2, Aspect3
t(aspect, aspect1).


t(aspect, aspect2).


t(aspect, aspect3).


% ectest/ec_reader_test_examples.e:2943
% shape Shape1, Shape2
t(shape, shape1).


t(shape, shape2).


% 
% predicate Shape(object,shape)
predicate(shape(object, shape)).


% predicate Arc(shape,aspect,aspect)
predicate(arc(shape, aspect, aspect)).


% fluent Aspect(object,aspect)
fluent(aspect(object, aspect)).


% event Change(object,aspect,aspect)
event(change(object, aspect, aspect)).


% ectest/ec_reader_test_examples.e:2949
% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:2952
% [object,aspect1,aspect2,shape,time]% 
% HoldsAt(Aspect(object,aspect1),time) &
% Shape(object,shape) &
% (Arc(shape,aspect1,aspect2) |
%  Arc(shape,aspect2,aspect1)) ->
% Initiates(Change(object,aspect1,aspect2),Aspect(object,aspect2),time).
holds_at(aspect(Object, Aspect1), Time), shape(Object, Shape), (arc(Shape, Aspect1, Aspect2);arc(Shape, Aspect2, Aspect1)) ->
	initiates(change(Object, Aspect1, Aspect2),
		  aspect(Object, Aspect2),
		  Time).


% 
% ectest/ec_reader_test_examples.e:2958
% 
% ectest/ec_reader_test_examples.e:2959
% [object,aspect1,aspect2,shape,time]% 
% HoldsAt(Aspect(object,aspect1),time) &
% Shape(object,shape) &
% (Arc(shape,aspect1,aspect2) |
%  Arc(shape,aspect2,aspect1)) ->
% Terminates(Change(object,aspect1,aspect2),Aspect(object,aspect1),time).
holds_at(aspect(Object, Aspect1), Time), shape(Object, Shape), (arc(Shape, Aspect1, Aspect2);arc(Shape, Aspect2, Aspect1)) ->
	terminates(change(Object, Aspect1, Aspect2),
		   aspect(Object, Aspect1),
		   Time).


% 
% ectest/ec_reader_test_examples.e:2965
% 
% ; preconditions (added)
% 
% ectest/ec_reader_test_examples.e:2968
% [object,aspect1,aspect2,time]% 
% Happens(Change(object,aspect1,aspect2),time) ->
% HoldsAt(Aspect(object,aspect1),time).
happens(change(Object, Aspect1, Aspect2), Time) ->
	holds_at(aspect(Object, Aspect1), Time).


% 
% 
% ectest/ec_reader_test_examples.e:2972
% [object,aspect1,aspect2,aspect3,time]% 
% Happens(Change(object,aspect1,aspect2),time) &
% Happens(Change(object,aspect1,aspect3),time) ->
% aspect2=aspect3.
happens(change(Object, Aspect1, Aspect2), Time), happens(change(Object, Aspect1, Aspect3), Time) ->
	Aspect2=Aspect3.


% 
% 
% ; Psi
% ectest/ec_reader_test_examples.e:2978
% 
% ectest/ec_reader_test_examples.e:2979
% [object,shape1,shape2]% 
% Shape(object,shape1) &
% Shape(object,shape2) ->
% shape1=shape2.
shape(Object, Shape1), shape(Object, Shape2) ->
	Shape1=Shape2.


% 
% 
% ectest/ec_reader_test_examples.e:2984
% [object,aspect1,aspect2,time]% 
% HoldsAt(Aspect(object,aspect1),time) &
% HoldsAt(Aspect(object,aspect2),time) ->
% aspect1=aspect2.
holds_at(aspect(Object, Aspect1), Time), holds_at(aspect(Object, Aspect2), Time) ->
	Aspect1=Aspect2.


% 
% 
% ectest/ec_reader_test_examples.e:2989
% [aspect1,aspect2]% 
% Arc(Shape1,aspect1,aspect2) <->
% (aspect1=Aspect1 & aspect2=Aspect2).
arc(shape1, Aspect1, Aspect2) <->
	Aspect1=aspect1,
	Aspect2=aspect2.


% 
% 
% ectest/ec_reader_test_examples.e:2993
% [aspect1,aspect2]% 
% Arc(Shape2,aspect1,aspect2) <->
% ((aspect1=Aspect1 & aspect2=Aspect3) |
%  (aspect1=Aspect3 & aspect2=Aspect2)).
arc(shape2, Aspect1, Aspect2) <->
	(   Aspect1=aspect1,
	    Aspect2=aspect3
	;   Aspect1=aspect3,
	    Aspect2=aspect2
	).


% 
% 
% ; Gamma
% ectest/ec_reader_test_examples.e:2999
% 
% HoldsAt(Aspect(Object1,Aspect1),0).
holds_at(aspect(object1, aspect1), 0).


% 
% HoldsAt(Aspect(Object1,Aspect2),1).
holds_at(aspect(object1, aspect2), 1).


% 
% 
% ;completion Delta Happens
% 
% ectest/ec_reader_test_examples.e:3005
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% 
% ectest/ec_reader_test_examples.e:3011
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter14/Workflow.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @incollection{CicekliYildirim:2000,
% ;   author = "Nihan Kesim Cicekli and Yakup Yildirim",
% ;   year = "2000",
% ;   title = "Formalizing workflows using the event calculus",
% ;   editor = "Mohamed T. Ibrahim and Josef K{\"{u}}ng and Norman Revell",
% ;   booktitle = "Database and Expert Systems Applications",
% ;   series = "Lecture Notes in Computer Science",
% ;   volume = "1873",
% ;   pages = "222--231",
% ;   address = "Berlin",
% ;   publisher = "Springer",
% ; }
% ;
% ; @unpublished{WFMC:1999,
% ;   author = "{Workflow Management Coalition}",
% ;   year = "1999",
% ;   title = "\uppercase{W}orkflow \uppercase{M}anagement \uppercase{C}oalition Terminology \& Glossary",
% ;   howpublished = "Document Number WFMC-TC-1011, Document Status -- Issue 3.0, Workflow Management Coalition, Winchester, UK",
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
% ectest/ec_reader_test_examples.e:3053
% 
% option modeldiff on
option(modeldiff, on).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% ectest/ec_reader_test_examples.e:3059
% sort activity
sort(activity).


% sort condition
sort(condition).


% activity A, B, C1, C2, C3, D, E1, E2, E3, F, G
t(activity, a).


t(activity, b).


t(activity, c1).


t(activity, c2).


t(activity, c3).


t(activity, d).


t(activity, e1).


t(activity, e2).


t(activity, e3).


t(activity, f).


t(activity, g).


% condition E1C, E2C, E3C, FC
t(condition, e1c).


t(condition, e2c).


t(condition, e3c).


t(condition, fc).


% 
% fluent Active(activity)
fluent(active(activity)).


% ectest/ec_reader_test_examples.e:3065
% fluent Completed(activity)
fluent(completed(activity)).


% fluent Condition(condition)
fluent(condition(condition)).


% noninertial Condition
noninertial(condition).


% 
% event Start(activity)
event(start(activity)).


% event End(activity)
event(end(activity)).


% ectest/ec_reader_test_examples.e:3071
% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:3074
% [activity,time]% 
% Initiates(Start(activity),Active(activity),time).
initiates(start(Activity), active(Activity), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3077
% [activity,time]% 
% Terminates(Start(activity),Completed(activity),time).
terminates(start(Activity), completed(Activity), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3080
% [activity,time]% 
% Initiates(End(activity),Completed(activity),time).
initiates(end(Activity), completed(Activity), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3083
% [activity,time]% 
% Terminates(End(activity),Active(activity),time).
terminates(end(Activity), active(Activity), Time).


% 
% 
% ; Delta
% 
% ; A; B
% ectest/ec_reader_test_examples.e:3089
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:3089
% [time]% 
% !HoldsAt(Active(B),time) &
% !HoldsAt(Completed(A),time-1) &
% HoldsAt(Completed(A),time) ->
% Happens(Start(B),time).
not(holds_at(active(b), Time)), not(holds_at(completed(a), Time-1)), holds_at(completed(a), Time) ->
	happens(start(b), Time).


% 
% 
% ; B; AND-split C1, C2, C3
% ectest/ec_reader_test_examples.e:3096
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:3096
% [time]% 
% !HoldsAt(Active(C1),time) &
% !HoldsAt(Completed(B),time-1) &
% HoldsAt(Completed(B),time) ->
% Happens(Start(C1),time).
not(holds_at(active(c1), Time)), not(holds_at(completed(b), Time-1)), holds_at(completed(b), Time) ->
	happens(start(c1), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3102
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:3102
% [time]% 
% !HoldsAt(Active(C2),time) &
% !HoldsAt(Completed(B),time-1) &
% HoldsAt(Completed(B),time) ->
% Happens(Start(C2),time).
not(holds_at(active(c2), Time)), not(holds_at(completed(b), Time-1)), holds_at(completed(b), Time) ->
	happens(start(c2), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3108
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:3108
% [time]% 
% !HoldsAt(Active(C3),time) &
% !HoldsAt(Completed(B),time-1) &
% HoldsAt(Completed(B),time) ->
% Happens(Start(C3),time).
not(holds_at(active(c3), Time)), not(holds_at(completed(b), Time-1)), holds_at(completed(b), Time) ->
	happens(start(c3), Time).


% 
% 
% ; AND-join C1, C2, C3; D
% ectest/ec_reader_test_examples.e:3115
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:3115
% [time]% 
% !HoldsAt(Active(D),time) &
% ((!HoldsAt(Completed(C1),time-1) & HoldsAt(Completed(C1),time))|
%  (!HoldsAt(Completed(C2),time-1) & HoldsAt(Completed(C2),time))|
%  (!HoldsAt(Completed(C3),time-1) & HoldsAt(Completed(C3),time))) &
% HoldsAt(Completed(C1),time) &
% HoldsAt(Completed(C2),time) &
% HoldsAt(Completed(C3),time) ->
% Happens(Start(D),time).
not(holds_at(active(d), Time)), (not(holds_at(completed(c1), Time-1)), holds_at(completed(c1), Time);not(holds_at(completed(c2), Time-1)), holds_at(completed(c2), Time);not(holds_at(completed(c3), Time-1)), holds_at(completed(c3), Time)), holds_at(completed(c1), Time), holds_at(completed(c2), Time), holds_at(completed(c3), Time) ->
	happens(start(d), Time).


% ectest/ec_reader_test_examples.e:3123
% 
% 
% ; D; XOR-split E1, E2, E3
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:3126
% [time]% 
% !HoldsAt(Active(E1),time) &
% !HoldsAt(Completed(D),time-1) &
% HoldsAt(Completed(D),time) &
% HoldsAt(Condition(E1C),time) ->
% Happens(Start(E1),time).
not(holds_at(active(e1), Time)), not(holds_at(completed(d), Time-1)), holds_at(completed(d), Time), holds_at(condition(e1c), Time) ->
	happens(start(e1), Time).


% 
% ectest/ec_reader_test_examples.e:3132
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:3133
% [time]% 
% !HoldsAt(Active(E2),time) &
% !HoldsAt(Completed(D),time-1) &
% HoldsAt(Completed(D),time) &
% HoldsAt(Condition(E2C),time) ->
% Happens(Start(E2),time).
not(holds_at(active(e2), Time)), not(holds_at(completed(d), Time-1)), holds_at(completed(d), Time), holds_at(condition(e2c), Time) ->
	happens(start(e2), Time).


% 
% ectest/ec_reader_test_examples.e:3139
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:3140
% [time]% 
% !HoldsAt(Active(E3),time) &
% !HoldsAt(Completed(D),time-1) &
% HoldsAt(Completed(D),time) &
% HoldsAt(Condition(E3C),time) ->
% Happens(Start(E3),time).
not(holds_at(active(e3), Time)), not(holds_at(completed(d), Time-1)), holds_at(completed(d), Time), holds_at(condition(e3c), Time) ->
	happens(start(e3), Time).


% 
% ectest/ec_reader_test_examples.e:3146
% 
% ; XOR-join E1, E2, E3; F
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:3148
% [time]% 
% !HoldsAt(Active(F),time) &
% ((!HoldsAt(Completed(E1),time-1) & HoldsAt(Completed(E1),time))|
%  (!HoldsAt(Completed(E2),time-1) & HoldsAt(Completed(E2),time))|
%  (!HoldsAt(Completed(E3),time-1) & HoldsAt(Completed(E3),time))) ->
% Happens(Start(F),time).
not(holds_at(active(f), Time)), (not(holds_at(completed(e1), Time-1)), holds_at(completed(e1), Time);not(holds_at(completed(e2), Time-1)), holds_at(completed(e2), Time);not(holds_at(completed(e3), Time-1)), holds_at(completed(e3), Time)) ->
	happens(start(f), Time).


% 
% ectest/ec_reader_test_examples.e:3154
% 
% ; while (FC) F; G
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:3156
% [time]% 
% !HoldsAt(Active(F),time) &
% !HoldsAt(Completed(F),time-1) &
% HoldsAt(Completed(F),time) &
% HoldsAt(Condition(FC),time) ->
% Happens(Start(F),time).
not(holds_at(active(f), Time)), not(holds_at(completed(f), Time-1)), holds_at(completed(f), Time), holds_at(condition(fc), Time) ->
	happens(start(f), Time).


% 
% ectest/ec_reader_test_examples.e:3162
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:3163
% [time]% 
% !HoldsAt(Active(G),time) &
% !HoldsAt(Completed(F),time-1) &
% HoldsAt(Completed(F),time) &
% !HoldsAt(Condition(FC),time) ->
% Happens(Start(G),time).
not(holds_at(active(g), Time)), not(holds_at(completed(f), Time-1)), holds_at(completed(f), Time), not(holds_at(condition(fc), Time)) ->
	happens(start(g), Time).


% 
% ectest/ec_reader_test_examples.e:3169
% 
% Delta:
directive(delta).


 % Happens(Start(A),0).
happens(start(a), 0).


% 
% Delta:
directive(delta).


 % Happens(End(A),1).
happens(end(a), 1).


% 
% Delta:
directive(delta).


 % Happens(End(B),3).
happens(end(b), 3).


% 
% Delta:
directive(delta).


 % Happens(End(C1),5).
happens(end(c1), 5).


% 
% Delta:
directive(delta).


 % Happens(End(C2),6).
happens(end(c2), 6).


% 
% ectest/ec_reader_test_examples.e:3175
% Delta:
directive(delta).


 % Happens(End(C3),7).
happens(end(c3), 7).


% 
% Delta:
directive(delta).


 % Happens(End(D),9).
happens(end(d), 9).


% 
% Delta:
directive(delta).


 % Happens(End(E2),11).
happens(end(e2), 11).


% 
% Delta:
directive(delta).


 % Happens(End(F),13).
happens(end(f), 13).


% 
% Delta:
directive(delta).


 % Happens(End(F),15).
happens(end(f), 15).


% 
% 
% ; Gamma
% ectest/ec_reader_test_examples.e:3182
% 
% ectest/ec_reader_test_examples.e:3183
% [activity] % !HoldsAt(Active(activity),0).
not(holds_at(active(Activity), 0)).


% 
% ectest/ec_reader_test_examples.e:3184
% [activity] % !HoldsAt(Completed(activity),0).
not(holds_at(completed(Activity), 0)).


% 
% ectest/ec_reader_test_examples.e:3185
% [time] % time=% 14 <-> HoldsAt(Condition(FC),time).
Time=14 <->
	holds_at(condition(fc), Time).


% 
% ectest/ec_reader_test_examples.e:3186
% [time] % !HoldsAt(Condition(E1C),time).
not(holds_at(condition(e1c), Time)).


% 
% ectest/ec_reader_test_examples.e:3187
% [time] % time=% 10 <-> HoldsAt(Condition(E2C),time).
Time=10 <->
	holds_at(condition(e2c), Time).


% 
% ectest/ec_reader_test_examples.e:3188
% [time] % !HoldsAt(Condition(E3C),time).
not(holds_at(condition(e3c), Time)).


% 
% 
% completion Delta Happens
completion([delta, happens]).


% 
% range time 0 18
range(time, 0, 18).


% range offset 1 1
range(offset, 1, 1).


% ectest/ec_reader_test_examples.e:3194
% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter6/ThielscherCircuit1.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{Thielscher:1997,
% ;   author = "Michael Thielscher",
% ;   year = "1997",
% ;   title = "Ramification and causality",
% ;   journal = "Artificial Intelligence",
% ;   volume = "89",
% ;   pages = "317--364",
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
% ectest/ec_reader_test_examples.e:3229
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% load foundations/ECCausal.e
load('foundations/ECCausal.e').


% 
% sort switch
sort(switch).


% ectest/ec_reader_test_examples.e:3235
% sort relay
sort(relay).


% sort light
sort(light).


% 
% switch S1, S2, S3
t(switch, s1).


t(switch, s2).


t(switch, s3).


% relay R
t(relay, r).


% light L
t(light, l).


% ectest/ec_reader_test_examples.e:3241
% 
% event Light(light)
event(light(light)).


% event Close(switch)
event(close(switch)).


% event Open(switch)
event(open(switch)).


% event Activate(relay)
event(activate(relay)).


% 
% ectest/ec_reader_test_examples.e:3247
% fluent Lit(light)
fluent(lit(light)).


% fluent Closed(switch)
fluent(closed(switch)).


% fluent Activated(relay)
fluent(activated(relay)).


% 
% ectest/ec_reader_test_examples.e:3251
% [time]% 
% Stopped(Lit(L),time) &
% Initiated(Closed(S1),time) &
% Initiated(Closed(S2),time) ->
% Happens(Light(L),time).
stopped(lit(l), Time), initiated(closed(s1), Time), initiated(closed(s2), Time) ->
	happens(light(l), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3257
% [time]% 
% Started(Closed(S2),time) &
% Initiated(Activated(R),time) ->
% Happens(Open(S2),time).
started(closed(s2), Time), initiated(activated(r), Time) ->
	happens(open(s2), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3262
% [time]% 
% Stopped(Activated(R),time) &
% Initiated(Closed(S1),time) &
% Initiated(Closed(S3),time) ->
% Happens(Activate(R),time).
stopped(activated(r), Time), initiated(closed(s1), Time), initiated(closed(s3), Time) ->
	happens(activate(r), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3268
% [switch,time] % Initiates(Close(switch),Closed(switch),time).
initiates(close(Switch), closed(Switch), Time).


% 
% ectest/ec_reader_test_examples.e:3269
% [switch,time] % Terminates(Open(switch),Closed(switch),time).
terminates(open(Switch), closed(Switch), Time).


% 
% ectest/ec_reader_test_examples.e:3270
% [relay,time] % Initiates(Activate(relay),Activated(relay),time).
initiates(activate(Relay), activated(Relay), Time).


% 
% ectest/ec_reader_test_examples.e:3271
% [light,time] % Initiates(Light(light),Lit(light),time).
initiates(light(Light), lit(Light), Time).


% 
% 
% !HoldsAt(Closed(S1),0).
not(holds_at(closed(s1), 0)).


% 
% HoldsAt(Closed(S2),0).
holds_at(closed(s2), 0).


% 
% HoldsAt(Closed(S3),0).
holds_at(closed(s3), 0).


% 
% !HoldsAt(Activated(R),0).
not(holds_at(activated(r), 0)).


% 
% ectest/ec_reader_test_examples.e:3277
% !HoldsAt(Lit(L),0).
not(holds_at(lit(l), 0)).


% 
% 
% Happens(Close(S1),0).
happens(close(s1), 0).


% 
% 
% completion Happens
completion(happens).


% 
% ectest/ec_reader_test_examples.e:3283
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% 
% ectest/ec_reader_test_examples.e:3289
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter6/CarryingABook1.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; Example: Carrying a Book (Effect Axioms)
% ;
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ;
% ectest/ec_reader_test_examples.e:3314
% 
% option modeldiff on
option(modeldiff, on).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% ectest/ec_reader_test_examples.e:3320
% sort object
sort(object).


% sort agent: object
subsort(agent, object).


% sort room
sort(room).


% 
% object Book
t(object, book).


% agent Nathan
t(agent, nathan).


% ectest/ec_reader_test_examples.e:3326
% room LivingRoom, Kitchen
t(room, livingRoom).


t(room, kitchen).


% 
% event LetGoOf(agent,object)
event(letGoOf(agent, object)).


% event PickUp(agent,object)
event(pickUp(agent, object)).


% event Walk(agent,room,room)
event(walk(agent, room, room)).


% 
% ectest/ec_reader_test_examples.e:3332
% fluent InRoom(object,room)
fluent(inRoom(object, room)).


% fluent Holding(agent,object)
fluent(holding(agent, object)).


% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:3337
% [agent,room1,room2,time]% 
% Initiates(Walk(agent,room1,room2),InRoom(agent,room2),time).
initiates(walk(Agent, Room1, Room2), inRoom(Agent, Room2), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3340
% [agent,room1,room2,time]% 
% room1!=% room2 ->
% Terminates(Walk(agent,room1,room2),InRoom(agent,room1),time).
Room1\=Room2 ->
	terminates(walk(Agent, Room1, Room2),
		   inRoom(Agent, Room1),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:3344
% [agent,object,room,time]% 
% HoldsAt(InRoom(agent,room),time) &
% HoldsAt(InRoom(object,room),time) ->
% Initiates(PickUp(agent,object),Holding(agent,object),time).
holds_at(inRoom(Agent, Room), Time), holds_at(inRoom(Object, Room), Time) ->
	initiates(pickUp(Agent, Object),
		  holding(Agent, Object),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:3349
% [agent,object,time]% 
% HoldsAt(Holding(agent,object),time) ->
% Terminates(LetGoOf(agent,object),Holding(agent,object),time).
holds_at(holding(Agent, Object), Time) ->
	terminates(letGoOf(Agent, Object),
		   holding(Agent, Object),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:3353
% [agent,object,room1,room2,time]% 
% HoldsAt(Holding(agent,object),time) ->
% Initiates(Walk(agent,room1,room2),InRoom(object,room2),time).
holds_at(holding(Agent, Object), Time) ->
	initiates(walk(Agent, Room1, Room2),
		  inRoom(Object, Room2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:3357
% [agent,object,room1,room2,time]% 
% HoldsAt(Holding(agent,object),time) &
% room1!=room2 ->
% Terminates(Walk(agent,room1,room2),InRoom(object,room1),time).
holds_at(holding(Agent, Object), Time), Room1\=Room2 ->
	terminates(walk(Agent, Room1, Room2),
		   inRoom(Object, Room1),
		   Time).


% 
% 
% ; Delta
% ectest/ec_reader_test_examples.e:3363
% 
% Happens(PickUp(Nathan,Book),0).
happens(pickUp(nathan, book), 0).


% 
% Happens(Walk(Nathan,LivingRoom,Kitchen),1).
happens(walk(nathan, livingRoom, kitchen), 1).


% 
% 
% ; Psi
% 
% ectest/ec_reader_test_examples.e:3369
% [object,room1,room2,time]% 
% HoldsAt(InRoom(object,room1),time) &
% HoldsAt(InRoom(object,room2),time) ->
% room1=room2.
holds_at(inRoom(Object, Room1), Time), holds_at(inRoom(Object, Room2), Time) ->
	Room1=Room2.


% 
% 
% ; Gamma
% ectest/ec_reader_test_examples.e:3375
% 
% HoldsAt(InRoom(Nathan,LivingRoom),0).
holds_at(inRoom(nathan, livingRoom), 0).


% 
% HoldsAt(InRoom(Book,LivingRoom),0).
holds_at(inRoom(book, livingRoom), 0).


% 
% 
% ; added:
% !HoldsAt(Holding(Nathan,Book),0).
not(holds_at(holding(nathan, book), 0)).


% 
% ectest/ec_reader_test_examples.e:3381
% [agent,time] % !HoldsAt(Holding(agent,agent),time).
not(holds_at(holding(Agent, Agent), Time)).


% 
% 
% completion Happens
completion(happens).


% 
% range time 0 2
range(time, 0, 2).


% range offset 1 1
range(offset, 1, 1).


% ectest/ec_reader_test_examples.e:3387
% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter6/ThielscherCircuit2.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{Thielscher:1997,
% ;   author = "Michael Thielscher",
% ;   year = "1997",
% ;   title = "Ramification and causality",
% ;   journal = "Artificial Intelligence",
% ;   volume = "89",
% ;   pages = "317--364",
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
% ectest/ec_reader_test_examples.e:3422
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort switch
sort(switch).


% sort relay
sort(relay).


% ectest/ec_reader_test_examples.e:3428
% sort light
sort(light).


% 
% switch S1, S2, S3
t(switch, s1).


t(switch, s2).


t(switch, s3).


% relay R
t(relay, r).


% light L
t(light, l).


% 
% ectest/ec_reader_test_examples.e:3434
% event Light(light)
event(light(light)).


% event Unlight(light)
event(unlight(light)).


% event Close(switch)
event(close(switch)).


% event Open(switch)
event(open(switch)).


% event Activate(relay)
event(activate(relay)).


% 
% ectest/ec_reader_test_examples.e:3440
% fluent Lit(light)
fluent(lit(light)).


% fluent Closed(switch)
fluent(closed(switch)).


% fluent Activated(relay)
fluent(activated(relay)).


% 
% ectest/ec_reader_test_examples.e:3444
% [time]% 
% !HoldsAt(Lit(L),time) &
% HoldsAt(Closed(S1),time) &
% HoldsAt(Closed(S2),time) ->
% Happens(Light(L),time).
not(holds_at(lit(l), Time)), holds_at(closed(s1), Time), holds_at(closed(s2), Time) ->
	happens(light(l), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3450
% [time]% 
% HoldsAt(Lit(L),time) &
% (!HoldsAt(Closed(S1),time) | !HoldsAt(Closed(S2),time)) ->
% Happens(Unlight(L),time).
holds_at(lit(l), Time), (not(holds_at(closed(s1), Time));not(holds_at(closed(s2), Time))) ->
	happens(unlight(l), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3455
% [time]% 
% HoldsAt(Closed(S2),time) &
% HoldsAt(Activated(R),time) ->
% Happens(Open(S2),time).
holds_at(closed(s2), Time), holds_at(activated(r), Time) ->
	happens(open(s2), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3460
% [time]% 
% !HoldsAt(Activated(R),time) &
% HoldsAt(Closed(S1),time) &
% HoldsAt(Closed(S3),time) ->
% Happens(Activate(R),time).
not(holds_at(activated(r), Time)), holds_at(closed(s1), Time), holds_at(closed(s3), Time) ->
	happens(activate(r), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3466
% [switch,time] % Initiates(Close(switch),Closed(switch),time).
initiates(close(Switch), closed(Switch), Time).


% 
% ectest/ec_reader_test_examples.e:3467
% [switch,time] % Terminates(Open(switch),Closed(switch),time).
terminates(open(Switch), closed(Switch), Time).


% 
% ectest/ec_reader_test_examples.e:3468
% [relay,time] % Initiates(Activate(relay),Activated(relay),time).
initiates(activate(Relay), activated(Relay), Time).


% 
% ectest/ec_reader_test_examples.e:3469
% [light,time] % Initiates(Light(light),Lit(light),time).
initiates(light(Light), lit(Light), Time).


% 
% ectest/ec_reader_test_examples.e:3470
% [light,time] % Terminates(Unlight(light),Lit(light),time).
terminates(unlight(Light), lit(Light), Time).


% 
% 
% !HoldsAt(Closed(S1),0).
not(holds_at(closed(s1), 0)).


% 
% HoldsAt(Closed(S2),0).
holds_at(closed(s2), 0).


% 
% HoldsAt(Closed(S3),0).
holds_at(closed(s3), 0).


% 
% !HoldsAt(Activated(R),0).
not(holds_at(activated(r), 0)).


% 
% ectest/ec_reader_test_examples.e:3476
% !HoldsAt(Lit(L),0).
not(holds_at(lit(l), 0)).


% 
% 
% Happens(Close(S1),0).
happens(close(s1), 0).


% 
% 
% completion Happens
completion(happens).


% 
% ectest/ec_reader_test_examples.e:3482
% range time 0 4
range(time, 0, 4).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% 
% ectest/ec_reader_test_examples.e:3488
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter6/ShanahanCircuit.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @inproceedings{Shanahan:1999a,
% ;   author = "Murray Shanahan",
% ;   year = "1999",
% ;   title = "The ramification problem in the event calculus",
% ;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}ixteenth \uppercase{I}nternational \uppercase{J}oint \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
% ;   pages = "140--146",
% ;   address = "San Mateo, CA",
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
% ectest/ec_reader_test_examples.e:3520
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort switch
sort(switch).


% sort relay
sort(relay).


% ectest/ec_reader_test_examples.e:3526
% sort light
sort(light).


% 
% switch S1, S2, S3
t(switch, s1).


t(switch, s2).


t(switch, s3).


% relay R
t(relay, r).


% light L
t(light, l).


% 
% ectest/ec_reader_test_examples.e:3532
% event Light(light)
event(light(light)).


% event Unlight(light)
event(unlight(light)).


% event Close(switch)
event(close(switch)).


% event Open(switch)
event(open(switch)).


% event Activate(relay)
event(activate(relay)).


% event Deactivate(relay)
event(deactivate(relay)).


% ectest/ec_reader_test_examples.e:3538
% 
% fluent Lit(light)
fluent(lit(light)).


% fluent Closed(switch)
fluent(closed(switch)).


% fluent Activated(relay)
fluent(activated(relay)).


% 
% ectest/ec_reader_test_examples.e:3543
% [time]% 
% !HoldsAt(Lit(L),time) &
% HoldsAt(Closed(S1),time) &
% HoldsAt(Closed(S2),time) ->
% Happens(Light(L),time).
not(holds_at(lit(l), Time)), holds_at(closed(s1), Time), holds_at(closed(s2), Time) ->
	happens(light(l), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3549
% [time]% 
% HoldsAt(Lit(L),time) &
% (!HoldsAt(Closed(S1),time) | !HoldsAt(Closed(S2),time)) ->
% Happens(Unlight(L),time).
holds_at(lit(l), Time), (not(holds_at(closed(s1), Time));not(holds_at(closed(s2), Time))) ->
	happens(unlight(l), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3554
% [time]% 
% HoldsAt(Closed(S2),time) &
% HoldsAt(Activated(R),time) ->
% Happens(Open(S2),time).
holds_at(closed(s2), Time), holds_at(activated(r), Time) ->
	happens(open(s2), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3559
% [time]% 
% !HoldsAt(Activated(R),time) &
% HoldsAt(Closed(S1),time) &
% HoldsAt(Closed(S2),time) &
% HoldsAt(Closed(S3),time) ->
% Happens(Activate(R),time).
not(holds_at(activated(r), Time)), holds_at(closed(s1), Time), holds_at(closed(s2), Time), holds_at(closed(s3), Time) ->
	happens(activate(r), Time).


% 
% ectest/ec_reader_test_examples.e:3565
% 
% ectest/ec_reader_test_examples.e:3566
% [time]% 
% HoldsAt(Activated(R),time) &
% (!HoldsAt(Closed(S1),time) |
%  !HoldsAt(Closed(S2),time) |
%  !HoldsAt(Closed(S3),time)) ->
% Happens(Deactivate(R),time).
holds_at(activated(r), Time), (not(holds_at(closed(s1), Time));not(holds_at(closed(s2), Time));not(holds_at(closed(s3), Time))) ->
	happens(deactivate(r), Time).


% 
% ectest/ec_reader_test_examples.e:3572
% 
% ectest/ec_reader_test_examples.e:3573
% [switch,time] % Initiates(Close(switch),Closed(switch),time).
initiates(close(Switch), closed(Switch), Time).


% 
% ectest/ec_reader_test_examples.e:3574
% [switch,time] % Terminates(Open(switch),Closed(switch),time).
terminates(open(Switch), closed(Switch), Time).


% 
% ectest/ec_reader_test_examples.e:3575
% [relay,time] % Initiates(Activate(relay),Activated(relay),time).
initiates(activate(Relay), activated(Relay), Time).


% 
% ectest/ec_reader_test_examples.e:3576
% [relay,time] % Terminates(Deactivate(relay),Activated(relay),time).
terminates(deactivate(Relay), activated(Relay), Time).


% 
% ectest/ec_reader_test_examples.e:3577
% [light,time] % Initiates(Light(light),Lit(light),time).
initiates(light(Light), lit(Light), Time).


% 
% ectest/ec_reader_test_examples.e:3578
% [light,time] % Terminates(Unlight(light),Lit(light),time).
terminates(unlight(Light), lit(Light), Time).


% 
% 
% !HoldsAt(Closed(S1),0).
not(holds_at(closed(s1), 0)).


% 
% HoldsAt(Closed(S2),0).
holds_at(closed(s2), 0).


% 
% HoldsAt(Closed(S3),0).
holds_at(closed(s3), 0).


% 
% !HoldsAt(Activated(R),0).
not(holds_at(activated(r), 0)).


% 
% ectest/ec_reader_test_examples.e:3584
% !HoldsAt(Lit(L),0).
not(holds_at(lit(l), 0)).


% 
% 
% Happens(Close(S1),0).
happens(close(s1), 0).


% 
% 
% completion Happens
completion(happens).


% 
% ectest/ec_reader_test_examples.e:3590
% range time 0 4
range(time, 0, 4).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% 
% ectest/ec_reader_test_examples.e:3596
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter6/CarryingABook2.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; Example: Carrying a Book (Release Axioms and State Constraints)
% ;
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:3620
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort object
sort(object).


% sort agent: object
subsort(agent, object).


% ectest/ec_reader_test_examples.e:3626
% sort room
sort(room).


% 
% object Book
t(object, book).


% agent Nathan
t(agent, nathan).


% room LivingRoom, Kitchen
t(room, livingRoom).


t(room, kitchen).


% 
% ectest/ec_reader_test_examples.e:3632
% event LetGoOf(agent,object)
event(letGoOf(agent, object)).


% event PickUp(agent,object)
event(pickUp(agent, object)).


% event Walk(agent,room,room)
event(walk(agent, room, room)).


% 
% fluent InRoom(object,room)
fluent(inRoom(object, room)).


% fluent Holding(agent,object)
fluent(holding(agent, object)).


% ectest/ec_reader_test_examples.e:3638
% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:3641
% [agent,room1,room2,time]% 
% Initiates(Walk(agent,room1,room2),InRoom(agent,room2),time).
initiates(walk(Agent, Room1, Room2), inRoom(Agent, Room2), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3644
% [agent,room1,room2,time]% 
% room1!=% room2 ->
% Terminates(Walk(agent,room1,room2),InRoom(agent,room1),time).
Room1\=Room2 ->
	terminates(walk(Agent, Room1, Room2),
		   inRoom(Agent, Room1),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:3648
% [agent,object,room,time]% 
% HoldsAt(InRoom(agent,room),time) &
% HoldsAt(InRoom(object,room),time) ->
% Initiates(PickUp(agent,object),Holding(agent,object),time).
holds_at(inRoom(Agent, Room), Time), holds_at(inRoom(Object, Room), Time) ->
	initiates(pickUp(Agent, Object),
		  holding(Agent, Object),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:3653
% [agent,object,time]% 
% HoldsAt(Holding(agent,object),time) ->
% Terminates(LetGoOf(agent,object),Holding(agent,object),time).
holds_at(holding(Agent, Object), Time) ->
	terminates(letGoOf(Agent, Object),
		   holding(Agent, Object),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:3657
% [agent,object,room,time]% 
% Releases(PickUp(agent,object),InRoom(object,room),time).
releases(pickUp(Agent, Object), inRoom(Object, Room), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3660
% [agent,object,room,time]% 
% HoldsAt(InRoom(agent,room),time) ->
% Initiates(LetGoOf(agent,object),InRoom(object,room),time).
holds_at(inRoom(Agent, Room), Time) ->
	initiates(letGoOf(Agent, Object),
		  inRoom(Object, Room),
		  Time).


% 
% 
% ; Delta
% 
% ectest/ec_reader_test_examples.e:3666
% Happens(PickUp(Nathan,Book),0).
happens(pickUp(nathan, book), 0).


% 
% Happens(Walk(Nathan,LivingRoom,Kitchen),1).
happens(walk(nathan, livingRoom, kitchen), 1).


% 
% 
% ; Psi
% 
% ectest/ec_reader_test_examples.e:3671
% [object,room1,room2,time]% 
% HoldsAt(InRoom(object,room1),time) &
% HoldsAt(InRoom(object,room2),time) ->
% room1=room2.
holds_at(inRoom(Object, Room1), Time), holds_at(inRoom(Object, Room2), Time) ->
	Room1=Room2.


% 
% 
% ectest/ec_reader_test_examples.e:3676
% [agent,object,room,time]% 
% HoldsAt(Holding(agent,object),time) &
% HoldsAt(InRoom(agent,room),time) ->
% HoldsAt(InRoom(object,room),time).
holds_at(holding(Agent, Object), Time), holds_at(inRoom(Agent, Room), Time) ->
	holds_at(inRoom(Object, Room), Time).


% 
% 
% ; Gamma
% ectest/ec_reader_test_examples.e:3682
% 
% HoldsAt(InRoom(Nathan,LivingRoom),0).
holds_at(inRoom(nathan, livingRoom), 0).


% 
% HoldsAt(InRoom(Book,LivingRoom),0).
holds_at(inRoom(book, livingRoom), 0).


% 
% 
% ; added:
% !HoldsAt(Holding(Nathan,Book),0).
not(holds_at(holding(nathan, book), 0)).


% 
% ectest/ec_reader_test_examples.e:3688
% [agent,time] % !HoldsAt(Holding(agent,agent),time).
not(holds_at(holding(Agent, Agent), Time)).


% 
% 
% completion Happens
completion(happens).


% 
% range time 0 2
range(time, 0, 2).


% range offset 1 1
range(offset, 1, 1).


% ectest/ec_reader_test_examples.e:3694
% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter7/HotAirBalloon.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{MillerShanahan:1999,
% ;   author = "Rob Miller and Murray Shanahan",
% ;   year = "1999",
% ;   title = "The event calculus in classical logic---\uppercase{A}lternative axiomatisations",
% ;   journal = "Link{\"{o}}ping Electronic Articles in Computer and Information Science",
% ;   volume = "4",
% ;   number = "016",
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
% ectest/ec_reader_test_examples.e:3729
% 
% option encoding 3
option(encoding, 3).


% option trajectory on
option(trajectory, on).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% ectest/ec_reader_test_examples.e:3735
% 
% sort balloon
sort(balloon).


% sort agent
sort(agent).


% sort height: integer
subsort(height, integer).


% 
% agent Nathan
t(agent, nathan).


% ectest/ec_reader_test_examples.e:3741
% balloon Balloon
t(balloon, balloon).


% 
% fluent HeaterOn(balloon)
fluent(heaterOn(balloon)).


% fluent Height(balloon,height)
fluent(height(balloon, height)).


% noninertial Height
noninertial(height).


% 
% ectest/ec_reader_test_examples.e:3747
% event TurnOnHeater(agent,balloon)
event(turnOnHeater(agent, balloon)).


% event TurnOffHeater(agent,balloon)
event(turnOffHeater(agent, balloon)).


% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:3752
% [agent,balloon,time]% 
% Initiates(TurnOnHeater(agent,balloon),HeaterOn(balloon),time).
initiates(turnOnHeater(Agent, Balloon), heaterOn(Balloon), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3755
% [agent,balloon,time]% 
% Terminates(TurnOffHeater(agent,balloon),HeaterOn(balloon),time).
terminates(turnOffHeater(Agent, Balloon), heaterOn(Balloon), Time).


% 
% 
% ; Delta
% 
% Delta:
directive(delta).


 % Happens(TurnOnHeater(Nathan,Balloon),0).
happens(turnOnHeater(nathan, balloon), 0).


% 
% ectest/ec_reader_test_examples.e:3761
% Delta:
directive(delta).


 % Happens(TurnOffHeater(Nathan,Balloon),2).
happens(turnOffHeater(nathan, balloon), 2).


% 
% 
% ; Psi
% 
% ectest/ec_reader_test_examples.e:3765
% [balloon,height1,height2,time]% 
% HoldsAt(Height(balloon,height1),time) &
% HoldsAt(Height(balloon,height2),time) ->
% height1=height2.
holds_at(height(Balloon, Height1), Time), holds_at(height(Balloon, Height2), Time) ->
	Height1=Height2.


% 
% 
% ; Pi
% ectest/ec_reader_test_examples.e:3771
% 
% ectest/ec_reader_test_examples.e:3772
% [balloon,height1,height2,offset,time]% 
% HoldsAt(Height(balloon,height1),time) &
% height2 = (height1 + offset) ->
% Trajectory(HeaterOn(balloon),time,Height(balloon,height2),offset).
holds_at(height(Balloon, Height1), Time), Height2=Height1+Offset ->
	trajectory(heaterOn(Balloon),
		   Time,
		   height(Balloon, Height2),
		   Offset).


% 
% 
% ectest/ec_reader_test_examples.e:3777
% [balloon,height1,height2,offset,time]% 
% HoldsAt(Height(balloon,height1),time) &
% height2 = (height1 - offset) ->
% AntiTrajectory(HeaterOn(balloon),time,Height(balloon,height2),offset).
holds_at(height(Balloon, Height1), Time), Height2=Height1-Offset ->
	antiTrajectory(heaterOn(Balloon),
		       Time,
		       height(Balloon, Height2),
		       Offset).


% 
% 
% ; Gamma
% ectest/ec_reader_test_examples.e:3783
% 
% HoldsAt(Height(Balloon,0),0).
holds_at(height(balloon, 0), 0).


% 
% 
% ; added:
% !HoldsAt(HeaterOn(Balloon),0).
not(holds_at(heaterOn(balloon), 0)).


% 
% 
% ectest/ec_reader_test_examples.e:3789
% completion Delta Happens
completion([delta, happens]).


% 
% range time 0 3
range(time, 0, 3).


% range height 0 2
range(height, 0, 2).


% range offset 1 2
range(offset, 1, 2).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:3796
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter7/FallingObjectWithEvents.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:3820
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort object
sort(object).


% sort agent
sort(agent).


% ectest/ec_reader_test_examples.e:3826
% sort height: integer
subsort(height, integer).


% 
% agent Nathan
t(agent, nathan).


% object Apple
t(object, apple).


% 
% fluent Falling(object)
fluent(falling(object)).


% ectest/ec_reader_test_examples.e:3832
% fluent Height(object,height)
fluent(height(object, height)).


% 
% event Drop(agent,object)
event(drop(agent, object)).


% event HitGround(object)
event(hitGround(object)).


% 
% ; Sigma
% ectest/ec_reader_test_examples.e:3838
% 
% ectest/ec_reader_test_examples.e:3839
% [agent,object,time]% 
% Initiates(Drop(agent,object),Falling(object),time).
initiates(drop(Agent, Object), falling(Object), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3842
% [agent,object,height,time]% 
% Releases(Drop(agent,object),Height(object,height),time).
releases(drop(Agent, Object), height(Object, Height), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3845
% [object,time]% 
% Terminates(HitGround(object),Falling(object),time).
terminates(hitGround(Object), falling(Object), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3848
% [object,height,time]% 
% HoldsAt(Height(object,height),time) ->
% Initiates(HitGround(object),Height(object,height),time).
holds_at(height(Object, Height), Time) ->
	initiates(hitGround(Object),
		  height(Object, Height),
		  Time).


% 
% 
% ; Delta
% 
% ectest/ec_reader_test_examples.e:3854
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:3854
% [object,time]% 
% HoldsAt(Falling(object),time) &
% HoldsAt(Height(object,0),time) ->
% Happens(HitGround(object),time).
holds_at(falling(Object), Time), holds_at(height(Object, 0), Time) ->
	happens(hitGround(Object), Time).


% 
% 
% Delta:
directive(delta).


 % Happens(Drop(Nathan,Apple),0).
happens(drop(nathan, apple), 0).


% 
% ectest/ec_reader_test_examples.e:3860
% 
% ; Psi
% 
% ectest/ec_reader_test_examples.e:3863
% [object,height1,height2,time]% 
% HoldsAt(Height(object,height1),time) &
% HoldsAt(Height(object,height2),time) ->
% height1=height2.
holds_at(height(Object, Height1), Time), holds_at(height(Object, Height2), Time) ->
	Height1=Height2.


% 
% 
% ; Pi
% ectest/ec_reader_test_examples.e:3869
% 
% ectest/ec_reader_test_examples.e:3870
% [object,height1,height2,offset,time]% 
% HoldsAt(Height(object,height1),time) &
% height2 = (height1 - offset) ->
% Trajectory(Falling(object),time,Height(object,height2),offset).
holds_at(height(Object, Height1), Time), Height2=Height1-Offset ->
	trajectory(falling(Object),
		   Time,
		   height(Object, Height2),
		   Offset).


% 
% 
% ; Gamma
% ectest/ec_reader_test_examples.e:3876
% 
% !HoldsAt(Falling(Apple),0).
not(holds_at(falling(apple), 0)).


% 
% HoldsAt(Height(Apple,3),0).
holds_at(height(apple, 3), 0).


% 
% 
% completion Delta Happens
completion([delta, happens]).


% 
% ectest/ec_reader_test_examples.e:3882
% range time 0 5
range(time, 0, 5).


% range height 0 3
range(height, 0, 3).


% range offset 1 3
range(offset, 1, 3).


% 
% ; End of file.
% 
% ectest/ec_reader_test_examples.e:3888
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter7/FallingObjectWithAntiTrajectory.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:3911
% 
% option encoding 3
option(encoding, 3).


% option trajectory on
option(trajectory, on).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% ectest/ec_reader_test_examples.e:3917
% 
% sort object
sort(object).


% sort agent
sort(agent).


% sort height: integer
subsort(height, integer).


% 
% agent Nathan
t(agent, nathan).


% ectest/ec_reader_test_examples.e:3923
% object Apple
t(object, apple).


% 
% fluent Falling(object)
fluent(falling(object)).


% fluent Height(object,height)
fluent(height(object, height)).


% noninertial Height
noninertial(height).


% 
% ectest/ec_reader_test_examples.e:3929
% event Drop(agent,object)
event(drop(agent, object)).


% event HitGround(object)
event(hitGround(object)).


% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:3934
% [agent,object,time]% 
% Initiates(Drop(agent,object),Falling(object),time).
initiates(drop(Agent, Object), falling(Object), Time).


% 
% 
% ectest/ec_reader_test_examples.e:3937
% [object,time]% 
% Terminates(HitGround(object),Falling(object),time).
terminates(hitGround(Object), falling(Object), Time).


% 
% 
% ; Delta
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:3942
% [object,time]% 
% HoldsAt(Falling(object),time) &
% HoldsAt(Height(object,0),time) ->
% Happens(HitGround(object),time).
holds_at(falling(Object), Time), holds_at(height(Object, 0), Time) ->
	happens(hitGround(Object), Time).


% 
% 
% Delta:
directive(delta).


 % Happens(Drop(Nathan,Apple),0).
happens(drop(nathan, apple), 0).


% 
% ectest/ec_reader_test_examples.e:3948
% 
% ; Psi
% 
% ectest/ec_reader_test_examples.e:3951
% [object,height1,height2,time]% 
% HoldsAt(Height(object,height1),time) &
% HoldsAt(Height(object,height2),time) ->
% height1=height2.
holds_at(height(Object, Height1), Time), holds_at(height(Object, Height2), Time) ->
	Height1=Height2.


% 
% 
% ; Pi
% ectest/ec_reader_test_examples.e:3957
% 
% ectest/ec_reader_test_examples.e:3958
% [object,height1,height2,offset,time]% 
% HoldsAt(Height(object,height1),time) &
% height2 = (height1 - offset) ->
% Trajectory(Falling(object),time,Height(object,height2),offset).
holds_at(height(Object, Height1), Time), Height2=Height1-Offset ->
	trajectory(falling(Object),
		   Time,
		   height(Object, Height2),
		   Offset).


% 
% 
% ectest/ec_reader_test_examples.e:3963
% [object,height,offset,time]% 
% HoldsAt(Height(object,height),time) ->
% AntiTrajectory(Falling(object),time,Height(object,height),offset).
holds_at(height(Object, Height), Time) ->
	antiTrajectory(falling(Object),
		       Time,
		       height(Object, Height),
		       Offset).


% 
% 
% ; Gamma
% 
% ectest/ec_reader_test_examples.e:3969
% !HoldsAt(Falling(Apple),0).
not(holds_at(falling(apple), 0)).


% 
% HoldsAt(Height(Apple,3),0).
holds_at(height(apple, 3), 0).


% 
% 
% completion Delta Happens
completion([delta, happens]).


% 
% range time 0 5
range(time, 0, 5).


% ectest/ec_reader_test_examples.e:3975
% range height 0 3
range(height, 0, 3).


% range offset 1 3
range(offset, 1, 3).


% 
% ; End of file.
% 
% 
% ectest/ec_reader_test_examples.e:3981
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter3/Telephone2.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:4003
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% sort phone
sort(phone).


% ectest/ec_reader_test_examples.e:4009
% 
% agent Agent1, Agent2
t(agent, agent1).


t(agent, agent2).


% phone Phone1, Phone2
t(phone, phone1).


t(phone, phone2).


% 
% fluent Ringing(phone,phone)
fluent(ringing(phone, phone)).


% fluent DialTone(phone)
fluent(dialTone(phone)).


% ectest/ec_reader_test_examples.e:4015
% fluent BusySignal(phone)
fluent(busySignal(phone)).


% fluent Idle(phone)
fluent(idle(phone)).


% fluent Connected(phone,phone)
fluent(connected(phone, phone)).


% fluent Disconnected(phone)
fluent(disconnected(phone)).


% 
% event PickUp(agent,phone)
event(pickUp(agent, phone)).


% ectest/ec_reader_test_examples.e:4021
% event SetDown(agent,phone)
event(setDown(agent, phone)).


% event Dial(agent,phone,phone)
event(dial(agent, phone, phone)).


% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:4026
% [agent,phone,time]% 
% HoldsAt(Idle(phone),time) ->
% Initiates(PickUp(agent,phone),DialTone(phone),time).
holds_at(idle(Phone), Time) ->
	initiates(pickUp(Agent, Phone),
		  dialTone(Phone),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4030
% [agent,phone,time]% 
% HoldsAt(Idle(phone),time) ->
% Terminates(PickUp(agent,phone),Idle(phone),time).
holds_at(idle(Phone), Time) ->
	terminates(pickUp(Agent, Phone),
		   idle(Phone),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4034
% [agent,phone,time]% 
% HoldsAt(DialTone(phone),time) ->
% Initiates(SetDown(agent,phone),Idle(phone),time).
holds_at(dialTone(Phone), Time) ->
	initiates(setDown(Agent, Phone),
		  idle(Phone),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4038
% [agent,phone,time]% 
% HoldsAt(DialTone(phone),time) ->
% Terminates(SetDown(agent,phone),DialTone(phone),time).
holds_at(dialTone(Phone), Time) ->
	terminates(setDown(Agent, Phone),
		   dialTone(Phone),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4042
% [agent,phone1,phone2,time]% 
% HoldsAt(DialTone(phone1),time) &
% HoldsAt(Idle(phone2),time) ->
% Initiates(Dial(agent,phone1,phone2),Ringing(phone1,phone2),time).
holds_at(dialTone(Phone1), Time), holds_at(idle(Phone2), Time) ->
	initiates(dial(Agent, Phone1, Phone2),
		  ringing(Phone1, Phone2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4047
% [agent,phone1,phone2,time]% 
% HoldsAt(DialTone(phone1),time) &
% HoldsAt(Idle(phone2),time) ->
% Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).
holds_at(dialTone(Phone1), Time), holds_at(idle(Phone2), Time) ->
	terminates(dial(Agent, Phone1, Phone2),
		   dialTone(Phone1),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4052
% [agent,phone1,phone2,time]% 
% HoldsAt(DialTone(phone1),time) &
% HoldsAt(Idle(phone2),time) ->
% Terminates(Dial(agent,phone1,phone2),Idle(phone2),time).
holds_at(dialTone(Phone1), Time), holds_at(idle(Phone2), Time) ->
	terminates(dial(Agent, Phone1, Phone2),
		   idle(Phone2),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4057
% [agent,phone1,phone2,time]% 
% HoldsAt(DialTone(phone1),time) &
% !HoldsAt(Idle(phone2),time) ->
% Initiates(Dial(agent,phone1,phone2),BusySignal(phone1),time).
holds_at(dialTone(Phone1), Time), not(holds_at(idle(Phone2), Time)) ->
	initiates(dial(Agent, Phone1, Phone2),
		  busySignal(Phone1),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4062
% [agent,phone1,phone2,time]% 
% HoldsAt(DialTone(phone1),time) &
% !HoldsAt(Idle(phone2),time) ->
% Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).
holds_at(dialTone(Phone1), Time), not(holds_at(idle(Phone2), Time)) ->
	terminates(dial(Agent, Phone1, Phone2),
		   dialTone(Phone1),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4067
% [agent,phone,time]% 
% HoldsAt(BusySignal(phone),time) ->
% Initiates(SetDown(agent,phone),Idle(phone),time).
holds_at(busySignal(Phone), Time) ->
	initiates(setDown(Agent, Phone),
		  idle(Phone),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4071
% [agent,phone,time]% 
% HoldsAt(BusySignal(phone),time) ->
% Terminates(SetDown(agent,phone),BusySignal(phone),time).
holds_at(busySignal(Phone), Time) ->
	terminates(setDown(Agent, Phone),
		   busySignal(Phone),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4075
% [agent,phone1,phone2,time]% 
% HoldsAt(Ringing(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone1),Idle(phone1),time).
holds_at(ringing(Phone1, Phone2), Time) ->
	initiates(setDown(Agent, Phone1),
		  idle(Phone1),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4079
% [agent,phone1,phone2,time]% 
% HoldsAt(Ringing(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone1),Idle(phone2),time).
holds_at(ringing(Phone1, Phone2), Time) ->
	initiates(setDown(Agent, Phone1),
		  idle(Phone2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4083
% [agent,phone1,phone2,time]% 
% HoldsAt(Ringing(phone1,phone2),time) ->
% Terminates(SetDown(agent,phone1),Ringing(phone1,phone2),time).
holds_at(ringing(Phone1, Phone2), Time) ->
	terminates(setDown(Agent, Phone1),
		   ringing(Phone1, Phone2),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4087
% [agent,phone1,phone2,time]% 
% HoldsAt(Ringing(phone1,phone2),time) ->
% Initiates(PickUp(agent,phone2),Connected(phone1,phone2),time).
holds_at(ringing(Phone1, Phone2), Time) ->
	initiates(pickUp(Agent, Phone2),
		  connected(Phone1, Phone2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4091
% [agent,phone1,phone2,time]% 
% HoldsAt(Ringing(phone1,phone2),time) ->
% Terminates(PickUp(agent,phone2),Ringing(phone1,phone2),time).
holds_at(ringing(Phone1, Phone2), Time) ->
	terminates(pickUp(Agent, Phone2),
		   ringing(Phone1, Phone2),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4095
% [agent,phone1,phone2,time]% 
% HoldsAt(Connected(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone1),Idle(phone1),time).
holds_at(connected(Phone1, Phone2), Time) ->
	initiates(setDown(Agent, Phone1),
		  idle(Phone1),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4099
% [agent,phone1,phone2,time]% 
% HoldsAt(Connected(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone1),Disconnected(phone2),time).
holds_at(connected(Phone1, Phone2), Time) ->
	initiates(setDown(Agent, Phone1),
		  disconnected(Phone2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4103
% [agent,phone1,phone2,time]% 
% HoldsAt(Connected(phone1,phone2),time) ->
% Terminates(SetDown(agent,phone1),Connected(phone1,phone2),time).
holds_at(connected(Phone1, Phone2), Time) ->
	terminates(setDown(Agent, Phone1),
		   connected(Phone1, Phone2),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4107
% [agent,phone1,phone2,time]% 
% HoldsAt(Connected(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone2),Idle(phone2),time).
holds_at(connected(Phone1, Phone2), Time) ->
	initiates(setDown(Agent, Phone2),
		  idle(Phone2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4111
% [agent,phone1,phone2,time]% 
% HoldsAt(Connected(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone2),Disconnected(phone1),time).
holds_at(connected(Phone1, Phone2), Time) ->
	initiates(setDown(Agent, Phone2),
		  disconnected(Phone1),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4115
% [agent,phone1,phone2,time]% 
% HoldsAt(Connected(phone1,phone2),time) ->
% Terminates(SetDown(agent,phone2),Connected(phone1,phone2),time).
holds_at(connected(Phone1, Phone2), Time) ->
	terminates(setDown(Agent, Phone2),
		   connected(Phone1, Phone2),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4119
% [agent,phone,time]% 
% HoldsAt(Disconnected(phone),time) ->
% Initiates(SetDown(agent,phone),Idle(phone),time).
holds_at(disconnected(Phone), Time) ->
	initiates(setDown(Agent, Phone),
		  idle(Phone),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4123
% [agent,phone,time]% 
% HoldsAt(Disconnected(phone),time) ->
% Terminates(SetDown(agent,phone),Disconnected(phone),time).
holds_at(disconnected(Phone), Time) ->
	terminates(setDown(Agent, Phone),
		   disconnected(Phone),
		   Time).


% 
% 
% ; Delta
% 
% ectest/ec_reader_test_examples.e:4129
% Happens(PickUp(Agent1,Phone1),0).
happens(pickUp(agent1, phone1), 0).


% 
% Happens(Dial(Agent1,Phone1,Phone2),1).
happens(dial(agent1, phone1, phone2), 1).


% 
% Happens(PickUp(Agent2,Phone2),2).
happens(pickUp(agent2, phone2), 2).


% 
% 
% ; Psi
% 
% ectest/ec_reader_test_examples.e:4135
% [phone,time]% 
% !HoldsAt(Ringing(phone,phone),time).
not(holds_at(ringing(Phone, Phone), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:4138
% [phone1,phone2,time]% 
% HoldsAt(Ringing(phone1,phone2),time) &
% phone1!=phone2 ->
% !HoldsAt(Ringing(phone2,phone1),time).
holds_at(ringing(Phone1, Phone2), Time), Phone1\=Phone2 ->
	not(holds_at(ringing(Phone2, Phone1), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:4143
% [phone,time]% 
% !HoldsAt(Connected(phone,phone),time).
not(holds_at(connected(Phone, Phone), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:4146
% [phone1,phone2,time]% 
% HoldsAt(Connected(phone1,phone2),time) &
% phone1!=phone2 ->
% !HoldsAt(Connected(phone2,phone1),time).
holds_at(connected(Phone1, Phone2), Time), Phone1\=Phone2 ->
	not(holds_at(connected(Phone2, Phone1), Time)).


% 
% 
% mutex Idle, DialTone, BusySignal, Disconnected
mutex([idle, dialTone, busySignal, disconnected]).


% ectest/ec_reader_test_examples.e:4152
% 
% ectest/ec_reader_test_examples.e:4153
% [phone1,phone2,time]% 
% HoldsAt(Idle(phone1),time) ->
% !HoldsAt(Ringing(phone1,phone2),time) &
% !HoldsAt(Connected(phone1,phone2),time).
holds_at(idle(Phone1), Time) ->
	not(holds_at(ringing(Phone1, Phone2), Time)),
	not(holds_at(connected(Phone1, Phone2), Time)).


% 
% 
% ; etc.
% ectest/ec_reader_test_examples.e:4159
% 
% ; Gamma
% 
% ectest/ec_reader_test_examples.e:4162
% [phone] % HoldsAt(Idle(phone),0).
holds_at(idle(Phone), 0).


% 
% 
% completion Happens
completion(happens).


% 
% range time 0 3
range(time, 0, 3).


% range offset 1 1
range(offset, 1, 1).


% ectest/ec_reader_test_examples.e:4168
% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter3/Telephone1.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:4194
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% sort phone
sort(phone).


% ectest/ec_reader_test_examples.e:4200
% 
% agent Agent1, Agent2
t(agent, agent1).


t(agent, agent2).


% phone Phone1, Phone2
t(phone, phone1).


t(phone, phone2).


% 
% fluent Ringing(phone,phone)
fluent(ringing(phone, phone)).


% fluent DialTone(phone)
fluent(dialTone(phone)).


% ectest/ec_reader_test_examples.e:4206
% fluent BusySignal(phone)
fluent(busySignal(phone)).


% fluent Idle(phone)
fluent(idle(phone)).


% fluent Connected(phone,phone)
fluent(connected(phone, phone)).


% fluent Disconnected(phone)
fluent(disconnected(phone)).


% 
% event PickUp(agent,phone)
event(pickUp(agent, phone)).


% ectest/ec_reader_test_examples.e:4212
% event SetDown(agent,phone)
event(setDown(agent, phone)).


% event Dial(agent,phone,phone)
event(dial(agent, phone, phone)).


% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:4217
% [agent,phone,time]% 
% HoldsAt(Idle(phone),time) ->
% Initiates(PickUp(agent,phone),DialTone(phone),time).
holds_at(idle(Phone), Time) ->
	initiates(pickUp(Agent, Phone),
		  dialTone(Phone),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4221
% [agent,phone,time]% 
% HoldsAt(Idle(phone),time) ->
% Terminates(PickUp(agent,phone),Idle(phone),time).
holds_at(idle(Phone), Time) ->
	terminates(pickUp(Agent, Phone),
		   idle(Phone),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4225
% [agent,phone,time]% 
% HoldsAt(DialTone(phone),time) ->
% Initiates(SetDown(agent,phone),Idle(phone),time).
holds_at(dialTone(Phone), Time) ->
	initiates(setDown(Agent, Phone),
		  idle(Phone),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4229
% [agent,phone,time]% 
% HoldsAt(DialTone(phone),time) ->
% Terminates(SetDown(agent,phone),DialTone(phone),time).
holds_at(dialTone(Phone), Time) ->
	terminates(setDown(Agent, Phone),
		   dialTone(Phone),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4233
% [agent,phone1,phone2,time]% 
% HoldsAt(DialTone(phone1),time) &
% HoldsAt(Idle(phone2),time) ->
% Initiates(Dial(agent,phone1,phone2),Ringing(phone1,phone2),time).
holds_at(dialTone(Phone1), Time), holds_at(idle(Phone2), Time) ->
	initiates(dial(Agent, Phone1, Phone2),
		  ringing(Phone1, Phone2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4238
% [agent,phone1,phone2,time]% 
% HoldsAt(DialTone(phone1),time) &
% HoldsAt(Idle(phone2),time) ->
% Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).
holds_at(dialTone(Phone1), Time), holds_at(idle(Phone2), Time) ->
	terminates(dial(Agent, Phone1, Phone2),
		   dialTone(Phone1),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4243
% [agent,phone1,phone2,time]% 
% HoldsAt(DialTone(phone1),time) &
% HoldsAt(Idle(phone2),time) ->
% Terminates(Dial(agent,phone1,phone2),Idle(phone2),time).
holds_at(dialTone(Phone1), Time), holds_at(idle(Phone2), Time) ->
	terminates(dial(Agent, Phone1, Phone2),
		   idle(Phone2),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4248
% [agent,phone1,phone2,time]% 
% HoldsAt(DialTone(phone1),time) &
% !HoldsAt(Idle(phone2),time) ->
% Initiates(Dial(agent,phone1,phone2),BusySignal(phone1),time).
holds_at(dialTone(Phone1), Time), not(holds_at(idle(Phone2), Time)) ->
	initiates(dial(Agent, Phone1, Phone2),
		  busySignal(Phone1),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4253
% [agent,phone1,phone2,time]% 
% HoldsAt(DialTone(phone1),time) &
% !HoldsAt(Idle(phone2),time) ->
% Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).
holds_at(dialTone(Phone1), Time), not(holds_at(idle(Phone2), Time)) ->
	terminates(dial(Agent, Phone1, Phone2),
		   dialTone(Phone1),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4258
% [agent,phone,time]% 
% HoldsAt(BusySignal(phone),time) ->
% Initiates(SetDown(agent,phone),Idle(phone),time).
holds_at(busySignal(Phone), Time) ->
	initiates(setDown(Agent, Phone),
		  idle(Phone),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4262
% [agent,phone,time]% 
% HoldsAt(BusySignal(phone),time) ->
% Terminates(SetDown(agent,phone),BusySignal(phone),time).
holds_at(busySignal(Phone), Time) ->
	terminates(setDown(Agent, Phone),
		   busySignal(Phone),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4266
% [agent,phone1,phone2,time]% 
% HoldsAt(Ringing(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone1),Idle(phone1),time).
holds_at(ringing(Phone1, Phone2), Time) ->
	initiates(setDown(Agent, Phone1),
		  idle(Phone1),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4270
% [agent,phone1,phone2,time]% 
% HoldsAt(Ringing(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone1),Idle(phone2),time).
holds_at(ringing(Phone1, Phone2), Time) ->
	initiates(setDown(Agent, Phone1),
		  idle(Phone2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4274
% [agent,phone1,phone2,time]% 
% HoldsAt(Ringing(phone1,phone2),time) ->
% Terminates(SetDown(agent,phone1),Ringing(phone1,phone2),time).
holds_at(ringing(Phone1, Phone2), Time) ->
	terminates(setDown(Agent, Phone1),
		   ringing(Phone1, Phone2),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4278
% [agent,phone1,phone2,time]% 
% HoldsAt(Ringing(phone1,phone2),time) ->
% Initiates(PickUp(agent,phone2),Connected(phone1,phone2),time).
holds_at(ringing(Phone1, Phone2), Time) ->
	initiates(pickUp(Agent, Phone2),
		  connected(Phone1, Phone2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4282
% [agent,phone1,phone2,time]% 
% HoldsAt(Ringing(phone1,phone2),time) ->
% Terminates(PickUp(agent,phone2),Ringing(phone1,phone2),time).
holds_at(ringing(Phone1, Phone2), Time) ->
	terminates(pickUp(Agent, Phone2),
		   ringing(Phone1, Phone2),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4286
% [agent,phone1,phone2,time]% 
% HoldsAt(Connected(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone1),Idle(phone1),time).
holds_at(connected(Phone1, Phone2), Time) ->
	initiates(setDown(Agent, Phone1),
		  idle(Phone1),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4290
% [agent,phone1,phone2,time]% 
% HoldsAt(Connected(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone1),Disconnected(phone2),time).
holds_at(connected(Phone1, Phone2), Time) ->
	initiates(setDown(Agent, Phone1),
		  disconnected(Phone2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4294
% [agent,phone1,phone2,time]% 
% HoldsAt(Connected(phone1,phone2),time) ->
% Terminates(SetDown(agent,phone1),Connected(phone1,phone2),time).
holds_at(connected(Phone1, Phone2), Time) ->
	terminates(setDown(Agent, Phone1),
		   connected(Phone1, Phone2),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4298
% [agent,phone1,phone2,time]% 
% HoldsAt(Connected(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone2),Idle(phone2),time).
holds_at(connected(Phone1, Phone2), Time) ->
	initiates(setDown(Agent, Phone2),
		  idle(Phone2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4302
% [agent,phone1,phone2,time]% 
% HoldsAt(Connected(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone2),Disconnected(phone1),time).
holds_at(connected(Phone1, Phone2), Time) ->
	initiates(setDown(Agent, Phone2),
		  disconnected(Phone1),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4306
% [agent,phone1,phone2,time]% 
% HoldsAt(Connected(phone1,phone2),time) ->
% Terminates(SetDown(agent,phone2),Connected(phone1,phone2),time).
holds_at(connected(Phone1, Phone2), Time) ->
	terminates(setDown(Agent, Phone2),
		   connected(Phone1, Phone2),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4310
% [agent,phone,time]% 
% HoldsAt(Disconnected(phone),time) ->
% Initiates(SetDown(agent,phone),Idle(phone),time).
holds_at(disconnected(Phone), Time) ->
	initiates(setDown(Agent, Phone),
		  idle(Phone),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4314
% [agent,phone,time]% 
% HoldsAt(Disconnected(phone),time) ->
% Terminates(SetDown(agent,phone),Disconnected(phone),time).
holds_at(disconnected(Phone), Time) ->
	terminates(setDown(Agent, Phone),
		   disconnected(Phone),
		   Time).


% 
% 
% ; Delta
% 
% ectest/ec_reader_test_examples.e:4320
% Delta:
directive(delta).


 % Happens(PickUp(Agent1,Phone1),0).
happens(pickUp(agent1, phone1), 0).


% 
% Delta:
directive(delta).


 % Happens(Dial(Agent1,Phone1,Phone2),1).
happens(dial(agent1, phone1, phone2), 1).


% 
% Delta:
directive(delta).


 % Happens(PickUp(Agent2,Phone2),2).
happens(pickUp(agent2, phone2), 2).


% 
% 
% ; Gamma
% 
% ectest/ec_reader_test_examples.e:4326
% [phone] % HoldsAt(Idle(phone),0).
holds_at(idle(Phone), 0).


% 
% ectest/ec_reader_test_examples.e:4327
% [phone] % !HoldsAt(DialTone(phone),0).
not(holds_at(dialTone(Phone), 0)).


% 
% ectest/ec_reader_test_examples.e:4328
% [phone] % !HoldsAt(BusySignal(phone),0).
not(holds_at(busySignal(Phone), 0)).


% 
% ectest/ec_reader_test_examples.e:4329
% [phone1,phone2] % !HoldsAt(Ringing(phone1,phone2),0).
not(holds_at(ringing(Phone1, Phone2), 0)).


% 
% ectest/ec_reader_test_examples.e:4330
% [phone1,phone2] % !HoldsAt(Connected(phone1,phone2),0).
not(holds_at(connected(Phone1, Phone2), 0)).


% 
% ectest/ec_reader_test_examples.e:4331
% [phone] % !HoldsAt(Disconnected(phone),0).
not(holds_at(disconnected(Phone), 0)).


% 
% 
% completion Delta Happens
completion([delta, happens]).


% 
% range time 0 3
range(time, 0, 3).


% range offset 1 1
range(offset, 1, 1).


% ectest/ec_reader_test_examples.e:4337
% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter12/DefaultLocation.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:4363
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort object
sort(object).


% sort agent: object
subsort(agent, object).


% ectest/ec_reader_test_examples.e:4369
% sort device: object
subsort(device, object).


% sort tv: device
subsort(tv, device).


% sort room
sort(room).


% 
% agent Nathan
t(agent, nathan).


% tv TV
t(tv, tv).


% ectest/ec_reader_test_examples.e:4375
% room LivingRoom, Kitchen
t(room, livingRoom).


t(room, kitchen).


% 
% event TurnOn(agent,device)
event(turnOn(agent, device)).


% event Walk(agent,room,room)
event(walk(agent, room, room)).


% 
% fluent InRoom(object,room)
fluent(inRoom(object, room)).


% ectest/ec_reader_test_examples.e:4381
% fluent On(device)
fluent(on(device)).


% fluent PluggedIn(device)
fluent(pluggedIn(device)).


% fluent BrokenSwitch(device)
fluent(brokenSwitch(device)).


% 
% predicate Ab1(device,time)
predicate(ab1(device, time)).


% predicate Ab2(room,time)
predicate(ab2(room, time)).


% ectest/ec_reader_test_examples.e:4387
% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:4390
% [agent,room1,room2,time]% 
% Initiates(Walk(agent,room1,room2),InRoom(agent,room2),time).
initiates(walk(Agent, Room1, Room2), inRoom(Agent, Room2), Time).


% 
% 
% ectest/ec_reader_test_examples.e:4393
% [agent,room1,room2,time]% 
% room1!=% room2 ->
% Terminates(Walk(agent,room1,room2),InRoom(agent,room1),time).
Room1\=Room2 ->
	terminates(walk(Agent, Room1, Room2),
		   inRoom(Agent, Room1),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4397
% [agent,device,time]% 
% !Ab1(device,time) ->
% Initiates(TurnOn(agent,device),On(device),time).
not(ab1(Device, Time)) ->
	initiates(turnOn(Agent, Device),
		  on(Device),
		  Time).


% 
% 
% ; Delta
% 
% ectest/ec_reader_test_examples.e:4403
% [agent,room1,room2,time]% 
% Happens(Walk(agent,room1,room2),time) ->
% room1!=room2 &
% HoldsAt(InRoom(agent,room1),time).
happens(walk(Agent, Room1, Room2), Time) ->
	Room1\=Room2,
	holds_at(inRoom(Agent, Room1), Time).


% 
% 
% ectest/ec_reader_test_examples.e:4408
% [agent,device,time]% 
% Happens(TurnOn(agent,device),time) ->
% ectest/ec_reader_test_examples.e:4410
% {room}%  HoldsAt(InRoom(agent,room),time) &
%        HoldsAt(InRoom(device,room),time).
exists([Room],  (happens(turnOn(Agent, Device), Time)->holds_at(inRoom(Agent, Room), Time), holds_at(inRoom(Device, Room), Time))).


% 
% 
% ectest/ec_reader_test_examples.e:4413
% [event1,event2,time]% 
% Happens(event1,time) &
% Happens(event2,time) ->
% event1=event2.
happens(Event1, Time), happens(Event2, Time) ->
	Event1=Event2.


% 
% 
% ; Theta
% ectest/ec_reader_test_examples.e:4419
% 
% Theta:
directive(theta).


 
% ectest/ec_reader_test_examples.e:4420
% [device,time] % HoldsAt(BrokenSwitch(device),time) -> Ab1(device,time).
holds_at(brokenSwitch(Device), Time) ->
	ab1(Device, Time).


% 
% Theta:
directive(theta).


 
% ectest/ec_reader_test_examples.e:4421
% [device,time] % !HoldsAt(PluggedIn(device),time) -> Ab1(device,time).
not(holds_at(pluggedIn(Device), Time)) ->
	ab1(Device, Time).


% 
% 
% ; Psi
% 
% ectest/ec_reader_test_examples.e:4425
% [object,room1,room2,time]% 
% HoldsAt(InRoom(object,room1),time) &
% HoldsAt(InRoom(object,room2),time) ->
% room1=room2.
holds_at(inRoom(Object, Room1), Time), holds_at(inRoom(Object, Room2), Time) ->
	Room1=Room2.


% 
% 
% ; Gamma
% ectest/ec_reader_test_examples.e:4431
% 
% ectest/ec_reader_test_examples.e:4432
% [tv] % !HoldsAt(On(tv),0).
not(holds_at(on(Tv), 0)).


% 
% ectest/ec_reader_test_examples.e:4433
% [tv] % !HoldsAt(BrokenSwitch(tv),0).
not(holds_at(brokenSwitch(Tv), 0)).


% 
% ectest/ec_reader_test_examples.e:4434
% [tv] % HoldsAt(PluggedIn(tv),0).
holds_at(pluggedIn(Tv), 0).


% 
% 
% HoldsAt(InRoom(Nathan,Kitchen),0).
holds_at(inRoom(nathan, kitchen), 0).


% 
% 
% ectest/ec_reader_test_examples.e:4438
% [time]% 
% !Ab2(LivingRoom,time) ->
% ectest/ec_reader_test_examples.e:4440
% {tv}%  HoldsAt(InRoom(tv,LivingRoom),time).
exists([Tv],  (not(ab2(livingRoom, Time))->holds_at(inRoom(Tv, livingRoom), Time))).


% 
% 
% ; goal
% 
% ectest/ec_reader_test_examples.e:4444
% {tv} % Happens(TurnOn(Nathan,tv),1).
exists([Tv], happens(turnOn(nathan, Tv), 1)).


% 
% 
% ; for two TVs:
% ;[tv,time] !HoldsAt(InRoom(tv,Kitchen),time).
% ;[tv,time] {room} HoldsAt(InRoom(tv,room),time).
% 
% ectest/ec_reader_test_examples.e:4450
% completion Theta Ab1
completion([theta, ab1]).


% completion Theta Ab2
completion([theta, ab2]).


% 
% range time 0 2
range(time, 0, 2).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:4457
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter12/Device.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:4481
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% sort device
sort(device).


% ectest/ec_reader_test_examples.e:4487
% 
% agent Nathan
t(agent, nathan).


% device Device1, AntiqueDevice1
t(device, device1).


t(device, antiqueDevice1).


% 
% predicate Ab1(device,time)
predicate(ab1(device, time)).


% 
% ectest/ec_reader_test_examples.e:4493
% fluent On(device)
fluent(on(device)).


% fluent PluggedIn(device)
fluent(pluggedIn(device)).


% fluent BrokenSwitch(device)
fluent(brokenSwitch(device)).


% 
% event TurnOn(agent,device)
event(turnOn(agent, device)).


% 
% ; Sigma
% ectest/ec_reader_test_examples.e:4500
% 
% ectest/ec_reader_test_examples.e:4501
% [agent,device,time]% 
% !Ab1(device,time) ->
% Initiates(TurnOn(agent,device),On(device),time).
not(ab1(Device, Time)) ->
	initiates(turnOn(Agent, Device),
		  on(Device),
		  Time).


% 
% 
% ; Delta
% 
% ectest/ec_reader_test_examples.e:4507
% Happens(TurnOn(Nathan,Device1),0).
happens(turnOn(nathan, device1), 0).


% 
% 
% ; Theta
% 
% Theta:
directive(theta).


 
% ectest/ec_reader_test_examples.e:4511
% [device,time] % HoldsAt(BrokenSwitch(device),time) -> Ab1(device,time).
holds_at(brokenSwitch(Device), Time) ->
	ab1(Device, Time).


% 
% Theta:
directive(theta).


 
% ectest/ec_reader_test_examples.e:4512
% [device,time] % !HoldsAt(PluggedIn(device),time) -> Ab1(device,time).
not(holds_at(pluggedIn(Device), Time)) ->
	ab1(Device, Time).


% 
% Theta:
directive(theta).


 
% ectest/ec_reader_test_examples.e:4513
% [time] % Ab1(AntiqueDevice1,time).
ab1(antiqueDevice1, Time).


% 
% 
% ; Gamma
% 
% !HoldsAt(On(Device1),0).
not(holds_at(on(device1), 0)).


% 
% !HoldsAt(BrokenSwitch(Device1),0).
not(holds_at(brokenSwitch(device1), 0)).


% 
% ectest/ec_reader_test_examples.e:4519
% HoldsAt(PluggedIn(Device1),0).
holds_at(pluggedIn(device1), 0).


% 
% 
% ; added:
% ectest/ec_reader_test_examples.e:4522
% [time] % !HoldsAt(On(AntiqueDevice1),time).
not(holds_at(on(antiqueDevice1), Time)).


% 
% ectest/ec_reader_test_examples.e:4523
% [time] % HoldsAt(PluggedIn(AntiqueDevice1),time).
holds_at(pluggedIn(antiqueDevice1), Time).


% 
% 
% ; entailed:
% ; HoldsAt(On(Device1),1).
% 
% completion Theta Ab1
completion([theta, ab1]).


% ectest/ec_reader_test_examples.e:4529
% completion Happens
completion(happens).


% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:4535
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter12/ErraticDevice.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:4559
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% sort device
sort(device).


% ectest/ec_reader_test_examples.e:4565
% 
% agent Nathan
t(agent, nathan).


% device Device1
t(device, device1).


% 
% predicate Ab1(device,time)
predicate(ab1(device, time)).


% 
% ectest/ec_reader_test_examples.e:4571
% fluent On(device)
fluent(on(device)).


% fluent PluggedIn(device)
fluent(pluggedIn(device)).


% fluent BrokenSwitch(device)
fluent(brokenSwitch(device)).


% fluent Erratic(device)
fluent(erratic(device)).


% 
% fluent DeterminingFluent(device)
fluent(determiningFluent(device)).


% ectest/ec_reader_test_examples.e:4577
% noninertial DeterminingFluent
noninertial(determiningFluent).


% 
% event TurnOn(agent,device)
event(turnOn(agent, device)).


% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:4583
% [agent,device,time]% 
% !Ab1(device,time) ->
% Initiates(TurnOn(agent,device),On(device),time).
not(ab1(Device, Time)) ->
	initiates(turnOn(Agent, Device),
		  on(Device),
		  Time).


% 
% 
% ; Delta
% 
% ectest/ec_reader_test_examples.e:4589
% Happens(TurnOn(Nathan,Device1),0).
happens(turnOn(nathan, device1), 0).


% 
% 
% ; Theta
% 
% 
% Theta:
directive(theta).


 
% ectest/ec_reader_test_examples.e:4594
% [device,time] % HoldsAt(BrokenSwitch(device),time) -> Ab1(device,time).
holds_at(brokenSwitch(Device), Time) ->
	ab1(Device, Time).


% 
% Theta:
directive(theta).


 
% ectest/ec_reader_test_examples.e:4595
% [device,time]% 
% HoldsAt(Erratic(device),time) & HoldsAt(DeterminingFluent(device),time) ->
% Ab1(device,time).
holds_at(erratic(Device), Time), holds_at(determiningFluent(Device), Time) ->
	ab1(Device, Time).


% 
% 
% Theta:
directive(theta).


 
% ectest/ec_reader_test_examples.e:4599
% [device,time] % !HoldsAt(PluggedIn(device),time) -> Ab1(device,time).
not(holds_at(pluggedIn(Device), Time)) ->
	ab1(Device, Time).


% 
% 
% ; Gamma
% 
% !HoldsAt(On(Device1),0).
not(holds_at(on(device1), 0)).


% 
% !HoldsAt(BrokenSwitch(Device1),0).
not(holds_at(brokenSwitch(device1), 0)).


% 
% ectest/ec_reader_test_examples.e:4605
% HoldsAt(Erratic(Device1),0).
holds_at(erratic(device1), 0).


% 
% HoldsAt(PluggedIn(Device1),0).
holds_at(pluggedIn(device1), 0).


% 
% 
% ; added:
% HoldsAt(DeterminingFluent(Device1),1).
holds_at(determiningFluent(device1), 1).


% 
% 
% ectest/ec_reader_test_examples.e:4611
% completion Theta Ab1
completion([theta, ab1]).


% completion Happens
completion(happens).


% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:4618
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter12/DefaultEvent.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:4642
% 
% option modeldiff on
option(modeldiff, on).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% ectest/ec_reader_test_examples.e:4648
% sort agent
sort(agent).


% sort clock
sort(clock).


% 
% fluent Beeping(clock)
fluent(beeping(clock)).


% fluent AlarmTime(clock,time)
fluent(alarmTime(clock, time)).


% fluent AlarmOn(clock)
fluent(alarmOn(clock)).


% ectest/ec_reader_test_examples.e:4654
% 
% event SetAlarmTime(agent,clock,time)
event(setAlarmTime(agent, clock, time)).


% event StartBeeping(clock)
event(startBeeping(clock)).


% event TurnOnAlarm(agent,clock)
event(turnOnAlarm(agent, clock)).


% event TurnOffAlarm(agent,clock)
event(turnOffAlarm(agent, clock)).


% 
% ectest/ec_reader_test_examples.e:4660
% predicate Ab1(clock,time)
predicate(ab1(clock, time)).


% 
% agent Nathan
t(agent, nathan).


% clock Clock
t(clock, clock).


% 
% ; Sigma
% ectest/ec_reader_test_examples.e:4666
% 
% ectest/ec_reader_test_examples.e:4667
% [agent,clock,time1,time2,time]% 
% HoldsAt(AlarmTime(clock,time1),time) &
% time1!=time2 ->
% Initiates(SetAlarmTime(agent,clock,time2),AlarmTime(clock,time2),time).
holds_at(alarmTime(Clock, Time1), Time), Time1\=Time2 ->
	initiates(setAlarmTime(Agent, Clock, Time2),
		  alarmTime(Clock, Time2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:4672
% [agent,clock,time1,time2,time]% 
% HoldsAt(AlarmTime(clock,time1),time) &
% time1!=time2 ->
% Terminates(SetAlarmTime(agent,clock,time2),AlarmTime(clock,time1),time).
holds_at(alarmTime(Clock, Time1), Time), Time1\=Time2 ->
	terminates(setAlarmTime(Agent, Clock, Time2),
		   alarmTime(Clock, Time1),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:4677
% [agent,clock,time]% 
% Initiates(TurnOnAlarm(agent,clock),AlarmOn(clock),time).
initiates(turnOnAlarm(Agent, Clock), alarmOn(Clock), Time).


% 
% 
% ectest/ec_reader_test_examples.e:4680
% [agent,clock,time]% 
% Terminates(TurnOffAlarm(agent,clock),AlarmOn(clock),time).
terminates(turnOffAlarm(Agent, Clock), alarmOn(Clock), Time).


% 
% 
% ectest/ec_reader_test_examples.e:4683
% [clock,time]% 
% Initiates(StartBeeping(clock),Beeping(clock),time).
initiates(startBeeping(Clock), beeping(Clock), Time).


% 
% 
% ectest/ec_reader_test_examples.e:4686
% [agent,clock,time]% 
% Terminates(TurnOffAlarm(agent,clock),Beeping(clock),time).
terminates(turnOffAlarm(Agent, Clock), beeping(Clock), Time).


% 
% 
% ; Delta
% 
% ectest/ec_reader_test_examples.e:4691
% [clock,time]% 
% HoldsAt(AlarmTime(clock,time),time) &
% HoldsAt(AlarmOn(clock),time) &
% !Ab1(clock,time) ->
% Happens(StartBeeping(clock),time).
holds_at(alarmTime(Clock, Time), Time), holds_at(alarmOn(Clock), Time), not(ab1(Clock, Time)) ->
	happens(startBeeping(Clock), Time).


% 
% 
% ectest/ec_reader_test_examples.e:4697
% Happens(SetAlarmTime(Nathan,Clock,2),0).
happens(setAlarmTime(nathan, clock, 2), 0).


% 
% Happens(TurnOnAlarm(Nathan,Clock),1).
happens(turnOnAlarm(nathan, clock), 1).


% 
% 
% ; Psi
% 
% ectest/ec_reader_test_examples.e:4702
% [clock,time1,time2,time]% 
% HoldsAt(AlarmTime(clock,time1),time) &
% HoldsAt(AlarmTime(clock,time2),time) ->
% time1=time2.
holds_at(alarmTime(Clock, Time1), Time), holds_at(alarmTime(Clock, Time2), Time) ->
	Time1=Time2.


% 
% 
% ; Gamma
% ectest/ec_reader_test_examples.e:4708
% 
% !HoldsAt(AlarmOn(Clock),0).
not(holds_at(alarmOn(clock), 0)).


% 
% !HoldsAt(Beeping(Clock),0).
not(holds_at(beeping(clock), 0)).


% 
% HoldsAt(AlarmTime(Clock,3),0).
holds_at(alarmTime(clock, 3), 0).


% 
% 
% completion Happens
completion(happens).


% ectest/ec_reader_test_examples.e:4714
% completion Theta Ab1
completion([theta, ab1]).


% 
% range time 0 3
range(time, 0, 3).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:4720
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter12/MethodD.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; Method (D)
% ;
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:4746
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort object
sort(object).


% 
% ectest/ec_reader_test_examples.e:4752
% object A,B
t(object, a).


t(object, b).


% 
% fluent P(object)
fluent(p(object)).


% fluent Q(object)
fluent(q(object)).


% fluent R(object)
fluent(r(object)).


% 
% ectest/ec_reader_test_examples.e:4758
% predicate Ab1(object,time)
predicate(ab1(object, time)).


% predicate Ab2(object,time)
predicate(ab2(object, time)).


% 
% ectest/ec_reader_test_examples.e:4761
% [object,time]% 
% HoldsAt(P(object),time) & !Ab1(object,time) ->
% HoldsAt(Q(object),time).
holds_at(p(Object), Time), not(ab1(Object, Time)) ->
	holds_at(q(Object), Time).


% 
% 
% ectest/ec_reader_test_examples.e:4765
% [object,time]% 
% HoldsAt(R(object),time) & !Ab2(object,time) ->
% !HoldsAt(Q(object),time).
holds_at(r(Object), Time), not(ab2(Object, Time)) ->
	not(holds_at(q(Object), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:4769
% [object,time]% 
% HoldsAt(R(object),time) -> HoldsAt(P(object),time).
holds_at(r(Object), Time) ->
	holds_at(p(Object), Time).


% 
% 
% HoldsAt(R(A),0).
holds_at(r(a), 0).


% 
% HoldsAt(P(B),0).
holds_at(p(b), 0).


% 
% !HoldsAt(R(B),0).
not(holds_at(r(b), 0)).


% 
% ectest/ec_reader_test_examples.e:4775
% 
% Theta:
directive(theta).


 % 
% ectest/ec_reader_test_examples.e:4777
% [object,time]% 
% HoldsAt(R(object),time) -> Ab1(object,time).
holds_at(r(Object), Time) ->
	ab1(Object, Time).


% 
% 
% range time 0 0
range(time, 0, 0).


% range offset 1 1
range(offset, 1, 1).


% 
% ectest/ec_reader_test_examples.e:4783
% completion Theta Ab1
completion([theta, ab1]).


% completion Theta Ab2
completion([theta, ab2]).


% 
% ; End of file.
% 
% 
% ectest/ec_reader_test_examples.e:4789
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter12/BrokenDevice.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:4811
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% sort device
sort(device).


% ectest/ec_reader_test_examples.e:4817
% 
% agent Nathan
t(agent, nathan).


% device Device1
t(device, device1).


% 
% predicate Ab1(device,time)
predicate(ab1(device, time)).


% 
% ectest/ec_reader_test_examples.e:4823
% fluent On(device)
fluent(on(device)).


% fluent PluggedIn(device)
fluent(pluggedIn(device)).


% fluent BrokenSwitch(device)
fluent(brokenSwitch(device)).


% 
% event TurnOn(agent,device)
event(turnOn(agent, device)).


% 
% ; Sigma
% ectest/ec_reader_test_examples.e:4830
% 
% ectest/ec_reader_test_examples.e:4831
% [agent,device,time]% 
% !Ab1(device,time) ->
% Initiates(TurnOn(agent,device),On(device),time).
not(ab1(Device, Time)) ->
	initiates(turnOn(Agent, Device),
		  on(Device),
		  Time).


% 
% 
% ; Delta
% 
% ectest/ec_reader_test_examples.e:4837
% Happens(TurnOn(Nathan,Device1),0).
happens(turnOn(nathan, device1), 0).


% 
% 
% ; Theta
% 
% Theta:
directive(theta).


 
% ectest/ec_reader_test_examples.e:4841
% [device,time] % HoldsAt(BrokenSwitch(device),time) -> Ab1(device,time).
holds_at(brokenSwitch(Device), Time) ->
	ab1(Device, Time).


% 
% Theta:
directive(theta).


 
% ectest/ec_reader_test_examples.e:4842
% [device,time] % !HoldsAt(PluggedIn(device),time) -> Ab1(device,time).
not(holds_at(pluggedIn(Device), Time)) ->
	ab1(Device, Time).


% 
% 
% ; Gamma
% 
% !HoldsAt(On(Device1),0).
not(holds_at(on(device1), 0)).


% 
% HoldsAt(BrokenSwitch(Device1),0).
holds_at(brokenSwitch(device1), 0).


% 
% ectest/ec_reader_test_examples.e:4848
% 
% ; added:
% HoldsAt(PluggedIn(Device1),0).
holds_at(pluggedIn(device1), 0).


% 
% 
% ; entailed:
% ; !HoldsAt(On(Device1),1).
% ectest/ec_reader_test_examples.e:4854
% 
% completion Theta Ab1
completion([theta, ab1]).


% completion Happens
completion(happens).


% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% ectest/ec_reader_test_examples.e:4860
% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter12/MethodB.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; Method (D)
% ;
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:4888
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort object
sort(object).


% 
% ectest/ec_reader_test_examples.e:4894
% object A,B
t(object, a).


t(object, b).


% 
% fluent P(object)
fluent(p(object)).


% fluent Q(object)
fluent(q(object)).


% predicate Ab(object,time)
predicate(ab(object, time)).


% 
% ectest/ec_reader_test_examples.e:4900
% [object,time]% 
% HoldsAt(P(object),time) & !Ab(object,time) ->
% HoldsAt(Q(object),time).
holds_at(p(Object), Time), not(ab(Object, Time)) ->
	holds_at(q(Object), Time).


% 
% 
% HoldsAt(P(A),0).
holds_at(p(a), 0).


% 
% HoldsAt(P(B),0).
holds_at(p(b), 0).


% 
% ectest/ec_reader_test_examples.e:4906
% 
% Theta:
directive(theta).


 % Ab(A,0).
ab(a, 0).


% 
% 
% range time 0 0
range(time, 0, 0).


% range offset 1 1
range(offset, 1, 1).


% 
% ectest/ec_reader_test_examples.e:4912
% completion Theta Ab
completion([theta, ab]).


% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter13/ModelFinding.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:4939
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% 
% ectest/ec_reader_test_examples.e:4945
% fluent Awake(agent)
fluent(awake(agent)).


% event WakeUp(agent)
event(wakeUp(agent)).


% 
% ectest/ec_reader_test_examples.e:4948
% [agent,time] % Initiates(WakeUp(agent),Awake(agent),time).
initiates(wakeUp(Agent), awake(Agent), Time).


% 
% ectest/ec_reader_test_examples.e:4949
% [agent,time] % Happens(WakeUp(agent),time) -> !HoldsAt(Awake(agent),time).
happens(wakeUp(Agent), Time) ->
	not(holds_at(awake(Agent), Time)).


% 
% 
% agent James
t(agent, james).


% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% ectest/ec_reader_test_examples.e:4955
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter13/Postdiction.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:4979
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% 
% ectest/ec_reader_test_examples.e:4985
% fluent Awake(agent)
fluent(awake(agent)).


% event WakeUp(agent)
event(wakeUp(agent)).


% 
% ectest/ec_reader_test_examples.e:4988
% [agent,time] % Initiates(WakeUp(agent),Awake(agent),time).
initiates(wakeUp(Agent), awake(Agent), Time).


% 
% ectest/ec_reader_test_examples.e:4989
% [agent,time] % Happens(WakeUp(agent),time) -> !HoldsAt(Awake(agent),time).
happens(wakeUp(Agent), Time) ->
	not(holds_at(awake(Agent), Time)).


% 
% 
% agent James
t(agent, james).


% Delta:
directive(delta).


 % Happens(WakeUp(James),0).
happens(wakeUp(james), 0).


% 
% HoldsAt(Awake(James),1).
holds_at(awake(james), 1).


% 
% 
% ectest/ec_reader_test_examples.e:4995
% completion Delta Happens
completion([delta, happens]).


% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% 
% ectest/ec_reader_test_examples.e:5001
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter13/Deduction2.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:5023
% 
% option timediff off
option(timediff, off).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% ectest/ec_reader_test_examples.e:5029
% sort agent
sort(agent).


% 
% fluent Awake(agent)
fluent(awake(agent)).


% event WakeUp(agent)
event(wakeUp(agent)).


% 
% ectest/ec_reader_test_examples.e:5034
% [agent,time] % Initiates(WakeUp(agent),Awake(agent),time).
initiates(wakeUp(Agent), awake(Agent), Time).


% 
% 
% agent James
t(agent, james).


% !HoldsAt(Awake(James),0).
not(holds_at(awake(james), 0)).


% 
% Delta:
directive(delta).


 % Happens(WakeUp(James),0).
happens(wakeUp(james), 0).


% 
% 
% ectest/ec_reader_test_examples.e:5040
% completion Delta Happens
completion([delta, happens]).


% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% 
% ectest/ec_reader_test_examples.e:5046
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter13/Deduction1.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:5068
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% 
% ectest/ec_reader_test_examples.e:5074
% fluent Awake(agent)
fluent(awake(agent)).


% event WakeUp(agent)
event(wakeUp(agent)).


% 
% ectest/ec_reader_test_examples.e:5077
% [agent,time] % Initiates(WakeUp(agent),Awake(agent),time).
initiates(wakeUp(Agent), awake(Agent), Time).


% 
% 
% agent James
t(agent, james).


% !HoldsAt(Awake(James),0).
not(holds_at(awake(james), 0)).


% 
% Delta:
directive(delta).


 % Happens(WakeUp(James),0).
happens(wakeUp(james), 0).


% 
% 
% ectest/ec_reader_test_examples.e:5083
% completion Delta Happens
completion([delta, happens]).


% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% 
% ectest/ec_reader_test_examples.e:5089
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter13/Abduction.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:5111
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% 
% ectest/ec_reader_test_examples.e:5117
% fluent Awake(agent)
fluent(awake(agent)).


% event WakeUp(agent)
event(wakeUp(agent)).


% 
% ectest/ec_reader_test_examples.e:5120
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
% ectest/ec_reader_test_examples.e:5126
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter4/AlarmClock.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:5152
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% sort clock
sort(clock).


% ectest/ec_reader_test_examples.e:5158
% 
% fluent Beeping(clock)
fluent(beeping(clock)).


% fluent AlarmTime(clock,time)
fluent(alarmTime(clock, time)).


% fluent AlarmOn(clock)
fluent(alarmOn(clock)).


% 
% event SetAlarmTime(agent,clock,time)
event(setAlarmTime(agent, clock, time)).


% ectest/ec_reader_test_examples.e:5164
% event StartBeeping(clock)
event(startBeeping(clock)).


% event TurnOnAlarm(agent,clock)
event(turnOnAlarm(agent, clock)).


% event TurnOffAlarm(agent,clock)
event(turnOffAlarm(agent, clock)).


% 
% agent Nathan
t(agent, nathan).


% clock Clock
t(clock, clock).


% ectest/ec_reader_test_examples.e:5170
% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:5173
% [agent,clock,time1,time2,time]% 
% HoldsAt(AlarmTime(clock,time1),time) &
% time1!=time2 ->
% Initiates(SetAlarmTime(agent,clock,time2),AlarmTime(clock,time2),time).
holds_at(alarmTime(Clock, Time1), Time), Time1\=Time2 ->
	initiates(setAlarmTime(Agent, Clock, Time2),
		  alarmTime(Clock, Time2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:5178
% [agent,clock,time1,time2,time]% 
% HoldsAt(AlarmTime(clock,time1),time) &
% time1!=time2 ->
% Terminates(SetAlarmTime(agent,clock,time2),AlarmTime(clock,time1),time).
holds_at(alarmTime(Clock, Time1), Time), Time1\=Time2 ->
	terminates(setAlarmTime(Agent, Clock, Time2),
		   alarmTime(Clock, Time1),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:5183
% [agent,clock,time]% 
% Initiates(TurnOnAlarm(agent,clock),AlarmOn(clock),time).
initiates(turnOnAlarm(Agent, Clock), alarmOn(Clock), Time).


% 
% 
% ectest/ec_reader_test_examples.e:5186
% [agent,clock,time]% 
% Terminates(TurnOffAlarm(agent,clock),AlarmOn(clock),time).
terminates(turnOffAlarm(Agent, Clock), alarmOn(Clock), Time).


% 
% 
% ectest/ec_reader_test_examples.e:5189
% [clock,time]% 
% Initiates(StartBeeping(clock),Beeping(clock),time).
initiates(startBeeping(Clock), beeping(Clock), Time).


% 
% 
% ectest/ec_reader_test_examples.e:5192
% [agent,clock,time]% 
% Terminates(TurnOffAlarm(agent,clock),Beeping(clock),time).
terminates(turnOffAlarm(Agent, Clock), beeping(Clock), Time).


% 
% 
% ; Delta
% 
% ectest/ec_reader_test_examples.e:5197
% [clock,time]% 
% HoldsAt(AlarmTime(clock,time),time) &
% HoldsAt(AlarmOn(clock),time) ->
% Happens(StartBeeping(clock),time).
holds_at(alarmTime(Clock, Time), Time), holds_at(alarmOn(Clock), Time) ->
	happens(startBeeping(Clock), Time).


% 
% 
% Happens(SetAlarmTime(Nathan,Clock,2),0).
happens(setAlarmTime(nathan, clock, 2), 0).


% 
% ectest/ec_reader_test_examples.e:5203
% Happens(TurnOnAlarm(Nathan,Clock),1).
happens(turnOnAlarm(nathan, clock), 1).


% 
% 
% ; Psi
% 
% ectest/ec_reader_test_examples.e:5207
% [clock,time1,time2,time]% 
% HoldsAt(AlarmTime(clock,time1),time) &
% HoldsAt(AlarmTime(clock,time2),time) ->
% time1=time2.
holds_at(alarmTime(Clock, Time1), Time), holds_at(alarmTime(Clock, Time2), Time) ->
	Time1=Time2.


% 
% 
% ; Gamma
% ectest/ec_reader_test_examples.e:5213
% 
% !HoldsAt(AlarmOn(Clock),0).
not(holds_at(alarmOn(clock), 0)).


% 
% !HoldsAt(Beeping(Clock),0).
not(holds_at(beeping(clock), 0)).


% 
% HoldsAt(AlarmTime(Clock,3),0).
holds_at(alarmTime(clock, 3), 0).


% 
% 
% completion Happens
completion(happens).


% ectest/ec_reader_test_examples.e:5219
% 
% range time 0 3
range(time, 0, 3).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% ectest/ec_reader_test_examples.e:5225
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter4/BankAccountServiceFee.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:5248
% 
% option modeldiff on
option(modeldiff, on).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% ectest/ec_reader_test_examples.e:5254
% sort account
sort(account).


% sort value: integer
subsort(value, integer).


% 
% account Account1, Account2
t(account, account1).


t(account, account2).


% 
% predicate EndOfMonth(time)
predicate(endOfMonth(time)).


% ectest/ec_reader_test_examples.e:5260
% function ServiceFee(account): value
function(serviceFee(account), value).


% function MinimumBalance(account): value
function(minimumBalance(account), value).


% 
% fluent ServiceFeeCharged(account)
fluent(serviceFeeCharged(account)).


% fluent Balance(account,value)
fluent(balance(account, value)).


% 
% ectest/ec_reader_test_examples.e:5266
% event Transfer(account,account,value)
event(transfer(account, account, value)).


% event MonthlyReset(account)
event(monthlyReset(account)).


% event ChargeServiceFee(account)
event(chargeServiceFee(account)).


% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:5272
% [account1,account2,value1,value2,value3,value4,time]% 
% HoldsAt(Balance(account1,value1),time) &
% HoldsAt(Balance(account2,value2),time) &
% value3>0 &
% value1>=value3 &
% value4=(value2+value3) ->
% Initiates(Transfer(account1,account2,value3),Balance(account2,value4),time).
holds_at(balance(Account1, Value1), Time), holds_at(balance(Account2, Value2), Time), Value3>0, Value1>=Value3, Value4=Value2+Value3 ->
	initiates(transfer(Account1, Account2, Value3),
		  balance(Account2, Value4),
		  Time).


% ectest/ec_reader_test_examples.e:5278
% 
% 
% ectest/ec_reader_test_examples.e:5280
% [account1,account2,value1,value2,value3,time]% 
% HoldsAt(Balance(account1,value1),time) &
% HoldsAt(Balance(account2,value2),time) &
% value3>0 &
% value1>=value3 ->
% Terminates(Transfer(account1,account2,value3),Balance(account2,value2),time).
holds_at(balance(Account1, Value1), Time), holds_at(balance(Account2, Value2), Time), Value3>0, Value1>=Value3 ->
	terminates(transfer(Account1, Account2, Value3),
		   balance(Account2, Value2),
		   Time).


% 
% ectest/ec_reader_test_examples.e:5286
% 
% ectest/ec_reader_test_examples.e:5287
% [account1,account2,value1,value2,value3,value4,time]% 
% HoldsAt(Balance(account1,value1),time) &
% HoldsAt(Balance(account2,value2),time) &
% value3>0 &
% value1>=value3 &
% value4=(value1-value3) ->
% Initiates(Transfer(account1,account2,value3),Balance(account1,value4),time).
holds_at(balance(Account1, Value1), Time), holds_at(balance(Account2, Value2), Time), Value3>0, Value1>=Value3, Value4=Value1-Value3 ->
	initiates(transfer(Account1, Account2, Value3),
		  balance(Account1, Value4),
		  Time).


% ectest/ec_reader_test_examples.e:5293
% 
% 
% ectest/ec_reader_test_examples.e:5295
% [account1,account2,value1,value2,value3,time]% 
% HoldsAt(Balance(account1,value1),time) &
% HoldsAt(Balance(account2,value2),time) &
% value3>0 &
% value1>=value3 ->
% Terminates(Transfer(account1,account2,value3),Balance(account1,value1),time).
holds_at(balance(Account1, Value1), Time), holds_at(balance(Account2, Value2), Time), Value3>0, Value1>=Value3 ->
	terminates(transfer(Account1, Account2, Value3),
		   balance(Account1, Value1),
		   Time).


% 
% ectest/ec_reader_test_examples.e:5301
% 
% ectest/ec_reader_test_examples.e:5302
% [account,time]% 
% Initiates(ChargeServiceFee(account),ServiceFeeCharged(account),time).
initiates(chargeServiceFee(Account), serviceFeeCharged(Account), Time).


% 
% 
% ectest/ec_reader_test_examples.e:5305
% [account,time]% 
% Terminates(MonthlyReset(account),ServiceFeeCharged(account),time).
terminates(monthlyReset(Account), serviceFeeCharged(Account), Time).


% 
% 
% ectest/ec_reader_test_examples.e:5308
% [account,value1,value2,time]% 
% HoldsAt(Balance(account,value1),time) &
% value2 = (value1-ServiceFee(account)) ->
% Initiates(ChargeServiceFee(account),
%           Balance(account,value2),
%           time).
holds_at(balance(Account, Value1), Time), Value2=Value1-serviceFee(Account) ->
	initiates(chargeServiceFee(Account),
		  balance(Account, Value2),
		  Time).


% 
% ectest/ec_reader_test_examples.e:5314
% 
% ectest/ec_reader_test_examples.e:5315
% [account,value,time]% 
% HoldsAt(Balance(account,value),time) ->
% Terminates(ChargeServiceFee(account),Balance(account,value),time).
holds_at(balance(Account, Value), Time) ->
	terminates(chargeServiceFee(Account),
		   balance(Account, Value),
		   Time).


% 
% 
% ; Delta
% 
% ectest/ec_reader_test_examples.e:5321
% [account,value,time]% 
% HoldsAt(Balance(account,value),time) &
% value<MinimumBalance(account) &
% !HoldsAt(ServiceFeeCharged(account),time) ->
% Happens(ChargeServiceFee(account),time).
holds_at(balance(Account, Value), Time), Value<minimumBalance(Account), not(holds_at(serviceFeeCharged(Account), Time)) ->
	happens(chargeServiceFee(Account), Time).


% 
% 
% ectest/ec_reader_test_examples.e:5327
% [account,time]% 
% EndOfMonth(time) ->
% Happens(MonthlyReset(account),time).
endOfMonth(Time) ->
	happens(monthlyReset(Account), Time).


% 
% 
% Happens(Transfer(Account1,Account2,1),0).
happens(transfer(account1, account2, 1), 0).


% 
% Happens(Transfer(Account1,Account2,1),0).
happens(transfer(account1, account2, 1), 0).


% 
% ectest/ec_reader_test_examples.e:5333
% 
% ; Psi
% 
% ectest/ec_reader_test_examples.e:5336
% [account,value1,value2,time]% 
% HoldsAt(Balance(account,value1),time) &
% HoldsAt(Balance(account,value2),time) ->
% value1=value2.
holds_at(balance(Account, Value1), Time), holds_at(balance(Account, Value2), Time) ->
	Value1=Value2.


% 
% 
% ; Gamma
% ectest/ec_reader_test_examples.e:5342
% 
% !HoldsAt(ServiceFeeCharged(Account1),0).
not(holds_at(serviceFeeCharged(account1), 0)).


% 
% !HoldsAt(ServiceFeeCharged(Account2),0).
not(holds_at(serviceFeeCharged(account2), 0)).


% 
% HoldsAt(Balance(Account1,3),0).
holds_at(balance(account1, 3), 0).


% 
% HoldsAt(Balance(Account2,1),0).
holds_at(balance(account2, 1), 0).


% 
% MinimumBalance(Account1)=3.
minimumBalance(account1)=3.


% 
% ectest/ec_reader_test_examples.e:5348
% MinimumBalance(Account2)=1.
minimumBalance(account2)=1.


% 
% ServiceFee(Account1)=1.
serviceFee(account1)=1.


% 
% ServiceFee(Account2)=1.
serviceFee(account2)=1.


% 
% ectest/ec_reader_test_examples.e:5351
% [time] % !EndOfMonth(time).
not(endOfMonth(Time)).


% 
% 
% completion Happens
completion(happens).


% 
% range time 0 3
range(time, 0, 3).


% range value 1 3
range(value, 1, 3).


% ectest/ec_reader_test_examples.e:5357
% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Exercises/Counter.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{DeneckerDupreBelleghem:1998,
% ;   author = "Marc Denecker and Daniele Theseider Dupr\'{e} and Kristof Van Belleghem",
% ;   year = "1998",
% ;   title = "An inductive definition approach to ramifications",
% ;   journal = "Link{\"{o}}ping Electronic Articles in Computer and Information Science",
% ;   volume = "3",
% ;   number = "007",
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
% ectest/ec_reader_test_examples.e:5393
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort counter
sort(counter).


% counter Counter1
t(counter, counter1).


% ectest/ec_reader_test_examples.e:5399
% 
% event FalseToTrue(counter)
event(falseToTrue(counter)).


% event TrueToFalse(counter)
event(trueToFalse(counter)).


% 
% fluent Count(counter,integer)
fluent(count(counter, integer)).


% fluent True(counter)
fluent(true(counter)).


% ectest/ec_reader_test_examples.e:5405
% fluent InputLine(counter)
fluent(inputLine(counter)).


% noninertial InputLine
noninertial(inputLine).


% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:5408
% [counter,time]% 
% !HoldsAt(True(counter),time) &
% HoldsAt(InputLine(counter),time) ->
% Happens(FalseToTrue(counter),time).
not(holds_at(true(Counter), Time)), holds_at(inputLine(Counter), Time) ->
	happens(falseToTrue(Counter), Time).


% 
% 
% Delta:
directive(delta).


 
% ectest/ec_reader_test_examples.e:5413
% [counter,time]% 
% HoldsAt(True(counter),time) &
% !HoldsAt(InputLine(counter),time) ->
% Happens(TrueToFalse(counter),time).
holds_at(true(Counter), Time), not(holds_at(inputLine(Counter), Time)) ->
	happens(trueToFalse(Counter), Time).


% 
% 
% ectest/ec_reader_test_examples.e:5418
% [counter,time] % Initiates(FalseToTrue(counter),True(counter),time).
initiates(falseToTrue(Counter), true(Counter), Time).


% 
% 
% ectest/ec_reader_test_examples.e:5420
% [counter,time] % Terminates(TrueToFalse(counter),True(counter),time).
terminates(trueToFalse(Counter), true(Counter), Time).


% 
% 
% ectest/ec_reader_test_examples.e:5422
% [counter,integer1,integer2,time]% 
% HoldsAt(Count(counter,integer1),time) &
% (integer2 = (integer1 + 1)) ->
% Initiates(FalseToTrue(counter),Count(counter,integer2),time).
holds_at(count(Counter, Integer1), Time), Integer2=Integer1+1 ->
	initiates(falseToTrue(Counter),
		  count(Counter, Integer2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:5427
% [counter,integer,time]% 
% HoldsAt(Count(counter,integer),time) ->
% Terminates(FalseToTrue(counter),Count(counter,integer),time).
holds_at(count(Counter, Integer), Time) ->
	terminates(falseToTrue(Counter),
		   count(Counter, Integer),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:5431
% [counter,integer1,integer2,time]% 
% HoldsAt(Count(counter,integer1),time) &
% HoldsAt(Count(counter,integer2),time) ->
% integer1 = integer2.
holds_at(count(Counter, Integer1), Time), holds_at(count(Counter, Integer2), Time) ->
	Integer1=Integer2.


% 
% 
% !HoldsAt(True(Counter1),0).
not(holds_at(true(counter1), 0)).


% 
% ectest/ec_reader_test_examples.e:5437
% !HoldsAt(InputLine(Counter1),0).
not(holds_at(inputLine(counter1), 0)).


% 
% HoldsAt(InputLine(Counter1),1).
holds_at(inputLine(counter1), 1).


% 
% HoldsAt(InputLine(Counter1),2).
holds_at(inputLine(counter1), 2).


% 
% HoldsAt(InputLine(Counter1),3).
holds_at(inputLine(counter1), 3).


% 
% !HoldsAt(InputLine(Counter1),4).
not(holds_at(inputLine(counter1), 4)).


% 
% !HoldsAt(InputLine(Counter1),5).
not(holds_at(inputLine(counter1), 5)).


% 
% ectest/ec_reader_test_examples.e:5443
% !HoldsAt(InputLine(Counter1),6).
not(holds_at(inputLine(counter1), 6)).


% 
% HoldsAt(InputLine(Counter1),7).
holds_at(inputLine(counter1), 7).


% 
% HoldsAt(InputLine(Counter1),8).
holds_at(inputLine(counter1), 8).


% 
% HoldsAt(InputLine(Counter1),9).
holds_at(inputLine(counter1), 9).


% 
% 
% HoldsAt(Count(Counter1,0),0).
holds_at(count(counter1, 0), 0).


% 
% ectest/ec_reader_test_examples.e:5449
% 
% completion Happens
completion(happens).


% 
% range integer 0 6
range(integer, 0, 6).


% range time 0 10
range(time, 0, 10).


% range offset 1 1
range(offset, 1, 1).


% ectest/ec_reader_test_examples.e:5455
% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Exercises/TeacherTells.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:5481
% 
% option modeldiff on
option(modeldiff, on).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% ectest/ec_reader_test_examples.e:5487
% sort agent
sort(agent).


% sort room
sort(room).


% sort fact
sort(fact).


% 
% agent Teacher, Student
t(agent, teacher).


t(agent, student).


% room Kitchen, Classroom
t(room, kitchen).


t(room, classroom).


% ectest/ec_reader_test_examples.e:5493
% fact Fact1, Fact2
t(fact, fact1).


t(fact, fact2).


% 
% fluent InRoom(agent,room)
fluent(inRoom(agent, room)).


% fluent ListeningTo(agent,agent)
fluent(listeningTo(agent, agent)).


% fluent Know(agent,fact)
fluent(know(agent, fact)).


% 
% ectest/ec_reader_test_examples.e:5499
% event Tell(agent,agent,fact)
event(tell(agent, agent, fact)).


% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:5503
% [agent1,agent2,fact,time]% 
% (
% ectest/ec_reader_test_examples.e:5504
% {room} HoldsAt(InRoom(agent1,room),time) &
%         HoldsAt(InRoom(agent2,room),time)) &
% HoldsAt(ListeningTo(agent2,agent1),time) ->
% Initiates(Tell(agent1,agent2,fact),Know(agent2,fact),time).
exists([Room],  ((holds_at(inRoom(Agent1, Room), Time), holds_at(inRoom(Agent2, Room), Time)), holds_at(listeningTo(Agent2, Agent1), Time)->initiates(tell(Agent1, Agent2, Fact), know(Agent2, Fact), Time))).


% 
% 
% ; Delta
% ectest/ec_reader_test_examples.e:5510
% 
% Happens(Tell(Teacher,Student,Fact1),0).
happens(tell(teacher, student, fact1), 0).


% 
% 
% ; Psi
% 
% ectest/ec_reader_test_examples.e:5515
% [agent,room1,room2,time]% 
% HoldsAt(InRoom(agent,room1),time) &
% HoldsAt(InRoom(agent,room2),time) ->
% room1 = room2.
holds_at(inRoom(Agent, Room1), Time), holds_at(inRoom(Agent, Room2), Time) ->
	Room1=Room2.


% 
% 
% ; Gamma
% ectest/ec_reader_test_examples.e:5521
% 
% ectest/ec_reader_test_examples.e:5522
% [agent,fact] % !HoldsAt(Know(agent,fact),0).
not(holds_at(know(Agent, Fact), 0)).


% 
% ectest/ec_reader_test_examples.e:5523
% [agent1,agent2] % HoldsAt(ListeningTo(agent1,agent2),0).
holds_at(listeningTo(Agent1, Agent2), 0).


% 
% ectest/ec_reader_test_examples.e:5524
% [agent] % HoldsAt(InRoom(agent,Classroom),0).
holds_at(inRoom(Agent, classroom), 0).


% 
% 
% completion Happens
completion(happens).


% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% ectest/ec_reader_test_examples.e:5530
% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Exercises/MixingPaints.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:5556
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort palette
sort(palette).


% sort color
sort(color).


% ectest/ec_reader_test_examples.e:5562
% 
% palette Palette1
t(palette, palette1).


% color Red, Yellow, Blue, Green
t(color, red).


t(color, yellow).


t(color, blue).


t(color, green).


% 
% event PlaceOnPalette(palette,color)
event(placeOnPalette(palette, color)).


% fluent OnPalette(palette,color)
fluent(onPalette(palette, color)).


% ectest/ec_reader_test_examples.e:5568
% 
% ectest/ec_reader_test_examples.e:5569
% [palette,color,time]% 
% !Happens(PlaceOnPalette(palette,Yellow),time) |
% !Happens(PlaceOnPalette(palette,Blue),time) ->
% Initiates(PlaceOnPalette(palette,color),OnPalette(palette,color),time).
(   not(happens(placeOnPalette(Palette, yellow), Time))
;   (   (   not(happens(placeOnPalette(Palette, yellow), Time))
;   not(happens(placeOnPalette(Palette, blue), Time))
->  initiates(placeOnPalette(Palette, Color),
	      onPalette(Palette, Color),
	      Time)
).


% 
% 
% ectest/ec_reader_test_examples.e:5574
% [palette,color1,color2,time]% 
% Happens(PlaceOnPalette(palette,Yellow),time) &
% color1 = Blue &
% color2 = Green ->
% Initiates(PlaceOnPalette(palette,color1),OnPalette(palette,color2),time).
happens(placeOnPalette(Palette, yellow), Time), Color1=blue, Color2=green ->
	initiates(placeOnPalette(Palette, Color1),
		  onPalette(Palette, Color2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:5580
% [palette,color1,color2,time]% 
% !(Happens(PlaceOnPalette(palette,Yellow),time) &
%   Happens(PlaceOnPalette(palette,Blue),time)) &
% HoldsAt(OnPalette(palette,color1),time) &
% color1 != color2 ->
% Terminates(PlaceOnPalette(palette,color2),OnPalette(palette,color1),time).
not((happens(placeOnPalette(Palette, yellow), Time), happens(placeOnPalette(Palette, blue), Time))), holds_at(onPalette(Palette, Color1), Time), Color1\=Color2 ->
	terminates(placeOnPalette(Palette, Color2),
		   onPalette(Palette, Color1),
		   Time).


% 
% ectest/ec_reader_test_examples.e:5586
% 
% ectest/ec_reader_test_examples.e:5587
% [palette,color1,color2,time]% 
% Happens(PlaceOnPalette(palette,Yellow),time) &
% HoldsAt(OnPalette(palette,color2),time) &
% color1 = Blue &
% color2 != Green ->
% Terminates(PlaceOnPalette(palette,color1),OnPalette(palette,color2),time).
happens(placeOnPalette(Palette, yellow), Time), holds_at(onPalette(Palette, Color2), Time), Color1=blue, Color2\=green ->
	terminates(placeOnPalette(Palette, Color1),
		   onPalette(Palette, Color2),
		   Time).


% 
% ectest/ec_reader_test_examples.e:5593
% 
% ; state constraint
% 
% ectest/ec_reader_test_examples.e:5596
% [palette,color1,color2,time]% 
% HoldsAt(OnPalette(palette,color1),time) &
% HoldsAt(OnPalette(palette,color2),time) ->
% color1 = color2.
holds_at(onPalette(Palette, Color1), Time), holds_at(onPalette(Palette, Color2), Time) ->
	Color1=Color2.


% 
% 
% ; (1) place green over red
% ectest/ec_reader_test_examples.e:5602
% HoldsAt(OnPalette(Palette1,Red),0).
holds_at(onPalette(palette1, red), 0).


% 
% Delta:
directive(delta).


 % Happens(PlaceOnPalette(Palette1,Green),0).
happens(placeOnPalette(palette1, green), 0).


% 
% 
% ; (2) place yellow+blue over green
% Delta:
directive(delta).


 % Happens(PlaceOnPalette(Palette1,Yellow),1).
happens(placeOnPalette(palette1, yellow), 1).


% 
% Delta:
directive(delta).


 % Happens(PlaceOnPalette(Palette1,Blue),1).
happens(placeOnPalette(palette1, blue), 1).


% 
% ectest/ec_reader_test_examples.e:5608
% 
% ; (3) place yellow
% Delta:
directive(delta).


 % Happens(PlaceOnPalette(Palette1,Yellow),2).
happens(placeOnPalette(palette1, yellow), 2).


% 
% 
% ; (4) place blue
% Delta:
directive(delta).


 % Happens(PlaceOnPalette(Palette1,Blue),3).
happens(placeOnPalette(palette1, blue), 3).


% 
% ectest/ec_reader_test_examples.e:5614
% 
% ; (5) place green
% Delta:
directive(delta).


 % Happens(PlaceOnPalette(Palette1,Yellow),4).
happens(placeOnPalette(palette1, yellow), 4).


% 
% Delta:
directive(delta).


 % Happens(PlaceOnPalette(Palette1,Blue),4).
happens(placeOnPalette(palette1, blue), 4).


% 
% 
% completion Delta Happens
completion([delta, happens]).


% ectest/ec_reader_test_examples.e:5620
% 
% range time 0 5
range(time, 0, 5).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% ectest/ec_reader_test_examples.e:5626
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Exercises/SnoozeAlarm.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; Example: Alarm Clock with snooze alarm added
% ;
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:5651
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% sort clock
sort(clock).


% ectest/ec_reader_test_examples.e:5657
% 
% fluent Beeping(clock)
fluent(beeping(clock)).


% fluent AlarmTime(clock,time)
fluent(alarmTime(clock, time)).


% fluent AlarmOn(clock)
fluent(alarmOn(clock)).


% 
% event SetAlarmTime(agent,clock,time)
event(setAlarmTime(agent, clock, time)).


% ectest/ec_reader_test_examples.e:5663
% event StartBeeping(clock)
event(startBeeping(clock)).


% event TurnOnAlarm(agent,clock)
event(turnOnAlarm(agent, clock)).


% event TurnOffAlarm(agent,clock)
event(turnOffAlarm(agent, clock)).


% 
% event PressSnooze(agent,clock)
event(pressSnooze(agent, clock)).


% 
% ectest/ec_reader_test_examples.e:5669
% agent Nathan
t(agent, nathan).


% clock Clock
t(clock, clock).


% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:5674
% [agent,clock,time1,time2,time]% 
% HoldsAt(AlarmTime(clock,time1),time) &
% time1!=time2 ->
% Initiates(SetAlarmTime(agent,clock,time2),AlarmTime(clock,time2),time).
holds_at(alarmTime(Clock, Time1), Time), Time1\=Time2 ->
	initiates(setAlarmTime(Agent, Clock, Time2),
		  alarmTime(Clock, Time2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:5679
% [agent,clock,time1,time2,time]% 
% HoldsAt(AlarmTime(clock,time1),time) &
% time1!=time2 ->
% Terminates(SetAlarmTime(agent,clock,time2),AlarmTime(clock,time1),time).
holds_at(alarmTime(Clock, Time1), Time), Time1\=Time2 ->
	terminates(setAlarmTime(Agent, Clock, Time2),
		   alarmTime(Clock, Time1),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:5684
% [agent,clock,time]% 
% Initiates(TurnOnAlarm(agent,clock),AlarmOn(clock),time).
initiates(turnOnAlarm(Agent, Clock), alarmOn(Clock), Time).


% 
% 
% ectest/ec_reader_test_examples.e:5687
% [agent,clock,time]% 
% Terminates(TurnOffAlarm(agent,clock),AlarmOn(clock),time).
terminates(turnOffAlarm(Agent, Clock), alarmOn(Clock), Time).


% 
% 
% ectest/ec_reader_test_examples.e:5690
% [clock,time]% 
% Initiates(StartBeeping(clock),Beeping(clock),time).
initiates(startBeeping(Clock), beeping(Clock), Time).


% 
% 
% ectest/ec_reader_test_examples.e:5693
% [agent,clock,time]% 
% Terminates(TurnOffAlarm(agent,clock),Beeping(clock),time).
terminates(turnOffAlarm(Agent, Clock), beeping(Clock), Time).


% 
% 
% ; added axioms:
% 
% ectest/ec_reader_test_examples.e:5698
% [agent,clock,time2,time]% 
% HoldsAt(Beeping(clock),time) &
% time2 = time+9 ->
% Initiates(PressSnooze(agent,clock),AlarmTime(clock,time2),time).
holds_at(beeping(Clock), Time), Time2=Time+9 ->
	initiates(pressSnooze(Agent, Clock),
		  alarmTime(Clock, Time2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:5703
% [agent,clock,time1,time2,time]% 
% HoldsAt(Beeping(clock),time) &
% HoldsAt(AlarmTime(clock,time1),time) &
% time2 = time+9 &
% time1 != time2 ->
% Terminates(PressSnooze(agent,clock),AlarmTime(clock,time1),time).
holds_at(beeping(Clock), Time), holds_at(alarmTime(Clock, Time1), Time), Time2=Time+9, Time1\=Time2 ->
	terminates(pressSnooze(Agent, Clock),
		   alarmTime(Clock, Time1),
		   Time).


% 
% ectest/ec_reader_test_examples.e:5709
% 
% ectest/ec_reader_test_examples.e:5710
% [agent,clock,time]% 
% Terminates(PressSnooze(agent,clock),Beeping(clock),time).
terminates(pressSnooze(Agent, Clock), beeping(Clock), Time).


% 
% 
% ; Delta
% 
% ectest/ec_reader_test_examples.e:5715
% [clock,time]% 
% HoldsAt(AlarmTime(clock,time),time) &
% HoldsAt(AlarmOn(clock),time) ->
% Happens(StartBeeping(clock),time).
holds_at(alarmTime(Clock, Time), Time), holds_at(alarmOn(Clock), Time) ->
	happens(startBeeping(Clock), Time).


% 
% 
% Happens(SetAlarmTime(Nathan,Clock,2),0).
happens(setAlarmTime(nathan, clock, 2), 0).


% 
% ectest/ec_reader_test_examples.e:5721
% Happens(TurnOnAlarm(Nathan,Clock),1).
happens(turnOnAlarm(nathan, clock), 1).


% 
% Happens(PressSnooze(Nathan,Clock),4).
happens(pressSnooze(nathan, clock), 4).


% 
% 
% ; Psi
% 
% ectest/ec_reader_test_examples.e:5726
% [clock,time1,time2,time]% 
% HoldsAt(AlarmTime(clock,time1),time) &
% HoldsAt(AlarmTime(clock,time2),time) ->
% time1=time2.
holds_at(alarmTime(Clock, Time1), Time), holds_at(alarmTime(Clock, Time2), Time) ->
	Time1=Time2.


% 
% 
% ; Gamma
% ectest/ec_reader_test_examples.e:5732
% 
% !HoldsAt(AlarmOn(Clock),0).
not(holds_at(alarmOn(clock), 0)).


% 
% !HoldsAt(Beeping(Clock),0).
not(holds_at(beeping(clock), 0)).


% 
% HoldsAt(AlarmTime(Clock,3),0).
holds_at(alarmTime(clock, 3), 0).


% 
% 
% completion Happens
completion(happens).


% ectest/ec_reader_test_examples.e:5738
% 
% range time 0 15
range(time, 0, 15).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% ectest/ec_reader_test_examples.e:5744
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Exercises/TelephoneBugs.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; Example: Telephone
% ;
% ; @book{Mueller:2006,
% ;   author = "Erik T. Mueller",
% ;   year = "2006",
% ;   title = "Commonsense Reasoning",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann/Elsevier",
% ; }
% ;
% ectest/ec_reader_test_examples.e:5769
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% sort phone
sort(phone).


% ectest/ec_reader_test_examples.e:5775
% 
% agent Agent1, Agent2
t(agent, agent1).


t(agent, agent2).


% phone Phone1, Phone2
t(phone, phone1).


t(phone, phone2).


% 
% fluent Ringing(phone,phone)
fluent(ringing(phone, phone)).


% fluent DialTone(phone)
fluent(dialTone(phone)).


% ectest/ec_reader_test_examples.e:5781
% fluent BusySignal(phone)
fluent(busySignal(phone)).


% fluent Idle(phone)
fluent(idle(phone)).


% fluent Connected(phone,phone)
fluent(connected(phone, phone)).


% fluent Disconnected(phone)
fluent(disconnected(phone)).


% 
% event PickUp(agent,phone)
event(pickUp(agent, phone)).


% ectest/ec_reader_test_examples.e:5787
% event SetDown(agent,phone)
event(setDown(agent, phone)).


% event Dial(agent,phone,phone)
event(dial(agent, phone, phone)).


% 
% ; Sigma
% 
% ectest/ec_reader_test_examples.e:5792
% [agent,phone,time]% 
% HoldsAt(Idle(phone),time) ->
% Initiates(PickUp(agent,phone),DialTone(phone),time).
holds_at(idle(Phone), Time) ->
	initiates(pickUp(Agent, Phone),
		  dialTone(Phone),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:5796
% [agent,phone,time]% 
% HoldsAt(Idle(phone),time) ->
% Terminates(PickUp(agent,phone),Idle(phone),time).
holds_at(idle(Phone), Time) ->
	terminates(pickUp(Agent, Phone),
		   idle(Phone),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:5800
% [agent,phone,time]% 
% HoldsAt(DialTone(phone),time) ->
% Initiates(SetDown(agent,phone),Idle(phone),time).
holds_at(dialTone(Phone), Time) ->
	initiates(setDown(Agent, Phone),
		  idle(Phone),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:5804
% [agent,phone,time]% 
% HoldsAt(DialTone(phone),time) ->
% Terminates(SetDown(agent,phone),DialTone(phone),time).
holds_at(dialTone(Phone), Time) ->
	terminates(setDown(Agent, Phone),
		   dialTone(Phone),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:5808
% [agent,phone1,phone2,time]% 
% HoldsAt(DialTone(phone1),time) &
% HoldsAt(Idle(phone2),time) ->
% Initiates(Dial(agent,phone1,phone2),Ringing(phone1,phone2),time).
holds_at(dialTone(Phone1), Time), holds_at(idle(Phone2), Time) ->
	initiates(dial(Agent, Phone1, Phone2),
		  ringing(Phone1, Phone2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:5813
% [agent,phone1,phone2,time]% 
% HoldsAt(DialTone(phone1),time) &
% HoldsAt(Idle(phone2),time) ->
% Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).
holds_at(dialTone(Phone1), Time), holds_at(idle(Phone2), Time) ->
	terminates(dial(Agent, Phone1, Phone2),
		   dialTone(Phone1),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:5818
% [agent,phone1,phone2,time]% 
% HoldsAt(DialTone(phone1),time) &
% HoldsAt(Idle(phone2),time) ->
% Terminates(Dial(agent,phone1,phone2),Idle(phone2),time).
holds_at(dialTone(Phone1), Time), holds_at(idle(Phone2), Time) ->
	terminates(dial(Agent, Phone1, Phone2),
		   idle(Phone2),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:5823
% [agent,phone1,phone2,time]% 
% HoldsAt(DialTone(phone1),time) &
% !HoldsAt(Idle(phone2),time) ->
% Initiates(Dial(agent,phone1,phone2),BusySignal(phone1),time).
holds_at(dialTone(Phone1), Time), not(holds_at(idle(Phone2), Time)) ->
	initiates(dial(Agent, Phone1, Phone2),
		  busySignal(Phone1),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:5828
% [agent,phone1,phone2,time]% 
% HoldsAt(DialTone(phone1),time) &
% !HoldsAt(Idle(phone2),time) ->
% Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).
holds_at(dialTone(Phone1), Time), not(holds_at(idle(Phone2), Time)) ->
	terminates(dial(Agent, Phone1, Phone2),
		   dialTone(Phone1),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:5833
% [agent,phone,time]% 
% HoldsAt(BusySignal(phone),time) ->
% Initiates(SetDown(agent,phone),Idle(phone),time).
holds_at(busySignal(Phone), Time) ->
	initiates(setDown(Agent, Phone),
		  idle(Phone),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:5837
% [agent,phone,time]% 
% HoldsAt(BusySignal(phone),time) ->
% Terminates(SetDown(agent,phone),BusySignal(phone),time).
holds_at(busySignal(Phone), Time) ->
	terminates(setDown(Agent, Phone),
		   busySignal(Phone),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:5841
% [agent,phone1,phone2,time]% 
% HoldsAt(Ringing(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone1),Idle(phone1),time).
holds_at(ringing(Phone1, Phone2), Time) ->
	initiates(setDown(Agent, Phone1),
		  idle(Phone1),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:5845
% [agent,phone1,phone2,time]% 
% HoldsAt(Ringing(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone1),Idle(phone2),time).
holds_at(ringing(Phone1, Phone2), Time) ->
	initiates(setDown(Agent, Phone1),
		  idle(Phone2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:5849
% [agent,phone1,phone2,time]% 
% HoldsAt(Ringing(phone1,phone2),time) ->
% Terminates(SetDown(agent,phone1),Ringing(phone1,phone2),time).
holds_at(ringing(Phone1, Phone2), Time) ->
	terminates(setDown(Agent, Phone1),
		   ringing(Phone1, Phone2),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:5853
% [agent,phone1,phone2,time]% 
% HoldsAt(Ringing(phone1,phone2),time) ->
% Initiates(PickUp(agent,phone2),Connected(phone1,phone2),time).
holds_at(ringing(Phone1, Phone2), Time) ->
	initiates(pickUp(Agent, Phone2),
		  connected(Phone1, Phone2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:5857
% [agent,phone1,phone2,time]% 
% HoldsAt(Ringing(phone1,phone2),time) ->
% Terminates(PickUp(agent,phone2),Ringing(phone1,phone2),time).
holds_at(ringing(Phone1, Phone2), Time) ->
	terminates(pickUp(Agent, Phone2),
		   ringing(Phone1, Phone2),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:5861
% [agent,phone1,phone2,time]% 
% HoldsAt(Connected(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone1),Idle(phone1),time).
holds_at(connected(Phone1, Phone2), Time) ->
	initiates(setDown(Agent, Phone1),
		  idle(Phone1),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:5865
% [agent,phone1,phone2,time]% 
% HoldsAt(Connected(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone1),Disconnected(phone2),time).
holds_at(connected(Phone1, Phone2), Time) ->
	initiates(setDown(Agent, Phone1),
		  disconnected(Phone2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:5869
% [agent,phone1,phone2,time]% 
% HoldsAt(Connected(phone1,phone2),time) ->
% Terminates(SetDown(agent,phone1),Connected(phone1,phone2),time).
holds_at(connected(Phone1, Phone2), Time) ->
	terminates(setDown(Agent, Phone1),
		   connected(Phone1, Phone2),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:5873
% [agent,phone1,phone2,time]% 
% HoldsAt(Connected(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone2),Idle(phone2),time).
holds_at(connected(Phone1, Phone2), Time) ->
	initiates(setDown(Agent, Phone2),
		  idle(Phone2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:5877
% [agent,phone1,phone2,time]% 
% HoldsAt(Connected(phone1,phone2),time) ->
% Initiates(SetDown(agent,phone2),Disconnected(phone1),time).
holds_at(connected(Phone1, Phone2), Time) ->
	initiates(setDown(Agent, Phone2),
		  disconnected(Phone1),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:5881
% [agent,phone1,phone2,time]% 
% HoldsAt(Connected(phone1,phone2),time) ->
% Terminates(SetDown(agent,phone2),Connected(phone1,phone2),time).
holds_at(connected(Phone1, Phone2), Time) ->
	terminates(setDown(Agent, Phone2),
		   connected(Phone1, Phone2),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:5885
% [agent,phone,time]% 
% HoldsAt(Disconnected(phone),time) ->
% Initiates(SetDown(agent,phone),Idle(phone),time).
holds_at(disconnected(Phone), Time) ->
	initiates(setDown(Agent, Phone),
		  idle(Phone),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:5889
% [agent,phone,time]% 
% HoldsAt(Disconnected(phone),time) ->
% Terminates(SetDown(agent,phone),Disconnected(phone),time).
holds_at(disconnected(Phone), Time) ->
	terminates(setDown(Agent, Phone),
		   disconnected(Phone),
		   Time).


% 
% 
% ; Delta
% 
% ; (1) Two agents dial each other simultaneously without first
% ; picking up phone.
% ectest/ec_reader_test_examples.e:5897
% Happens(Dial(Agent1,Phone1,Phone2),0).
happens(dial(agent1, phone1, phone2), 0).


% 
% Happens(Dial(Agent2,Phone2,Phone1),0).
happens(dial(agent2, phone2, phone1), 0).


% 
% 
% ; (2) Two agents dial each other simultaneously.
% Happens(PickUp(Agent1,Phone1),1).
happens(pickUp(agent1, phone1), 1).


% 
% Happens(PickUp(Agent2,Phone2),1).
happens(pickUp(agent2, phone2), 1).


% 
% ectest/ec_reader_test_examples.e:5903
% Happens(Dial(Agent1,Phone1,Phone2),2).
happens(dial(agent1, phone1, phone2), 2).


% 
% Happens(Dial(Agent2,Phone2,Phone1),2).
happens(dial(agent2, phone2, phone1), 2).


% 
% Happens(SetDown(Agent1,Phone1),3).
happens(setDown(agent1, phone1), 3).


% 
% Happens(SetDown(Agent2,Phone2),3).
happens(setDown(agent2, phone2), 3).


% 
% 
% ; (3) One agent dials another agent just as the other
% ; agent picks up the phone.
% ectest/ec_reader_test_examples.e:5910
% Happens(PickUp(Agent1,Phone1),4).
happens(pickUp(agent1, phone1), 4).


% 
% Happens(Dial(Agent1,Phone1,Phone2),5).
happens(dial(agent1, phone1, phone2), 5).


% 
% Happens(PickUp(Agent2,Phone2),5).
happens(pickUp(agent2, phone2), 5).


% 
% 
% ; Psi
% 
% ectest/ec_reader_test_examples.e:5916
% [phone,time]% 
% !HoldsAt(Ringing(phone,phone),time).
not(holds_at(ringing(Phone, Phone), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:5919
% [phone1,phone2,time]% 
% HoldsAt(Ringing(phone1,phone2),time) &
% phone1!=phone2 ->
% !HoldsAt(Ringing(phone2,phone1),time).
holds_at(ringing(Phone1, Phone2), Time), Phone1\=Phone2 ->
	not(holds_at(ringing(Phone2, Phone1), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:5924
% [phone,time]% 
% !HoldsAt(Connected(phone,phone),time).
not(holds_at(connected(Phone, Phone), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:5927
% [phone1,phone2,time]% 
% HoldsAt(Connected(phone1,phone2),time) &
% phone1!=phone2 ->
% !HoldsAt(Connected(phone2,phone1),time).
holds_at(connected(Phone1, Phone2), Time), Phone1\=Phone2 ->
	not(holds_at(connected(Phone2, Phone1), Time)).


% 
% 
% mutex Idle, DialTone, BusySignal, Disconnected
mutex([idle, dialTone, busySignal, disconnected]).


% ectest/ec_reader_test_examples.e:5933
% 
% ectest/ec_reader_test_examples.e:5934
% [phone1,phone2,time]% 
% HoldsAt(Idle(phone1),time) ->
% !HoldsAt(Ringing(phone1,phone2),time) &
% !HoldsAt(Connected(phone1,phone2),time).
holds_at(idle(Phone1), Time) ->
	not(holds_at(ringing(Phone1, Phone2), Time)),
	not(holds_at(connected(Phone1, Phone2), Time)).


% 
% 
% ; contradicts (3) above:
% ;[phone1,phone2,time]
% ;HoldsAt(DialTone(phone2),time) ->
% ;!HoldsAt(Ringing(phone1,phone2),time) &
% ;!HoldsAt(Connected(phone1,phone2),time).
% ectest/ec_reader_test_examples.e:5944
% 
% ; etc.
% 
% ; Gamma
% 
% ectest/ec_reader_test_examples.e:5949
% [phone] % HoldsAt(Idle(phone),0).
holds_at(idle(Phone), 0).


% 
% 
% completion Happens
completion(happens).


% 
% range time 0 6
range(time, 0, 6).


% range offset 1 1
range(offset, 1, 1).


% ectest/ec_reader_test_examples.e:5955
% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter11/HungryCat.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ectest/ec_reader_test_examples.e:5992
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort object
sort(object).


% sort agent: object
subsort(agent, object).


% ectest/ec_reader_test_examples.e:5998
% sort food: object
subsort(food, object).


% sort surface
sort(surface).


% sort plan
sort(plan).


% 
% reified sort belief
reified_sort(belief).


% 
% ectest/ec_reader_test_examples.e:6004
% agent Cat
t(agent, cat).


% surface Floor, Chair, Shelf, Table
t(surface, floor).


t(surface, chair).


t(surface, shelf).


t(surface, table).


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


% ectest/ec_reader_test_examples.e:6010
% predicate SoundPlan(agent,belief,plan,time)
predicate(soundPlan(agent, belief, plan, time)).


% 
% fluent On(object,surface)
fluent(on(object, surface)).


% fluent Goal(agent,belief)
fluent(goal(agent, belief)).


% fluent CanJump(surface,surface)
fluent(canJump(surface, surface)).


% fluent Plan(agent,belief,plan)
fluent(plan(agent, belief, plan)).


% ectest/ec_reader_test_examples.e:6016
% fluent Satiated(agent)
fluent(satiated(agent)).


% fluent Believe(agent,belief)
fluent(believe(agent, belief)).


% 
% event AddPlan(agent,belief,plan)
event(addPlan(agent, belief, plan)).


% event DropPlan(agent,belief,plan)
event(dropPlan(agent, belief, plan)).


% event Jump(agent,surface,surface)
event(jump(agent, surface, surface)).


% ectest/ec_reader_test_examples.e:6022
% event Move(surface,surface,surface)
event(move(surface, surface, surface)).


% event Eat(agent,food)
event(eat(agent, food)).


% event Wait(agent)
event(wait(agent)).


% 
% belief BSatiated(agent)
t(belief, 'bSatiated(agent)').


% belief BCanJump(surface,surface)
t(belief, 'bCanJump(surface').


t(belief, 'surface)').


% ectest/ec_reader_test_examples.e:6028
% belief BOn(object,surface)
t(belief, 'bOn(object').


t(belief, 'surface)').


% 
% ; Sigma
% 
% ; A5
% ectest/ec_reader_test_examples.e:6033
% [agent,belief,plan,time]% 
% Initiates(AddPlan(agent,belief,plan),Plan(agent,belief,plan),time).
initiates(addPlan(Agent, Belief, Plan), plan(Agent, Belief, Plan), Time).


% 
% 
% ; A6
% ectest/ec_reader_test_examples.e:6037
% [agent,belief,plan,time]% 
% Terminates(DropPlan(agent,belief,plan),Plan(agent,belief,plan),time).
terminates(dropPlan(Agent, Belief, Plan), plan(Agent, Belief, Plan), Time).


% 
% 
% ectest/ec_reader_test_examples.e:6040
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
% ectest/ec_reader_test_examples.e:6045
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
% ectest/ec_reader_test_examples.e:6050
% [surface1,surface2,surface3,time]% 
% Initiates(Move(surface1,surface2,surface3),CanJump(surface1,surface3),time).
initiates(move(Surface1, Surface2, Surface3), canJump(Surface1, Surface3), Time).


% 
% 
% ectest/ec_reader_test_examples.e:6053
% [surface1,surface2,surface3,time]% 
% Terminates(Move(surface1,surface2,surface3),CanJump(surface1,surface2),time).
terminates(move(Surface1, Surface2, Surface3), canJump(Surface1, Surface2), Time).


% 
% 
% ectest/ec_reader_test_examples.e:6056
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
% ectest/ec_reader_test_examples.e:6061
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
% ectest/ec_reader_test_examples.e:6066
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


% ectest/ec_reader_test_examples.e:6072
% 
% 
% ectest/ec_reader_test_examples.e:6074
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


% ectest/ec_reader_test_examples.e:6080
% 
% 
% ectest/ec_reader_test_examples.e:6082
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
% ectest/ec_reader_test_examples.e:6088
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
% ectest/ec_reader_test_examples.e:6094
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
% ectest/ec_reader_test_examples.e:6100
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
% ectest/ec_reader_test_examples.e:6107
% 
% ; A7
% ectest/ec_reader_test_examples.e:6109
% [agent,belief,plan,time]% 
% HoldsAt(Goal(agent,belief),time) &
% !HoldsAt(Believe(agent,belief),time) &
% SelectedPlan(agent,belief,plan,time) &
% (!{plan1} HoldsAt(Plan(agent,belief,plan1),time)) ->
% Happens(AddPlan(agent,belief,plan),time).
holds_at(goal(Agent, Belief), Time), not(holds_at(believe(Agent, Belief), Time)), selectedPlan(Agent, Belief, Plan, Time), not(exists([Plan1], holds_at(plan(Agent, Belief, Plan1), Time))) ->
	happens(addPlan(Agent, Belief, Plan), Time).


% 
% ectest/ec_reader_test_examples.e:6115
% 
% ; A8
% ectest/ec_reader_test_examples.e:6117
% [agent,belief,time]% 
% HoldsAt(Plan(agent,belief,P1),time) &
% !HoldsAt(Believe(agent,belief),time) &
% SoundPlan(agent,belief,P1,time) ->
% Happens(Jump(Cat,Floor,Chair),time).
holds_at(plan(Agent, Belief, p1), Time), not(holds_at(believe(Agent, Belief), Time)), soundPlan(Agent, Belief, p1, Time) ->
	happens(jump(cat, floor, chair), Time).


% 
% 
% ectest/ec_reader_test_examples.e:6123
% [agent,belief,time]% 
% HoldsAt(Plan(agent,belief,P1a),time) &
% !HoldsAt(Believe(agent,belief),time) &
% SoundPlan(agent,belief,P1a,time) ->
% Happens(Wait(Cat),time).
holds_at(plan(Agent, Belief, p1a), Time), not(holds_at(believe(Agent, Belief), Time)), soundPlan(Agent, Belief, p1a, Time) ->
	happens(wait(cat), Time).


% 
% 
% ectest/ec_reader_test_examples.e:6129
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
% ectest/ec_reader_test_examples.e:6136
% [agent,belief,plan,time]% 
% HoldsAt(Plan(agent,belief,plan),time) ->
% Happens(DropPlan(agent,belief,plan),time).
holds_at(plan(Agent, Belief, Plan), Time) ->
	happens(dropPlan(Agent, Belief, Plan), Time).


% 
% 
% ; A10
% ectest/ec_reader_test_examples.e:6141
% [agent,belief,time]% 
% HoldsAt(Plan(agent,belief,P1),time) &
% !HoldsAt(Believe(agent,belief),time) &
% SoundPlan(agent,belief,P1,time) ->
% Happens(AddPlan(agent,belief,P1a),time).
holds_at(plan(Agent, Belief, p1), Time), not(holds_at(believe(Agent, Belief), Time)), soundPlan(Agent, Belief, p1, Time) ->
	happens(addPlan(Agent, Belief, p1a), Time).


% 
% 
% ectest/ec_reader_test_examples.e:6147
% [agent,belief,time]% 
% HoldsAt(Plan(agent,belief,P1a),time) &
% !HoldsAt(Believe(agent,belief),time) &
% SoundPlan(agent,belief,P1a,time) ->
% Happens(AddPlan(agent,belief,P1b),time).
holds_at(plan(Agent, Belief, p1a), Time), not(holds_at(believe(Agent, Belief), Time)), soundPlan(Agent, Belief, p1a, Time) ->
	happens(addPlan(Agent, Belief, p1b), Time).


% 
% 
% ectest/ec_reader_test_examples.e:6153
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
% ectest/ec_reader_test_examples.e:6160
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
% ectest/ec_reader_test_examples.e:6167
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
% ectest/ec_reader_test_examples.e:6176
% 
% ectest/ec_reader_test_examples.e:6177
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


% ectest/ec_reader_test_examples.e:6194
% 
% 
% 
% ; SoundPlan
% 
% ectest/ec_reader_test_examples.e:6199
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


% ectest/ec_reader_test_examples.e:6205
% 
% 
% ; Gamma
% 
% ectest/ec_reader_test_examples.e:6209
% [agent,belief]% 
% HoldsAt(Goal(agent,belief),0) <->
% (agent=Cat & belief=BSatiated(Cat)).
holds_at(goal(Agent, Belief), 0) <->
	Agent=cat,
	Belief=bSatiated(cat).


% 
% 
% ectest/ec_reader_test_examples.e:6213
% [agent,belief,plan] % !HoldsAt(Plan(agent,belief,plan),0).
not(holds_at(plan(Agent, Belief, Plan), 0)).


% 
% 
% ectest/ec_reader_test_examples.e:6215
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
% ectest/ec_reader_test_examples.e:6220
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
% ectest/ec_reader_test_examples.e:6225
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
% ectest/ec_reader_test_examples.e:6230
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
% ectest/ec_reader_test_examples.e:6236
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


% ectest/ec_reader_test_examples.e:6242
% 
% range time 0 7
range(time, 0, 7).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% ectest/ec_reader_test_examples.e:6248
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2006/Chapter11/Lottery.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{OrtonyCloreCollins:1988,
% ;   author = "Andrew Ortony and Gerald L. Clore and Allan M. Collins",
% ;   year = "1988",
% ;   title = "The Cognitive Structure of Emotions",
% ;   address = "Cambridge",
% ;   publisher = "Cambridge University Press",
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
% ectest/ec_reader_test_examples.e:6279
% 
% option modeldiff on
option(modeldiff, on).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% ectest/ec_reader_test_examples.e:6285
% sort agent
sort(agent).


% sort aboutevent
sort(aboutevent).


% sort desirability: integer
subsort(desirability, integer).


% 
% agent Kate, Lisa
t(agent, kate).


t(agent, lisa).


% aboutevent WinLotteryKate, WinLotteryLisa
t(aboutevent, winLotteryKate).


t(aboutevent, winLotteryLisa).


% ectest/ec_reader_test_examples.e:6291
% 
% fluent Joy(agent,aboutevent)
fluent(joy(agent, aboutevent)).


% fluent Desirability(agent,agent,aboutevent,desirability)
fluent(desirability(agent, agent, aboutevent, desirability)).


% fluent Believe(agent,aboutevent)
fluent(believe(agent, aboutevent)).


% fluent Like(agent,agent)
fluent(like(agent, agent)).


% fluent HappyFor(agent,agent,aboutevent)
fluent(happyFor(agent, agent, aboutevent)).


% ectest/ec_reader_test_examples.e:6297
% 
% event WinLottery(agent)
event(winLottery(agent)).


% event AddJoy(agent,aboutevent)
event(addJoy(agent, aboutevent)).


% event AddHappyFor(agent,agent,aboutevent)
event(addHappyFor(agent, agent, aboutevent)).


% 
% ; Sigma
% ectest/ec_reader_test_examples.e:6303
% 
% ectest/ec_reader_test_examples.e:6304
% [agent,aboutevent,time]% 
% Initiates(AddJoy(agent,aboutevent),Joy(agent,aboutevent),time).
initiates(addJoy(Agent, Aboutevent), joy(Agent, Aboutevent), Time).


% 
% 
% ectest/ec_reader_test_examples.e:6307
% [agent1,agent2,aboutevent,time]% 
% Initiates(AddHappyFor(agent1,agent2,aboutevent),
%           HappyFor(agent1,agent2,aboutevent),
%           time).
initiates(addHappyFor(Agent1, Agent2, Aboutevent), happyFor(Agent1, Agent2, Aboutevent), Time).


% 
% 
% ectest/ec_reader_test_examples.e:6312
% [agent1,agent2,aboutevent,time]% 
% (agent1=Kate & aboutevent=WinLotteryKate) |
% (agent1=Lisa & aboutevent=WinLotteryLisa) ->
% Initiates(WinLottery(agent1),Believe(agent2,aboutevent),time).
(   Agent1=kate,
    Aboutevent=winLotteryKate
;   (   (   Agent1=kate,
    Aboutevent=winLotteryKate
;   Agent1=lisa,
    Aboutevent=winLotteryLisa
->  initiates(winLottery(Agent1),
	      believe(Agent2, Aboutevent),
	      Time)
).


% 
% 
% ; Delta
% ectest/ec_reader_test_examples.e:6318
% 
% ectest/ec_reader_test_examples.e:6319
% [agent,aboutevent,desirability,time]% 
% !HoldsAt(Joy(agent,aboutevent),time) &
% HoldsAt(Desirability(agent,agent,aboutevent,desirability),time) &
% desirability=1 &
% HoldsAt(Believe(agent,aboutevent),time) ->
% Happens(AddJoy(agent,aboutevent),time).
not(holds_at(joy(Agent, Aboutevent), Time)), holds_at(desirability(Agent, Agent, Aboutevent, Desirability), Time), Desirability=1, holds_at(believe(Agent, Aboutevent), Time) ->
	happens(addJoy(Agent, Aboutevent), Time).


% 
% ectest/ec_reader_test_examples.e:6325
% 
% ectest/ec_reader_test_examples.e:6326
% [agent1,agent2,aboutevent,desirability1,desirability2,time]% 
% !HoldsAt(HappyFor(agent1,agent2,aboutevent),time) &
% HoldsAt(Desirability(agent1,agent2,aboutevent,desirability1),time) &
% desirability1=1 &
% HoldsAt(Desirability(agent1,agent1,aboutevent,desirability2),time) &
% desirability2=1 &
% HoldsAt(Like(agent1,agent2),time) &
% HoldsAt(Believe(agent1,aboutevent),time) &
% agent1 != agent2 ->
% Happens(AddHappyFor(agent1,agent2,aboutevent),time).
not(holds_at(happyFor(Agent1, Agent2, Aboutevent), Time)), holds_at(desirability(Agent1, Agent2, Aboutevent, Desirability1), Time), Desirability1=1, holds_at(desirability(Agent1, Agent1, Aboutevent, Desirability2), Time), Desirability2=1, holds_at(like(Agent1, Agent2), Time), holds_at(believe(Agent1, Aboutevent), Time), Agent1\=Agent2 ->
	happens(addHappyFor(Agent1, Agent2, Aboutevent),
		Time).


% ectest/ec_reader_test_examples.e:6335
% 
% 
% Happens(WinLottery(Kate),0).
happens(winLottery(kate), 0).


% 
% 
% ; Psi
% 
% ectest/ec_reader_test_examples.e:6341
% [agent1,agent2,aboutevent,desirability1,desirability2,time]% 
% HoldsAt(Desirability(agent1,agent2,aboutevent,desirability1),time) &
% HoldsAt(Desirability(agent1,agent2,aboutevent,desirability2),time) ->
% desirability1 = desirability2.
holds_at(desirability(Agent1, Agent2, Aboutevent, Desirability1), Time), holds_at(desirability(Agent1, Agent2, Aboutevent, Desirability2), Time) ->
	Desirability1=Desirability2.


% 
% 
% ; Gamma
% ectest/ec_reader_test_examples.e:6347
% 
% ectest/ec_reader_test_examples.e:6348
% [agent,aboutevent] % !HoldsAt(Joy(agent,aboutevent),0).
not(holds_at(joy(Agent, Aboutevent), 0)).


% 
% ectest/ec_reader_test_examples.e:6349
% [agent1,agent2,aboutevent] % !HoldsAt(HappyFor(agent1,agent2,aboutevent),0).
not(holds_at(happyFor(Agent1, Agent2, Aboutevent), 0)).


% 
% ectest/ec_reader_test_examples.e:6350
% [aboutevent] % !HoldsAt(Believe(Kate,aboutevent),0).
not(holds_at(believe(kate, Aboutevent), 0)).


% 
% ectest/ec_reader_test_examples.e:6351
% [aboutevent] % !HoldsAt(Believe(Lisa,aboutevent),0).
not(holds_at(believe(lisa, Aboutevent), 0)).


% 
% ectest/ec_reader_test_examples.e:6352
% [agent1,agent2,time] % HoldsAt(Like(agent1,agent2),time).
holds_at(like(Agent1, Agent2), Time).


% 
% 
% ectest/ec_reader_test_examples.e:6354
% [time] % HoldsAt(Desirability(Lisa,Kate,WinLotteryKate,1),time).
holds_at(desirability(lisa, kate, winLotteryKate, 1), Time).


% 
% ectest/ec_reader_test_examples.e:6355
% [time] % HoldsAt(Desirability(Kate,Kate,WinLotteryKate,1),time).
holds_at(desirability(kate, kate, winLotteryKate, 1), Time).


% 
% ectest/ec_reader_test_examples.e:6356
% [time] % HoldsAt(Desirability(Lisa,Lisa,WinLotteryKate,1),time).
holds_at(desirability(lisa, lisa, winLotteryKate, 1), Time).


% 
% ectest/ec_reader_test_examples.e:6357
% [time] % HoldsAt(Desirability(Kate,Kate,WinLotteryLisa,0),time).
holds_at(desirability(kate, kate, winLotteryLisa, 0), Time).


% 
% ectest/ec_reader_test_examples.e:6358
% [time] % HoldsAt(Desirability(Kate,Lisa,WinLotteryLisa,0),time).
holds_at(desirability(kate, lisa, winLotteryLisa, 0), Time).


% 
% ectest/ec_reader_test_examples.e:6359
% [time] % HoldsAt(Desirability(Kate,Kate,WinLotteryLisa,0),time).
holds_at(desirability(kate, kate, winLotteryLisa, 0), Time).


% 
% ectest/ec_reader_test_examples.e:6360
% [time] % HoldsAt(Desirability(Kate,Lisa,WinLotteryKate,0),time).
holds_at(desirability(kate, lisa, winLotteryKate, 0), Time).


% 
% ectest/ec_reader_test_examples.e:6361
% [time] % HoldsAt(Desirability(Lisa,Lisa,WinLotteryLisa,0),time).
holds_at(desirability(lisa, lisa, winLotteryLisa, 0), Time).


% 
% ectest/ec_reader_test_examples.e:6362
% [time] % HoldsAt(Desirability(Lisa,Kate,WinLotteryLisa,1),time).
holds_at(desirability(lisa, kate, winLotteryLisa, 1), Time).


% 
% 
% completion Happens
completion(happens).


% 
% range time 0 3
range(time, 0, 3).


% range desirability -1 1
range(desirability, -1, 1).


% ectest/ec_reader_test_examples.e:6368
% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Manual/Example1a.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ectest/ec_reader_test_examples.e:6388
% 
% option timediff off
option(timediff, off).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% ectest/ec_reader_test_examples.e:6394
% sort agent
sort(agent).


% 
% fluent Awake(agent)
fluent(awake(agent)).


% event WakeUp(agent)
event(wakeUp(agent)).


% 
% ectest/ec_reader_test_examples.e:6399
% [agent,time] % Initiates(WakeUp(agent),Awake(agent),time).
initiates(wakeUp(Agent), awake(Agent), Time).


% 
% 
% agent James
t(agent, james).


% !HoldsAt(Awake(James),0).
not(holds_at(awake(james), 0)).


% 
% Delta:
directive(delta).


 % Happens(WakeUp(James),0).
happens(wakeUp(james), 0).


% 
% 
% ectest/ec_reader_test_examples.e:6405
% completion Delta Happens
completion([delta, happens]).


% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% 
% ectest/ec_reader_test_examples.e:6411
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Manual/Example1.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ectest/ec_reader_test_examples.e:6426
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% 
% ectest/ec_reader_test_examples.e:6432
% fluent Awake(agent)
fluent(awake(agent)).


% event WakeUp(agent)
event(wakeUp(agent)).


% 
% ectest/ec_reader_test_examples.e:6435
% [agent,time] % Initiates(WakeUp(agent),Awake(agent),time).
initiates(wakeUp(Agent), awake(Agent), Time).


% 
% 
% agent James
t(agent, james).


% !HoldsAt(Awake(James),0).
not(holds_at(awake(james), 0)).


% 
% Delta:
directive(delta).


 % Happens(WakeUp(James),0).
happens(wakeUp(james), 0).


% 
% 
% ectest/ec_reader_test_examples.e:6441
% completion Delta Happens
completion([delta, happens]).


% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% 
% ectest/ec_reader_test_examples.e:6447
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Manual/Example4.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ectest/ec_reader_test_examples.e:6461
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


% ectest/ec_reader_test_examples.e:6467
% event WakeUp(agent)
event(wakeUp(agent)).


% 
% ectest/ec_reader_test_examples.e:6469
% [agent,time] % Initiates(WakeUp(agent),Awake(agent),time).
initiates(wakeUp(Agent), awake(Agent), Time).


% 
% ectest/ec_reader_test_examples.e:6470
% [agent,time] % Happens(WakeUp(agent),time) -> !HoldsAt(Awake(agent),time).
happens(wakeUp(Agent), Time) ->
	not(holds_at(awake(Agent), Time)).


% 
% 
% agent James, Jessie
t(agent, james).


t(agent, jessie).


% !HoldsAt(Awake(James),0).
not(holds_at(awake(james), 0)).


% 
% !HoldsAt(Awake(Jessie),0).
not(holds_at(awake(jessie), 0)).


% 
% HoldsAt(Awake(James),1).
holds_at(awake(james), 1).


% 
% ectest/ec_reader_test_examples.e:6476
% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Manual/Example3.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ectest/ec_reader_test_examples.e:6495
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


% ectest/ec_reader_test_examples.e:6501
% event WakeUp(agent)
event(wakeUp(agent)).


% 
% ectest/ec_reader_test_examples.e:6503
% [agent,time] % Initiates(WakeUp(agent),Awake(agent),time).
initiates(wakeUp(Agent), awake(Agent), Time).


% 
% 
% agent James, Jessie
t(agent, james).


t(agent, jessie).


% !HoldsAt(Awake(James),0).
not(holds_at(awake(james), 0)).


% 
% HoldsAt(Awake(James),1).
holds_at(awake(james), 1).


% 
% 
% ectest/ec_reader_test_examples.e:6509
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Manual/Example2.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ectest/ec_reader_test_examples.e:6527
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


% ectest/ec_reader_test_examples.e:6533
% event WakeUp(agent)
event(wakeUp(agent)).


% 
% ectest/ec_reader_test_examples.e:6535
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
% ectest/ec_reader_test_examples.e:6541
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2004b/RunningAndDriving2.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @inproceedings{Mueller:2004b,
% ;   author = "Erik T. Mueller",
% ;   year = "2004",
% ;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
% ;   editor = "Valerie Barr and Zdravko Markov",
% ;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
% ;   pages = "147--152",
% ;   address = "Menlo Park, CA",
% ;   publisher = "AAAI Press",
% ; }
% ;
% ectest/ec_reader_test_examples.e:6570
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% 
% ectest/ec_reader_test_examples.e:6576
% fluent Tired(agent)
fluent(tired(agent)).


% 
% event Move(agent)
event(move(agent)).


% event Run(agent)
event(run(agent)).


% event Drive(agent)
event(drive(agent)).


% 
% ectest/ec_reader_test_examples.e:6582
% [agent,time]% 
% Happens(Move(agent),time) ->
% Happens(Run(agent),time) | Happens(Drive(agent),time).
(   ( happens(move(Agent), Time)->happens(run(Agent), Time)
    )
;   happens(drive(Agent), Time)
).


% 
% 
% xor Run, Drive
xor([run, drive]).


% 
% ectest/ec_reader_test_examples.e:6588
% [agent,time] % Initiates(Run(agent),Tired(agent),time).
initiates(run(Agent), tired(Agent), Time).


% 
% 
% agent James
t(agent, james).


% 
% !HoldsAt(Tired(James),0).
not(holds_at(tired(james), 0)).


% 
% Happens(Move(James),0).
happens(move(james), 0).


% 
% ectest/ec_reader_test_examples.e:6594
% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% ectest/ec_reader_test_examples.e:6600
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2004b/OffOn.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @inproceedings{Mueller:2004b,
% ;   author = "Erik T. Mueller",
% ;   year = "2004",
% ;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
% ;   editor = "Valerie Barr and Zdravko Markov",
% ;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
% ;   pages = "147--152",
% ;   address = "Menlo Park, CA",
% ;   publisher = "AAAI Press",
% ; }
% ;
% ectest/ec_reader_test_examples.e:6626
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% sort switch
sort(switch).


% ectest/ec_reader_test_examples.e:6632
% 
% fluent On(switch)
fluent(on(switch)).


% fluent Off(switch)
fluent(off(switch)).


% event TurnOn(agent,switch)
event(turnOn(agent, switch)).


% event TurnOff(agent,switch)
event(turnOff(agent, switch)).


% 
% ectest/ec_reader_test_examples.e:6638
% noninertial Off
noninertial(off).


% 
% ectest/ec_reader_test_examples.e:6640
% [switch,time] % HoldsAt(Off(switch),time) <-> !HoldsAt(On(switch),time).
holds_at(off(Switch), Time) <->
	not(holds_at(on(Switch), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:6642
% [agent,switch,time] % Initiates(TurnOn(agent,switch),On(switch),time).
initiates(turnOn(Agent, Switch), on(Switch), Time).


% 
% ectest/ec_reader_test_examples.e:6643
% [agent,switch,time] % Terminates(TurnOff(agent,switch),On(switch),time).
terminates(turnOff(Agent, Switch), on(Switch), Time).


% 
% 
% agent James
t(agent, james).


% switch Switch1
t(switch, switch1).


% 
% !HoldsAt(On(Switch1),0).
not(holds_at(on(switch1), 0)).


% 
% ectest/ec_reader_test_examples.e:6649
% Happens(TurnOn(James,Switch1),0).
happens(turnOn(james, switch1), 0).


% 
% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:6655
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2004b/TV2.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @inproceedings{Mueller:2004b,
% ;   author = "Erik T. Mueller",
% ;   year = "2004",
% ;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
% ;   editor = "Valerie Barr and Zdravko Markov",
% ;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
% ;   pages = "147--152",
% ;   address = "Menlo Park, CA",
% ;   publisher = "AAAI Press",
% ; }
% ;
% ectest/ec_reader_test_examples.e:6682
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% sort switch
sort(switch).


% ectest/ec_reader_test_examples.e:6688
% sort tv
sort(tv).


% 
% function TVOf(switch): tv
function(tVOf(switch), tv).


% fluent SwitchOn(switch)
fluent(switchOn(switch)).


% fluent TVOn(tv)
fluent(tVOn(tv)).


% fluent PluggedIn(tv)
fluent(pluggedIn(tv)).


% ectest/ec_reader_test_examples.e:6694
% event TurnOn(agent,switch)
event(turnOn(agent, switch)).


% event TurnOff(agent,switch)
event(turnOff(agent, switch)).


% 
% ectest/ec_reader_test_examples.e:6697
% [agent,switch,time] % Initiates(TurnOn(agent,switch),SwitchOn(switch),time).
initiates(turnOn(Agent, Switch), switchOn(Switch), Time).


% 
% 
% ectest/ec_reader_test_examples.e:6699
% [agent,switch,tv,time]% 
% TVOf(switch)=tv & HoldsAt(PluggedIn(tv),time) ->
% Initiates(TurnOn(agent,switch),TVOn(tv),time).
tVOf(Switch)=Tv, holds_at(pluggedIn(Tv), Time) ->
	initiates(turnOn(Agent, Switch), tVOn(Tv), Time).


% 
% 
% agent James
t(agent, james).


% switch Switch1
t(switch, switch1).


% ectest/ec_reader_test_examples.e:6705
% tv TV1
t(tv, tv1).


% 
% TVOf(Switch1)=TV1.
tVOf(switch1)=tv1.


% 
% !HoldsAt(PluggedIn(TV1),0).
not(holds_at(pluggedIn(tv1), 0)).


% 
% !HoldsAt(SwitchOn(Switch1),0).
not(holds_at(switchOn(switch1), 0)).


% 
% !HoldsAt(TVOn(TV1),0).
not(holds_at(tVOn(tv1), 0)).


% 
% ectest/ec_reader_test_examples.e:6711
% Happens(TurnOn(James,Switch1),0).
happens(turnOn(james, switch1), 0).


% 
% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:6717
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2004b/Approve.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; example of concurrent events with cumulative or canceling effects
% ;
% ; @inproceedings{Mueller:2004b,
% ;   author = "Erik T. Mueller",
% ;   year = "2004",
% ;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
% ;   editor = "Valerie Barr and Zdravko Markov",
% ;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
% ;   pages = "147--152",
% ;   address = "Menlo Park, CA",
% ;   publisher = "AAAI Press",
% ; }
% ;
% ectest/ec_reader_test_examples.e:6746
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% 
% ectest/ec_reader_test_examples.e:6752
% event ApproveOf(agent,agent)
event(approveOf(agent, agent)).


% event DisapproveOf(agent,agent)
event(disapproveOf(agent, agent)).


% fluent Happy(agent)
fluent(happy(agent)).


% fluent Confused(agent)
fluent(confused(agent)).


% 
% ectest/ec_reader_test_examples.e:6757
% [agent1,agent2,time]% 
% !Happens(DisapproveOf(agent1,agent2),time) ->
% Initiates(ApproveOf(agent1,agent2),Happy(agent2),time).
not(happens(disapproveOf(Agent1, Agent2), Time)) ->
	initiates(approveOf(Agent1, Agent2),
		  happy(Agent2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:6761
% [agent1,agent2,time]% 
% !Happens(ApproveOf(agent1,agent2),time) ->
% Terminates(DisapproveOf(agent1,agent2),Happy(agent2),time).
not(happens(approveOf(Agent1, Agent2), Time)) ->
	terminates(disapproveOf(Agent1, Agent2),
		   happy(Agent2),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:6765
% [agent1,agent2,time]% 
% Happens(DisapproveOf(agent1,agent2),time) ->
% Initiates(ApproveOf(agent1,agent2),Confused(agent2),time).
happens(disapproveOf(Agent1, Agent2), Time) ->
	initiates(approveOf(Agent1, Agent2),
		  confused(Agent2),
		  Time).


% 
% 
% agent James, Peter
t(agent, james).


t(agent, peter).


% 
% ectest/ec_reader_test_examples.e:6771
% [agent] % !HoldsAt(Happy(agent),0) & !HoldsAt(Confused(agent),0).
not(holds_at(happy(Agent), 0)),
not(holds_at(confused(Agent), 0)).


% 
% 
% Happens(ApproveOf(Peter,James),0).
happens(approveOf(peter, james), 0).


% 
% Happens(DisapproveOf(Peter,James),0).
happens(disapproveOf(peter, james), 0).


% 
% 
% completion Happens
completion(happens).


% ectest/ec_reader_test_examples.e:6777
% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% ectest/ec_reader_test_examples.e:6783
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2004b/Leaf.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @inproceedings{Mueller:2004b,
% ;   author = "Erik T. Mueller",
% ;   year = "2004",
% ;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
% ;   editor = "Valerie Barr and Zdravko Markov",
% ;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
% ;   pages = "147--152",
% ;   address = "Menlo Park, CA",
% ;   publisher = "AAAI Press",
% ; }
% ;
% ectest/ec_reader_test_examples.e:6809
% 
% option trajectory on
option(trajectory, on).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% ectest/ec_reader_test_examples.e:6815
% sort object
sort(object).


% sort height: integer
subsort(height, integer).


% 
% fluent Height(object,height)
fluent(height(object, height)).


% fluent Falling(object)
fluent(falling(object)).


% event StartFalling(object)
event(startFalling(object)).


% ectest/ec_reader_test_examples.e:6821
% event HitsGround(object)
event(hitsGround(object)).


% 
% ectest/ec_reader_test_examples.e:6823
% [object,height1,height2,time]% 
% HoldsAt(Height(object,height1),time) &
% HoldsAt(Height(object,height2),time) ->
% height1=height2.
holds_at(height(Object, Height1), Time), holds_at(height(Object, Height2), Time) ->
	Height1=Height2.


% 
% 
% ectest/ec_reader_test_examples.e:6828
% [object,time]% 
% Initiates(StartFalling(object),Falling(object),time).
initiates(startFalling(Object), falling(Object), Time).


% 
% 
% ectest/ec_reader_test_examples.e:6831
% [object,height,time]% 
% Releases(StartFalling(object),Height(object,height),time).
releases(startFalling(Object), height(Object, Height), Time).


% 
% 
% ectest/ec_reader_test_examples.e:6834
% [object,height1,height2,offset,time]% 
% HoldsAt(Height(object,height1),time) &
% height2=height1-offset ->
% Trajectory(Falling(object),time,Height(object,height2),offset).
holds_at(height(Object, Height1), Time), Height2=Height1-Offset ->
	trajectory(falling(Object),
		   Time,
		   height(Object, Height2),
		   Offset).


% 
% 
% ectest/ec_reader_test_examples.e:6839
% [object,time]% 
% HoldsAt(Falling(object),time) &
% HoldsAt(Height(object,0),time) ->
% Happens(HitsGround(object),time).
holds_at(falling(Object), Time), holds_at(height(Object, 0), Time) ->
	happens(hitsGround(Object), Time).


% 
% 
% ;[object,height1,height2,time]
% ;HoldsAt(Height(object,height1),time) &
% ;height1 != height2 ->
% ;Terminates(HitsGround(object),Height(object,height2),time).
% ectest/ec_reader_test_examples.e:6848
% 
% ectest/ec_reader_test_examples.e:6849
% [object,height,time]% 
% HoldsAt(Height(object,height),time) ->
% Initiates(HitsGround(object),Height(object,height),time).
holds_at(height(Object, Height), Time) ->
	initiates(hitsGround(Object),
		  height(Object, Height),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:6853
% [object,time]% 
% Terminates(HitsGround(object),Falling(object),time).
terminates(hitsGround(Object), falling(Object), Time).


% 
% 
% object Leaf
t(object, leaf).


% 
% !HoldsAt(Falling(Leaf),0).
not(holds_at(falling(leaf), 0)).


% 
% ectest/ec_reader_test_examples.e:6859
% HoldsAt(Height(Leaf,4),0).
holds_at(height(leaf, 4), 0).


% 
% Happens(StartFalling(Leaf),2).
happens(startFalling(leaf), 2).


% 
% 
% completion Happens
completion(happens).


% 
% range time 0 7
range(time, 0, 7).


% ectest/ec_reader_test_examples.e:6865
% range offset 1 4
range(offset, 1, 4).


% range height 0 4
range(height, 0, 4).


% 
% ; End of file.
% 
% 
% ectest/ec_reader_test_examples.e:6871
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2004b/RunningAndDriving1.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @inproceedings{Mueller:2004b,
% ;   author = "Erik T. Mueller",
% ;   year = "2004",
% ;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
% ;   editor = "Valerie Barr and Zdravko Markov",
% ;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
% ;   pages = "147--152",
% ;   address = "Menlo Park, CA",
% ;   publisher = "AAAI Press",
% ; }
% ;
% ectest/ec_reader_test_examples.e:6896
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% 
% ectest/ec_reader_test_examples.e:6902
% fluent Tired(agent)
fluent(tired(agent)).


% 
% event Move(agent)
event(move(agent)).


% event Run(agent)
event(run(agent)).


% event Drive(agent)
event(drive(agent)).


% 
% ectest/ec_reader_test_examples.e:6908
% [agent,time]% 
% Happens(Move(agent),time) ->
% Happens(Run(agent),time) | Happens(Drive(agent),time).
(   ( happens(move(Agent), Time)->happens(run(Agent), Time)
    )
;   happens(drive(Agent), Time)
).


% 
% 
% xor Run, Drive
xor([run, drive]).


% 
% ectest/ec_reader_test_examples.e:6914
% [agent,time] % Initiates(Run(agent),Tired(agent),time).
initiates(run(Agent), tired(Agent), Time).


% 
% 
% agent James
t(agent, james).


% 
% !HoldsAt(Tired(James),0).
not(holds_at(tired(james), 0)).


% 
% Happens(Move(James),0).
happens(move(james), 0).


% 
% ectest/ec_reader_test_examples.e:6920
% HoldsAt(Tired(James),1).
holds_at(tired(james), 1).


% 
% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:6926
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2004b/TV1.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @inproceedings{Mueller:2004b,
% ;   author = "Erik T. Mueller",
% ;   year = "2004",
% ;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
% ;   editor = "Valerie Barr and Zdravko Markov",
% ;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
% ;   pages = "147--152",
% ;   address = "Menlo Park, CA",
% ;   publisher = "AAAI Press",
% ; }
% ;
% ectest/ec_reader_test_examples.e:6953
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort agent
sort(agent).


% sort switch
sort(switch).


% ectest/ec_reader_test_examples.e:6959
% sort tv
sort(tv).


% 
% function TVOf(switch): tv
function(tVOf(switch), tv).


% fluent SwitchOn(switch)
fluent(switchOn(switch)).


% fluent TVOn(tv)
fluent(tVOn(tv)).


% fluent PluggedIn(tv)
fluent(pluggedIn(tv)).


% ectest/ec_reader_test_examples.e:6965
% event TurnOn(agent,switch)
event(turnOn(agent, switch)).


% event TurnOff(agent,switch)
event(turnOff(agent, switch)).


% 
% ectest/ec_reader_test_examples.e:6968
% [agent,switch,time] % Initiates(TurnOn(agent,switch),SwitchOn(switch),time).
initiates(turnOn(Agent, Switch), switchOn(Switch), Time).


% 
% 
% ectest/ec_reader_test_examples.e:6970
% [agent,switch,tv,time]% 
% TVOf(switch)=tv & HoldsAt(PluggedIn(tv),time) ->
% Initiates(TurnOn(agent,switch),TVOn(tv),time).
tVOf(Switch)=Tv, holds_at(pluggedIn(Tv), Time) ->
	initiates(turnOn(Agent, Switch), tVOn(Tv), Time).


% 
% 
% agent James
t(agent, james).


% switch Switch1
t(switch, switch1).


% ectest/ec_reader_test_examples.e:6976
% tv TV1
t(tv, tv1).


% 
% TVOf(Switch1)=TV1.
tVOf(switch1)=tv1.


% 
% HoldsAt(PluggedIn(TV1),0).
holds_at(pluggedIn(tv1), 0).


% 
% !HoldsAt(SwitchOn(Switch1),0).
not(holds_at(switchOn(switch1), 0)).


% 
% !HoldsAt(TVOn(TV1),0).
not(holds_at(tVOn(tv1), 0)).


% 
% ectest/ec_reader_test_examples.e:6982
% Happens(TurnOn(James,Switch1),0).
happens(turnOn(james, switch1), 0).


% 
% 
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:6988
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2004b/RouletteWheel.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @inproceedings{Mueller:2004b,
% ;   author = "Erik T. Mueller",
% ;   year = "2004",
% ;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
% ;   editor = "Valerie Barr and Zdravko Markov",
% ;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
% ;   pages = "147--152",
% ;   address = "Menlo Park, CA",
% ;   publisher = "AAAI Press",
% ; }
% ;
% ectest/ec_reader_test_examples.e:7015
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort wheel
sort(wheel).


% sort value: integer
subsort(value, integer).


% ectest/ec_reader_test_examples.e:7021
% 
% fluent WheelValueDeterminingFluent(wheel,value)
fluent(wheelValueDeterminingFluent(wheel, value)).


% fluent WheelValue(wheel,value)
fluent(wheelValue(wheel, value)).


% noninertial WheelValueDeterminingFluent
noninertial(wheelValueDeterminingFluent).


% event Spin(wheel)
event(spin(wheel)).


% 
% ectest/ec_reader_test_examples.e:7027
% [wheel,value1,value2,time]% 
% HoldsAt(WheelValue(wheel,value1),time) &
% HoldsAt(WheelValue(wheel,value2),time) ->
% value1=value2.
holds_at(wheelValue(Wheel, Value1), Time), holds_at(wheelValue(Wheel, Value2), Time) ->
	Value1=Value2.


% 
% 
% ectest/ec_reader_test_examples.e:7032
% [wheel,value1,value2,time]% 
% HoldsAt(WheelValueDeterminingFluent(wheel,value1),time) &
% HoldsAt(WheelValueDeterminingFluent(wheel,value2),time) ->
% value1=value2.
holds_at(wheelValueDeterminingFluent(Wheel, Value1), Time), holds_at(wheelValueDeterminingFluent(Wheel, Value2), Time) ->
	Value1=Value2.


% 
% 
% ectest/ec_reader_test_examples.e:7037
% [wheel,value,time]% 
% HoldsAt(WheelValueDeterminingFluent(wheel,value),time) ->
% Initiates(Spin(wheel),WheelValue(wheel,value),time).
holds_at(wheelValueDeterminingFluent(Wheel, Value), Time) ->
	initiates(spin(Wheel),
		  wheelValue(Wheel, Value),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:7041
% [wheel,value1,value2,time]% 
% HoldsAt(WheelValue(wheel,value1),time) &
% HoldsAt(WheelValueDeterminingFluent(wheel,value2),time) &
% value1!=value2 ->
% Terminates(Spin(wheel),WheelValue(wheel,value1),time).
holds_at(wheelValue(Wheel, Value1), Time), holds_at(wheelValueDeterminingFluent(Wheel, Value2), Time), Value1\=Value2 ->
	terminates(spin(Wheel),
		   wheelValue(Wheel, Value1),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:7047
% [wheel,time]% 
% ectest/ec_reader_test_examples.e:7048
% {value} % HoldsAt(WheelValueDeterminingFluent(wheel,value),time).
exists([Value], holds_at(wheelValueDeterminingFluent(Wheel, Value), Time)).


% 
% 
% wheel Wheel
t(wheel, wheel).


% 
% HoldsAt(WheelValue(Wheel,7),0).
holds_at(wheelValue(wheel, 7), 0).


% 
% Happens(Spin(Wheel),0).
happens(spin(wheel), 0).


% 
% ectest/ec_reader_test_examples.e:7054
% HoldsAt(WheelValueDeterminingFluent(Wheel,7),1).
holds_at(wheelValueDeterminingFluent(wheel, 7), 1).


% 
% 
% completion Happens
completion(happens).


% 
% range value 7 10
range(value, 7, 10).


% range time 0 1
range(time, 0, 1).


% ectest/ec_reader_test_examples.e:7060
% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Mueller2004b/PickUp.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @inproceedings{Mueller:2004b,
% ;   author = "Erik T. Mueller",
% ;   year = "2004",
% ;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
% ;   editor = "Valerie Barr and Zdravko Markov",
% ;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
% ;   pages = "147--152",
% ;   address = "Menlo Park, CA",
% ;   publisher = "AAAI Press",
% ; }
% ;
% ectest/ec_reader_test_examples.e:7090
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort object
sort(object).


% sort agent: object
subsort(agent, object).


% ectest/ec_reader_test_examples.e:7096
% sort physobj: object
subsort(physobj, object).


% sort location
sort(location).


% 
% fluent At(object,location)
fluent(at(object, location)).


% fluent Holding(agent,physobj)
fluent(holding(agent, physobj)).


% event PickUp(agent,physobj)
event(pickUp(agent, physobj)).


% ectest/ec_reader_test_examples.e:7102
% event SetDown(agent,physobj)
event(setDown(agent, physobj)).


% event Move(agent,location,location)
event(move(agent, location, location)).


% 
% ; state constraints
% 
% ectest/ec_reader_test_examples.e:7107
% [agent,location,physobj,time]% 
% HoldsAt(At(agent,location),time) &
% HoldsAt(Holding(agent,physobj),time) ->
% HoldsAt(At(physobj,location),time).
holds_at(at(Agent, Location), Time), holds_at(holding(Agent, Physobj), Time) ->
	holds_at(at(Physobj, Location), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7112
% [object,location1,location2,time]% 
% HoldsAt(At(object,location1),time) &
% HoldsAt(At(object,location2),time) ->
% location1=location2.
holds_at(at(Object, Location1), Time), holds_at(at(Object, Location2), Time) ->
	Location1=Location2.


% 
% 
% ; effect axioms
% ectest/ec_reader_test_examples.e:7118
% 
% ectest/ec_reader_test_examples.e:7119
% [agent,location1,location2,time]% 
% Initiates(Move(agent,location1,location2),At(agent,location2),time).
initiates(move(Agent, Location1, Location2), at(Agent, Location2), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7122
% [agent,location1,location2,time]% 
% Terminates(Move(agent,location1,location2),At(agent,location1),time).
terminates(move(Agent, Location1, Location2), at(Agent, Location1), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7125
% [agent,physobj,time]% 
% Initiates(PickUp(agent,physobj),Holding(agent,physobj),time).
initiates(pickUp(Agent, Physobj), holding(Agent, Physobj), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7128
% [agent,physobj,time]% 
% Terminates(SetDown(agent,physobj),Holding(agent,physobj),time).
terminates(setDown(Agent, Physobj), holding(Agent, Physobj), Time).


% 
% 
% ; preconditions
% 
% ectest/ec_reader_test_examples.e:7133
% [agent,location1,location2,time]% 
% Happens(Move(agent,location1,location2),time) ->
% HoldsAt(At(agent,location1),time).
happens(move(Agent, Location1, Location2), Time) ->
	holds_at(at(Agent, Location1), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7137
% [agent,physobj,time]% 
% Happens(PickUp(agent,physobj),time) ->
% ectest/ec_reader_test_examples.e:7139
% {location}%  HoldsAt(At(agent,location),time) &
%            HoldsAt(At(physobj,location),time).
exists([Location],  (happens(pickUp(Agent, Physobj), Time)->holds_at(at(Agent, Location), Time), holds_at(at(Physobj, Location), Time))).


% 
% 
% ; releases
% 
% ectest/ec_reader_test_examples.e:7144
% [agent,physobj,location,time]% 
% Releases(PickUp(agent,physobj),At(physobj,location),time).
releases(pickUp(Agent, Physobj), at(Physobj, Location), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7147
% [agent,physobj,location,time]% 
% HoldsAt(At(agent,location),time) ->
% Initiates(SetDown(agent,physobj),At(physobj,location),time).
holds_at(at(Agent, Location), Time) ->
	initiates(setDown(Agent, Physobj),
		  at(Physobj, Location),
		  Time).


% 
% 
% ;[agent,physobj,location1,location2,time]
% ;HoldsAt(At(agent,location1),time) &
% ;location1 != location2 ->
% ;Terminates(SetDown(agent,physobj),At(physobj,location2),time).
% ectest/ec_reader_test_examples.e:7155
% 
% agent James
t(agent, james).


% physobj Coin
t(physobj, coin).


% location L1, L2, L3, L4
t(location, l1).


t(location, l2).


t(location, l3).


t(location, l4).


% 
% !HoldsAt(Holding(James,Coin),0).
not(holds_at(holding(james, coin), 0)).


% 
% ectest/ec_reader_test_examples.e:7161
% HoldsAt(At(Coin,L4),0).
holds_at(at(coin, l4), 0).


% 
% HoldsAt(At(James,L1),0).
holds_at(at(james, l1), 0).


% 
% Happens(Move(James,L1,L2),0).
happens(move(james, l1, l2), 0).


% 
% Happens(Move(James,L2,L3),1).
happens(move(james, l2, l3), 1).


% 
% Happens(Move(James,L3,L4),2).
happens(move(james, l3, l4), 2).


% 
% Happens(PickUp(James,Coin),3).
happens(pickUp(james, coin), 3).


% 
% ectest/ec_reader_test_examples.e:7167
% Happens(Move(James,L4,L3),4).
happens(move(james, l4, l3), 4).


% 
% Happens(Move(James,L3,L2),5).
happens(move(james, l3, l2), 5).


% 
% Happens(SetDown(James,Coin),6).
happens(setDown(james, coin), 6).


% 
% Happens(Move(James,L2,L3),7).
happens(move(james, l2, l3), 7).


% 
% Happens(Move(James,L3,L4),8).
happens(move(james, l3, l4), 8).


% 
% 
% ectest/ec_reader_test_examples.e:7173
% completion Happens
completion(happens).


% 
% range time 0 9
range(time, 0, 9).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:7179
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/FrankEtAl2003/Story1.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ectest/ec_reader_test_examples.e:7204
% 
% option modeldiff on
option(modeldiff, on).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% ectest/ec_reader_test_examples.e:7210
% sort agent
sort(agent).


% 
% load examples/FrankEtAl2003/FrankEtAl.e
load('examples/FrankEtAl2003/FrankEtAl.e').


% 
% agent Bob, Jilly
t(agent, bob).


t(agent, jilly).


% 
% ectest/ec_reader_test_examples.e:7216
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


% 
% ectest/ec_reader_test_examples.e:7222
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


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:7229
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/FrankEtAl2003/FrankEtAl.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ectest/ec_reader_test_examples.e:7254
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


% ectest/ec_reader_test_examples.e:7260
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


% ectest/ec_reader_test_examples.e:7266
% 
% xor PlaySoccer, PlayHideAndSeek, PlayComputerGame, PlayWithDog
xor([playSoccer, playHideAndSeek, playComputerGame, playWithDog]).


% 
% ectest/ec_reader_test_examples.e:7269
% [agent,time]% 
% HoldsAt(PlaySoccer(agent),time) ->
% HoldsAt(Outside(agent),time).
holds_at(playSoccer(Agent), Time) ->
	holds_at(outside(Agent), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7273
% [agent,time]% 
% HoldsAt(PlaySoccer(agent),time) ->
% ({agent1} agent1!=agent & HoldsAt(PlaySoccer(agent1),time)).
holds_at(playSoccer(Agent), Time) ->
	exists([Agent1],
	       (Agent1\=Agent, holds_at(playSoccer(Agent1), Time))).


% 
% 
% ectest/ec_reader_test_examples.e:7277
% [agent,time]% 
% HoldsAt(PlayHideAndSeek(agent),time) ->
% ({agent1} agent1!=agent & HoldsAt(PlayHideAndSeek(agent1),time)).
holds_at(playHideAndSeek(Agent), Time) ->
	exists([Agent1],
	       (Agent1\=Agent, holds_at(playHideAndSeek(Agent1), Time))).


% 
% 
% ectest/ec_reader_test_examples.e:7281
% [agent,time]% 
% HoldsAt(PlayComputerGame(agent),time) ->
% !HoldsAt(Outside(agent),time).
holds_at(playComputerGame(Agent), Time) ->
	not(holds_at(outside(Agent), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:7285
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
% ectest/ec_reader_test_examples.e:7291
% 
% ectest/ec_reader_test_examples.e:7292
% [agent,time]% 
% HoldsAt(PlaySoccer(agent),time) &
% HoldsAt(Win(agent),time) ->
% !HoldsAt(PlaySoccer(agent),time+1).
holds_at(playSoccer(Agent), Time), holds_at(win(Agent), Time) ->
	not(holds_at(playSoccer(Agent), Time+1)).


% 
% 
% ectest/ec_reader_test_examples.e:7297
% [agent,time]% 
% HoldsAt(PlayHideAndSeek(agent),time) &
% HoldsAt(Win(agent),time) ->
% !HoldsAt(PlayHideAndSeek(agent),time+1).
holds_at(playHideAndSeek(Agent), Time), holds_at(win(Agent), Time) ->
	not(holds_at(playHideAndSeek(Agent), Time+1)).


% 
% 
% ectest/ec_reader_test_examples.e:7302
% [agent,time]% 
% HoldsAt(PlayComputerGame(agent),time) &
% HoldsAt(Win(agent),time) ->
% !HoldsAt(PlayComputerGame(agent),time+1).
holds_at(playComputerGame(Agent), Time), holds_at(win(Agent), Time) ->
	not(holds_at(playComputerGame(Agent), Time+1)).


% 
% 
% ectest/ec_reader_test_examples.e:7307
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
% ectest/ec_reader_test_examples.e:7313
% [agent,time]% 
% HoldsAt(PlaySoccer(agent),time) ->
% !HoldsAt(Raining(),time).
holds_at(playSoccer(Agent), Time) ->
	not(holds_at(raining(), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:7317
% [agent,time]% 
% HoldsAt(Win(agent),time) ->
% !({agent1} agent1!=agent & HoldsAt(Win(agent1),time)).
holds_at(win(Agent), Time) ->
	not(exists([Agent1],
		   (Agent1\=Agent, holds_at(win(Agent1), Time)))).


% 
% 
% ectest/ec_reader_test_examples.e:7321
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
% ectest/ec_reader_test_examples.e:7328
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/GiunchigliaEtAl2004/MonkeyPrediction.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{Giunchiglia:2004,
% ;   author = "Enrico Giunchiglia and Joohyung Lee and Vladimir Lifschitz and Norman C. McCain and Hudson Turner",
% ;   year = "2004",
% ;   title = "Nonmonotonic causal theories",
% ;   journal = "Artificial Intelligence",
% ;   volume = "153",
% ;   pages = "49--104",
% ; }
% ;
% ectest/ec_reader_test_examples.e:7353
% 
% ; deduction
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% load examples/GiunchigliaEtAl2004/MonkeyBananas.e
load('examples/GiunchigliaEtAl2004/MonkeyBananas.e').


% ectest/ec_reader_test_examples.e:7359
% 
% HoldsAt(At(Monkey,L1),0).
holds_at(at(monkey, l1), 0).


% 
% HoldsAt(At(Bananas,L2),0).
holds_at(at(bananas, l2), 0).


% 
% HoldsAt(At(Box,L3),0).
holds_at(at(box, l3), 0).


% 
% Happens(Walk(L3),0).
happens(walk(l3), 0).


% 
% Happens(PushBox(L2),1).
happens(pushBox(l2), 1).


% 
% ectest/ec_reader_test_examples.e:7365
% 
% completion Happens
completion(happens).


% 
% range time 0 2
range(time, 0, 2).


% range offset 0 0
range(offset, 0, 0).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:7372
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/GiunchigliaEtAl2004/MonkeyPlanning.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{Giunchiglia:2004,
% ;   author = "Enrico Giunchiglia and Joohyung Lee and Vladimir Lifschitz and Norman C. McCain and Hudson Turner",
% ;   year = "2004",
% ;   title = "Nonmonotonic causal theories",
% ;   journal = "Artificial Intelligence",
% ;   volume = "153",
% ;   pages = "49--104",
% ; }
% ;
% ectest/ec_reader_test_examples.e:7397
% 
% ; planning
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% load examples/GiunchigliaEtAl2004/MonkeyBananas.e
load('examples/GiunchigliaEtAl2004/MonkeyBananas.e').


% ectest/ec_reader_test_examples.e:7403
% 
% HoldsAt(At(Monkey,L1),0).
holds_at(at(monkey, l1), 0).


% 
% HoldsAt(At(Bananas,L2),0).
holds_at(at(bananas, l2), 0).


% 
% HoldsAt(At(Box,L3),0).
holds_at(at(box, l3), 0).


% 
% HoldsAt(HasBananas(),4).
holds_at(hasBananas(), 4).


% 
% 
% ; PLAN Happens(Walk(L3),0).
% ; PLAN Happens(PushBox(L2),1).
% ; PLAN Happens(ClimbOn(),2).
% ; PLAN Happens(GraspBananas(),3).
% ectest/ec_reader_test_examples.e:7413
% 
% ; one event at a time
% ectest/ec_reader_test_examples.e:7415
% [event1,event2,time] % Happens(event1,time) & Happens(event2,time) ->
% event1=event2.
happens(Event1, Time), happens(Event2, Time) ->
	Event1=Event2.


% 
% 
% range time 0 4
range(time, 0, 4).


% range offset 0 0
range(offset, 0, 0).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:7422
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/GiunchigliaEtAl2004/MonkeyPostdiction.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{Giunchiglia:2004,
% ;   author = "Enrico Giunchiglia and Joohyung Lee and Vladimir Lifschitz and Norman C. McCain and Hudson Turner",
% ;   year = "2004",
% ;   title = "Nonmonotonic causal theories",
% ;   journal = "Artificial Intelligence",
% ;   volume = "153",
% ;   pages = "49--104",
% ; }
% ;
% ectest/ec_reader_test_examples.e:7447
% 
% ; postdiction
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% load examples/GiunchigliaEtAl2004/MonkeyBananas.e
load('examples/GiunchigliaEtAl2004/MonkeyBananas.e').


% ectest/ec_reader_test_examples.e:7453
% 
% HoldsAt(At(Monkey,L1),0).
holds_at(at(monkey, l1), 0).


% 
% HoldsAt(At(Bananas,L2),0).
holds_at(at(bananas, l2), 0).


% 
% Happens(Walk(L3),0).
happens(walk(l3), 0).


% 
% Happens(PushBox(L2),1).
happens(pushBox(l2), 1).


% 
% 
% ectest/ec_reader_test_examples.e:7459
% completion Happens
completion(happens).


% 
% range time 0 2
range(time, 0, 2).


% range offset 0 0
range(offset, 0, 0).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:7465
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/GiunchigliaEtAl2004/MonkeyBananas.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{Giunchiglia:2004,
% ;   author = "Enrico Giunchiglia and Joohyung Lee and Vladimir Lifschitz and Norman C. McCain and Hudson Turner",
% ;   year = "2004",
% ;   title = "Nonmonotonic causal theories",
% ;   journal = "Artificial Intelligence",
% ;   volume = "153",
% ;   pages = "49--104",
% ; }
% ;
% ectest/ec_reader_test_examples.e:7490
% 
% sort object
sort(object).


% sort location
sort(location).


% 
% object Monkey, Bananas, Box
t(object, monkey).


t(object, bananas).


t(object, box).


% location L1, L2, L3
t(location, l1).


t(location, l2).


t(location, l3).


% ectest/ec_reader_test_examples.e:7496
% 
% fluent At(object,location)
fluent(at(object, location)).


% fluent OnBox()
fluent(onBox()).


% fluent HasBananas()
fluent(hasBananas()).


% 
% event Walk(location)
event(walk(location)).


% ectest/ec_reader_test_examples.e:7502
% event PushBox(location)
event(pushBox(location)).


% event ClimbOn()
event(climbOn()).


% event ClimbOff()
event(climbOff()).


% event GraspBananas()
event(graspBananas()).


% 
% ectest/ec_reader_test_examples.e:7507
% [object,location1,location2,time]% 
% HoldsAt(At(object,location1),time) &
% HoldsAt(At(object,location2),time) ->
% location1=location2.
holds_at(at(Object, Location1), Time), holds_at(at(Object, Location2), Time) ->
	Location1=Location2.


% 
% 
% ectest/ec_reader_test_examples.e:7512
% [object,location,time]% 
% object=% Monkey ->
% Initiates(Walk(location),At(object,location),time).
Object=monkey ->
	initiates(walk(Location),
		  at(Object, Location),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:7516
% [object,location1,location2,time]% 
% object=% Monkey &
% HoldsAt(At(object,location1),time) ->
% Terminates(Walk(location2),At(object,location1),time).
Object=monkey, holds_at(at(Object, Location1), Time) ->
	terminates(walk(Location2),
		   at(Object, Location1),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:7521
% [location,time]% 
% Happens(Walk(location),time) ->
% !HoldsAt(At(Monkey,location),time) &
% !HoldsAt(OnBox(),time).
happens(walk(Location), Time) ->
	not(holds_at(at(monkey, Location), Time)),
	not(holds_at(onBox(), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:7526
% [location,time]% 
% HoldsAt(HasBananas(),time) &
% HoldsAt(At(Monkey,location),time) ->
% HoldsAt(At(Bananas,location),time).
holds_at(hasBananas(), Time), holds_at(at(monkey, Location), Time) ->
	holds_at(at(bananas, Location), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7531
% [object,location,time]% 
% object=% Box | object=Monkey ->
% Initiates(PushBox(location),At(object,location),time).
(   Object=box
;   (   (   Object=box
;   Object=monkey
->  initiates(pushBox(Location),
	      at(Object, Location),
	      Time)
).


% 
% 
% ectest/ec_reader_test_examples.e:7535
% [object,location1,location2,time]% 
% (object=Box | object=Monkey) &
% HoldsAt(At(object,location1),time) ->
% Terminates(PushBox(location2),At(object,location1),time).
(Object=box;Object=monkey), holds_at(at(Object, Location1), Time) ->
	terminates(pushBox(Location2),
		   at(Object, Location1),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:7540
% [location,time]% 
% Happens(PushBox(location),time) ->
% ({location1}
%   HoldsAt(At(Box,location1),time) &
%   HoldsAt(At(Monkey,location1),time)) &
% !HoldsAt(At(Monkey,location),time) &
% !HoldsAt(OnBox(),time).
happens(pushBox(Location), Time) ->
	exists([Location1],
	       (holds_at(at(box, Location1), Time), holds_at(at(monkey, Location1), Time))),
	not(holds_at(at(monkey, Location), Time)),
	not(holds_at(onBox(), Time)).


% ectest/ec_reader_test_examples.e:7546
% 
% 
% ectest/ec_reader_test_examples.e:7548
% [time] % Initiates(ClimbOn(),OnBox(),time).
initiates(climbOn(), onBox(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7550
% [time]% 
% Happens(ClimbOn(),time) ->
% !HoldsAt(OnBox(),time).
happens(climbOn(), Time) ->
	not(holds_at(onBox(), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:7554
% [time] % Terminates(ClimbOff(),OnBox(),time).
terminates(climbOff(), onBox(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7556
% [time]% 
% Happens(ClimbOff(),time) ->
% HoldsAt(OnBox(),time).
happens(climbOff(), Time) ->
	holds_at(onBox(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7560
% [time] % Initiates(GraspBananas(),HasBananas(),time).
initiates(graspBananas(), hasBananas(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7562
% [object,location,time]% 
% object=% Bananas ->
% Releases(GraspBananas(),At(object,location),time).
Object=bananas ->
	releases(graspBananas(), at(Object, Location), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7566
% [time]% 
% Happens(GraspBananas(),time) ->
% ({location1}
%   HoldsAt(At(Bananas,location1),time) &
%   HoldsAt(At(Monkey,location1),time)) &
% HoldsAt(OnBox(),time).
happens(graspBananas(), Time) ->
	exists([Location1],
	       (holds_at(at(bananas, Location1), Time), holds_at(at(monkey, Location1), Time))),
	holds_at(onBox(), Time).


% 
% ectest/ec_reader_test_examples.e:7572
% 
% ectest/ec_reader_test_examples.e:7573
% [time]% 
% HoldsAt(OnBox(),time) ->
% ectest/ec_reader_test_examples.e:7575
% {location1}%  HoldsAt(At(Box,location1),time) &
%             HoldsAt(At(Monkey,location1),time).
exists([Location1],  (holds_at(onBox(), Time)->holds_at(at(box, Location1), Time), holds_at(at(monkey, Location1), Time))).


% 
% 
% ; End of file.
% 
% 
% ectest/ec_reader_test_examples.e:7581
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Antoniou1997/Student.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; conflicting defaults: method (D)
% ; \fullciteA[p. 157]{Antoniou:1997}
% ;
% ; @book{Antoniou:1997,
% ;   author = "Grigoris Antoniou",
% ;   year = "1997",
% ;   title = "Nonmonotonic Reasoning",
% ;   address = "Cambridge, MA",
% ;   publisher = "MIT Press",
% ; }
% ;
% ectest/ec_reader_test_examples.e:7606
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort x
sort(x).


% 
% ectest/ec_reader_test_examples.e:7612
% predicate Adult(x)
predicate(adult(x)).


% predicate Student(x)
predicate(student(x)).


% predicate Employed(x)
predicate(employed(x)).


% predicate Ab1(x)
predicate(ab1(x)).


% predicate Ab2(x)
predicate(ab2(x)).


% 
% ectest/ec_reader_test_examples.e:7618
% x Mary
t(x, mary).


% 
% Student(Mary).
student(mary).


% 
% 
% ectest/ec_reader_test_examples.e:7622
% [x] % Adult(x) & !Ab1(x) -> Employed(x).
adult(X), not(ab1(X)) ->
	employed(X).


% 
% ectest/ec_reader_test_examples.e:7623
% [x] % Student(x) & !Ab2(x) -> !Employed(x).
student(X), not(ab2(X)) ->
	not(employed(X)).


% 
% ectest/ec_reader_test_examples.e:7624
% [x] % Student(x) -> Adult(x).
student(X) ->
	adult(X).


% 
% Theta:
directive(theta).


 
% ectest/ec_reader_test_examples.e:7625
% [x] % Student(x) -> Ab1(x).
student(X) ->
	ab1(X).


% 
% 
% range time 0 0
range(time, 0, 0).


% range offset 1 1
range(offset, 1, 1).


% 
% completion Theta Ab1
completion([theta, ab1]).


% ectest/ec_reader_test_examples.e:7631
% completion Theta Ab2
completion([theta, ab2]).


% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Antoniou1997/Dropout.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; dealing with conflicting defaults by adding conditions
% ; to one of the conflicting rules
% ; \fullciteA[p. 56]{Antoniou:1997}
% ;
% ; @book{Antoniou:1997,
% ;   author = "Grigoris Antoniou",
% ;   year = "1997",
% ;   title = "Nonmonotonic Reasoning",
% ;   address = "Cambridge, MA",
% ;   publisher = "MIT Press",
% ; }
% ;
% ectest/ec_reader_test_examples.e:7662
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort x
sort(x).


% 
% ectest/ec_reader_test_examples.e:7668
% predicate Dropout(x)
predicate(dropout(x)).


% predicate Adult(x)
predicate(adult(x)).


% predicate Employed(x)
predicate(employed(x)).


% predicate Ab1(x)
predicate(ab1(x)).


% predicate Ab2(x)
predicate(ab2(x)).


% 
% ectest/ec_reader_test_examples.e:7674
% x Bill
t(x, bill).


% 
% Dropout(Bill).
dropout(bill).


% 
% 
% ectest/ec_reader_test_examples.e:7678
% [x] % Dropout(x) & !Ab1(x) -> Adult(x).
dropout(X), not(ab1(X)) ->
	adult(X).


% 
% ectest/ec_reader_test_examples.e:7679
% [x] % Adult(x) & !Dropout(x) & !Ab2(x) -> Employed(x).
adult(X), not(dropout(X)), not(ab2(X)) ->
	employed(X).


% 
% 
% range time 0 0
range(time, 0, 0).


% range offset 1 1
range(offset, 1, 1).


% 
% completion Theta Ab1
completion([theta, ab1]).


% ectest/ec_reader_test_examples.e:7685
% completion Theta Ab2
completion([theta, ab2]).


% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Shanahan1999/Happy.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @incollection{Shanahan:1999,
% ;   author = "Shanahan, Murray",
% ;   year = "1999",
% ;   title = "The Event Calculus explained",
% ;   editor = "Michael J. Wooldridge and Manuela M. Veloso",
% ;   booktitle = "Artificial Intelligence Today: Recent Trends and Developments",
% ;   series = "Lecture Notes in Computer Science",
% ;   volume = "1600",
% ;   pages = "409--430",
% ;   address = "Berlin",
% ;   publisher = "Springer",
% ; }
% ;
% ; deduction
% ;
% ; modifications from Shanahan's formulation:
% ; InitiallyN -> !HoldsAt
% ; InitiallyP -> HoldsAt
% ; timestamps
% ;
% ectest/ec_reader_test_examples.e:7724
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% sort person
sort(person).


% event Feed(person)
event(feed(person)).


% ectest/ec_reader_test_examples.e:7730
% event Clothe(person)
event(clothe(person)).


% fluent Happy(person)
fluent(happy(person)).


% fluent Hungry(person)
fluent(hungry(person)).


% fluent Cold(person)
fluent(cold(person)).


% noninertial Happy
noninertial(happy).


% 
% ectest/ec_reader_test_examples.e:7736
% [person,time]% 
% HoldsAt(Happy(person),time) <->
% !HoldsAt(Hungry(person),time) &
% !HoldsAt(Cold(person),time).
holds_at(happy(Person), Time) <->
	not(holds_at(hungry(Person), Time)),
	not(holds_at(cold(Person), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:7741
% [person,time]% 
% Terminates(Feed(person),Hungry(person),time).
terminates(feed(Person), hungry(Person), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7744
% [person,time]% 
% Terminates(Clothe(person),Cold(person),time).
terminates(clothe(Person), cold(Person), Time).


% 
% 
% person Fred
t(person, fred).


% 
% HoldsAt(Hungry(Fred),0).
holds_at(hungry(fred), 0).


% 
% ectest/ec_reader_test_examples.e:7750
% !HoldsAt(Cold(Fred),0).
not(holds_at(cold(fred), 0)).


% 
% Happens(Feed(Fred),1).
happens(feed(fred), 1).


% 
% 
% completion Happens
completion(happens).


% 
% range time 0 2
range(time, 0, 2).


% ectest/ec_reader_test_examples.e:7756
% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Shanahan1999/ThielscherCircuit.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{Thielscher:1997,
% ;   author = "Michael Thielscher",
% ;   year = "1997",
% ;   title = "Ramification and causality",
% ;   journal = "Artificial Intelligence",
% ;   volume = "89",
% ;   pages = "317--364",
% ; }
% ;
% ; @incollection{Shanahan:1999,
% ;   author = "Shanahan, Murray",
% ;   year = "1999",
% ;   title = "The Event Calculus explained",
% ;   editor = "Michael J. Wooldridge and Manuela M. Veloso",
% ;   booktitle = "Artificial Intelligence Today: Recent Trends and Developments",
% ;   series = "Lecture Notes in Computer Science",
% ;   volume = "1600",
% ;   pages = "409--430",
% ;   address = "Berlin",
% ;   publisher = "Springer",
% ; }
% ;
% ; deduction
% ;
% ; modifications from Shanahan's formulation:
% ; timestamps
% ;
% ectest/ec_reader_test_examples.e:7802
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% load foundations/ECCausal.e
load('foundations/ECCausal.e').


% 
% event LightOn()
event(lightOn()).


% ectest/ec_reader_test_examples.e:7808
% event Close1()
event(close1()).


% event Open2()
event(open2()).


% event CloseRelay()
event(closeRelay()).


% 
% fluent Light()
fluent(light()).


% fluent Switch1()
fluent(switch1()).


% ectest/ec_reader_test_examples.e:7814
% fluent Switch2()
fluent(switch2()).


% fluent Switch3()
fluent(switch3()).


% fluent Relay()
fluent(relay()).


% 
% ectest/ec_reader_test_examples.e:7818
% [time]% 
% Stopped(Light(),time) &
% Initiated(Switch1(),time) &
% Initiated(Switch2(),time) ->
% Happens(LightOn(),time).
stopped(light(), Time), initiated(switch1(), Time), initiated(switch2(), Time) ->
	happens(lightOn(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7824
% [time]% 
% Started(Switch2(),time) &
% Initiated(Relay(),time) ->
% Happens(Open2(),time).
started(switch2(), Time), initiated(relay(), Time) ->
	happens(open2(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7829
% [time]% 
% Stopped(Relay(),time) &
% Initiated(Switch1(),time) &
% Initiated(Switch3(),time) ->
% Happens(CloseRelay(),time).
stopped(relay(), Time), initiated(switch1(), Time), initiated(switch3(), Time) ->
	happens(closeRelay(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7835
% [time] % Initiates(LightOn(),Light(),time).
initiates(lightOn(), light(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7837
% [time] % Terminates(Open2(),Switch2(),time).
terminates(open2(), switch2(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7839
% [time] % Initiates(CloseRelay(),Relay(),time).
initiates(closeRelay(), relay(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7841
% [time] % Initiates(Close1(),Switch1(),time).
initiates(close1(), switch1(), Time).


% 
% 
% !HoldsAt(Switch1(),0).
not(holds_at(switch1(), 0)).


% 
% HoldsAt(Switch2(),0).
holds_at(switch2(), 0).


% 
% HoldsAt(Switch3(),0).
holds_at(switch3(), 0).


% 
% !HoldsAt(Relay(),0).
not(holds_at(relay(), 0)).


% 
% ectest/ec_reader_test_examples.e:7847
% !HoldsAt(Light(),0).
not(holds_at(light(), 0)).


% 
% 
% Happens(Close1(),0).
happens(close1(), 0).


% 
% 
% completion Happens
completion(happens).


% 
% ectest/ec_reader_test_examples.e:7853
% range time 0 1
range(time, 0, 1).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% 
% ectest/ec_reader_test_examples.e:7859
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Shanahan1999/CoinToss.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{Kartha:1994,
% ;   author = "G. Neelakantan Kartha",
% ;   year = "1994",
% ;   title = "Two counterexamples related to \uppercase{B}aker's approach to the frame problem",
% ;   journal = "Artificial Intelligence",
% ;   volume = "69",
% ;   number = "1--2",
% ;   pages = "379--391",
% ; }
% ;
% ; @incollection{Shanahan:1999,
% ;   author = "Shanahan, Murray",
% ;   year = "1999",
% ;   title = "The Event Calculus explained",
% ;   editor = "Michael J. Wooldridge and Manuela M. Veloso",
% ;   booktitle = "Artificial Intelligence Today: Recent Trends and Developments",
% ;   series = "Lecture Notes in Computer Science",
% ;   volume = "1600",
% ;   pages = "409--430",
% ;   address = "Berlin",
% ;   publisher = "Springer",
% ; }
% ;
% ; model finding
% ;
% ; modifications from Shanahan's formulation:
% ; InitiallyP -> HoldsAt
% ; pruning of models irrelevant to example
% ; timestamps
% ;
% ectest/ec_reader_test_examples.e:7903
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% event Toss()
event(toss()).


% fluent ItsHeads()
fluent(itsHeads()).


% ectest/ec_reader_test_examples.e:7909
% fluent Heads()
fluent(heads()).


% noninertial ItsHeads
noninertial(itsHeads).


% 
% ectest/ec_reader_test_examples.e:7912
% [time] % HoldsAt(ItsHeads(),time) -> Initiates(Toss(),Heads(),time).
holds_at(itsHeads(), Time) ->
	initiates(toss(), heads(), Time).


% 
% ectest/ec_reader_test_examples.e:7913
% [time] % !HoldsAt(ItsHeads(),time) -> Terminates(Toss(),Heads(),time).
not(holds_at(itsHeads(), Time)) ->
	terminates(toss(), heads(), Time).


% 
% 
% HoldsAt(Heads(),0).
holds_at(heads(), 0).


% 
% Happens(Toss(),1).
happens(toss(), 1).


% 
% Happens(Toss(),2).
happens(toss(), 2).


% 
% Happens(Toss(),3).
happens(toss(), 3).


% 
% ectest/ec_reader_test_examples.e:7919
% 
% ; prune models irrelevant to example:
% HoldsAt(ItsHeads(),0).
holds_at(itsHeads(), 0).


% 
% HoldsAt(ItsHeads(),4).
holds_at(itsHeads(), 4).


% 
% 
% completion Happens
completion(happens).


% ectest/ec_reader_test_examples.e:7925
% 
% range time 0 4
range(time, 0, 4).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% ectest/ec_reader_test_examples.e:7931
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Shanahan1999/ChessBoard.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; due to Raymond Reiter
% ;
% ; @inproceedings{KarthaLifschitz:1994,
% ;   author = "G. Neelakantan Kartha and Vladimir Lifschitz",
% ;   year = "1994",
% ;   title = "Actions with indirect effects (preliminary report)",
% ;   editor = "Jon Doyle and Erik Sandewall and Pietro Torasso",
% ;   booktitle = "\uppercase{P}roceedings of the \uppercase{F}ourth \uppercase{I}nternational \uppercase{C}onference on \uppercase{P}rinciples of \uppercase{K}nowledge \uppercase{R}epresentation and \uppercase{R}easoning",
% ;   pages = "341--350",
% ;   address = "San Francisco",
% ;   publisher = "Morgan Kaufmann",
% ; }
% ;
% ; @incollection{Shanahan:1999,
% ;   author = "Shanahan, Murray",
% ;   year = "1999",
% ;   title = "The Event Calculus explained",
% ;   editor = "Michael J. Wooldridge and Manuela M. Veloso",
% ;   booktitle = "Artificial Intelligence Today: Recent Trends and Developments",
% ;   series = "Lecture Notes in Computer Science",
% ;   volume = "1600",
% ;   pages = "409--430",
% ;   address = "Berlin",
% ;   publisher = "Springer",
% ; }
% ;
% ; model finding
% ;
% ; modifications from Shanahan's formulation:
% ; InitiallyN -> !HoldsAt
% ; pruning of models irrelevant to example
% ; timestamps
% ;
% ectest/ec_reader_test_examples.e:7979
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% event Throw()
event(throw()).


% fluent ItsBlack()
fluent(itsBlack()).


% ectest/ec_reader_test_examples.e:7985
% fluent ItsWhite()
fluent(itsWhite()).


% fluent OnBlack()
fluent(onBlack()).


% fluent OnWhite()
fluent(onWhite()).


% noninertial ItsBlack, ItsWhite
noninertial([itsBlack, itsWhite]).


% 
% ectest/ec_reader_test_examples.e:7990
% [time]% 
% HoldsAt(ItsWhite(),time) ->
% Initiates(Throw(),OnWhite(),time).
holds_at(itsWhite(), Time) ->
	initiates(throw(), onWhite(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7994
% [time]% 
% HoldsAt(ItsBlack(),time) ->
% Initiates(Throw(),OnBlack(),time).
holds_at(itsBlack(), Time) ->
	initiates(throw(), onBlack(), Time).


% 
% 
% ectest/ec_reader_test_examples.e:7998
% [time] % HoldsAt(ItsWhite(),time) | HoldsAt(ItsBlack(),time).
(   holds_at(itsWhite(), Time)
;   holds_at(itsBlack(), Time)
).


% 
% 
% !HoldsAt(OnWhite(),0).
not(holds_at(onWhite(), 0)).


% 
% !HoldsAt(OnBlack(),0).
not(holds_at(onBlack(), 0)).


% 
% Happens(Throw(),1).
happens(throw(), 1).


% 
% 
% ; prune models irrelevant to example:
% ectest/ec_reader_test_examples.e:8005
% HoldsAt(ItsWhite(),0).
holds_at(itsWhite(), 0).


% 
% HoldsAt(ItsBlack(),0).
holds_at(itsBlack(), 0).


% 
% HoldsAt(ItsWhite(),2).
holds_at(itsWhite(), 2).


% 
% HoldsAt(ItsBlack(),2).
holds_at(itsBlack(), 2).


% 
% 
% completion Happens
completion(happens).


% ectest/ec_reader_test_examples.e:8011
% 
% range time 0 2
range(time, 0, 2).


% range offset 1 1
range(offset, 1, 1).


% 
% ; End of file.
% 
% ectest/ec_reader_test_examples.e:8017
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/Shanahan1999/RussianTurkey.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @book{Sandewall:1994,
% ;   author = "Sandewall, Erik",
% ;   year = "1994",
% ;   title = "Features and Fluents: The Representation of Knowledge about Dynamical Systems",
% ;   volume = "I",
% ;   address = "Oxford",
% ;   publisher = "Oxford University Press",
% ; }
% ;
% ; @incollection{Shanahan:1999,
% ;   author = "Shanahan, Murray",
% ;   year = "1999",
% ;   title = "The Event Calculus explained",
% ;   editor = "Michael J. Wooldridge and Manuela M. Veloso",
% ;   booktitle = "Artificial Intelligence Today: Recent Trends and Developments",
% ;   series = "Lecture Notes in Computer Science",
% ;   volume = "1600",
% ;   pages = "409--430",
% ;   address = "Berlin",
% ;   publisher = "Springer",
% ; }
% ;
% ; model finding
% ;
% ; modifications from Shanahan's formulation:
% ; InitiallyP -> HoldsAt
% ; added [time] Terminates(Shoot(),Loaded(),time).
% ; added !HoldsAt(Loaded(),0) to prune models
% ; timestamps
% ;
% ectest/ec_reader_test_examples.e:8062
% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% 
% event Load()
event(load()).


% event Shoot()
event(shoot()).


% ectest/ec_reader_test_examples.e:8068
% event Spin()
event(spin()).


% fluent Loaded()
fluent(loaded()).


% fluent Alive()
fluent(alive()).


% 
% ectest/ec_reader_test_examples.e:8072
% [time] % Initiates(Load(),Loaded(),time).
initiates(load(), loaded(), Time).


% 
% ectest/ec_reader_test_examples.e:8073
% [time] % HoldsAt(Loaded(),time) -> Terminates(Shoot(),Alive(),time).
holds_at(loaded(), Time) ->
	terminates(shoot(), alive(), Time).


% 
% ectest/ec_reader_test_examples.e:8074
% [time] % Releases(Spin(),Loaded(),time).
releases(spin(), loaded(), Time).


% 
% ectest/ec_reader_test_examples.e:8075
% [time] % Terminates(Shoot(),Loaded(),time).
terminates(shoot(), loaded(), Time).


% 
% 
% HoldsAt(Alive(),0).
holds_at(alive(), 0).


% 
% !HoldsAt(Loaded(),0).
not(holds_at(loaded(), 0)).


% 
% Happens(Load(),1).
happens(load(), 1).


% 
% Happens(Spin(),2).
happens(spin(), 2).


% 
% ectest/ec_reader_test_examples.e:8081
% Happens(Shoot(),3).
happens(shoot(), 3).


% 
% 
% completion Happens
completion(happens).


% 
% range time 0 4
range(time, 0, 4).


% range offset 1 1
range(offset, 1, 1).


% ectest/ec_reader_test_examples.e:8087
% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/AkmanEtAl2004/ZooTest4.2.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{Akman:2004,
% ;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
% ;   year = "2004",
% ;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
% ;   journal = "Artificial Intelligence",
% ;   volume = "153",
% ;   pages = "105--140",
% ; }
% ;
% ectest/ec_reader_test_examples.e:8114
% 
% option encoding 3
option(encoding, 3).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% load examples/AkmanEtAl2004/ZooWorld.e
load('examples/AkmanEtAl2004/ZooWorld.e').


% ectest/ec_reader_test_examples.e:8120
% 
% human Homer
t(human, homer).


% elephant Jumbo
t(elephant, jumbo).


% 
% Species(Homer)=HumanSpecies.
species(homer)=humanSpecies.


% 
% Adult(Homer).
adult(homer).


% 
% ectest/ec_reader_test_examples.e:8126
% Species(Jumbo)=ElephantSpecies.
species(jumbo)=elephantSpecies.


% 
% Adult(Jumbo).
adult(jumbo).


% 
% 
% !HoldsAt(Opened(GateAO),0).
not(holds_at(opened(gateAO), 0)).


% 
% ectest/ec_reader_test_examples.e:8130
% {position} % HoldsAt(Pos(Homer,position),0) & Outside=Loc(position).
exists([Position],  (holds_at(pos(homer, Position), 0), outside=loc(Position))).


% 
% ectest/ec_reader_test_examples.e:8131
% {position} % HoldsAt(Pos(Jumbo,position),0) & CageA=Loc(position).
exists([Position],  (holds_at(pos(jumbo, Position), 0), cageA=loc(Position))).


% 
% 
% ectest/ec_reader_test_examples.e:8133
% {position} % HoldsAt(Pos(Homer,position),5) & CageA=Loc(position).
exists([Position],  (holds_at(pos(homer, Position), 5), cageA=loc(Position))).


% 
% ectest/ec_reader_test_examples.e:8134
% {position} % HoldsAt(Pos(Jumbo,position),5) & Outside=Loc(position).
exists([Position],  (holds_at(pos(jumbo, Position), 5), outside=loc(Position))).


% 
% 
% ectest/ec_reader_test_examples.e:8136
% [animal,time] % !HoldsAt(Mounted(Homer,animal),time).
not(holds_at(mounted(homer, Animal), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:8138
% [human] % HoldsAt(PosDeterminingFluent(human,1),5).
holds_at(posDeterminingFluent(Human, 1), 5).


% 
% ectest/ec_reader_test_examples.e:8139
% [event,animal] % !HoldsAt(DoneBy(event,animal),5).
not(holds_at(doneBy(Event, Animal), 5)).


% 
% 
% ;HoldsAt(Pos(Homer,7),0).
% ;HoldsAt(Pos(Jumbo,4),0).
% ;Happens(Move(Jumbo,3),0).
% ;Happens(Open(Homer,GateAO),0).
% ;Happens(Move(Homer,4),1).
% ;Happens(Move(Jumbo,1),1).
% ;Happens(Move(Jumbo,3),2).
% ;Happens(Mount(Homer,Jumbo),2).
% ;Happens(Move(Jumbo,4),3).
% ;!Happens(Move(Homer,2),3).
% ;Happens(Move(Jumbo,7),4).
% ;!Happens(Mount(Homer,Jumbo),3).
% ;!Happens(Mount(Homer,Jumbo),4).
% ;[position] !Happens(Move(Homer,position),4).
% ectest/ec_reader_test_examples.e:8155
% 
% range time 0 5
range(time, 0, 5).


% range position 1 8
range(position, 1, 8).


% range offset 0 0
range(offset, 0, 0).


% 
% completion Happens
completion(happens).


% ectest/ec_reader_test_examples.e:8161
% 
% ; End of file.
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/AkmanEtAl2004/ZooTest5.1.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{Akman:2004,
% ;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
% ;   year = "2004",
% ;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
% ;   journal = "Artificial Intelligence",
% ;   volume = "153",
% ;   pages = "105--140",
% ; }
% ;
% ectest/ec_reader_test_examples.e:8188
% 
% option encoding 3
option(encoding, 3).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% load examples/AkmanEtAl2004/ZooWorld.e
load('examples/AkmanEtAl2004/ZooWorld.e').


% ectest/ec_reader_test_examples.e:8194
% 
% human Homer
t(human, homer).


% elephant Jumbo
t(elephant, jumbo).


% horse Silver
t(horse, silver).


% 
% Species(Homer)=HumanSpecies.
species(homer)=humanSpecies.


% 
% ectest/ec_reader_test_examples.e:8200
% Adult(Homer).
adult(homer).


% 
% Species(Jumbo)=ElephantSpecies.
species(jumbo)=elephantSpecies.


% 
% Adult(Jumbo).
adult(jumbo).


% 
% Species(Silver)=HorseSpecies.
species(silver)=horseSpecies.


% 
% Adult(Silver).
adult(silver).


% 
% 
% ectest/ec_reader_test_examples.e:8206
% {position}% 
% !HoldsAt(Pos(Homer,position),0) &
% HoldsAt(Pos(Jumbo,position),0) &
% HoldsAt(Pos(Homer,position),1) &
% !HoldsAt(Pos(Jumbo,position),1).
exists([Position],  (not(holds_at(pos(homer, Position), 0)), holds_at(pos(jumbo, Position), 0), holds_at(pos(homer, Position), 1), not(holds_at(pos(jumbo, Position), 1)))).


% 
% HoldsAt(Mounted(Homer,Silver),0).
holds_at(mounted(homer, silver), 0).


% 
% ectest/ec_reader_test_examples.e:8212
% 
% option manualrelease on
option(manualrelease, on).


% ectest/ec_reader_test_examples.e:8214
% [human, animal] % !ReleasedAt(Mounted(human, animal),0).
not(releasedAt(mounted(Human, Animal), 0)).


% 
% ectest/ec_reader_test_examples.e:8215
% [gate] % !ReleasedAt(Opened(gate),0).
not(releasedAt(opened(Gate), 0)).


% 
% ectest/ec_reader_test_examples.e:8216
% [position] % ReleasedAt(Pos(Homer,position),0).
releasedAt(pos(homer, Position), 0).


% 
% ectest/ec_reader_test_examples.e:8217
% [position] % !ReleasedAt(Pos(Jumbo,position),0).
not(releasedAt(pos(jumbo, Position), 0)).


% 
% ectest/ec_reader_test_examples.e:8218
% [position] % !ReleasedAt(Pos(Silver,position),0).
not(releasedAt(pos(silver, Position), 0)).


% 
% 
% ectest/ec_reader_test_examples.e:8220
% [human] % HoldsAt(PosDeterminingFluent(human,1),1).
holds_at(posDeterminingFluent(Human, 1), 1).


% 
% ectest/ec_reader_test_examples.e:8221
% [event,animal] % !HoldsAt(DoneBy(event,animal),1).
not(holds_at(doneBy(Event, Animal), 1)).


% 
% 
% ;HoldsAt(Opened(GateAO),0).
% ;HoldsAt(Pos(Homer,3),0).
% ;HoldsAt(Pos(Jumbo,2),0).
% ;HoldsAt(Pos(Silver,3),0).
% ;Happens(Move(Jumbo,4),0).
% ;Happens(ThrowOff(Silver,Homer),0).
% ;HoldsAt(PosDeterminingFluent(Homer,2),0).
% ectest/ec_reader_test_examples.e:8230
% 
% range time 0 1
range(time, 0, 1).


% range position 1 8
range(position, 1, 8).


% range offset 0 0
range(offset, 0, 0).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:8236
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/AkmanEtAl2004/ZooTest3.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{Akman:2004,
% ;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
% ;   year = "2004",
% ;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
% ;   journal = "Artificial Intelligence",
% ;   volume = "153",
% ;   pages = "105--140",
% ; }
% ;
% ectest/ec_reader_test_examples.e:8261
% 
% option encoding 3
option(encoding, 3).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% load examples/AkmanEtAl2004/ZooWorld.e
load('examples/AkmanEtAl2004/ZooWorld.e').


% ectest/ec_reader_test_examples.e:8267
% 
% human Homer
t(human, homer).


% dog Snoopy
t(dog, snoopy).


% 
% Species(Homer)=HumanSpecies.
species(homer)=humanSpecies.


% 
% Adult(Homer).
adult(homer).


% 
% ectest/ec_reader_test_examples.e:8273
% Species(Snoopy)=DogSpecies.
species(snoopy)=dogSpecies.


% 
% Adult(Snoopy).
adult(snoopy).


% 
% 
% !HoldsAt(Opened(GateAO),0).
not(holds_at(opened(gateAO), 0)).


% 
% ectest/ec_reader_test_examples.e:8277
% {position} % HoldsAt(Pos(Homer,position),0) & Outside=Loc(position).
exists([Position],  (holds_at(pos(homer, Position), 0), outside=loc(Position))).


% 
% ectest/ec_reader_test_examples.e:8278
% {position} % HoldsAt(Pos(Snoopy,position),0) & CageA=Loc(position).
exists([Position],  (holds_at(pos(snoopy, Position), 0), cageA=loc(Position))).


% 
% 
% ectest/ec_reader_test_examples.e:8280
% {position} % HoldsAt(Pos(Homer,position),2) & CageA=Loc(position).
exists([Position],  (holds_at(pos(homer, Position), 2), cageA=loc(Position))).


% 
% ectest/ec_reader_test_examples.e:8281
% {position} % HoldsAt(Pos(Snoopy,position),2) & Outside=Loc(position).
exists([Position],  (holds_at(pos(snoopy, Position), 2), outside=loc(Position))).


% 
% 
% ectest/ec_reader_test_examples.e:8283
% [human] % HoldsAt(PosDeterminingFluent(human,1),2).
holds_at(posDeterminingFluent(Human, 1), 2).


% 
% ectest/ec_reader_test_examples.e:8284
% [event,animal] % !HoldsAt(DoneBy(event,animal),2).
not(holds_at(doneBy(Event, Animal), 2)).


% 
% 
% range time 0 2
range(time, 0, 2).


% range position 1 8
range(position, 1, 8).


% range offset 0 0
range(offset, 0, 0).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:8291
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/AkmanEtAl2004/ZooWorld.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{Akman:2004,
% ;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
% ;   year = "2004",
% ;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
% ;   journal = "Artificial Intelligence",
% ;   volume = "153",
% ;   pages = "105--140",
% ; }
% ;
% ectest/ec_reader_test_examples.e:8316
% 
% sort position: integer
subsort(position, integer).


% sort location
sort(location).


% sort cage: location
subsort(cage, location).


% sort gate
sort(gate).


% sort animal
sort(animal).


% ectest/ec_reader_test_examples.e:8322
% sort elephant: animal
subsort(elephant, animal).


% sort horse: animal
subsort(horse, animal).


% sort dog: animal
subsort(dog, animal).


% sort human: animal
subsort(human, animal).


% sort species
sort(species).


% 
% ectest/ec_reader_test_examples.e:8328
% function Loc(position): location
function(loc(position), location).


% function Side1(gate): position
function(side1(gate), position).


% function Side2(gate): position
function(side2(gate), position).


% function Species(animal): species
function(species(animal), species).


% 
% predicate Accessible(position,position,time)
predicate(accessible(position, position, time)).


% ectest/ec_reader_test_examples.e:8334
% predicate Adult(animal)
predicate(adult(animal)).


% predicate Large(animal)
predicate(large(animal)).


% predicate LargeSpecies(species)
predicate(largeSpecies(species)).


% predicate Neighbor(position,position)
predicate(neighbor(position, position)).


% predicate Sides(position,position,gate)
predicate(sides(position, position, gate)).


% 
% ectest/ec_reader_test_examples.e:8340
% event Close(human,gate)
event(close(human, gate)).


% event GetOff(human,animal)
event(getOff(human, animal)).


% event Mount(human,animal)
event(mount(human, animal)).


% event Move(animal,position)
event(move(animal, position)).


% event Open(human,gate)
event(open(human, gate)).


% event ThrowOff(animal,human)
event(throwOff(animal, human)).


% ectest/ec_reader_test_examples.e:8346
% 
% fluent AbnormalEncroachment(human)
fluent(abnormalEncroachment(human)).


% noninertial AbnormalEncroachment
noninertial(abnormalEncroachment).


% fluent DoneBy(event,animal)
fluent(doneBy(event, animal)).


% noninertial DoneBy
noninertial(doneBy).


% fluent Mounted(human,animal)
fluent(mounted(human, animal)).


% ectest/ec_reader_test_examples.e:8352
% fluent MountFails(human)
fluent(mountFails(human)).


% noninertial MountFails
noninertial(mountFails).


% fluent Moves(animal)
fluent(moves(animal)).


% noninertial Moves
noninertial(moves).


% fluent Opened(gate)
fluent(opened(gate)).


% fluent Pos(animal,position)
fluent(pos(animal, position)).


% ectest/ec_reader_test_examples.e:8358
% fluent PosDeterminingFluent(human,position)
fluent(posDeterminingFluent(human, position)).


% noninertial PosDeterminingFluent
noninertial(posDeterminingFluent).


% fluent ThrowOffFails(animal,human)
fluent(throwOffFails(animal, human)).


% noninertial ThrowOffFails
noninertial(throwOffFails).


% 
% species HumanSpecies, ElephantSpecies, HorseSpecies, DogSpecies
t(species, humanSpecies).


t(species, elephantSpecies).


t(species, horseSpecies).


t(species, dogSpecies).


% ectest/ec_reader_test_examples.e:8364
% location Outside
t(location, outside).


% 
% LargeSpecies(HumanSpecies).
largeSpecies(humanSpecies).


% 
% LargeSpecies(ElephantSpecies).
largeSpecies(elephantSpecies).


% 
% LargeSpecies(HorseSpecies).
largeSpecies(horseSpecies).


% 
% !LargeSpecies(DogSpecies).
not(largeSpecies(dogSpecies)).


% 
% ectest/ec_reader_test_examples.e:8370
% 
% ectest/ec_reader_test_examples.e:8371
% [event,animal,time]% 
% HoldsAt(DoneBy(event,animal),time) <->
% (Happens(event,time) &
%  (({gate} event=Close(animal,gate)) |
%   ({animal1} event=GetOff(animal,animal1))|
%   ({animal1} event=Mount(animal,animal1))|
%   ({position} event=Move(animal,position))|
%   ({gate} event=Open(animal,gate)) |
%   ({human1} event=ThrowOff(animal,human1)))).
holds_at(doneBy(Event, Animal), Time) <->
	happens(Event, Time),
	(   exists([Gate], Event=close(Animal, Gate))
	;   exists([Animal1],
		   Event=getOff(Animal, Animal1))
	;   exists([Animal15],
		   Event=mount(Animal, Animal15))
	;   exists([Position],
		   Event=move(Animal, Position))
	;   exists([Gate7], Event=open(Animal, Gate7))
	;   exists([Human1],
		   Event=throwOff(Animal, Human1))
	).


% ectest/ec_reader_test_examples.e:8379
% 
% 
% ectest/ec_reader_test_examples.e:8381
% [event1,event2,animal,time]% 
% HoldsAt(DoneBy(event1,animal),time) &
% HoldsAt(DoneBy(event2,animal),time) ->
% event1=event2.
holds_at(doneBy(Event1, Animal), Time), holds_at(doneBy(Event2, Animal), Time) ->
	Event1=Event2.


% 
% 
% ectest/ec_reader_test_examples.e:8386
% [animal] % Large(animal) <-> (Adult(animal) & LargeSpecies(Species(animal))).
large(Animal) <->
	adult(Animal),
	largeSpecies(species(Animal)).


% 
% 
% ectest/ec_reader_test_examples.e:8388
% [position] 
% ectest/ec_reader_test_examples.e:8388
% {position1} % position1!=% position & Neighbor(position,position1).
exists([Position1],  (Position1\=Position, neighbor(Position, Position1))).


% 
% 
% ectest/ec_reader_test_examples.e:8390
% [position] % !Neighbor(position,position).
not(neighbor(Position, Position)).


% 
% 
% ectest/ec_reader_test_examples.e:8392
% [position1,position2]% 
% Neighbor(position1,position2) ->
% Neighbor(position2,position1).
neighbor(Position1, Position2) ->
	neighbor(Position2, Position1).


% 
% 
% ectest/ec_reader_test_examples.e:8396
% [cage] % cage!=% Outside.
Cage\=outside.


% 
% 
% ectest/ec_reader_test_examples.e:8398
% [position1,position2,gate]% 
% Sides(position1,position2,gate) <->
% ((Side1(gate)=position1 &
%   Side2(gate)=position2) |
%  (Side2(gate)=position1 &
%   Side1(gate)=position2)).
sides(Position1, Position2, Gate) <->
	(   side1(Gate)=Position1,
	    side2(Gate)=Position2
	;   side2(Gate)=Position1,
	    side1(Gate)=Position2
	).


% 
% ectest/ec_reader_test_examples.e:8404
% 
% ectest/ec_reader_test_examples.e:8405
% [gate] % Loc(Side1(gate))!=Loc(Side2(gate)).
loc(side1(Gate))\=loc(side2(Gate)).


% 
% 
% ectest/ec_reader_test_examples.e:8407
% [position1,position2,gate1,gate2]% 
% Sides(position1,position2,gate1) &
% Sides(position1,position2,gate2) ->
% gate1=gate2.
sides(Position1, Position2, Gate1), sides(Position1, Position2, Gate2) ->
	Gate1=Gate2.


% 
% 
% ectest/ec_reader_test_examples.e:8412
% [position1,position2,gate]% 
% Sides(position1,position2,gate) ->
% Neighbor(position1,position2).
sides(Position1, Position2, Gate) ->
	neighbor(Position1, Position2).


% 
% 
% ectest/ec_reader_test_examples.e:8416
% [position1,position2]% 
% Loc(position1) != Loc(position2) &
% Neighbor(position1,position2) ->
% ectest/ec_reader_test_examples.e:8419
% {gate}%  Sides(position1,position2,gate).
exists([Gate],  (loc(Position1)\=loc(Position2), neighbor(Position1, Position2)->sides(Position1, Position2, Gate))).


% 
% 
% ectest/ec_reader_test_examples.e:8421
% [animal,position1,position2,time]% 
% HoldsAt(Pos(animal,position1),time) &
% HoldsAt(Pos(animal,position2),time) ->
% position1=position2.
holds_at(pos(Animal, Position1), Time), holds_at(pos(Animal, Position2), Time) ->
	Position1=Position2.


% 
% 
% ectest/ec_reader_test_examples.e:8426
% [animal,time]% 
% ectest/ec_reader_test_examples.e:8427
% {position} % HoldsAt(Pos(animal,position),time).
exists([Position], holds_at(pos(Animal, Position), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:8429
% [animal1,animal2,position,time]% 
% (animal1!=animal2 &
%  Large(animal1) &
%  Large(animal2) &
%  HoldsAt(Pos(animal1,position),time) &
%  HoldsAt(Pos(animal2,position),time)) ->
% (({human} human=animal1 & HoldsAt(Mounted(human,animal2),time)) |
%  ({human} human=animal2 & HoldsAt(Mounted(human,animal1),time))).
Animal1\=Animal2, large(Animal1), large(Animal2), holds_at(pos(Animal1, Position), Time), holds_at(pos(Animal2, Position), Time) ->
	(   exists([Human],
		   (Human=Animal1, holds_at(mounted(Human, Animal2), Time)))
	;   exists([Human5],
		   (Human5=Animal2, holds_at(mounted(Human5, Animal1), Time)))
	).


% ectest/ec_reader_test_examples.e:8436
% 
% 
% ectest/ec_reader_test_examples.e:8438
% [human,position1,position2,time]% 
% HoldsAt(PosDeterminingFluent(human,position1),time) &
% HoldsAt(PosDeterminingFluent(human,position2),time) ->
% position1=position2.
holds_at(posDeterminingFluent(Human, Position1), Time), holds_at(posDeterminingFluent(Human, Position2), Time) ->
	Position1=Position2.


% 
% 
% ectest/ec_reader_test_examples.e:8443
% [animal,position,time]% 
% Initiates(Move(animal,position),Pos(animal,position),time).
initiates(move(Animal, Position), pos(Animal, Position), Time).


% 
% 
% ectest/ec_reader_test_examples.e:8446
% [animal,position1,position2,time]% 
% HoldsAt(Pos(animal,position1),time) ->
% Terminates(Move(animal,position2),Pos(animal,position1),time).
holds_at(pos(Animal, Position1), Time) ->
	terminates(move(Animal, Position2),
		   pos(Animal, Position1),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:8450
% [animal,position,time]% 
% Happens(Move(animal,position),time) ->
% !HoldsAt(Pos(animal,position),time).
happens(move(Animal, Position), Time) ->
	not(holds_at(pos(Animal, Position), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:8454
% [human,position,time]% 
% Happens(Move(human,position),time) ->
% !{animal} HoldsAt(Mounted(human,animal),time).
happens(move(Human, Position), Time) ->
	not(exists([Animal],
		   holds_at(mounted(Human, Animal), Time))).


% 
% 
% ectest/ec_reader_test_examples.e:8458
% [human,gate,time]% 
% Initiates(Open(human,gate),Opened(gate),time).
initiates(open(Human, Gate), opened(Gate), Time).


% 
% 
% ectest/ec_reader_test_examples.e:8461
% [human,gate,time]% 
% Happens(Open(human,gate),time) ->
% !HoldsAt(Opened(gate),time) &
% (!{animal} HoldsAt(Mounted(human,animal),time)) &
% ({position}
%  (Side1(gate)=position | Side2(gate)=position) &
%  HoldsAt(Pos(human,position),time)).
happens(open(Human, Gate), Time) ->
	not(holds_at(opened(Gate), Time)),
	not(exists([Animal],
		   holds_at(mounted(Human, Animal), Time))),
	exists([Position],
	       ((side1(Gate)=Position;side2(Gate)=Position), holds_at(pos(Human, Position), Time))).


% ectest/ec_reader_test_examples.e:8467
% 
% 
% ectest/ec_reader_test_examples.e:8469
% [human,gate,time]% 
% Terminates(Close(human,gate),Opened(gate),time).
terminates(close(Human, Gate), opened(Gate), Time).


% 
% 
% ectest/ec_reader_test_examples.e:8472
% [human,gate,time]% 
% Happens(Close(human,gate),time) ->
% HoldsAt(Opened(gate),time) &
% (!{animal} HoldsAt(Mounted(human,animal),time)) &
% ectest/ec_reader_test_examples.e:8476
% {position}% 
% (Side1(gate)=position | Side2(gate)=position) &
% HoldsAt(Pos(human,position),time).
exists([Position],  (happens(close(Human, Gate), Time)->holds_at(opened(Gate), Time), not(exists([Animal], holds_at(mounted(Human, Animal), Time))), (side1(Gate)=Position;side2(Gate)=Position), holds_at(pos(Human, Position), Time))).


% 
% 
% ectest/ec_reader_test_examples.e:8480
% [human,animal,position,time]% 
% HoldsAt(Mounted(human,animal),time) &
% HoldsAt(Pos(animal,position),time) ->
% HoldsAt(Pos(human,position),time).
holds_at(mounted(Human, Animal), Time), holds_at(pos(Animal, Position), Time) ->
	holds_at(pos(Human, Position), Time).


% 
% 
% ectest/ec_reader_test_examples.e:8485
% [animal,time]% 
% HoldsAt(Moves(animal),time) <->
% ({position}
%  HoldsAt(Pos(animal,position),time) &
%  !HoldsAt(Pos(animal,position),time+1)).
holds_at(moves(Animal), Time) <->
	exists([Position],
	       (holds_at(pos(Animal, Position), Time), not(holds_at(pos(Animal, Position), Time+1)))).


% 
% 
% ectest/ec_reader_test_examples.e:8491
% [human,time]% 
% HoldsAt(MountFails(human),time) <->
% ({animal}
%   Happens(Mount(human,animal),time) &
%   HoldsAt(Moves(animal),time)).
holds_at(mountFails(Human), Time) <->
	exists([Animal],
	       (happens(mount(Human, Animal), Time), holds_at(moves(Animal), Time))).


% 
% 
% ectest/ec_reader_test_examples.e:8497
% [human,animal,position,time]% 
% !HoldsAt(Moves(animal),time) ->
% Releases(Mount(human,animal),Pos(human,position),time).
not(holds_at(moves(Animal), Time)) ->
	releases(mount(Human, Animal),
		 pos(Human, Position),
		 Time).


% 
% 
% ectest/ec_reader_test_examples.e:8501
% [human,animal,time]% 
% !HoldsAt(Moves(animal),time) ->
% Initiates(Mount(human,animal),Mounted(human,animal),time).
not(holds_at(moves(Animal), Time)) ->
	initiates(mount(Human, Animal),
		  mounted(Human, Animal),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:8505
% [human,animal,position,time]% 
% HoldsAt(Pos(animal,position),time) &
% HoldsAt(Moves(animal),time) ->
% Initiates(Mount(human,animal),Pos(human,position),time).
holds_at(pos(Animal, Position), Time), holds_at(moves(Animal), Time) ->
	initiates(mount(Human, Animal),
		  pos(Human, Position),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:8510
% [human,animal,position,time]% 
% HoldsAt(Pos(human,position),time) &
% HoldsAt(Moves(animal),time) ->
% Terminates(Mount(human,animal),Pos(human,position),time).
holds_at(pos(Human, Position), Time), holds_at(moves(Animal), Time) ->
	terminates(mount(Human, Animal),
		   pos(Human, Position),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:8515
% [human,animal,time]% 
% Happens(Mount(human,animal),time) ->
% Large(animal).
happens(mount(Human, Animal), Time) ->
	large(Animal).


% 
% 
% ectest/ec_reader_test_examples.e:8519
% [human,animal,time]% 
% HoldsAt(Mounted(human,animal),time) ->
% Large(animal).
holds_at(mounted(Human, Animal), Time) ->
	large(Animal).


% 
% 
% ectest/ec_reader_test_examples.e:8523
% [human1,human2,time]% 
% Happens(Mount(human1,human2),time) ->
% !Large(human1).
happens(mount(Human1, Human2), Time) ->
	not(large(Human1)).


% 
% 
% ectest/ec_reader_test_examples.e:8527
% [human1,human2,time]% 
% HoldsAt(Mounted(human1,human2),time) ->
% !Large(human1).
holds_at(mounted(Human1, Human2), Time) ->
	not(large(Human1)).


% 
% 
% ectest/ec_reader_test_examples.e:8531
% [human,animal,time]% 
% Happens(Mount(human,animal),time) ->
% !{human1} human1!=human & HoldsAt(Mounted(human1,animal),time).
happens(mount(Human, Animal), Time) ->
	not(exists([Human1],
		   (Human1\=Human, holds_at(mounted(Human1, Animal), Time)))).


% 
% 
% ectest/ec_reader_test_examples.e:8535
% [human1,human2,animal,time]% 
% HoldsAt(Mounted(human1,animal),time) &
% HoldsAt(Mounted(human2,animal),time) ->
% human1=human2.
holds_at(mounted(Human1, Animal), Time), holds_at(mounted(Human2, Animal), Time) ->
	Human1=Human2.


% 
% 
% ectest/ec_reader_test_examples.e:8540
% [human,animal,time]% 
% Happens(Mount(human,animal),time) ->
% !{human1} human1!=human & HoldsAt(Mounted(human1,human),time).
happens(mount(Human, Animal), Time) ->
	not(exists([Human1],
		   (Human1\=Human, holds_at(mounted(Human1, Human), Time)))).


% 
% 
% ectest/ec_reader_test_examples.e:8544
% [human1,human2,time]% 
% Happens(Mount(human1,human2),time) ->
% ectest/ec_reader_test_examples.e:8546
% {animal}%  HoldsAt(Mounted(human2,animal),time).
exists([Animal],  (happens(mount(Human1, Human2), Time)->holds_at(mounted(Human2, Animal), Time))).


% 
% 
% ectest/ec_reader_test_examples.e:8548
% [human1,human2,time]% 
% HoldsAt(Mounted(human1,human2),time) ->
% !{animal} HoldsAt(Mounted(human2,animal),time).
holds_at(mounted(Human1, Human2), Time) ->
	not(exists([Animal],
		   holds_at(mounted(Human2, Animal), Time))).


% 
% 
% ectest/ec_reader_test_examples.e:8552
% [human,animal,time]% 
% Happens(Mount(human,animal),time) ->
% !{animal1} HoldsAt(Mounted(human,animal1),time).
happens(mount(Human, Animal), Time) ->
	not(exists([Animal1],
		   holds_at(mounted(Human, Animal1), Time))).


% 
% 
% ectest/ec_reader_test_examples.e:8556
% [human,animal,time]% 
% !HoldsAt(Moves(animal),time) ->
% Terminates(GetOff(human,animal),Mounted(human,animal),time).
not(holds_at(moves(Animal), Time)) ->
	terminates(getOff(Human, Animal),
		   mounted(Human, Animal),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:8560
% [human,animal,position,time]% 
% !HoldsAt(Moves(animal),time) &
% HoldsAt(PosDeterminingFluent(human,position),time) ->
% Initiates(GetOff(human,animal),Pos(human,position),time).
not(holds_at(moves(Animal), Time)), holds_at(posDeterminingFluent(Human, Position), Time) ->
	initiates(getOff(Human, Animal),
		  pos(Human, Position),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:8565
% [human,animal,position,time]% 
% !HoldsAt(Moves(animal),time) &
% HoldsAt(Pos(human,position),time) ->
% Terminates(GetOff(human,animal),Pos(human,position),time).
not(holds_at(moves(Animal), Time)), holds_at(pos(Human, Position), Time) ->
	terminates(getOff(Human, Animal),
		   pos(Human, Position),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:8570
% [human,animal,position1,position2,time]% 
% !HoldsAt(Moves(animal),time) &
% HoldsAt(Pos(human,position1),time) &
% position1!=position2 ->
% Terminates(GetOff(human,animal),Pos(human,position2),time).
not(holds_at(moves(Animal), Time)), holds_at(pos(Human, Position1), Time), Position1\=Position2 ->
	terminates(getOff(Human, Animal),
		   pos(Human, Position2),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:8576
% [human,animal,time]% 
% Happens(GetOff(human,animal),time) ->
% HoldsAt(Mounted(human,animal),time).
happens(getOff(Human, Animal), Time) ->
	holds_at(mounted(Human, Animal), Time).


% 
% 
% ectest/ec_reader_test_examples.e:8580
% [animal1,human,time]% 
% HoldsAt(ThrowOffFails(animal1,human),time) <->
% ({position,animal2}
%  animal2!=human &
%  HoldsAt(PosDeterminingFluent(human,position),time) &
%  Large(animal2) &
%  HoldsAt(Pos(animal2,position),time+1)).
holds_at(throwOffFails(Animal1, Human), Time) <->
	exists([Position, Animal2],
	       (Animal2\=Human, holds_at(posDeterminingFluent(Human, Position), Time), large(Animal2), holds_at(pos(Animal2, Position), Time+1))).


% ectest/ec_reader_test_examples.e:8586
% 
% 
% ectest/ec_reader_test_examples.e:8588
% [animal,human,position,time]% 
% HoldsAt(PosDeterminingFluent(human,position),time) &
% !HoldsAt(ThrowOffFails(animal,human),time) ->
% Initiates(ThrowOff(animal,human),Pos(human,position),time).
holds_at(posDeterminingFluent(Human, Position), Time), not(holds_at(throwOffFails(Animal, Human), Time)) ->
	initiates(throwOff(Animal, Human),
		  pos(Human, Position),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:8593
% [animal,human,position,time]% 
% HoldsAt(Pos(human,position),time) &
% !HoldsAt(ThrowOffFails(animal,human),time) ->
% Terminates(ThrowOff(animal,human),Pos(human,position),time).
holds_at(pos(Human, Position), Time), not(holds_at(throwOffFails(Animal, Human), Time)) ->
	terminates(throwOff(Animal, Human),
		   pos(Human, Position),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:8598
% [animal,human,position1,position2,time]% 
% !HoldsAt(ThrowOffFails(animal,human),time) &
% HoldsAt(Pos(human,position1),time) &
% !HoldsAt(PosDeterminingFluent(human,position2),time) &
% position1!=position2 ->
% Terminates(ThrowOff(animal,human),Pos(human,position2),time).
not(holds_at(throwOffFails(Animal, Human), Time)), holds_at(pos(Human, Position1), Time), not(holds_at(posDeterminingFluent(Human, Position2), Time)), Position1\=Position2 ->
	terminates(throwOff(Animal, Human),
		   pos(Human, Position2),
		   Time).


% 
% ectest/ec_reader_test_examples.e:8604
% 
% ectest/ec_reader_test_examples.e:8605
% [human,time]% 
% (!{animal} Happens(ThrowOff(animal,human),time) |
%            Happens(GetOff(human,animal),time)) ->
% HoldsAt(PosDeterminingFluent(human,1),time).
not(exists([Animal], happens(throwOff(Animal, Human), Time)));happens(getOff(Human, animal), Time) ->
	holds_at(posDeterminingFluent(Human, 1), Time).


% 
% 
% ectest/ec_reader_test_examples.e:8610
% [human,position,animal1,animal2,time]% 
% HoldsAt(PosDeterminingFluent(human,position),time) &
% HoldsAt(ThrowOffFails(animal1,human),time) &
% HoldsAt(Pos(animal2,position),time) ->
% Initiates(ThrowOff(animal1,human),Mounted(human,animal2),time).
holds_at(posDeterminingFluent(Human, Position), Time), holds_at(throwOffFails(Animal1, Human), Time), holds_at(pos(Animal2, Position), Time) ->
	initiates(throwOff(Animal1, Human),
		  mounted(Human, Animal2),
		  Time).


% 
% 
% ectest/ec_reader_test_examples.e:8616
% [human,animal,time]% 
% !HoldsAt(ThrowOffFails(animal,human),time) ->
% Terminates(ThrowOff(animal,human),Mounted(human,animal),time).
not(holds_at(throwOffFails(Animal, Human), Time)) ->
	terminates(throwOff(Animal, Human),
		   mounted(Human, Animal),
		   Time).


% 
% 
% ectest/ec_reader_test_examples.e:8620
% [animal,human,time]% 
% Happens(ThrowOff(animal,human),time) ->
% HoldsAt(Mounted(human,animal),time).
happens(throwOff(Animal, Human), Time) ->
	holds_at(mounted(Human, Animal), Time).


% 
% 
% ectest/ec_reader_test_examples.e:8624
% [animal,human,time]% 
% Happens(ThrowOff(animal,human),time) ->
% !Happens(GetOff(human,animal),time).
happens(throwOff(Animal, Human), Time) ->
	not(happens(getOff(Human, Animal), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:8628
% [animal,human,time]% 
% Happens(GetOff(human,animal),time) ->
% !Happens(ThrowOff(animal,human),time).
happens(getOff(Human, Animal), Time) ->
	not(happens(throwOff(Animal, Human), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:8632
% [position1,position2,time]% 
% Accessible(position1,position2,time) <->
% (Neighbor(position1,position2) &
%  !{gate} Sides(position1,position2,gate) &
%          !HoldsAt(Opened(gate),time)).
accessible(Position1, Position2, Time) <->
	thereExists((neighbor(Position1, Position2), not([gate])),
		    (sides(Position1, Position2, gate), not(holds_at(opened(gate), Time)))).


% 
% 
% ectest/ec_reader_test_examples.e:8638
% [animal,position1,position2,time]% 
% (position1!=position2 &
%  HoldsAt(Pos(animal,position1),time) &
%  HoldsAt(Pos(animal,position2),time+1)) ->
% Accessible(position1,position2,time).
Position1\=Position2, holds_at(pos(Animal, Position1), Time), holds_at(pos(Animal, Position2), Time+1) ->
	accessible(Position1, Position2, Time).


% 
% 
% ectest/ec_reader_test_examples.e:8644
% [human,time]% 
% HoldsAt(AbnormalEncroachment(human),time) <->
% (HoldsAt(MountFails(human),time) |
%  ({position,animal1,animal2}
%    HoldsAt(PosDeterminingFluent(human,position),time) &
%    !HoldsAt(ThrowOffFails(animal2,human),time) &
%    Happens(ThrowOff(animal2,human),time) &
%    animal1!=human &
%    Large(animal1) &
%    HoldsAt(Pos(animal1,position),time) &
%    !HoldsAt(Pos(animal1,position),time+1))).
holds_at(abnormalEncroachment(Human), Time) <->
	(   holds_at(mountFails(Human), Time)
	;   exists([Position, Animal1, Animal2],
		   (holds_at(posDeterminingFluent(Human, Position), Time), not(holds_at(throwOffFails(Animal2, Human), Time)), happens(throwOff(Animal2, Human), Time), Animal1\=Human, large(Animal1), holds_at(pos(Animal1, Position), Time), not(holds_at(pos(Animal1, Position), Time+1))))
	).


% ectest/ec_reader_test_examples.e:8654
% 
% 
% ectest/ec_reader_test_examples.e:8656
% [animal1,animal2,position,time]% 
% HoldsAt(Pos(animal1,position),time) &
% !HoldsAt(Pos(animal1,position),time+1) &
% !HoldsAt(Pos(animal2,position),time) &
% HoldsAt(Pos(animal2,position),time+1) ->
% (!Large(animal1) |
%  !Large(animal2) |
%  ({human} human=animal2 & HoldsAt(AbnormalEncroachment(human),time))).
holds_at(pos(Animal1, Position), Time), not(holds_at(pos(Animal1, Position), Time+1)), not(holds_at(pos(Animal2, Position), Time)), holds_at(pos(Animal2, Position), Time+1) ->
	(   not(large(Animal1))
	;   not(large(Animal2))
	;   exists([Human],
		   (Human=Animal2, holds_at(abnormalEncroachment(Human), Time)))
	).


% ectest/ec_reader_test_examples.e:8663
% 
% 
% ectest/ec_reader_test_examples.e:8665
% [animal1,animal2,position1,position2,time]% 
% animal1!=% animal2 &
% Large(animal1) & Large(animal2) &
% HoldsAt(Pos(animal1,position1),time) &
% HoldsAt(Pos(animal1,position2),time+1) &
% HoldsAt(Pos(animal2,position1),time) &
% HoldsAt(Pos(animal2,position2),time+1) ->
% !{gate} Sides(position1,position2,gate).
Animal1\=Animal2, large(Animal1), large(Animal2), holds_at(pos(Animal1, Position1), Time), holds_at(pos(Animal1, Position2), Time+1), holds_at(pos(Animal2, Position1), Time), holds_at(pos(Animal2, Position2), Time+1) ->
	not(exists([Gate],
		   sides(Position1, Position2, Gate))).


% ectest/ec_reader_test_examples.e:8672
% 
% 
% ectest/ec_reader_test_examples.e:8674
% [animal1,animal2,position1,position2,time]% 
% animal1!=% animal2 &
% Large(animal1) & Large(animal2) &
% HoldsAt(Pos(animal1,position1),time) &
% HoldsAt(Pos(animal1,position2),time+1) &
% HoldsAt(Pos(animal2,position2),time) &
% HoldsAt(Pos(animal2,position1),time+1) ->
% !{gate} Sides(position1,position2,gate).
Animal1\=Animal2, large(Animal1), large(Animal2), holds_at(pos(Animal1, Position1), Time), holds_at(pos(Animal1, Position2), Time+1), holds_at(pos(Animal2, Position2), Time), holds_at(pos(Animal2, Position1), Time+1) ->
	not(exists([Gate],
		   sides(Position1, Position2, Gate))).


% ectest/ec_reader_test_examples.e:8681
% 
% 
% ectest/ec_reader_test_examples.e:8683
% [gate,position1,position2,time]% 
% HoldsAt(Opened(gate),time) &
% !HoldsAt(Opened(gate),time+1) &
% Sides(position1,position2,gate) ->
% !{animal}
% HoldsAt(Pos(animal,position1),time) &
% HoldsAt(Pos(animal,position2),time+1).
holds_at(opened(Gate), Time), not(holds_at(opened(Gate), Time+1)), sides(Position1, Position2, Gate) ->
	not(exists([Animal],
		   (holds_at(pos(Animal, Position1), Time), holds_at(pos(Animal, Position2), Time+1)))).


% ectest/ec_reader_test_examples.e:8689
% 
% 
% gate GateAO
t(gate, gateAO).


% cage CageA
t(cage, cageA).


% 
% Loc(1)=CageA.
loc(1)=cageA.


% 
% ectest/ec_reader_test_examples.e:8695
% Loc(2)=CageA.
loc(2)=cageA.


% 
% Loc(3)=CageA.
loc(3)=cageA.


% 
% Loc(4)=CageA.
loc(4)=cageA.


% 
% Loc(5)=Outside.
loc(5)=outside.


% 
% Loc(6)=Outside.
loc(6)=outside.


% 
% Loc(7)=Outside.
loc(7)=outside.


% 
% ectest/ec_reader_test_examples.e:8701
% Loc(8)=Outside.
loc(8)=outside.


% 
% 
% ectest/ec_reader_test_examples.e:8703
% [position1,position2]% 
% Neighbor(position1,position2) <->
% ((position1=1 & position2=2) |
%  (position1=1 & position2=3) |
%  (position1=1 & position2=4) |
%  (position1=2 & position2=3) |
%  (position1=2 & position2=4) |
%  (position1=3 & position2=4) |
%  (position1=5 & position2=6) |
%  (position1=5 & position2=7) |
%  (position1=5 & position2=8) |
%  (position1=6 & position2=7) |
%  (position1=6 & position2=8) |
%  (position1=7 & position2=8) |
%  (position2=1 & position1=2) |
%  (position2=1 & position1=3) |
%  (position2=1 & position1=4) |
%  (position2=2 & position1=3) |
%  (position2=2 & position1=4) |
%  (position2=3 & position1=4) |
%  (position2=5 & position1=6) |
%  (position2=5 & position1=7) |
%  (position2=5 & position1=8) |
%  (position2=6 & position1=7) |
%  (position2=6 & position1=8) |
%  (position2=7 & position1=8) |
%  (position1=4 & position2=7) |
%  (position2=4 & position1=7)).
neighbor(Position1, Position2) <->
	(   Position1=1,
	    Position2=2
	;   Position1=1,
	    Position2=3
	;   Position1=1,
	    Position2=4
	;   Position1=2,
	    Position2=3
	;   Position1=2,
	    Position2=4
	;   Position1=3,
	    Position2=4
	;   Position1=5,
	    Position2=6
	;   Position1=5,
	    Position2=7
	;   Position1=5,
	    Position2=8
	;   Position1=6,
	    Position2=7
	;   Position1=6,
	    Position2=8
	;   Position1=7,
	    Position2=8
	;   Position2=1,
	    Position1=2
	;   Position2=1,
	    Position1=3
	;   Position2=1,
	    Position1=4
	;   Position2=2,
	    Position1=3
	;   Position2=2,
	    Position1=4
	;   Position2=3,
	    Position1=4
	;   Position2=5,
	    Position1=6
	;   Position2=5,
	    Position1=7
	;   Position2=5,
	    Position1=8
	;   Position2=6,
	    Position1=7
	;   Position2=6,
	    Position1=8
	;   Position2=7,
	    Position1=8
	;   Position1=4,
	    Position2=7
	;   Position2=4,
	    Position1=7
	).


% ectest/ec_reader_test_examples.e:8730
% 
% 
% Side1(GateAO)=4.
side1(gateAO)=4.


% 
% Side2(GateAO)=7.
side2(gateAO)=7.


% 
% 
% ; End of file.
% ectest/ec_reader_test_examples.e:8736
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/AkmanEtAl2004/ZooTest4.1.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{Akman:2004,
% ;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
% ;   year = "2004",
% ;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
% ;   journal = "Artificial Intelligence",
% ;   volume = "153",
% ;   pages = "105--140",
% ; }
% ;
% ectest/ec_reader_test_examples.e:8761
% 
% option encoding 3
option(encoding, 3).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% load examples/AkmanEtAl2004/ZooWorld.e
load('examples/AkmanEtAl2004/ZooWorld.e').


% ectest/ec_reader_test_examples.e:8767
% 
% human Homer
t(human, homer).


% elephant Jumbo
t(elephant, jumbo).


% 
% Species(Homer)=HumanSpecies.
species(homer)=humanSpecies.


% 
% Adult(Homer).
adult(homer).


% 
% ectest/ec_reader_test_examples.e:8773
% Species(Jumbo)=ElephantSpecies.
species(jumbo)=elephantSpecies.


% 
% Adult(Jumbo).
adult(jumbo).


% 
% 
% !HoldsAt(Opened(GateAO),0).
not(holds_at(opened(gateAO), 0)).


% 
% ectest/ec_reader_test_examples.e:8777
% {position} % HoldsAt(Pos(Homer,position),0) & Outside=Loc(position).
exists([Position],  (holds_at(pos(homer, Position), 0), outside=loc(Position))).


% 
% ectest/ec_reader_test_examples.e:8778
% {position} % HoldsAt(Pos(Jumbo,position),0) & CageA=Loc(position).
exists([Position],  (holds_at(pos(jumbo, Position), 0), cageA=loc(Position))).


% 
% 
% ectest/ec_reader_test_examples.e:8780
% {position} % HoldsAt(Pos(Homer,position),4) & CageA=Loc(position).
exists([Position],  (holds_at(pos(homer, Position), 4), cageA=loc(Position))).


% 
% ectest/ec_reader_test_examples.e:8781
% {position} % HoldsAt(Pos(Jumbo,position),4) & Outside=Loc(position).
exists([Position],  (holds_at(pos(jumbo, Position), 4), outside=loc(Position))).


% 
% 
% ectest/ec_reader_test_examples.e:8783
% [human] % HoldsAt(PosDeterminingFluent(human,1),4).
holds_at(posDeterminingFluent(Human, 1), 4).


% 
% ectest/ec_reader_test_examples.e:8784
% [event,animal] % !HoldsAt(DoneBy(event,animal),4).
not(holds_at(doneBy(Event, Animal), 4)).


% 
% 
% ; ccalc.2.0b.8.3 single model
% ;HoldsAt(Pos(Homer,7),0).
% ;HoldsAt(Pos(Jumbo,2),0).
% ;Happens(Move(Jumbo,4),0).
% ;Happens(Open(Homer,GateAO),0).
% ;Happens(Mount(Homer,Jumbo),1).
% ;Happens(ThrowOff(Jumbo,Homer),2).
% ;HoldsAt(PosDeterminingFluent(Homer,1),2).
% ;Happens(Move(Jumbo,7),3).
% ;Happens(Mount(Homer,Jumbo),3).
% ectest/ec_reader_test_examples.e:8796
% 
% range time 0 4
range(time, 0, 4).


% range position 1 8
range(position, 1, 8).


% range offset 0 0
range(offset, 0, 0).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:8802
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/AkmanEtAl2004/ZooTest2.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{Akman:2004,
% ;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
% ;   year = "2004",
% ;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
% ;   journal = "Artificial Intelligence",
% ;   volume = "153",
% ;   pages = "105--140",
% ; }
% ;
% ectest/ec_reader_test_examples.e:8827
% 
% option encoding 3
option(encoding, 3).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% load examples/AkmanEtAl2004/ZooWorld.e
load('examples/AkmanEtAl2004/ZooWorld.e').


% ectest/ec_reader_test_examples.e:8833
% 
% human Homer
t(human, homer).


% 
% Species(Homer)=HumanSpecies.
species(homer)=humanSpecies.


% 
% Adult(Homer).
adult(homer).


% 
% 
% ectest/ec_reader_test_examples.e:8839
% !HoldsAt(Opened(GateAO),0).
not(holds_at(opened(gateAO), 0)).


% 
% ectest/ec_reader_test_examples.e:8840
% {position} % HoldsAt(Pos(Homer,position),0) & Outside=Loc(position).
exists([Position],  (holds_at(pos(homer, Position), 0), outside=loc(Position))).


% 
% ectest/ec_reader_test_examples.e:8841
% {position} % HoldsAt(Pos(Homer,position),2) & CageA=Loc(position).
exists([Position],  (holds_at(pos(homer, Position), 2), cageA=loc(Position))).


% 
% 
% ectest/ec_reader_test_examples.e:8843
% [human] % HoldsAt(PosDeterminingFluent(human,1),2).
holds_at(posDeterminingFluent(Human, 1), 2).


% 
% ectest/ec_reader_test_examples.e:8844
% [event,animal] % !HoldsAt(DoneBy(event,animal),2).
not(holds_at(doneBy(Event, Animal), 2)).


% 
% 
% range time 0 2
range(time, 0, 2).


% range position 1 8
range(position, 1, 8).


% range offset 0 0
range(offset, 0, 0).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:8851
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/AkmanEtAl2004/ZooTest6.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{Akman:2004,
% ;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
% ;   year = "2004",
% ;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
% ;   journal = "Artificial Intelligence",
% ;   volume = "153",
% ;   pages = "105--140",
% ; }
% ;
% ectest/ec_reader_test_examples.e:8876
% 
% option encoding 3
option(encoding, 3).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% load examples/AkmanEtAl2004/ZooWorld.e
load('examples/AkmanEtAl2004/ZooWorld.e').


% ectest/ec_reader_test_examples.e:8882
% 
% human Homer
t(human, homer).


% elephant Jumbo
t(elephant, jumbo).


% 
% Species(Homer)=HumanSpecies.
species(homer)=humanSpecies.


% 
% Adult(Homer).
adult(homer).


% 
% ectest/ec_reader_test_examples.e:8888
% Species(Jumbo)=ElephantSpecies.
species(jumbo)=elephantSpecies.


% 
% Adult(Jumbo).
adult(jumbo).


% 
% 
% HoldsAt(Mounted(Homer,Jumbo),0).
holds_at(mounted(homer, jumbo), 0).


% 
% HoldsAt(Pos(Jumbo,1),0).
holds_at(pos(jumbo, 1), 0).


% 
% Happens(ThrowOff(Jumbo,Homer),0).
happens(throwOff(jumbo, homer), 0).


% 
% ectest/ec_reader_test_examples.e:8894
% 
% option manualrelease on
option(manualrelease, on).


% ectest/ec_reader_test_examples.e:8896
% [human, animal] % !ReleasedAt(Mounted(human, animal),0).
not(releasedAt(mounted(Human, Animal), 0)).


% 
% ectest/ec_reader_test_examples.e:8897
% [gate] % !ReleasedAt(Opened(gate),0).
not(releasedAt(opened(Gate), 0)).


% 
% ectest/ec_reader_test_examples.e:8898
% [position] % ReleasedAt(Pos(Homer,position),0).
releasedAt(pos(homer, Position), 0).


% 
% ectest/ec_reader_test_examples.e:8899
% [position] % !ReleasedAt(Pos(Jumbo,position),0).
not(releasedAt(pos(jumbo, Position), 0)).


% 
% 
% ectest/ec_reader_test_examples.e:8901
% [human] % HoldsAt(PosDeterminingFluent(human,1),1).
holds_at(posDeterminingFluent(Human, 1), 1).


% 
% ectest/ec_reader_test_examples.e:8902
% [event,animal] % !HoldsAt(DoneBy(event,animal),1).
not(holds_at(doneBy(Event, Animal), 1)).


% 
% 
% range time 0 1
range(time, 0, 1).


% range position 1 8
range(position, 1, 8).


% range offset 0 0
range(offset, 0, 0).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:8909
% 
% 
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/AkmanEtAl2004/ZooTest1.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{Akman:2004,
% ;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
% ;   year = "2004",
% ;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
% ;   journal = "Artificial Intelligence",
% ;   volume = "153",
% ;   pages = "105--140",
% ; }
% ;
% ectest/ec_reader_test_examples.e:8934
% 
% option encoding 3
option(encoding, 3).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% load examples/AkmanEtAl2004/ZooWorld.e
load('examples/AkmanEtAl2004/ZooWorld.e').


% ectest/ec_reader_test_examples.e:8940
% 
% human Homer
t(human, homer).


% elephant Jumbo
t(elephant, jumbo).


% 
% Species(Homer)=HumanSpecies.
species(homer)=humanSpecies.


% 
% Adult(Homer).
adult(homer).


% 
% ectest/ec_reader_test_examples.e:8946
% Species(Jumbo)=ElephantSpecies.
species(jumbo)=elephantSpecies.


% 
% Adult(Jumbo).
adult(jumbo).


% 
% 
% !HoldsAt(Opened(GateAO),0).
not(holds_at(opened(gateAO), 0)).


% 
% HoldsAt(Pos(Homer,6),0).
holds_at(pos(homer, 6), 0).


% 
% ectest/ec_reader_test_examples.e:8951
% [time] % HoldsAt(Pos(Jumbo,1),time).
holds_at(pos(jumbo, 1), Time).


% 
% 
% ; goal
% HoldsAt(Mounted(Homer,Jumbo),4).
holds_at(mounted(homer, jumbo), 4).


% 
% 
% ;ABDUCE
% ;Happens(Move(Homer,7),0).
% ;Happens(Open(Homer,GateAO),1).
% ;Happens(Move(Homer,4),2).
% ;Happens(Mount(Homer,Jumbo),3).
% ectest/ec_reader_test_examples.e:8961
% 
% ectest/ec_reader_test_examples.e:8962
% [human] % HoldsAt(PosDeterminingFluent(human,1),4).
holds_at(posDeterminingFluent(Human, 1), 4).


% 
% ectest/ec_reader_test_examples.e:8963
% [event,animal] % !HoldsAt(DoneBy(event,animal),4).
not(holds_at(doneBy(Event, Animal), 4)).


% 
% 
% range time 0 4
range(time, 0, 4).


% range position 1 8
range(position, 1, 8).


% range offset 0 0
range(offset, 0, 0).


% 
% ectest/ec_reader_test_examples.e:8969
% option timediff off
option(timediff, off).


% option modeldiff on
option(modeldiff, on).


% 
% ; End of file.
% 
% 
% ectest/ec_reader_test_examples.e:8975
% 
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
% ; FILE: examples/AkmanEtAl2004/ZooTest5.2.e
% ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
% ; @article{Akman:2004,
% ;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
% ;   year = "2004",
% ;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
% ;   journal = "Artificial Intelligence",
% ;   volume = "153",
% ;   pages = "105--140",
% ; }
% ;
% ectest/ec_reader_test_examples.e:8998
% 
% option encoding 3
option(encoding, 3).


% 
% load foundations/Root.e
load('foundations/Root.e').


% load foundations/EC.e
load('foundations/EC.e').


% load examples/AkmanEtAl2004/ZooWorld.e
load('examples/AkmanEtAl2004/ZooWorld.e').


% ectest/ec_reader_test_examples.e:9004
% 
% human Homer
t(human, homer).


% elephant Jumbo
t(elephant, jumbo).


% horse Silver
t(horse, silver).


% 
% Species(Homer)=HumanSpecies.
species(homer)=humanSpecies.


% 
% ectest/ec_reader_test_examples.e:9010
% Adult(Homer).
adult(homer).


% 
% Species(Jumbo)=ElephantSpecies.
species(jumbo)=elephantSpecies.


% 
% Adult(Jumbo).
adult(jumbo).


% 
% Species(Silver)=HorseSpecies.
species(silver)=horseSpecies.


% 
% Adult(Silver).
adult(silver).


% 
% 
% ectest/ec_reader_test_examples.e:9016
% {position}% 
% !HoldsAt(Pos(Homer,position),0) &
% HoldsAt(Pos(Jumbo,position),0) &
% HoldsAt(Pos(Homer,position),1) &
% !HoldsAt(Pos(Jumbo,position),1).
exists([Position],  (not(holds_at(pos(homer, Position), 0)), holds_at(pos(jumbo, Position), 0), holds_at(pos(homer, Position), 1), not(holds_at(pos(jumbo, Position), 1)))).


% 
% ectest/ec_reader_test_examples.e:9021
% [animal,time] % !Happens(ThrowOff(animal,Homer),time).
not(happens(throwOff(Animal, homer), Time)).


% 
% 
% ectest/ec_reader_test_examples.e:9023
% [human] % HoldsAt(PosDeterminingFluent(human,1),1).
holds_at(posDeterminingFluent(Human, 1), 1).


% 
% ectest/ec_reader_test_examples.e:9024
% [event,animal] % !HoldsAt(DoneBy(event,animal),1).
not(holds_at(doneBy(Event, Animal), 1)).


% 
% 
% ;HoldsAt(Opened(GateAO),0).
% ;HoldsAt(Pos(Homer,3),0).
% ;HoldsAt(Pos(Jumbo,2),0).
% ;HoldsAt(Pos(Silver,7),0).
% ;Happens(Move(Jumbo,4),0).
% ;Happens(Move(Silver,8),0).
% ;Happens(Mount(Homer,Jumbo),0).
% ectest/ec_reader_test_examples.e:9033
% 
% range time 0 1
range(time, 0, 1).


% range position 1 8
range(position, 1, 8).


% range offset 0 0
range(offset, 0, 0).


% 
% ; End of file.
% ectest/ec_reader_test_examples.e:9039
% translate: ending  File: ectest/ec_reader_test_examples.e.pro 
