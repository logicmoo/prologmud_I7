
:-include(library('ec_planner/ec_test_incl')).
% loading('examples/Mueller2006/Exercises/MixingPaints.e')
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
% examples/Mueller2006/Exercises/MixingPaints.e:19
% 
% load foundations/Root.e% load foundations/EC.e% 
% sort palette
 /*
sort(palette).
*/
sort(palette).

% sort color
 /*
sort(color).
*/
sort(color).

% examples/Mueller2006/Exercises/MixingPaints.e:25
% 
% palette Palette1
 /*
t(palette,palette1).
*/
sort(palette).
t(palette,palette1).

% color Red, Yellow, Blue, Green
 /*
t(color,red).
*/
sort(color).
t(color,red).

 /*
t(color,yellow).
*/
sort(color).
t(color,yellow).

 /*
t(color,blue).
*/
sort(color).
t(color,blue).

 /*
t(color,green).
*/
sort(color).
t(color,green).

% 
% event PlaceOnPalette(palette,color)
 /*
event(placeOnPalette(palette,color)).
*/
event(placeOnPalette(palette,color)).

% fluent OnPalette(palette,color)
 /*
fluent(onPalette(palette,color)).
*/
fluent(onPalette(palette,color)).

% examples/Mueller2006/Exercises/MixingPaints.e:31
% 
% examples/Mueller2006/Exercises/MixingPaints.e:32
% [palette,color,time]% 
% !Happens(PlaceOnPalette(palette,Yellow),time) |
% !Happens(PlaceOnPalette(palette,Blue),time) ->
% Initiates(PlaceOnPalette(palette,color),OnPalette(palette,color),time).
 /*
not(happens(placeOnPalette(Palette,yellow),Time)) ; not(happens(placeOnPalette(Palette,blue),Time)) -> initiates(placeOnPalette(Palette,Color),
	  onPalette(Palette,Color),
	  Time).
*/
not(happens(placeOnPalette(Palette,yellow),Time)) ; axiom(initiates(placeOnPalette(Palette,Color),
		onPalette(Palette,Color),
		Time),
      [not(happens(placeOnPalette(Palette,blue),Time))]).

% 
% 
% examples/Mueller2006/Exercises/MixingPaints.e:37
% [palette,color1,color2,time]% 
% Happens(PlaceOnPalette(palette,Yellow),time) &
% color1 = Blue &
% color2 = Green ->
% Initiates(PlaceOnPalette(palette,color1),OnPalette(palette,color2),time).
 /*
happens(placeOnPalette(Palette, yellow), Time), Color1=blue, Color2=green ->
    initiates(placeOnPalette(Palette, Color1),
              onPalette(Palette, Color2),
              Time).
*/
axiom(initiates(placeOnPalette(Palette,Color1),
		onPalette(Palette,Color2),
		Time),
      [ happens(placeOnPalette(Palette,yellow),Time),
	equals(Color1,blue),
	equals(Color2,green)
      ]).

% 
% 
% examples/Mueller2006/Exercises/MixingPaints.e:43
% [palette,color1,color2,time]% 
% !(Happens(PlaceOnPalette(palette,Yellow),time) &
%   Happens(PlaceOnPalette(palette,Blue),time)) &
% HoldsAt(OnPalette(palette,color1),time) &
% color1 != color2 ->
% Terminates(PlaceOnPalette(palette,color2),OnPalette(palette,color1),time).
 /*
not((happens(placeOnPalette(Palette, yellow), Time), happens(placeOnPalette(Palette, blue), Time))), holds_at(onPalette(Palette, Color1), Time), Color1\=Color2 ->
    terminates(placeOnPalette(Palette, Color2),
               onPalette(Palette, Color1),
               Time).
*/
axiom(terminates(placeOnPalette(Palette,Color2),
		 onPalette(Palette,Color1),
		 Time),
      [ neg(happens(placeOnPalette(Palette,yellow),Time) ',' happens(placeOnPalette(Palette,blue),Time)),
	holds_at(onPalette(Palette,Color1),Time),
	diff(Color1,Color2)
      ]).

% 
% examples/Mueller2006/Exercises/MixingPaints.e:49
% 
% examples/Mueller2006/Exercises/MixingPaints.e:50
% [palette,color1,color2,time]% 
% Happens(PlaceOnPalette(palette,Yellow),time) &
% HoldsAt(OnPalette(palette,color2),time) &
% color1 = Blue &
% color2 != Green ->
% Terminates(PlaceOnPalette(palette,color1),OnPalette(palette,color2),time).
 /*
happens(placeOnPalette(Palette, yellow), Time), holds_at(onPalette(Palette, Color2), Time), Color1=blue, Color2\=green ->
    terminates(placeOnPalette(Palette, Color1),
               onPalette(Palette, Color2),
               Time).
*/
axiom(terminates(placeOnPalette(Palette,Color1),
		 onPalette(Palette,Color2),
		 Time),
      [ happens(placeOnPalette(Palette,yellow),Time),
	holds_at(onPalette(Palette,Color2),Time),
	equals(Color1,blue),
	diff(Color2,green)
      ]).

% 
% examples/Mueller2006/Exercises/MixingPaints.e:56
% 
%; state constraint
% 
% examples/Mueller2006/Exercises/MixingPaints.e:59
% [palette,color1,color2,time]% 
% HoldsAt(OnPalette(palette,color1),time) &
% HoldsAt(OnPalette(palette,color2),time) ->
% color1 = color2.
 /*
holds_at(onPalette(Palette, Color1), Time), holds_at(onPalette(Palette, Color2), Time) ->
    Color1=Color2.
*/
axiom(equals(Color1,Color2),
      [ holds_at(onPalette(Palette,Color1),Time),
	holds_at(onPalette(Palette,Color2),Time)
      ]).

% 
% 
%; (1) place green over red
% examples/Mueller2006/Exercises/MixingPaints.e:65
% HoldsAt(OnPalette(Palette1,Red),0).
 /*
holds_at(onPalette(palette1,red),0).
*/
axiom(holds_at(onPalette(palette1,red),0),[]).

% 
% Delta:
 /*
directive(delta).
*/
directive(delta).

 % Happens(PlaceOnPalette(Palette1,Green),0).
 /*
happens(placeOnPalette(palette1,green),0).
*/
axiom(happens(placeOnPalette(palette1,green),0),[]).

% 
% 
%; (2) place yellow+blue over green
% Delta:
 /*
directive(delta).
*/
directive(delta).

 % Happens(PlaceOnPalette(Palette1,Yellow),1).
 /*
happens(placeOnPalette(palette1,yellow),1).
*/
axiom(happens(placeOnPalette(palette1,yellow),1),[]).

% 
% Delta:
 /*
directive(delta).
*/
directive(delta).

 % Happens(PlaceOnPalette(Palette1,Blue),1).
 /*
happens(placeOnPalette(palette1,blue),1).
*/
axiom(happens(placeOnPalette(palette1,blue),1),[]).

% 
% examples/Mueller2006/Exercises/MixingPaints.e:71
% 
%; (3) place yellow
% Delta:
 /*
directive(delta).
*/
directive(delta).

 % Happens(PlaceOnPalette(Palette1,Yellow),2).
 /*
happens(placeOnPalette(palette1,yellow),2).
*/
axiom(happens(placeOnPalette(palette1,yellow),2),[]).

% 
% 
%; (4) place blue
% Delta:
 /*
directive(delta).
*/
directive(delta).

 % Happens(PlaceOnPalette(Palette1,Blue),3).
 /*
happens(placeOnPalette(palette1,blue),3).
*/
axiom(happens(placeOnPalette(palette1,blue),3),[]).

% 
% examples/Mueller2006/Exercises/MixingPaints.e:77
% 
%; (5) place green
% Delta:
 /*
directive(delta).
*/
directive(delta).

 % Happens(PlaceOnPalette(Palette1,Yellow),4).
 /*
happens(placeOnPalette(palette1,yellow),4).
*/
axiom(happens(placeOnPalette(palette1,yellow),4),[]).

% 
% Delta:
 /*
directive(delta).
*/
directive(delta).

 % Happens(PlaceOnPalette(Palette1,Blue),4).
 /*
happens(placeOnPalette(palette1,blue),4).
*/
axiom(happens(placeOnPalette(palette1,blue),4),[]).

% 
% 
% completion Delta Happens
 /*
completion(delta).
*/
completion(delta).

 /*
completion(happens).
*/
completion(happens).

% examples/Mueller2006/Exercises/MixingPaints.e:83
% 
% range time 0 5
 /*
range(time,0,5).
*/
range(time,0,5).

% range offset 1 1
 /*
range(offset,1,1).
*/
range(offset,1,1).

% 
%; End of file.
