
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
% load foundations/Root.e
% load foundations/EC.e
% 
% sort palette
 /*
.
*/
.

% sort color
 /*
.
*/
.

% examples/Mueller2006/Exercises/MixingPaints.e:25
% 
% palette Palette1
 /*
.
*/
.
.

% color Red, Yellow, Blue, Green
 /*
.
*/
.
.

 /*
.
*/
.
.

 /*
.
*/
.
.

 /*
.
*/
.
.

% 
% event PlaceOnPalette(palette,color)
 /*
.
*/
.

% fluent OnPalette(palette,color)
 /*
.
*/
.

% examples/Mueller2006/Exercises/MixingPaints.e:31
% 
% examples/Mueller2006/Exercises/MixingPaints.e:32
% [palette,color,time]% 
% !Happens(PlaceOnPalette(palette,Yellow),time) |
% !Happens(PlaceOnPalette(palette,Blue),time) ->
% Initiates(PlaceOnPalette(palette,color),OnPalette(palette,color),time).
 /*
.
*/
.

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
.

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
.

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
.

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
.

% 
% 
%; (1) place green over red
% examples/Mueller2006/Exercises/MixingPaints.e:65
% HoldsAt(OnPalette(Palette1,Red),0).
 /*
.
*/
.

% 
% Delta:
 /*
.
*/
.

 % Happens(PlaceOnPalette(Palette1,Green),0).
 /*
.
*/
.

% 
% 
%; (2) place yellow+blue over green
% Delta:
 /*
.
*/
.

 % Happens(PlaceOnPalette(Palette1,Yellow),1).
 /*
.
*/
.

% 
% Delta:
 /*
.
*/
.

 % Happens(PlaceOnPalette(Palette1,Blue),1).
 /*
.
*/
.

% 
% examples/Mueller2006/Exercises/MixingPaints.e:71
% 
%; (3) place yellow
% Delta:
 /*
.
*/
.

 % Happens(PlaceOnPalette(Palette1,Yellow),2).
 /*
.
*/
.

% 
% 
%; (4) place blue
% Delta:
 /*
.
*/
.

 % Happens(PlaceOnPalette(Palette1,Blue),3).
 /*
.
*/
.

% 
% examples/Mueller2006/Exercises/MixingPaints.e:77
% 
%; (5) place green
% Delta:
 /*
.
*/
.

 % Happens(PlaceOnPalette(Palette1,Yellow),4).
 /*
.
*/
.

% 
% Delta:
 /*
.
*/
.

 % Happens(PlaceOnPalette(Palette1,Blue),4).
 /*
.
*/
.

% 
% 
% completion Delta Happens
 /*
.
*/
.

 /*
.
*/
.

% examples/Mueller2006/Exercises/MixingPaints.e:83
% 
% range time 0 5
 /*
.
*/
.

% range offset 1 1
 /*
.
*/
.

% 
%; End of file.
