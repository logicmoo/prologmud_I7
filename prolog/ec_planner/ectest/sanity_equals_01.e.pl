:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(pfc).
 %  loading(always,'ectest/sanity_equals_01.e').

% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/ectest/sanity_equals_01.e:1
% predicate Neighbor(position,position)
 %  predicate(neighbor(position,position)).
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/ectest/sanity_equals_01.e:2
==> mpred_prop(neighbor(position,position),predicate).
==> meta_argtypes(neighbor(position,position)).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/ectest/sanity_equals_01.e:3
% [position1,position2]
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

 /*  neighbor(Position1, Position2) <->
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
 */

 /*  neighbor(Position1, Position2) ->
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
 */

 /*  not(neighbor(Position1, Position2)) :-
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 3))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 6))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 6))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 6))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 7))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position1, 4))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position2, 4))
       ;   not(equals(Position1, 7))
       ).
 */
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/ectest/sanity_equals_01.e:30
axiom(not(neighbor(Position1, Position2)),
   
    [  (not(equals(Position1, 1));not(equals(Position2, 2))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7)))
    ]).

 /*  equals(Position1, 1) :-
       ( (   not(equals(Position1, 1))
         ;   not(equals(Position2, 3))
         ),
         (   not(equals(Position1, 1))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 2))
         ;   not(equals(Position2, 3))
         ),
         (   not(equals(Position1, 2))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 3))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 6))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 1),
   
    [  (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 2) :-
       ( (   not(equals(Position1, 1))
         ;   not(equals(Position2, 3))
         ),
         (   not(equals(Position1, 1))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 2))
         ;   not(equals(Position2, 3))
         ),
         (   not(equals(Position1, 2))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 3))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 6))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 2),
   
    [  (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 1) :-
       ( (   not(equals(Position1, 1))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 2))
         ;   not(equals(Position2, 3))
         ),
         (   not(equals(Position1, 2))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 3))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 6))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 1),
   
    [  (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 3) :-
       ( (   not(equals(Position1, 1))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 2))
         ;   not(equals(Position2, 3))
         ),
         (   not(equals(Position1, 2))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 3))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 6))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 3),
   
    [  (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 1) :-
       ( (   not(equals(Position1, 2))
         ;   not(equals(Position2, 3))
         ),
         (   not(equals(Position1, 2))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 3))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 6))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 1),
   
    [  (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 4) :-
       ( (   not(equals(Position1, 2))
         ;   not(equals(Position2, 3))
         ),
         (   not(equals(Position1, 2))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 3))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 6))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 4),
   
    [  (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 2) :-
       ( (   not(equals(Position1, 2))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 3))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 6))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 2),
   
    [  (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 3) :-
       ( (   not(equals(Position1, 2))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 3))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 6))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 3),
   
    [  (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 2) :-
       ( (   not(equals(Position1, 3))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 6))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 2),
   
    [  (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 4) :-
       ( (   not(equals(Position1, 3))
         ;   not(equals(Position2, 4))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 6))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 4),
   
    [  (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 3) :-
       ( (   not(equals(Position1, 5))
         ;   not(equals(Position2, 6))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 3),
   
    [  (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 4) :-
       ( (   not(equals(Position1, 5))
         ;   not(equals(Position2, 6))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 4),
   
    [  (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 5) :-
       ( (   not(equals(Position1, 5))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 5),
   
    [  (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 6) :-
       ( (   not(equals(Position1, 5))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 5))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 6),
   
    [  (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 5) :-
       ( (   not(equals(Position1, 5))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 5),
   
    [  (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 7) :-
       ( (   not(equals(Position1, 5))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 7),
   
    [  (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 5) :-
       ( (   not(equals(Position1, 6))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 5),
   
    [  (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 8) :-
       ( (   not(equals(Position1, 6))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position1, 6))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 8),
   
    [  (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 6) :-
       ( (   not(equals(Position1, 6))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 6),
   
    [  (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 7) :-
       ( (   not(equals(Position1, 6))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 7),
   
    [  (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 6) :-
       ( (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 6),
   
    [  (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 8) :-
       ( (   not(equals(Position1, 7))
         ;   not(equals(Position2, 8))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 8),
   
    [  (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 7) :-
       ( (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 7),
   
    [  (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 8) :-
       ( (   not(equals(Position2, 1))
         ;   not(equals(Position1, 2))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 8),
   
    [  (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 1) :-
       ( (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 1),
   
    [  (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 2) :-
       ( (   not(equals(Position2, 1))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 2),
   
    [  (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 1) :-
       ( (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 1),
   
    [  (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 3) :-
       ( (   not(equals(Position2, 1))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 3),
   
    [  (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 1) :-
       ( (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 1),
   
    [  (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 4) :-
       ( (   not(equals(Position2, 2))
         ;   not(equals(Position1, 3))
         ),
         (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 4),
   
    [  (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 2) :-
       ( (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 2),
   
    [  (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 3) :-
       ( (   not(equals(Position2, 2))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 3),
   
    [  (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 2) :-
       ( (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 2),
   
    [  (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 4) :-
       ( (   not(equals(Position2, 3))
         ;   not(equals(Position1, 4))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 4),
   
    [  (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 3) :-
       ( (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 3),
   
    [  (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 4) :-
       ( (   not(equals(Position2, 5))
         ;   not(equals(Position1, 6))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 4),
   
    [  (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 5) :-
       ( (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 3))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 5),
   
    [  (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 6) :-
       ( (   not(equals(Position2, 5))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 3))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 6),
   
    [  (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 5) :-
       ( (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 6))
       ),
       (   not(equals(Position2, 3))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 5),
   
    [  (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 7) :-
       ( (   not(equals(Position2, 5))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 6))
       ),
       (   not(equals(Position2, 3))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 7),
   
    [  (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 5) :-
       ( (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 6))
       ),
       (   not(equals(Position2, 3))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 5),
   
    [  (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 8) :-
       ( (   not(equals(Position2, 6))
         ;   not(equals(Position1, 7))
         ),
         (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 6))
       ),
       (   not(equals(Position2, 3))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 8),
   
    [  (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 6) :-
       ( (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 6))
       ),
       (   not(equals(Position2, 3))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 6),
   
    [  (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 7) :-
       ( (   not(equals(Position2, 6))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 6))
       ),
       (   not(equals(Position2, 3))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 7),
   
    [  (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 6) :-
       ( (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 6))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 6))
       ),
       (   not(equals(Position2, 3))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 6),
   
    [  (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 8) :-
       ( (   not(equals(Position2, 7))
         ;   not(equals(Position1, 8))
         ),
         (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 6))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 6))
       ),
       (   not(equals(Position2, 3))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 8),
   
    [  (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 7) :-
       ( (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 6))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 6))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 6))
       ),
       (   not(equals(Position2, 3))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 7),
   
    [  (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 8) :-
       ( (   not(equals(Position1, 4))
         ;   not(equals(Position2, 7))
         ),
         (   not(equals(Position2, 4))
         ;   not(equals(Position1, 7))
         )
       ),
       (   not(equals(Position2, 6))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 6))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 6))
       ),
       (   not(equals(Position2, 3))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 8),
   
    [  (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 4) :-
       (   not(equals(Position2, 4))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 7))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 6))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 6))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 6))
       ),
       (   not(equals(Position2, 3))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 4),
   
    [  (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 7) :-
       (   not(equals(Position2, 4))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 7))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 6))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 6))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 6))
       ),
       (   not(equals(Position2, 3))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 7),
   
    [  (not(equals(Position2, 4));not(equals(Position1, 7))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position2, 4) :-
       (   not(equals(Position1, 4))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position2, 7))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 6))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 6))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 6))
       ),
       (   not(equals(Position2, 3))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position2, 4),
   
    [  (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

 /*  equals(Position1, 7) :-
       (   not(equals(Position1, 4))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position2, 7))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 6))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 6))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 8))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 7))
       ),
       (   not(equals(Position2, 5))
       ;   not(equals(Position1, 6))
       ),
       (   not(equals(Position2, 3))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 2))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 4))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 3))
       ),
       (   not(equals(Position2, 1))
       ;   not(equals(Position1, 2))
       ),
       (   not(equals(Position1, 7))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 6))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 8))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 7))
       ),
       (   not(equals(Position1, 5))
       ;   not(equals(Position2, 6))
       ),
       (   not(equals(Position1, 3))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 2))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 4))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 3))
       ),
       (   not(equals(Position1, 1))
       ;   not(equals(Position2, 2))
       ),
       neighbor(Position1, Position2).
 */
axiom(equals(Position1, 7),
   
    [  (not(equals(Position1, 4));not(equals(Position2, 7))),
       (not(equals(Position2, 7));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 8))),
       (not(equals(Position2, 6));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 8))),
       (not(equals(Position2, 5));not(equals(Position1, 7))),
       (not(equals(Position2, 5));not(equals(Position1, 6))),
       (not(equals(Position2, 3));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 4))),
       (not(equals(Position2, 2));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 4))),
       (not(equals(Position2, 1));not(equals(Position1, 3))),
       (not(equals(Position2, 1));not(equals(Position1, 2))),
       (not(equals(Position1, 7));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 8))),
       (not(equals(Position1, 6));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 8))),
       (not(equals(Position1, 5));not(equals(Position2, 7))),
       (not(equals(Position1, 5));not(equals(Position2, 6))),
       (not(equals(Position1, 3));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 4))),
       (not(equals(Position1, 2));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 4))),
       (not(equals(Position1, 1));not(equals(Position2, 3))),
       (not(equals(Position1, 1));not(equals(Position2, 2))),
      neighbor(Position1, Position2)
    ]).

axiom(neighbor(Position1, Position2),
   
    [  (equals(Position1, 1), equals(Position2, 2);
        equals(Position1, 1), equals(Position2, 3);
        equals(Position1, 1), equals(Position2, 4);
        equals(Position1, 2), equals(Position2, 3);
        equals(Position1, 2), equals(Position2, 4);
        equals(Position1, 3), equals(Position2, 4);
        equals(Position1, 5), equals(Position2, 6);
        equals(Position1, 5), equals(Position2, 7);
        equals(Position1, 5), equals(Position2, 8);
        equals(Position1, 6), equals(Position2, 7);
        equals(Position1, 6), equals(Position2, 8);
        equals(Position1, 7), equals(Position2, 8);
        equals(Position2, 1), equals(Position1, 2);
        equals(Position2, 1), equals(Position1, 3);
        equals(Position2, 1), equals(Position1, 4);
        equals(Position2, 2), equals(Position1, 3);
        equals(Position2, 2), equals(Position1, 4);
        equals(Position2, 3), equals(Position1, 4);
        equals(Position2, 5), equals(Position1, 6);
        equals(Position2, 5), equals(Position1, 7);
        equals(Position2, 5), equals(Position1, 8);
        equals(Position2, 6), equals(Position1, 7);
        equals(Position2, 6), equals(Position1, 8);
        equals(Position2, 7), equals(Position1, 8);
        equals(Position1, 4), equals(Position2, 7);
        equals(Position2, 4), equals(Position1, 7))
    ]).

axiom(equals(X, Y), [call(X = Y)]).
axiom(not(equals(X, Y)), [call(X \= Y)]).
%axiom((G ; _), [G]).
%axiom((_ ; G), [G]).
axiom(neighbor(1, 2), []).
axiom(neighbor(X, Y), [call(X>Y), call(trace), neighbor(Y, X)]).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/ectest/sanity_equals_01.e:34
%; Prolog code starts with ;:-
:-  maplist(call, 
 [ 
    assert(( test_neighbor(X, Y) :- must(ec_prove(neighbor(X, Y))), must(ec_prove(neighbor(Y, X))) )),

    assert(( test_not_neighbor(X, Y) :- must(ec_prove(not(neighbor(X, Y)))), must(ec_prove(not(neighbor(Y, X)))) )),

    test_neighbor(1, 2),
    test_neighbor(1, 3),
    test_neighbor(1, 4),
    test_neighbor(2, 3),
    test_neighbor(2, 4),
    test_neighbor(3, 4),
    test_neighbor(4, 7),
    test_not_neighbor(4, 8),
    test_neighbor(5, 6),
    test_neighbor(5, 7),
    test_neighbor(5, 8),
    test_neighbor(6, 7),
    test_neighbor(6, 8),
    test_neighbor(7, 8) ]).


