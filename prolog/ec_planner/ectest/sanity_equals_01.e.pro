% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/ectest/sanity_equals_01.e:0
% translate: begining  File: /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/ectest/sanity_equals_01.e.pro 
:-expects_dialect(ecalc).

% predicate Neighbor(position,position)
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/ectest/sanity_equals_01.e:2
predicate(neighbor(position,position)).


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
% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/ectest/sanity_equals_01.e:30
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


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/ectest/sanity_equals_01.e:33
% [x,y]
 % Neighbor_rev(y,x) <-> Neighbor(x,y).
neighbor_rev(Y, X) <->
    neighbor(X, Y).


% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/ectest/sanity_equals_01.e:35
%; Prolog code starts with ; and %
:- register_tests(
                  [ test_tru(neighbor(1, 2)),
                    test_tru(neighbor(1, 3)),
                    test_tru(neighbor(1, 4)),
                    test_tru(neighbor(2, 3)),
                    test_tru(neighbor(2, 4)),
                    test_tru(neighbor(3, 4)),
                    test_tru(neighbor(5, 6)),
                    test_tru(neighbor(5, 7)),
                    test_tru(neighbor(5, 8)),
                    test_tru(neighbor(6, 7)),
                    test_tru(neighbor(6, 8)),
                    test_tru(neighbor(7, 8)),
                    test_tru(neighbor_rev(1, 2)),
                    test_tru(neighbor_rev(1, 3)),
                    test_tru(neighbor_rev(1, 4)),
                    test_tru(neighbor_rev(2, 3)),
                    test_tru(neighbor_rev(2, 4)),
                    test_tru(neighbor_rev(3, 4)),
                    test_tru(neighbor_rev(5, 6)),
                    test_tru(neighbor_rev(5, 7)),
                    test_tru(neighbor_rev(5, 8)),
                    test_tru(neighbor_rev(6, 7)),
                    test_tru(neighbor_rev(6, 8)),
                    test_tru(neighbor_rev(7, 8)),
                    test_tru(neighbor(4, 7)),
                    test_tru(neighbor_rev(4, 7))
                  ]).



% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/ectest/sanity_equals_01.e:37
:- run_tests.

% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/ectest/sanity_equals_01.e:66
% translate: ending  File: /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/ectest/sanity_equals_01.e.pro 
