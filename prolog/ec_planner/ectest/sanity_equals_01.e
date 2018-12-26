
predicate Neighbor(position,position)

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



; Prolog code starts with ;:-

;:- 
 maplist(call, 
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

;:- run_tests.

