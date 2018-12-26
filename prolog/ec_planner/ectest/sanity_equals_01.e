
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


[x,y] Neighbor_rev(y,x) <-> Neighbor(x,y).

; Prolog code starts with ; and %

;:- 
 register_tests([ 
    test_tru(neighbor(1 ,2) ),
    test_tru(neighbor(1 ,3) ),
    test_tru(neighbor(1 ,4) ),
    test_tru(neighbor(2 ,3) ),
    test_tru(neighbor(2 ,4) ),
    test_tru(neighbor(3 ,4) ),
    test_tru(neighbor(5 ,6) ),
    test_tru(neighbor(5 ,7) ),
    test_tru(neighbor(5 ,8) ),
    test_tru(neighbor(6 ,7) ),
    test_tru(neighbor(6 ,8) ),
    test_tru(neighbor(7 ,8) ),
    test_tru(neighbor_rev(1 ,2) ),
    test_tru(neighbor_rev(1 ,3) ),
    test_tru(neighbor_rev(1 ,4) ),
    test_tru(neighbor_rev(2 ,3) ),
    test_tru(neighbor_rev(2 ,4) ),
    test_tru(neighbor_rev(3 ,4) ),
    test_tru(neighbor_rev(5 ,6) ),
    test_tru(neighbor_rev(5 ,7) ),
    test_tru(neighbor_rev(5 ,8) ),
    test_tru(neighbor_rev(6 ,7) ),
    test_tru(neighbor_rev(6 ,8) ),
    test_tru(neighbor_rev(7 ,8) ),
    test_tru(neighbor(4 ,7) ),
    test_tru(neighbor_rev(4 ,7) )]).

;:- run_tests.

