/*****************************************************************************/
/*  number_rules(RuleList,FromList,NewRuleList).                             */
/*                                                                           */
/*  Takes the Rulelist and makes it into the format Number,From,Text)        */
/*  and stuffs it in NewRuleList.                                            */
/*****************************************************************************/

number_rules([],_FromList,[]):-!.

number_rules([Rule|Rest],FromList,[NewRule|NewRest]):-
   number_rule(Rule,FromList,NewRule),
   number_rules(Rest,FromList,NewRest).

number_rule(r(N1,N2,Clause),_,r(N1,N2,Clause)).
number_rule(Rule,FromList,r(N,FromList,Rule)):-
   newrulecount(N).

/****************************************************************************/
/*                                                                          */
/* because of the new normal form, modal subsumption takes place if:        */
/*                                                                          */
/* (1) a modal literal is implied by true                                   */
/* (2) the clauses are exactly the same                                     */
/* (3) a literal clause is a unit clause                                    */
/*     and the negated literal is the antecedent of the modal clause        */
/*                                                                          */
/****************************************************************************/

dis_implies(true imp B, _C imp B):-!.
dis_implies(_A imp B, true imp B):-!.
dis_implies(A imp B, A imp B):-!.

/* 
   this only fails: 
   cut introduced to prevent redo 
   there will never be a modal literal in a literal clause 
*/

dis_implies(_A imp _B, _C):-fail,!.

dis_implies(A, NegA imp _C):-
  is_literal(A),!,
  snff_negate(A,NegA).

/* literal subsumption takes place as usual */

dis_implies(A,B):-!,
  list_strip_or(A,A1),
  list_strip_or(B,B1),
  subset(A1,B1).

/***************************************************/
   
set_implies([],_):-!.

set_implies(X,X):-!.

set_implies(_,[[true]]):-!.

set_implies(_,[true]):-!.

set_implies([[false]],_):-!.

set_implies([false],_):-!.

set_implies([H|Tail],[H1]):-
    is_literal(H1),
    is_literal(H),!,
    member(H1,[H|Tail]).

set_implies([H1|Tail1],[H2|Tail2]):-
    is_literal(H1),
    is_literal(H2),!,
    subset([H2|Tail2],[H1|Tail1]).

set_implies([H1],[H|Tail]):-
    is_modal_literal(H1),
    is_modal_literal(H),!,
    modal_member(H1,[H|Tail]).

set_implies([H1|Tail1],[H2|Tail2]):-
    is_modal_literal(H1),
    is_modal_literal(H2),!,
    modal_subset([H1|Tail1],[H2|Tail2]).

set_implies([H1],[H2|Tail2]):-
    is_literal(H1),!,
    is_superset_of_member_of_list([H1],[H2|Tail2]).

set_implies([H1|Tail1],[H2|Tail2]):-
    is_literal(H1),!,
    is_superset_of_member_of_list([H1|Tail1],[H2|Tail2]).

set_implies([H1|Tail1],[H2|Tail2]):-
    is_literal(H2),!,
    set_implies(H1,[H2|Tail2]),
    set_implies(Tail1,[H2|Tail2]).

set_implies([H1|List1],List2):-
    set_implies(H1,List2),
    set_implies(List1,List2).

/*******************************************************************************/
/* strip(Clause, List)                                                         */
/*                                                                             */
/*   This predicate removes all ands or ors from the clause (they should be    */
/*   all ands or all ors) and sticks remaining propositions in a list.         */
/*******************************************************************************/

strip(X or Y, AList):-
   strip(X, XList),
   strip(Y, YList),
   append(XList, YList, AList), !. % Added cuts to stop choosing alternative rules on failure

strip(X and Y, AList):-
   strip(X, XList),
   strip(Y, YList),
   append(XList, YList, AList), !. % Added cuts to stop choosing alternative rules on failure

strip(X, [X]).

/******************************************************************************/

list_strip_or(X or Y,NewXY):-
  list_strip_or(X,NewX),
  list_strip_or(Y,NewY),
  append(NewX,NewY,NewXY),!.

list_strip_or(X,[NewX]):-
  simplify_and(X,NewX).

/*******************************************************************************/
/* strip_or(Clause, List)                                                      */
/*                                                                             */
/*   This predicate removes all ands or ors from Clause which is disjunctive   */
/*   and puts them on a bracketed List eg (a and b) or c would be [[a,b],[c]]  */
/*******************************************************************************/

strip_or(X or Y, NewXY):-
  strip_or(X, NewX),
  strip_or(Y, NewY),
  append(NewX, NewY, NewXY),!.

strip_or(X and Y, [NewXY]):-
   strip(X and Y, NewXY),!.

strip_or(X,[[X]]) :- !.

/*******************************************************************************/
/* strip_and(Clause, List)                                                     */
/*                                                                             */
/*   This predicate removes all ands or ors from Clause which is disjunctive   */
/*   and puts them on a bracketed List eg (a and b) or c would be [[a,b],[c]]  */
/*******************************************************************************/

strip_and(X and Y, NewXY):-
  strip_and(X, NewX),
  strip_and(Y, NewY),
  append(NewX, NewY, NewXY),!.

strip_and(X or Y, [NewXY]):-
   strip(X or Y, NewXY),!.

strip_and(X,[[X]]) :- !.

/**************************************************************************/

member_of_2nd_is_subset_of_1st([],List1,[_|Tail2]):-
    member_of_2nd_is_subset_of_1st(List1,List1,Tail2),!.

member_of_2nd_is_subset_of_1st([H1|_],_,[H2|_]):-
    subset(H2,H1),!.

member_of_2nd_is_subset_of_1st([_|Tail1],List1, [H2|Tail2]):-
    member_of_2nd_is_subset_of_1st(Tail1,List1, [H2|Tail2]),!.

/********************************************************************************/

modal_subset([],_):-!.

modal_subset(X, X):-!.

modal_subset([H |Tail], BList) :-
   modal_member(H, BList),!,
   modal_subset(Tail, BList).

/* modal_member(Item, List) :- Item occurs in List. */
/* Note X must be instantiated for this to work otherwise we will just
   return the head of our clause.
*/

modal_member(X, [X | _]):- !.

modal_member(X, [sometime X|_]):- !.

modal_member(X, [neg always neg X|_]):- !.

modal_member(X, [_ | Tail]) :- modal_member(X, Tail).

/* ******************
   * SIMPLIFICATION *
   ****************** */

/******************************************************************************/
/*   simplify(Rules, NewRules)                                                */
/*                                                                            */
/*  Looks at every rule in Rules and performs standard simplification ie.     */
/*            1. replacing a conjunction with complementary literals by false */
/*            2. replacing a disjunction with complementary literals by true  */
/*            3. removing true from a list of conjuncts                       */
/*            4. removing false from a list of conjuncts                      */
/******************************************************************************/

simplify([],[]).
simplify([Rule | Rest], NewRules) :-
	simplify_rule(Rule, SRule),
	simplify(Rest, NewRest),
	append(SRule, NewRest, NewRules).

/*****************************************************************************/
/* simplify_rule (Rule, NewRules)                                            */
/*                                                                           */ 
/* Performs various simplifications on Rule. These are                       */
/*   - dealing with true and false in conjunctions/disjunctions              */
/*   - removing duplicate literals                                           */
/*   - dealing with complemetary literals                                    */
/*   - getting rid of rules which are always true                            */
/*   - expanding rules of the form slast P imp false                         */
/*****************************************************************************/

simplify_rule(r(N1,N2,X imp Y),[r(N1,N2,NewR)]):-
        is_modal_rule(X imp Y),
        snff_negate(X, NegX),
        simplify_rule(r(N1,N2,NegX or Y),[r(N1,N2,R)]),
        change_form(R,[NewR]).

simplify_rule(r(N1,N2,R), FinalSet) :-
	simplify_true_false(R,U),
	remove_true_rules(r(N1,N2,U),RuleSet),   % Ruleset can either be empty, 
                                                 % or the single rule as a list
	expand_false_rules(RuleSet, FinalSet).   

/*****************************************************************************/
/*  simplify_rule(Rule, NewRule)                                             */ 
/*                                                                           */ 
/*  Simplifies Rule by                                                       */
/*   - dealing with true and false in conjunctions/disjunctions              */
/*   - removing duplicate literals                                           */
/*   - dealing with complemetary literals                                    */
/*****************************************************************************/

simplify_true_false(P imp next F, NewP imp next NewF ) :-
        simplify_and(P, NewishP),
        simplify_and_dup(NewishP, NewP),
	simplify_or(F,NewishF),
        simplify_or_dup(NewishF, NewList),
        disjoin(NewList,NewF).

simplify_true_false(F, NewF) :-
        disjunction_of_modal_literals(F),
	simplify_or(F,NewishF),
        simplify_or_dup(NewishF, NewerF),
        simplify_or_modal(NewerF,NewList),
        disjoin(NewList, NewF).

/***********************************************************************/
/*    simplify_and(Conj,NewConj)                                       */
/*                                                                     */ 
/*   Takes a conjunction Conj and if true is a conjunct it removes it  */
/*   or if false is a conjunct false is returned in NewConj, otherwise */
/*   NewConj returns what is left.                                     */
/***********************************************************************/

simplify_and(A and true, NewA) :- simplify_and(A, NewA),!.

simplify_and(true and A, NewA) :- simplify_and(A, NewA),!.

simplify_and(_ and false, false):-!.

simplify_and(false and _, false):-!.

simplify_and(A and _, false) :- simplify_and(A, false),!.

simplify_and(_ and B, false) :- simplify_and(B, false),!.

simplify_and(A and B, NewB) :- 
	simplify_and(A, true),!,
	simplify_and(B, NewB).

simplify_and(A and B, NewA) :- 
	simplify_and(B, true),!,
	simplify_and(A, NewA).

simplify_and(A and B, NewA and NewB) :- 
	simplify_and(A, NewA),
	simplify_and(B, NewB),!.

simplify_and(neg neg P, NewP):-
        simplify_and(P, NewP).

simplify_and(neg true,false).

simplify_and(neg false,true).

simplify_and(P, P).


/*************************************************************************/
/*  simplify_and_dup(OldConj,NewConj)                                    */
/*                                                                       */ 
/*   Takes a conjunction, removes and duplicate literals or              */
/*   complementary literals and returns the remaining conjunction in     */
/*   NewConj.                                                            */
/*************************************************************************/

simplify_and_dup(A, false):-   % If we have complementary literals in a conjunct return false
   strip(A, AList),
   comp_lits_and(AList),!.

simplify_and_dup(A, NewA):-
   strip(A, AList),
   remove_duplicate(AList, NewList),
   conjoin(NewList, NewA).

/***********************************************************************/
/*  remove_duplicate(List, NewList)                                    */
/*                                                                     */ 
/*  Takes a list of nodes (List) of the form [a,b,c,d] and returns a   */
/*  second list with any duplicates removed.                           */
/***********************************************************************/

remove_duplicate(false,false):- !.

remove_duplicate(List1,List2):-
  list_to_set(List1,List2).


/***********************************************************************/
/*    simplify_or(Disj,NewDisj)                                        */
/*                                                                     */ 
/*   Takes a disjunction Disj and if false is a disjunct it removes it */
/*   or if true is a disjunct true is returned in NewDisj, otherwise   */
/*   NewDisj returns what is left.                                     */
/***********************************************************************/

simplify_or(A or false, NewA) :- simplify_or(A, NewA).

simplify_or(false or A, NewA) :- simplify_or(A, NewA).

simplify_or(_ or true, true).

simplify_or(true or _, true).

simplify_or(A or _, true) :- simplify_or(A, true).

simplify_or(_ or B, true) :- simplify_or(B, true).

simplify_or(A or B, NewB) :- 
	simplify_or(A, false),
	simplify_or(B, NewB).

simplify_or(A or B, NewA) :- 
	simplify_or(B, false),
	simplify_or(A, NewA).

simplify_or(A or B, NewA or NewB) :- 
	simplify_or(A, NewA),
	simplify_or(B, NewB).

simplify_or(neg neg P, NewP):-
        simplify_or(P,NewP).

simplify_or(neg true,false).

simplify_or(neg false,true).

simplify_or(P, P).

/*************************************************************************/
/*  simplify_or_dup(OldDisj,NewDisj)                                     */
/*                                                                       */ 
/*   Takes a disjunction, OldDisj removes and duplicate literals, deals  */
/*   with complementary literals and returns the remaining disjunction   */
/*   in NewDisj.                                                         */
/*************************************************************************/

simplify_or_dup(A, [true]):-        % If we have complementary literals in a disjunct return true.
   strip(A, AList),
   check_comp_lits(AList).

simplify_or_dup(A, [true]):-        % If we have complementary literals in a disjunct return true.
   strip(A, AList),
   check_modal_lits(AList).

simplify_or_dup(A, NewList):-
   strip(A, AList),
   remove_duplicate(AList, NewList).

/****************************************************/

check_comp_lits(AList):-
   comp_lits_and(AList).

/***************************************************/

check_modal_lits(AList):-
   modal_lits_and(AList).

/**************************************************/

simplify_or_modal([],[]):-!.

simplify_or_modal([neg k neg L|Rest],[neg k neg L |Final]):-
   remove_member(L,Rest, NewerRest), !,          % Because p -> ~K~p
   remove_member(k L,NewerRest, NewestRest),     % Because K p -> p
   simplify_or_modal(NewestRest,Final).         

simplify_or_modal([k L|Rest],Final):-
   member(L,Rest), !,                            % Because K p -> p
   simplify_or_modal(Rest,Final).            

simplify_or_modal([k L|Rest],Final):-
   member(neg k neg L,Rest), !,                  % Because K p -> ~K~p
   simplify_or_modal(Rest,Final).            

simplify_or_modal([k L|Rest],[k L|Final]):-
   simplify_or_modal(Rest,Final).            

simplify_or_modal([L|Rest],Final):-
   member(neg k neg L,Rest), !,            % Because p -> ~K~p
   remove_member(k L,Rest, NewerRest),     % Because K p -> p
   simplify_or_modal(NewerRest,Final).            

simplify_or_modal([L|Rest],[L|Final]):-
   remove_member(k L,Rest, NewerRest),     % Because K p -> p
   simplify_or_modal(NewerRest,Final).            

simplify_or_modal([L|Rest],[L|Final]):-
   simplify_or_modal(Rest,Final).

/**********************************************/

comp_lits(X,Y):-
  negate_modal_lit(X,Y).

/***************************************************************/
/*  comp_lits_and(List)                                        */
/*                                                             */ 
/* Returns true if complementary literals are found in List    */
/***************************************************************/

comp_lits_and([H|Tail]):-
    negate_modal_lit(H, NegH),
    member(NegH,Tail).

comp_lits_and([_|Tail]):-
  comp_lits_and(Tail).

/**************************************************************/

modal_lits_and([sometime X|List]):- !,
  negate_modal_lit(X,NegX),
  member(NegX,List).

modal_lits_and([always _|List]):- !,
  modal_lits_and(List).

modal_lits_and([X|List]):- !,
  negate_modal_lit(X,NegX),
  member(sometime NegX,List).

modal_lits_and([_|List]):- !,
   modal_lits_and(List).

/***************************************************************/
/*  comp_lits_or(List)                                         */
/*                                                             */ 
/* Returns true if complementary literals are found in List    */
/***************************************************************/

comp_lits_or([H|Tail]):-
     negate_all(H, NegH),
     node_is_member_of(NegH,Tail).

comp_lits_or([_|Tail]):-
  comp_lits_or(Tail).

/***************************************************************/
/*  negate_all(OldList,NewList)                                */
/*                                                             */ 
/* Negate all takes a list OldList which represents part of a  */
/* disjunct and negates all its members which are returned in  */
/* NewList.                                                    */
/***************************************************************/

negate_all([],[]).

negate_all([H|Tail],[NegH|NewTail]):-
   negate_modal_lit(H, NegH),
   negate_all(Tail,NewTail).

/*****************************************************************/
/* remove_true_rules(Rule,NewRule)                               */
/*                                                               */
/* Takes a rule in the form r(Number,FromList,Text) and if it is */
/* always true ie of the form ...imp next true                   */
/*                       or   ...imp true                        */
/* NewRule is return with the Text set to [], otherwise the rule */
/* is returned as it is.                                         */
/*****************************************************************/

remove_true_rules( r(N1,N2,_ imp true), r(N1,N2,[])).
remove_true_rules( r(N1,N2,_ imp next true), r(N1,N2,[])).
remove_true_rules( r(N1,N2,true),r(N1,N2,[])).
remove_true_rules( R, [R] ).

/**********************************************************************/
/*  expand_false_rules(Rules, NewRules)                               */
/*                                                                    */
/* Takes the ruleset ie rules to be simplified which is currently     */
/* either a rule of the form r(_,_,[]) ie the text bit is empty as    */
/* rule has been removed as it is always true or it is a list with a  */
/* single rule in. If the rule is of the form                         */
/*               P imp next false                                     */
/* then this is rewritten into two new rules                          */
/*               true imp next ~P                                     */
/*               start imp ~P                                         */
/* The FromList of the original rule is just copied into the FromList */
/* of these two new rules to avoid having to save the rule which is   */
/* going to disappear. Otherwise the rule remains as it is.           */
/**********************************************************************/

expand_false_rules(r(_,_,[]),[]):-!.

expand_false_rules([],[]):-!.

expand_false_rules( [r(_,N2, P imp next false)|Rest ], [r(NewR1,N2,NegP)|NewRules ] ) :-
       snff_negate(P, NegP),
       newrulecount(NewR1),
       expand_false_rules(Rest, NewRules).

expand_false_rules( [ Other | Rest ], [ Other | NewRest ] ) :-
	expand_false_rules(Rest, NewRest).

/*********************************************************/
/* disjoin(List, Disjunction)                            */
/*                                                       */ 
/* Takes a list of nodes (List) eg [a,b,c] and returns   */
/* them separated by or eg a or b or c in Disjunction    */
/*********************************************************/

disjoin([], false):-!.
disjoin(List, Form) :- disjoin2(List, Form).

/*********************************************************/

disjoin2([F|[]], F):-!.
disjoin2([F | Rest], F or NewForm) :- disjoin2(Rest, NewForm).

/*********************************************************/
/* conjoin(List, Conjunction)                            */
/*                                                       */ 
/* Takes a list of nodes (List) eg [a,b,c] and returns   */
/* them separated by "and" eg a and b and c in           */
/* Conjunction.                                          */
/*********************************************************/

conjoin([], true):-!.
conjoin(List, Form) :- conjoin2(List, Form).

/********************************************************/

conjoin2([F|[]],F):-!.
conjoin2([F | Rest], F and NewForm) :- conjoin2(Rest, NewForm).

/*****************************************************************************/
/* is_superset_of__member_of_list(Node, List)                                */ 
/*                                                                           */
/* Returns true if one of List is a subset of Node. Usful for detecting      */
/* conjunctions on the rhs of a rule given we are trying to generate a       */
/* node.                                                                     */
/*                                                                           */
/*****************************************************************************/

is_superset_of_member_of_list(Node, [H|_]):-
   subset(H, Node).

is_superset_of_member_of_list(Node, [_|List]):-
   is_superset_of_member_of_list(Node, List).

/*****************************************************************************/
/* node_is_member_of(NewNode, ListofNodes)                                   */ 
/*                                                                           */
/* Similar to node_is_superset_of_list but returns true if the new node and  */
/* one of the nodes from list of nodes are the same (set wise) ie the        */
/* predicate would return true if new node was [a,b] and either [a, b] or    */
/* [b,a] were found in ListOfNodes.                                          */
/*                                                                           */
/*****************************************************************************/

node_is_member_of(NewNode, [Node|_]):-
   same(NewNode, Node), !.

node_is_member_of(NewNode, [_|OldNodes]):-
   node_is_member_of(NewNode, OldNodes).

/*****************************************************************************/
/* same(NodeX,NodeY)                                                         */
/*                                                                           */
/* Takes two nodes and returns true if they are equivalent ie [a] and [a]    */
/* would return true, also [a,b] and [b,a], but [a,b,c] and [b,c] would not. */
/*                                                                           */
/*****************************************************************************/

same(X,X) :- !.

same(X,Y):-
  subset(X,Y),!,
  subset(Y,X).

/*****************************************************************************/
/* remove_duplicate_nodes(NodeList, NewNodes).                               */
/*                                                                           */
/* Takes a list of nodes (NodeList) and removes any duplicate nodes which    */
/* may exist, ie it returns the set of nodes obtained from the list of nodes.*/
/*                                                                           */
/*****************************************************************************/

remove_duplicate_nodes([],[]):-!.

remove_duplicate_nodes([Node|NodeList], NewNodes):-
   node_is_member_of(Node, NodeList), !,
   remove_duplicate_nodes(NodeList, NewNodes).

remove_duplicate_nodes([Node|NodeList], [Node|NewNodes]):-
   remove_duplicate_nodes(NodeList, NewNodes).

/***************************************************************************/

remove_member(X,[X|Rest],NewList):-
   remove_member(X,Rest,NewList).

remove_member(X,[Y|Rest],[Y|NewList]):-
   remove_member(X,Rest,NewList).

remove_member(_,[],[]).

/*********************************************************************************************/
/*  rewrite_and_or(List,Disjunction)                                                         */
/*                                                                                           */
/* Takes a list in the form [[...],[..],[...],[....]] and conjoins all the [..] bits ie      */
/* sticks and between the literals and replaces the , in the original lis by or.             */
/*********************************************************************************************/

rewrite_and_or([H], NewH):-
   conjoin(H,NewH).
 
rewrite_and_or([H|Tail], NewH or NewTail):-
  conjoin(H, NewH),
  rewrite_and_or(Tail, NewTail).

/******************************************************************************************/

negate_modal_lit(neg k neg X, k neg X).

negate_modal_lit(k neg X, neg k neg X).

negate_modal_lit(neg k X, k X).

negate_modal_lit(k X, neg k X).

negate_modal_lit(neg X, X).

negate_modal_lit(X, neg X).












