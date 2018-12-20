/* ************************************************************** */
/* Operators                                                      */
/* ************************************************************** */

/*  propositional operators are: */

?-op(140, fy, neg).
?-op(160, xfy, and).
?-op(160, xfy, or).
?-op(180, xfy, imp).

/*  unary temporal operators are: */

?-op(140, fy, sometime).
?-op(140, fy, always).
?-op(140, fy, next).

/*  binary temporal operators are: */

?-op(160, xfy, until).
?-op(160, xfy, unless).

/*  knowledge operator: */

?-op(140, fy, k).

/**********************************************/
/*                                            */
/*  Modal Literals                            */
/*                                            */
/**********************************************/

is_new(new(_)).

is_snl_literal(nkn(_)).

is_snl_literal(neg nkn(_)).

is_modal(k L):-
  is_literal(L).

is_modal(neg k L):-
  is_literal(L).

is_modal_literal(L):-
  is_modal(L).

is_modal_literal(L):-
  is_literal(L).

/*****************************************************************************/
/* is_literal(X).                                                            */
/*                                                                           */
/* Returns true if X is a proposition or negation of a proposition.          */
/*****************************************************************************/

is_literal([_]):- !,fail.       % The exclusion of lists is needed for some predicates 
                                % in ntempres.pl (eg includes_or).
is_literal([_|_]):-!,fail.

is_literal(neg X):-
  is_literal(X).

is_literal(X):-
  is_proposition(X).

is_literal(X):-
  is_constant(X).

is_not_literal(neg X):- !,
   is_not_proposition(X).

is_not_literal(X):-
   is_not_proposition(X).


/**********************************************/
/*                                            */
/* Constants                                  */
/*                                            */
/**********************************************/

is_constant(true).
is_constant(false).
is_constant(start).

/*****************************************************************************/
/* is_proposition(X).                                                        */
/*                                                                           */
/* Returns true if X is a proposition.                                       */
/*****************************************************************************/

is_proposition(Formula) :- 
   not(is_not_proposition(Formula)).

/*****************************************************************************/
/* is_not_proposition(X).                                                    */
/*                                                                           */
/* Returns true if X is not a proposition ie. X is a formula which includes  */
/* temporal operators, boolean operators, or are true or false.              */
/*****************************************************************************/

is_not_proposition([]).
is_not_proposition(X) :- is_constant(X).
is_not_proposition(_ and _).
is_not_proposition(_ or _).
is_not_proposition(neg _).
is_not_proposition(sometime _).
is_not_proposition(always _).
is_not_proposition(_ imp _).
is_not_proposition(next _).
is_not_proposition(k _).
is_not_proposition(_ until _).
is_not_proposition(_ unless _).

/*****************************************************************************/
/*  conjunctive(X) :- X is an alpha formula                                  */
/*                                                                           */
/*****************************************************************************/

conjunctive(_ and _).
conjunctive(neg(_ or _)).
conjunctive(neg(_ imp _)).

/*****************************************************************************/
/* disjunctive(X) :- X is a beta formula                                     */
/*                                                                           */
/*****************************************************************************/

disjunctive(neg(_ and _)).
disjunctive(_ or _).
disjunctive(_ imp _).

/**********************************************/
/*                                            */
/*  Counters: definitions, functions          */
/*                                            */
/**********************************************/

/* for renaming predicates */

?-dynamic predcount/1. /* for sicstus only */

/* for skolem functions/constants */

?-dynamic rulecount/1. /* for sicstus only */

?-dynamic clear/0.     /* for sicstus only */

/* predcount(N) :- N is the current Skolem function index */

predcount(1).
rulecount(1).

clear :-retract(predcount(_)), 
        assert(predcount(1)).


/*****************************************************************************/
/*        newpredcount(N)                                                    */
/*                                                                           */
/*        N is the current new predicate index, and as a                     */
/*        side effect, the remembered value is incremented.                  */
/*                                                                           */
/*****************************************************************************/

newpredcount(N) :- predcount(N),
	           retract(predcount(N)),
		   M is N+1,
		   assert(predcount(M)).

startrulecount(N) :- rulecount(N),
                     retract(rulecount(N)),
                     assert(rulecount(1)).
startrulecount(1).

newrulecount(N) :- rulecount(N),
	          retract(rulecount(N)),
		  M is N+1,
		  assert(rulecount(M)).

/********************************************************/
/* New propositions                                     */
/********************************************************/

new_temp_prop(V):- newpredcount(N),
                   term_to_atom(N,NN),
                   string_concat('tmpp',NN,NV),
                   atom_to_term(NV,V,_).

new_dontknow_prop(X,X):- is_snl_literal(X).
new_dontknow_prop(V,X):- V=.. [nkn|[X]].

new_conj_prop(V,true):-V=[true].
new_conj_prop(V,X):- V=.. [new|[X]].

/* ************************************************************** */
/* OUTPUT UTILITIES FOR CLAUSES                                   */
/* ************************************************************** */

/* Remove comments if you do not wish a very verbose output */

% my_writef(_X,_Y):-true,!.
% my_write(_X):-true,!.
% write_ruleset(_X):-true,!.

/* For very verbose output */

 my_writef(X,Y):-writef(X,Y).
 my_write(X):-write(X).

/* ************************************************************** */
/* Formatted output of the prover: lists of clauses               */
/* ************************************************************** */

write_ruleset((Initial,Literal,Modal,Temporal)) :-
        nl,write('Initial Clauses'),nl, 
	write('['), nl, write_ruleset1(Initial),!, write(']'), nl,
        nl,write('Literal Clauses'),nl, 
	write('['), nl, write_ruleset1(Literal),!, write(']'), nl,
        nl,write('Modal Clauses'),nl, 
	write('['), nl, write_ruleset1(Modal),!, write(']'), nl,
        nl,write('Temporal Clauses'),nl, 
	write('['), nl, write_ruleset1(Temporal),!, write(']'), nl.

write_ruleset(Ruleset) :- 
	write('['), nl, write_ruleset1(Ruleset),!, write(']'), nl.

write_ruleset1([Rule | Rest]) :- 
	write('  '), write_form(Rule), nl, write_ruleset1(Rest).

write_ruleset1([]).

/* ************************************************************** */
/* Formatted output of the prover: clauses                        */
/* ************************************************************** */

write_form(r(X,Y,false)) :- 
        write(X), 
        write(' '), 
        write(Y), 
        write(' '), 
        write(' false ').

write_form(r(X,Y,P)) :-
        write(X), 
        write(' '), 
        write(Y), 
        write(' '), 
        write_form(P).

write_form(P imp F) :- 
        write_form(P), 
        write(' => '), 
        write_form(F).

write_form(neg X) :- 
        write('~'), !, 
        write_form(X).

write_form(X and Y) :-
	write('('),
	!,
	write_form(X), 
	write(' & '),
	write_form(Y),
	write(')').

write_form(X or Y) :- 
	write('('),
	!,
	write_form(X),
	write(' | '),
	write_form(Y),
	write(')').

write_form(X until Y) :- 
	write('('),
	!,
	write_form(X),
	write(' U '),
	write_form(Y),
	write(')').

write_form(X unless Y) :- 
	write('('),
	!,
	write_form(X),
	write(' W '),
	write_form(Y),
	write(')').

write_form(next X) :- write('O'), !, write_form(X).
write_form(sometime X) :- write('<>'), !, write_form(X).
write_form(always X) :- write('[]'), !, write_form(X).
write_form(k X) :- write('k'), !, write_form(X).
write_form(new(X)) :- write('new('),!,write_form(X),write(')').
write_form(nkn(X)) :- write('nkn('),!,write_form(X),write(')').
write_form(X) :- write(X).


/**********************************************************************/
/* write_otter_rules(Rules)                                           */
/*                                                                    */
/* Takes a list of clauses (Rules) of the Prolog form r(No,List,Rule) */
/* where Rule is true imp F or P imp next F and rewrites              */
/* them in an Otter format i.e. if F is a or b in the above two rules */
/* the first rules would be output as s_a | s_b. and the second as    */
/* -slast_P | s_a | s_b.                                              */
/*                                                                    */
/**********************************************************************/

write_otter_rules(X,Y,Z):-
  write_otter_rules(X),
  write_otter_rules(Y),
  write_otter_rules(Z).

write_otter_rules(X,Y):-
  write_otter_rules(X),
  write_otter_rules(Y).

write_otter_rules([]).

write_otter_rules([r(_,_,Rule)|RRest]):-
  write_otter_rule(Rule),
  write_otter_rules(RRest).

write_otter_rules([Rule|RRest]):-
  write_otter_rule(Rule),
  write_otter_rules(RRest).

/**********************************************************************/
/* write_otter_rule(Rule)                                             */
/*                                                                    */
/* Takes a claue (Rule) of the Prolog form r(No,List,Rule) where Rule */
/* is true imp F or P imp next F and rewrites them in an              */
/* Otter format i.e. if F is a or b in the above two rules the first  */
/* rules would be output as s_a | s_b.                                */
/* and the second as -slast_P | s_a | s_b.                            */
/*                                                                    */
/**********************************************************************/

write_otter_rule(B):-
  disjunction_of_literals(B),!,
  write_otter_disjunct(B),
  write('.'),nl.

write_otter_rule(A imp next B):-
  write_otter_conjunct(A),
  write(' | '),
  write_otter_disjunct(B),
  write('.'),nl.

/**********************************************************************/
/* write_otter_conjunct(Conj)                                         */
/*                                                                    */
/* Takes a literal or conjunction of literals (Conj) of the Prolog    */
/* form a and b and c from the lhs of global rules and rewrites them  */
/* as -slast_a | -slast_b | -slast_c.                                 */
/*                                                                    */
/**********************************************************************/

write_otter_conjunct(false):-
  write('$F').

write_otter_conjunct(tmp_p(X)):-
  write('-slast_tmp_p_'),
  write(X).

write_otter_conjunct(neg A):-
  write('-slast_neg_'),
  write(A).

write_otter_conjunct(new(X)):-
  trace,
  write('-slast_new_'),
  write_otter_and_proposition(X).

write_otter_conjunct(nkn(X)):-
  write('-slast_nkn_'),
  write_otter_proposition(X).

write_otter_conjunct(A):-
  is_literal(A),
  write('-slast_'),
  write(A).

write_otter_conjunct(A and B):-
  write_otter_conjunct(A),
  write(' | '),
  write_otter_conjunct(B).


/**********************************************************************/
/* write_otter_disjunct(Disj)                                         */
/*                                                                    */
/* Takes a literal or disjunction of literals (Disj) of the Prolog    */
/* form a or b or neg c from the rhs of step clauses and rewrites them*/
/* as s_a | s_b | -s_c. We are assuming the disjunct is from the rhs  */
/* of a step clause or a literal clause.                              */
/*                                                                    */
/**********************************************************************/

write_otter_disjunct(false):-
  write('$F').

write_otter_disjunct(neg A):-
  write('-'),
  write_otter_disjunct(A).

write_otter_disjunct(tmp_p(X)):-
  write('s_tmp_p_'),
  write(X).

write_otter_disjunct(new(X)):-
  write('s_new_'),
  write_otter_and_proposition(X).

write_otter_disjunct(nkn(X)):-
  write('s_nkn_'),
  write_otter_proposition(X).

write_otter_disjunct(A):-
  is_literal(A),
  write('s_'),
  write(A).

write_otter_disjunct(A or B):-
  write_otter_disjunct(A),
  write(' | '),
  write_otter_disjunct(B).

/*********************************************/
/* Write the different kind of propositions  */
/* in the Otter format                       */
/*********************************************/

write_otter_proposition(neg X):-
  write('neg_'),
  write_otter_proposition(X).

write_otter_proposition(new(X)):-
  write('new_'),
  write_otter_and_proposition(X).

write_otter_proposition(nkn(X)):-
  write('nkn_'),
  write_otter_proposition(X).

write_otter_proposition(X):-
  is_literal(X),
  write(X).

/************************************************/
/* The following is needed in order to write    */
/* the conjunctions that label the propositions */
/* that rename conjunctions                     */
/************************************************/

write_otter_and_proposition(tmp_p(X) and B):-
  write_otter_and_proposition(B and tmp_p(X)).

write_otter_and_proposition(A and B):-
  write_otter_and_proposition(A),
  write('_'),
  write_otter_and_proposition(B).

write_otter_and_proposition(A):-
  write_otter_proposition(A).

/*****************************************************************************/
/* write_list(ERulePaths)                                                    */
/*                                                                           */
/* Takes a list comprising of eventualites paired with a list of loop paths  */
/* already found (ERulePaths). It writes out the eventuality and the paths   */
/* already found.                                                            */
/*****************************************************************************/

write_list([]).

write_list([[Rule,Path]|Rest]):-
   write('     ['),
   write_form(Rule),
   write(', '),
   write(Path),
   write('     ]\n'),
   write_list(Rest).


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


/* snff(Initial, Final) :- The Initial set of rules, is transformed to the
                           Final set, which are in DSNF_K form. */

/********************************************/
/* The Final set of rules is in DSNF_K form */
/********************************************/

snff([],[]).

snff(Final, Final) :-
        snff_in_form_list(Final).    

snff(Initial, Final) :-
        snff_anchor_to_start(Initial,Anchored),
        snff_rewrite_list(Anchored,Final).

/****************************************************************/
/* Anchor formulae to start if it is not already in DSNF_K form */
/****************************************************************/

snff_anchor_to_start([],[]):-!.

snff_anchor_to_start([H|Tail], [H|NewRest]):-
        in_form(H),
        snff_anchor_to_start(Tail,NewRest).

%% probably easier in the separation of clauses).

snff_anchor_to_start([H|Tail], [start imp V, V imp (H)|NewRest]):-
        new_temp_prop(V),
        snff_anchor_to_start(Tail, NewRest).

/*************************************************/
/* Rewrite rules, so they are in the DSNF_K form */
/*************************************************/

snff_rewrite_list([],[]):-!.
snff_rewrite_list([Rule|Rest],[Rule|NewRest]):-
         in_form(Rule),
         snff_rewrite_list(Rest,NewRest).

snff_rewrite_list([Rule|Rest],Other):-
         snff_rewrites(Rule,NewRule, NewInform),
         snff_rewrite_list(NewRule,NewerRule),
         append(NewInform,NewerRule,FinalRule),
         snff_rewrite_list(Rest,NewRest),
         append(FinalRule,NewRest,Other).

/*************************************************/
/* Rewrite a given clause                        */
/*************************************************/

snff_rewrites(X imp (A and B),[X imp A, X imp B],[]).

snff_rewrites(X imp neg(A and B),[X imp neg A or neg B],[]).

snff_rewrites(X imp (A imp B),[X imp neg A or B],[]).

snff_rewrites(X imp neg (A imp B),[X imp A, X imp neg B],[]).

snff_rewrites(X imp neg neg A ,[X imp A],[]).

snff_rewrites(X imp k A,[X imp neg V],[V imp k A, neg V imp k A]):-
        is_literal(A),
        simplify_and(neg A,NewA),
        new_dontknow_prop(V,NewA).

snff_rewrites(X imp k A ,[V imp A],[X imp k V]):-
        is_not_literal(A),
        new_temp_prop(V).

snff_rewrites(X imp neg k A ,[X imp V],[V imp neg k A, neg V imp k A]):-
        is_literal(A),
        simplify_and(neg A, NewA),
        new_dontknow_prop(V,NewA).

snff_rewrites(X imp neg k A ,[X imp neg k neg V, V imp neg A],[]):-
        is_not_literal(A),
        new_temp_prop(V).

snff_rewrites(X imp A or B, Rest,[NegX or NewA or NewB]):-
        snff_negate(X,NegX),
        check_dnf(A, NewA, ARest),
        check_dnf(B, NewB, BRest),
        append(ARest,BRest,Rest).

snff_rewrites(X imp next A ,[V imp A],[X imp next V]):-
%        is_not_literal(A),
        new_temp_prop(V).

snff_rewrites(X imp neg next A,[X imp next neg A],[]).

snff_rewrites(X imp (A until B),[X imp V until B, V imp A],[]):-
        is_not_literal(A),
        new_temp_prop(V).

snff_rewrites(X imp (A until B),[X imp A until V, V imp B],[]):-
        is_not_literal(B),
        new_temp_prop(V).

snff_rewrites(X imp (A until B),[],[neg X or B or A, neg X or B or V, V imp next (B or A), V imp next (B or V), X imp sometime B]):-
        is_literal(A),
        is_literal(B),
        new_temp_prop(V).

snff_rewrites(X imp neg (A until B),[X imp ((neg B) unless (neg A and neg B))],[]).

snff_rewrites(X imp (A unless B),[X imp V unless B, V imp A],[]):-
        is_not_literal(A),
        new_temp_prop(V).

snff_rewrites(X imp (A unless B) ,[X imp A unless V, V imp B],[]):-
        is_not_literal(B),
        new_temp_prop(V).

snff_rewrites(X imp (A unless B) ,[],[neg X or B or A, neg X or B or V, V imp next (B or A), V imp next (B or V)]):-
        is_literal(A),
        is_literal(B),
        new_temp_prop(V).

snff_rewrites(X imp neg (A unless B) ,[X imp ((neg B) until (neg A and neg B))],[]).

snff_rewrites(X imp (sometime A) ,[V imp A],[X imp sometime V]):-
        is_not_literal(A),
        new_temp_prop(V).

snff_rewrites(X imp (neg sometime A) ,[X imp always neg A],[]).

snff_rewrites(X imp (always A) ,[X imp always V , V imp A],[]):-
        is_not_literal(A),
        new_temp_prop(V).

snff_rewrites(X imp (always A) ,[],[neg X or A, V imp next A, neg X or V, V imp next V]):-
        is_literal(A),
        new_temp_prop(V).

snff_rewrites(X imp (neg always A) ,[X imp sometime neg A ],[]).

snff_rewrites(X imp (Y),[],[neg X or Y]):-
      conjunction_of_literals(X),
      disjunction_of_modal_literals(Y).

/***********************************/
/* check if rule is in DSNF_K form */
/***********************************/

snff_in_form_list([]):-!.

snff_in_form_list([H|Tail]):-
      in_form(H),
      snff_in_form_list(Tail).        

/**********************************/
/* Normal Form                    */
/**********************************/

in_form(start imp X):-
      disjunction_of_literals(X).

in_form(X):-
      disjunction_of_literals(X).

in_form(X imp k A):-
      is_literal(X),
      is_literal(A).

in_form(X imp neg k A):-
      is_literal(X),
      is_literal(A).

in_form(X imp sometime Y):-
       conjunction_of_literals(X),
       is_literal(Y).

in_form(X imp next(Y)):-
      conjunction_of_literals(X),
      disjunction_of_literals(Y).

/************************************/
/* Disjunction of literals          */
/************************************/

disjunction_of_literals(X):-
       strip_or(X,StrippedX),
       is_literal_list(StrippedX).

/**********************************/
/* Disjucntion of modal literals  */
/**********************************/

disjunction_of_modal_literals(X):-
       strip_or(X,StrippedX),
       is_modal_literal_list(StrippedX).

/***********************************/
/* Conjunction of literals         */
/***********************************/

conjunction_of_literals(X):-
       strip_and(X,StrippedX),
       is_literal_list(StrippedX).

/**************************************************/
/* Verifies if a given list is a list of literals */
/**************************************************/

is_literal_list([[H]|Tail]):-
      is_literal(H),
      is_literal_list(Tail).
      
is_literal_list([]).       

/********************************************************/
/* Verifies if a given list is a list of modal literals */
/********************************************************/

is_modal_literal_list([]).

is_modal_literal_list([[k H]|Tail]):-
      is_literal(H),
      is_modal_literal_list(Tail).

is_modal_literal_list([[neg k H]|Tail]):-
      is_literal(H),
      is_modal_literal_list(Tail).

is_modal_literal_list([[H]|Tail]):-
      is_literal(H),
      is_modal_literal_list(Tail).

/******************** **************/
/* Check if subformulae are in DNF */
/***********************************/           

check_dnf(A or B, NewA or NewB, Rest):-
        check_dnf(A, NewA, ARest),
        check_dnf(B, NewB, BRest),
        append(ARest,BRest,Rest).

check_dnf(A imp B, NewAll, Rest):-
        check_dnf(neg A or B, NewAll, Rest).

check_dnf(A, A, []):-
       is_literal(A).

check_dnf(k A, neg V, [V imp neg k A, neg V imp k A]):-
        is_literal(A),
        snff_negate(A,NegA),
        new_dontknow_prop(V,NegA).

check_dnf(neg k A, V, [V imp neg k A, neg V imp k A]):-
        is_literal(A),
        snff_negate(A,NegA),
        new_dontknow_prop(V,NegA).

check_dnf(A, V, [V imp A]):-
        new_temp_prop(V). %% solves the case where A is nnot literal in KA, negKA

/*******************************/
/* Form to NNF_Form            */
/*******************************/

snff_negate(neg X, X):- !.

snff_negate(X and Y, NotX or NotY):- 
        !, 
        snff_negate(X, NotX),
        snff_negate(Y, NotY).

snff_negate(X or Y, NotX and NotY):- 
        !, 
        snff_negate(X, NotX), 
	snff_negate(Y, NotY).

snff_negate(true, false).

snff_negate(false, true).

snff_negate(X, neg X):- 
       is_proposition(X).


/**************************************************************************/
/*
   This program is an implementation of nontemporal resolution. 
The predicates related to substitution have been written by Michael, 
non temporal resolution predicates are based on those written by Michael, 
and subsumtion predicates are written by Clare.

The following procedures are necessary :-

        1. Check for false
        2. Simplification ie x => next false converted to
                             true => ~x  
        3. Subsumption
        4. Non temporal resolution
        5. Provides new real resolvents
        6. Repeat 1 to 5 until false found or no new resolvents

Note :-
======
The predicate
                fullmres(OldRules,NewRules)

sets the non temporal resolution in progress and expects a set of clauses derived 
from translation to SNF (OldRules),which have been numbered and rewritten to the form

                r(Number,FromList,Text)

where Number is the number of the clause, FromList, will originally be empty, and Text 
is the actual clause itself. It returns NewRules ie what has been derived from non 
temporal resolution. This predicate is called from fullres.pl as part of the main cycle.

     A predicate cyclemres is called which recurs on itself until no new resolvents have 
been derived or false has been derived. This predicate and the predicates it calls maintains 
a distinction round each cycle of the Old ruleset and those newly derived from resolution, 
so simpilfication may just be done on the new ruleset, subsumption may be done between the 
new ruleset and the old ruleset, and then non temporal resolution may be done between the 
new ruleset and the old ruleset,before combining the two and starting again around the cycle.

********************************************************************************/

/*******************************************************************************/
/*  mres(LiteralRules,
         ModalRules,
         InitialRules,
         NewLRules,
         NewMRules)                                                            */


/* This sets the nontemporal resolution off, the first three arguments,        */
/* containing literal, modal and initial clauses,                              */
/* derived from SNF which have been rewriten into the form                     */
/* r(Number,FromList,Text), where currently FromList will be empty.            */
/* NewLRules and NewMRules will contain the new ruleset after no more          */
/* non temporal resolution can take place, or false if false has been derived. */
/*******************************************************************************/

mres(LiteralRules,ModalRules,Initial,NewLRules,NewMRules):-
     cyclemres(1,[],[],LiteralRules,ModalRules,Initial,NewLRules,NewMRules).    

% Leave 2nd and 3rd arguments empty so as to treat the original ruleset as "new" resolvents.

/*****************************************************************************/
/*                                                                           */
/* This predicate is called from fullres.pl during the full resolution       */
/* process. OldRules are in the form r(Number,FromList,Text), where          */
/* Number is the ruless number, FromList contains the numbers of the rules   */
/* which were the parents of this resolvent, and Text is the rule itself     */
/* (in SNF). NewishRules contain the new resolvents obtained from the        */
/* last temporal resolution step. SRules contains a rules which have been    */
/* subsumed on previous cycles round the non-temporal resolution process.    */
/* NewRules will contain the new ruleset                                     */
/* after no more non temporal resolution can take place, or false if a       */
/* contradiction has been derived. NewSRules contains SRules plus any new    */
/* rules which ahve been subsumed.                                           */
/*****************************************************************************/

mres(OldLiteral,OldModal,NewishLiteral,NewishModal,Initial,NewLiteral,NewModal):-
   cyclemres(1,OldLiteral,OldModal,NewishLiteral,NewishModal,Initial,NewLiteral,NewModal).

/*****************************************************************************/
/*  cyclemres(Count,
              OldLiteral,
              OldModal,
              NewishLiteral,
              NewishModal,
              Initial,
              NewLiteral,
              NewModal
              )                                                              */
/*                                                                           */
/*  Cycles around the non temporal resolution cycle, checking for false and  */
/*  if found returning false, otherwise performing simplification,           */
/*  subsumption, nontemporal resolution and then recurring on itself until   */
/*  false or an empty set of resolvents is detected. Count is the current    */
/*  cycle we are on, OldRules are rules from previous cyles rounds the       */
/*  non-temporal resolution processes. NewishResolvents are the new          */
/*  resolvents generated during the last cycle of non-temporal resolution    */
/*  and SRules are the store of rules which have been subsumed which may need*/
/*  to be kept to generate a proof if false is derived. NewRules is the      */
/*  final ruleset after no more non-temporal resolution can be done and      */
/*  NewSRules is the new set of subsumed rules.                              */
/*****************************************************************************/

cyclemres(_,[],[],[],[],_,[],[]).
%:-my_write('\ncyclemres ==> (1)\n').                     % No new resolvents.

cyclemres(_,LRules,MRules,[],[],_,LRules,MRules):-
%    my_write('\ncyclemres ==> (2)\n'),
    my_write('Have generated no new resolvents,current literal/modal clauses are :-\n'),
    write_ruleset(LRules),                   % Terminate non temporal resolution
    write_ruleset(MRules).                   % as no new Rules have been derived.

cyclemres(_,_,_,NewLRules,_,[r(_,_,X)],[r(0,[],false)],_):-
%    my_write('\ncyclemres ==> (2.5)\n'),
    snff_negate(X,NegX),
    test_member(NegX,NewLRules),
    simplify(NewLRules,SNewLRules),
    self_subsumption(SNewLRules,SubNewLRules),
    flatten(SubNewLRules,FSubNewLRules),
    my_write('\nNew literal clauses include (neg start).\n'),
    write_ruleset(FSubNewLRules),      % Terminate non temporal resolution as false
    write('\nHave generated false in initial clauses.\n').

cyclemres(_,_OldLRules,OldMRules,NewLRules,_,_,[r(0,[],false)],OldMRules):-
%    my_write('\ncyclemres ==> (3)\n'),
    test_member(false,NewLRules),
    my_write('\nNew literal clauses include false.\n'),
    write_ruleset(NewLRules),      % Terminate non temporal resolution as false
    write('\nHave generated false in literal clauses.\n').

cyclemres(_,_OldLRules,_OldMRules,_,NewMRules,_,[r(0,[],false)],[r(0,[],false)]):-
%    my_write('\ncyclemres ==> (4)\n'),
    test_member(false,NewMRules),
    my_write('\nNewModal clauses include false.\n'),
    write_ruleset(NewMRules),   % Terminate non temporal resolution as false
    write('\nHave generated false in modal clauses.\n').

% Occasionally ran out of space during non-temporal resolution so have hadded a specific garbage collect.

cyclemres(Count,OldLRules,OldMRules,NewLRules,NewMRules,Initial,NewerLRules,NewerMRules):-
%   my_write('\ncyclemres ==> (5)\n'),
%   garbage_collect,
   writef('\nCycle Number%t.',[Count]),
   my_k_to_literal(NewMRules,KRules),
   append(KRules,NewLRules,AddedNewLRules),
   simplify(AddedNewLRules,SimplifiedLRules),
   simplify(NewMRules,SimplifiedMRules),
   garbage_collect,
   subsumption(OldLRules,SimplifiedLRules,SubOldLRules,SubNewLRules1),
   trim_stacks,
   subsumption(OldMRules,SubNewLRules1,SubOldMRules1,SubNewLRules2),
   trim_stacks,
   subsumption(SimplifiedMRules,SubNewLRules2,SubNewMRules1,SubNewLRules),
   trim_stacks,
   subsumption(SubOldMRules1,SubNewMRules1,SubOldMRules,SubNewMRules),
   trim_stacks,
   my_write('\nOld Literal Clauses\n'),
   write_ruleset(SubOldLRules),
   my_write('\nOld Modal Clauses\n'),
   write_ruleset(SubOldMRules),
   my_write('\nNew Literal Clauses\n'),
   write_ruleset(SubNewLRules),
   my_write('\nNew Modal Clauses\n'),
   write_ruleset(SubNewMRules),
   do_mres(SubOldLRules,SubOldMRules,SubNewLRules,SubNewMRules,ResLRules,ResMRules,NewOldLRules,NewOldMRules),
   NewCount is Count + 1,!,garbage_collect,
   cyclemres(NewCount,NewOldLRules,NewOldMRules,ResLRules,ResMRules,Initial,NewerLRules,NewerMRules).

/* ************************ */
/* SUBSUMPTION              */
/* ************************ */

/**********************************************************************************/
/*  subsumption(Old_Rules,New_Rules,SRules,New_Old_Rules,New_New_Rules,NewSRules) */
/*                                                                                */
/*   This predicate takes every rule in New_Rules and attempts subsumption        */
/*   between every rule and every other in this list. Then subsumption is         */
/*   performed between everything in the subsumed version of New_Rules and        */
/*   Old_Rules. New_Old_Rules is then the subsumed version of Old_Rules and       */
/*   New_New_Rules is the subsumed version of New_Rules. SRules contains rules    */
/*   which have been subsumed on previous cycles and NewSRules contain            */
/*   SRules plus any rules subsumed on this cycle.                                */
/**********************************************************************************/

subsumption(Old_Rules,New_Rules,New_Old_Rules,FNew_New_Rules) :-
  self_subsumption(New_Rules,Newish_Rules),
  flatten(Newish_Rules,FNewish_Rules),!,
  subsumption2(Old_Rules,FNewish_Rules,New_Old_Rules,New_New_Rules),
  flatten(New_New_Rules,FNew_New_Rules),!.

/******************************************************************************/
/*  self_subsumption([R | Rest],SRules,New_Rules,NewSRules)                   */
/*                                                                            */
/*  Attempts subsumption between R and Rest, recurrs on Rest and outputs      */
/*  subsumed list in New_Rules. SRules are the rules which have been subsumed */
/*  on previous cycles and any new ones subsumed are added to NewSRules.      */
/******************************************************************************/

self_subsumption([],[]):-!.

self_subsumption([R|Rest],[NewR|NewRules]):-
  subsume_rule(Rest,R,Remain_Rest,NewR),!,
  self_subsumption(Remain_Rest,NewRules).


/***********************************************************************************/
/*  subsumption2(OldRules,[R | Rest],SRules,New_Old_Rules,New_New_Rules,NewSRules) */
/*                                                                                 */
/*  Attempts subsumption between R and OldRules, recurrs on Rest and outputs       */
/*  lists after subsumption in New_Old_Rules and New_New_Rules. SRules are the     */
/*  rules which have been subsumed on previous cycles and any new ones             */
/*  subsumed are added to NewSRules.                                               */
/***********************************************************************************/

subsumption2(Old_Rules,[],Old_Rules,[]):-!.

subsumption2([],NewRules,[],NewRules):-!.

subsumption2(Old_Rules,[R|Rest],New_Old_Rules,[NewR|New_Rules]):-
   subsume_rule(Old_Rules,R,Newish_Old_Rules,NewR),!,
   subsumption2(Newish_Old_Rules,Rest,New_Old_Rules,New_Rules).

/*******************************************************************************/
/* subsume_rule(Old_Rules,ThisRule,SRules,New_Old_Rules,New_ThisRule,NewSRules)*/
/*                                                                             */
/*   This predicate compares ThisRule with every rule in Old_Rules. Rules are  */
/*   in the format r(Number,FromList,Text). If ThisRule is subsumed by a       */
/*   rule in Old_Rules then Old_rules are returned and New_ThisRule will be    */
/*   and empty list but if ThisRule subsumes a Rule in Old_Rules, the rule     */
/*   that has been subsumed is removed from Old_Rules and the predicate the    */
/*   attempts subsumption between ThisRule and the rest of Old_Rules.          */
/*   It is important that the predicates are in the order they appear in, as   */
/*   if there are duplicate rules ie Rule1 subsumes Rule2 and vice versa we    */
/*   want to remove ThisRule and return an empty list as New_ThisRule          */
/*   rather than the other way around for we are depending upon no new         */
/*   resolvents to be derived for our non-temporal resolution process to       */
/*   terminate. SRules contains any rules which have been subsumed previously  */
/*   NewSRules will also contain rules subsumed on this cycle.                 */
/*******************************************************************************/

% Firstly looking at cases of strong last implies something

subsume_rule([],r(N1,N2,G),[],[r(N1,N2,G)]):-!.
subsume_rule([],R,[],[R]):-!.

subsume_rule([r(N1,N2,F) | Rules],r(_,_,G),[r(N1,N2,F) | Rules],[]) :-
             dis_implies(F,G),!.                                % P => F

subsume_rule([r(_,_,F) | Rules],r(N1,N2,G),New_Old_Rules,New_New_Rules) :-
            dis_implies(G ,F),!,                        % Q => G subsumes P => F
	    subsume_rule(Rules,r(N1,N2,G),New_Old_Rules,New_New_Rules).

% default behaviour: ie no subsumption takes place

subsume_rule([ R | Rules],r(N1,N2,G),[ R | New_Rules],New_New_Rules) :-
	subsume_rule(Rules,r(N1,N2,G),New_Rules,New_New_Rules).

% default behaviour to catch any of the cases not mentioned above

subsume_rule([ R1 | Rules],R2,[ R1 | New_Old_Rules],New_New_Rules) :-
   subsume_rule(Rules,R2,New_Old_Rules,New_New_Rules).


/* ************************* */
/* NON-TEMPORAL RESOLUTION * */
/* ************************* */

/*********************************************************************************/
/* mres(OrigRules,[Rule|Rest],NewRules,Final)                                    */
/*                                                                               */
/*   Michaels code changed slightly to do non temporal resolution between        */
/*   two lists OrigRules and [Rule|Rest] (new rules derived from resolution last */
/*   time round the loop). After resolving Rule with every rule in OrigRules it  */
/*   is added to OrigRules, and the first rule from Rest is resolved with all    */
/*   rules in OrigRules + Rule, and so on. When Rest is empty, OrigRules which   */
/*   now has all of [Rule|Rest] attached is copied to NewOrig and resolvents     */
/*   found built up in Final. The first time round the loop ie after             */
/*   initial conversion to SNF OrigRules will be empty, so gradually [Rule|Rest] */
/*   will be copied into it and effectively resolved on itself. Rules are in the */
/*   form r(Number,FromList,Text) . When resolution takes place a new            */
/*   number for the rule is placed in Number and FromList will have the two rule */
/*   numbers of the parents of the new rule.                                     */
/*********************************************************************************/


do_mres(LNewOrig,MOrigRules,LNew,RNew,FinalL,FinalM,NewOrigL2,NewOrigM2):-
%    my_write('\ndo_mres ==> (2)\n'),
    do_mres(LNewOrig,MOrigRules,LNew,FinalL1,FinalM1,NewOrigL1,NewOrigM1),
    do_mres(NewOrigL1,NewOrigM1,RNew,FinalL2,FinalM2,NewOrigL2,NewOrigM2),
    append(FinalL1,FinalL2,FinalL),
    append(FinalM1,FinalM2,FinalM).

do_mres(LNewOrig,MNewOrig,[],[],[],LNewOrig,MNewOrig).
%:-my_write('\ndo_mres ==> (1).\n').

do_mres(LOrigRules,MOrigRules,[Rule|Rest],FinalL,FinalM,NewOrigL,NewOrigM) :-
%        my_write('\ndo_mres ==> (3).\n'),
	mresolve(Rule,LOrigRules,MOrigRules,RulesL1,RulesM1),
        add_to_set(LOrigRules,MOrigRules,Rule,Rest,RulesL1,RulesM1,FinalL,FinalM,NewOrigL,NewOrigM).

/*************************************************************************************************/

add_to_set(LOrigRules,MOrigRules,Rule,Rest,RulesL1,RulesM1,FinalL,FinalM,NewOrigL,NewOrigM) :-
%        my_write('\nadd_to_set ==> (1)\n'),
        is_modal_rule(Rule),!,
	do_mres(LOrigRules,[Rule|MOrigRules],Rest,NewRulesL,NewRulesM,NewOrigL,NewOrigM),
	append(RulesL1,NewRulesL,FinalL),
	append(RulesM1,NewRulesM,FinalM).

add_to_set(LOrigRules,MOrigRules,Rule,Rest,RulesL1,RulesM1,FinalL,FinalM,NewOrigL,NewOrigM) :-
%        my_write('\nadd_to_set ==> (2)\n'),
	do_mres([Rule|LOrigRules],MOrigRules,Rest,NewRulesL,NewRulesM,NewOrigL,NewOrigM),
	append(RulesL1,NewRulesL,FinalL),
	append(RulesM1,NewRulesM,FinalM).

/*********************************************************************************/
/*  mresolve(Rule,OrigRules,NewRules)                                            */
/*                                                                               */
/*  Takes a rule of the form r(Number,FromList,Text) and tries resolution        */
/*  between the Text of this rule and the Text of all the other rules in         */
/*  OrigRules. Any new resolvents derived are put into new rules where the       */
/*  Number of the new resolvent is the next new rule number, and FromList is     */
/*  filled with the rule numbers of the parents of the resolvent.                */
/*********************************************************************************/

/* First,the cases where resolution cannot take place */

mresolve(_,[],[],[],[]).
% :-my_write('\nmresolve ==> (1)\n').

/* Next,resolution of initial rules: */

mresolve(r(N1,N2,F),[r(N3,_,G) | Rest],Other,LFinal,MFinal) :-
%        my_write('\nmresolve ==> (2)\n'),
        basic_nt_resolve(F,G,Resolvent,[],[]),
	mresolve(r(N1,N2,F),Rest,Other,NewLRules,NewMRules),
	generate_new_resolvent([N1,N3],Resolvent,NewLRules,NewMRules,LFinal,MFinal).

mresolve(Rule,[_ | Rest],Other,NewLRules,NewMRules) :-
%        my_write('\nmresolve ==> (3)\n'),
	mresolve(Rule,Rest,Other,NewLRules,NewMRules).

mresolve(r(N1,N2,F),[],[r(N3,_,G) | Rest],LFinal,MFinal) :-
%        my_write('\nmresolve ==> (4)\n'),
        basic_nt_resolve(F,G,Resolvent,[],[]),
	mresolve(r(N1,N2,F),[],Rest,NewLRules,NewMRules),
	generate_new_resolvent([N1,N3],Resolvent,NewLRules,NewMRules,LFinal,MFinal).

mresolve(Rule,[],[_ | Rest],NewLRules,NewMRules) :-
%        my_write('\nmresolve ==> (5)\n'),
	mresolve(Rule,[],Rest,NewLRules,NewMRules).

/*****************************************************************************/
/*  generate_new_resolvent(FromList,ResolventList,Rules,NewRules             */
/*                                                                           */
/* If Resolvent is empty this just returns Rules as they are, otherwise a    */
/* new rule is created and added to the front of Rules.                      */
/*****************************************************************************/

generate_new_resolvent(_,[],LR,MR,LR,MR).

generate_new_resolvent(FromList,[Resolvent],LRules,MRules,FinalLRules,FinalMRules):-
      is_modal_disjunct(Resolvent),!,
      simplify_rule(r(_,FromList,Resolvent),[r(_,FromList,SimpRule)]),
      change_form(SimpRule,RulesList),
      number_rules(RulesList,FromList,NumberedRules),
      simplify(NumberedRules,SimpRules),
      separate_rules(SimpRules,_ERules,_Initial,NewLRules,NewMRules,_Temporal),
      append(LRules,NewLRules,FinalLRules),
      append(MRules,NewMRules,FinalMRules).

generate_new_resolvent(FromList,[Resolvent],LRules,MRules,[SimpRule | LRules],MRules):-
      newrulecount(N),
      simplify_rule(r(N,FromList,Resolvent),[SimpRule]).

/******************************************************************************/
/*  basic_nt_resolve(Disj1,Disj2,Resolvent,S1,S2) :-                          */
/*                                                                            */
/*      Tries to resolve the clauses Disj1 and Disj2 together.                */
/*      If it fails, Resolvent is [], if it succeeds,                         */
/*	resolvent is [resolvent]. S1 is a list recording what is left          */
/*	of Disj1, while S2 is what is left of Disj2.                           */
/******************************************************************************/ 

basic_nt_resolve(A imp B,Disj2,Resolvent,S1,S2):- 
        snff_negate(A,NegA),!,
        basic_nt_resolve(NegA or B,Disj2,Resolvent,S1,S2).        

basic_nt_resolve(Disj1,A imp B,Resolvent,S1,S2):-
        snff_negate(A,NegA),!,
        basic_nt_resolve(Disj1,NegA or B,Resolvent,S1,S2).

basic_nt_resolve(A or B,Disj2,Resolvent,S1,S2) :-
	basic_nt_resolve(A,Disj2,Resolvent,[B|S1],S2).

basic_nt_resolve(A or B,Disj2,Resolvent,S1,S2) :-
	basic_nt_resolve(B,Disj2,Resolvent,[A|S1],S2).

basic_nt_resolve(_ or _, _,_,_,_) :- fail.

basic_nt_resolve(A,Disj2,Resolvent,S1,S2) :-
	internal_nt_resolve(A,Disj2,Resolvent,S1,S2).

/*****************************************************************************/
/*  internal_nt_resolve(L,Disj, Resolvent,S1,S2)                             */
/*                                                                           */
/*  Attempts to resolve L which is now a proposition or negated proposition  */
/*  with Disj. S1 and S2 have what remains of the original two disjuncts     */
/*  we were trying to resolve. The resolvent created is returned in          */
/*  Resolvent                                                                */
/*****************************************************************************/

internal_nt_resolve(L,A or B, Resolvent,S1,S2) :-
	internal_nt_resolve(L,A,Resolvent,S1,[B|S2]).

internal_nt_resolve(L,A or B,Resolvent,S1,S2) :- 
	internal_nt_resolve(L,B,Resolvent,S1,[A|S2]).

internal_nt_resolve(_,_ or _,_,_,_) :- fail.

internal_nt_resolve(L,M,[Resolvent],S1,S2) :-
        resolveable(L,M,S1,S2,NewS1,NewS2),
	append(NewS1,NewS2,NewLst),
	disjoin(NewLst,Resolvent).

/*********************/
/* Basic Resolution  */
/*********************/

resolveable(L,M,S1,S2,S1,S2):-
  comp_lits(L,M).

/********************/
/* Modal Resolution */
/********************/

resolveable(L,M,S1,S2,NewS1,NewS2):-
  sometime_rule(L,M,S1,S2,NewS1,NewS2). 

resolveable(L,M,S1,S2,NewS1,NewS2):- 
  always_rule(L,M,S1,S2,NewS1,NewS2). 

/**********/ 
/* MRES 2 */ 
/**********/ 

sometime_rule(k L,k M,S1,S2,S1,S2):-
  comp_lits(L,M). 

/**********/ 
/* MRES 3 */ 
/**********/ 

sometime_rule(k L,M,S1,S2,S1,S2):-
  comp_lits(L,M).

sometime_rule(L,k M,S1,S2,S1,S2):-
  comp_lits(L,M).

/**********/ 
/* MRES 4 */ 
/**********/ 

always_rule(neg k L,L,S1,S2,S1,NewS2):-
  change_resolvent(S2,NewS2).

always_rule(L,neg k L,S1,S2,NewS1,S2):-
  change_resolvent(S1,NewS1).

always_rule(neg k neg L,M,S1,S2,S1,NewS2):-
  comp_lits(L,M), 
  change_resolvent(S2,NewS2). 

always_rule(L,neg k neg M,S1,S2,NewS1,S2):- 
  comp_lits(L,M), 
  change_resolvent(S1,NewS1). 

/****************/ 
/* MOD FUNCTION */ 
/****************/ 

change_resolvent([A or B|Rest], NewAll):-
  change_resolvent(A or B,NewAB),
  change_resolvent(Rest, NewRest), 
  append(NewAB,NewRest, NewAll).

change_resolvent(A or B, NewAB):-
  change_resolvent(A, NewA),
  change_resolvent(B, NewB),
  append(NewA, NewB, NewAB).

change_resolvent([A|Rest],NewAll):-
  change_resolvent(A,NewA),
  change_resolvent(Rest,NewRest),
  append(NewA,NewRest,NewAll).

change_resolvent([],[]).

change_resolvent(X,[X]):-is_snl_literal(X).

change_resolvent(neg k L,[neg k L]):-is_literal(L).

change_resolvent(k L,[k L]):-is_literal(L).

change_resolvent(k neg L,[k neg L]):-is_literal(L).        % Clare is this right?

change_resolvent(neg k neg L,[neg k neg L]):-is_literal(L).

change_resolvent(neg L,[neg k L]):-is_literal(L).

change_resolvent(L,[neg k neg L]):-is_literal(L). 

/**********************************/ 

is_modal_rule(r(_,_,Y imp X)):- 
   is_modal_rule(Y imp X). 

is_modal_rule(_Y imp X):- 
  is_modal_disjunct(X). 

/**********************************/ 

is_modal_disjunct(k _). 

is_modal_disjunct(neg k _). 

is_modal_disjunct(A or _):- 
  is_modal_disjunct(A). 

is_modal_disjunct(_ or B):- 
  is_modal_disjunct(B). 

/***************************************/

is_list_of_only_modals(A or B):-
  is_modal(A), 
  is_list_of_only_modals(B).

is_list_of_only_modals(A):-
  is_modal(A).

is_list_of_only_literals(A or B):-
  is_literal(A), 
  is_list_of_only_literals(B).

is_list_of_only_literals(A):-
   is_literal(A). 

/*****************************************************************/
/* The following returns a modal clause in the new normal form   */
/*****************************************************************/ 

change_form(true,[true]).

change_form(Disjunction,[true imp Disjunction]):-
  is_modal(Disjunction).

change_form(neg k X or Disj1, [Disj2 imp neg k X]):-
  is_literal(Disj1),!,
  snff_negate(Disj1,Disj2).

change_form(k X or Disj1, [Disj2 imp k X]):-
  is_literal(Disj1),!,
  snff_negate(Disj1,Disj2).

change_form(Disj1 or neg k X, [Disj2 imp neg k X] ):-
  is_literal(Disj1),!,
  snff_negate(Disj1,Disj2).

change_form(Disj1 or k X, [Disj2 imp k X]):-
  is_literal(Disj1),!,
  snff_negate(Disj1,Disj2).

change_form(Disjunction,[Disjunction]):-
   is_list_of_only_literals(Disjunction).

change_form(Disjunction,List):- 
  is_list_of_only_modals(Disjunction), 
  snl_form(Disjunction,Literals,Modals),
  append([Literals],Modals,List). 

change_form(Disj1 or Disj2,Rest):-
  is_literal(Disj1), 
  snff_rewrite_list([neg Disj1 imp (Disj2)],Rest).

change_form(Disj1 or Disj2,Rest):- 
  is_not_literal(Disj1),!, 
  change_order(Disj1 or Disj2, NewDisj),
  change_form(NewDisj,Rest). 

/************************************************/ 

change_order(A or B,NewOrder):- 
  strip_or(A,NewA), 
  strip_or(B,NewB), 
  flatten(NewA,FlatA), 
  flatten(NewB,FlatB), 
  append(FlatB,FlatA,New), 
  disjoin(New,NewOrder). 


/*********************************************************************/
/* The following returns the SNL literals and definition clauses     */
/* from a given formula                                              */
/*                                                                   */
/* (1) the SNL literal itself                                        */
/* (2) the disjunction of SNL literals                               */
/* (3) the SNL(NEW), in case of a conjunction                        */
/* (4) SNL(literal), in case of a non SNL literal                    */
/*                                                                   */
/*********************************************************************/

snl(Formula,Formula,_,[],[]):- 
   is_snl_literal(Formula). 

snl(NewLiteralA or NewLiteralB,A or B,N,Literals,Modals):- 
   snl(NewLiteralA,A,N,Literals1,Modals1), 
   snl(NewLiteralB,B,N,Literals2,Modals2), 
   append(Literals1,Literals2,Literals), 
   append(Modals1,Modals2,Modals). 

snl(NewLiteral,A and B,N,Literals,Modals):- 
  snl_conjunction(NewLiteral,A and B,N,Literals,Modals). 

snl(NewLiteral,Formula,N,[],[Rule1,Rule2]):- 
   is_literal(Formula),!, 
   new_dontknow_prop(NewLiteral,Formula), 
   snff_negate(Formula, NegF), 
   snff_negate(NewLiteral, NegNewLiteral), 
   number_rule(NewLiteral imp neg k NegF,N,Rule1), 
   number_rule(NegNewLiteral imp k NegF,N,Rule2). 

/****************************************************************/ 
/* The following takes a formula and returns the corresponding  */
/* SNL definition clauses.                                      */
/****************************************************************/ 


snl_form(k L,[NegNewProp],[NewProp imp neg k L, NegNewProp imp k L]):- 
   snff_negate(L,NegL), 
   new_dontknow_prop(NewProp, NegL), 
   snff_negate(NewProp,NegNewProp). 

snl_form(neg k L,[NewProp],[NewProp imp neg k L, NegNewProp imp k L]):- 
   snff_negate(L,NegL), 
   new_dontknow_prop(NewProp, NegL), 
   snff_negate(NewProp,NegNewProp). 

snl_form(A or B,Literals,Modals):- 
   snl_form(A,Literals1,Modals1), 
   snl_form(B,Literals2,Modals2), 
   append(Literals1,Literals2,LiteralList), 
   disjoin(LiteralList,Literals), 
   append(Modals1,Modals2,Modals). 

/************************************************************/ 
/* The following deals with conjunctions of literals        */
/* and returns the new proposition renaming the conjunction */
/* as well as its corresponding definition clauses          */
/************************************************************/

/** Conjunctions of only snl literasl **/

snl_conjunction(SimpSNL,Conjunction,_,[],[]):-
  strip_and(Conjunction,List), 
  flatten(List,FList), 
  remove_news(FList,NewList),
  separate_snl_literals(NewList,NewishList,OldSNLList),
  subset(NewishList,[]), 
  conjoin(OldSNLList,OldSNL), 
  simplify_and(OldSNL,SimpSNL). 

/** conjunctions with at most one non snl-literal **/

snl_conjunction(NewConj,Conjunction,N,[],[Rule1,Rule2]):- 
  strip_and(Conjunction,List),
  flatten(List,FList),
  remove_news(FList,NewList),
  separate_snl_literals(NewList,NewishList,OldSNLList),
  test_conjunction(NewishList,Prop), 
  conjoin(OldSNLList,OldSNL), 
  new_dontknow_prop(NewSNL,Prop), 
  simplify_and(OldSNL and NewSNL,NewConj), 
  snff_negate(Prop,NegProp), 
  snff_negate(NewSNL,NegNewSNL), 
  number_rule(NewSNL imp neg k NegProp,N,Rule1), 
  number_rule(NegNewSNL imp k NegProp,N,Rule2). 

/** conjunction with more than one non snl literal **/

snl_conjunction(NewConj,Conjunction,N,Literals,SimplifiedRules):-
  strip_and(Conjunction,List),
  flatten(List,FList),
  remove_news(FList,NewList),
  separate_snl_literals(NewList,NewishList,OldSNLList),
  conjoin(OldSNLList,OldSNL),
  conjoin(NewishList,Formula),
  simplify_and(Formula,SFormula),
  new_conj_prop(NewProp,SFormula),
  snff_negate(SFormula,NFormula),
  number_rule(NewProp or NFormula,N,Definition),
  simplify([Definition],Literals),  
  new_dontknow_prop(NewSNL,NewProp),
  generate_new_modals(NewSNL,N,NewishList,NewModals),
  simplify_and(OldSNL and NewSNL,NewConj),
  snff_negate(NewProp,NegNewProp),
  snff_negate(NewSNL,NegNewSNL),
  number_rule(NewSNL imp neg k NegNewProp,N,Rule1),
  number_rule(NegNewSNL imp k NegNewProp,N,Rule2),
  append([Rule1],[Rule2],Definitions),
  append(Definitions,NewModals,Rules),
  simplify(Rules,SimplifiedRules).

/* generate new modals for conjunctions */

generate_new_modals(_,_,[],[]).

generate_new_modals(NewSNL,N,[Head|Tail],[Definition|NewModals]):-
  snff_negate(NewSNL,NegNewSNL),
  snff_negate(Head,NegHead),
  number_rule(NegNewSNL imp k NegHead,N,Definition),
  generate_new_modals(NewSNL,N,Tail,NewModals).

test_conjunction([Head|Tail],Head):- 
  subset(Tail,[]). 

/* takes a new proposition and returns its label */

remove_news([],[]). 

remove_news([new(X)|List],[X|NewList]):- 
  remove_news(List,NewList). 

remove_news([X|List],[X|NewList]):- 
  remove_news(List,NewList). 

/* remove SNL literals from a list of conjuncts */

separate_snl_literals(List,Literals,SNLLiterals):-
  sublist(is_snl_literal,List,SNLLiterals),
  subtract(List,SNLLiterals,Lit),
  sort(Lit,Literals).

/* My implementation of MRES5                    */

my_k_to_literal([],[]):- !. 

my_k_to_literal([r(N,_M,X imp k Y)|Rest],[Rule|NewRest]):-!, 
  snff_negate(X,NegX),
  number_rule(NegX or Y,[N,mres5],Rule),
  my_k_to_literal(Rest,NewRest).
 
my_k_to_literal([r(_,_,_ imp neg k _)|Rest],NewRest):-!, 
  my_k_to_literal(Rest,NewRest).


:- include(myrempres).

/* Predicates for collecting statistics */

collect_statistics((CPUTime,Inferences,LocalUsed,GlobalUsed,TrailUsed)):-
  statistics(cputime,CPUTime),
  statistics(inferences,Inferences),
  statistics(localused,LocalUsed),
  statistics(globalused,GlobalUsed),
  statistics(trailused,TrailUsed).

calculate_statistics((CPUTime1,Inferences1,LocalUsed1,GlobalUsed1,TrailUsed1),
		     (CPUTime2,Inferences2,LocalUsed2,GlobalUsed2,TrailUsed2)):-
  write('\n\nStatistics\n\n'),
  CPUTime is CPUTime2 - CPUTime1,
  writef('CPUTime:%w\n',[CPUTime]),
  Inferences is Inferences2 - Inferences1,
  writef('Inferences:%w\n',[Inferences]),
  LocalUsed is LocalUsed2 - LocalUsed1,
  writef('Local Stack:%t\n',[LocalUsed]),
  GlobalUsed is GlobalUsed2 - GlobalUsed1,
  writef('Global Stack:%t\n',[GlobalUsed]),
  TrailUsed is TrailUsed2 - TrailUsed1,
  writef('Trail Stack:%t\n',[TrailUsed]).

/* Predicate that invokes the other predicates that will perform resolution */
     
otres(Rules):-
  clear,
  collect_statistics(X),
  otres_allres(Rules,_),
  collect_statistics(Y),
  calculate_statistics(X,Y),
  newrulecount(N),
  Counter is N - 1,
  writef('\nNumber of clauses generated: %t\n',[Counter]).

/************************************************************/
/* First call to the predicates that perform resolution.    */
/* It takes a formula, transform it into the snf, set the   */
/* initial literal to be unique, number and separate rules, */
/* generate the SNL clauses and call modal resolution.      */
/************************************************************/

otres_allres(Rules,AllNewRules):-
   startrulecount(_),
   snff(Rules,SNFRules),
   check_initial(SNFRules,NewSNFRules),
   number_rules(NewSNFRules,[],NumberedSNF),
   separate_and_format_rules(NumberedSNF,ERules,ERulePaths,OtherRules),
   generate_snl(OtherRules,NOtherRules),
   simplify_lists(NOtherRules,(Initial,Literal,Modal,Temporal)),
   my_write('\nSet of clauses after translation to SNF :\n'),
   write_ruleset((Initial,Literal,Modal,Temporal)),
   mres(Literal,Modal,Initial,NewerLiteral,NewerModal),
   write('\notres_allres (1) ==> Usable rules are \n'),
   write_otter_rules(NewerLiteral),nl,
   write('\notres_allres (1) ==> Rules in the SoS are \n'),
   write_otter_rules(Temporal),nl,
   read(OtterRules),
   otres_test_otter_output(OtterRules,(Initial,NewerLiteral,NewerModal,Temporal),ERulePaths,ERules,AllNewRules).

/* predicate called after temporal resolution takes place */

otres_allres(OldRules,NewRules,ERulePaths,ERules,AllNewRules):-
   number_rules(NewRules,[],NumberedRules),
   simplify(NumberedRules,SimpleRules),
   split_temp_lit_rules(SimpleRules,NewLiteral,NewTemporal),
   my_write('otres_allres (2) ==> New literal rules are \n'),write_ruleset(NewLiteral),
   my_write('otres_allres (2) ==> New temporal rules are \n'),write_ruleset(NewTemporal),
   step_resolution(NewLiteral,NewTemporal,OldRules,ERules,ERulePaths,AllNewRules).

/************************************************************************************/
/* the next predicates are  renaming possible disjunctions on the rhs of initial    */
/* clauses. This is because in this program, the initial resolution is NOT          */
/* implemented. Instead, you have a simple check: if neg X (in the literal clauses) */
/* is generated, checks if X is in the initial set, generating false in the literal */
/* set of clauses.                                                                  */
/************************************************************************************/

check_initial(Initial,Final):-
   new_temp_prop(V),
   change_initial(Initial,V,NewList),
   append([start imp V],NewList,Final).

change_initial([],_,[]):-!.

change_initial([start imp X|Rest],V,[neg V or X|NewRest]):-
   change_initial(Rest,V,NewRest).

change_initial([H|Rest],V,[H|NewRest]):-
   change_initial(Rest,V,NewRest).

/********************************************************************************/
/* The following generates the SNL clauses and definitions from a given set of  */
/* step clauses.                                                                */
/********************************************************************************/

generate_snl((Initial,Literal,Modal,Temporal),(Initial,NewLiteral,NewModal,NewTemporal)):-
   my_write('\nGenerating SNL Clauses\n'),
   my_write('\nTemporal Clauses are \n'),
   write_ruleset(Temporal),
   generate_snl(Temporal,Literal,Modal,NewTemporal,NewLiteral,NewModal),
   my_write('\nNew Temporal Clauses are \n'),
   write_ruleset(NewTemporal).

generate_snl([],Literal,Modal,[],Literal,Modal).

generate_snl([r(N1,M,X imp next Y)|List],OldLiteral,OldModal,
 [r(N1,M,X imp next Y),r(N2,[N1],NewX imp next NewY)|NewList],NewerLiteral,NewerModal):-
   newrulecount(N2),
   snl(NewX,X,[N1],NewLiterals1,NewModals1),
   snl(NewY,Y,[N1],NewLiterals2,NewModals2),
   append(NewLiterals1,NewLiterals2,NewLiteral),
   append(NewLiteral,OldLiteral,Literal),
   append(NewModals1,NewModals2,NewModal),
   append(NewModal,OldModal,Modal),
   generate_snl(List,Literal,Modal,NewList,NewerLiteral,NewerModal).

generate_snl([X|List],OldLiteral,OldModal,[X|NewList],NewerLiteral,NewerModal):-
   generate_snl(List,OldLiteral,OldModal,NewList,NewerLiteral,NewerModal).

/********************************************************************************/
/* The following generates the SNL clauses and definitions from a given set of  */
/* step clauses, after resolution has been performed.                           */
/********************************************************************************/

generate_otter_snl(OldRules,NewOldRules,NewishLiteral,OldModal,SNewModal):-
   generate_snl(OldRules,[],OldModal,NewOldRules,NewLiteral,NewModal),
   remove_duplicate_rules(NewLiteral,NewerLiteral),
   remove_already_existing(NewerLiteral,NewOldRules,NewishLiteral),
   get_new_modal_rules(OldModal,NewModal,NewerModal),
   remove_duplicate_rules(NewerModal,SNewModal).

/* Remove already existing clauses */

remove_already_existing([],_,[]).

remove_already_existing([r(_,_,Rule)|Tail],OldRules,ReallyNew):-
   test_member(Rule,OldRules),!,
   remove_already_existing(Tail,OldRules,ReallyNew).

remove_already_existing([Head|Tail],OldRules,[Head|ReallyNew]):-   
   remove_already_existing(Tail,OldRules,ReallyNew).

/**********************************************************************************/

separate_and_format_rules(Rules,ERules,ERulePaths,(InitialRules,LiteralRules,ModalRules,TemporalRules)):-
   separate_rules(Rules,AllERules,InitialRules,LiteralRules,ModalRules,TemporalRules),
   remove_duplicate_erules(AllERules, ERules),
   my_write('\n\nThe list of eventuality rules is \n\n'),
   write_ruleset(ERules),
   process_eventualities(ERules, ERulePaths).

/*****************************************************************************/
/* separate_rules(Rules, ERules Others)                                      */
/*                                                                           */
/* This predicate takes a list of rules and splits it into two lists one     */
/* containing the separate_rules which have to be solved and the other       */
/* containg the remaining rules which are global box rules ie                */
/*           slast P imp F,                                                  */
/* where P is a conjunction and F a disjunction, any other rules being       */
/* disregarded.                                                              */
/*                                                                           */
/*****************************************************************************/

separate_rules([],[],[],[],[],[]):-!.

separate_rules([r(N1,N2, F imp sometime G)|Rules],
               [r(N1,N2, F imp sometime G)|ERules],InitialRules,LiteralRules,ModalRules,TemporalRules) :- !,
    separate_rules(Rules,ERules,InitialRules, LiteralRules, ModalRules, TemporalRules).
% Add sometime clause to ERules


separate_rules([r(N1,N2,P imp next F)|Rules],
                ERules,InitialRules, LiteralRules, ModalRules,[r(N1,N2, P imp next F)|TemporalRules]) :- !,
   separate_rules(Rules,ERules, InitialRules, LiteralRules, ModalRules, TemporalRules).
% Add step clauses to TemporalRules

separate_rules([r(N1,N2,start imp F)|Rules],
               ERules,[r(N1,N2,F)|InitialRules],LiteralRules, ModalRules,TemporalRules) :- !,
   separate_rules(Rules,ERules,InitialRules, LiteralRules,ModalRules, TemporalRules).
% Add initial clauses to InitialRules

separate_rules([r(N1,N2,X imp k A)|Rules],
               ERules,InitialRules,LiteralRules, [r(N1,N2, X imp k A)|ModalRules],TemporalRules) :-!,
   separate_rules(Rules,ERules, InitialRules, LiteralRules, ModalRules, TemporalRules).
% Add modal clause to ModalRules

separate_rules([r(N1,N2,X imp neg k A)|Rules],
               ERules,InitialRules, LiteralRules, [r(N1,N2, X imp neg k A)|ModalRules],TemporalRules) :-!,
   separate_rules(Rules,ERules, InitialRules, LiteralRules,ModalRules, TemporalRules).
% Add modal clause to ModalRules

separate_rules([r(N1,N2,F)|Rules],
               ERules,InitialRules,[r(N1,N2,F)|LiteralRules], ModalRules,TemporalRules) :-
   disjunction_of_literals(F),!,
   separate_rules(Rules,ERules, InitialRules, LiteralRules,ModalRules, TemporalRules).
% Add literal clause to LiteralRules

separate_rules([_|Rules],ERules,InitialRules, LiteralRules, ModalRules,TemporalRules) :- 
   separate_rules(Rules,ERules, InitialRules, LiteralRules, ModalRules,TemporalRules).
% Ignore other rules.

/*****************************************************************************/
/* remove_duplicate_erules(OldList, NewList).                                */
/*                                                                           */
/* Takes old list and removes duplicates to give NewList.                    */
/* N.B. Only works with  with initial and global eventuality rules and NOT   */
/* initial and global box rules.                                             */
/*                                                                           */
/* Two predicates have been maintained as the one above is called frequently */
/* during nontemporal resolution where as this one is only called            */
/* occasionally.                                                             */
/*****************************************************************************/

remove_duplicate_rules([],[]).

remove_duplicate_rules([r(_,_,Rule)|Rest], NewRules):-
   test_member(Rule, Rest),!,
   remove_duplicate_rules(Rest, NewRules).

remove_duplicate_rules([Rule|Rest], [Rule|NewRules]):-
   remove_duplicate_rules(Rest, NewRules).

/*******************************************************/

remove_duplicate_erules([],[]).

remove_duplicate_erules([Rule|Rest], NewRules):-
   erule_in_list(Rule, Rest),!,
   remove_duplicate_erules(Rest, NewRules).

remove_duplicate_erules([Rule|Rest], [Rule|NewRules]):-
   remove_duplicate_erules(Rest, NewRules).

/*****************************************************************************/
/* erule_in_list(Rule, List)                                                 */
/*                                                                           */
/* Takes a rule which is either an initial or global eventuality rule and    */
/* tests whether it is a member of List.                                     */
/*****************************************************************************/

erule_in_list(r(_,_, P imp sometime F), [r(_,_,Q imp sometime F)|_]):-
   strip(P, StrippedP),
   strip(Q, StrippedQ),
   same(StrippedP, StrippedQ).

erule_in_list(r(N1,N2, P imp sometime F), [_|Rest]):-
   erule_in_list(r(N1,N2, P imp sometime F), Rest).

/*********************************************************************************************/
/* process_eventualities(EList, ShortEList)                                                  */
/*                                                                                           */
/* Takes a list of eventualities EList removes the eventaulity with no duplicates and stores */
/* in a structured list as below.                                                            */
/*********************************************************************************************/

process_eventualities(EList, ShortEList):-
   extract_eventualities(EList, NewEList),
   remove_duplicate(NewEList, NewerEList),
   eventualities_paths(NewerEList,ShortEList).

/*********************************************************************************************/
/* extract_eventualites(EList, ShortEList)                                                   */
/*                                                                                           */
/* Takes a list of eventualities EList removes the eventuality with no duplicates            */
/*********************************************************************************************/

extract_eventualities([],[]).

extract_eventualities([r(_,_, _ imp sometime F)|Rest], [F|NewRest]):-
    extract_eventualities(Rest, NewRest).

/********************************************************************************************/
/*   eventualities_paths(EList,NewEList).                                                    */
/*                                                                                           */
/* Takes a list of eventuality rules (EList), and makes it into a list whose members are     */
/* sublists of the form [ERule, []]...etc where the [] is for filling with the previous paths*/
/* found for that eventuality.                                                               */
/*                                                                                           */
/*********************************************************************************************/

eventualities_paths([],[]).

eventualities_paths([H|Tail],[[H,[]]|NewTail]):-
  eventualities_paths(Tail,NewTail).

/*****************************************************************************************/

simplify_lists((Initial,Literal,Modal,Temporal),(SInitial,SLiteral,SModal,STemporal)):-
    simplify(Initial,SInitial),
    simplify(Literal,SLiteral),
    simplify(Modal,SModal),
    simplify(Temporal,STemporal).

/*****************************************************************************************/

step_resolution(NewerLiteral,NewerTemporal,(Initial,Literal,Modal,Temporal),ERules,ERulePaths,AllNewRules):-
   mres(Literal,Modal,NewerLiteral,[],Initial,NewLiteral,NewModal),
   do_step_resolution(NewerLiteral,NewerTemporal,(Initial,Literal,Modal,Temporal),ERules,ERulePaths,AllNewRules,NewLiteral,NewModal).

do_step_resolution(_,_,_,_,_,_,[r(0,[],false)],_):-
   otres_test_new_rules(_,r(0,[],false),_,_,_,_).

do_step_resolution(NewerLiteral,NewerTemporal,(Initial,Literal,_,Temporal),ERules,ERulePaths,AllNewRules,NewLiteral,NewModal):-
%   write(Literal),nl,write(Modal),nl,write(NewerLiteral),nl,write(NewLiteral),nl,
   my_write('step_resolution ==> New temporal rules are\n'),write_ruleset(NewerTemporal),
   get_new_literal_rules(Literal,NewLiteral,ExtraLiteral),
   my_write('step_resolution ==> New literal rules are\n'),write_ruleset(ExtraLiteral),
   write('\nstep_resolution ==> Usable rules are \n'),
   write_otter_rules(Literal,Temporal),nl,
   write('\nstep_resolution ==>Rules in the SoS are \n'),
   write_otter_rules(NewerTemporal,ExtraLiteral),nl,
   read(OtterRules),
   otres_test_otter_output(OtterRules,(Initial,NewerLiteral,NewModal,Temporal),ERulePaths,ERules,AllNewRules).

/**********************************************************************/

otres_test_otter_output([true imp next false],(_,_,_,_),_,_,NewAllRules):-
   write('\nHave generated false (1).\n'),
   otres_test_new_rules(_,r(0,[],false),_,_,_,NewAllRules).

otres_test_otter_output([false],(_,_,_,_),_,_,NewAllRules):-
   write('\nHave generated false (2).\n'),
   otres_test_new_rules(_,r(0,[],false),_,_,_,NewAllRules).

otres_test_otter_output(OldOtterRules,([r(_,_,X)],Literal,_,Temporal),_,_,NewAllRules):-
   test_member(neg X,OldOtterRules),
   rewrite_rules(OldOtterRules,NewOldRules,NewOtterRules),
   give_rules_old_numbers(NewOldRules,Literal,Temporal,NewishOldRules),
   sort(NewishOldRules,SortedRules),
   my_write('\nOutput from otter ==> New literal rules are \n'),write_ruleset(NewOtterRules),
   my_write('\nOutput from otter ==> Remaining rules are\n'),write_ruleset(SortedRules),
   write('\nHave generated false (3).\n'),
   otres_test_new_rules(_,r(0,[],false),_,_,_,NewAllRules).

otres_test_otter_output(OldOtterRules,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules):-
   rewrite_rules(OldOtterRules,NewOldRules,NewOtterRules),
   give_rules_old_numbers(NewOldRules,Literal,Temporal,NewishOldRules),
%   my_write('\nOldOtterRules - All Rules coming from otter\n'),write_ruleset(OldOtterRules),
%   my_write('\n\nNewOtterRules - Simplified Rules (p imp next false\n'),write_ruleset(NewOtterRules),
%   my_write('\n\nNewOldRules - Remaining rules\n'),write_ruleset(NewishOldRules),
    generate_otter_snl(NewishOldRules,SNLNewOldRules,NewLiteral,Modal,NewModal),
    append(NewOtterRules,NewLiteral,NewerOtterRules),
    my_write('\nOutput from otter ==> New literal rules (A imp next false) are \n'),write_ruleset(NewerOtterRules),
    my_write('\nOutput from otter ==> Remaining rules (after snl) are\n'),write_ruleset(SNLNewOldRules),
    my_write('\nOutput from snl   ==> New modal rules are\n'),write_ruleset(NewModal),
    run_tempres_or_otter(SNLNewOldRules,NewerOtterRules,NewModal,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules).

/**********************************************************************/

/**********************************************************************/

run_tempres_or_otter(NonTempRules,[],NewModal,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules):-
   my_write('\nrun_tempres_or_otter ==> (1)\n'),
   split_temp_lit_rules(NonTempRules,NewLiteral,NewTemporal),
%   my_write('NonTempRules - All rules coming from otter\n'),write_ruleset(NonTempRules),
%   my_write('NewLiteral - Literal rules coming from otter\n'),write_ruleset(NewLiteral),
%   my_write('NewTemporal - Temporal rules coming from otter\n'),write_ruleset(NewTemporal),
   get_new_literal_rules_after_temporal(Literal,NewLiteral,SmallNewLiteral),
   my_write('Literal - Old Literal clauses\n'),write_ruleset(Literal),
   my_write('NewLiteral - All Literal clauses coming from otter\n'),write_ruleset(NewLiteral),
   my_write('SmallNewLiteral - the really new clauses generated by otter\n'),write_ruleset(SmallNewLiteral),
   run_tempres_or_modal(SmallNewLiteral,NewLiteral,NewModal,NonTempRules,NewTemporal,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules).

run_tempres_or_otter(NonTempRules,[],_,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules):-
   my_write('\nrun_tempres_or_otter ==> (2)\n'),
   write_ruleset(NonTempRules),
   my_write('Attempting Breadth-First Temporal Resolution \n'),
   full_tempres(NonTempRules,ERulePaths, ERules, MoreRules,NewERulePaths,_RulesUsed),
   otres_test_new_rules(NonTempRules,MoreRules,(Initial,Literal,Modal,Temporal),NewERulePaths,ERules,AllNewRules).

run_tempres_or_otter(UsableRules,SoSRules,_,(Initial,Literal,Modal,Temporal),ERulePaths,ERules,AllNewRules):-
   my_write('\nrun_tempres_or_otter ==> (3)\n'),
   write('\nrun_tempres_or_otter ==> Usable rules are \n'),
   write_otter_rules(UsableRules),nl,
   write('\nrun_tempres_or_otter ==> Rules in the SoS are \n'),
   write_otter_rules(SoSRules),nl,
   read(OtterRules),
   otres_test_otter_output(OtterRules,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules).

/**********************************************************************/

run_tempres_or_modal([],NewLiteral,_,AllTemporal,NewTemporal,(Initial,_Literal,Modal,_Temporal),ERulePaths, ERules,AllNewRules):-
   my_write('\nrun_tempres_or_modal ==> (1)\n'),
   my_write('Attempting Breadth-First Temporal Resolution \n'),
   full_tempres(AllTemporal,ERulePaths, ERules, MoreRules,NewERulePaths,_RulesUsed),
   otres_test_new_rules(AllTemporal,MoreRules,(Initial,NewLiteral,Modal,NewTemporal),NewERulePaths, ERules,AllNewRules).


run_tempres_or_modal(ExtraLiteral,NewLiteral,NewModal,_,NewTemporal,(Initial,Literal,Modal,_Temporal),
                                   ERulePaths, ERules,AllNewRules):-
   my_write('\nrun_tempres_or_modal ==> (2)\n'),
   mres(Literal,Modal,ExtraLiteral,NewModal,Initial,FinalLiteral,FinalModal),
   get_new_literal_rules(Literal,NewLiteral,ExtraLiteral1),
%   my_write('\nLiteral'),
%   write_ruleset(Literal),
%   my_write('\nNewLiteral'),
%   write_ruleset(NewLiteral),
%   my_write('\nExtraLiteral1'),
%   write_ruleset(ExtraLiteral1),
   run_tempres_or_temporal(ExtraLiteral1,FinalLiteral,(Initial,Literal,FinalModal,NewTemporal),ERulePaths, ERules,AllNewRules).

/**********************************************************************/

run_tempres_or_temporal(_,[r(_,_,false)],_,_,_,_):-
   otres_test_new_rules(_,r(_,_,false),_,_,_,_).

run_tempres_or_temporal([],NewLiteral,(Initial,_Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules):-
   my_write('\nrun_tempres_or_temporal ==> (1)\n'),
   my_write('Attempting Breadth-First Temporal Resolution \n'),
   append(Temporal,NewLiteral,AllTemporal),
   full_tempres(AllTemporal,ERulePaths, ERules,MoreRules,NewERulePaths,_RulesUsed),
   otres_test_new_rules(AllTemporal,MoreRules,(Initial,NewLiteral,Modal,Temporal),NewERulePaths, ERules,AllNewRules).

run_tempres_or_temporal(ExtraLiteral,NewLiteral,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules):-
   my_write('\nrun_tempres_or_temporal ==> (2)\n'),
%   k_to_literal(Modal,Kliteral),
   write('\nrun_tempres_or_temporal ==> Usable rules are \n'),
   write_otter_rules(Literal,Temporal),nl,
   write('\nrun_tempres_or_temporal ==> Rules in the SoS are \n'),
   write_otter_rules(ExtraLiteral),nl,
   read(OtterRules),
   otres_test_otter_output(OtterRules,(Initial,NewLiteral,Modal,Temporal),ERulePaths, ERules,AllNewRules).

/*************************************************************************************/

otres_test_new_rules(_,r(N1,N2,false),_,_,_,r(N1,N2,false)):-
   write('Formula is unsatisfiable (1).\n').

otres_test_new_rules(_,[],_,_,_,[]):-
   write('Formula is satisfiable (2).\n').    % Note this really is satisfiable        
                     
otres_test_new_rules(OldRules,NewRules,(Initial,_,Modal,_),ERulePaths, ERules,NewAllRules):-
   split_temp_lit_rules(OldRules,Literal,Temporal),
   otres_allres((Initial,Literal,Modal,Temporal),NewRules,ERulePaths,ERules,NewAllRules).

/**********************************************************************/
/* rewrite_rules(OtterRules,OrdinaryRules,ImpFalseRules)              */
/*                                                                    */
/* Goes through the list of rules output by Otter and rewrites        */
/* rules of the form slast A imp false to their correct form and adds */
/* them to ImpFalseRules and after numbering copies the rest to       */
/* OrdinaryRules.                                                     */
/*                                                                    */
/**********************************************************************/

rewrite_rules([],[],[]).

rewrite_rules([false|_],[false],[]):-!.

rewrite_rules([P imp next false|Rules],NewOldRules,[NewRule1|NewRules]):- !,
  snff_negate(P, NegP),
  number_rule(NegP,[],NewRule1),
  rewrite_rules(Rules,NewOldRules,NewRules).

rewrite_rules([Rule|Rest],NewOldRules,NewRest):-
  number_rule(Rule,[],NRule),
  simplify_rule(NRule, SRule),
  test_add_rule(SRule,Rest,NewOldRules,NewRest).

/********************************************************************************************/

test_add_rule([],Rest,NewOldRules,NewRest):- !,
  rewrite_rules(Rest,NewOldRules,NewRest).
  
test_add_rule([Rule],Rest,[Rule|NewOldRules],NewRest):- 
  rewrite_rules(Rest,NewOldRules,NewRest).
  
/*********************************************************************************************/
/* full_tempres(Rules, ERulePaths, ERules, NewRules,NewERulePaths)                           */
/*                                                                                           */
/* Takes the current ruleset (Rules), a list of the eventualities with previous loop paths   */
/* found (ERulePaths), a list of the sometimes rules (ERules) and attempts temporal          */
/* between one of the eventualities and Rules. The path detected (LoopPath) is stored with   */
/* the appropriate eventuality, NewERule in ERulePaths, new resolvents from this loop path   */
/* and the eventuality are generated and stored in NewRules.                                 */
/*********************************************************************************************/

full_tempres(r(N1,N2,false), _, _, r(N1,N2,false), _,_).

full_tempres(_,[],ERules,NewRules,[],_):-
   write('\nfull_tempres (1) ==> No previous loop paths.\n'),
   process_path([],_,_,[],_,ERules,NewRules, _).

full_tempres(Rules,ERulePaths,ERules,NewRules,NewERulePaths,RulesUsed):-
   write('\nfull_tempres (2) ==> Previous loop paths.\n'),
   tempres(Rules,ERulePaths,NewERule,RelatedRules,RulesUsed,LoopPath),
   process_path(LoopPath,RelatedRules,RulesUsed,ERulePaths,NewERule,ERules,NewRules,NewERulePaths).

/*********************************************************************************************/
/*  process_path(LoopPath,RelatedRules,RulesUsed,ERulePaths,ERule,ERules, NewRules,          */
/*                                                           RotatedERulePaths).             */
/*                                                                                           */
/* Takes a loop found through a set of rules (LoopPath), a list of the rules which imply the */
/* current eventuality (RelatedRules), a list of the rule numbers which have been use in the */
/* loop we have found (RulesUsed), the list which stores each                                */
/* eventuality and the list of loops already found (ERulePaths), an eventuality (ERule) which*/
/* relates to this loop and a list of the sometimes rules (ERules). The new loop for this    */
/* ERule is addeded to ERulePaths. The ERulePaths list is rotated so the head is the         */
/* eventuality which followed ERule ie the new loop and ERule will now appear at the end of  */
/* the list. This is done in an attempt to find "easy" rules first. If the ERulePaths list   */
/* is cycled round then then one loop is found for each eventuality rule (if one exists) and */
/* so on rather than trying to find all the loops for each eventuality in turn. The new      */
/* resolvents are generated from this loop path to give NewRules and the updated and rotated */
/* ERulePathsare returned in RotatedERulePaths.                                              */
/*********************************************************************************************/

process_path([],_,_,_,_ERule,_, [], _):-
   my_write('No more loop paths.\n').

process_path(LoopPath,RelatedRules,RulesUsed,ERulePaths,ERule,ERules, NewRules, RotatedERulePaths):-
   write('The eventuality (for possibly several rules) being considered is '),
   write_form(ERule),nl,
   write('New Loop detected is '),
   write(LoopPath),nl,
   write('Loops previously detected are \n'), write('    [\n'),
   write_list(ERulePaths),
   write('    ]\n'),
   add_new_path_to_found_previously(ERule,LoopPath,ERulePaths,NodePath,NewERulePaths),
   rotate_rule(ERule,NewERulePaths,RotatedERulePaths),
   new_rules(ERule,LoopPath,ERules,RulesUsed,RelatedRules,NodePath,NewRules),
   write_ruleset(NewRules).

/*********************************************************************************************/
/*  rotate_rule(ERule,ERulePaths,NewList)                                                    */
/*                                                                                           */
/* Take an eventuality ERule, and a list of eventualities paired with previous loop paths    */
/* found (ERulePaths). It searches for ERule in ERulePaths and makes the eventuality         */
/* following ERule in the list the head and the portion of the list from the front to where  */
/* ERule was located to the end of the new list (NewList). This was an attempt to find all   */
/* "easy" loops first. Rotating the eventuality rules in this manner means that the temporal */
/* resolution process will try to find a loop for each eventuality (if one exists) and then  */
/* a second, and third etc.                                                                  */
/*********************************************************************************************/

rotate_rule(ERule,ERulePaths,NewList):-
     split_list(ERule,ERulePaths,FirstBit, LastBit),
     append(LastBit,FirstBit,NewList).

/*********************************************************************************************/
/*   split_list(ERule,ERulePaths,FirstBit, LastBit)                                          */
/*                                                                                           */
/* Takes an eventuality ERule, and a list of eventualities paired with loop paths previously */
/* found (ERulePaths). It searches down ERulePaths looking for an ERule in the list. It      */
/* stores all the things before this and including this as FirstBit, and all the remaining   */
/* stuff as LastBit. The aim is the rotate the eventualities and lists in ERulePaths to try  */
/* attempt to find a loop for each eventuality in turn.                                      */
/*********************************************************************************************/

split_list(ERule,[[ERule,PrevPath]|Rest],[[ERule,PrevPath]], Rest).

split_list(ERule,[[AnERule,PrevPath]|Rest],[[AnERule,PrevPath]|FirstBit], LastBit):-
   split_list(ERule,Rest,FirstBit,LastBit).

/*********************************************************************************************/
/*  new_rules(ERule, CycleRules, ERules, RulesUsed,RelatedRules,NodePath, NewRules)          */
/*                                                                                           */
/*  Takes the eventuality (ERule) we have detected a loop for, and a list of rules           */
/*  (CycleRules) which have been generated from the the loop detected, and the complete      */
/*  list of eventuality rules (ERules) and returns a list of new resolvents (NewRules).      */
/*                                                                                           */
/*********************************************************************************************/

new_rules(ERule, CycleRules, ERules, _RuleNoUsed,RelatedRules,NodePath,NewRules):-
  get_rel_rules(ERule, ERules, RelRules),
  generate_new_rules(RelRules, CycleRules, _RuleNoUsed,RelatedRules,NodePath,NewRules).

/*********************************************************************************************/
/* add_new_path_to_found_previously(ERule,NewPath,ERulePaths,NodePath,NewERulePaths)         */
/*                                                                                           */
/* The new loop path NewPath found for the current eventuality ERule is added                */
/* to the list ERulePaths which contains a list of eventualities and the                     */
/* previously discovered.                                                                    */
/*                                                                                           */
/*********************************************************************************************/

add_new_path_to_found_previously(ERule,NewPath,[[ERule, OldPaths]|Rest],_NodePath,[[ERule,[NewPath|OldPaths]]|Rest]).

add_new_path_to_found_previously(ERule,NewPath,[ERulePath|Rest],NodePath,[ERulePath|NewRest]):-
   add_new_path_to_found_previously(ERule,NewPath,Rest,NodePath,NewRest).

/*********************************************************************************************/
/* get_rel_rules(Eventuality, ERules, RelRules)                                              */
/*                                                                                           */
/* Takes the eventuality (Eventuality) for which a loop has been found, and the list of      */
/* rules with eventualities (ERules) and picks out the rules related to the current          */
/* eventuality.                                                                              */
/*********************************************************************************************/

get_rel_rules(_, [], []).

get_rel_rules(F, [r(N1,N2,P imp sometime F)|Rest], [r(N1,N2,P imp sometime F)|NewRest]):-
    get_rel_rules(F, Rest, NewRest).

get_rel_rules(F, [_|Rest], NewRest):-
    get_rel_rules(F, Rest, NewRest).

/*********************************************************************************************/
/* generate_new_rules(RelRules, CycleRules, RuleNoUsed,RelatedRules,NodePath,NewRules)       */
/*                                                                                           */
/* Takes a list of eventuality rules (RelRules) which are related to the eventuality in      */
/* question, and a list of rules (CycleRules) which have generated from the loop detected    */
/* from the eventuality in question, a list of the numbers of the rules used in the loop     */
/* detected (RuleNoUsed), a list of rules of the form slast R imp p, where "p" is the        */
/* eventuality being currently considered (RelatedRules), a list of the nodes used in the    */
/* loop ( and unstructured list) (NodePath) and produces a list of new resolvents.           */
/*********************************************************************************************/

generate_new_rules([],_,_,_,_,[]).

generate_new_rules([RelRule|Rest],CycleRules, _RuleNoUsed,RelatedRules,NodePath,AllNewRules):-
   generate_new_rule(RelRule, CycleRules, _AllRulesUsed,SomeNewRules),
   generate_new_rules(Rest, CycleRules, _AllRulesUsed,RelatedRules, NodePath,OtherNewRules),
   append(SomeNewRules, OtherNewRules, AllNewRules).

/*********************************************************************************************/
/*   generate_new_rule(RelRule, CycleRules, RuleNoUsed, SomeNewRules)                        */
/*                                                                                           */
/* Takes an eventuality rule (RelRule), a set of rules we have found a loop in               */
/* (CycleRules), and a list of the rule numbers used in the loop detected (RuleNoUsed) and   */
/* generates the new resolvents, (SomeNewRules).                                             */
/*********************************************************************************************/

generate_new_rule(r(N1,_, A imp sometime F), [[true]],_RuleNoUsed,[r(0,[N1],NegA or F)]):-
      snff_negate(A,NegA).

generate_new_rule(r(N1,_,true imp sometime F), [[NegF]],_RuleNoUsed,[r(0,[N1],F)]):-
      disjunction_of_literals(F),
      snff_negate(F, NegF).

generate_new_rule(r(_N1,_,P imp sometime F),DisjunctList,_RuleNoUsed,NewRules):-
      neg_cycle_list(DisjunctList,NewList),
      construct_new_rules(P,NewList,F,NewRules).

/***************************************************************************************/

construct_new_rules(P, List, Eventuality,[V imp next (V or Eventuality), NegP or V or Eventuality |Rest]):-
      snff_negate(P, NegP),
      new_temp_prop(V),
      build_rules(NegP,Eventuality,V,List,Rest). 

/********************************************************************************************/

build_rules(NegP,Eventuality,V,[H|Tail],[NegP or Eventuality or H, V imp next (H or Eventuality)|Rest]):-
      build_rules(NegP,Eventuality,V,Tail,Rest).

build_rules(_NegP,_Eventuality,_V,[],[]).

/*********************************************************************************************/
/*       neg_cycle_list(List,NewList).                                                       */
/*                                                                                           */
/* Takes a list (List) which is made up of sublists where the sublists are conjuncts and the */
/* main list is disjunctive. This predicate negates each item in the sublists and disjoins   */
/* them. The sublists are then conjoined together and returned in a NewList.                 */
/*********************************************************************************************/

neg_cycle_list([[P]],[NegP]):-
     snff_negate(P,NegP).

neg_cycle_list([List],[NegP]):-
     snff_negate_list(List,NegP).

neg_cycle_list([List|Lists], [NegP| Rest]):-
     snff_negate_list(List,NegP),
     neg_cycle_list(Lists,Rest).

/**************************************************************************************/

snff_negate_list([H],NegH):-
   snff_negate(H,NegH).

snff_negate_list([H|Rest],NegH or NewRest):-
   snff_negate(H,NegH),
   snff_negate_list(Rest,NewRest).

/**************************************************************************************/

split_temp_lit_rules([r(N,M,A imp next B)|Rest],Literal,[r(N,M,A imp next B)|Temporal]):-
   split_temp_lit_rules(Rest,Literal,Temporal).

split_temp_lit_rules([r(N,M,B)|Rest],[r(N,M,B)|Literal],Temporal):-
   disjunction_of_literals(B),
   split_temp_lit_rules(Rest,Literal,Temporal).

split_temp_lit_rules([A imp next B|Rest],Literal,[A imp next B|Temporal]):-
   split_temp_lit_rules(Rest,Literal,Temporal).

split_temp_lit_rules([B|Rest],[B|Literal],Temporal):-
   disjunction_of_literals(B),
   split_temp_lit_rules(Rest,Literal,Temporal).

split_temp_lit_rules([],[],[]).

/*******************************************************************************/

get_new_modal_rules(Modal,[r(N,_,_)|NewModal],Other):-
   rule_member(N,Modal),!,
   get_new_modal_rules(Modal,NewModal,Other).

get_new_modal_rules(Modal,[r(_,_,Rule)|NewModal],Other):-
   test_member(Rule,Modal),
   get_new_modal_rules(Modal,NewModal,Other).
   
get_new_modal_rules(Modal,[r(N,M,Rule)|NewModal],[r(N,M,Rule)|Other]):-
   get_new_modal_rules(Modal,NewModal,Other).

get_new_modal_rules(_,[],[]).

/******************************************************************************/

get_new_literal_rules(Literal,[r(N,_,_)|NewLiteral],Other):-
   rule_member(N,Literal),!,
   get_new_literal_rules(Literal,NewLiteral,Other).

get_new_literal_rules(Literal,[r(N,M,Rule)|NewLiteral],[r(N,M,Rule)|Other]):-
   get_new_literal_rules(Literal,NewLiteral,Other).

get_new_literal_rules(_,[],[]).

/********************************************************************************/

get_new_literal_rules_after_temporal(Literal,[r(_,_,Rule)|NewLiteral],Other):-
   rule_in_list(Rule,Literal),!,
   get_new_literal_rules_after_temporal(Literal,NewLiteral,Other).

get_new_literal_rules_after_temporal(Literal,[r(N,M,Rule)|NewLiteral],[r(N,M,Rule)|Other]):-
   get_new_literal_rules_after_temporal(Literal,NewLiteral,Other).

get_new_literal_rules_after_temporal(_,[],[]).

/**************************************************/

rule_in_list(Rule,[r(_,_,Rule)|_]):- !.

rule_in_list(D,[r(_,_,E)|_]):-
    strip(D,SD),
    strip(E,SE),
    same(SD,SE),!.

rule_in_list(D,[_|Rest]):-!,
    rule_in_list(D,Rest).

/*******************************************/

rule_member(N,[r(N,_,_)|_]):- !.

rule_member(N,[_|Rest]):- 
   rule_member(N,Rest).

rule_member(_,[]):-!,fail.

/*****************************************/

test_member(X,[X|_]).

test_member(X,[r(_,_,X)|_]).

test_member(X,[_|Rest]):-
    test_member(X,Rest).

/**********************************************************/
/* Gives the clauses coming from Otter their old numbers. */
/* Previously, clauses were just renumbered, making the   */
/* reading of the proof difficult.                        */
/**********************************************************/

give_rules_old_numbers([],_,_,[]).

give_rules_old_numbers([r(N1,N2,X imp next Y)|Tail],Literal,Temporal,[NRule|NewishOldRules]):-
  find_same_rule(r(N1,N2,X imp next Y),Temporal,NRule),
  give_rules_old_numbers(Tail,Literal,Temporal,NewishOldRules).

give_rules_old_numbers([r(N1,N2,X)|Tail],Literal,Temporal,[NRule|NewishOldRules]):-
  find_same_rule(r(N1,N2,X),Literal,NRule),
  give_rules_old_numbers(Tail,Literal,Temporal,NewishOldRules).

find_same_rule(r(N1,N2,Clause),List,r(N1,N2,Clause)):-
   not(member(r(_,_,Clause),List)).

find_same_rule(r(N1,N2,Clause),List,r(N5,N6,Clause)):-
   sublist(same_rule(r(N1,N2,Clause)),List,SameList),
   sort(SameList,Sorted),
   nth1(1,Sorted,r(N3,N4,Clause)),
   test_number(N1,N2,N3,N4,N5,N6).

same_rule(r(_,_,X),r(_,_,Y)):-
   strip_or(X,SX),
   strip_or(Y,SY),
   flatten(SX,FX),
   flatten(SY,FY),
   subset(FX,FY),
   subset(FY,FX).

test_number(N1,N2,N3,_N4,N1,N2):- N1 < N3.
test_number(_N1,_N2,N3,N4,N3,N4).

% ?-[definitions,output,rules,mysnff,mres,mytempres,fullres].
ex354:-otres([start imp x,
              x imp sometime p,
              neg x or neg p,
              x imp next y,
              neg y or neg p,
              neg y or t,
              t imp next neg p,
              t imp next t]).

ex5211:-otres([start imp x,
               x imp k y,
               y imp sometime p,
               x imp k neg p,
               x imp neg k neg z,
               z imp next w,
               neg w or neg p,
               neg w or t,
               t imp next neg p,
               t imp next t]).

ex5212:-otres([start imp x,
               x imp sometime y,
               y imp k p,
               x imp k neg p,
               x imp next z,
               neg z or w,
               neg z or t,
               t imp next w,
               t imp next t,
               w imp neg k p]).

ex621:-otres([start imp x,
              x imp k y,
              y imp next p,
              x imp next z,
              z imp neg k p]).

ex7121:-otres([start imp x,
               x imp next y,
               y imp k p,
               x imp neg k neg z,
               z imp next neg p]).

ex7122:-otres([start imp x,
               x imp next y,
               y imp k p,
               x imp neg k neg v,
               neg v or t,
               neg v or u,
               t imp next (w or neg p),
               u imp next (neg w or neg p)]).

ex7123:-otres([start imp x,
               neg x or z or y,
               neg x or z or w,
               w imp next (z or y),
               w imp next (z or w),
               x imp sometime z,
               z imp k p2,
               y imp k p1,
               x imp neg k neg r,
               neg r or t or s,
               neg r or t or u,
               u imp next (t or s),
               u imp next (t or u),
               t imp neg k p1,
               t imp neg k p2,
               s imp neg k p2]).

ex81:-otres([neg(k always p imp always k p)]).
ex82:-otres([neg(always k p imp k always p)]).
ex83:-otres([(k sometime p and k neg p) imp k next sometime p]).

ex84:-otres([start imp x,
             neg x or y,
             neg x or a,
             neg x or b,
             a and b imp next y,
             a imp next a,
             b imp next b,
             y imp k p,
             x imp neg k neg w,
             w imp sometime neg p]).

ex85:-otres([start imp x,
             neg x or y,
             neg x or a,
             neg x or b,
             a imp next y,
             b imp next y,
             a imp next a,
             b imp next b,
             y imp k p,
             x imp neg k neg w,
             w imp sometime neg p]).


ex86:-otres([start imp x,
             neg x or y,
             neg x or a,
             neg x or b,
             a imp next (c or y),
             b imp next (neg c or y),
             a imp next a,
             b imp next b,
             y imp k p,
             x imp neg k neg w,
             w imp sometime neg p]).

ex87:-otres([start imp y,
             neg y or a,
             neg y or b,
             neg y or l,
             x imp next l,
             a imp next l,
             b imp next (c or d),
             c imp next a,
             d imp next a,
             a imp next x,
             x imp next b,
             y imp sometime neg l]).


