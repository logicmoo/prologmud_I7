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
