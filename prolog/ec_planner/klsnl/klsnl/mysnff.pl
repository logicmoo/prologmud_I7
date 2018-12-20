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


