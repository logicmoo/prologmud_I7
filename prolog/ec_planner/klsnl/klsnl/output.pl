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

