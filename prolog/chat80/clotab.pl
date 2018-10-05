/* @(#)clotab.pl	24.1 2/23/88 */


/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
/*
 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or_xmask included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

% Normal form masks

ix_is_pp(#(1,_,_,_)).

ix_is_pred(#(_,1,_,_)).

% unused?
% is_trace(#(_,_,1,_)).

ix_is_adv(#(_,_,_,1)).

ix_trace(#(_,_,1,_),#(0,0,0,0)).

ix_trace(#(0,0,1,0)).

ix_adv(#(0,0,0,1)).

ix_empty(#(0,0,0,0)).

ix_np_all(#(1,1,1,0)).

ix_s_all(#(1,0,1,1)).

ix_np_no_trace(#(1,1,0,0)).

% Mask operations

ix_plus(#(B1,B2,B3,B4),#(C1,C2,C3,C4),#(D1,D2,D3,D4)) :-
   or_xmask(B1,C1,D1),
   or_xmask(B2,C2,D2),
   or_xmask(B3,C3,D3),
   or_xmask(B4,C4,D4).

ix_minus(#(B1,B2,B3,B4),#(C1,C2,C3,C4),#(D1,D2,D3,D4)) :-
   not_xmask(B1,C1,D1),
   not_xmask(B2,C2,D2),
   not_xmask(B3,C3,D3),
   not_xmask(B4,C4,D4).

or_xmask(1,_,1).
or_xmask(0,1,1).
or_xmask(0,0,0).

not_xmask(X,0,X).
not_xmask(_X,1,0).

% Noun phrase position features

ix_role(subj,_,#(1,0,0)).
ix_role(compl,_,#(0,_,_)).
ix_role(undef,main,#(_,0,_)).
ix_role(undef,aux,#(0,_,_)).
ix_role(undef,decl,_).
ix_role(nil,_,_).

subj_case(#(1,0,0)).
verb_case(#(0,1,0)).
prep_case(#(0,0,1)).
compl_case(#(0,_,_)).
