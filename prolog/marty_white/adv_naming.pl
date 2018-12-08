

:- export(i_name_lc/2).

%= 	 	 

%% i_name_lc( ?OType, ?IType) is semidet.
%
% Instance Name Not Loop Checked.
%
i_name_lc(OType,IType):-typename_to_iname0('',OType,IOType),!,string_equal_ci(IOType,IType).



%= 	 	 

%% to_iname( ?T, ?T) is semidet.
%
% Converted To Iname.
%
to_iname(T,TT):- var(T),!,freeze(T,to_iname(T,TT)).
to_iname(T,TT):- not(current_predicate(i_name/3)),!,T=TT.
%to_iname(T,TT):- (not_log_op(T),i_name(t,T,TT))->true;TT=T.



%= 	 	 

%% toUpperCamelcase( ?Type, ?TypeUC) is semidet.
%
% Converted To Upper Camelcase.
%
toUpperCamelcase(Type,TypeUC):-toCamelcase(Type,TypeUC). % ,toPropercase(TypeC,TypeUC),!.
:- export(i_name/2).


icn_tcn(I,IC):-atom(I),i_name('t',I,IC)->I\==IC.

%= 	 	 

%% i_name( ?OType, ?IType) is semidet.
%
% Instance Name.
%
i_name(OType,IType):-typename_to_iname0('',OType,IOType),!,IOType=IType.
:- export(i_name/3).

%= 	 	 

%% i_name( ?I, ?OType, ?IType) is semidet.
%
% Instance Name.
%
i_name(I,OType,IType):-typename_to_iname0(I,OType,IOType),!,IOType=IType.

:- export(typename_to_iname0/3).


%= 	 	 

%% typename_to_iname0( ?I, ?OType, ?IType) is semidet.
%
% Typename Converted To Iname Primary Helper.
%
typename_to_iname0(I, [], O):- trace_or_throw(bad_typename_to_iname0(I, [], O)).
%typename_to_iname0(I,OType,IType):- fail, (type_prefix(Prefix,_)),atom_concat(Prefix,Type,OType),capitalized(Type),!,typename_to_iname0(I,Type,IType).
typename_to_iname0(I,Type,IType):-nonvar(Type),toUpperCamelcase(Type,UType),atom_concat(I,UType,IType).

:- export(split_name_type/3).
:- '$hide'(split_name_type/3).

%= 	 	 

%% split_name_type( ?Suggest, ?InstName, ?Type) is semidet.
%
% Split Name Type.
%
split_name_type(Suggest,InstName,Type):- maybe_notrace(split_name_type_0(Suggest,NewInstName,NewType)),!,must((NewInstName=InstName,NewType=Type)),!.


%= 	 	 

%% split_name_type_0( ?S, ?P, ?C) is semidet.
%
% split name type  Primary Helper.
%
split_name_type_0(S,P,C):- string(S),!,atom_string(A,S),split_name_type_0(A,P,C),!.
%split_name_type_0(FT,FT,ttExpressionType):-a(ttExpressionType,FT),!,dmsg(trace_or_throw(ttExpressionType(FT))),fail.
split_name_type_0(T,T,C):- compound(T),functor(T,C,_),!.
split_name_type_0(T,T,C):- quietly((once(atomic_list_concat_safe([CO,'-'|_],T)),atom_string(C,CO))).
split_name_type_0(T,T,C):- quietly((atom(T),atom_codes(T,AC),last(AC,LC),is_digit(LC),append(Type,Digits,AC),
  catch(number_codes(_,Digits),_,fail),atom_codes(CC,Type),!,i_name(t,CC,C))).
split_name_type_0(C,P,C):- var(P),atom(C),i_name(i,C,I),gensym(I,P),!.





%= 	 	 

%% toCamelAtom0( :TermA, ?O) is semidet.
%
% Converted To Camel Atom Primary Helper.
%
toCamelAtom0([A],O):-nonvar(A),!,toPropercase(A,O),!.
toCamelAtom0([A|List],O):-!,toPropercase(A,AO),toCamelAtom0(List,LO),atom_concat(AO,LO,O).
toCamelAtom0(A,O):-toPropercase(A,O),!.



%= 	 	 

%% to_prefixed( ?Prefix, ?I, ?O) is semidet.
%
% Converted To Prefixed.
%
to_prefixed(Prefix,I,O):-to_atomic_name(I,i_name(Prefix),O).

:- meta_predicate to_atomic_name(?,2,?).

%= 	 	 

%% to_atomic_name( ?I, :PRED2Pred, ?O) is semidet.
%
% Converted To Atomic Name.
%
to_atomic_name(I,Pred,O):-is_list(I),toCamelAtom0(I,A),!,to_atomic_name(A,Pred,O).
to_atomic_name(I,Pred,O):-string(I),!,string_to_atom(I,A),!,to_atomic_name(A,Pred,O).
%to_atomic_name(Name,Pred,O):-atomic(Name),ereq(mudKeyword(W,KW)),string_equal_ci(Name,KW),!,to_atomic_name(W,Pred,O).
to_atomic_name(Name,Pred,_):- not(atom(Name)),!,trace_or_throw(todo(not_atom_to_atomic_name(Name,Pred))).
to_atomic_name(Name,Pred,O):- call(Pred,Name,O).


guess_textname(Name,Text):- guess_textname(Name,'',Text).
guess_textname(Name,Sep,Text):-atomic(Name),to_case_breaks(Name,ListN),to_case_breaks_trimed(Name,ListN,Sep,Text),!.

to_case_breaks_trimed(Name,[xti(TextL,ClassL),xti(TextR,ClassR)|ListN],Sep,Text):-  ClassL==ClassR,!,
    maplist(to_descriptive_name(Name),[xti(TextL,ClassL),xti(TextR,ClassR)|ListN],Desc),
    (string(Sep) -> atomics_to_string(Desc,Sep,Text) ; atomic_list_concat(Desc,Sep,Text)).

to_case_breaks_trimed(Name,[xti(_,lower),xti(TextR,ClassR)|ListN],Sep,Text):-
    maplist(to_descriptive_name(Name),[xti(TextR,ClassR)|ListN],Desc),
    (string(Sep) -> atomics_to_string(Desc,Sep,Text) ; atomic_list_concat(Desc,Sep,Text)).

to_case_breaks_trimed(Name,ListN,Sep,Text):- is_list(ListN),!,
    maplist(to_descriptive_name(Name),ListN,Desc),
    (string(Sep) -> atomics_to_string(Desc,Sep,Text) ; atomic_list_concat(Desc,Sep,Text)).



%to_descriptive_name(For,Desc,Atom):- type_descriptive_name(Type,Desc,Atom),isa(For,Type),!.
%to_descriptive_name(_For,Pefix,Desc):- (type_prefix(Pefix,TypeName)), guess_textname(TypeName,Desc).
%to_descriptive_name(For,xti(Pefix,lower),Desc):-!,to_descriptive_name(For,Pefix,Desc).
to_descriptive_name(For,xti(Pefix,_),Desc):-!,to_descriptive_name(For,Pefix,Desc).
to_descriptive_name(_For,X,X).

type_prefix(t,'Type').
