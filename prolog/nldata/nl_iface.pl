:- module(nl_iface,[]).

:- prolog_load_context(file,File),
   absolute_file_name('.',X,[relative_to(File),file_type(directory)]),
   (user:file_search_path(nldata,X)-> true ; asserta(user:file_search_path(nldata,X))).


:- reexport(tt0_iface).
:- reexport(ac_xnl_iface).
:- reexport(clex_iface).
:- reexport(nldata('../boxer/boxer')).

		 /*******************************
		 *          FIND WORDNET	*
		 *******************************/

set_rel_path_from_here:- 
   prolog_load_context(file,File),
   absolute_file_name('WNprolog-3.0/prolog/',WNDB,[relative_to(File),file_type(directory)]),
   setenv('WNDB', WNDB).

:- getenv('WNDB', _WNDB) -> true ; set_rel_path_from_here.

:- reexport(wn30_iface).
:- load_wordnet.

end_of_file.


