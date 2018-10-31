:- module(clex_iface,[]).

:- ensure_loaded(nl_iface).
:- if(\+ (exists_file('clex_nldata.qlf'))).
:- format(user_error,'~NCreating for the first time clex_nldata.qlf ... ~n',[]).
:- time(load_files(clex_nldata,[qcompile(auto)])).
:- endif.
:- reexport(clex_nldata).
:- reexport(talk_db_iface).


