
set_ec_option(N,V):- retractall(etmp:ec_option(N,_)),asserta(etmp:ec_option(N,V)).


