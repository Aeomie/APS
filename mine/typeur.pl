% Comment
bt_prog(prog(Cs)) :-
    bt_cmds(G,Cs),
    is_init_env(G).

is_init_env(G) :-
    G = [
        ("true", bool),
        ("false", bool),
        ("Not", fun(bool, bool)),
        ("Eq", fun([int, int], bool)),
        ("lt", fun([int, int], bool)),
        ("add", fun([int, int], int)),
        ("sub", fun([int, int], int)),
        ("mul", fun([int, int], int)),
        ("div", fun([int, int], int))
    ].

bt_cmds(G,X):-
    bt_defs(G,X);
    bt_stat(G,X).

bt_addtoE(G, X, T,[(X,T) | G]).


% to check arguments in function calls / recursive calls
bt_check_args(G,[(X,T) | Rest]) :-
   member((X,T),G),
   bt_check_args(G,Rest).

bt_addArgs(G, [(X,T) | Rest]):-
    [(X,T) | G],
    bt_check_args(G,Rest);

bt_check_args(G,[]). % to end recursion


bt_defs(G,const(X,T,E),[(X,T) | G]):-
    member((E,T), G).
bt_defs(G,fun(X,T,Args,e), [ (X, fun(Args,T)) | G ]) :-
    bt_check_args(G,Args),
    bt_expr(G,e,T). % type T to expression
bt_defs(G,funRec(X,T,Args,e), [  (X, fun(Args,T)) | G ]) :-
    bt_check_args(G,Args),
    bt_expr( [ X, fun(Args,T) | G ],e,T).

bt_stat(G, echo(E)) :- bt_expr(G, E, int).

% , is And
% ; is Or
bt_expr(G,num(X)):-
    member((X,int),G).

bt_expr(G,id(X),T) :-
    member((X,T),G).

bt_expr(G,if(E1,E2,E3),T):-
    bt_expr(G,E1,bool),
    bt_expr(G,E2,T),
    bt_expr(G,E3,T).

bt_expr(G,and(E1,E2), bool) :-
    bt_expr(G,E1,bool),
    bt_expr(G,E2,bool).

bt_expr(G,or(E1,E2),bool):-
    bt_expr(G,E1,bool),
    bt_expr(G,E2,bool).


bt_compareArgs(G, [(X,T) | Rest], [(X2,T2) | Rest2]):-
    member((X,T),G),
    member((X2,T),G).

bt_expr(G, app(E,Args),T):-
    bt_expr(G, E, fun(Args2,T)),
    bt_compareArgs(Args2,Args).

bt_expr(G,lambda(Args,X),T):-
       bt_addArgs(G,Args),
       bt_expr(G,X,T).



%%:-
%%read(P),
%%bt_prog(P).
%%
