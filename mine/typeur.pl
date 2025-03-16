% , Is And
% ; is Or
% Prog
bt_prog(prog(Cs)) :-
    write('In prog : '),write(Cs),nl,
    is_init_env(G),
    bt_cmds(G,Cs,void).

% Initial Environment
is_init_env(G) :-
    G = [
        (true, bool),
        (false, bool),
        (not, fun([bool], bool)),
        (eq, fun([int, int], bool)),
        (lt, fun([int, int], bool)),
        (add, fun([int, int], int)),
        (sub, fun([int, int], int)),
        (mul, fun([int, int], int)),
        (div, fun([int, int], int))
    ].

/******CMDS******/
%!  defined two bt_cmds , because i had an issue with bt_def being ran when bt_stat should run
bt_cmds(_, [], void).

bt_cmds(G, [dec(X) | RestCmds], void) :-
    write('in cmds def : '),write(X), nl,
    write('rest : '),write(RestCmds),nl,
    bt_defs(G, X, New_G),
    bt_cmds(New_G, RestCmds, void).

bt_cmds(G, [Cmd | RestCmds], void) :-
    write('In cmds stat : '), write(Cmd), nl,
    bt_stat(G, Cmd, void),
    bt_cmds(G, RestCmds, void).


/******END CMDS******/
/*-----------DEFS------------*/

%!   DEFS
bt_defs(G,const(X,T,E),New_G):-
    write('In const : '),write(X),nl,
    bt_expr(G,E,T),
    New_G = [(X,T) | G].


bt_defs(G,fun(FuncName,T,Args,E), New_G) :-
    write('In func : '),write(FuncName),nl,
    append(Args,G,G_Temp),
    bt_expr(G_Temp,E,T), % type T to expression
    bt_get_types(Args, Result),
    New_G = [(FuncName,func(Result,T))| G].

bt_defs(G,funRec(FuncName, T, Args, E), New_G) :-
    write('In funcRec : '),write(FuncName),nl,
    append(Args, G, G_Temp),
    bt_get_types(Args, Result),
    G_Temp2 = [(FuncName, func(Result, T)) | G_Temp],
    bt_expr(G_Temp2, E, T),
    New_G = [(FuncName, func(Result, T)) | G].

/*-----------END DEFS------------*/

/*******INSTRUCTIONS*******/

bt_stat(G, echo(E),void) :-
    write('In stat : '),write(E),nl,
    bt_expr(G, E, int).

/****** END INSTRUCTIONS********/

/************EXPRESSIONS**************/

% num
bt_expr(_,num(X),int):-
    write('in Num '),write(X),nl,
    integer(X).

%id
bt_expr(G,id(X),T) :-
    write('in ID : '), write(X),nl,
    parcours(X,G,T).

%if
bt_expr(G,if(E1,E2,E3),T):-
    write('in If : '),nl,
    write('cond : '), write(E1),nl,
    write('body: '), write(E2),nl,
    write('alternant : '), write(E3),nl,
    bt_expr(G,E1,bool),
    bt_expr(G,E2,T),
    bt_expr(G,E3,T).

%and
bt_expr(G,and(E1,E2), bool) :-
    write('in And : '),nl,
    write('Left: '), write(E1),nl,
    write('Right: '), write(E2),nl,
    bt_expr(G,E1,bool),
    bt_expr(G,E2,bool).

%or
bt_expr(G,or(E1,E2),bool):-
    write('in Or : '),nl,
    write('Left: '), write(E1),nl,
    write('Right: '), write(E2),nl,
    bt_expr(G,E1,bool),
    bt_expr(G,E2,bool).

%app
bt_expr(G, app(E,Args),T):-
    write('in App '),nl,
    bt_expr(G, E, fun(ArgsTypes,T)), %% checks if E is func, since it checks if E is of type fun
    bt_compareArgs(G,Args,ArgsTypes).

% Lambda
bt_expr(G,lambda(Args,E),func(Result,T)):-
    write('in Lambda '),nl,
    append(Args,G,New_G),
    bt_get_types(Args,Result),
    bt_expr(New_G,E,T).

/*************END EXPRESSIONS***************/


/*******HELPER Functions**********/

%!  Func used to check if Arg type is correct
bt_compareArgs(_,[],[]).
bt_compareArgs(G, [Arg | RestArgs], [Type | RestTypes]):-
    write('in Compare args'),nl,
    bt_expr(G,Arg,Type),
    bt_compareArgs(G,RestArgs,RestTypes).

% Debug func , just to add to Environment
bt_addtoE(G, X, T,[(X,T) | G]).


% func that gets u the types from ARGS list
bt_get_types([],[]). % to stop the recursive call
bt_get_types([(_,T)|ARGS],[T|Result]):-
    bt_get_types(ARGS,Result).


parcours(X, [(X,V)|_], V).
parcours(X, [_|XS], V) :- parcours(X, XS, V).

/*******END HELPER Functions**********/

:-
    read(Program),
    bt_prog(Program).
