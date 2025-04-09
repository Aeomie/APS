% , Is And
% ; is Or
% Prog
bt_prog(prog(Cs)) :-
    %write('In prog : '),write(Cs),nl,
    is_init_env(G),
    bt_block(G,Cs,void).

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
    %write('in cmds def : '),write(X), nl,
    %write('rest : '),write(RestCmds),nl,
    bt_defs(G, X, New_G),
    bt_cmds(New_G, RestCmds, void).

bt_cmds(G, [Cmd | RestCmds], void) :-
    %write('In cmds stat : '), write(Cmd), nl,
    bt_stat(G, Cmd, void),
    bt_cmds(G, RestCmds, void).


/******END CMDS******/
/*-----------DEFS------------*/

%!   DEFS
bt_defs(G,const(X,T,E),New_G):-
    %write('In const : '),write(X),nl,
    bt_expr(G,E,T),
    New_G = [(X,T) | G].


bt_defs(G,fun(FuncName,T,Args,E), New_G) :-
    %write('In func : '),write(FuncName),nl,
    append(Args,G,G_Temp),
    bt_expr(G_Temp,E,T), % type T to expression
    bt_get_types(Args, Result),
    New_G = [(FuncName,fun(Result,T))| G].

bt_defs(G,funRec(FuncName, T, Args, E), New_G) :-
    %write('In funcRec : '),write(FuncName),nl,
    append(Args, G, G_Temp),
    bt_get_types(Args, Result),
    G_Temp2 = [(FuncName, fun(Result, T)) | G_Temp],
    bt_expr(G_Temp2, E, T),
    New_G = [(FuncName, fun(Result, T)) | G].

bt_defs(G,var(Var,T),New_G):-
    write("in Var "),write(Var),write(' type: '),write(T),
    New_G = [(Var,ref(T)) | G],write(New_G).

bt_defs(G, proc(ProcName, Args, Body), New_G) :-
    %write('In Proc : '), write(ProcName), nl,
    append(Args, G, G_Temp),
    bt_get_typesP(Args, Result),
    bt_block(G_Temp, Body, void),
    New_G = [(ProcName, fun(Result,  void)) | G].

bt_defs(G, procRec(ProcName, Args,Body), New_G):-
    %write('In Proc Rec : '), write(ProcName), nl,
    append(Args,G,G_Temp),
    bt_get_typesP(Args,Result),
    G_Temp2 = [(ProcName, fun(Result, void)) | G_Temp],
    bt_block(G_Temp2, Body, void),
    New_G = [(ProcName, fun(Result,void)) |G].
/*-----------END DEFS------------*/

/*******INSTRUCTIONS*******/

bt_stat(G, echo(E),void) :-
    write('In Echo : '),write(E),nl,
    bt_expr(G, E, int).

bt_stat(G,set(id(Var), Expr),void):-
    bt_expr(G,Expr,T),
    write('In stat '),write(Var),write(T),nl,
    bt_exprp(G,adr(Var),ref(T)).

bt_stat(G,ifblock(Cond,Body,Alt),void):-
    bt_expr(G, Cond , bool),
    bt_block(G, Body, void),
    bt_block(G, Alt, void).

bt_stat(G,while(Cond,Body),void):-
    bt_expr(G, Cond, bool),
    bt_block(G, Body,void).

bt_stat(G,call(IdVar,Args),void):-
    write("in call"),nl,
    bt_expr(G,IdVar,fun(ArgsTypes,void)),
    write('ArgTypes: '),write(ArgsTypes),nl,
    write("Args : ") , write(Args),nl,
    bt_compareArgsP(G,Args,ArgsTypes).



/****** END INSTRUCTIONS********/

/************EXPRESSIONS**************/

%num
bt_expr(_,num(X),int):-
    %write('in Num '),write(X),nl,
    integer(X).

%id
bt_expr(G,id(X),T) :-
    %write('in ID : '), write(X),nl,
    member((X,T),G).

bt_expr(G,id(X),T):-
    % checks if its type ref
    member((X,ref(T)),G).

%if
bt_expr(G,if(E1,E2,E3),T):-
    %write('in If : '),nl,
    %write('cond : '), write(E1),nl,
    %write('body: '), write(E2),nl,
    %write('alternant : '), write(E3),nl,
    bt_expr(G,E1,bool),
    bt_expr(G,E2,T),
    bt_expr(G,E3,T).

%and
bt_expr(G,and(E1,E2), bool) :-
    %write('in And : '),nl,
    %write('Left: '), write(E1),nl,
    %write('Right: '), write(E2),nl,
    bt_expr(G,E1,bool),
    bt_expr(G,E2,bool).

%or
bt_expr(G,or(E1,E2),bool):-
    %write('in Or : '),nl,
    %write('Left: '), write(E1),nl,
    %write('Right: '), write(E2),nl,
    bt_expr(G,E1,bool),
    bt_expr(G,E2,bool).

% Lambda
bt_expr(G,lambda(Args,E),fun(Result,T)):-
    %write('in Lambda '),nl,
    append(Args,G,New_G),
    bt_get_types(Args,Result),
    bt_expr(New_G,E,T).

%app
bt_expr(G, app(E,Args),T):-
    %write('in App '),nl,
    %write('Expression : '),write(E),nl,
    %write('Args : '), write(Args),nl,
    %write('Environment : ' ), write(G),nl,
    %write('Type T : '),write(T),nl,
    bt_expr(G, E, fun(ArgsTypes,T)), %% checks if E is func, since it checks if E is of type fun
    bt_compareArgs(G,Args,ArgsTypes).


/*************END EXPRESSIONS***************/

/*************START PROC EXPR***************/

bt_exprp(G,adr(X),ref(T)):-
    write('In exprp '),write(X),write(' type: '),write(T),
    bt_expr(G,id(X),ref(T)).

bt_exprp(G,X,T) :-
    bt_expr(G,X,T).


/*************END PROC EXPR***************/


/***Block handle***/

bt_block(G,block(Cs),void):-
    %write("in block"),nl,
    bt_cmds(G,Cs,void).

/***End of Block***/
/*******HELPER Functions**********/

%Args
%
%!  Func used to check if Arg type is correct
bt_compareArgs(_,[],[]).
bt_compareArgs(G, [Arg | RestArgs], [Type | RestTypes]):-
    %write('in Compare args'),nl,
    bt_expr(G,Arg,Type),
    bt_compareArgs(G,RestArgs,RestTypes).

%ArgP
bt_compareArgsP(_, [], []).
bt_compareArgsP(G, [Argp | RestArgsp], [Type | RestTypes]):-
    %in compareargsP
    write('Arg: '),write(Argp),nl,
    write('Type: '),write(Type),nl,
    bt_exprp(G,Argp,Type),
    bt_compareArgsP(G,RestArgsp,RestTypes).

% Debug func , just to add to Environment
bt_addtoE(G, X, T,[(X,T) | G]).


% func that gets u the types from ARGS list
bt_get_types([],[]). % to stop the recursive call
bt_get_types([(_,T)|ARGS],[T|Result]):-
    bt_get_types(ARGS,Result).

bt_get_typesP([],[]).

bt_get_typesP([(_,T) | ARGS], [T|Result]):-
    bt_get_typesP(ARGS,Result).

bt_get_typesP([var(_,T) | ARGS], [ref(T)|Result]):-
    bt_get_typesP(ARGS,Result).

/*******END HELPER Functions**********/

:-
    read(Program),
    bt_prog(Program).
