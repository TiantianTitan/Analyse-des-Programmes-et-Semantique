g0(
    [(true,bool),
    (false,bool),
    (not,arrow([bool],bool)),
    (add,arrow([int,int],int)),
    (lt,arrow([int,int],bool)),
    (sub,arrow([int,int],int)),
    (mul,arrow([int,int],int)),
    (div,arrow([int,int],int)),
    (eq,arrow([int,int],bool))
    ]
).

%"******************************************************************************************************************"
% "Verifie si le argument est defini dans le env"

inEnv([(V,T)|_],V,T).
inEnv([(_,_)|E],V,T) :-     inEnv(E,V,T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% "ARG"
checkArgsType([],[]).
checkArgsType([(_,T)|ArgsList],[T|TypeList]) :- checkArgsType(ArgsList,TypeList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% "ListType"
checkListType(_,[],[]).
checkListType(Env, [E|ExprList], [T|TypeList]) :- type_expr(Env, E, T), checkListType(Env, ExprList, TypeList).
%"******************************************************************************************************************"
checkListTypeP(_,[],[]).
checkListTypeP(Env, [E|ExprList], [T|TypeList]) :- type_expar(Env, E, T), checkListTypeP(Env, ExprList, TypeList).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% "Programme"
type_prog(prog(Block),void) :- 
    writeln("Env check"),
    g0(E),
    writeln("Env check fini, block start"),
    type_block(E,Block,void),
    writeln("bolck end").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
% "Block"
type_block(E,block(Cmds),void) :-
    type_cmds(E,Cmds,void).    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% "Cmds"

% "type_cmds(Z,[],void)."
type_cmds(_,[],void). 

% "type_cmds(Z,[echo(num(1))],void)."
type_cmds(Env,[Stat | Cs],void):-
    writeln("Stat Starts"),
    type_stat(Env,Stat,void),
    writeln("Stat Ends"),
    type_cmds(Env,Cs,void).

% "type_cmds(Z,[const(x,int,num(1)),const(y,int,num(2)),echo(false)],void)."
% "type_cmds(Z,[const(x,bool,false),const(y,int,num(2)),echo(false)],void)."
type_cmds(Env,[Def|Cs],void):-
    writeln("Def Starts"),
    writeln(Env),
    type_def(Env,Def,Env_New),
    writeln("Def End, new Env is :"),
    writeln(Env_New),
    writeln("Suit cmds Starts"),
    type_cmds(Env_New,Cs,void),
    writeln("cmds Ends").   
  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% "Def"

type_def(Env,const(Ident,Type,Expr),[(Ident,Type)|Env]) :-
    type_expr(Env,Expr,Type).

% "type_def(Z,fun(rr,bool,[(x,bool)],not(false)),TOTO)."
type_def(Env, fun(FunName,TypeReturn, ArgsL, Expr), [(FunName, arrow(Type, TypeReturn)) | Env]) :- 
    checkArgsType(ArgsL, Type), 
    append(ArgsL, Env, EnvRes), 
    type_expr(EnvRes, Expr, TypeReturn).



% "type_def(Z,funrec(rr,bool,[(y,int)],not(false)),TOTO)."
type_def(Env, funrec(FunName,TypeReturn,ArgsL,Expr), [(FunName, arrow(Type,TypeReturn)) | Env]) :- 
    checkArgsType(ArgsL, Type), 
    append(ArgsL, Env, EnvRes), 
    type_expr([(FunName,arrow(Type,TypeReturn))| EnvRes],Expr,TypeReturn).

type_def(Env,var(X,T),[(X,ref(T))|Env]).

% "Process"
type_def(Env, proc(ProcName, ArgsL, Block), [(ProcName, arrow(Types, void)) | Env]):-
    append(ArgsL, Env, EnvRes),
    checkArgsType(ArgsL, Types),
    type_block(EnvRes, Block, void).

% "Process Rec"
type_def(Env, procrec(ProcName, ArgsL, Block), [(ProcName, arrow(Types, void))|Env]):-
    append(ArgsL, Env, EnvRes),
    checkArgsType(ArgsL, Types),
    type_block([(ProcName, arrow(Types, void))|EnvRes], Block, void).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% "Stat"
type_stat(Env,echo(E),void) :-
    type_expr(Env,E,int).

% " "
type_stat(Env,set(Id, E),void) :-
    inEnv(Env, Id, ref(Type)),
    type_expr(Env, E, Type).

type_stat(Env,set(Id, E),void) :-
    type_expr(Env, ident(Id), Type),
    type_expr(Env, E, Type).

type_stat(Env,set(Id, E),void) :-
    type_expr(Env, Id, Type),
    type_expr(Env, E, Type).


% "IF"
type_stat(Env,ifstat(E, Block1, Block2), void) :-
    writeln("IF Starts"),
    type_expr(Env, E, bool),
    type_block(Env, Block1, void),
    type_block(Env, Block2, void).

% "While"
type_stat(Env, while(E, Block), void) :-
    type_expr(Env, E, bool),
    type_block(Env, Block, void).

% "CALL"
type_stat(Env, call(Name, ExprList), void):-
    writeln("Call Starts"),
    inEnv(Env,Name,arrow(TypeList, void)),
    checkListTypeP(Env, ExprList, TypeList).
    
type_expar(Env,adr(ident(X)),ref(T)) :- inEnv(Env,X,ref(T)).
type_expar(Env,expr(X),T) :- type_expr(Env,X,T), \+(T=ref(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% "Expr"

type_expr(_,true,bool).
type_expr(_,false,bool).

type_expr(_,num(N),int) :- 
    integer(N).

% "type_expr([(x,int)],ident(x),int)."
type_expr(Env,ident(V),Type):-   
    inEnv(Env,V,Type).

type_expr(Env,ident(V),Type):-   
    inEnv(Env,V,ref(Type)).


% echo "prog([echo(if(true,num(1),num(2)))])."| swipl -g main -t halt prog.pl
type_expr(Env,if(Cond,Cons,Alt),Type) :- 
    type_expr(Env,Cond,bool),
    type_expr(Env,Cons,Type),
    type_expr(Env,Alt,Type).

type_expr(Env,not(B),bool):-            type_expr(Env,B,bool).
type_expr(Env,and(B1,B2),bool):-        type_expr(Env,B1,bool),     type_expr(Env,B2,bool).
type_expr(Env,or(B1,B2),bool):-         type_expr(Env,B1,bool),     type_expr(Env,B2,bool).
type_expr(Env,eq(I1,I2),bool):-         type_expr(Env,I1,int),      type_expr(Env,I2,int).
type_expr(Env,app(E,Es),T):-
    type_expr(Env,E,arrow(Ts,T)), 
    checkListType(Env,Es,Ts).
type_expr(Env,abs(ArgsL,E),arrow(Ts,T)):-  checkArgsType(ArgsL,Ts),append(ArgsL,Env,EnvRes),type_expr(EnvRes,E,T).

type_expr(Env,alloc(E), vec(_)) :- type_expr(Env,E,int).
type_expr(Env,len(E), int) :- type_expr(Env,E,vec(_)).
type_expr(Env,nth(E1,E2),T) :- type_expr(Env,E1,vec(T)), type_expr(Env,E2,int), writeln("New Env is:"), writeln(Env).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

% =============== I : input    O : output =================
main :- read(user_input, I), type_prog(I,O), write(O),nl.



