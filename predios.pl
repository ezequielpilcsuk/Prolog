:- use_module(library(clpfd)).

% Ideia de solução é:
%     Garantir que todos diferentes em uma linha
%     Garantir que soma de cada linha seja igual ao necessário
%     Pensando em usar outra estrutura de dados,
%     passar uma matrix do jogo e duas listas de tuplas referentes aos prédios a serem vistos

% game(Matrix,LeftRight,UpDown).
puzzleSize(5).
maxValue(5).

solution(1,Mx,Lr,Ud) :-
    Mx = [[5,3,1,4,2],
          [1,2,3,5,4],
          [3,4,5,2,1],
          [4,5,2,1,3],
          [2,1,4,3,5]],
    Lr = [[1,3],
          [4,2],
          [3,3],
          [2,0],
          [0,1]],
    Ud = [[0,0],
          [3,2],
          [3,2],
          [0,2],
          [0,1]].

problem(1,Mx,Lr,Ud) :-
    Mx = [[_,_,_,_,_],
          [_,_,_,_,_],
          [_,_,_,_,_],
          [_,_,_,_,_],
          [_,_,_,_,_]],
    Lr = [[1,3],
          [4,2],
          [3,3],
          [2,0],
          [0,1]],
    Ud = [[0,0],
          [3,2],
          [3,2],
          [0,2],
          [0,1]].
        
linhaTeste(1,[3,2,4,5,1], [3,2]).

        % chamada para printar solução
        % problem(666, Rows),
        % sudoku(Rows),
        % maplist(labeling([ff]), Rows),
        % maplist(portray_clause, Rows).

wolkenkratzer(Matrix,Lr,Ud) :- 
    puzzleSize(X),
    length(Matrix,X),maplist(same_length(Matrix), Matrix),
    append(Matrix,Vs), Vs ins 1..X,
    maplist(all_distinct,Matrix),
    transpose(Matrix,MatrixT),maplist(all_distinct,MatrixT),
    checkLines(Matrix,Lr),
    checkLines(MatrixT,Ud).



nPredios([],0,_).
nPredios([H|T],A,M) :- 
    H > M,
    nPredios(T,A1,H),
    A is A1+1,!.
nPredios([_|T],A,M) :- nPredios(T,A,M).

checkLine(L,[C1,C2]) :- 
    nPredios(L,C1,0),
    reverse(L,Lr),
    nPredios(Lr,C2,0).

% solution(1,Mx,Lr,Ud),checkLines(Mx,Lr),transpose(Mx,Mxt),checkLines(Mxt,Ud).
checkLines([],[]).
checkLines([Row|T1],[Cond|T2]) :-checkLine(Row,Cond), checkLines(T1,T2).

label(Col, Row, N) :- format("(~w) ~w ~w num\n", [N,Col,Row]).
label(Col, Row, _) :- format("~w ~w clear\n", [Col,Row]), false.

% solution(1,P) :-
%         P = [linha(1,[5,3,1,4,2],3),
%              linha(4,[1,2,3,5,4],2),
%              linha(3,[3,4,5,2,1],3),
%              linha(2,[4,5,2,1,3],0),
%              linha(0,[2,1,4,3,5],1),
%              linha(0,[5,1,3,4,2],0),
%              linha(3,[3,2,4,5,1],2),
%              linha(3,[1,3,5,2,4],2),
%              linha(0,[4,5,2,1,3],2),
%              linha(0,[2,4,1,3,5],1)]. 
% problem(1, P) :-
%         P = [linha(1,[],3),
%              linha(4,[],2),
%              linha(3,[],3),
%              linha(2,[],0),
%              linha(0,[],1),
%              linha(0,[],0),
%              linha(3,[],2),
%              linha(3,[],2),
%              linha(0,[],2),
%              linha(0,[],1)].

% getList(linha(_,L,_),L).

% getLists([],[]).
% getLists([H|T],[L1|L2]) :- getList(H,L1),getLists(T,L2).

% getX(linha(X,_,_),X).

% getY(linha(_,_,Y),Y).