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
        
linhaTeste(1,[3,2,4,5,1], [0,0]).

        % chamada para printar solução
        % problem(666, Rows),
        % sudoku(Rows),
        % maplist(labeling([ff]), Rows),
        % maplist(portray_clause, Rows).
        % problem(0,X),sudoku(X),maplist(labeling([ff]),X),maplist(portray_clause,X).

wolkenkratzer(Matrix,Lr,Ud) :- 
    puzzleSize(X),maxValue(Y),S is Y-X+1,
    length(Matrix,X),maplist(same_length(Matrix), Matrix),
    append(Matrix,Vs), Vs ins S..X,
    checkLines(Matrix,Lr),
    transpose(Matrix,MatrixT),
    checkLines(MatrixT,Ud).

nPredios([],0,_).
nPredios([H|T],A,M) :- 
    H #> M,
    nPredios(T,A1,H),
    A #= A1+1,!.
nPredios([_|T],A,M) :- nPredios(T,A,M).

checkLine(L,[0,0]):- all_distinct(L),!.
checkLine(L,[C1,0]) :-
    all_distinct(L),
    nPredios(L,C1,0),!.
checkLine(L,[0,C2]) :-
    all_distinct(L),
    reverse(L,Lr),
    nPredios(Lr,C2,0),!.
checkLine(L,[C1,C2]) :- 
    all_distinct(L),
    nPredios(L,C1,0),
    reverse(L,Lr),
    nPredios(Lr,C2,0),!.

% solution(1,Mx,Lr,Ud),checkLines(Mx,Lr),transpose(Mx,Mxt),checkLines(Mxt,Ud).
checkLines([],[]).
checkLines([Row|T1],[Cond|T2]) :-checkLine(Row,Cond), checkLines(T1,T2).