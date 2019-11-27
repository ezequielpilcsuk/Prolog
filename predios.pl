:- use_module(library(clpfd)).

% Ideia de solução é:
%     Garantir que todos diferentes em uma linha
%     Garantir que soma de cada linha seja igual ao necessário
%     Pensando em usar outra estrutura de dados,
%     passar uma matrix do jogo e duas listas de tuplas referentes aos prédios a serem vistos

% Descobri que ele completa o jogo quando ele só encontra uma forma de preencher corretamente.
% necessário, de alguma forma, caso encontre false, testar outro possivel m

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

solution(2,Mx,Lr,Ud) :-
    Mx = [[_,1,5,4,_],
          [2,5,4,3,1],
          [5,3,2,1,4],
          [4,2,1,5,3],
          [_,4,3,2,_]],
    Lr = [[0,3],
          [0,4],
          [1,2],
          [0,0],
          [3,0]],
    Ud = [[2,0],
          [0,0],
          [0,3],
          [0,0],
          [3,0]].

problem(1,Mx,Lr,Ud) :-
    Mx = [[5,3,1,4,2],
          [_,_,_,_,_],
          [3,4,5,2,1],
          [_,_,2,1,3],
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

problem(2,Mx,Lr,Ud) :-
    Mx = [[_,_,_,_,_],
          [_,_,_,_,_],
          [_,_,_,_,_],
          [_,_,_,_,_],
          [_,_,_,_,_]],
    Lr = [[0,3],
          [0,4],
          [1,2],
          [0,0],
          [3,0]],
    Ud = [[2,0],
          [0,0],
          [0,3],
          [0,0],
          [3,0]].

        

        
linhaTeste(1,[3,2,4,5,1], [0,0]).

        % chamada para printar solução, funciona se não é necessário chamar 
        % solution(1, M,X,Y),
        % wolkenkratzer(M,X,Y),
        % maplist(labeling([leftmost]), M),
        % maplist(portray_clause, M).
        

wolkenkratzer(Matrix,Lr,Ud) :- 
    puzzleSize(X),maxValue(Y),S is Y-X+1,
    length(Matrix,X),maplist(same_length(Matrix), Matrix),
    append(Matrix,Vs), Vs ins S..X,
    checkLines(Matrix,Lr),
    transpose(Matrix,MatrixT),
    checkLines(MatrixT,Ud).
    % jogo original não preve restrições às diagonais
    % diagonales(Matrix,[D1,D2]),
    % all_distinct(D1),
    % all_distinct(D2).

nPredios(L,R) :- nPredios(L,R,0).
nPredios([],0,_).
nPredios([H|T],A,M) :- 
    H #> M,
    nPredios(T,A1,H),
    A #= A1+1,!.
nPredios([_|T],A,M) :- nPredios(T,A,M).

checkLine(L,[0,0]):- all_distinct(L),!.
checkLine(L,[C1,0]) :-
    all_distinct(L),
    nPredios(L,C1),!.
checkLine(L,[0,C2]) :-
    all_distinct(L),
    reverse(L,Lr),
    nPredios(Lr,C2),!.
checkLine(L,[C1,C2]) :- 
    all_distinct(L),
    nPredios(L,C1),
    reverse(L,Lr),
    nPredios(Lr,C2),!.

checkLines([],[]).
checkLines([Row|T1],[Cond|T2]) :-checkLine(Row,Cond), checkLines(T1,T2).

diagonales(Matrix, [L1, L2]) :-
    length(Matrix, N),
    findall(E, (between(1,N,I), nth1(I, Matrix, Row), nth1(I, Row, E)), L1),
    findall(E, (between(1,N,I), J is N+1-I, nth1(I, Matrix, Row), nth1(J,Row,E)),L2).