:- use_module(library(clpfd)).
:- set_prolog_flag(cpu_count, 12).

% Ideia de solução é:
%     Garantir que todos diferentes em uma linha
%     Garantir que soma de cada linha seja igual ao necessário
%     Pensando em usar outra estrutura de dados,
%     passar uma matrix do jogo e duas listas de tuplas referentes aos prédios a serem vistos

% Descobri que ele completa o jogo quando ele só encontra uma forma de preencher corretamente.
% necessário, de alguma forma, caso encontre false, testar outro possivel m

% game(Matrix,LeftRightRestrict,UpDownRestrict).
puzzleSize(6).
maxValue(5).

%  Estrutura de dados usada, Mx é referente ao prédios do puzzle
%  Lr é referente ao número de predios a serem vistos, respectivamente, da esquerda e da direita
%  Ud é referente ao número de predios a serem vistos, respectivamente, de cima e de baixo
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
    Mx = [[_,_,_,_,_],
          [_,5,4,3,_],
          [_,3,_,1,_],
          [_,2,1,5,_],
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

% Tem 0
problem(218,Mx,Lr,Ud) :-
      Mx = [[_,_,_,_,_],
            [_,_,_,_,_],
            [_,_,_,_,_],
            [_,_,_,_,_],
            [_,_,_,_,_]],
      Lr = [[0,0],
            [0,4],
            [0,3],
            [0,0],
            [4,0]],
      Ud = [[0,0],
            [2,0],
            [2,0],
            [0,3],
            [0,0]].

% Tem 0st
problem(16,Mx,Lr,Ud) :-
      Mx = [[_,_,_,_,_,_],
            [_,_,_,_,_,_],
            [_,_,_,_,_,_],
            [_,_,_,_,_,_],
            [_,_,_,_,_,_],
            [_,_,_,_,_,_]],
      Lr = [[2,2],
            [3,2],
            [2,2],
            [1,4],
            [4,1],
            [3,2]],
      Ud = [[2,2],
            [3,2],
            [1,2],
            [4,1],
            [2,4],
            [3,2]].

      %  chamada para printar solução
      %   problem(1, M,X,Y),
      %   wolkenkratzer(M,X,Y),
      %   concurrent_maplist(label, M),
      %   concurrent_maplist(portray_clause, M).

%  Solucionador do jogo
wolkenkratzer(Matrix,Lr,Ud) :- 
      %  Seta número máximo e tamanho do puzzle
      puzzleSize(X),maxValue(Y),S is Y-X+1,
      %  Garante que cada linha na matriz tenha o mesmo tamanho do que a matriz
      length(Matrix,X),concurrent_maplist(same_length(Matrix), Matrix),
      %  Garante que todos os valores da matriz estão entre os valores possíveis
      append(Matrix,Vs), Vs ins S..Y,
      %  Garante que todos os números em cada linha sejam diferentes
      concurrent_maplist(all_distinct,Matrix),transpose(Matrix,MatrixT),
      %  Garante que todos os números em cada coluna sejam diferentes
      concurrent_maplist(all_distinct,MatrixT),numlist(S,Y,Usable),
      %  Garante que linhas e colunas respeitem as restrições dos prédios vistos
      checkLines(Usable,Matrix,Lr),checkLines(Usable,MatrixT,Ud).
    % DESCOMENTAR SE NECESSÁRIO DIAGONAIS
    % diagonales(Matrix,[D1,D2]),
    % all_distinct(D1),
    % all_distinct(D2).

%  nPredios(NumerosDisponíveis,PrediosVistos,Lista)
%  Chama a função com variável auxiliar referente ao maior número visto
nPredios(Usable,N,Lista):-nPredios(Usable,0,N,Lista).
%  Caso não haja números disponíveis e não preciso ver mais prédios, retorna.
nPredios([],_,0,[]).
%  Caso preciso ver prédios, pego elemento disponível maior do que o Max
%  e chamo recursivamente a função removendo o elemento escolhido dos disponíveis
%  e diminuindo a quantidade de prédios a serem vistos.
nPredios(Usable,Max,N,[X|Lista]):- 
      N #\= 0, member(X,Usable), 
      delete(Usable,X,NewUsable),
      X #> Max, N #= N1 + 1,
      nPredios(NewUsable,X,N1,Lista).
%  Caso não preciso ver mais prédios ou o valor escolhido seja menor do que o max,
%  chamo a função recursivamente somente removendo o valor escolhido dos disponíveis
nPredios(Usable,Max,N,[X|Lista]):- 
      member(X,Usable), 
      delete(Usable,X,NewUsable),
      X #=< Max, nPredios(NewUsable,Max,N,Lista).

%  Dados valores possíveis e as restrições de uma linha,
%  chama nPredios nas restrições necessárias
checkLine(_,_,[0,0]).
checkLine(Usable,L,[C1,0]) :- C1 \= 0,nPredios(Usable,C1,L).
checkLine(Usable,L,[0,C2]) :- C2 \= 0,reverse(L,Lr), nPredios(Usable,C2,Lr).
checkLine(Usable,L,[C1,C2]):- C1 \= 0, C2 \= 0,nPredios(Usable,C1,L),reverse(L,Lr), nPredios(Usable,C2,Lr).

%  Dado um conjunto de linhas e um conjunto de restrições,
%  checa ou preenche as linhas de acordo com as respectivas restrições
checkLines(_,[],[]).
checkLines(Usable,[Row|T1],[Cond|T2]) :- checkLine(Usable,Row,Cond), checkLines(Usable,T1,T2).

%  Dada uma Matriz, retorna as duas diagonais da matriz
diagonals(Matrix, [L1, L2]) :-
    length(Matrix, N),
    findall(E, (between(1,N,I), nth1(I, Matrix, Row), nth1(I, Row, E)), L1),
    findall(E, (between(1,N,I), J is N+1-I, nth1(I, Matrix, Row), nth1(J,Row,E)),L2).