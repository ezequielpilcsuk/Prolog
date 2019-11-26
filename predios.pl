:- use_module(library(clpfd)).

% Ideia de solução é:
%     Usar 2n linhas para representar linhas e colunas
%     Garantir que todos diferentes em uma linha
%     Garantir que [0..n] linhas == transpose [n..2n]
%     Garantir que soma de cada linha seja igual ao necessário

problem(1, [linha(1,[5,3,1,4,2],3),
            linha(4,[1,2,3,5,4],2),
            linha(3,[3,4,5,2,1],3),
            linha(2,[4,5,2,1,3],0),
            linha(0,[2,1,4,3,5],1),
            linha(0,[5,1,3,4,2],0),
            linha(3,[3,2,4,5,1],2),
            linha(3,[1,3,5,2,4],2),
            linha(0,[],2),
            linha(0,[],1)]).


wolkenkratzer(Linhas) :- 
    length(Linhas,X),
    maplist(same_length(Linhas), Linhas).




getList(linha(_,L,_),L).

getLists([],[]).
getLists([H|T],[L1|L2]) :- getList(H,L1),getLists(T,L2).

linhaTeste(1,linha(3, [3,2,4,5,1], 2)).
linha(3, [3,2,4,5,1], 2).

nPredios([],0,_).
nPredios([H|T],A,M) :- 
    H > M,
    nPredios(T,A1,H),
    A is A1+1,!.
nPredios([_|T],A,M) :- nPredios(T,A,M).

checkLine(linha(E,L,D)) :- 
    nPredios(L,E,0),
    reverse(L,Lr),
    nPredios(Lr,D,0),
    all_distinct(L).

