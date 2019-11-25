:- use_module(library(clpfd)).

% Ideia de solução é:
%     Usar 2n linhas para representar linhas e colunas
%     Garantir que todos diferentes em uma linha
%     Garantir que [0..n] linhas == transpose [n..2n]
%     Garantir que soma de cada linha seja igual ao necessário

% wolkenkratzer(Rows) :-
%     length(Rows, 5), maplist(same_length(Rows), Rows),
%     append

%    3 [2,3,5,4,1] 3

predios(_,0, _):-!.
predios([H|T], N, M) :- 


satisfies(E,L,D) :- predios(L,D), reverse(L,LR), predios(LR,E).