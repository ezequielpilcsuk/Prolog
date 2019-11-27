:- use_module(library(clpfd)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Constraint posting
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

sudoku(Rows) :-
        length(Rows, 9), maplist(same_length(Rows), Rows),
        append(Rows, Vs), Vs ins 1..9,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns), maplist(all_distinct, Columns),
        Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
        blocks(As, Bs, Cs), blocks(Ds, Es, Fs), blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
        all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
        blocks(Ns1, Ns2, Ns3).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Animation.

   A frozen goal for each variable emits PostScript instructions to
   draw a number. On backtracking, the field is cleared.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

animate(Rows) :- animate(Rows, 1).

animate([], _).
animate([Row|Rows], N) :-
        animate(Row, 1, N),
        N1 #= N + 1,
        animate(Rows, N1).

animate([], _, _).
animate([C|Cs], Col, Row) :-
        freeze(C, label(Col, Row, C)),
        Col1 #= Col + 1,
        animate(Cs, Col1, Row).

label(Col, Row, N) :- format("(~w) ~w ~w num\n", [N,Col,Row]).
label(Col, Row, _) :- format("~w ~w clear\n", [Col,Row]), false.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Sample problems.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
problem(0,P) :-
        P = [[_,_,_,_,_,_,_,_,_],
             [_,_,_,_,_,_,_,_,_],
             [_,_,_,_,_,_,_,_,_],
             [_,_,_,_,_,_,_,_,_],
             [_,_,_,_,_,_,_,_,_],
             [_,_,_,_,_,_,_,_,_],
             [_,_,_,_,_,_,_,_,_],
             [_,_,_,_,_,_,_,_,_],
             [_,_,_,_,_,_,_,_,_]].

problem(1, P) :- % shokyuu
        P = [[1,_,_,8,_,4,_,_,_],
             [_,2,_,_,_,_,4,5,6],
             [_,_,3,2,_,5,_,_,_],
             [_,_,_,4,_,_,8,_,5],
             [7,8,9,_,5,_,_,_,_],
             [_,_,_,_,_,6,2,_,3],
             [8,_,1,_,_,_,7,_,_],
             [_,_,_,1,2,3,_,8,_],
             [2,_,5,_,_,_,_,_,9]].


problem(2, P) :-  % shokyuu
        P = [[_,_,2,_,3,_,1,_,_],
             [_,4,_,_,_,_,_,3,_],
             [1,_,5,_,_,_,_,8,2],
             [_,_,_,2,_,_,6,5,_],
             [9,_,_,_,8,7,_,_,3],
             [_,_,_,_,4,_,_,_,_],
             [8,_,_,_,7,_,_,_,4],
             [_,9,3,1,_,_,_,6,_],
             [_,_,7,_,6,_,5,_,_]].

problem(3, P) :-
        P = [[1,_,_,_,_,_,_,_,_],
             [_,_,2,7,4,_,_,_,_],
             [_,_,_,5,_,_,_,_,4],
             [_,3,_,_,_,_,_,_,_],
             [7,5,_,_,_,_,_,_,_],
             [_,_,_,_,_,9,6,_,_],
             [_,4,_,_,_,6,_,_,_],
             [_,_,_,_,_,_,_,7,1],
             [_,_,_,_,_,1,_,3,_]].

problem(666,P) :-
        P = [[8,_,_,_,_,_,_,_,_],
             [_,_,3,6,_,_,_,_,_],
             [_,7,_,_,9,_,2,_,_],
             [_,5,_,_,_,7,_,_,_],
             [_,_,_,_,4,5,7,_,_],
             [_,_,_,1,_,_,_,3,_],
             [_,_,1,_,_,_,_,6,8],
             [_,_,8,5,_,_,_,1,_],
             [_,9,_,_,_,_,4,_,_]].


show(Options, Rows) :-
    sudoku(Rows),
    open(pipe('gs -dNOPROMPT -g680x680 -dGraphicsAlphaBits=2 -r150 -q'),
            write, Out, [buffer(false)]),
    tell(Out),
    phrase(postscript, Ps),
    format(Ps),
    append(Rows, Vs),
    call_cleanup((animate(Rows),labeling(Options, Vs)), close(Out)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Examples:

   ?- problem(1, Rows), sudoku(Rows),
      maplist(labeling([ff]), Rows), maplist(portray_clause, Rows).
   [1, 5, 6, 8, 9, 4, 3, 2, 7].
   [9, 2, 8, 7, 3, 1, 4, 5, 6].
   [4, 7, 3, 2, 6, 5, 9, 1, 8].
   [3, 6, 2, 4, 1, 7, 8, 9, 5].
   [7, 8, 9, 3, 5, 2, 6, 4, 1].
   [5, 1, 4, 9, 8, 6, 2, 7, 3].
   [8, 3, 1, 5, 4, 9, 7, 6, 2].
   [6, 9, 7, 1, 2, 3, 5, 8, 4].
   [2, 4, 5, 6, 7, 8, 1, 3, 9].
   Rows = [[1, 5, 6, 8, 9, 4, 3, 2|...], ...]

   ?- show([ff], Rows).

   ?- problem(3, Rows), show([ff], Rows).

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */