% square(row,col,val)
:- dynamic playersquares/3.
:- dynamic aIsquares/3.

list_board :-
	setof([R,C,V],playersquares(R,C,V),PlayerList),
	writeln('player squares'),
	write_list(PlayerList),
	setof([R,C,V],aIsquares(R,C,V),AiList),
	writeln('ai squares'),
	write_list(AiList).

createBoard(N) :-
	createBoard(N,N).

createBoard(0,_).
createBoard(Row,Col) :-
    createCol(Row,Col),
    NextRow is Row -1, % What does the -1 signify
    createBoard(NextRow,Col).

createCol(Row,1) :- % What does the 1 signify
    assertz(playersquares(Row,1,e)),
    assertz(aIsquares(Row,1,e)). % (Row,1,e) ??

createCol(CurrentRow,CurrentCol) :-
    assertz(playersquares(CurrentRow,CurrentCol,e)),
    assertz(aIsquares(CurrentRow,CurrentCol,e)),
    NextCol is CurrentCol -1,
    createCol(CurrentRow,NextCol).


%createRows(N,_,[]).%%first row giving empty list
%createRows(R,C,Board):-
	%NextRow is R -1,
%	createRows(NextRow,C,FinalRow),
%	createcol(NextRow,C,RowCols),
%	append(RowCols,NextRow,Board).

% For each row index created, create a set of columns.
%
write_list([]).
write_list([H|T]) :-
    write(H),nl,
    write_list(T).

place(N) :-
    Row1 is random(N),
    Col1 is random(N),
    ai_place(s,Row1,Col1).

player_place(Symbol,Row,Col) :-
	retract(playersquares(Row,Col,_)),
	assertz(playersquares(Row,Col,Symbol)).

ai_place(Symbol,Row,Col) :-
	retract(aIsquares(Row,Col,_)),
	assertz(aIsquares(Row,Col,Symbol)).

action(R,Col,Symbol):-
	player_place(Symbol,R,Col),
	member(Symbol,list_board),
	member([R,Col,Symbol],list_board).

% Hit
:- dynamic hit/1.


test :-
	write('dfgdf'),
	read(Input),
	write(Input).

fired(R,Col,Symbol):-
	player_place ->.

% Miss
:- dynamic miss/1.
