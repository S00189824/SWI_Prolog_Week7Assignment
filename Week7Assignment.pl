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
    NextRow is Row -1, %
    createBoard(NextRow,Col).

createCol(Row,1) :- % What does the 1 signify
    assertz(playersquares(Row,1,e)),
    assertz(aIsquares(Row,1,e)). %

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

player_place(Symbol,Row,Col) :-% Find the square using a the row and column passed in, removes tha sqaure and relaces i wih he symbol passed in
	retract(playersquares(Row,Col,_)),
	assertz(playersquares(Row,Col,Symbol)).

player_attack_enemy(Row,Col,Symbol) :-
	aIsquares(Row,Col,SymbolDelete),
	retract(aIsquares(Row,Col,SymbolDelete)),
	assertz(aIsquares(Row,Col,Symbol)).


ai_place(Symbol,Row,Col) :-
	retract(aIsquares(Row,Col,_)),
	assertz(aIsquares(Row,Col,Symbol)).

action(R,Col,Symbol):-
	player_place(Symbol,R,Col),
	member(Symbol,list_board),
	member([R,Col,Symbol],list_board).

% Hit
:- dynamic hit/1.
%Miss
:- dynamic miss/1.

place([Symbol|T],Board,FinalBoard) :-
    maxCol(N),
    random_between(1,N,R),
    random_between(1,N,C),
    place(Symbol,R,C,Board,NewBoard),
    place(T,NewBoard,FinalBoard).

playershoot :-
	write('Enter Row'),
	read(Row),
	write('EnterColumn'),
	read(Column),
	hitOrMiss_AI(Row,Column,Result),
	player_attack_enemy(Row,Column,Result).

aiShoot():-
	write('enemy shooting'),
	choose_computer_move().

hitOrMiss_AI(Row,Col,Value):-
	aIsquares(Row,Col,Symbol),
	(Symbol = s -> Value = h
	    ; Value = m).

computer(Row,Col,Board):-
	setof([R,C]),newBoard.

hitOrMiss_Player(Row,Col,Value):-
	playersquares(Row,Col,Symbol),
	(Symbol = s -> Value = h
	    ; Value = m
	).
% to check if it is ship hit or else player wins.
player_Win:-
	(   aIsquares(_,_,s) -> aiShoot; write('playerWin')).
%two boards taken from latest example with manual options of hit miss

board(c, [
                 [1,1,e],[1,2,m],[1,3,e],
                 [2,1,e],[2,2,e],[2,3,b]
    ]).

board(h, [
                 [1,1,e],[1,2,m],[1,3,e],
                 [2,1,b],[2,2,x],[2,3,e]
    ]).
% here computer choice passing in these args for getting all row cols
choose_computer_move(ChosenRow,ChosenCol) :-
    board(h,B),
    findall([R,C],(member([R,C,V],B),
                   \+hit(c,[R,C,V]),
                     \+miss(c,[R,C,V])),[[ChosenRow,ChosenCol]|_]).
/*
human()

computer()

%fired(Row,Col,Symbol):-
	%hit(h,Row,Col).

*/
