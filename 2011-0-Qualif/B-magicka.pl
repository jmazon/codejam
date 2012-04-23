% -*- Prolog -*-

split(L,S) :- split(L,[],S).
split([],A,[A2]) :- reverse(A,A2).
split([32|L],A,[A2|S]) :- !, reverse(A,A2), split(L,[],S).
split([H|T],A,S) :- split(T,[H|A],S).

splitAt(N,S,H,T) :- splitAt(N,S,H,T,[]).
splitAt(0,T,H,T,A) :- !, reverse(A,H).
splitAt(N,[S|SS],H,T,A) :- N2 is N-1, splitAt(N2,SS,H,T,[S|A]).

main :-
	current_input(Stdin),
	read_line_to_codes(Stdin,ST),
	number_codes(T,ST),
	between(1,T,I),
	read_line_to_codes(Stdin,TC),
	split(TC,[SNC|TC2]),
	number_codes(NC,SNC),
	splitAt(NC,TC2,CS,[SND|TC3]),
	number_codes(ND,SND),
	splitAt(ND,TC3,DS,[_,S]),
	solve(S,S2,CS,DS),
	maplist(char_code,S3,S2),
	swritef(SO,'Case #%w: %w\n',[I,S3]),
        string_to_atom(SS,SO),
        string_to_list(SS,O),
        reformat(O,O2),
	string_to_list(R,O2),
	write(R),
	fail.

solve(S,S2,CS,DS) :- solve(S,S2,CS,DS,[]).
solve([S|SS],S2,CS,DS,[A|AA]) :- 
	( member([S,A,R],CS) ; member([A,S,R],CS) ), !,
	solve(SS,S2,CS,DS,[R|AA]).
solve([S|SS],S2,CS,DS,A) :-
	( member([S,O],DS) ; member([O,S],DS) ),
	member(O,A), !,
	solve(SS,S2,CS,DS,[]).
solve([S|SS],S2,CS,DS,A) :- solve(SS,S2,CS,DS,[S|A]).
solve([],S2,_,_,A) :- reverse(A,S2).

reformat([],[]).
reformat([44|T],[44,32|T2]) :- !, reformat(T,T2).
reformat([H|T],[H|T2]) :- reformat(T,T2).
