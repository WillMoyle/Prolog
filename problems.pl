% VARIOUS PROLOG PROBLEMS
% Author: Will Moyle
% Last Updated: 23/01/15

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LIST MANIPULATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

my_last(X, [X]).
my_last(X, [_|T]):-
	my_last(X, T).


penult(X, [X, _]).
penult(X, [_|T]):-
	penult(X, T).


element_at(X, L, N):-
	append(L1, [X|_], L),
	length(L1, M),
	N is M+1.


len(0, []).
len(X, [_|T]):-
	len(Y, T),
	X is Y+1.


reverse([],[]).
reverse([H|T1], L):-
	reverse(T1, T2),
	append(T2,[H],L).


palindrome(X):-
	reverse(X, X).

is_list([]).
is_list([_|_]).
	
my_flatten([],[]).
my_flatten([H1|T1],L):-
	is_list(H1) ->
	(my_flatten(H1, [H2|T2]),
	 append(T2, T1, T3),
	 my_flatten(T3, T4),
	 L = [H2|T4]);
	(my_flatten(T1, T5),
	 L = [H1|T5]).


min_element(X, [X]).
min_element(X, [H|T]) :-
	min_element(Y, T),
	(H < Y -> X = H;
	X = Y).

remove_first(_, [], []).
remove_first(X, [X|T], T).
remove_first(X, [H|T], L) :-
	H\= X,
	remove_first(X, T, NewList),
	L = [H|NewList].

my_sort([], []).
my_sort(Unsorted, Sorted) :-
	min_element(X, Unsorted),
	remove_first(X, Unsorted, Tail),
	sort(Tail, NewSort),
	Sorted = [X|NewSort].

swap(List1, X, Y, List2) :-
	append(Start, [First|End], List1),
	Z is X-1,
	length(Start, Z),
	Diff is Y - X - 1,
	append(A, [Second|B], End),
	length(A, Diff),
	append(Start, [Second], M),
	append(M, A, N),
	append(N, [First], P),
	append(P, B, List2).


/*bubble_sort([], []).
bubble_sort([H1|T1], [H2|T2]) :-
	min_element(*/



sublist([], _).
sublist([H|T], X):-
	member(H, X),
	sublist(T, X).

difference(L1, L2, L):-
	findall(X, (member(X, L1), \+member(X, L2)), L).

sift(L, N, Result):-
	findall(X, (member(X, L), X =< N), Result).

shared(L1, L2, I):-
	setof(X, (member(X, L1), member(X, L2)), I).

common(L1, L2, I):-
	(\+shared(L1, L2, I) -> I = []);
	shared(L1, L2, I).

delete([], []).
delete([X], [X]).
delete([H1, _|T1], [H1|T2]):-
	delete(T1, T2).

process([], [], [], []).
process(X, [], [], X).
process([], X, [], X).
process(L1, L2, Consistent, Inconsistent):-
	findall((Name,Number,MoreInfo),
		(member((Name,Number,MoreInfo), L2),
		member((Name,Number),L1)),
		Consistent),
	findall((Name,Number,MoreInfo),
		(member((Name,Number,MoreInfo), L2),
		\+member((Name,Number),L1)),
		Inconsistent).


duplicate([],[]).
duplicate([H|T1],[H,H|T2]):-
	duplicate(T1,T2).


same_elements([H],H,1).
same_elements(L,H,N):-
	N > 0,	
	M is N-1,	
	same_elements(T,H,M),
	append(T, [H], L).


duplicate([],_,[]).
duplicate([H1|T1], N, L):-
	same_elements(H2, H1, N),
	append(H2, T2, L),
	duplicate(T1, N, T2).


drop(L1, N, L1):-
	length(L1, M),
	M < N.
drop(L1, N, L2):-
	append(X, [_|T1], L1),
	M is N-1,	
	length(X, M),
	append(X, T2, L2),
	drop(T1, N, T2).


split(L, N, L1, L2):-
	append(L1, L2, L),
	length(L1, N).


slice(L1, Start, End, L2):-
	End2 is Start-1,
	split(L1, End2, _, Rest),
	Length is End-Start+1,
	split(Rest, Length, L2, _).


remove_at(X, List, N, Rest):-
	element_at(X, List, N),
	append(Start, [X|Tail], List),
	append(Start, Tail, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ARITHMETIC
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

natural_number(0).
natural_number(X) :-
	natural_number(Y),
	X is Y+1.

natural_number(Min, Min, Max) :-
	Min =< Max.
natural_number(X, Min, Max) :-
	natural_number(Y, Min, Max),
	Y =< Max,	
	X is Y + 1.

list_of_nats([1],1).
list_of_nats([N|T],N):-
	N > 0,
	M is N-1,
	list_of_nats(T, M).

nat(M, N):-
	list_of_nats(L, N),
	member(M, L).


list_of_nats2([2],2).
list_of_nats2([N|T],N):-
	N > 1,
	M is N-1,
	list_of_nats2(T, M).

divides(X, Y):-
	list_of_nats2(L,Y),
	member(X, L),
	member(Z, L),
	Y is Z * X.


divisors(X, L):-
	setof(Y, divides(Y, X), L),!; L = [].

prime(X):-
	divisors(X, []).

max(X, Y, X):-
	X > Y,!.
max(X, Y, Y):-
	Y >= X.

min(X, Y, X):-
	X < Y,!.
min(X, Y, Y):-
	Y =< X.

gcd(0, X, X).
gcd(X, 0, X).
gcd(X, Y, G):-
	X > 0,
	Y > 0,
	max(X, Y, Max),
	min(X, Y, Min),
	Diff is Max - Min,
	gcd(Min, Diff, G).

coprime(X, Y):-
	gcd(X, Y, 1).

totient_phi(N, Phi):-
	setof(M, (X is N-1, nat(M, X), coprime(M, N)), L),
	length(L, Phi).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GRAPHS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%graph([b-c, f-c, g-h, d, f-b, k-f]).
graph([a-b,b-c,b-e,c-e,a-d,d-e,e-h,h-g,d-f,d-g,f-g]).
%graph([a-b,a-c,a-d,b-c,b-d,c-d]).

connected(G, A, B):-
	graph(G),
	(member(A-B, G);
	member(B-A, G)).



route(G, Start, End, Path) :-
	graph(G),	
	route(G, Start, End, Path, [Start]).
route(_, End, End, Path, Reverse) :-
	reverse(Reverse, Path).
route(G, Start, End, Path, Visited) :-
	connected(G, Start, Next),
	\+member(Next, Visited),
	NewVisited = [Next|Visited],
	route(G, Next, End, Path, NewVisited).
	
cycle(G, Start, Path) :-
	connected(G, Start, Next),
	route(G, Next, Start, NewPath),
	NewPath \= [Next, Start],
	Path = [Start|NewPath]. 


all_nodes(G, List) :-
	graph(G),
	findall(Node,
	      (member(Node-_, G);
	       member(_-Node, G)),
	      List1),
	setof(X, member(X, List1), List).

number_nodes(G, N) :-
	all_nodes(G, List),
	length(List, N).


tree(G, T) :-
	graph(G),
	tree(G, T, [], []).

tree(G, T, [], []) :-
	connected(G, A, B),
	tree(G, T, [A, B], [A-B]).

tree(G, T, VisitedNodes, T) :-
	number_nodes(G, N),
	length(VisitedNodes, L),
	L = N.

tree(G, T, VisitedNodes, Accumulator) :-
	member(FirstNode, VisitedNodes),
	connected(G, FirstNode, NewNode),
	\+member(NewNode, VisitedNodes),
	tree(G, T, [NewNode|VisitedNodes], [FirstNode-NewNode|Accumulator]).

is_connected(G) :-
	tree(G, _).

is_tree(G) :-
	tree(G, G).
	 

print_test(G) :-
	write('Write a sentence'),
	nl,
	read(X),
	nl,
	write('Printing:'),
	nl,
	write(X).




/*



%%%%%%% LEARN THIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
path(G, Start, End, Path) :-
	path(G, Start, End, [Start], Path).

path(_, End, End, RPath, Path) :-
	reverse(RPath, Path).

path(G, Start, End, Visited, Path) :-
	connected(G, Start, Next),
	\+ memberchk(Next, Visited),
	path(G, Next, End, [Next|Visited], Path).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cycle(G, A, P) :-
	connected(G, A, Next),
	path(G, Next, A, Path),
	Path \= [Next, A],
	P = [A|Path].

all_nodes(G, List):-
	graph(G),
	findall(X, (member(X-_,G); member(_-X, G)), List1),
	setof(X, member(X, List1), List).

number_nodes(G, N):-
	all_nodes(G, List),
	length(List, N).

new_route(G, Start, End, Discovered):-
	graph(G),
	connected(G, Start, End),
	\+memberchk(End, Discovered).

s_tree(G, T) :-
	graph(G),
	s_tree(G, T, [], [], _).
			 
s_tree(G, T, [], [], _) :-
	connected(G, A, B),
	s_tree(G, T, [A, B], [A-B], 0).
s_tree(G, T, Discovered, Paths, Attempts) :-
	length(Discovered, N),
	number_nodes(G, M),
	(M = N -> T = Paths;
	 (Discovered = [H|Tail],
	  (new_route(G, H, New, Discovered) ->
	   (NewPaths = [H-New|Paths],
	    NewDiscovered = [New|Discovered],
	    s_tree(G, T, NewDiscovered, NewPaths, 0));
	   (Attempts = N -> fail;
	    (Y is Attempts+1,
	     append(Tail, [H], NewDiscovered2),
	     s_tree(G, T, NewDiscovered2, Paths, Y)))))).
	
is_tree(G) :-
	s_tree(G, G),!.

is_connected(G) :-
	s_tree(G, _),!. */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MISCELLANEOUS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

diagonal(X1-Y1, X2-Y2) :-
	YDiff is Y1 - Y2,
	XDiff is X1 - X2,
	(YDiff = XDiff;
	 YDiff is 0 - XDiff).

test_for_diagonals(X, [Y]) :-
	\+diagonal(X, Y).
test_for_diagonals(X, [H|T]) :-
	\+diagonal(X, H),
	test_for_diagonals(X, T).


no_diagonals([_]).
no_diagonals([H|T]) :-
	test_for_diagonals(H, T),
	no_diagonals(T).


horizontal(_-X, _-X).

convert([A, B, C, D, E, F, G, H], [1-A, 2-B, 3-C, 4-D, 5-E, 6-F, 7-G, 8-H]).


generate_list_from_elements(List, Elements) :-
	generate(List, Elements, []).

generate(Used, Elements, Used) :-
	length(Used, N),
	length(Elements, M),
	M = N.
generate(List, Elements, Used) :-
	member(New, Elements),
	\+member(New, Used),
	generate(List, Elements, [New|Used]).

gener8(List) :-
	generate_list_from_elements(List, [1,2,3,4,5,6,7,8]).


queen(List) :-
	gener8(List),
	convert(List, PairedList),
	no_diagonals(PairedList).