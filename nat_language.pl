% MSc Computer Science - 2nd Assessed Prolog Lab Exercise
% NATURAL LANGUAGE MODEL
% Author: Will Moyle
% Last Updated: 19/12/14

% The Lexicon:

article([the]).
article([a]).
article([an]).

noun([grass]).
noun([cow]).
noun([girl]).
noun([boy]).
noun([apple]).
noun([song]).

verb([eats]).
verb([chews]).
verb([kicks]).
verb([sings]).

adverb([slowly]).  
adverb([deliberately]).  
adverb([merrily]).
adverb([sweetly]).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 1 -------------------------------------------------------------------
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% noun_phrase/1 - arg is a combination of article and noun:
noun_phrase(P) :-
	article(A),
	noun(N),	
	append(A,N,P).


% verb_phrase/1 - arg is a verb or a verb and a noun phrase
verb_phrase(V) :-
	verb(V).
verb_phrase(P) :-
	verb(V),
	noun_phrase(N),
	append(V, N, P).


% sentence/1 - arg is a noun phrase followed by a verb phrase
sentence(S) :-
	noun_phrase(N),
	verb_phrase(V),
	append(N,V,S).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 2 -------------------------------------------------------------------
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% first_char/2 - 2nd arg is first letter of 1st arg
first_char(Word, Letter) :-	
	atom_chars(Word, [Letter|_]).


% vowel_start/1 - 1st arg is a word that starts with a vowel
vowel_start([Word]) :-
	Vowels = [a, e, i, o, u],
	first_char(Word, Letter),
	member(Letter, Vowels).


% noun_phrase_better/1 - 1st arg is a noun phrase with appropriate 'a' or 'an'
noun_phrase_better(Phrase) :-
	article(A),
	noun(N),
	append(A, N, Phrase),
	(vowel_start(N) ->
	member(A, [[the], [an]]);
	member(A, [[the], [a]])).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 3 -------------------------------------------------------------------
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% all_adverbs/1 - arg is a list of all adverbs in the lexicon
all_adverbs(List) :-
	findall(Word, adverb([Word]), List).


% adverb_list/1 - arg is a unique list of adverbs
adverb_list(List) :-
	all_adverbs(ListAdverbs),
	subset(ListAdverbs, SubList),
	permutate(SubList, List),
	List \= [].


% add_comma_and/2 - 2nd arg is same as 1st arg with ',' or 'and'
add_comma_and([], []).
add_comma_and([X], [X]).
add_comma_and([X, Y], [X, 'and', Y]).
add_comma_and([H|T1], [H|T2]) :-
	length(T1, X), 
	X > 1,
	T2 = [','|T1commas],
	add_comma_and(T1, T1commas).


% cadvs/1 - arg is a list of adverbs with appropriate ',' and 'and'
cadvs(Phrase) :-
	adverb_list(ListAds),
	add_comma_and(ListAds, Phrase).


% verb_phrase_better/1 - arg is verb phrase with possible cadvs
verb_phrase_better(Phrase) :-	        % case 1 - verb
	verb(Phrase).
verb_phrase_better(Phrase) :-
	noun_phrase_better(Noun),       % case 2 - verb and noun
	verb(Verb),
	append(Verb, Noun, Phrase).
verb_phrase_better(Phrase) :-           % case 3 - adverbs and verb
	verb(Verb),
	cadvs(Adverbs),
	append(Adverbs, Verb, Phrase).
verb_phrase_better(Phrase) :-           % case 4 - adverbs, verb, and noun
	noun_phrase_better(Noun),
	verb(Verb),
	cadvs(Adverbs),
	append(Adverbs, Verb, Noun, Phrase).


% sentence_better/1 - arg is a concatenation of a better noun and verb phrase
sentence_better(Sentence) :-
	noun_phrase_better(Noun),
	verb_phrase_better(Verb),	
	append(Noun, Verb, Sentence).


% Helper Functions -------------------------------------------------------------


% append/4 - 1st, 2nd & 3rd arg concatenate to 4th arg
append(A, B, C, List) :-
	append(A, B, TempList),
	append(TempList, C, List).


% subset/2 - 2nd arg is a subset of the 1st arg
subset([], []).
subset([Head|Tail1], [Head|Tail2]):-
 	subset(Tail1, Tail2).
subset([_|Tail1], Tail2):-
	subset(Tail1, Tail2).


% permutate/2 -  1st arg is a permutation of the 2nd arg
permutate([], []).
permutate(List, [Head|Perm]) :-
	delete_first(Head, List, Rest),
	permutate(Rest, Perm).


% delete_first/3 - 1st occurence of 1st arg in 2nd arg is removed in 3rd arg
delete_first(A, [A|Tail], Tail).
delete_first(A, [Head|Tail1], [Head|Tail2]) :-
	delete_first(A, Tail1, Tail2).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 4 -------------------------------------------------------------------
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% noun_verb/3 - 2nd arg is noun, 3rd arg is verb, 1st arg is 2nd appended to 3rd
noun_verb(Phrase, Noun, Verb) :-
	noun(Noun),
	verb(Verb),
	append(Noun, Verb, Phrase).


% actions/3 - 3rd arg is list of verbs performed by 1st arg in sentences 2nd arg
actions(Subject, Full, Actions) :-
	noun([Subject]),	
	%combined_sentence(Full), % can check that 2nd arg is correct
	findall(Verb,
		(section_of(Full, Phrase),
		 noun_verb(Phrase, [Subject], [Verb])),
		Actions).


% Helper Functions -------------------------------------------------------------


% section_of/2 - 2nd arg contains the list in 1st arg in order
section_of(_, []).
section_of([H|T1], [H|T2]) :-
	append(T2, _, T1).
section_of([_|T1], T2) :-
	section_of(T1, T2).

/* FUNCTION REMOVED AS CORRECT SENTENCE WILL ALWAYS BE GIVEN
combined_sentence/1 - arg is a concatenation of sentences with 'and' between 
combined_sentence(Full) :-
	sentence(Full).
combined_sentence(Full) :-
	sentence(First),	
	append(First, [and], Second, Full),
	combined_sentence(Second). */


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 5 -------------------------------------------------------------------
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/* noun_adverb_verb/4 - 2nd arg is noun, 3rd arg is cadvs or [] (without
                      commas), 4th arg is verb, 1st arg is combination of all */
noun_adverb_verb(Phrase, Noun, Adverbs, Verb) :-
	noun(Noun),
	((cadvs(Adverbs1),
	  remove_elements(',', Adverbs1, Adverbs)); % remove line if adverbs
	                                            % have commas
	 Adverbs = []),
	verb(Verb),
	append(Noun, Adverbs, Verb, Phrase).


/* actions_and_adverbs/3 - 3rd arg is a list of (verb, adverb) pairs for all
                           actions performed by 1st arg in 2nd arg */
actions_and_adverbs(Actor, Text, As) :-
	noun([Actor]),
	%combined_sentence_better(Text), % can check that 2nd arg is correct
	findall((Verb,Adverbs2),
		(section_of(Text, Phrase),
		 noun_adverb_verb(Phrase, [Actor], Adverbs1, [Verb]),
		 remove_elements(and, Adverbs1, Adverbs2)),
		As).


% Helper Functions -------------------------------------------------------------


% remove_elements/3 - every instance of 1st arg is removed from 2nd in 3rd
remove_elements(_, [], []).
remove_elements(A, [A|Tail], List) :-
	remove_elements(A, Tail, List).
remove_elements(A, [Head|Tail], List1) :-
	Head \= A,
	remove_elements(A, Tail, List2),
	append([Head], List2, List1).


/* FUNCTIONS REMOVED AS CORRECT SENTENCE WILL ALWAYS BE GIVEN
% sentence_better_no_comma/1 - ',' is not an element in the list argument
sentence_better_no_comma(NoComma) :-
	sentence_better(Sentence),
	remove_elements(',', Sentence, NoComma).

% combined_sentence_better/1 - arg is a concatenation of better sentences
   without commas
combined_sentence_better(Full) :-
	sentence_better_no_comma(Full).
combined_sentence_better(Full) :-
	sentence_better_no_comma(First),	
	append(First, [and], Second, Full),
	combined_sentence_better(Second). */
