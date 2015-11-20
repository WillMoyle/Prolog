% MSc Computer Science - 1st Assessed Prolog Lab Exercise
% HOGWARTS MODEL
% Author: Will Moyle
% Last Updated: 19/11/14


% the students in Hogwarts
student(hp, 'Harry James Potter', gryffindor).
student(hg, 'Hermione Jean Granger', gryffindor).
student(rw, 'Ronald Weasley', gryffindor).
student(ll, 'Luna Lovegood', ravenclaw).
student(cc, 'Cho Chang', ravenclaw).
student(tb, 'Terry Boot', ravenclaw).
student(ha, 'Hannah Abbott', hufflepuff).
student(cd, 'Cedric Diggory', hufflepuff).
student(nt, 'Nymphadora Tonks',hufflepuff).
student(dm, 'Draco Malfoy', slytherin).
student(gg, 'Gregory Goyle', slytherin).
student(vc, 'Vincent Crabbe', slytherin).
student(wm, 'Will Moyle', ravenclaw). % Question 1 - create new student

% the teachers in Hogwarts
teacher(ad, 'Albus Percival Wulfric Brian Dumbledore').
teacher(ff, 'Filius Flitwick').
teacher(rh, 'Rubeus Hagrid').
teacher(gl, 'Gilderoy Lockhart').
teacher(rl, 'Remus John Lupin').
teacher(mm, 'Minerva McGonagall').
teacher(qq, 'Quirinus Quirrell').
teacher(ss, 'Severus Snape').
teacher(ps, 'Pomona Sprout').
teacher(st, 'Sibyll Patricia Trelawney').
teacher(mh, 'Madam Hooch').
teacher(as, 'Aurora Sinistra').
teacher(cub, 'Cuthbert Binns').
teacher(bb, 'Bathsheba Babbling').
teacher(sv, 'Septima Vector').
teacher(chb, 'Charity Burbage').
teacher(wt, 'Wilkie Twycross').

% compulsory courses for the MSc in Magic
compCourse(astro, 'Astronomy', as).
compCourse(charms, 'Charms', ff).
compCourse(defence, 'Defence against the Dark Arts', qq).
compCourse(fly, 'Flying', mh).
compCourse(herb, 'Herbology', ps).
compCourse(history, 'History of Magic', cub).
compCourse(potions, 'Potions', ss).
compCourse(trans, 'Transfiguration', mm).

% optional courses for the MSc in Magic
optCourse(runes, 'Study of Ancient Runes', bb).
optCourse(arith, 'Arithmancy', sv).
optCourse(muggle, 'Muggle Studies', chb).
optCourse(creatures, 'Care of Magical Creatures', rh).
optCourse(div, 'Divination', st).
optCourse(app, 'Apparition', wt).
optCourse(choir, 'Frog Choir', ff).
optCourse(quid, 'Quidditch', mh).

% the houses in Hogwards
house(gryffindor).
house(slytherin).
house(ravenclaw).
house(hufflepuff).

% Question 2:

% enrolled_opt - student with short name SID is enrolled in optional course SCN
enrolled_opt(wm, X) :-
	X = arith; X = app; X = quid.
enrolled_opt(hp, X) :-
	X = runes; X = arith; X = creatures.
enrolled_opt(hg, X) :-
	X = runes; X = arith; X = muggle;
	X = creatures; X = div; X = app.
enrolled_opt(rw, X) :-
	X = quid; X = div; X = creatures.
enrolled_opt(ll, X) :-
	X = runes; X = arith; X = quid.
enrolled_opt(cc, X) :-
	X = runes; X = muggle; X = choir.
enrolled_opt(tb, X) :-
	X = runes; X = arith; X = div.
enrolled_opt(ha, X) :-
	X = runes; X = muggle; X = creatures.
enrolled_opt(cd, X) :-
	X = arith; X = app; X = quid.
enrolled_opt(nt, X) :-
	X = div; X = choir; X = creatures.
enrolled_opt(dm, X) :-
	X = quid; X = arith; X = choir.
enrolled_opt(gg, X) :-
	X = runes; X = arith; X = quid.
enrolled_opt(vc, X) :-
	X = muggle; X = choir; X = div.


% Question 3:

% enrolled - student with ID SID is enrolled in course with short name SCN

enrolled(SID, SCN) :-
	compCourse(SCN, _, _),
	student(SID, _, _).

enrolled(SID, SCN) :-
	enrolled_opt(SID, SCN).


% Question 4:

% teaches - the wizard of name TN teaches course with short name SCN
teaches(TN, SCN) :-
	teacher(TID, TN),
	(compCourse(SCN, _, TID);
	optCourse(SCN, _, TID)).


% Question 5:

% taughtBy - the student named SN studies a course taught by teacher named TN
taughtBy(SN, TN) :-
	teaches(TN, SCN),
	student(SID, SN, _),
	enrolled(SID, SCN).


% Question 6: 

% takesOption - the student with name SN takes the optional course called CN
takesOption(SN, CN) :-
	student(SID, SN, _),
	enrolled_opt(SID, SCN),
	optCourse(SCN, CN, _).


% Question 7:

% takesAllOptions - the student with name SN takes all optional courses named in list OptCourses
takesAllOptions(SN, OptCourses) :-
	setof(CN, takesOption(SN, CN), OptCourses).


% Question 8:

% housePairs - Students is a list of SID-SN pairs for all students in House
housePairs(House, Students) :-
	setof(SID-SN, student(SID, SN, House), Students).

% studentsInHouse - students contains a list of student names, organised by student IDs, in House
studentsInHouse(House, Students) :-
	house(House),
	(housePairs(House, List) ->
	findall(SN, SID^member(SID-SN, List), Students);
	Students = []).

	
% Question 9:

% course - there exists an optional or compulsory course with name CN and ID SCN
course(SCN, CN) :-
	compCourse(SCN, CN, _).
course(SCN, CN) :-
	optCourse(SCN, CN, _).

% houseStudy - a student named SN is in House and studies course with ID SCN
houseStudy(House, SN, SCN):-
	student(SID, SN, House),
	enrolled(SID, SCN).

% houseStudyList - a list of student names (Students) in House studying course with ID SCN
houseStudyList(House, Students, SCN):-
	house(House),
	course(SCN, _),	
	(setof(SN, houseStudy(House, SN, SCN), Students1) ->
	Students = Students1;
	Students = []).

% studentsOnCourse - returns a list of (House-Students) where students are all in House and study SCN
studentsOnCourse(SCN, CN, StudentsByHouse) :-
	course(SCN, CN),
	setof(House-Students, houseStudyList(House, Students, SCN), StudentsByHouse).


% Question 10:

% sharedCourse - optional course named CN is studied by both students with names SN1 & SN2
sharedCourse(SN1, SN2, CN) :-
	student(SID1, SN1, _),
	student(SID2, SN2, _),
	optCourse(SCN, CN, _),
	enrolled(SID1, SCN),
	enrolled(SID2, SCN),
	SN1 \= SN2.


% Question 11:

% allSharedOptions - Courses is a list of all optional course names studied by both SN1 and SN2
allSharedOptions(SN1, SN2, Courses) :-
	setof(Course,sharedCourse(SN1,SN2,Course),Courses).

% sameOptions - students named SN1 and SN2 study 3 of the same optional courses in list Courses
sameOptions(SN1, SN2, Courses) :-
	allSharedOptions(SN1, SN2, Courses),
	length(Courses, 3).

