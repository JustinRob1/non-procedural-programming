/* ---------------------------------------------------------

Question 1 (a):

    We have a database of student marks, in the form of a relation with attribute names
    c325(Semester, Name, as1, as2, as3, as4, midterm, final)
    There are some facts about the setup of course components, in the form setup(Semester,Type,Max,Percentage)
    where Type is one of {as1,as2,as3,as4,midterm,final}, Max is the maximum marks for Type and percentage is 
    the weight of Type in the course.


    query1(+Semester, +Name, -Total)

    Given a semester and a student name, Total is bound to the total mark, 
    in terms of percentage out of 100, of the student for that semester.

    The predicate works by first calculating the total marks for each component, 
    then computing the weighted sum of marks for each component, and finally
    computing the total marks.

    Test Cases: 
        ?- query1(fall_2021, kim, X).
        X = 81.53333333333335 ?;
        false

--------------------------------------------------------- */

query1(Semester, Name, Total) :-
    c325(Semester, Name, AS1, AS2, AS3, AS4, Midterm, Final),
    setup(Semester, as1, MaxAS1, PercentAS1),
    setup(Semester, as2, MaxAS2, PercentAS2),
    setup(Semester, as3, MaxAS3, PercentAS3),
    setup(Semester, as4, MaxAS4, PercentAS4),
    setup(Semester, midterm, MaxMidterm, PercentMidterm),
    setup(Semester, final, MaxFinal, PercentFinal),
    WAS1 is (AS1/MaxAS1)*PercentAS1,
    WAS2 is (AS2/MaxAS2)*PercentAS2,
    WAS3 is (AS3/MaxAS3)*PercentAS3,
    WAS4 is (AS4/MaxAS4)*PercentAS4,
    WMidterm is (Midterm/MaxMidterm)*PercentMidterm,
    WFinal is (Final/MaxFinal)*PercentFinal,
    Total is (WAS1 + WAS2 + WAS3 + WAS4 + WMidterm + WFinal) * 100.

/* ---------------------------------------------------------

Question 1 (b):

    query2(+Semester, -L).

    Given a semester, this predicate finds all students whose final exam shows an improvement over the midterm, 
    meaning that the percentage obtained from the final is (strictly) better than that of the midterm.

    The predicate works by first finding all students whose final exam shows an improvement over the midterm, 
    then converting the list of names to a set.

    Test case:
        ?- query2(fall_2021, X).
        X = [aperf, ...] 

--------------------------------------------------------- */

query2(Semester, L) :-
    findall(Name, (c325(Semester, Name, _, _, _, _, Midterm, Final),
                   setup(Semester, midterm, MaxMidterm, _),
                   setup(Semester, final, MaxFinal, _),
                   Midterm < Final,
                   PercentMidterm is (Midterm/MaxMidterm)*100,
                   PercentFinal is (Final/MaxFinal)*100,
                   PercentFinal > PercentMidterm),
            Names),
    list_to_set(Names, L).

/* ---------------------------------------------------------

Question 1 (c):

    query3(+Semester,+Name,+Type,+NewMark)

    Updates the record of Name for Semester where Type gets NewMark. 
    If the record is not in the database, print the message "record not found".

    This predicate works by first checking if the record exists in the database, 
    then updating the record if it exists.

    Test cases:
        ?- query3(fall_2021, kim, final, 87).
        true

        ?- query3(fall_2014, jim, as1, 53).
        record not found
        true

--------------------------------------------------------- */

query3(Semester, Name, Type, NewMark) :-
    c325(Semester, Name, AS1, AS2, AS3, AS4, Midterm, Final),
    setup(Semester, Type, Max, _),
    (Type = as1, OldMark is AS1;
     Type = as2, OldMark is AS2;
     Type = as3, OldMark is AS3;
     Type = as4, OldMark is AS4;
     Type = midterm, OldMark is Midterm;
     Type = final, OldMark is Final),
    retract(c325(Semester, Name, AS1, AS2, AS3, AS4, Midterm, Final)),
    NewMark =< Max,
    NewMark >= 0,
    (Type = as1, assert(c325(Semester, Name, NewMark, AS2, AS3, AS4, Midterm, Final));
     Type = as2, assert(c325(Semester, Name, AS1, NewMark, AS3, AS4, Midterm, Final));
     Type = as3, assert(c325(Semester, Name, AS1, AS2, NewMark, AS4, Midterm, Final));
     Type = as4, assert(c325(Semester, Name, AS1, AS2, AS3, NewMark, Midterm, Final));
     Type = midterm, assert(c325(Semester, Name, AS1, AS2, AS3, AS4, NewMark, Final));
     Type = final, assert(c325(Semester, Name, AS1, AS2, AS3, AS4, Midterm, NewMark))),
    !. 

query3(Semester, Name, Type, NewMark) :-
    write('record not found').

/* ---------------------------------------------------------

Question 2:

    encrypt(W1,W2,W3)

    Assigns the letters in the words with distinct digits so that the addition of the values of W1 and W2 equals that of W3. 
    Assumes W1 and W2 are of the same length. If the length of W1 and W2 is N, then the length of W3 is either N or N+1. 
    The leading bit in any word cannot be assigned zero.

    The predicate works by first finding the set of letters in W1, W2, and W3, such that the sum of the digits of W1 and W2
    equals the digits of W3. Then, it checks that the leading letter in W1, W2, and W3 is not zero. 
    Finally, it checks that all the letters in W1, W2, and W3 are distinct and prints the solution.

    Test cases:
        encrypt([S,E,N,D], [M,O,R,E], [M,O,N,E,Y]).
        S = 9,
        E = 5,
        N = 6,
        D = 7,
        M = 1,
        O = 0,
        R = 8,
        Y = 2.

--------------------------------------------------------- */

:- use_module(library(clpfd)).

encrypt(W1,W2,W3) :- 
    length(W1,N),           
    length(W3,N1),   
    append(W1,W2,W),
    append(W,W3,L),
    list_to_set(L,Letters),     
    [LeadLetter1|_] = W1,   
    [LeadLetter2|_] = W2,
    [LeadLetter3|_] = W3,
    !,                      
    Letters ins 0..9,
    all_different(Letters),
    LeadLetter1 #\= 0,
    LeadLetter2 #\= 0,
    LeadLetter3 #\= 0,
    sum(W1,#=,(N1+1)*LeadLetter1+SumW1),
    sum(W2,#=,(N1+1)*LeadLetter2+SumW2),
    sum(W3,#=,(N1+1)*LeadLetter3+SumW3),
    SumW3 #= SumW1 + SumW2,
    label(Letters).

/* ---------------------------------------------------------

Question 3:

    grid(N, Rows)

    Creates a 9x9 grid of variables. The grid is represented as a list of rows.

    The predicate works by first creating a list of lists of variables, where each list represents a row. 
    Then, it creates a list of lists of variables, where each list represents a column. 
    Finally, it creates a list of lists of variables, where each list represents a 3x3 box.


    xtranspose(+Rows, -Columns)

    Transposes a 9x9 grid of variables. The grid is represented as a list of rows.

    The predicate works by recursively picking the first element of each list and creating a new list with them by using
    the predicate. 
    This list is then added to the result and the process is repeated with the rest of the lists. Finally we return
    the list of lists where each element is a column of the original list.


    xall-distinct(+Rows)

    Ensures that all variables in a 9x9 grid of variables get distinct values.

    The predicate works by recursively apply all_distinct to each list in the list of lists.


    split_list(L, first, rest)

    The utility predicate takes a list of lists as input and returns two lists, first contains the first element of each of the input lists, 
    and rest contains the remaining elements of each of the input lists.

    It works by recursively picking the first element of each list and creating a new list with them by using the predicate.
    This list is then added to the result and the process is repeated with the rest of the lists. Finally we return
    the list of lists where each element is a column of the original list.

    Test cases:
        ?- t(Rows).
            [1,5,6,8,9,4,3,2,7]
            [9,2,8,7,3,1,4,5,6]
            [4,7,3,2,6,5,9,1,8]
            [3,6,2,4,1,7,8,9,5]
            [7,8,9,3,5,2,6,4,1]
            [5,1,4,9,8,6,2,7,3]
            [8,3,1,5,4,9,7,6,2]
            [6,9,7,1,2,3,5,8,4]
            [2,4,5,6,7,8,1,3,9]
            Rows = [[1, 5, 6, 8, 9, 4, 3, 2|...], [9, 2, 8, 7, 3, 1, 4|...], [4, 7, 3, 2, 6, 5|...], 
            [3, 6, 2, 4, 1|...], [7, 8, 9, 3|...], [5, 1, 4|...], [8, 3|...], [6|...], [...|...]].

--------------------------------------------------------- */

:- use_module(library(clpfd)).

grid(N, Rows) :-
    length(Rows, N),
    maplist(same_length(Rows), Rows).

xtranspose([], []).
xtranspose([R|Rs], Cs) :-
    xtranspose(R, [R|Rs], Cs).

xtranspose([], _, []).
xtranspose([_|Rs], Es, [Cs|Css]) :-
    split_list(Es, Cs, Es1),
    xtranspose(Rs, Es1, Css).

xall_distinct([]).
xall_distinct([L|Ls]) :-
    all_distinct(L),
    xall_distinct(Ls).

split_list([], [], []).
split_list([[R|Cs0]|Css0], [R|Rs], [Cs0|Css]) :-
    split_list(Css0, Rs, Css).

sudoku(Rows) :-
    grid(9, Rows),
        % Rows now is a 9x9 grid of variables
    append(Rows, Vs),
        % Vs is a list of all 9*9 variables in Rows
    Vs ins 1..9,
    xall_distinct(Rows),
        % Variables of each row get distinct values
    xtranspose(Rows, Columns),
        % get the columns of 9x9 grid
    xall_distinct(Columns),
    Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
        % need references to rows
    blocks(As, Bs, Cs),
        % deal with three rows at a time
    blocks(Ds, Es, Fs),
    blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
    blocks(Ns1, Ns2, Ns3).

problem(P) :-
    P = [[1,_,_,8,_,4,_,_,_],
	 [_,2,_,_,_,_,4,5,6],
	 [_,_,3,2,_,5,_,_,_],
	 [_,_,_,4,_,_,8,_,5],
	 [7,8,9,_,5,_,_,_,_],
	 [_,_,_,_,_,6,2,_,3],
	 [8,_,1,_,_,_,7,_,_],
	 [_,_,_,1,2,3,_,8,_],
	 [2,_,5,_,_,_,_,_,9]].

t(Rows) :-
    problem(Rows),
    sudoku(Rows),
    maplist(labeling([ff]), Rows),
    maplist(writeln, Rows).

/* ---------------------------------------------------------

Question 4:
    Given facts of the form:
        paper(ID, Co-author1, Co-author2, Subject)
        reviewer(Name, Subject1, Subject2)
    where each paper is identified by its id number, co-authors (for simplicity assume no more than two; for single-authored papers, 
    Co-author2 is filled with xxx), and subject area, and each reviewer is identified by name and two subject areas of expertise.


    assign(W1,W2).

    Returns two lists W1 and W2, containing the names of the reviewers assigned to each paper.

    The predicate works by first finding all the paper ids and reviewer names, and then creating two lists of the same length as the number of papers.
    These lists are then passed to the assign_paper predicate, which recursively assigns papers to reviewers. The assign_paper predicate first checks
    that the two reviewers are not the same person, and that they are not both experts in the same subject area. It then checks that the paper is
    in the subject area of one of the reviewers. If all these conditions are met, the paper is assigned to the reviewers, and the lists of papers
    assigned to each reviewer are updated. The assign_paper predicate then calls itself recursively to assign the next paper. If any of the conditions
    are not met, the predicate fails, and the next possible assignment is tried.

    The workload predicate then checks that the workload of each reviewer is less than or equal to the maximum workload.


    Test cases:
    paper(1,lily,xxx,ai).
    paper(2,peter,john,database).
    paper(3,ann,xxx,theory).
    paper(4,ken,lily,network).
    paper(5,kris,xxx,games).

    reviewer(lily,theory,network).
    reviewer(john,ai,theory).
    reviewer(peter,database,network).
    reviewer(ann,theory,network).
    reviewer(kris,theory,games).
    reviewer(ken,database,games).
    reviewer(bill,database,ai).
    reviewer(jim,theory,games).

    workLoadAtMost(2).

    ?- assign(W1,W2).
    W1 = [lily, peter, ann, ken, kris],
    W2 = [john, john, lily, bill, jim] ;
            
--------------------------------------------------------- */

assign(W1,W2) :-
    findall(PID,paper(PID,_,_,_),PIDs),
    findall(RName,reviewer(RName,_,_),RNames),
    length(PIDs,N),
    length(RNames,M),
    length(W1,N),
    length(W2,N),
    assign_papers(PIDs,W1,W2,[],[]),
    workload(W1,W2,M).

assign_papers([],[],[],_,_).
assign_papers([PID|RestPIDs],[Reviewer1|RestW1],[Reviewer2|RestW2],PapersAssigned1,PapersAssigned2) :-
    reviewer(Reviewer1,S1,_),
    reviewer(Reviewer2,S2,_),
    S1 \= S2,
    paper(PID,_,_,Subject),
    (S1 = Subject ; S2 = Subject),
    \+ member(PID,PapersAssigned1),
    \+ member(PID,PapersAssigned2),
    append([PID],PapersAssigned1,NewPapersAssigned1),
    append([PID],PapersAssigned2,NewPapersAssigned2),
    assign_papers(RestPIDs,RestW1,RestW2,NewPapersAssigned1,NewPapersAssigned2).

workload([],[],_).
workload([Reviewer|RestW1],[_,_|RestW2],M) :-
    findall(PID,(member(PID,[_,_|RestW2]),member(Reviewer,[_,_|RestW1])),PapersAssigned),
    length(PapersAssigned,K),
    K =< M,
    workload(RestW1,RestW2,M).
