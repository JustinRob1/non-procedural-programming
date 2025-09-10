/* ---------------------------------------------------------

Question 1:

    setIntersect(+S1,+S2,-S3)

    Returns the list S3, where S1 and S2 are lists of atoms, and S3 is a list of
    atoms that are in both S1 and S2. We assume S1 is a list of distinct atoms.

    The predicate works by first checking if the first element of S1 is in S2, if it is, 
    then we add it to the list S3 and recursively call setIntersect. If the first element of S1 is not in S2, 
    then we recursively call setIntersect. The base case is when S1 is empty, which returns an empty list.

    Test Cases:
        setIntersect([a,b,c,d,e,g], [b,a,c,e,f,q], S) => S = [a,b,c,e]
        setIntersect([a,b,c,d,e,g], [f,q], S) => S = []
        setIntersect([a,b,c,d,e,g], [a,b,c,d,e,g,f,q], S) => S = [a,b,c,d,e,g]

--------------------------------------------------------- */

setIntersect([], _, []).
setIntersect([A|S1], S2, [A|S3]) :- member(A, S2), setIntersect(S1, S2, S3).
setIntersect([A|S1], S2, S3) :- \+ member(A, S2), setIntersect(S1, S2, S3).


/* ---------------------------------------------------------

Question 2:

    swap(+L, -R)

    Returns the list R where the first two atoms in L are swapped positions, so are the next two atoms, and so on. 
    If the number of atoms in L is odd, then the last atom is left as is. Note that L may be empty, in which case R should be empty as well.

    The predicate works by first checking if the list is empty, if it is, then we return an empty list. If the list is not empty, then we check if the
    list has only one element, if it does, then we return the list. If the list has more than one element, then we swap the first two elements and
    recursively call swap on the tail of the list. 

    Test Cases:
        swap([a,1,b,2], W) =>  W = [1,a,2,b]
        swap([a,1,b], W) => W = [1,a,b]
        swap([a], W) => W = [a]
        swap([], W) => W = []
        swap([a,b,c,d,e,f,g], W) => W = [b,a,d,c,f,e,g].
        swap([a, [1, 2], b, [3, 4], c], W) => W = [[1, 2], a, [3, 4], b, c]

--------------------------------------------------------- */

swap([], []).
swap([A], [A]).
swap([A,B|L], [B,A|L1]) :- swap(L, L1).

/* ---------------------------------------------------------

Question 3:

    filter(+L,+OP,+N,-L1)

    Returns the list L1, which is a flat list containing all the numbers in L such that the condition specified by OP and N is satisfied.
    OP is one of the following words: equal, greaterThan, and lessThan. N is a number. L is a possibly nested list of numbers.

    This predicate works by first checking if the list is empty, if it is, then we return an empty list. If the list is not empty, then we check if the
    first element of the list is not a nested list, if it is not, then we check if the condition is satisfied, if it is, then we add the element to the
    list L1 and recursively call filter on the tail of the list acoording to the condition and the number N. If the condition is not satisfied and the first 
    element is not a list then we do not add the element to the list L1 and recursively call filter on the tail of the list. If the first element is a list, 
    then we flatten the list by using the flatten predicate and recursively call filter on the flattened list.

    Test Cases:
        filter([3,4,[5,2],[1,7,3]],greaterThan,3,W) => W = [4,5,7]
        filter([3,4,[5,2],[1,7,3]],equal,3,W) => W = [3,3]
        filter([3,4,[5,2],[1,7,3]],lessThan,3,W) => W = [2,1]
            
--------------------------------------------------------- */

filter([], _, _, []).
filter([A|L], OP, N, L1) :- 
    is_list(A),
    filter(A, OP, N, L2),
    filter(L, OP, N, L3),
    append(L2, L3, L1).
filter([A|L], OP, N, [A|L1]) :-
    \+ is_list(A),
    (OP == equal, A =:= N;
     OP == greaterThan, A > N;
     OP == lessThan, A < N),
    filter(L, OP, N, L1).
filter([A|L], OP, N, L1) :-
    \+ is_list(A),
    filter(L, OP, N, L1).
filter(L, OP, N, L1) :-
    flatten(L, L2),
    filter(L2, OP, N, L1).

/* ---------------------------------------------------------

Question 4:

    countAll(+L,-N)

    Returns the list N, such that N is a list of pairs [a,n] representing that atom a occurs in L n times where L 
    is a flat list of atoms. These pairs appear in increasing order. If the atom is a number then I will assume that
    the numbers appear about the letters

    The predicate works by first sorting the list L, then calling pair which counts the number of times. After the predicate
    pair returns with the list of pairs, it sorts the list N2 by the second element of the pairs in
    increasing order, and returns N2. This uses the built-in predicate sort/4.

    The helper predicate pair works by recursively counting the number of times the first element of the list appears in the list 
    by using the count predicate. After the count predicate returns the number of times the first element appears in the list,
    we add the pair of the element and the count to the list N2 and recursively call pair on the tail of the list. 

    The helper predicate count works by recursively checking if the first element of the list is equal to the element we are looking for,
    if it is, then we increment the count and recursively call count on the tail of the list. If the first element of the list is not equal
    to the element we are looking for, then we do not increment the count and recursively call count on the tail of the list.

    Test Cases:
        countAll([a,b,e,c,c,b],N) => N = [[a,1],[e,1],[b,2],[c 2]]
        countAll([a,b,e,c,c,b,a,a],N) => N = [[e,1],[b,2],[c 2],[a,3]]
        countAll([], N) => N = []
        countAll([a,1,a,1,a,1,a,1,a,1], N) => N = [[1,5],[a,5]]

    Sorting Algorithm Source: https://www.swi-prolog.org/pldoc/doc_for?object=sort/4
            
--------------------------------------------------------- */

countAll(L, N) :-
    sort(L, N1),
    pair(N1, L, N2),
    sort(2, @=<, N2, N).

pair([], _, []).
pair([A|L1], L, [[A, N]|L2]) :-
    count(A, L, N),
    pair(L1, L, L2).

count(_, [], 0).
count(A, [A|L1], N) :- 
    count(A, L1, M),
    N is M + 1.
count(A, [B|L1], N) :-
    A \= B,
    count(A, L1, N).

/* ---------------------------------------------------------

Question 5:
    
    sub(+L,+S,-L1)

    Returns the list L1, such that L1 is the result of replacing all occurrences of xi in L with ei. S is a list of pairs in the 
    form [[x1,e1],...,[xn,en]] and L is a possibly nested list of atoms. Note: S is intended as a substitution. In this case, Xi's are distinct, and they do not occur in ei's. I will assume that any expression in ei are not evaluated and are substituted as is.

    The predicate works by first checking if the list is empty, if it is, then we return an empty list. The second clause
    checks if first element is a non-empty list. If it is then we subsitute the first element A with the second element B
    in the result list S. It then recursively applies the same substitution process to the remaining elements in the input list 
    L1 and puts the results in the output list L2. The third clause checks whether A is in the substitution list S. If it is, 
    it returns the corresponding value B. Else the element A is not in the substitution list S, A is left unchanged.

    Test Cases
        sub([a,[a,d],[e,a]],[[a,2]],L) => L = [2,[2,d],[e,2]].
        sub([a,[a,d],[e,a]],[[a,2],[d,3]],L) => L = [2,[2,3],[e,2]].
        sub([a,b,c,d,a], [[a, 1+2]], L). => L = [1+2, b, c, d, 1+2].

--------------------------------------------------------- */

sub([], _, []).
sub([A|L1], S, [B|L2]) :- sub(A, S, B), sub(L1, S, L2).
sub(A, S, B) :- member([A,B], S), !.
sub(A, _, A).

/* ---------------------------------------------------------

Question 6:
    
    clique(?L) 
    
    Returns the list L, such that L is a clique in the graph defined by the facts edge/2 and node/1. 
    A clique is a subset of nodes such that every pair of nodes in the clique are connected by an edge. 
    The order in which your predicate generates cliques does not matter. Findall(L, clique(L), Cliques) will 
    unify Cliques with a list containing all cliques

    The predicate clique takes a list L and checks if it is a clique. It first gets all the nodes in the graph by calling findall
    and then calls subsetClique to get all the subsets of that nodes in the graph. It then checks if the subset is a clique by calling
    checkClique. If it is a clique, then it returns the subset L. If it is not a clique, then it fails and tries the next subset of 
    the nodes in the graph.

    The helper predicate subsetClique takes a list L and a list L1 and checks if L is a subset of L1. It first checks if L is empty, if it is,
    then it returns true. The second clause checks if the first element of L is in L1. If it is, then it recursively calls subsetClique on the
    tail of L and L1. If the first element of L is not in L1, then it returns false.

    The helper predicate checkClique takes a list L and checks if it is a clique. It first checks if L is empty, if it is, then it returns true.
    The second clause checks if the first element of L is connected to every other element in L. If it is, then it recursively calls checkClique
    on the tail of L. If the first element of L is not connected to every other element in L, then it returns false.

    Test Cases
    First define the facts for the graph
        ?- [user].
        |: edge(a,b).
        |: edge(b,c).
        |: edge(c,a).
        |: node(a).
        |: node(b).
        |: node(c).
        |: ^D
        true.

        ?- clique(L).
        L = [a, b, c] ;
        L = [a, b] ;
        L = [a, c] ;
        L = [a] ;
        L = [b, c] ;
        L = [b] ;
        L = [c] ;
        L = [].

        ?- clique([a, b]).
        true ;
        false.

        ?- clique([f]).
        false.

--------------------------------------------------------- */

clique(L) :-
    findall(N, node(N), L1),
    subsetClique(L, L1),
    checkClique(L).

subsetClique([], []).
subsetClique([A|L1], [A|L2]) :- subsetClique(L1, L2).
subsetClique(L1, [_|L2]) :- subsetClique(L1, L2).

checkClique([]).
checkClique([_]).
checkClique([A, B|L1]) :- edge(A, B), checkClique([B|L1]).

/* ---------------------------------------------------------

Question 7:
    
    convert(+Term,-Result)

    Returns the list Result, such that Result is the result of converting the list Term.
    Term is a list (possibly empty) of single letters representing a string with the convention:
    e represents an empty space, q represents a single quote Result holds the same as term except that: anything between 
    two matching q's is not changed;any e's outside of a pair of matching q's are removed; any letter outside a pair of 
    matching q's is changed to the letter w; an unmatched q will be left as is. Any string with an odd number of occurrences 
    of q has the last occurrence of q unmatched; all the preceding ones are matched as the first and second
    form a pair, the 3rd and the 4th form the next pair, and so on.

    The predicate convert works by first checking if the input list is empty, if it is, then it returns an empty list. If the input list 
    starts with a q and the rest of the list contains a matching closing q then the output list starts with a q and we called copy 
    to copy the contents between the quotes into the output list. If the input list starts with a q but there is no matching closing q, 
    then the q should be copied to the output list, and the rest of the input list should be recursively converted to the output list.
    If the input list starts with an e, then we check if the rest of the list contains a matching closing q. If it does, then we call
    convert on the rest of the list. If it does not, then we call convert on the rest of the list and add the result to the output list.
    If the input list starts with a letter other than e or q, then we check if the rest of the list contains a matching closing q. If it does, 
    then we call convert on the rest of the list and add the letter w to the output list. If it does not, then we call convert on the rest of
    the list and add the letter w to the output list. 

    The helper copy predicate copies the contents of the input list between the two matchin letter q's

    The helper match predicate works by checking if there are two matching letter q in the input list. 

    convert([], []).: This rule says that an empty input list converts to an empty output list.

    Test Cases:
        convert([e,e,a,e,b,e],R) => R = [w,w]
        convert([e,q,a,b,e,e],R) => R = [q, w,w]
        convert([e,a,e,e],R) => R = [w]
        convert([e,q,a,e,b,q,e,a,e],R) => [q,a,e,b,q,w]
        convert([a,q,e,l,q,r,e,q,b,e],R) => R = [w,q,e,l,q,w,q,w]
        convert([q,e,q,b,q,e,l,q,a,e],R) => R = [q,e,q,w,q,e,l,q,w] 
          
--------------------------------------------------------- */

convert(Term, Result) :-
    convert_helper(Term, Result, []).

% base case: empty output list
convert_helper(_, [], _) :- !.

% case 1: q is not found in the list, replace non-e letters with w, remove e's
convert_helper([X|Xs], [W|Ws], _) :-
    X \= q,
    X \= e,
    !,
    W = w,
    convert_helper(Xs, Ws, []).

convert_helper([e|Xs], Ws, Qs) :-
    convert_helper(Xs, Ws, Qs).

% case 2: q is found but unmatched, replace non-e letters with w, remove e's
convert_helper([q|Xs], Ws, []) :-
    !,
    convert_helper(Xs, Ws, [q]).

convert_helper([X|Xs], [W|Ws], Qs) :-
    X \= q,
    !,
    W = w,
    convert_helper(Xs, Ws, Qs).

convert_helper([q|Xs], R, [q|Qs]) :-
    % find the next matching q
    find_matching_q(Xs, Qs, Rest),
    % copy the letters between the matching q's
    copy_between_q(Xs, Qs, Copied),
    % continue with the rest of the list
    convert_helper(Rest, RestResult, []),
    append(Copied, RestResult, R).

% helper to find the next matching q
find_matching_q([q|Xs], Qs, Xs) :-
    % if the number of q's seen so far is odd, then this is the matching q
    length(Qs, Length),
    Odd is Length mod 2,
    Odd = 1,
    !.
find_matching_q([q|Xs], Qs, R) :-
    % if the number of q's seen so far is even, then this is not the matching q
    length(Qs, Length),
    Odd is Length mod 2,
    Odd = 0,
    % continue searching for the next matching q
    find_matching_q(Xs, [q|Qs], R).
find_matching_q([e|Xs], Qs, R) :-
    find_matching_q(Xs, Qs, R).
find_matching_q([q|Xs], Qs, R) :-
    % check if the remaining list ends with a q
    append(_, [q], Xs),
    length(Qs, Length),
    Odd is Length mod 2,
    Odd = 1,
    % if the number of q's seen so far is odd, then this is the matching q
    !,
    R = Xs.
find_matching_q([_|Xs], Qs, R) :-
    find_matching_q(Xs, Qs, R).

copy_between_q([], _, []) :- !.

copy_between_q([q|Xs], [q|Qs], [q|Copied]) :-
    % if the number of q's seen so far is odd, then this is the matching q
    length(Qs, Length),
    Odd is Length mod 2,
    Odd = 1,
    !,
    copy_between_q(Xs, [], Copied).

copy_between_q([q|Xs], Qs, [q|Copied]) :-
    % if the number of q's seen so far is even, then this is not the matching q
    length(Qs, Length),
    Odd is Length mod 2,
    Odd = 0,
    % continue copying letters between the matching q's
    copy_between_q(Xs, [q|Qs], Copied).

copy_between_q([e|Xs], Qs, Copied) :-
    copy_between_q(Xs, Qs, Copied).

copy_between_q([X|Xs], Qs, [X|Copied]) :-
    copy_between_q(Xs, Qs, Copied).






