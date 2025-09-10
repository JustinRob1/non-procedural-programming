#| Question 1.
    This function returns the number of times that E appears in L. E is an atom and L may be nested. 

    The function works by first checking if L is null and if the first element of L is null. If both are true then return 0.
    Check to see if L is an atom and if E is equal to L then return 1, if not return 0. If E is nil, the current atom is nil but
    the rest of the list is not null add 1 and call occurs with the rest of the list. If the rest of L is null then return 1 since
    we have a single nil atom. Else return the sum of the recursive calls of occurs with the car and cdr of L since there is a nested
    list so we need to get the element inside the list

    Test Cases:
    (occurs 'a '(a (a b) ((c) a))) => 3
    (occurs 'a '(a (a (b)) b)) => 2
    (occurs 'a '(d (nil (b)) b)) => 0
    (occurs 'a '()) => 0
    (occurs 'nil '(nil)) => 1
|#

(defun occurs (E L)
    (cond ((and (null L) (null (car L))) 0)
    ((atom L) (if (equal E L) 1 0))
    ((and (null E) (and (null (car L)) (not (null (cdr L))))) (+ 1 (occurs E (cdr L))))
    ((and (null E) (and (null (car L)) (null (cdr L)))) 1)
    (t (+ (occurs E (car L)) (occurs E (cdr L)))))
)

#| Question 2.

    This function takes x which is a list with sublists nested to any depth and returns a list of atoms such that
    all the atoms appearing in x also appear in the result and in the same order. Any occurrence of NIL nested inside 
    the list x is treated as an atom. If x is empty, the result should be an empty list. 

    The function works by checking if the first element of x is an atom and will append the atom to a recursive call
    of xflatten with the rest of the list. If the first element of the list is not an atom then call xflatten by appending 
    the car of x to the cdr of x which will get the elements inside the nested list.

    Test Cases:
    (xflatten '(a (b c) d)) => (a b c d)
    (xflatten '((((a))))) => (a)
    (xflatten '(a (b c) (d ((e)) f))) => (a b c d e f)
    (xflatten '(a (b nil c) nil (d ((e)) f))) => (a b nil c nil d e f)
    (xflatten '()) => nil

|#

(defun xflatten (x) 
    (cond ((null x) nil)
    ((atom (car x)) (cons (car x) (xflatten(cdr x)))) 
    (t (xflatten (append (car x) (cdr x)))))
)

#| Question 3.

    This function takes x as a list of atoms and removes repeated ones in x. The order of the elements in the resulting list s
    preserve the order in the given list x. 

    Using the occurs function we can check if the first element of x is repeated in the rest of the list. If it is not repeated
    then append the first element of x to a recursive call of remove-duplicate with the rest of the list since we know that the 
    atom is unique in the list. If it is repeated then we can just call remove-duplicate with the rest of the list, not appending 
    the first element since it is a duplicate. Occurs will check for nil values so we know that any duplicate nil values will be removed.

    Test Cases:
    (remove-duplicate '(a b c a d b)) => (a b c d) or (c a d b)
    (remove-duplicate '(a b c a d b e c)) => expected output: (a b c d e)
    (remove-duplicate '(1 2 3 2 4 3 5)) => expected output: (1 2 3 4 5)
    (remove-duplicate '()) => expected output: ()
    di => expected output: (a b c nil)
|#

(defun remove-duplicate (x)
    (cond ((null x) nil)
    ((equal (occurs (car x) (cdr x)) 0) (cons (car x) (remove-duplicate(cdr x))))
    (t (remove-duplicate (cdr x))))
)

#| Question 4.

    This function mixes the elements of L1 and L2 into a single list, by choosing elements from L2 and L1 alternatingly. 
    If one list is shorter than the other, then all remaining elements from the longer list are appended to the end. nil 
    will be ignored but '(nil) is treated as an atom and will be mixed. Any nested lists will be appended as is.

    This function works by checking if L1 and L2 are null. If L1 is nil then we will return L2 since there are no more elements
    in L1 so the rest of the elements of L2 will be added to the end of the list. The same situation occurs if L2 is nil. 
    If both L1 and L2 are not nil then we will append the first element of L2 to the first element of L1 and then recursively
    call mix with the rest of L1 and L2.

    (mix '(a b c) '(d e f)) => (d a e b f c)
    (mix '(1 2 3) '(a)) => (a 1 2 3)
    (mix '((a) (b c)) '(d e f g h)) => (d (a) e (b c) f g h)
    (mix '(1 2 3) nil) => (1 2 3)
    (mix '(1 2 3) '(nil)) => (nil 1 2 3)
    (mix nil '(nil)) => nil
|#

(defun mix (L1 L2)
    (cond ((null L1) L2)
    ((null L2) L1)
    (t (cons (car L2) (cons (car L1) (mix (cdr L1) (cdr L2))))))
)


#| Question 4.

    This function returns a list of two sublists, the first one of which is the list of elements in L at even positions 
    and the second is the list of elements in L at odd positions. If L is empty, then a list of two empty lists is returned.               

    This function works by first returning a list of two nil values if L is null. If the rest of the list is null then we will
    return a the first element of L in a list with the nil value since this is the last element in the list L. Else the rest of L
    is not null so we will return a list of a list the second element of L and then recursively call split to add all the second elements
    to the first list. Similarly we will return  a list of the first element of L and then recursively call split to add all the first elements
    to the second list. A list of these two lists will be returned

    (split '(1 2 3 4 5 6)) => ((2 4 6) (1 3 5))
    (split '((a) (b c) (d e f) g h)) => (((b c) g) ((a) (d e f) h))
    (split '()) => (nil nil)
    (split '(a)) => (nil (a))
    (split '(a b)) => ((b) (a))
|#

(defun split (L)
    (cond ((null L) (list nil nil))
    ((null (cdr L)) (list nil (list (car L))))
    (t (list (cons (cadr L) (car (split (cddr L)))) (cons (car L) (cadr (split (cddr L)))))))
)

#| Question 5.

    This function that returns a list of all subsets of L.  The order of subsets in the resulting list is unimportant.

    This function works by calling the accumulator gen-subsets with an empty list and the list L so that the accumulator can 
    generate a list of all the subsets. The first gen-subsets call will return the list of all subsets of L and thus allsubsets
    will return the same list.

    (allsubsets nil) -> (nil)
    (allsubsets '(a)) -> (nil (a)) 
    (allsubsets '(a b)) -> (nil (a) (b) (a b))
    (allsubsets '(a b c)) -> (nil (a) (b) (c) (a b) (a c) (b c) (a b c))
|#

(defun allsubsets (L)
    (gen-subsets nil L)
)

#|
    The accumulator gen-subsets takes an empty list as its first parameter and the list L from allsubsets. The accumulator works by
    returning A as a list if L is null since all of the atoms in L have been added. Otherwise the accumulator will append the result of two
    recursive calls of gen-subsets. The first recursive call will eventually return A as a list when L is null as it will continually call
    gen-subsets with the rest of L. The second recursive call will have a list with the first element being the first element of L constructed
    with A and the rest of L. This will add another subset to the accumulator. These recursive calls continue until L is null and the accumulator 
    is returned as a list of all subsets of L.

|#

(defun gen-subsets (A L)
    (if (null L) (list A)
    (append (gen-subsets A (cdr L)) (gen-subsets (cons (car L) A) (cdr L))))
)

#| Question 6.

    This function replaces each occurrence of atom A in the list L with the expression E. 

    This function works by checking if the first element of L is a list. If it is a list then the function will construct a list with the
    result of calling substitute0 with the first element of L and then another call of substitute0 with rest of L. We perform this condition to
    check for nested lists and get the car of L in order to check inside the nested list since we need to compare atoms. If the first element of L 
    is not a list then it must be an atom so we check to see if it is equal to A. If it is equal to A then we will add E to a recursive call of substitute0 
    with the rest of the list L, replacing A with E in this case. Else the atom A is not equal to the atom we are checking so we add A to a recursive call 
    of substitute0 since the atom does not need to be replaced.

    (substitute0 'a 'b '(a (a 2) (1 2 a))) -> (b (b 2) (1 2 b))
    (substitute0 'a 'b '(1 2 3)) -> (1 2 3)
    (substitute0 'a '(1 2) '(a (b a) (c e) (a))) -> ((1 2) (B (1 2)) (C E) ((1 2)))
|#

(defun substitute0 (A E L)
    (cond ((null L) nil)
    ((not (atom (car L))) (cons (substitute0 A E (car L)) (substitute0 A E (cdr L))))
    ((equal A (car L)) (cons E (substitute0 A E (cdr L))))
    (t (cons (car L) (substitute0 A E (cdr L)))))
)


#| Question 7.

    This function returns a list of all web pages that can be reached from x (x should not be part of the result). 
    The order of the web pages in the resulting list is unimportant. x is a web page, L is a list of pairs representing linkage.
    A web page A is said to refer to another web page B iff A contains a (direct) link to B, and A and B are not the same web page 
    (i.e., a web page referring to itself doesn't count).

    This function works by calling the utility function remove0 on the result of the utility function loop-links. We call remove0 on the result
    of loop-links because loop-links will return a list that will contain x, but x should not be part of the result so we need to move it. 

    (reached 'google '((google shopify) (google aircanada) (amazon aircanada))) -> (SHOPIFY AIRCANADA)
    (reached 'google '((google shopify) (shopify amazon) (amazon google))) -> (SHOPIFY AMAZON)
    (reached 'google '((google shopify) (shopify amazon) (amazon indigo))) ->(SHOPIFY AMAZON INDIGO)
    (reached 'google '((google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google))) -> (SHOPIFY AIRCANADA DELTA)
    (reached 'shopify '((google amazon) (shopify google) (google shopify))) => (GOOGLE AMAZON)
|#


(defun reached (x L)
    (remove0 x (loop-links x L))
)

#| 
    This utility function checks if the element x is in the list L and removes x if so. It works by checking if x equals the first element
    of L and if it does then it will just return the rest of L, removing x from the process. Else if will add the element L to the result of
    recursively calling remove0 with the rest of the list.
|# 

(defun remove0 (x L)
    (cond ((null L) nil)
    ((equal x (car L)) (cdr L))
    (t (cons (car L) (remove0 x (cdr L)))))
)

#| 
    This utility function first determines if x is an atom (which will occur on the first iteration of the function) and if it is then it will
    it will just put x into a list so the function check-links can work properly. It will determine if x is equal to the result of calling check-links,
    and if they are equal that indicates that no new links have been found so we just x. If they are not equal then new links have been found so we must call 
    loop-links with x as the result of check-links in order to check for more connections for the new links that have been found. This enables the function to detect 
    if there are 'backwards' links that are not detected by the initial check-linkscall. If x is not an atom then it is a list so we determine if check-links is null. 
    If check-links is null then we return x since no new links have been found. Else then new links have been found so we must continue calling loop-links until no new links 
    have been foundLoop-links will continue iterating until no new links are found. Once no new links are found, the function will return a list of all the links that can be 
    reached from x and including x.
|# 

(defun loop-links (x L)
    (if (atom x) (if (null (remove-duplicate (check-links (list x) L (list x)))) (list x) (loop-links (remove-duplicate (check-links (list x) L '())) L))
    (if (null (remove-duplicate (check-links x L x))) x (loop-links (remove-duplicate (check-links x L x)) L)))
)

#| 
    This utility function iterates through the list L and determines for each link if in if there is an element in x that is equal to the first element of the link.
    If there is then it will return 1, indicating that the link is reachable from x. Else it will return 0. This adds any reachable links to the list A. If there are 
    no new reachable links then it simply checks the next link in the list L. The list A will be returned by this function.

|# 

(defun check-links (x L A)
    (cond ((null L) nil)
    ((equal (reachable x (car L)) 1) (if (equal (occurs (cadar L) A) 0) (append (append A (cdar L)) (check-links x (cdr L) (append A (cdar L)))) (check-links x (cdr L) A)))
    (t (check-links x (cdr L) A)))
)

#| 
    This utility function determines if any element in list x is equal to the first element of list l. If there is a match then 1 will be returned. If there is not a 
    match then the next element will be checked. 
|# 

(defun reachable (x L)
    (cond ((null x) nil)
    ((equal (car x) (car L)) 1)
    (t (reachable (cdr x) L)))
)

#| Question 7.

    The argument S is a list of atoms naming web pages, and L is a list of pairs representing linkage. Multiple links from (A,B) count as one for the 
    importance of web page B. The function returns a permutation of S such that the web pages are ordered by the most referenced web page is the first in 
    the list, and so on. If two web pages are equally important in terms of references, then it doesn't matter how they are ordered.

    The function works by first using remove-duplicate0 to remove all duplicate links in the list L since multiple links from (a, b) count 
    as one for the importance. Then it uses get-count that will return a list that contains a pair that contains the web page and the number of 
    times it is referenced. Then it uses mySort to sort the list in descending order and then finally calls first-link to get a list of all the 
    webpage names since we do not need to return the number of times it was referenced

    (rank '(google shopify aircanada amazon) '((google shopify) (google aircanada) (amazon aircanada))) -> (AIRCANADA SHOPIFY GOOGLE AMAZON)
    (rank '(google shopify amazon) '((google shopify) (shopify amazon) (amazon google))) -> (GOOGLE SHOPIFY AMAZON)
    (rank '(google shopify amazon indigo) '((google shopify) (shopify amazon) (amazon indigo))) -> (SHOPIFY AMAZON INDIGO GOOGLE)
    (rank '(google shopify aircanada amazon delta) '((google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google))) -> (AIRCANADA SHOPIFY DELTA GOOGLE AMAZON)

|#

(defun rank (S L)
    (first-link (mySort (get-count S (remove-duplicate0 L))))
)

#|
    This utility function uses the occurs function defined in Question 1. and constructs a list that counts the number 
    of times that car S occurs in the second link of L. The function calls second-link L that returns a list of all the 
    second links in L in order to count the number of times that car S occurs in the list. The function then constructs a 
    list of that result with a recursive call of get-count to get the count of each element in S
|# 

(defun get-count (S L)
    (if (null S) nil
    (cons (cons (car S) (list (occurs (car S) (second-link L)))) (get-count (cdr S) L)))
)

#|
    This utility function returns a list of all the first links in L by constructing a list of the first atom of the 
    first link in the list L and then recursively calling first-link to get the rest of the list to get all the 
    first links
|# 

(defun first-link (L)
    (if (null L) nil
    (cons (caar L) (first-link (cdr L))))
)

#|
    This utility function returns a list of all the second links in L by constructing a list of the seoncd atom of the 
    first link in the list L and then recursively calling second-link to get the rest of the list to get all the 
    second links. If the first atom of the link equals the second atom of the 
|# 

(defun second-link (L)
    (cond ((null L) nil)
    ((equal (caar L) (cadar L)) (second-link (cdr L)))
    (t (cons (cadar L) (second-link (cdr L)))))
)

#|
    This utility function sorts the list L in descending order by using the greaterThan function defined below and the lisp
    built-in sort function. This function was taken from the assignment description.
|# 

(defun mySort (L)
    (sort L 'greaterThan))

#|
    This utility function returns true if the second element of L1 is greater than the second element of L2 and false otherwise.
    This function was taken from the assignment description.
|# 

(defun greaterThan (L1 L2)
    (> (cadr L1) (cadr L2)))

#|
    This utility function is a retooled version of remove-duplicate from Question 3. It uses a retooled version of occurs0 to determine
    if the first link in the list L occurs in the rest of the list L. The function returns a list that has all duplicate links removed.
    If occurs returns 0 then the link is unique so construct a list with this link and recursively call remove-duplicate0 with the rest 
    of the list. Else the link does occur in the list again so recursively call remove-duplicate0 with the rest of the list, skipping over
    the link since it is a duplicate.
|# 

(defun remove-duplicate0 (x)
    (cond ((null x) nil)
    ((equal (occurs0 (car x) (cdr x)) 0) (cons (car x) (remove-duplicate0 (cdr x))))
    (t (remove-duplicate0 (cdr x))))
)

#|
    This utility function is a retooled version of occurs from Question 1. It returns the number of times that the link E occurs in the list L.
    The the link E equals the first link in the list L then add 1 to the result of the recursive call of occurs0 with the rest of the list. Otherwise 
    the two links are not equal so add 0 to the result of the recursive call of occurs0 with the rest of the list.
|# 

(defun occurs0 (E L)
    (cond ((null L) 0)
    ((if (equal E (car L)) (+ 1 (occurs0 E (cdr L)))))
    (t (+ 0 (occurs0 E (cdr L)))))
)
