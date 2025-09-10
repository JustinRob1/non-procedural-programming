
#| 
    This function acts like a FUN interpreter. It takes a program P and an expression E and returns the result of evaluating E with respect to P.
    For primitive functions we will use the call pattern (fl-interp Exp nil)
    For user defined function we will use the call pattern (fl-interp Exp P) where P is a list of the form ((f (x1 ... xn) Exp) ... (g (y1 ... ym) Exp))
    where f and g are function names, xi and yi are variable names, and Exp is an expression.

    The function works by first checking if E is a primitive function. If it is, then it is evaluated. If it is not, then it is checked if it is a user defined function. 
    If it is, then the arguments are evaluated and the function is applied to the evaluated arguments. If it is not, then it is returned as if it is quoted in lisp.

    Primative Test Cases:
    (fl-interp '(+ 1 2) nil) => 3
    (fl-interp '(rest (1 2 (3))) nil) => (2 (3))
    (fl-interp '(if (> 1 2) 3 4) nil) => 4
    (fl-interp '(cons (first (1 2 3)) (cons a nil)) nil) => (1 a)
    (fl-interp '(equal (1 2 3) (1 2 3)) nil) => T

    User Defined Function Test Cases:
    (fl-interp '(f 1 2) '((f (x y) (+ x y)))) => 3
    (fl-interp '(f (f 2)) '((f (X) = (* X X))))=> 16
    (fl-interp '(a (+ 1 2)) '((a (X) = (+ X 1)))) => 4
    (fl-interp '(last (s u p)) '((last (x) = (if (null (rest x)) (first x) (last (rest x)))))) => p
    (fl-interp '(factorial 4) '((factorial (x) = (if (= x 1) 1 (* x (factorial (- x 1))))))) => 24
|#

(defun fl-interp (E P)
    (cond 
    ; If E is a number then return the number
    ; Else return the value of the variable
    ; Example: (FL-INTERP X ((X (3)) (A (X) = (+ X 1)))) => 3
    ((atom E) (if (numberp E) E (cadar P)))
    ; If E is a list then evaluate the function and the arguments
    (t (let ((f (car E)) (arg (cdr E))) 
        ; Handle the built-in functions
	    (cond 
        ((eq f 'if) (if (fl-interp (car arg) P) (fl-interp (cadr arg) P) (fl-interp (caddr arg) P)))
        ((eq f 'null) (null (fl-interp (car arg) P)))
        ((eq f 'atom) (atom (fl-interp (car arg) P)))
        ((eq f 'eq) (eq (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
        ((eq f 'cons) (cons (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
        ((eq f 'equal) (equal (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
        ((eq f 'car) (car (fl-interp (car arg) P)))
        ((eq f 'cdr) (cdr (fl-interp (car arg) P)))
        ((eq f 'first) (car (fl-interp (car arg) P)))
        ((eq f 'rest) (cdr (fl-interp (car arg) P)))
        ((eq f 'number) (numberp (fl-interp (car arg) P)))
        ((eq f '+) (+ (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
        ((eq f '-) (- (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
        ((eq f '*) (* (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
        ((eq f '>) (> (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
        ((eq f '<) (< (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
        ((eq f '=) (= (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
        ((eq f 'and) (and (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
        ((eq f 'or) (or (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
        ((eq f 'not) (not (fl-interp (car arg) P)))

        ; if f is a user-defined function,
        ;    then evaluate the arguments
        ;         and apply f to the evaluated arguments
        ;             (applicative order reduction)
        ((not (null P))
        (let* ((params (cadr (car P)))
                (body (caddr (cdar P)))
                (bindings (mapcar #'list params (mapcar #'(lambda (a) (fl-interp a P)) (if (atom arg) (list arg) arg)))))
            (fl-interp body (append bindings P))))
        ; otherwise f is undefined (not intended to be a function),
        ; the E is returned, as if it is quoted in lisp 
        (t E))))))

    
(print (if (eq (fl-interp '(+ (* 2 2) (* 2 (- (+ 2 (+ 1 (- 7 4))) 2))) nil) '12) 'P19-OK 'P19-error))
; (and (symbolp f) (assoc f P))