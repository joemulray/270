#lang racket

; Solution to CS 270 Lab 2.  Jeremy Johnson
; Note this file can be loaded and run in Dr. Racket
; After running the definitions you can experiment with them.

; Inputs:  f is a function of 1 input and x = [x1 ... xn] is a list.
; Output:  a list [f(x1) ... f(xn)]
; Note:  reimplementing library function map
(define (map1 f x)
  (if (null? x)
      '()
      (cons (f (first x)) (map1 f (rest x)))))

; Inputs:  f is a function of 2 inpus, an initial value init
;          and x = [x1 ... xn] is a list.
; Output:  init when x = null, otherwise
;          an element obtained combining the elements of x from left to right
;          (f x1 (f x2 (f x3  ... (f xn init)... )
; Note:  reimplementing library function foldr
(define (reduce f init x)
  (if (null? x)
      init
      (f (first x) (reduce f init (rest x)))))

; Inputs x a list of numbers
; Output a nonnegative number equal to the number of negative entries in x.
; Note the use of lambda to create an unnamed function.  This prevents having
; to define the function.  Also note that we could have implemented this
; directly with a recursive function, but here we are reusing map and reduce.

(define  (countnegative x)
  (reduce + 0 (map1 (lambda (y) (if (negative? y) 1 0))
                    x)))
; An alternative implementation of countnegative that uses the function
; charpred to create the function (characteristic function) that is input
; to map1.

; Input:  pred? is a predicate.
; Output: a function that returns 1 when pred? is true and 0 otherwise.
(define (charfun pred?)
  (lambda (x) (if (pred? x) 1 0)))

; Inputs x a list of numbers
; Output a nonnegative number equal to the number of negative entries in x.
(define (countnegative2 x)
  (reduce + 0 (map (charfun negative?) x)))

; The use of charfun suggest that we could pass a predicate to count rather
; than hard coding negative?

; Inputs x a list of numbers and a predicative pred?
; Output a nonnegative number = to the number of entries in x that satisfy pred?
(define (countif pred? x)
  (reduce + 0 (map (charfun pred?) x)))

; now (countnegative x) = (countif negative? x)

; Inputs: a positive integer n
; Outputs:  a list of strings of length n.  Each string represents a tiling
;           of a 2 x n rectangle with vertical "V" or two stacked horizontal
;           dominoes "HH".
; Note:  The algorithm recursively constructs all tilings of a 2 x (n-1)
;        rectangle and sticks a "V" in front to obtain a tiling of size 2 x n,
;        and recursively constructs all tilings of size 2 x (n-2) and sticks
;        a "HH" in front to obtain a tiling of size 2 x n.  GenDominoes then
;        returns the list which appends these two recursive constructions.

(define (GenDominoes n)
  (cond
    [(= n 1) '("V")]
    [(= n 2) '("VV" "HH")]
    [else 
      (append
       (map (lambda (x) (string-append "V" x))
              (GenDominoes (- n 1)))
        (map (lambda (x) (string-append "HH" x))
              (GenDominoes (- n 2))))]))

; In the above implementation note that the function mapped over the recursive
; calls to GenDominoes is obtained by fixing the first input to string-append
; to either "V" or "HH".  This is a special case of a more general construction
; called currying.  A curried version of a function is obtained by fixing
; one more more of the inputs.  Racket provides the function curry which can
; be used to construct curried versions of a function.  See GenDominoesV2
; for an example and also consult the online documentation.
; 
; ((curry string-append) "V") returns the function which appends "V" to a
; string and is mapped over (GenDominoes (- n 1))

(define (GenDominoesV2 n)
  (cond
    [(= n 1) '("V")]
    [(= n 2) '("VV" "HH")]
    [else 
      (append
       (map ((curry string-append) "V")
              (GenDominoesV2 (- n 1)))
       (map ((curry string-append) "HH")
              (GenDominoes (- n 2))))]))

; Examples
; > (GenDominoes 2)
; '("VV" "HH")
; > (GenDominoes 3)
; '("VVV" "VHH" "HHV")
; > (GenDominoes 4)
; '("VVVV" "VVHH" "VHHV" "HHVV" "HHHH")
;
; Note that the length of (GenDominoes n) is equal to the sum of the lengths
; of (GenDominoes (- n 1)) and (GenDominoes (- n 2)).  This is clear from
; the recursive construction.  Such numbers are called Fibonacci numbers.
; The sequence of lenghts can be obtained through the following
;
; > (map (compose length GenDominoes) (range 1 10))
; '(1 2 3 5 8 13 21 34 55)
;
; Note that use of compose which creates a function which first applies
; GenDominoes and then applies length to the result.
  