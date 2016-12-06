#lang racket

#|
Lab 1:  Introduction to Racket and unit testing in Racket
CS 270 (Math Foundations of CS)
In this part of the lab, you will learn how to run unit tests in
Racket and will review the sorting examples shown in the second
lecture “Specifications, Testing and Formal Verification”.  

Go to the course webpage and get the file lab1.rkt in the Labs section.
Open the file in a text editor and quickly review the instructions in
the file.  This file is intended to be loaded into DrRacket
(in fact, you can double click on it and it should open in DrRacket –
alternatively once DrRacket is open you can load the file using Open
in the File menu.  Complete the questions described in the file.
Make sure you save all of the modifications you make.  The changes
should be made in the Definitions window and you can save them using
“Save Definitions” under the File menu.
|#

(require racket/contract)
(require rackunit)
(require rackunit/text-ui)

; Predicate to determine if the input is a list of integers.
; Input:   L is an arbitrary Racket object.
; Output:  boolean which is true if L is a list of integers and
;          false otherwise.
; Note:  The library function list? is a predicate to detect lists.
;        A predicate to detect lists of a given type can be obtained
;        from (flat-contract-predicate integer?)
(define (intlist? L)
  (if (null? L)
      #t
      (and (pair? L) (integer? (first L)) (intlist? (rest L)))))

; A predicate to determine if a list of integers is sorted.
; Input:  L a list of integers
; Output: boolean true of L is sorted false otherwise
(define (sorted? L)
   (cond
   [(null? L) #t]
   [(equal? (length L) 1) #t]
   [else (and (<= (first L) (second L)) 
         (sorted? (rest L)))]
   ))


; A predicate to determine if one list is permutation of another
; Input:  P and L are lists of integers
; Output: boolean which is true if L is a permutation of P
;         false otherwise.
(define (permutation? P L)
  (if (null? P)
      (null? L)
      (and (member (car P) L)
           (permutation? (remove (first P) P) (remove (first P) L)))))

; A function to insert an integer into a sorted list
; Input:  (and (integer? x) (intlist? L) (sorted? L))
; Output: (let (M (insert x L))
;              (and (intlist? M) (sorted? M) (permutation? M (cons x L))))
(define (insert x L)
  (cond
   [(null? L) (list x)]
   [(<= x (first L)) (cons x L)]
   [else (cons (first L) (insert x (rest L)))]
   ))

; A function to sort a list of integers using insertion sort
; Input:  (intlist? L)
; Output: (let (M (insertionsort L))
;              (and (intlist? M) (sorted? M) (permutation? M L)))
(define (insertionsort L)
  (if (null? L)
    null
    (insert (first L) (insertionsort (cdr L)))))

; Unit tests - tests all possible lists of three elements.
(define-test-suite insertionsort3-suite
  (check-equal? 
    (insertionsort '(1 2 3)) '(1 2 3))

  (check-equal? 
    (insertionsort '(1 3 2)) '(1 2 3))

  (check-equal? 
    (insertionsort '(2 1 3)) '(1 2 3))

  (check-equal? 
    (insertionsort '(2 3 1)) '(1 2 3))

  (check-equal? 
    (insertionsort '(3 1 2)) '(1 2 3))

  (check-equal? 
    (insertionsort '(3 2 1)) '(1 2 3))

  (check-equal? 
    (insertionsort '(1 1 2)) '(1 1 2))

  (check-equal? 
    (insertionsort '(1 2 1)) '(1 1 2))

  (check-equal? 
    (insertionsort '(2 1 1)) '(1 1 2))

  (check-equal? 
    (insertionsort '(1 1 1)) '(1 1 1))
)
(run-tests insertionsort3-suite 'verbose)

; Question 1
; Unit tests - test all possible insertions into a sorted list of
; length 2.  You may assume the sorted list is (1 3) and you need
; 3 cases that cover the different possible locations where x can be
; inserted.

(define-test-suite insert3-suite
  (check-equal? 
    (insert 0 '(1 3)) '(0 1 3))
  
  (check-equal? 
    (insert 2 '(1 3)) '(1 2 3))

  (check-equal? 
    (insert 4 '(1 3)) '(1 3 4))
)

(run-tests insert3-suite 'verbose)

; Question 2
; Unit tests - Write unit tests to test the base cases for insert and
; insertionsort.

(define-test-suite basecases-suite
 
  (check-equal? 
    (insert 0 '()) '(0))
  
  (check-equal? 
    (insertionsort '()) '())
)

(run-tests basecases-suite 'verbose)



#|
   Question 4.
   Implement a function (sort3 L) which inputs a list of three integers
   and returns a list of the same 3 numbers in sorted order.  Your
   function should inline the comparisons using (if expr then else)
   and should not use recursion.  You should use the functions first, second
   and third to access the three elements of the list L.  Use the function
   list to construct the resulting list.  Verify the correctness of your
   sort3 using the same unit test that was applied to insertionsort.
|#

(define (sort3 L)
  (if (<= (second L) (third L))
      (if (<= (first L) (second L))
          (list (first L) (second L) (third L))
          (if (<= (first L) (third L))
              (list (second L) (first L) (third L))
              (list (second L) (third L) (first L))))
      (if (<= (first L) (third L))
          (list (first L) (third L) (second L))
          (if (<= (first L) (second L))
              (list (third L) (first L) (second L))
              (list (third L) (second L) (first L))))
      ))

; Unit tests - tests all possible lists of three elements.
(define-test-suite sort3-suite
  (check-equal? 
    (insertionsort '(1 2 3)) '(1 2 3))

  (check-equal? 
    (sort3 '(1 3 2)) '(1 2 3))

  (check-equal? 
    (sort3 '(2 1 3)) '(1 2 3))

  (check-equal? 
    (sort3 '(2 3 1)) '(1 2 3))

  (check-equal? 
    (sort3 '(3 1 2)) '(1 2 3))

  (check-equal? 
    (sort3 '(3 2 1)) '(1 2 3))

  (check-equal? 
    (sort3 '(1 1 2)) '(1 1 2))

  (check-equal? 
    (sort3 '(1 2 1)) '(1 1 2))

  (check-equal? 
    (sort3 '(2 1 1)) '(1 1 2))

  (check-equal? 
    (sort3 '(1 1 1)) '(1 1 1))
)
(run-tests sort3-suite 'verbose)