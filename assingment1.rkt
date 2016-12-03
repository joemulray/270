#|
CS 270 Math Foundations of CS
Fall 2016-17
Instructor:  Profs. Jeremy Johnson and Mark Boady

Assignment 1
Introduction to Racket and boolean functions

In the first part of the assignment students will implement the boolean
functions not, and, or xor, implies, and iff using Racket's
if expression.  Note that Racket already implements these
Boolean functions using the names above.  Therefore we will
name our implementations noti, andi, ori, xori, impliesi and iffi
so as not to cause confusion.  Your implementation may not use
the builtin Boolean functions and must only use if expressions.
This exercise shows that all Boolean functions can be implemented
using if and hence they need not be primitive.

The functions noti, andi, ori, xori, impliesi and iffi are specified
informally in the specification comments and are also specified
using unit tests.  For each function you need to put in the missing
code and make sure it passes the corresponding unit tests.  Function
definitions and the corresponding unit tests are commented out. As
you implement each function, uncomment the definition, fill in the
implementation, and uncomment the corresponding unit test to test
your implementation.

After you have implemented noti, andi, xori, impliesi and iffi and
the have passed all of their associated unit tests, you are to write
unit tests to verify the following Boolean identities:

1)  (impliesi p q) = (ori (noti p) q) for all possible Boolean values
for the variables p and q.

2) (iffi p q) = (andi (impliesi p q) (impliesi q p)) for all possible
boolean values for the variables p and q.

In the second part of the assignment, students students are to implement
several Boolean functions which take an arbitrary number of inputs.
First you are to use foldr to implement and applied to a list of
Boolean values and or applied to a list or Boolean values. Then you
are to implement, using recursion, several functions that input a list
of integers and check for all ones, atleast one one, exactly one one, and
an odd number of ones.

There are a total of 13 questions.  All but two of the questions (7 and 8)
ask you to implement the specified function and to test it with the
provided unit tests, which serve as further specification.  The questions
that do not ask for a function definition ask you to provide a
unit test that verifies a specified property of Boolean functions.
|#


#lang racket

(require rackunit)
(require rackunit/text-ui)


;Question 1.  Fill in the ... in the definition of noti to implement
;logical negation.  Uncomment the corresponding unit test to test
;your implementation.


; logical negation
; input:  (boolean? e)
; output:  (boolean? (noti e)) true e is false and false if e is true
(define (noti e)
  (if (boolean? e)
      (if(equal? e #t) #f #t) #f))


(define-test-suite testnoti
  (check-equal?
    (noti #f) #t)
  (check-equal?
    (noti #t) #f)
)



(print "Question 1:  Testing noti")
(newline)
(run-tests testnoti 'verbose)


;Question 2.  Fill in the ... in the definition of andi to implement
;logical and.  Uncomment the corresponding unit test to test
;your implementation.


; logical and
; input:  e1 and e2 are boolean expressions
; output:  (boolean? (andi e1 e2)) true if both e1 and e2 are true
(define (andi e1 e2)
  (if (equal? #t e1)
      (if (equal? #t e2) #t #f) #f)
  )

(define-test-suite testandi
  (check-equal?
    (andi #f #f) #f)
  (check-equal?
    (andi #f #t) #f)
  (check-equal?
    (andi #t #f) #f)
  (check-equal?
    (andi #t #t) #t)
)

(print "Question 2:  Testing andi")
(newline)
(run-tests testandi 'verbose)


;Question 3.  Fill in the ... in the definition of ori to implement
;logical or.  Uncomment the corresponding unit test to test
;your implementation.

; logical or
; input:  e1 and e2 are boolean expressions
; output:  (boolean? (ori e1 e2)) true if either e1 or e2 are true

(define (ori e1 e2)
  (if (equal? #t e1) #t
      (if (equal? #t e2) #t #f)
  ))

(define-test-suite testori
  (check-equal?
    (ori #f #f) #f)
  (check-equal?
    (ori #f #t) #t)
  (check-equal?
    (ori #t #f) #t)
  (check-equal?
    (ori #t #t) #t)
)

(print "Question 3:  Testing ori")
(newline)
(run-tests testori 'verbose)


;Question 4.  Fill in the ... in the definition of xori to implement
;logical exclusive or.  Uncomment the corresponding unit test to test
;your implementation.


; logical xor
; input:  e1 and e2 are boolean expressions
; output:  (boolean? (xori e1 e2)) true if exactly one of e1 or e2
;          are true

(define (xori e1 e2)
  (if(equal? e1 e2) #f
     (if(equal? #t e1) #t
        (if (equal? #t e2) #t #f)))
     )

(define-test-suite testxori
  (check-equal?
    (xori #f #f) #f)
  (check-equal?
    (xori #f #t) #t)
  (check-equal?
    (xori #t #f) #t)
  (check-equal?
    (xori #t #t) #f)
)

(print "Question 4:  Testing xori")
(newline)
(run-tests testxori 'verbose)


;Question 5.  Fill in the ... in the definition of impliesi to implement
;logical implication.  Uncomment the corresponding unit test to test
;your implementation.


; logical implication
; input:  e1 and e2 are boolean expressions
; output:  (boolean? (impliesi e1 e2)) true if e1 is false or
; e1 is true and e2 is true
(define (impliesi e1 e2)
  (if(equal? e1 e2) #t
     (if(equal? #t e1)
        (if (equal? #t e2) #t #f) #t))
     )


(define-test-suite testimpliesi
  (check-equal?
    (impliesi #f #f) #t)
  (check-equal?
    (impliesi #f #t) #t)
  (check-equal?
    (impliesi #t #f) #f)
  (check-equal?
    (impliesi #t #t) #t)
)

(print "Question 5:  Testing impliesi")
(newline)
(run-tests testimpliesi 'verbose)


;Question 6.  Fill in the ... in the definition of iffi to implement
;logical equivalence (if and only if).  Uncomment the corresponding
;unit test to test your implementation.


; logical iffi
; input:  e1 and e2 are boolean expressions
; output:  (boolean? (iffi e1 e2)) true if e1 and e2 are both true of
;           both false
(define (iffi e1 e2)
  (if(equal? e1 e2) #t #f))

(define-test-suite testiffi
  (check-equal?
    (iffi #f #f) #t)
  (check-equal?
    (iffi #f #t) #f)
  (check-equal?
    (iffi #t #f) #f)
  (check-equal?
    (iffi #t #t) #t)
  (check-equal?
    (impliesi #f #f) #t)
  (check-equal?
    (impliesi #f #t) #t)
  (check-equal?
    (impliesi #t #f) #f)
  (check-equal?
    (impliesi #t #t) #t)
  )

(print "Question 6:  Testing iffi")
(newline)
(run-tests testiffi 'verbose)


; Question 7.  Define a unit test to test the Boolean equivalence
; (iffi e1 e2) <-> (andi (impliesi e1 e2) (impliesi e2 e1))
; Note that the symbol <-> means equivalent.
; You should create test cases for all possible values (#f and #t) of
; the Boolean variables e1 and e2 and test the that two expressions
; (iffi e1 e2) and (andi (impliesi e1 e2) (impliesi e2 e1)) are equal
; for all possible values of e1 and e2. Replace ... by your test cases.


; (iffi e1 e2) <-> (andi (impliesi e1 e2) (impliesi e2 e1))
(define-test-suite testiffequiv
   (check-equal?
    (iffi #f #f) #t)
  (check-equal?
    (iffi #f #t) #f)
  (check-equal?
    (iffi #t #f) #f)
  (check-equal?
    (iffi #t #t) #t)
  
)

(print "Question 7:  Testing iff equivalence")
(newline)
(run-tests testiffequiv 'verbose)



; Question 8.  Define a unit test to test the Boolean equivalence
; (impliesi e1 e2) <-> (ori (noti e1) e2)
; Note that the symbol <-> means equivalent.
; You should create test cases for all possible values (#f and #t) of
; the Boolean variables e1 and e2 and test the that two expressions
; (impliesi e1 e2) and (ori (noti e1) e2) are equal for
; all possible values of e1 and e2.  Replace ... by your test cases.


; (implies e1 e2) <-> (ori (noti e1) e2)
(define-test-suite testimpliesequiv
  (check-equal?
    (ori #f #f) #f)
  (check-equal?
    (ori #f #t) #t)
  (check-equal?
    (ori #t #f) #t)
  (check-equal?
    (ori #t #t) #t)
)

(print "Question 8:  Testing implies equivalence")
(newline)
(run-tests testimpliesequiv 'verbose)



;Question 9.
; Use foldr to implement (andlist L) where (andlist L) is the Boolean
; and of a list of Boolean values L.  Note that (and L1 ... Ln) is
; defined as (and L1 (and L2 ... (and L_{n-1} Ln) ... ) and is true
; when all of the Li are true.
;
; Use foldr to implement (orlist L) where (orlist L) is the Boolean
; or of a list of Boolean values L.  Note that (or L1 ... Ln) is
; defined as (or L1 (or L2 ... (or L_{n-1} Ln) ... ) and is true
; when all atleast one of the Li are true.
; In both cases, fill in the ... with the proper arguments to foldr.
; Use the unit test provided to check your implementations.


(define (andlist L)
 (foldr (lambda(e1 e2) (and e1 e2)) #t L )
     )

(define (orlist L)
  (foldr (lambda(e1 e2) (or e1 e2)) #f L )
  )

(define-test-suite testboollist
  (check-equal? (andlist '()) #t)
  (check-equal? (orlist '()) #f)
  (check-equal? (andlist '(#t #t #t)) #t)
  (check-equal? (andlist '(#t #f #t)) #f)
  (check-equal? (orlist '(#f #t #f)) #t)
  (check-equal? (orlist '(#f #f #f)) #f)
)

(print "Question 9:  Testing orlist and andlist")
(newline)
(run-tests testboollist 'verbose)


;Question 10.
; Write a recursive function allones to check if a list of integers
; contains all ones.  Use the provided unit test to test your
; implementation.  Fill in the ... with your implementation.  Don't
; forget the base case and the necessary recursion. 
; 


; Check if a list contains all ones
; Input:  L is a list of integers.
; Output: a boolean value which is true when all of the elements in L
;         are equal to one and false otherwise.
(define (allones L)
  (if (null? L) #t
      (if (= (first L) 1) (allones (rest L)) #f)
          ))

(define-test-suite testallones
  (check-equal?
    (allones '(1 0 1)) #f)
  (check-equal?
    (allones '(1 1 1)) #t)
)

(print "Question 10:  Testing allones")
(newline)
(run-tests testallones 'verbose)


;Question 11.
; Write a recursive function atleastone to check if a list of integers
; contains atleast one one. Use the provided unit test to test your
; implementation.  Don't forget the base case and the necessary recursion. 


; Check if a list contains atleast one one
; Input:  L is a list of integers.
; Output: a boolean value which is true when atleast one of the elements
;          in L is equal to one and false otherwise.
(define (atleastone L)
  (if (null? L) #f
      (if (= (first L) 1) #t (atleastone (rest L))
          )))

(define-test-suite testatleastone
  (check-equal?

    (atleastone '(0 0 0)) #f)
  (check-equal?
    (atleastone '(0 0 1)) #t)
  (check-equal?
    (atleastone '(1 0 1)) #t)
  (check-equal?
    (atleastone '(1 1 1)) #t)
)

(print "Question 11:  Testing atleastone")
(newline)
(run-tests testatleastone 'verbose)


;Question 12.
; Write a recursive function exactlyone to check if a list of integers
; contains exactly one one. Use the provided unit test to test your
; implementation.  Don't forget the base case and the necessary recursion. 

; Check if a list contains exactly one one
; Input:  L is a list of integers.
; Output: a boolean value which is true when exactly one of the elements
;          in L is equal to one and false otherwise.
(define (exactlyone L) (if (= 1 (isOne L)) #t #f))
(define (isOne L)
     (if(null? L) 0
      (if(= 1 (first L)) (+ 1(isOne(rest L))) (isOne(rest L)))
      ))

(define-test-suite testexactlytone
  (check-equal?
    (exactlyone '(0 0 0)) #f)
  (check-equal?
    (exactlyone '(0 0 1)) #t)
  (check-equal?
    (exactlyone '(1 0 1)) #f)
  (check-equal?
    (exactlyone '(1 1 1)) #f)
)

(print "Question 12:  Testing exactlyones")
(newline)
(run-tests testexactlytone 'verbose)

;Question 13.
; Write a recursive function oddones to check if a list of integers
; contains an odd number of ones. Use the provided unit test to test
; your implementation.  Don't forget the base case and the necessary
; recursion. 


; Check if a list contains an odd number of ones
; Input:  L is a list of integers.
; Output: a boolean value which is true when an odd number of the elements
;          in L is equal to one and false otherwise. 
(define (oddones L) (if (odd? (isOdd L)) #t #f))
(define (isOdd L)
     (if(null? L) 0
      (if(odd? (first L)) (+ 1(isOdd(rest L))) (isOdd(rest L)))
      )
 )
      
(define-test-suite testoddones
  (check-equal?
    (oddones '(0 0 0)) #f)
  (check-equal?
    (oddones '(0 0 1)) #t)
  (check-equal?
    (oddones '(1 0 1)) #f)
  (check-equal?
    (oddones '(1 1 1)) #t)
)

(print "Question 13:  Testing oddones")
(newline)
(run-tests testoddones 'verbose)
