#lang racket
#|
CS 270 - Mathematical Foundations of Computer Science
Drexel University Fall 2016-2017
Homework Assignment 8
Due Friday Dec. 2 at 11:59pm

Student name:  TODO: JOSEPH MULRAY

For this homework, you will need to use DrRacket.
You should have installed it by now on your machine,
and know the basics of using it. Make sure you have the latest version.

The goal of this assignment is to further develop recursive programming
skills and to reason informally and formally about recursive programs.

Instructions for using this file:

- open this file in DrRacket as assign8.rkt

- insert your solutions into this file where indicated (for instance as
"'replace-this-with-your-implementation")

- make sure the entire file is accepted by DrRacket. If you don't finish some
problems, comment them out. The same is true for any English text that you may add.
This file already contains many comments, so you can see what the syntax is.

- Submit your homework through Blackboard (learning.drexel.edu) 

- All function names are given, do not change the names of any functions

Finally, add an ASCII text file "comments.txt" into the same directory where you put
assign8.rkt, with the following contents:

(i)  a summary of how much time you spent on this homework

(ii) any feedback on the homework you may have, such as errors in it,
     sources of misunderstanding, general difficulty, did you learn something, etc.

(iii) any problems that were not completed

|#


;; We use rackunit package to do unit tests. When you start,
;; all the tests will be failing. Once you implement the required
;; functions, the unit tests associated with those functions should
;; pass.
;; 
;;
(require rackunit)
(require rackunit/text-ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assignment Overview
;
; The goal of this assignment is to create a function that converts
; an arbitrary boolean expression into an equivalent one in conjuctive
; normal form (CNF).  Recall that CNF is a conjunction (and) of
; clauses which are disjunctions (or) of literals.  A literal is either
; a variable or a negated variable.  Note that a single clause is in
; CNF as is a single literal or constant.
;
; For this assignment we assume that boolean expression only consist of
; constants, variables, not, and, and or.  Other boolean functions such
; as implies, and iff can be converted into equivalent expressions using
; just constants, variables, not, and, and or. Using the tautologies
; (implies a b) <-> (or (not a) b) and
; (iff a b) <-> (and (implies a b) (implies b a))
;
; Part 0 of the assignment provides bool-eval and other helper functions
; such as op1, op2, is-not?, is-or?, and is-and? that you can use in the
; rest of the assignment.  bool-eval provides a template for writing
; recursive functions for processing boolean expressions.
;
; Conversion to conjunctive normal form is done in two steps:
; 1) [nnf] convert to negative normal form using DeMorgan's law
; 2) [cnf] convert expressions in negative normal form to conjunctive normal form
;    using the distributive law.
;
; Separate functions is-nnf? and is_cnf? are written to check that the output
; from nnf and cnf is in negative normal form or conjunctive normal form respectively.
; Code for (1) is provided in Part I.  Students should study this code before
; writing code for (2) in Part II.  In Part II students write is-cnf? and
; the function nnf along with helper functions that guide them through
; the implementation of is-cnf? and cnf.  Test suites are provided to test and
; help specify is-cnf? and cnf. The test suites for is-cnf? and cnf are
; commented out.  When you are testing these functions you should uncomment
; the test suites.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 0.  Auxiliary and helper functions that may be used in the assignment
;
;          bool-eval, lookup,op1, op2,
;          is-reserved-word? is-constant?, is-variable?, is-not?, is-or?
;          is-and?,
;          eval-constant, eval-variable, eval-not, eval-or, eval-and.


;bool-eval
;Inputs: a boolean expression and an environment
;Ouput: a boolean equal to the value of the input expression.
(define (bool-eval expression environment)
  (cond
    [;Case 1 Constants
     (is-constant? expression)
     (eval-constant expression environment)
    ]
    [;Case 2 Variables
     (is-variable? expression)
     (eval-variable expression environment)
    ]
    [;Case 3 not statements
     (is-not? expression)
     (eval-not expression environment)
    ]
    [;Case 4 or statements
     (is-or? expression)
     (eval-or expression environment)
    ]
    [;Case 5 and statements
     (is-and? expression)
     (eval-and expression environment)
    ]
    [;Else Case
     else
     (error 'bool-eval "Expression given was invalid")
    ]
  )
)

; lookup
; Inputs: A variable name and environment
; Outputs: The value of the variable or an error
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lookup target environment)
  ;First check for error
  (if (null? environment)
    ;then
    (error 'lookup "Variable Name Not Found")
    ;else
    (if 
      ;If the first pair the one we want?
      (equal? target (first (first environment)))
      ;then return it's value
      (first (rest (first environment)))
      ;Else keep looking
      (lookup target (rest environment))
     );end inner if
  );end outer if
)
;op1 and op2
;access functions to get first and second operands to a boolean expression
(define (op1 expr)
  (second expr))

(define (op2 expr)
  (third expr))

;is-reserved-word?
; Inputs: A symbol
; Outputs: A boolean.  #t if the the input is a reserved word #f otherwise
(define (is-reserved-word? word)
  (cond
   [ (equal? word '#t) #t]
   [ (equal? word '#f) #t]
   [ (equal? word 'or) #t]
   [ (equal? word 'and) #t]
   [ (equal? word 'not) #t]
   [ (equal? word 'implies) #t]
   [ (equal? word 'iff) #t]
   ;Otherwise
   [ else #f]
  )
 )

;is-constant?: Returns true if the expression is a constant
(define (is-constant? expression)
  (or (equal? expression #t) (equal? expression #f))
 )

;eval-constant: returns the boolean value of the constant
(define (eval-constant expression environment)
  expression
)

;is-variable?: Returns true when the expression is a symbol that is not reserved
(define (is-variable? expression)
  (and
   (symbol? expression)
   (not (is-reserved-word? expression))
  )
)

;eval-variable: returns the value associated with a variable
;This is why you wrote the lookup function!
(define (eval-variable variable environment)
  (lookup variable environment)
)

;is-or?: returns true if the expression is an or
(define (is-or? expression)
  (and (list? expression) (equal? (first expression) 'or))
)

;eval-or: evaluate or expression
(define (eval-or expression environment)
  (or 
   (bool-eval (first (rest expression)) environment)
   (bool-eval (first (rest (rest expression))) environment)
  )
)

;is-and?: returns true if the expression is an and
(define (is-and? expression)
  (and (list? expression) (equal? (first expression) 'and))
)

;eval-and: evaluate an and statement
(define (eval-and expression environment)
  (and 
   (bool-eval (first (rest expression)) environment)
   (bool-eval (first (rest (rest expression))) environment)
  )
)

;is-not?: returns true if the expression is a not
(define (is-not? expression)
  (and (list? expression) (equal? (first expression) 'not))
)

;eval-not: evaluate a not expression
(define (eval-not expression environment)
  ;Note that not only takes 1 argument
  (not
   (bool-eval (first (rest expression)) environment)
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Part I.  Negative Normal Form (NNF).
;  Contains function is-nnf? to check if a boolean expression is in NNF.
;  Contains function nnf and its helper function nnf-not to convert
;  a boolean expression into an equivalent expression that is in NNF.
;  The test suite nnf-suite is provided to test these functions.
;  The following property should be satisfied.
;  (and (is-nnf? (nnf expr)) (equal? (bool-eval expr env))
;                            (equal? (bool-eval (nnf expr) env)))
;        
;  This part of the assignment is provided as a model for the second part.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;is-nnf?
;Input: a boolean expression
;Output: a boolean which is true if the input expression is in negative normal form.
(define (is-nnf? expr)
  (cond
   [ (is-constant? expr) #t ]
   [ (is-variable? expr) #t ]
   [ (is-not? expr) (is-variable? (op1 expr)) ]
   [ (is-or? expr) (and (is-nnf? (op1 expr)) (is-nnf? (op2 expr))) ]
   [ (is-and? expr) (and (is-nnf? (op1 expr)) (is-nnf? (op2 expr))) ]
  ) 
)

;nnf - convert boolean expression to negative normal form.
;Input: A boolean expression
;Output: A boolean expression that is equivalent to the input and
;        is in negative normal form.
;        (and (is-nnf? (nnf expr)) (equal? (bool-eval expr env))
;                                  (equal? (bool-eval (nnf expr) env)))
(define (nnf expr)
  (cond
   [ (is-constant? expr) expr ]
   [ (is-variable? expr) expr ]
   [ (is-not? expr) (nnf-not expr) ]
   [ (is-or? expr) (list 'or (nnf (op1 expr)) (nnf (op2 expr))) ]
   [ (is-and? expr) (list 'and (nnf (op1 expr)) (nnf (op2 expr))) ]
  ) 
)

;nnf-not.  Convert a negation into negative normal form.
;Input: A boolean expression that is a negation.
;Output: A boolean expression that is in NNF and equivalent to the input.
(define (nnf-not expr)
  (cond
   [ (is-constant? (op1 expr)) (not expr) ]
   [ (is-variable? (op1 expr)) expr ]
   [ (is-not? (op1 expr)) (nnf (op1 (op1 expr))) ]
   [ (is-or? (op1 expr)) (list 'and (nnf (list 'not (op1 (op1 expr))))
                                   (nnf (list 'not (op2 (op1 expr))))) ]
   [ (is-and? (op1 expr)) (list 'or (nnf (list 'not (op1 (op1 expr))))
                                   (nnf (list 'not (op2 (op1 expr))))) ]
  )
)



(define-test-suite nnf-suite
(check-equal? 
(is-nnf? '#f) #t)
(check-equal? 
(is-nnf? 'x) #t)
(check-equal? 
(is-nnf? '(not x)) #t)
(check-equal? 
(is-nnf? '(and x (not y))) #t)
(check-equal? 
(is-nnf? '(and x (or y (not z)))) #t)
(check-equal? 
(is-nnf? '(not (not x))) #f)
(check-equal? 
(is-nnf? '(not (or x y))) #f)
(check-equal? (nnf 'x) 'x)
(check-equal? (nnf '(not x)) '(not x))
(check-equal? (nnf '(not (not x))) 'x)
(check-equal? (nnf '(not (or x y))) '(and (not x) (not y)))
(check-equal? (nnf '(not (and x y))) '(or (not x) (not y)))
(check-equal? (nnf '(not (or (and x (not y)) (not z))))
             '(and (or (not x) y) z))
)
(run-tests nnf-suite 'verbose)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Part II. Conjunctive Normal Form (CNF).
;  Contains function is-cnf? and its helper functions no-and? and no-or-above-and?
;  to check if a boolean expression is in CNF.
;
;  Contains function cnf and its helper function distrib-orand to convert
;  a boolean expression into an equivalent expression that is in CNF.
;  The test suites is-cnf-suite and cnf-suite are provided to test these functions.
;  The following property should be satisfied.
;  (and (is-cnf? (cnf expr)) (equal? (bool-eval expr env))
;                            (equal? (bool-eval (cnf expr) env)))
;        
;  Students must provide and test the following functions:
;  is-cnf?, no-and?, no-or-above-and?, cnf, and distrib-orand.
;
;  is-cnf? checks to see that an expression is in NNF using is-nnf?
;  and has no ors above ands using no-or-above-and?
;
;  no-or-above-and? recursively traverses a boolean expression checking
;  that there are no ors above ands.  When an or is encountered the
;  function no-and? is used to check that the operands to the or do
;  not contain any ands.
;
;  cnf calls nnf to convert the input expression to NNF and then calls
;  nnf2cnf to convert the resulting NNF expression to CNF.  After recursively
;  converting the operands to an or, nnf2cnf calls distrib-orand to
;  distribute the and over ors.
;





;
;  When implementing cnf and is-cnf? first outline how they are to be
;  implemented using the helper functions and then implement and test the
;  helper functions before testing the top level cnf and is-cnf? functions.
;  is-cnf? can be used to help test cnf.
;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;is-cnf? check if a boolean expression is in CNF.
;  A boolean expression is in CNF if it is in NNF and there are no
;  ors above above ands
;


(define (is-cnf? expr)
   (and (is-nnf? expr) (no-or-above-and? expr)
        )
  )

;no-or-above-and? check that a boolean expression contains no ors above ands.
(define (no-or-above-and? expr)
 (cond
   [ (is-constant? expr) #t ]
   [ (is-variable? expr) #t ]
   [ (is-not? expr) (no-or-above-and? (op1 expr)) ]
   [ (is-or? expr) (and (no-and? (op1 expr)) (no-and? (op2 expr)))]
   [ (is-and? expr) (and (no-or-above-and? (op1 expr)) (no-or-above-and? (op2 expr)))
     ]
   
))

;no-and? check if a boolean expression contains any ands.
(define (no-and? expr)
   (cond
   [ (is-constant? expr) #t ]
   [ (is-variable? expr) #t ]
   [ (is-and? expr) #f]
   [ (is-not? expr) (no-and? (op1 expr))]
   [ (is-or? expr) (and (no-and? (op1 expr)) (no-and? (op2 expr)))]

   )
  )


;test suite for is-cnf?  Uncomment when you are ready to test is-cnf?
(define-test-suite is-cnf-suite
(check-equal? 
  (is-cnf? 'x) #t)
(check-equal?
  (is-cnf? '(or x (or y z))) #t)
(check-equal?
  (is-cnf? '(or x (and y z))) #f)
(check-equal?
  (is-cnf? '(and x (or y z))) #t)
(check-equal?
  (is-cnf? '(and (or x (not y)) (and (or y (not z))
                 (or (not x) (or y z))))) #t)
(check-equal?
  (is-cnf? '(and (or x (not y)) 
                 (or (not x) (and y z)))) #f)
)
(run-tests is-cnf-suite 'verbose)


; cnf
; Input:  A boolean expression
; Output: A boolean expression in CNF that is equivalent to the input expression.
(define (cnf expr)
 (nnf2cnf (nnf expr))
  )

; nnf2cnf
; Input:  A boolean expression in NNF
; Output: A boolean expression in CNF that is equivalent to the input expression.
(define (nnf2cnf expr)
  (cond
   [ (is-constant? expr) expr ]
   [ (is-variable? expr) expr ]
   [ (is-not? expr) expr]
   [ (is-and? expr) (list `and (cnf (op1 expr)) (cnf (op2 expr)))]
   [ (is-or? expr) (distrib-orand (cnf (op1 expr)) (cnf (op2 expr)))]

         )
)

; distrib-orand.  Distribute or over and.
; Input:  Two expressions in CNF
; Output: An expression in CNF equivalent to (or expr1 expr2)
; There are three cases.  Let expr1 = E1 and expr2 = E2
; 1) E1 = E11 /\ E12.  (E11 \/ E2) /\ (E12 \/ E2)
; 2) E2 = E21 /\ E22.  (E1 \/ E21) /\ (E1 \/ E22)
; 3) E1 and E2 contain no ands.  E1 \/ E2
; In the first two cases distrib-orand must be called recursively as
; there may be additional ands to distribute.

(define (distrib-orand expr1 expr2)
(cond
  [(is-and? expr1) (list `and (distrib-orand (op1 expr1) expr2)
                         (distrib-orand (op2 expr1) expr2))]
  [(is-and? expr2) (list `and (distrib-orand expr1 (op1 expr2) )
                         (distrib-orand expr1 (op2 expr2) ))]
  [else (list `or expr1 expr2)]
  )
)

; test suite for cnf  Uncomment when you are ready to test cnf.
(define-test-suite cnf-suite
(check-equal?
   (distrib-orand 'x 'y) '(or x y))
(check-equal? 
  (distrib-orand '(and x y) 'z) '(and (or x z) (or y z)))
(check-equal? 
  (distrib-orand 'u '(and x y)) '(and (or u x) (or u y)))
(check-equal? 
  (cnf 'x) 'x)
(check-equal? 
  (cnf '(not x)) '(not x))
(check-equal? 
  (cnf '(not (not x))) 'x)
(check-equal? 
  (cnf '(and x y)) '(and x y))
(check-equal? 
  (cnf '(not (and x y))) '(or (not x) (not y)))
(check-equal? 
  (cnf '(or x y)) '(or x y))
(check-equal? 
  (cnf '(and (or x y) (or (not x) z))) '(and (or x y) (or (not x) z)))
(check-equal? 
  (cnf '(or (and x y) z)) '(and (or x z) (or y z)))
(check-equal? 
  (cnf '(or x (and y z))) '(and (or x y) (or x z)))
(check-equal? 
  (cnf '(or (and x (and y z)) w)) '(and (or x w) (and (or y w) (or z w))))
)
(run-tests cnf-suite 'verbose)
