#lang racket
#|
CS 270 - Mathematical Foundations of Computer Science
Drexel University Fall 2016-2017
Homework Assignment 6
Due Date: Friday Nov. 11 at 11:59 PM

Student name:  TODO: Joseph Mulray jam699@drexel.edu

For this homework, you will need to use DrRacket. You should have installed it by now on your machine, and know the basics of using it. Make sure you have the latest version.


The goal of this assignment is to think about recursive programming and to
review boolean expressions and evaluation.

Instructions for using this file:

- open this file in DrRacket as assign6.rkt

- insert your solutions into this file where indicated (for instance as "'replace-this-with-your-implementation")

- make sure the entire file is accepted by DrRacket. If you don't finish some problems, comment them out. The same is true for any English text that you may add. This file already contains many comments, so you can see what the syntax is.

- Submit your homework through Blackboard (learning.drexel.edu) 

- All function names are given, do not change the names of any functions

Finally, add an ASCII text file "comments.txt" into the same directory where you put assignment_6.rkt, with the following contents:

(i)  a summary of how much time you spent on this homework

(ii) any feedback on the homework you may have, such as errors in it, sources of misunderstanding, general difficulty, did you learn something, etc.

(iii) any problems that were not completed

|#

;; We use rackunit package to do unit tests. When you start,
;; all the tests will be failing. Once you implement the required
;; functions, the unit tests associated with those functions should
;; pass. Do not modify the unit test blocks.
;; 
;;
(require rackunit)
(require rackunit/text-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assignment Overview
;
; The goal of this assignment is to create a boolean evaluator
; and use it to determine if a boolean expression is satisfied or not. 
; An expression is satisified if it is true for a setting of 
; variables.
;
; This framework could also be used to determine if it is 
; possible to make a statement true for all setting of 
; variables, i.e. a tautology checker.
; Either directly by observing that Phi is a tautology iff
; ~Phi is not satisfiable or though minor modification of the code.
;
; We will slowly build up all the tools needed by solving a
; series of smaller problems first. You need to do this 
; assignment from top to bottom. Functions are defined to be 
; used later on.
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1 Lookup
;
; If we want to set the values of variables in our boolean
; expressions, we need a way to store variable values.
;
; The lookup functions takes a variable name and an environment. 
; It returns the value the variable is assigned.
; If the variable is not assigned it returns an error.
;
; The environment is a list of pairs 
;    (variable_name variable_value)
;
; Inputs: A variable name and environment
; Outputs: The value of the variable or an error
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Hints: Here is an example of how to throw an error.
; Copy it into your session and see what it does
;
; (error 'lookup "Variable Name Not Found")
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lookup target environment)
 (if (equal? (assq target environment) #f)
     (error 'lookup "Variable Name Not Found")
     (second (assq target environment)) )
)



(define-test-suite lookup-suite
(check-equal? 
  (lookup 'a '( (a #t) (b #f))) ;returns #t
  #t)

(check-equal?
  (lookup 'b '( (a #t) (b #f))) ;returns #f
  #f)
)
(run-tests lookup-suite 'verbose)
;You can test the error with the next line.
;Make sure to comment it back out so you can complete the rest of the assignment
;(lookup 'c '( (a #t) (b #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2 bool-eval
;
; Next we need to evaluate a boolean expression given a list of variables
; We will build up an evaluator that can evaluate boolean expressions containing
; Variables, Constants (#t,#f), not, or, and, implies, iff
;
; We will start with just variables, constants and "or".
; The new expression you should add is "and".
; Test your evaluator after each new case is added.
;
; Input: A boolean expression and an environment
; Output: The result of evaluating the expression with the variable values 
;          from the environment
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;General Helper Function
;The following words cannot be used as variable names
; (or and not implies iff #t #f)
; We need a test so that the system doesn't treat these as variables
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

;Helper Functions for Constants
;is-constant: Returns true if the expression is a constant
(define (is-constant? expression)
  ;This is true when the expression is #t or #f
  ;It is false otherwise
  (or (equal? expression #t) (equal? expression #f))
 )
;Tests
(is-constant? '#t) ;true
(is-constant? '#f) ;true
(is-constant? 'or) ;false

;eval-constant: returns the boolean value of the constant
;Note: we don't need environment here, but for consistency all 
; our eval functions will take environment as their second input
(define (eval-constant expression environment)
  expression
)
;Tests
(eval-constant '#t '( (a #t) (b #f))) ;returns #t
(eval-constant '#f '( (a #t) (b #f))) ;returns #f


;Helper Functions for Variables
;is-variable?: Returns true when the expression is a symbol
(define (is-variable? expression)
  (and
   (symbol? expression)
   (not (is-reserved-word? expression))
  )
)
;Tests
(is-variable? 'a)
(is-variable? 'b)
(is-variable? '#t)
(is-variable? '#f)
(is-variable? 'or)
(is-variable? 'and)
(is-variable? 'not)
(is-variable? 'implies)
(is-variable? 'iff)
;eval-variable: returns the value associated with a variable
;This is why you wrote the lookup function!
(define (eval-variable variable environment)
  (lookup variable environment)
)

;From Here Down, the functions call each other

;Helper Functions for the Or Statement
;is-or: returns true if the statement is an or
(define (is-or? expression)
  (equal? (first expression) 'or)
)
;Evaluate the or statement
;Here is where it gets interesting
;To evaluate an or statement, we need to evaluate the inputs first
(define (eval-or expression environment)
  ;Use the built in or to find the actual value
  ;so the or looks like (or something1 something2)
  ; (first expression) the word or
  ; (first (rest expression)) the expression something1
  ; (first (rest (rest expression))) the expression something2
  (or 
   (bool-eval (first (rest expression)) environment)
   (bool-eval (first (rest (rest expression))) environment)
  )
)

;You have to implement the remaining definitions
;Uncomment this code as you complete parts. You don't have to do it all at once.
;For example, you can implement and test "and" without "implies" if you just
;leave all the implies code commented out.

;is-and: returns true if the statement is an and
(define (is-and? expression)
  (equal? (first expression) 'and)
)

;eval-and: evaluate an and statement
(define (eval-and expression environment)
  (and 
  (bool-eval (first (rest expression)) environment)
  (bool-eval (first (rest (rest expression))) environment)
  )
)

;is-not: returns true if the statement is a not
(define (is-not? expression)
    (equal? (first expression) 'not)
)

;eval-not: evaluate a not expression
(define (eval-not expression environment)
  (not (bool-eval (first (rest expression)) environment)
)
  )

;is-implies: returns true if the statement is a implies
(define (is-implies? expression)
  (equal? (first expression) 'implies)
)


;eval-implies: evaluate an implies expression
(define (eval-implies expression environment)
  (implies 
  (bool-eval (first (rest expression)) environment)
  (bool-eval (first (rest (rest expression))) environment)
  )
)

(define (iff first second)
  (if (and first second) #t (if (and (not first) (not second))
                                #t #f)
      )
  )

;is-iff: returns true if the statement is an iff
(define (is-iff? expression)
  (equal? (first expression) 'iff)
)

;eval-iff; evaluate an iff expression
(define (eval-iff expression environment)
  (iff 
  (bool-eval (first (rest expression)) environment)
  (bool-eval (first (rest (rest expression))) environment)
  )
  )

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
    [;Case 3 or statements
     (is-or? expression)
     (eval-or expression environment)
    ]
    [;Case 4 not statements
    (is-not? expression)
     (eval-not expression environment)
    ]
    [;Case 5 and statements
    (is-and? expression)
    (eval-and expression environment)
    ]
    [;Case 6 implies statements
    (is-implies? expression)
    (eval-implies expression environment)
    ]
    [;Case 7 iff
    (is-iff? expression)
    (eval-iff expression environment)
    ]
     [;Else Case
     else
     (error 'bool-eval "Expression given was invalid")
    ]
  )
)
;Uncomment the Sections of tests as you complete each set of definitions

;Test Constants
(define-test-suite bool-eval-suite
(check-equal? 
(bool-eval '#t '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '#f '( (a #t) (b #f))) ;False
#f)
;Test Variables
(check-equal? 
(bool-eval 'a '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval 'b '( (a #t) (b #f))) ;False
#f)
;Test Or
(check-equal? 
(bool-eval '(or a a) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(or a b) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(or b a) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(or b b) '( (a #t) (b #f))) ;False
#f)
;Test not
(check-equal? 
(bool-eval '(not a) '( (a #t) (b #f))) ;False
#f)
(check-equal? 
(bool-eval '(not b) '( (a #t) (b #f))) ;True
#t)
;Test and
(check-equal? 
(bool-eval '(and a a) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(and a b) '( (a #t) (b #f))) ;False
#f)
(check-equal? 
(bool-eval '(and b a) '( (a #t) (b #f))) ;False
#f)
(check-equal? 
(bool-eval '(and b b) '( (a #t) (b #f))) ;False
#f)
;Test implies
(check-equal? 
(bool-eval '(implies a a) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(implies a b) '( (a #t) (b #f))) ;False
#f)
(check-equal? 
(bool-eval '(implies b a) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(implies b b) '( (a #t) (b #f))) ;True
#t)
;Test iff
(check-equal? 
(bool-eval '(iff a a) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(iff a b) '( (a #t) (b #f))) ;False
#f)
(check-equal? 
(bool-eval '(iff b a) '( (a #t) (b #f))) ;False
#f)
(check-equal? 
(bool-eval '(iff b b) '( (a #t) (b #f))) ;True
#t)
)
(run-tests bool-eval-suite 'verbose)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 3: get-variables
;
; If we want to test all setting for variables, then we need to know what
; the variables are
;
; Inputs: a boolean expression
; Outputs: a list with the variables from the expression
; The output should contain NO duplicate variable names.
;
; Hint: What does append do?
; Hint: What does remove-duplicates do?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-variables expression)(get-list (flatten expression) `()))
(define (get-list expression array)
  (if (null? expression) (remove-duplicates (reverse array))
      (if (equal? #f (member (first expression) `(iff not implies and or #t #f))) (get-list (rest expression) (list* (car expression) array))
                  (get-list (rest expression) array))
          )
      )
 
      ;end defunc


(define-test-suite get-variables-suite
(check-equal? 
(get-variables 'a); returns (a)
'(a))

(check-equal? 
(get-variables '(or a b)); returns the list (a b)
'(a b))

(check-equal? 
(get-variables '(and (or a (not b)) (implies c (iff d #t)))); returns the list (a b c d)
'(a b c d))

(check-equal? 
(get-variables '(and (not a) a)); returns (a)
'(a))

(check-true 
(or
  (equal? (get-variables '(and (or a b) (and b a)));returns (b a) (Either one of these is correct)
          '(b a))
  (equal? (get-variables '(and (or a b) (and b a)));returns (a b) (Either one of these is correct)
          '(a b))
))
;(b a) or (a b) is correct, but you will find (b a) is the 
;easier approach.

)
(run-tests get-variables-suite 'verbose)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 4: make-truth-table
;
; Given a list of variables give the truth table with all settings of varaibles
;
; Input: A list of variables
; Output: A list of environments with all possible settings for the variables
;
;Hint: What does the list command do? For example (list 1 2 3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This sounds easy, but having the output look right is actually a little difficult
;We will make some helper functions to get us there

;Variable Bindings
;For any variable, it can be either true or false
; Given a variable, we want to create a set of bindings with the possibilities
; ( ( (var_name_1 value_1) ) ( (var_name_1 value_2) ) )
;This seems like it has a lot of nested lists, but we will need them all.
;At the top-most point we returns a list of environments
; (make_bindings 'a) returns (environments for a)
; (environments for a) = ( (environment where a=t) (environment where a=nil))
; = ( ((a t)) ((b t)))
(define (make_bindings name)
  (append (list(list (list* name `(#t)))) (list (list (list* name `(#f)))))
)

(define-test-suite make-bindings-suite
(check-equal? 
(make_bindings 'a) ; returns (((a #t)) ((a #f))) 
'(((a #t)) ((a #f))) 
)

(check-equal? 
(make_bindings 'b) ; returns (((b #t)) ((b #f))) 
'(((b #t)) ((b #f))) 
)
)
(run-tests make-bindings-suite 'verbose)



;Insert Binding
; Given a binding and a list of environments, 
; Append the binding to the beginning of each environment
; (insert_binding (X) ( (a b c) (m p q) ...))
; returns 
; ( (X a b c) (X m p q) ...)
; Inserting into the empty list should return the empty list
;(define (insert_binding binding environments)
;  (if (empty? environments) environments (binding-list binding environments `())
; )
;  )



(define (insert_binding binding environments)
 (if (cons? environments)
     (cons (append binding (first environments))
           (insert_binding binding (rest environments))
           ) environments)
  )

;(define (insert_binding binding environments)
;  (if (empty? environments) environments
;      (list (add-between (first environments) binding #:before-first binding
;               #:before-last binding
;               #:splice? #t)
;      ))
;  )

;(define (binding-list binding environments array)
;  (if (null? environments) array
;      (binding-list binding (rest environments) (add-between array #:after-last (append binding (first environments)))
;                    )
;      )
;  )

(define-test-suite insert-bindings-suite
(check-equal? 
(insert_binding '((a #t)) '()) ; ( )
'()
)

(check-equal? 
(insert_binding '((oranges #t)) '( ((grapes #f)) )) ; ( ((oranges #t) (grapes #f)) )
'( ((oranges #t) (grapes #f)) )
'((oranges #t) ((grapes #f)))
)

(check-equal? 
(insert_binding '((a #t)) '( ((b #t)) ((b #f)) )) ;( ((a #t) (b #t)) ((a #t) (b #f)) )
'( ((a #t) (b #t)) ((a #t) (b #f)) ))
)
(run-tests insert-bindings-suite 'verbose)


;Insert Multiple Bindings
; When we are extending the truth table, we are adding more then one binding at a time
; Next, we define a function that takes a list of bindings and addes then to the environment.
; Case 1: When the environment is null, return the bindings
; Case 2: When the bindings are null, return null
; Case 3: Neither bindings nor environment are null
; 
; This is extending the truth table. Lets look at an example for 2 variables
; The empty table look like ()
; The table with just b looks like
; ( ; start of table
;   ( (b #t) ) ;First row has one column
;   ( (b #f) ) ;Second row has one column
; ) ; end of table
; This is the same as inserting the bindings of b into the null environment
; The table for a and b looks like
; (;start of table
;   ( (a #t) (b #t)) ;Row 1
;   ( (a #t) (b #f)) ;Row 2
;   ( (a #f) (b #t)) ;Row 3
;   ( (a #f) (b #f)) ;Row 4
;); End of table
; The rows 1-2 are inserting the first element in the binding into each row in the previous table
; Then we need to append this to the next value in each row of the previous table (rows 3-4)
(define (insert_multiple_bindings bindings environments)
  (cond
    [(eq? environments `()) bindings]
    [(eq? bindings `()) `()]
    [else
     (append (insert_binding (first bindings) environments)
             (insert_multiple_bindings (rest bindings) environments)
            )

     ]))

(define-test-suite insert-multiple-bindings-suite
(check-equal? 
 (insert_multiple_bindings '( ((a #t)) ((a #f))) '())
 '( ((a #t)) ((a #f)))
)

(check-equal? 
 (insert_multiple_bindings '() '( ((b #t)) ((b #f))))
 '()
)

(check-equal? 
(insert_multiple_bindings '( ((a #t)) ((a #f))) '( ((b #t)) ((b #f))) )
'( 
  ((a #t) (b #t))
  ((a #t) (b #f))
  ((a #f) (b #t))
  ((a #f) (b #f))
)
);end of check
)
(run-tests insert-multiple-bindings-suite 'verbose)


;Extend Table
;Given a table of truth values, extend it by adding a new variable name.
;(extend_table var_name current_table)
;Use the functions you have previously defined.

;(define (extend_table var_name current_table)
;  (if (null? current_table) (list (list (append (list var_name) `(#t))) (list(append (list var_name) `(#f))))
;      (insert_multiple_bindings
;       (list (list (append (list var_name) `(#t))) (list(append (list var_name) `(#f)))) current_table)
;      )
;)

(define (extend_table var_name current_table)
      (insert_multiple_bindings (make_bindings var_name) current_table)
  )
   


(define-test-suite extend-table-suite
(check-equal? 
(extend_table 'a '()) 
'( (( a #t)) ((a #f)) )
)

(check-equal?  (extend_table 'a '( (( b #t)) ((b #f)) ))
'( 
  ((a #t) (b #t))
  ((a #t) (b #f))
  ((a #f) (b #t))
  ((a #f) (b #f))
 )
)
)
(run-tests extend-table-suite 'verbose)

;Gen Truth Table
; Given a list of variables generate the truth table for them

;(define (make-truth-table var_names)
;  (if (null? var_names) '() ;Else
;    (extend_table (first var_names) (make-truth-table (rest var_names)))
;  )
;)

(define (make-truth-table var_names)
  (if (equal? var_names '()) '() ;Else
    (extend_table (first var_names) (make-truth-table (rest var_names)))
  )
)


(define-test-suite make-truth-table-suite

(check-equal? 
(make-truth-table '(a));returns ( ((a #t)) ((a #f)))
'( ((a #t)) ((a #f))))

(check-equal? 
(make-truth-table '(a b))
;Result with pretty spacing
;( 
;      ( ( a #t) ( b #t))
;      ( ( a #t) ( b #f))
;      ( ( a #f) ( b #t))
;      ( ( a #f) ( b #f))
;)
'( ((a #t) (b #t)) ((a #t) (b #f)) ((a #f) (b #t)) ((a #f) (b #f)))
);end of check

;This answer is very long (2^4 possibilities)
(check-equal? 
(make-truth-table '(a b c d)) 
'(
    ((a #t) (b #t) (c #t) (d #t))
    ((a #t) (b #t) (c #t) (d #f))
    ((a #t) (b #t) (c #f) (d #t))
    ((a #t) (b #t) (c #f) (d #f))
    ((a #t) (b #f) (c #t) (d #t))
    ((a #t) (b #f) (c #t) (d #f))
    ((a #t) (b #f) (c #f) (d #t))
    ((a #t) (b #f) (c #f) (d #f))

    ((a #f) (b #t) (c #t) (d #t))
    ((a #f) (b #t) (c #t) (d #f))
    ((a #f) (b #t) (c #f) (d #t))
    ((a #f) (b #t) (c #f) (d #f))
    ((a #f) (b #f) (c #t) (d #t))
    ((a #f) (b #f) (c #t) (d #f))
    ((a #f) (b #f) (c #f) (d #t))
    ((a #f) (b #f) (c #f) (d #f))
))
)
(run-tests make-truth-table-suite 'verbose)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 5: is_tautology
;
; Given a boolean expression, decide if it is a tautology (always true)
;
; Input: A boolean expression with variables
; Output: True if the expression is a tautology, false otherwise
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;A few more helper functions
;Given an expression and a truth table
;Run the expression on each item and make a list of the results
(define (run-on-truth-table expression tt)
  (cond
    [(cons? tt)
     (append (cons (bool-eval expression (first tt)) `())
                   (append (run-on-truth-table expression (rest tt)) `() ))
     ]
    [else `()] ) )
;Tests
(define-test-suite run-on-truth-table-suite

(check-equal? 
(run-on-truth-table '(not a) (make-truth-table '(a))) ;returns (#f #t)
'(#f #t))

(check-equal? 
(run-on-truth-table '(or (not a) a) (make-truth-table '(a))); returns (#t #t)
'(#t #t))

(check-equal? 
(run-on-truth-table '(and a b) (make-truth-table '(a b)));returns (#t #f #f #f)
'(#t #f #f #f))
)
(run-tests run-on-truth-table-suite 'verbose)

;Now you have a list with the results all all your tests
;we need to decide if atleast one is true
;Given a list of boolean values, return true if atleast one is true 
; Return false otherwise
(define (atleast-one-true my_list)
  (if (null? my_list)
      #f
      (if (equal? (first my_list) #t) #t
          (atleast-one-true (rest my_list))
          )))

;Tests
(define-test-suite atleast-one-true-suite

(check-equal? 
(atleast-one-true '());returns #f
#f)

(check-equal? 
(atleast-one-true '(#t));returns #t
#t)

(check-equal? 
(atleast-one-true '(#f #f #f)); returns #f
#f)

(check-equal? 
(atleast-one-true '(#t #f #t)); returns #t
#t)

(check-equal? 
(atleast-one-true '(#f #f #t)); returns #t
#t)
)
(run-tests atleast-one-true-suite 'verbose)

;Finally, determine if the expression is satisfied using all the components
; you have built up
(define (is-satisfied? bool-expression)
    (atleast-one-true (run-on-truth-table bool-expression 
                                        (make-truth-table
                                         (get-variables bool-expression))))
  )
  

;Test Case
(define-test-suite is-satisfied-suite

(check-equal? 
(is-satisfied? '(or (not a) a)) ;returns true
#t)

(check-equal? 
(is-satisfied? '(and (not a) a)) ;returns false
#f)
 
(check-equal? 
 (is-satisfied? 
  '(not (iff (implies a b) (or (not a) b)))
  );returns false
#f)

(check-equal?
  (is-satisfied?
   '(and (or p (or q (not p))) (or (not p) (or q r)))
  ); returns true
 #t)

(check-equal?
  (is-satisfied?
   '(and (or p (or q (not r))) (or (not p) (or q r)))
  ); returns true
 #t)

(check-equal?
  (is-satisfied?
   '(and (or p q) (and (not p) (not q)))
  ); returns false
 #f)
  
(check-equal?
  (is-satisfied?
   '(and (or p q) (and (or (not p) (not q)) (and (or p (not q))
                                                 (or (not p) q))))
  ); returns false
 #f)
)
(run-tests is-satisfied-suite 'verbose)
