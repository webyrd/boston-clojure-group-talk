;; miniKanren resources:

;; Website:
;; http://minikanren.org/

;; Client-side Web editor/miniKanren system (new!):
;; http://tca.github.io/veneer/editor.html


;; In miniKanren we are concerned with relations rather than functions.

;; functional view of two-argument addition:
(+ 4 5) => 9

;; relational view: the relation +r contains triples of natural numbers
;; for which the first two numbers sum to the third:
(+r 4 5 9)

;; a few of the triples:
(4 5 9) ;; in the +r relation
(4 5 3) ;; not in the +r relation
(4 5 z)  z = 9
(x 5 9)  x = 4
(x y 9)  x = 0, y = 9 or x = 1, y = 8 or ...
(x y z)  x = 0, y = 0, z = 0 or x = 1, y = 0, z = 1 or ...


;; Time for some livecoding...

Vicare Scheme version 0.1d2, 64-bit
Copyright (c) 2006-2010 Abdulaziz Ghuloum and contributors

;; load miniKanren:
> (load "mk.scm")
;; miniKanren has three core logical operators, and one interface operator:
> ; ==, fresh, conde    run
;;
;; let's start with ==, which performs *unification*
(== 3 3)
#<procedure>
;; need to wrap uses of the logical operators within the 'run' interface form:
> (run 1 (x) (== 3 x))
(3)
> (run 2 (x) (== 3 x))
(3)
> (run* (x) (== 3 x))
(3)
> (run* (x) (== 3 3))
(_.0)
> (run* (x) (== x 3))
(3)
> (run* (x) (== 3 4))
()
;; fresh both introduces new logic variables and introduces a
;; conjunction (logic 'and') inside of its body:
> (run* (q)
    (fresh (x y)
      (== 3 x)
      (== 4 y)))
(_.0)
> (run* (q)
    (fresh (x y)
      (== 3 x)
      (== 4 y)
      (== (list x y) q)))
((3 4))
> (run* (x y)
    (fresh ()
      (== 3 x)
      (== 4 y)))
((3 4))
;; 'run' itself can talk multiple query variables, and performs a
;; conjunction of the miniKanren goals in its body.  'run' has an
;; implicit 'fresh' inside of its definition...
> (run* (x y)
    (== 3 x)
    (== 4 y))
((3 4))
> (run* (x y)
    (== 3 x)
    (== 4 y)
    (== 5 x))
()
> (run* (x y)
    (== 3 x)
    (== 4 y)
    (== 3 x))
((3 4))
;; 'conde' is designed to resemble Scheme's 'cond' form.  'conde'
;; performs a disjunction (logical 'or') of its clauses.  Each 'conde'
;; clause performs a conjunction of the goals inside of it.  In other
;; words, 'conde' expresses a disjunction of conjunctions (an 'or' of
;; 'and' expressions).
> (run 1 (x y)
    (conde
      ((== x 3) (== y 4))
      ((== y 1))
      ((== y 2) (== x 0))))
((3 4))
> (run 1 (x y)
    (conde
      ((== y 2) (== x 0))
      ((== x 3) (== y 4))
      ((== y 1))))
((0 2))
> (run 2 (x y)
    (conde
      ((== y 2) (== x 0))
      ((== x 3) (== y 4))
      ((== y 1))))
((0 2) (3 4))
> (run* (x y)
    (conde
      ((== y 2) (== x 0))
      ((== x 3) (== y 4))
      ((== y 1))))
((0 2)
 (3 4)
 (_.0 1))
> (run* (x y)
    (conde
      ((== x 3) (== y 4))
      ((== y 1))
      ((== y 2) (== x 0))))
((3 4)
 (_.0 1)
 (0 2))
;; Scheme's append:
> (define append
    (lambda (l s)
      (cond
        ((null? l) s)
        (else (cons (car l) (append (cdr l) s))))))
> (append '(a b c) '(d e))
(a b c d e)
;; reordering the cond clauses causes trouble!
> (define append
    (lambda (l s)
      (cond
        (#t (cons (car l) (append (cdr l) s)))
        ((null? l) s))))
> (append '(a b c) '(d e))
Unhandled exception
 Condition components:
   1. &assertion
   2. &who: car
   3. &message: "argument does not have required pair structure"
   4. &irritants: (())
;; We can 'guard' the clauses with explicit tests ("Dijkstra guards"):
> (define append
    (lambda (l s)
      (cond
        ((null? l) s)
        ((not (null? l))
         (cons (car l) (append (cdr l) s))))))
;; Now we can safefuly swap the 'cond' clauses:
> (define append
    (lambda (l s)
      (cond
        ((not (null? l))
         (cons (car l) (append (cdr l) s)))
        ((null? l) s))))
> (append '(a b c) '(d e))
(a b c d e)
;; It is this style of guarding the clauses to make sure they make
;; sense individually that we must adopt in miniKanren when using
;; 'conde'.
> (run* (x y)
    (conde
      ((== x 3) (== y 4))
      ((== y 1))
      ((== y 2) (== x 0))))
((3 4) (_.0 1) (0 2))
> (run* (x y)
    (conde
      ((== x 3) (== y 4))
      ((== y 1))
      ((== y 2) (== x 0))))
((3 4) (_.0 1) (0 2))
> (run* (x y)
    (conde
      ((== x 3) (== y 4))
      ((== y 1))
      ((== y 2) (== x 0))))
((3 4) (_.0 1) (0 2))
> 

Process scheme finished
Vicare Scheme version 0.1d2, 64-bit
Copyright (c) 2006-2010 Abdulaziz Ghuloum and contributors

;; Time for relational 'append' ('appendo')
> (load "mk.scm")
;; Babby steps!  Let's just write the base case:
> (define appendo
    (lambda (l s out)
      (conde
        ((== '() l) (== s out)))))
;; It works!
> (run* (q) (appendo '() '(a b c) q))
((a b c))
> (run* (q) (appendo q '(a b c) '(a b c)))
(())
;; Will this work?
> (define appendo
    (lambda (l s out)
      (conde
        ((== '() l) (== s out))
        (else (cons (car l)
                    (append (cdr l) s))))))
Unhandled exception
 Condition components:
   1. &who: else
   2. &message: "incorrect usage of auxiliary keyword"
   3. &syntax:
       form: (else c)
       subform: #f
   4. &trace: #<syntax (else c)>
;; No!  conde doesn't even have a notion of 'else'!
> (define appendo
    (lambda (l s out)
      (conde
        ((== '() l) (== s out))
        ((fresh (a d res)
           (== (cons a d) l)
           (appendo d s res)
           (== (cons a res) out))))))
;; That's more like it.  Unifying 'l' with the pair (cons a d)
;; ensures 'l' is not the empty lists, and acts as our Dijkstra
;; guard.

;; running "forwards" (boring!)
> (run* (q) (appendo '(a b c) '(d e) q))
((a b c d e))
;; running "backwards":
> (run 1 (q) (appendo q '(d e) '(a b c d e)))
((a b c))
;; Is there a second answer?
> (run 2 (q) (appendo q '(d e) '(a b c d e)))
  C-c C-cUnhandled exception
 Condition components:
   1. &interrupted
   2. &message: "received an interrupt signal"
;; Uh oh!  Infinite loop!
;;
;; Better put the recursive call after the == calls:
> (define appendo
    (lambda (l s out)
      (conde
        ((== '() l) (== s out))
        ((fresh (a d res)
           (== (cons a d) l)
           (== (cons a res) out)
           (appendo d s res))))))
;; Can even swap the conde clauses!
> (define appendo
    (lambda (l s out)
      (conde
        ((fresh (a d res)
           (== (cons a d) l)
           (== (cons a res) out)
           (appendo d s res)))
        ((== '() l) (== s out)))))
> (run 1 (q) (appendo q '(d e) '(a b c d e)))
((a b c))
> (run 2 (q) (appendo q '(d e) '(a b c d e)))
((a b c))
;; That's better!
> (run* (q) (appendo q '(d e) '(a b c d e)))
((a b c))
> (define appendo
    (lambda (l s out)
      (conde
        ((fresh (a d res)
           (== (cons a res) out)
           (== (cons a d) l)
           (appendo d s res)))
        ((== '() l) (== s out)))))
> (run* (q) (appendo q '(d e) '(a b c d e)))
((a b c))
> (define appendo
    (lambda (l s out)
      (conde
        ((fresh (a d res)
           (appendo d s res)
           (== (cons a res) out)
           (== (cons a d) l)))
        ((== '() l) (== s out)))))
> (run 1 (q) (appendo q '(d e) '(a b c d e)))
((a b c))
> (run 2 (q) (appendo q '(d e) '(a b c d e)))
  C-c C-cUnhandled exception
 Condition components:
   1. &interrupted
   2. &message: "received an interrupt signal"
> (define appendo
    (lambda (l s out)
      (conde
        ((fresh (a d res)
           (== (cons a res) out)
           (== (cons a d) l)
           (appendo d s res)))
        ((== '() l) (== s out)))))
> (run 2 (q) (appendo q '(d e) '(a b c d e)))
((a b c))
;; More interesting example of running backwards:
> (run* (x y) (appendo x y '(a b c d e)))
((() (a b c d e))
 ((a) (b c d e))
 ((a b) (c d e))
 ((a b c) (d e))
 ((a b c d) (e))
 ((a b c d e) ()))
;; More generally than running "forward" or "backwards",
;; we can also handle partially instantiated arguments:
> (run* (x y) (appendo `(a b . ,x) y '(a b c d e)))
((() (c d e))
 ((c) (d e))
 ((c d) (e))
 ((c d e) ()))
;; This query has no solution...
> (run* (x y) (appendo `(a z b . ,x) y '(a b c d e)))
()
> (run* (x y) (appendo `(a b . ,x) `(d . ,y) '(a b c d e)))
(((c) (e)))
;; We can even leave all three arguments fresh...
> (run 10 (x y z) (appendo x y z))
((() _.0 _.0)
 ((_.0) _.1 (_.0 . _.1))
 ((_.0 _.1) _.2 (_.0 _.1 . _.2))
 ((_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
 ((_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 _.3 . _.4))
 ((_.0 _.1 _.2 _.3 _.4) _.5 (_.0 _.1 _.2 _.3 _.4 . _.5))
 ((_.0 _.1 _.2 _.3 _.4 _.5) _.6 (_.0 _.1 _.2 _.3 _.4 _.5 . _.6))
 ((_.0 _.1 _.2 _.3 _.4 _.5 _.6) _.7 (_.0 _.1 _.2 _.3 _.4 _.5 _.6 . _.7))
 ((_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7) _.8 (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 . _.8))
 ((_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8) _.9 (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 . _.9)))
;; Time to try something more advanced.  A Scheme interpreter
;; implemented as a miniKanren relation.
> (load "interp-with-variadic-lambda.scm")
;; This version of the interpreter has a fairly sophisticated subset
;; of Scheme, include variable argument lambdas ("varargs"), apply,
;; letrec, cons, car, cdr, list, etc.
;;
;; The eval-expo relation takes three arguments:
;;
;; (eval-expo expr env val)
;;
;; where evaluating Scheme expression 'expr' in environment 'env'
;; results in value 'val'.  For our queries we'll set the initial
;; environment to be empty: ().  The 'evalo' relation is just like
;; 'eval-expo', except that it only takes 'expr' and 'val', and sets
;; 'env' to ().
;;
;; First let's evaluate a boolean value:
> (run* (out)
    (eval-expo '#t '() out))
(#t)
;; #t evaluates to itself...
;;
;; How about a cons expression?
> (run* (out)
    (eval-expo '(cons 3 4) '() out))
()
;; This fails, since our interpreter doesn't support numeric
;; expressions.  But the interpreter *does* support 'quote':
> (run* (out)
    (eval-expo '(cons '3 '4) '() out))
((3 . 4))
;; It works!
;;
;; Time to run backwards!  Let's generate a Scheme expression that
;; evaluates to the list (I love you):
> (run 1 (q)
    (eval-expo q '() '(I love you)))
('(I love you))
;; '(I love you) is certainly one such expression.
;; Let' get another expression:
> (run 2 (q)
    (eval-expo q '() '(I love you)))
('(I love you)
 (list 'I 'love 'you))
;; And a third:
> (run 3 (q)
    (eval-expo q '() '(I love you)))
('(I love you) (list 'I 'love 'you)
  (((lambda _.0 '(I love you)))
   (=/= ((_.0 quote)))
   (sym _.0)))
;; In the last answer the Scheme expression is ((lambda _.0 '(I love you))),
;; which is equivalent to ((lambda x '(I love you))).
;; The =/= and sym side constraints let us know _.0 represents a Scheme symbol,
;; and that the symbol can't be 'quote' (since otherwise we'd end up shadowing the
;; built in quote form)
;;
;; Does the last expresion really work?
> ((lambda x '(I love you)))
(I love you)
;; Yep
;;
;; (lambda x '(I love you)) is a variadic function, and takes any number of arguments:
> ((lambda x '(I love you)) 3 4 5 6)
(I love you)
;; Let's get 99 answers:
> (run 99 (q)
    (eval-expo q '() '(I love you)))
('(I love you)
 (list 'I 'love 'you)
 (((lambda _.0 '(I love you)))
  (=/= ((_.0 quote)))
  (sym _.0))
 (((lambda _.0 '(I love you)) '_.1)
  (=/= ((_.0 quote)))
  (sym _.0) (absento (closure _.1)))
 (((lambda _.0 '(I love you)) '_.1 '_.2)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2)))
 ((letrec ((_.0 (lambda _.1 _.2))) '(I love you))
  (=/= ((_.0 quote)))
  (sym _.1))
 (((lambda _.0 '(I love you)) (list))
  (=/= ((_.0 quote)))
  (sym _.0))
 (((lambda _.0 '(I love you)) (lambda _.1 _.2))
  (=/= ((_.0 quote)))
  (sym _.0 _.1))
 ((lambda () '(I love you)))
 ((list ((lambda _.0 'I)) 'love 'you)
  (=/= ((_.0 quote)))
  (sym _.0))
 (((lambda _.0 '(I love you)) '_.1 '_.2 '_.3)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2) (closure _.3)))
 (((lambda _.0 '(I love you)) (list) '_.1)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1)))
 (((lambda _.0 '(I love you)) '_.1 (list))
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1)))
 ((list 'I 'love ((lambda _.0 'you)))
  (=/= ((_.0 quote)))
  (sym _.0))
 (((lambda _.0 '(I love you)) '_.1 (lambda _.2 _.3))
  (=/= ((_.0 quote)))
  (sym _.0 _.2)
  (absento (closure _.1)))
 ((list 'I ((lambda _.0 'love)) 'you)
  (=/= ((_.0 quote)))
  (sym _.0))
 (((lambda _.0 '(I love you)) (lambda _.1 _.2) '_.3)
  (=/= ((_.0 quote)))
  (sym _.0 _.1)
  (absento (closure _.3)))
 ((list ((lambda _.0 'I) '_.1) 'love 'you)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1)))
 (((lambda _.0 _.0) 'I 'love 'you)
  (sym _.0))
 ((list 'I 'love ((lambda _.0 'you) '_.1))
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1)))
 (((lambda _.0 '(I love you)) '_.1 '_.2 '_.3 '_.4)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2) (closure _.3) (closure _.4)))
 (((lambda _.0 '(I love you)) (list) '_.1 '_.2)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2)))
 (((lambda _.0 '(I love you)) '_.1 (list) '_.2)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2)))
 ((letrec ((_.0 (lambda () _.1))) '(I love you))
  (=/= ((_.0 quote))))
 (((lambda _.0 '(I love you)) '_.1 '_.2 (list))
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2)))
 (((lambda _.0 '(I love you)) (list) (list))
  (=/= ((_.0 quote)))
  (sym _.0))
 ((list 'I ((lambda _.0 'love) '_.1) 'you)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1)))
 (((lambda _.0 '(I love you)) '_.1 '_.2 (lambda _.3 _.4))
  (=/= ((_.0 quote)))
  (sym _.0 _.3)
  (absento (closure _.1) (closure _.2)))
 (((lambda _.0 '(I love you)) (lambda () _.1))
  (=/= ((_.0 quote)))
  (sym _.0))
 (((lambda _.0 '(I love you)) (list) (lambda _.1 _.2))
  (=/= ((_.0 quote)))
  (sym _.0 _.1))
 ((list 'I 'love ((lambda _.0 'you) '_.1 '_.2))
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2)))
 ((list 'I 'love (letrec ((_.0 (lambda _.1 _.2))) 'you))
  (=/= ((_.0 quote)))
  (sym _.1))
 (((lambda _.0 '(I love you)) (list '_.1))
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1)))
 ((list 'I 'love ((lambda _.0 'you) (list)))
  (=/= ((_.0 quote)))
  (sym _.0))
 (((lambda _.0 '(I love you)) '_.1 (lambda _.2 _.3) '_.4)
  (=/= ((_.0 quote)))
  (sym _.0 _.2)
  (absento (closure _.1) (closure _.4)))
 ((list ((lambda _.0 'I)) 'love ((lambda _.1 'you)))
  (=/= ((_.0 quote)) ((_.1 quote)))
  (sym _.0 _.1))
 ((list ((lambda _.0 'I)) ((lambda _.1 'love)) 'you)
  (=/= ((_.0 quote)) ((_.1 quote)))
  (sym _.0 _.1))
 (((lambda _.0 '(I love you)) (lambda _.1 _.2) '_.3 '_.4)
  (=/= ((_.0 quote)))
  (sym _.0 _.1)
  (absento (closure _.3) (closure _.4)))
 ((list 'I 'love ((lambda _.0 'you) (lambda _.1 _.2)))
  (=/= ((_.0 quote))) (sym _.0 _.1))
 ((list 'I ((lambda _.0 'love)) ((lambda _.1 'you)))
  (=/= ((_.0 quote)) ((_.1 quote)))
  (sym _.0 _.1))
 (((lambda _.0 '(I love you)) '_.1 '_.2 '_.3 '_.4 '_.5)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2) (closure _.3) (closure _.4) (closure _.5)))
 (((lambda _.0 '(I love you)) (list) '_.1 '_.2 '_.3)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2) (closure _.3)))
 (((lambda _.0 '(I love you)) '_.1 (list) '_.2 '_.3)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2) (closure _.3)))
 (list 'I 'love ((lambda () 'you)))
 ((lambda () (list 'I 'love 'you)))
 ((list ((lambda _.0 'I) '_.1 '_.2) 'love 'you)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2)))
 (((lambda _.0 '(I love you)) '_.1 '_.2 (list) '_.3)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2) (closure _.3)))
 (((lambda _.0 '(I love you)) (lambda _.1 _.2) (list))
  (=/= ((_.0 quote)))
  (sym _.0 _.1))
 ((list ((lambda _.0 'I)) 'love ((lambda _.1 'you) '_.2))
  (=/= ((_.0 quote)) ((_.1 quote)))
  (sym _.0 _.1)
  (absento (closure _.2)))
 (((lambda _.0 '(I love you)) (list) (list) '_.1)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1)))
 (((lambda _.0 '(I love you)) (lambda () _.1) '_.2)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.2)))
 (((lambda _.0 '(I love you)) '_.1 '_.2 '_.3 (list))
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2) (closure _.3)))
 (((lambda _.0 '(I love you)) (list) '_.1 (list))
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1)))
 ((letrec ((_.0 (lambda (_.1) _.2))) '(I love you))
  (=/= ((_.0 quote)))
  (sym _.1))
 (((lambda _.0 '(I love you)) '_.1 (list) (list))
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1)))
 ((list 'I 'love ((lambda _.0 'you) '_.1 '_.2 '_.3))
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2) (closure _.3)))
 ((list 'I ((lambda _.0 'love)) ((lambda _.1 'you) '_.2))
  (=/= ((_.0 quote)) ((_.1 quote)))
  (sym _.0 _.1)
  (absento (closure _.2)))
 (((lambda _.0 '(I love you)) (lambda _.1 _.2) (lambda _.3 _.4))
  (=/= ((_.0 quote)))
  (sym _.0 _.1 _.3))
 (((lambda () ((lambda _.0 '(I love you)))))
  (=/= ((_.0 quote)))
  (sym _.0))
 (((lambda _.0 '(I love you)) '_.1 '_.2 '_.3 (lambda _.4 _.5))
  (=/= ((_.0 quote)))
  (sym _.0 _.4)
  (absento (closure _.1) (closure _.2) (closure _.3)))
 ((list 'I 'love ((lambda _.0 'you) (list) '_.1))
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1)))
 (((lambda _.0 '(I love you)) '_.1 (lambda () _.2))
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1)))
 (((lambda _.0 '(I love you)) (list) '_.1 (lambda _.2 _.3))
  (=/= ((_.0 quote)))
  (sym _.0 _.2)
  (absento (closure _.1)))
 (((lambda _.0 '(I love you)) '_.1 (list) (lambda _.2 _.3))
  (=/= ((_.0 quote)))
  (sym _.0 _.2)
  (absento (closure _.1)))
 ((apply (lambda _.0 '(I love you)) '())
  (=/= ((_.0 quote)))
  (sym _.0))
 ((list (letrec ((_.0 (lambda _.1 _.2))) 'I) 'love 'you)
  (=/= ((_.0 quote)))
  (sym _.1))
 ((list 'I 'love ((lambda _.0 'you) '_.1 (list)))
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1)))
 ((list ((lambda _.0 'I)) ((lambda _.1 'love) '_.2) 'you)
  (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1)
  (absento (closure _.2)))
 ((car '((I love you) . _.0))
  (absento (closure _.0)))
 (((lambda _.0 '(I love you)) '_.1 (list '_.2))
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2)))
 (((lambda (_.0) '(I love you)) '_.1) (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1)))
 ((list ((lambda _.0 'I)) 'love ((lambda _.1 'you) '_.2 '_.3))
  (=/= ((_.0 quote)) ((_.1 quote)))
  (sym _.0 _.1)
  (absento (closure _.2) (closure _.3)))
 ((list 'I 'love ((lambda _.0 'you) '_.1 (lambda _.2 _.3)))
  (=/= ((_.0 quote)))
  (sym _.0 _.2)
  (absento (closure _.1)))
 ((list ((lambda _.0 'I)) 'love (letrec ((_.1 (lambda _.2 _.3))) 'you))
  (=/= ((_.0 quote)) ((_.1 quote)))
  (sym _.0 _.2))
 (((lambda _.0 '(I love you)) '_.1 '_.2 (lambda _.3 _.4) '_.5)
  (=/= ((_.0 quote)))
  (sym _.0 _.3)
  (absento (closure _.1) (closure _.2) (closure _.5)))
 ((list ((lambda _.0 'I) '_.1) 'love ((lambda _.2 'you)))
  (=/= ((_.0 quote)) ((_.2 quote)))
  (sym _.0 _.2)
  (absento (closure _.1)))
 (((lambda (_.0) _.0) '(I love you))
  (sym _.0))
 (((lambda _.0 '(I love you)) (list) (lambda _.1 _.2) '_.3)
  (=/= ((_.0 quote)))
  (sym _.0 _.1)
  (absento (closure _.3)))
 (((lambda _.0 '(I love you)) (list '_.1) '_.2)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2)))
 ((list 'I ((lambda _.0 'love)) ((lambda _.1 'you) '_.2 '_.3))
  (=/= ((_.0 quote)) ((_.1 quote)))
  (sym _.0 _.1)
  (absento (closure _.2) (closure _.3)))
 ((list 'I ((lambda _.0 'love) '_.1 '_.2) 'you)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2)))
 ((list ((lambda _.0 'I) '_.1) ((lambda _.2 'love)) 'you)
  (=/= ((_.0 quote)) ((_.2 quote)))
  (sym _.0 _.2)
  (absento (closure _.1)))
 ((list 'I ((lambda _.0 'love)) (letrec ((_.1 (lambda _.2 _.3))) 'you))
  (=/= ((_.0 quote)) ((_.1 quote)))
  (sym _.0 _.2))
 (((lambda _.0 '(I love you)) '_.1 (lambda _.2 _.3) '_.4 '_.5)
  (=/= ((_.0 quote)))
  (sym _.0 _.2)
  (absento (closure _.1) (closure _.4) (closure _.5)))
 (((lambda () ((lambda _.0 '(I love you)) '_.1)))
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1)))
 ((list ((lambda _.0 'I)) 'love ((lambda _.1 'you) (list)))
  (=/= ((_.0 quote)) ((_.1 quote)))
  (sym _.0 _.1))
 (((lambda _.0 '(I love you)) (lambda _.1 _.2) '_.3 '_.4 '_.5)
  (=/= ((_.0 quote)))
  (sym _.0 _.1)
  (absento (closure _.3) (closure _.4) (closure _.5)))
 ((list 'I ((lambda _.0 'love) '_.1) ((lambda _.2 'you)))
  (=/= ((_.0 quote)) ((_.2 quote)))
  (sym _.0 _.2)
  (absento (closure _.1)))
 ((list 'I 'love ((lambda _.0 'you) (lambda _.1 _.2) '_.3))
  (=/= ((_.0 quote)))
  (sym _.0 _.1)
  (absento (closure _.3)))
 ((list 'I ((lambda _.0 'love)) ((lambda _.1 'you) (list)))
  (=/= ((_.0 quote)) ((_.1 quote)))
  (sym _.0 _.1))
 (((lambda _.0 '(I love you)) '_.1 '_.2 '_.3 '_.4 '_.5 '_.6)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2) (closure _.3) (closure _.4) (closure _.5) (closure _.6)))
 (((lambda _.0 '(I love you)) (list) '_.1 '_.2 '_.3 '_.4)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2) (closure _.3) (closure _.4)))
 ((list ((lambda _.0 'I)) 'love ((lambda _.1 'you) (lambda _.2 _.3)))
  (=/= ((_.0 quote)) ((_.1 quote)))
  (sym _.0 _.1 _.2))
 ((list 'I (letrec ((_.0 (lambda _.1 _.2))) 'love) 'you)
  (=/= ((_.0 quote)))
  (sym _.1))
 (((lambda _.0 '(I love you)) '_.1 (list) '_.2 '_.3 '_.4)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2) (closure _.3) (closure _.4)))
 (((lambda _.0 _.0) ((lambda _.1 'I)) 'love 'you)
  (=/= ((_.1 quote)))
  (sym _.0 _.1))
 (((lambda _.0 '(I love you)) '_.1 '_.2 (list) '_.3 '_.4)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2) (closure _.3) (closure _.4)))
 (((lambda _.0 '(I love you)) (lambda _.1 _.2) (list) '_.3)
  (=/= ((_.0 quote)))
  (sym _.0 _.1)
  (absento (closure _.3)))
 (((lambda _.0 '(I love you)) (list) (list) '_.1 '_.2)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2))))
;; Whew!  Lots of answers!
;;
;; Let's try one of them...
> ((lambda _.0 '(I love you)) (list) (list) '_.1 '_.2)
(I love you)
;; Works!  Although this version of the interpreter like to generate
;; variadic lambdas that ignore the arguments passed in!
;;
;; Here's another of the answers:
> (list 'I ((lambda _.0 'love)) ((lambda _.1 'you) (list)))
(I love you)
;; Let's look at the thousandth answer:
> (car (reverse (run 1000 (q)
    (eval-expo q '() '(I love you)))))
(((lambda _.0 '(I love you)) '_.1 '_.2 '_.3 (list) '_.4
   (list) '_.5)
  (=/= ((_.0 quote))) (sym _.0)
  (absento (closure _.1) (closure _.2) (closure _.3)
    (closure _.4) (closure _.5)))
;; More variadic functions that ignore their arguments...
;;
;; And let's check it...
> ((lambda _.0 '(I love you)) '_.1 '_.2 '_.3 (list) '_.4
   (list) '_.5)
(I love you)
;; Now let's generate a program 'q' that evaluates to itself.
;; Such a program is known as a 'Quine':
;;
;; http://www.nyx.net/~gthompso/quine.htm
> (run 1 (q)
    (eval-expo q '() q))
(#t)
;; Well, the self-evaluating literal #t is indeed a (boring) quine:
> #t
#t
;; as is #f
> (run 2 (q)
    (eval-expo q '() q))
(#t #f)
;; But the third quine is more interesting!
> (run 3 (q)
    (eval-expo q '() q))
(#t #f
  (((lambda (_.0) (list _.0 (list 'quote _.0)))
     '(lambda (_.0) (list _.0 (list 'quote _.0))))
    (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
    (sym _.0)))
;; Here is the Scheme expression:
> ((lambda (_.0) (list _.0 (list 'quote _.0)))
     '(lambda (_.0) (list _.0 (list 'quote _.0))))
((lambda (_.0) (list _.0 (list 'quote _.0)))
  '(lambda (_.0) (list _.0 (list 'quote _.0))))
;; Yep, it evaluates to itself!
;;
;; Now, let's generate twin quines, or "twines", which are two
;; distinct programs 'p' and 'q' that evaluate to each other.  We use
;; the disequality constraint =/= to enforce that 'p' and 'q' are
;; distinct.
> (run 1 (p q)
    (=/= p q)
    (eval-expo p '() q)
    (eval-expo q '() p))
((('((lambda (_.0)
       (list 'quote (list _.0 (list 'quote _.0))))
      '(lambda (_.0)
         (list 'quote (list _.0 (list 'quote _.0)))))
    ((lambda (_.0)
       (list 'quote (list _.0 (list 'quote _.0))))
      '(lambda (_.0)
         (list 'quote (list _.0 (list 'quote _.0))))))
   (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
   (sym _.0)))
;; Let's test...
> '((lambda (_.0)
       (list 'quote (list _.0 (list 'quote _.0))))
      '(lambda (_.0)
         (list 'quote (list _.0 (list 'quote _.0)))))
((lambda (_.0) (list 'quote (list _.0 (list 'quote _.0))))
  '(lambda (_.0) (list 'quote (list _.0 (list 'quote _.0)))))
> ((lambda (_.0) (list 'quote (list _.0 (list 'quote _.0))))
  '(lambda (_.0) (list 'quote (list _.0 (list 'quote _.0)))))
'((lambda (_.0) (list 'quote (list _.0 (list 'quote _.0))))
   '(lambda (_.0) (list 'quote (list _.0 (list 'quote _.0)))))
;; Works!  One of the expression has a quote in front...
;;
;; Here is another, more interesting quine generated from the test
;; 'quines-in-context-of-append' in 'variadic-lambda-tests.scm':
> (apply
   (lambda _.0
     (list 'apply (apply (lambda (_.1) _.1) _.0)
           (apply (lambda _.2 (list 'quote _.0)) '(_.3))))
   '((lambda _.0
       (list 'apply (apply (lambda (_.1) _.1) _.0)
             (apply (lambda _.2 (list 'quote _.0)) '(_.3))))))
(apply
  (lambda _.0
    (list 'apply (apply (lambda (_.1) _.1) _.0)
      (apply (lambda _.2 (list 'quote _.0)) '(_.3))))
  '((lambda _.0
      (list 'apply (apply (lambda (_.1) _.1) _.0)
        (apply (lambda _.2 (list 'quote _.0)) '(_.3))))))
;; Using our relational Scheme interpreter we can take the standard
;; Scheme *functional* definition of 'append', and use it as a *relation*.
;; This is possible because the Scheme interpreter is a relation, not a function.
;;
;; Running forwards, appending (a b c) and (d e)
> (run* (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     q))
((a b c d e))
;; Works!
;;
;; Now lets run it backwards, trying to infer *every* 'x' such that
;; appending 'x' to (b c) yields (a b c):
> (run* (x)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append ,x '(b c)))
     '(a b c)))
  C-c C-cUnhandled exception
 Condition components:
   1. &interrupted
   2. &message: "received an interrupt signal"
;; Infinite loop!  Hmm.  That's weird.  Shouldn't there only be one answer, (a)?
;;
;; Let's ask for just one answer:
> (run 1 (x)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append ,x '(b c)))
     '(a b c)))
('(a))
;; Yep, got back the list (a) as expected.  Let's ask for a second answer for 'x':
> (run 2 (x)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append ,x '(b c)))
     '(a b c)))
('(a)
 (list 'a))
;; Aha!  miniKanren is generating *expressions* that *evaluate* to the list (a).
;; Let's ask for more:
> (run 4 (x)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append ,x '(b c)))
     '(a b c)))
('(a)
 (list 'a)
 (((lambda _.0 '(a)))
  (=/= ((_.0 quote)))
  (sym _.0))
 (((lambda _.0 _.0) 'a)
  (sym _.0)))
;; All of those expressions evaluate to (a).  No wonder run*
;; diverged--there are infinitely many answers!
;;
;; If we want to simulate the behavior of 'appendo', we can just make
;; sure 'x' is quoted:
> (run 4 (x)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append (quote ,x) '(b c)))
     '(a b c)))
((a))
;; Now indeed there is only one answer:
> (run* (x)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append (quote ,x) '(b c)))
     '(a b c)))
((a))
;; We can even place a variable inside the Scheme code for append, and
;; have miniKanren figure out the missing code...
> (run 1 (x)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             ,x
                             (cons (car l) (append (cdr l) s))))))
        (append '(a) '(b c)))
     '(a b c)))
(s)
;; We can also run queries like (I love you) and the quines-generating
;; query withing the context of the 'append' definition:
> (run 10 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        ,q)
     '(I love you)))
('(I love you)
 (list 'I 'love 'you)
 (((lambda _.0 '(I love you)))
  (=/= ((_.0 quote)))
  (sym _.0))
 ((letrec ((_.0 (lambda _.1 _.2))) '(I love you))
  (=/= ((_.0 quote)))
  (sym _.1))
 (((lambda _.0 '(I love you)) '_.1)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1)))
 (((lambda _.0 '(I love you)) append)
  (=/= ((_.0 quote)))
  (sym _.0))
 ((list ((lambda _.0 'I)) 'love 'you)
  (=/= ((_.0 quote)))
  (sym _.0))
 (((lambda _.0 '(I love you)) '_.1 '_.2)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1) (closure _.2)))
 (((lambda _.0 _.0) 'I 'love 'you)
  (sym _.0))
 (((lambda _.0 '(I love you)) '_.1 append)
  (=/= ((_.0 quote)))
  (sym _.0)
  (absento (closure _.1))))
;; Here is one of the answers.  'append' is used in a fairly boring
;; way, as a 'dummy' value passed in to the variadic function:
> ((lambda _.0 '(I love you)) append)
(I love you)
;; more interesting answers can be found in 'variadic-lambda-tests.scm', such as:
(apply (lambda _.0 (apply append '((I love) (you)))) '())

(list (apply (lambda _.0 'I) '()) 'love (append '() 'you))

((lambda _.0 '(I love you)) append append append append)

(apply (lambda _.0 (apply append _.0)) '(() (I love you)))

((lambda _.0 _.0) 'I 'love ((lambda _.1 'you) append))
;; By reordering the conde clauses in the relational interpreter we
;; can alter the search order, affecting which Scheme forms appear in
;; the answers most often.
