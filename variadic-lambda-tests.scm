(load "interp-with-variadic-lambda.scm")
(load "test-check.scm")

;;; These tests accreted over time, while I was experimenting with
;;; different versions of the relational interpreter.  As a result,
;;; many of the tests won't run correctly without modifying the
;;; interpreter.
;;;
;;; The file 'boston-transcript.scm' constains a few queries which
;;; work well with this version of the relational interpeter.
;;;
;;; This test file is intended mostly for inspiration, and as an
;;; example of the sorts of queries can be run against the relational
;;; interpreter.  Please experiment on your own!  You may want to
;;; comment out or reorder various language forms in the relational
;;; interpreter in order to speed up or bias the search.

(define stream-tests
  (lambda ()

    (test "stream-1"
      (run* (q)
        (evalo
         `(letrec ((fives
                    (lambda ()
                      (cons (quote 5)
                            fives))))
            fives)
         q))
      '((closure (lambda () (cons '5 fives)) (ext-rec ((fives (lambda () (cons '5 fives)))) ()))))

    (test "stream-2a"
      (run* (q)
        (evalo
         `(letrec ((fives
                    (lambda ()
                      (cons (quote 5)
                            fives))))
            (car ((cdr (fives)))))
         q))
      '(5))

    (test "stream-2b"
      (run 2 (q)
        (absento '5 q)
        (absento 'cons q)
        (absento 'list q)
        (evalo
         `(letrec ((fives
                    (lambda ()
                      (cons (quote 5)
                            fives))))
            ,q)
         5))
      '((car (fives))
        ((lambda () (car (fives))))))

    (test "stream-2c"
      (run 3 (q)
        (absento '5 q)
        (evalo
         `(letrec ((fives
                    (lambda ()
                      (cons (quote 5)
                            fives))))
            ,q)
         5))
      '((car (fives))
        ((lambda () (car (fives))))
        (((lambda (_.0) (car (fives))) '_.1)
         (=/= ((_.0 car)) ((_.0 fives))) (sym _.0) (absento (5 _.1) (closure _.1)))))

    (test "stream-2d"
      (run 1 (q)
        (absento '5 q)
        (evalo
         `(letrec ((fives
                    (lambda ()
                      (cons (quote 5)
                            fives))))
            ,q)
         '(5 5)))
      '((list (car (fives)) (car (fives)))))

    #|
;;; doesn't come back anytime soon      ;
    (test "stream-2e"
    (run 1 (q)
    (absento '5 q)
    (absento 'list q)
    (evalo
    `(letrec ((fives
    (lambda ()
    (cons (quote 5)
    fives))))
    ,q)
    '(5 5)))
    '???)
    |#

    #|
;;; doesn't come back anytime soon      ;
    (test "stream-2f"
    (run 1 (q)
    (absento '5 q)
    (absento 'list q)
    (evalo
    `(letrec ((fives
    (lambda ()
    (cons (quote 5)
    fives))))
    ,q)
    '(5 . 5)))
    '???)
    |#

    (test "stream-5a"
      (run* (q)
        (evalo
         `(letrec ((nums
                    (lambda (n)
                      (lambda ()
                        (cons n
                              (nums (list 's n)))))))
            (car ((cdr ((cdr ((nums 'z))))))))
         q))
      '((s (s z))))


    (test "stream-3a"
      (run* (q)
        (evalo
         `(letrec ((fives-and-sixes
                    (lambda ()
                      (cons (quote 5)
                            (cons (quote 6)
                                  fives-and-sixes)))))
            (fives-and-sixes))
         q))
      '((5 6 closure
           (lambda () (cons '5 (cons '6 fives-and-sixes)))
           (ext-rec
            ((fives-and-sixes
              (lambda () (cons '5 (cons '6 fives-and-sixes)))))
            ()))))

    (test "stream-3b"
      (run* (q)
        (evalo
         `(letrec ((fives-and-sixes
                    (lambda ()
                      (cons (quote 5)
                            (cons (quote 6)
                                  fives-and-sixes)))))
            (car (fives-and-sixes)))
         q))
      '(5))

    (test "stream-3c"
      (run* (q)
        (evalo
         `(letrec ((fives-and-sixes
                    (lambda ()
                      (cons (quote 5)
                            (cons (quote 6)
                                  fives-and-sixes)))))
            (car (cdr (fives-and-sixes))))
         q))
      '(6))

    (test "stream-3d"
      (run* (q)
        (evalo
         `(letrec ((fives-and-sixes
                    (lambda ()
                      (cons (quote 5)
                            (cons (quote 6)
                                  fives-and-sixes)))))
            ((cdr (cdr (fives-and-sixes)))))
         q))
      '((5 6 closure
           (lambda () (cons '5 (cons '6 fives-and-sixes)))
           (ext-rec
            ((fives-and-sixes
              (lambda () (cons '5 (cons '6 fives-and-sixes)))))
            ()))))

    
    ))

(test "reverse-acc-1"
  (run* (q)
    (evalo
     `(letrec ((reverse-acc
                (lambda (ls acc)
                  (if (null? ls)
                      acc
                      (reverse-acc
                       (cdr ls)
                       (cons (car ls) acc))))))
        (reverse-acc '(5 4 3 2 1) '()))
     q))
  '((1 2 3 4 5)))

(test "reverse-1"
  (run* (q)
    (evalo
     `(letrec ((reverse-acc
                (lambda (ls acc)
                  (if (null? ls)
                      acc
                      (reverse-acc
                       (cdr ls)
                       (cons (car ls) acc))))))
        ((lambda (reverse)
           (reverse '(5 4 3 2 1)))
         (lambda (ls) (reverse-acc ls '()))))
     q))
  '((1 2 3 4 5)))

(test "reverse-smart-1"
  (run* (q)
    (evalo
     `((lambda (reverse)
           (reverse '(5 4 3 2 1)))
         (lambda (ls)
           (letrec ((reverse-acc
                     (lambda (ls acc)
                       (if (null? ls)
                           acc
                           (reverse-acc
                            (cdr ls)
                            (cons (car ls) acc))))))
             (reverse-acc ls '()))))
     q))
  '((1 2 3 4 5)))


(test "I love you reverse-real-1"
  (run 100 (q)
    (evalo
     `((lambda (reverse)
         ,q)
       (lambda (ls)
         (letrec ((reverse-acc
                   (lambda (ls acc)
                     (if (null? ls)
                         acc
                         (reverse-acc
                          (cdr ls)
                          (cons (car ls) acc))))))
           (reverse-acc ls '()))))
     '(I love you)))
  '???)

(test "I love you reverse-acc-1"
  (run 100 (q)
    (absento 'apply q)
    (evalo
     `(letrec ((reverse-acc
                (lambda (ls acc)
                  (if (null? ls)
                      acc
                      (reverse-acc
                       (cdr ls)
                       (cons (car ls) acc))))))
        ,q)
     '(I love you)))
  '???)

(test "I love you reverse-1"
  (run 100 (q)
    (absento 'apply q)
    (evalo
     `(letrec ((reverse-acc
                (lambda (ls acc)
                  (if (null? ls)
                      acc
                      (reverse-acc
                       (cdr ls)
                       (cons (car ls) acc))))))
        ((lambda (reverse)
           ,q)
         (lambda (ls) (reverse-acc ls '()))))
     '(I love you)))
  '???)


(test "reverse-acc-quines-1"
  (run 10 (q)
    (absento 'apply q)
    (evalo
     `(letrec ((reverse-acc
                (lambda (ls acc)
                  (if (null? ls)
                      acc
                      (reverse-acc
                       (cdr ls)
                       (cons (car ls) acc))))))
        ,q)
     q))
  '???)

(test "reverse-quines-1"
  (run 10 (q)
    (absento 'apply q)
    (evalo
     `(letrec ((reverse-acc
                (lambda (ls acc)
                  (if (null? ls)
                      acc
                      (reverse-acc
                       (cdr ls)
                       (cons (car ls) acc))))))
        ((lambda (reverse)
           ,q)
         (lambda (ls) (reverse-acc ls '()))))
     q))
  '???)





;; (lambda x x) should be equivalent to 'list'

(test "1"
  (run* (args)
    (evalo `((lambda x x) (quote f) . ,args) '(a b c d e)))
  '())

(test "2"
  (run 10 (args)
    (evalo `((lambda x x) . ,args) '(a b c d e)))
  '(('a 'b 'c 'd 'e)
    ((((lambda _.0 'a)) 'b 'c 'd 'e) (=/= ((_.0 quote)))
     (sym _.0))
    (('a ((lambda _.0 'b)) 'c 'd 'e) (=/= ((_.0 quote)))
     (sym _.0))
    (('a 'b ((lambda _.0 'c)) 'd 'e) (=/= ((_.0 quote)))
     (sym _.0))
    ((((lambda _.0 'a) '_.1) 'b 'c 'd 'e) (=/= ((_.0 quote)))
     (sym _.0) (absento (closure _.1)))
    (('a ((lambda _.0 'b) '_.1) 'c 'd 'e) (=/= ((_.0 quote)))
     (sym _.0) (absento (closure _.1)))
    (('a 'b 'c 'd ((lambda _.0 'e))) (=/= ((_.0 quote)))
     (sym _.0))
    (('a 'b 'c ((lambda _.0 'd)) 'e) (=/= ((_.0 quote)))
     (sym _.0))
    ((((lambda _.0 'a)) ((lambda _.1 'b)) 'c 'd 'e)
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1))
    (('a 'b ((lambda _.0 'c) '_.1) 'd 'e) (=/= ((_.0 quote)))
     (sym _.0) (absento (closure _.1)))))

(test "3"
  (run 10 (args)
    (evalo `((lambda x x) (quote a) . ,args) '(a b c d e)))
  '(('b 'c 'd 'e)
    ((((lambda _.0 'b)) 'c 'd 'e) (=/= ((_.0 quote)))
     (sym _.0))
    (('b ((lambda _.0 'c)) 'd 'e) (=/= ((_.0 quote)))
     (sym _.0))
    ((((lambda _.0 'b) '_.1) 'c 'd 'e) (=/= ((_.0 quote)))
     (sym _.0) (absento (closure _.1)))
    (('b 'c 'd ((lambda _.0 'e))) (=/= ((_.0 quote)))
     (sym _.0))
    (('b 'c ((lambda _.0 'd)) 'e) (=/= ((_.0 quote)))
     (sym _.0))
    (('b ((lambda _.0 'c) '_.1) 'd 'e) (=/= ((_.0 quote)))
     (sym _.0) (absento (closure _.1)))
    (('b 'c 'd ((lambda _.0 'e) '_.1)) (=/= ((_.0 quote)))
     (sym _.0) (absento (closure _.1)))
    ((((lambda _.0 'b)) ((lambda _.1 'c)) 'd 'e)
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1))
    (('b 'c ((lambda _.0 'd) '_.1) 'e) (=/= ((_.0 quote)))
     (sym _.0) (absento (closure _.1)))))

(test "5"
  (run* (args)
    (evalo `((lambda x (list x)) . ,args) '(a b c d e)))
  '())

(test "6"
  (run 10 (args)
    (evalo `((lambda x (list x)) . ,args) '((a b c d e))))
  '(('a 'b 'c 'd 'e)
    ((((lambda _.0 'a)) 'b 'c 'd 'e) (=/= ((_.0 quote)))
     (sym _.0))
    (('a ((lambda _.0 'b)) 'c 'd 'e) (=/= ((_.0 quote)))
     (sym _.0))
    (('a 'b ((lambda _.0 'c)) 'd 'e) (=/= ((_.0 quote)))
     (sym _.0))
    ((((lambda _.0 'a) '_.1) 'b 'c 'd 'e) (=/= ((_.0 quote)))
     (sym _.0) (absento (closure _.1)))
    (('a ((lambda _.0 'b) '_.1) 'c 'd 'e) (=/= ((_.0 quote)))
     (sym _.0) (absento (closure _.1)))
    (('a 'b 'c 'd ((lambda _.0 'e))) (=/= ((_.0 quote)))
     (sym _.0))
    (('a 'b 'c ((lambda _.0 'd)) 'e) (=/= ((_.0 quote)))
     (sym _.0))
    ((((lambda _.0 'a)) ((lambda _.1 'b)) 'c 'd 'e)
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1))
    (('a 'b ((lambda _.0 'c) '_.1) 'd 'e) (=/= ((_.0 quote)))
     (sym _.0) (absento (closure _.1)))))

(test "7"
  (run 10 (args)
    (evalo `((lambda x (cons x x)) . ,args) '((a b c d e) . (a b c d e))))
  '(('a 'b 'c 'd 'e)
    ((((lambda _.0 'a)) 'b 'c 'd 'e) (=/= ((_.0 quote)))
     (sym _.0))
    (('a ((lambda _.0 'b)) 'c 'd 'e) (=/= ((_.0 quote)))
     (sym _.0))
    (('a 'b ((lambda _.0 'c)) 'd 'e) (=/= ((_.0 quote)))
     (sym _.0))
    ((((lambda _.0 'a) '_.1) 'b 'c 'd 'e) (=/= ((_.0 quote)))
     (sym _.0) (absento (closure _.1)))
    (('a ((lambda _.0 'b) '_.1) 'c 'd 'e) (=/= ((_.0 quote)))
     (sym _.0) (absento (closure _.1)))
    (('a 'b 'c 'd ((lambda _.0 'e))) (=/= ((_.0 quote)))
     (sym _.0))
    (('a 'b 'c ((lambda _.0 'd)) 'e) (=/= ((_.0 quote)))
     (sym _.0))
    ((((lambda _.0 'a)) ((lambda _.1 'b)) 'c 'd 'e)
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1))
    (('a 'b ((lambda _.0 'c) '_.1) 'd 'e) (=/= ((_.0 quote)))
     (sym _.0) (absento (closure _.1)))))

;; demonstrate that 'list' really is equivalent to '(lambda x x)'
(test "4"
  (run 10 (args)
    (evalo `(list . ,args) '(a b c d e)))
  '(('a 'b 'c 'd 'e)
    ((((lambda _.0 'a)) 'b 'c 'd 'e) (=/= ((_.0 quote)))
     (sym _.0))
    (('a ((lambda _.0 'b)) 'c 'd 'e) (=/= ((_.0 quote)))
     (sym _.0))
    (('a 'b ((lambda _.0 'c)) 'd 'e) (=/= ((_.0 quote)))
     (sym _.0))
    ((((lambda _.0 'a) '_.1) 'b 'c 'd 'e) (=/= ((_.0 quote)))
     (sym _.0) (absento (closure _.1)))
    (('a ((lambda _.0 'b) '_.1) 'c 'd 'e) (=/= ((_.0 quote)))
     (sym _.0) (absento (closure _.1)))
    (('a 'b 'c 'd ((lambda _.0 'e))) (=/= ((_.0 quote)))
     (sym _.0))
    (('a 'b 'c ((lambda _.0 'd)) 'e) (=/= ((_.0 quote)))
     (sym _.0))
    ((((lambda _.0 'a)) ((lambda _.1 'b)) 'c 'd 'e)
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1))
    (('a 'b ((lambda _.0 'c) '_.1) 'd 'e) (=/= ((_.0 quote)))
     (sym _.0) (absento (closure _.1)))))


(test "letrec-1"
  (run* (x y)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append (quote ,x) (quote ,y)))
     '(a b c)))
  '((() (a b c)) ((a) (b c)) ((a b) (c)) ((a b c) ())))


(test "apply-1"
  (run 1 (q)
    (evalo `(apply (lambda (x y z) y) (list (quote a) (quote b) (quote c))) q))
  '(b))

(test "apply-2"
  (run 1 (q)
    (evalo `(apply (lambda x (car (cdr x))) (list (quote a) (quote b) (quote c))) q))  
  '(b))

;; correct but slow version
(letrec ((append (lambda (l s)
                   (if (null? l)
                       s
                       (cons (car l) (append (cdr l) s))))))
  (letrec ((append* (lambda x
                      (if (null? x)
                          '()
                          (append (car x) (apply append* (cdr x)))))))
    (append* (quote (a b)) (quote (c)) (quote (d e)))))

(test "apply*o-slow-1"
  (run 1 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (letrec ((append* (lambda x
                            (if (null? x)
                                '()
                                (append (car x) (apply append* (cdr x)))))))
          (append*)))   
     q))
  '(()))

(test "apply*o-slow-2"
  (run 1 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (letrec ((append* (lambda x
                            (if (null? x)
                                '()
                                (append (car x) (apply append* (cdr x)))))))
          (append* (quote (a)))))   
     q))
  '((a)))

(test "apply*o-slow-3"
  (run 1 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (letrec ((append* (lambda x
                            (if (null? x)
                                '()
                                (append (car x) (apply append* (cdr x)))))))
          (append* (quote (a)) (quote (b)))))   
     q))
  '((a b)))


;; faster version, apparently
(letrec ((append (lambda (l s)
                   (if (null? l)
                       s
                       (cons (car l) (append (cdr l) s))))))
  (letrec ((append*-aux (lambda (lol)
                          (if (null? lol)
                              '()
                              (append (car lol) (append*-aux (cdr lol)))))))
    ((lambda (append*)
       (append* (quote (a)) (quote (b)) (quote (c))))
     (lambda x (append*-aux x)))))

(test "apply*o-faster-1"
  (time (run 1 (q)
          (evalo
           `(letrec ((append (lambda (l s)
                               (if (null? l)
                                   s
                                   (cons (car l) (append (cdr l) s))))))
              (letrec ((append*-aux (lambda (lol)
                                      (if (null? lol)
                                          '()
                                          (append (car lol) (append*-aux (cdr lol)))))))
                ((lambda (append*)
                   (append* (quote (a))))
                 (lambda x (append*-aux x)))))
           q)))
  '((a)))

(test "apply*o-faster-2"
  (time (run 1 (q)
          (evalo
           `(letrec ((append (lambda (l s)
                               (if (null? l)
                                   s
                                   (cons (car l) (append (cdr l) s))))))
              (letrec ((append*-aux (lambda (lol)
                                      (if (null? lol)
                                          '()
                                          (append (car lol) (append*-aux (cdr lol)))))))
                ((lambda (append*)
                   (append* (quote (a)) (quote (b))))
                 (lambda x (append*-aux x)))))
           q)))
  '((a b)))


;; ~ 6 seconds under Vicare
;; 
;; Doesn't come back within a few minutes when the second argument to 'evalo' is left fresh.
;; I suspect this is an order of calls issue.
(test "apply*o-faster-3-hint"
  (time (run 1 (q)
          (evalo
           `(letrec ((append (lambda (l s)
                               (if (null? l)
                                   s
                                   (cons (car l) (append (cdr l) s))))))
              (letrec ((append*-aux (lambda (lol)
                                      (if (null? lol)
                                          '()
                                          (append (car lol) (append*-aux (cdr lol)))))))
                ((lambda (append*)
                   (append* (quote (a)) (quote (b)) (quote (c))))
                 (lambda x (append*-aux x)))))
           '(a b c))))
  '(_.0))

;; this version is much faster than the other two
(letrec ((append*-aux (lambda (lol)
                        (if (null? lol)
                            '()
                            (if (null? (car lol))
                                (append*-aux (cdr lol))
                                (cons (car (car lol)) (append*-aux (cons (cdr (car lol)) (cdr lol)))))))))
  ((lambda (append*)
     (append* (quote (a)) (quote (b)) (quote (c))))
   (lambda x (append*-aux x))))

(test "apply*o-faster-still-1"
  (time (run 1 (q)
          (evalo
           `(letrec ((append*-aux (lambda (lol)
                                    (if (null? lol)
                                        '()
                                        (if (null? (car lol))
                                            (append*-aux (cdr lol))
                                            (cons (car (car lol)) (append*-aux (cons (cdr (car lol)) (cdr lol)))))))))
              ((lambda (append*)
                 (append* (quote (a))))
               (lambda x (append*-aux x))))
           q)))
  '((a)))

(test "apply*o-faster-still-2"
  (time (run 1 (q)
          (evalo
           `(letrec ((append*-aux (lambda (lol)
                                    (if (null? lol)
                                        '()
                                        (if (null? (car lol))
                                            (append*-aux (cdr lol))
                                            (cons (car (car lol)) (append*-aux (cons (cdr (car lol)) (cdr lol)))))))))
              ((lambda (append*)
                 (append* (quote (a)) (quote (b))))
               (lambda x (append*-aux x))))
           q)))
  '((a b)))

(test "apply*o-faster-still-3-hint"
  (time (run 1 (q)
          (evalo
           `(letrec ((append*-aux (lambda (lol)
                                    (if (null? lol)
                                        '()
                                        (if (null? (car lol))
                                            (append*-aux (cdr lol))
                                            (cons (car (car lol)) (append*-aux (cons (cdr (car lol)) (cdr lol)))))))))
              ((lambda (append*)
                 (append* (quote (a)) (quote (b)) (quote (c))))
               (lambda x (append*-aux x))))
           '(a b c))))
  '(_.0))

(test "apply*o-faster-still-3-no-hint"
  (time (run 1 (q)
          (evalo
           `(letrec ((append*-aux (lambda (lol)
                                    (if (null? lol)
                                        '()
                                        (if (null? (car lol))
                                            (append*-aux (cdr lol))
                                            (cons (car (car lol)) (append*-aux (cons (cdr (car lol)) (cdr lol)))))))))
              ((lambda (append*)
                 (append* (quote (a)) (quote (b)) (quote (c))))
               (lambda x (append*-aux x))))
           q)))
  '((a b c)))

(test "faster-still-append*-backwards-1"
  (time (run 5 (q)
          (evalo
           `(letrec ((append*-aux (lambda (lol)
                                    (if (null? lol)
                                        '()
                                        (if (null? (car lol))
                                            (append*-aux (cdr lol))
                                            (cons (car (car lol)) (append*-aux (cons (cdr (car lol)) (cdr lol)))))))))
              ((lambda (append*)
                 (append* . ,q))
               (lambda x (append*-aux x))))
           '(a b c))))
  '(('(a b c)) ((list 'a 'b 'c))
    (((letrec ((_.0 (lambda _.1 _.2))) '(a b c)))
     (=/= ((_.0 quote))) (sym _.1))
    (((list (letrec ((_.0 (lambda _.1 _.2))) 'a) 'b 'c))
     (=/= ((_.0 quote))) (sym _.1))
    ('(a b c) '())))

(test "faster-still-append*-backwards-2"
  ;; Includes answers like:
  ;;
  ;; ((list 'a 'b))
  ;;
  ;; ((list 'a) '(b))
  ;;
  ;; ('(a b) (apply append*-aux '(())))
  ;;
  ;; ((((lambda _.0 '(a b)) '_.1 append*)) (=/= ((_.0 quote))) (sym _.0) (absento (closure _.1)))
  ;;
  ;;
  (time (length (run 100 (q)
                  (evalo
                   `(letrec ((append*-aux (lambda (lol)
                                            (if (null? lol)
                                                '()
                                                (if (null? (car lol))
                                                    (append*-aux (cdr lol))
                                                    (cons (car (car lol)) (append*-aux (cons (cdr (car lol)) (cdr lol)))))))))
                      ((lambda (append*)
                         (append* . ,q))
                       (lambda x (append*-aux x))))
                   '(a b)))))
  100)


(test "faster-still-append*-backwards-3"
  ;; cool answers (from a run 1000, actually...):
  ;;
  ;; (((apply (lambda (_.0) _.0) '((I love you)))) (sym _.0))
  ;;
  ;; (((list 'I 'love 'you) ((lambda _.0 (list)) (list)))
  ;;   (=/= ((_.0 list))) (sym _.0))
  ;;
  ;; ('(I love you)
  ;;  (apply append* '(() () () () () () () () () () () ())))
  (time (length (run 500 (q)
                  (evalo
                   `(letrec ((append*-aux (lambda (lol)
                                            (if (null? lol)
                                                '()
                                                (if (null? (car lol))
                                                    (append*-aux (cdr lol))
                                                    (cons (car (car lol)) (append*-aux (cons (cdr (car lol)) (cdr lol)))))))))
                      ((lambda (append*)
                         (append* . ,q))
                       (lambda x (append*-aux x))))
                   '(I love you)))))
  500)


(test "I love you 1000"
  (time (length (run 1000 (q) (evalo q '(I love you)))))
  1000)

(test "I-love-you-in-context-of-append"
  #|

  cool examples
  
  (apply (lambda _.0 (apply append '((I love) (you)))) '())

  (list (apply (lambda _.0 'I) '()) 'love (append '() 'you))

  ((lambda _.0 '(I love you)) append append append append)

  (apply (lambda _.0 (apply append _.0)) '(() (I love you)))

  ((lambda _.0 _.0) 'I 'love ((lambda _.1 'you) append))
  
  |#
  (run 1000 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        ,q)
     '(I love you)))
  '???)

;; Cool quines:
(test "cool quines"
  (run 5 (q) (evalo q q))
  '(#t
    #f
    (((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))
    ((apply
      (lambda (_.0)
        (list 'apply _.0 (list 'quote (list _.0))))
      '((lambda (_.0)
          (list 'apply _.0 (list 'quote (list _.0))))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))
    ((apply (lambda _.0 (list 'apply _.0 (list 'quote _.0)))
            '(lambda _.0 (list 'apply _.0 (list 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))))

;; Other cool quines:
;;
;; (apply
;;  (lambda (_.0)
;;    (list (apply (lambda _.1 'apply) '()) _.0
;;          (list 'quote (list _.0))))
;;  '((lambda (_.0)
;;      (list (apply (lambda _.1 'apply) '()) _.0
;;            (list 'quote (list _.0))))))
;;
;; ((lambda (_.0)
;;    (list (apply (lambda _.1 _.0) '()) (list 'quote _.0)))
;;  '(lambda (_.0)
;;     (list (apply (lambda _.1 _.0) '()) (list 'quote _.0))))
;;
;; (apply
;;  (lambda _.0
;;    (list 'apply (apply (lambda (_.1) _.1) _.0)
;;          (apply (lambda _.2 (list 'quote _.0)) '(_.3))))
;;  '((lambda _.0
;;      (list 'apply (apply (lambda (_.1) _.1) _.0)
;;            (apply (lambda _.2 (list 'quote _.0)) '(_.3))))))

(test "quines-in-context-of-append"
#|  
  Here are the super cool answers, which actually use 'append':
  
  (apply
   (lambda _.0
     (list 'apply (apply append _.0) (list 'quote _.0)))
   '(()
     (lambda _.0
       (list 'apply (apply append _.0) (list 'quote _.0)))))
  
  (apply
   (lambda _.0
     (list 'apply (apply append _.0)
           (apply (lambda _.1 (list 'quote _.1)) _.0)))
   '(()
     (lambda _.0
       (list 'apply (apply append _.0)
             (apply (lambda _.1 (list 'quote _.1)) _.0)))))  

  (apply
   (lambda _.0
     (list 'apply (apply append _.0)
           (apply (lambda _.1 (list 'quote _.0)) _.0)))
   '(()
     (lambda _.0
       (list 'apply (apply append _.0)
             (apply (lambda _.1 (list 'quote _.0)) _.0)))))

  (apply
   (lambda _.0
     (list 'apply (apply append _.0)
           (list (apply (lambda _.1 'quote) _.0) _.0)))
   '(()
     (lambda _.0
       (list 'apply (apply append _.0)
             (list (apply (lambda _.1 'quote) _.0) _.0)))))

  (apply
   (lambda _.0
     (list (apply (lambda _.1 'apply) _.0)
           (apply append _.0) (list 'quote _.0)))
   '(()
     (lambda _.0
       (list (apply (lambda _.1 'apply) _.0)
             (apply append _.0) (list 'quote _.0)))))  
|#

  (run 60 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        ,q)
     q))
  '(#t
    #f
    ((apply (lambda _.0 (list 'apply _.0 (list 'quote _.0)))
            '(lambda _.0 (list 'apply _.0 (list 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))
    ((apply
      (lambda (_.0)
        (list 'apply _.0 (list 'quote (list _.0))))
      '((lambda (_.0)
          (list 'apply _.0 (list 'quote (list _.0))))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))
    (((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))
    ((apply
      (lambda _.0
        (list 'apply (list 'lambda '_.0 _.0)
              (list 'quote _.0)))
      '(list 'apply (list 'lambda '_.0 _.0) (list 'quote _.0)))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))
    ((apply
      (lambda _.0
        (list (apply (lambda _.1 'apply) _.0) _.0
              (list 'quote _.0)))
      '(lambda _.0
         (list (apply (lambda _.1 'apply) _.0) _.0
               (list 'quote _.0))))
     (=/= ((_.0 apply)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure))
          ((_.1 quote)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0 (list 'apply (car _.0) (list 'quote _.0)))
      '((lambda _.0 (list 'apply (car _.0) (list 'quote _.0)))))
     (=/= ((_.0 car)) ((_.0 closure)) ((_.0 list))
          ((_.0 quote)))
     (sym _.0))
    (((lambda (_.0)
        (list (list 'lambda '(_.0) _.0) (list 'quote _.0)))
      '(list (list 'lambda '(_.0) _.0) (list 'quote _.0)))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))
    ((apply
      (lambda (_.0)
        (list 'apply (list 'lambda '(_.0) _.0)
              (list 'quote (list _.0))))
      '((list 'apply (list 'lambda '(_.0) _.0)
              (list 'quote (list _.0)))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))
    ((apply
      (lambda (_.0)
        (list 'apply _.0 (list 'list (list 'quote _.0))))
      (list
       '(lambda (_.0)
          (list 'apply _.0 (list 'list (list 'quote _.0))))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))
    ((apply
      (lambda _.0
        (list (apply (lambda _.1 'apply) '()) _.0
              (list 'quote _.0)))
      '(lambda _.0
         (list (apply (lambda _.1 'apply) '()) _.0
               (list 'quote _.0))))
     (=/= ((_.0 apply)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure))
          ((_.1 quote)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list 'apply (car _.0)
              (apply (lambda _.1 (list 'quote _.1)) _.0)))
      '((lambda _.0
          (list 'apply (car _.0)
                (apply (lambda _.1 (list 'quote _.1)) _.0)))))
     (=/= ((_.0 apply)) ((_.0 car)) ((_.0 closure))
          ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
          ((_.1 closure)) ((_.1 list)) ((_.1 quote)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list 'apply (apply (lambda _.1 _.1) _.0)
              (list 'quote _.0)))
      '(lambda _.0
         (list 'apply (apply (lambda _.1 _.1) _.0)
               (list 'quote _.0))))
     (=/= ((_.0 apply)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list 'apply (apply append _.0) (list 'quote _.0)))
      '(()
        (lambda _.0
          (list 'apply (apply append _.0) (list 'quote _.0)))))
     (=/= ((_.0 append)) ((_.0 apply)) ((_.0 closure))
          ((_.0 list)) ((_.0 quote)))
     (sym _.0))
    ((apply
      (lambda _.0
        (list 'apply (car _.0)
              (apply (lambda _.1 (list 'quote _.0)) _.0)))
      '((lambda _.0
          (list 'apply (car _.0)
                (apply (lambda _.1 (list 'quote _.0)) _.0)))))
     (=/= ((_.0 _.1)) ((_.0 apply)) ((_.0 car))
          ((_.0 closure)) ((_.0 lambda)) ((_.0 list))
          ((_.0 quote)) ((_.1 closure)) ((_.1 list))
          ((_.1 quote)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list 'apply (car _.0)
              (list (apply (lambda _.1 'quote) _.0) _.0)))
      '((lambda _.0
          (list 'apply (car _.0)
                (list (apply (lambda _.1 'quote) _.0) _.0)))))
     (=/= ((_.0 apply)) ((_.0 car)) ((_.0 closure))
          ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
          ((_.1 closure)) ((_.1 quote)))
     (sym _.0 _.1))
    (((lambda (_.0)
        (list _.0 (apply (lambda _.1 (list 'quote _.1)) _.0)))
      '(lambda (_.0)
         (list _.0 (apply (lambda _.1 (list 'quote _.1)) _.0))))
     (=/= ((_.0 apply)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure))
          ((_.1 list)) ((_.1 quote)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (apply
         (lambda _.1 (list 'apply _.1 (list 'quote _.1)))
         _.0))
      '(lambda _.0
         (apply
          (lambda _.1 (list 'apply _.1 (list 'quote _.1)))
          _.0)))
     (=/= ((_.0 apply)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure))
          ((_.1 list)) ((_.1 quote)))
     (sym _.0 _.1))
    (((lambda (_.0)
        (list _.0 (apply (lambda _.1 (list 'quote _.0)) _.0)))
      '(lambda (_.0)
         (list _.0 (apply (lambda _.1 (list 'quote _.0)) _.0))))
     (=/= ((_.0 _.1)) ((_.0 apply)) ((_.0 closure))
          ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
          ((_.1 closure)) ((_.1 list)) ((_.1 quote)))
     (sym _.0 _.1))
    (((lambda (_.0)
        (list _.0 (list (apply (lambda _.1 'quote) _.0) _.0)))
      '(lambda (_.0)
         (list _.0 (list (apply (lambda _.1 'quote) _.0) _.0))))
     (=/= ((_.0 apply)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure))
          ((_.1 quote)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list (apply (lambda _.1 'apply) _.0) (car _.0)
              (list 'quote _.0)))
      '((lambda _.0
          (list (apply (lambda _.1 'apply) _.0) (car _.0)
                (list 'quote _.0)))))
     (=/= ((_.0 apply)) ((_.0 car)) ((_.0 closure))
          ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
          ((_.1 closure)) ((_.1 quote)))
     (sym _.0 _.1))
    ((apply
      (lambda (_.0)
        (list 'apply _.0
              (list (apply (lambda _.1 'quote) _.0) (list _.0))))
      '((lambda (_.0)
          (list 'apply _.0
                (list (apply (lambda _.1 'quote) _.0) (list _.0))))))
     (=/= ((_.0 apply)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure))
          ((_.1 quote)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list 'apply (car _.0)
              (list (apply (lambda _.1 'quote) '()) _.0)))
      '((lambda _.0
          (list 'apply (car _.0)
                (list (apply (lambda _.1 'quote) '()) _.0)))))
     (=/= ((_.0 apply)) ((_.0 car)) ((_.0 closure))
          ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
          ((_.1 closure)) ((_.1 quote)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list (apply (lambda _.1 'apply) _.0)
              (list 'lambda '_.0 _.0) (list 'quote _.0)))
      '(list (apply (lambda _.1 'apply) _.0)
             (list 'lambda '_.0 _.0) (list 'quote _.0)))
     (=/= ((_.0 apply)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure))
          ((_.1 quote)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list 'apply (apply (lambda _.1 _.0) _.0)
              (list 'quote _.0)))
      '(lambda _.0
         (list 'apply (apply (lambda _.1 _.0) _.0)
               (list 'quote _.0))))
     (=/= ((_.0 _.1)) ((_.0 apply)) ((_.0 closure))
          ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
          ((_.1 closure)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list 'apply (car _.0)
              (apply (lambda _.1 (list 'quote _.0)) '())))
      '((lambda _.0
          (list 'apply (car _.0)
                (apply (lambda _.1 (list 'quote _.0)) '())))))
     (=/= ((_.0 _.1)) ((_.0 apply)) ((_.0 car))
          ((_.0 closure)) ((_.0 lambda)) ((_.0 list))
          ((_.0 quote)) ((_.1 closure)) ((_.1 list))
          ((_.1 quote)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list 'apply (car _.0)
              (list 'quote (apply (lambda _.1 _.1) _.0))))
      '((lambda _.0
          (list 'apply (car _.0)
                (list 'quote (apply (lambda _.1 _.1) _.0))))))
     (=/= ((_.0 apply)) ((_.0 car)) ((_.0 closure))
          ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
          ((_.1 closure)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (apply
         (lambda _.1 (list 'apply _.1 (list 'quote _.0)))
         _.0))
      '(lambda _.0
         (apply
          (lambda _.1 (list 'apply _.1 (list 'quote _.0)))
          _.0)))
     (=/= ((_.0 _.1)) ((_.0 apply)) ((_.0 closure))
          ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
          ((_.1 closure)) ((_.1 list)) ((_.1 quote)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list 'apply (car _.0)
              (list 'quote (apply (lambda _.1 _.0) _.0))))
      '((lambda _.0
          (list 'apply (car _.0)
                (list 'quote (apply (lambda _.1 _.0) _.0))))))
     (=/= ((_.0 _.1)) ((_.0 apply)) ((_.0 car))
          ((_.0 closure)) ((_.0 lambda)) ((_.0 list))
          ((_.0 quote)) ((_.1 closure)))
     (sym _.0 _.1))
    (((lambda (_.0) ((lambda _.1 _.1) _.0 (list 'quote _.0)))
      '(lambda (_.0) ((lambda _.1 _.1) _.0 (list 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 list))
          ((_.0 quote)) ((_.1 closure)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0 (list 'apply (car _.0) (list 'quote _.0)))
      '((lambda _.0 (list 'apply (car _.0) (list 'quote _.0)))
        _.1))
     (=/= ((_.0 car)) ((_.0 closure)) ((_.0 list))
          ((_.0 quote)))
     (sym _.0) (absento (closure _.1)))
    ((apply
      (lambda _.0
        (list 'apply (car _.0) ((lambda _.1 _.1) 'quote _.0)))
      '((lambda _.0
          (list 'apply (car _.0)
                ((lambda _.1 _.1) 'quote _.0)))))
     (=/= ((_.0 car)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure)))
     (sym _.0 _.1))
    (((lambda (_.0)
        (list (apply (lambda _.1 _.1) _.0) (list 'quote _.0)))
      '(lambda (_.0)
         (list (apply (lambda _.1 _.1) _.0) (list 'quote _.0))))
     (=/= ((_.0 apply)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list (apply (lambda _.1 'apply) '(_.2)) _.0
              (list 'quote _.0)))
      '(lambda _.0
         (list (apply (lambda _.1 'apply) '(_.2)) _.0
               (list 'quote _.0))))
     (=/= ((_.0 apply)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure))
          ((_.1 quote)))
     (sym _.0 _.1) (absento (closure _.2)))
    ((apply
      (lambda _.0
        (list 'apply (car _.0)
              (list 'quote (apply (lambda _.1 _.0) '()))))
      '((lambda _.0
          (list 'apply (car _.0)
                (list 'quote (apply (lambda _.1 _.0) '()))))))
     (=/= ((_.0 _.1)) ((_.0 apply)) ((_.0 car))
          ((_.0 closure)) ((_.0 lambda)) ((_.0 list))
          ((_.0 quote)) ((_.1 closure)))
     (sym _.0 _.1))
    ((apply
      (lambda (_.0)
        (list 'apply (list 'lambda '(_.0) _.0)
              (list 'list (list 'quote _.0))))
      (list
       '(list 'apply (list 'lambda '(_.0) _.0)
              (list 'list (list 'quote _.0)))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))
    ((apply
      (lambda _.0
        (list 'apply _.0
              (apply (lambda _.1 (list 'quote _.1)) _.0)))
      '(lambda _.0
         (list 'apply _.0
               (apply (lambda _.1 (list 'quote _.1)) _.0))))
     (=/= ((_.0 apply)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure))
          ((_.1 list)) ((_.1 quote)))
     (sym _.0 _.1))
    ((apply
      (lambda (_.0)
        (list 'apply (apply (lambda _.1 _.1) _.0)
              (list 'quote (list _.0))))
      '((lambda (_.0)
          (list 'apply (apply (lambda _.1 _.1) _.0)
                (list 'quote (list _.0))))))
     (=/= ((_.0 apply)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure)))
     (sym _.0 _.1))
    ((apply
      (lambda (_.0)
        (list 'apply _.0
              (apply (lambda _.1 (list 'quote (list _.1))) _.0)))
      '((lambda (_.0)
          (list 'apply _.0
                (apply (lambda _.1 (list 'quote (list _.1))) _.0)))))
     (=/= ((_.0 apply)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure))
          ((_.1 list)) ((_.1 quote)))
     (sym _.0 _.1))
    (((lambda (_.0)
        (list _.0 (list (apply (lambda _.1 'quote) '()) _.0)))
      '(lambda (_.0)
         (list _.0 (list (apply (lambda _.1 'quote) '()) _.0))))
     (=/= ((_.0 apply)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure))
          ((_.1 quote)))
     (sym _.0 _.1))
    (((lambda (_.0)
        (list _.0 (apply (lambda _.1 (list 'quote _.0)) '())))
      '(lambda (_.0)
         (list _.0 (apply (lambda _.1 (list 'quote _.0)) '()))))
     (=/= ((_.0 _.1)) ((_.0 apply)) ((_.0 closure))
          ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
          ((_.1 closure)) ((_.1 list)) ((_.1 quote)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list 'apply (apply append _.0)
              (apply (lambda _.1 (list 'quote _.1)) _.0)))
      '(()
        (lambda _.0
          (list 'apply (apply append _.0)
                (apply (lambda _.1 (list 'quote _.1)) _.0)))))
     (=/= ((_.0 append)) ((_.0 apply)) ((_.0 closure))
          ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
          ((_.1 closure)) ((_.1 list)) ((_.1 quote)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list 'apply (car _.0)
              (list 'quote (apply (lambda _.1 _.0) '(_.2)))))
      '((lambda _.0
          (list 'apply (car _.0)
                (list 'quote (apply (lambda _.1 _.0) '(_.2)))))))
     (=/= ((_.0 _.1)) ((_.0 apply)) ((_.0 car))
          ((_.0 closure)) ((_.0 lambda)) ((_.0 list))
          ((_.0 quote)) ((_.1 closure)))
     (sym _.0 _.1) (absento (closure _.2)))
    ((apply
      (lambda _.0
        (list 'apply (car _.0)
              (apply (lambda _.1 (list 'quote _.0)) '(_.2))))
      '((lambda _.0
          (list 'apply (car _.0)
                (apply (lambda _.1 (list 'quote _.0)) '(_.2))))))
     (=/= ((_.0 _.1)) ((_.0 apply)) ((_.0 car))
          ((_.0 closure)) ((_.0 lambda)) ((_.0 list))
          ((_.0 quote)) ((_.1 closure)) ((_.1 list))
          ((_.1 quote)))
     (sym _.0 _.1) (absento (closure _.2)))
    ((apply
      (lambda (_.0)
        (list 'apply _.0
              (list (apply (lambda _.1 'quote) '()) (list _.0))))
      '((lambda (_.0)
          (list 'apply _.0
                (list (apply (lambda _.1 'quote) '()) (list _.0))))))
     (=/= ((_.0 apply)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure))
          ((_.1 quote)))
     (sym _.0 _.1))
    (((lambda (_.0)
        (list _.0 (list 'quote (apply (lambda _.1 _.1) _.0))))
      '(lambda (_.0)
         (list _.0 (list 'quote (apply (lambda _.1 _.1) _.0)))))
     (=/= ((_.0 apply)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list 'apply (apply append _.0)
              (apply (lambda _.1 (list 'quote _.0)) _.0)))
      '(()
        (lambda _.0
          (list 'apply (apply append _.0)
                (apply (lambda _.1 (list 'quote _.0)) _.0)))))
     (=/= ((_.0 _.1)) ((_.0 append)) ((_.0 apply))
          ((_.0 closure)) ((_.0 lambda)) ((_.0 list))
          ((_.0 quote)) ((_.1 closure)) ((_.1 list))
          ((_.1 quote)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list 'apply _.0
              (apply (lambda _.1 (list 'quote _.0)) _.0)))
      '(lambda _.0
         (list 'apply _.0
               (apply (lambda _.1 (list 'quote _.0)) _.0))))
     (=/= ((_.0 _.1)) ((_.0 apply)) ((_.0 closure))
          ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
          ((_.1 closure)) ((_.1 list)) ((_.1 quote)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list 'apply (apply append _.0)
              (list (apply (lambda _.1 'quote) _.0) _.0)))
      '(()
        (lambda _.0
          (list 'apply (apply append _.0)
                (list (apply (lambda _.1 'quote) _.0) _.0)))))
     (=/= ((_.0 append)) ((_.0 apply)) ((_.0 closure))
          ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
          ((_.1 closure)) ((_.1 quote)))
     (sym _.0 _.1))
    ((apply
      (lambda (_.0)
        (list 'apply _.0
              (apply (lambda _.1 (list 'quote (list _.0))) _.0)))
      '((lambda (_.0)
          (list 'apply _.0
                (apply (lambda _.1 (list 'quote (list _.0))) _.0)))))
     (=/= ((_.0 _.1)) ((_.0 apply)) ((_.0 closure))
          ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
          ((_.1 closure)) ((_.1 list)) ((_.1 quote)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list 'apply _.0
              (list (apply (lambda _.1 'quote) _.0) _.0)))
      '(lambda _.0
         (list 'apply _.0
               (list (apply (lambda _.1 'quote) _.0) _.0))))
     (=/= ((_.0 apply)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure))
          ((_.1 quote)))
     (sym _.0 _.1))
    (((lambda (_.0)
        (list _.0 (list 'quote (apply (lambda _.1 _.0) _.0))))
      '(lambda (_.0)
         (list _.0 (list 'quote (apply (lambda _.1 _.0) _.0)))))
     (=/= ((_.0 _.1)) ((_.0 apply)) ((_.0 closure))
          ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
          ((_.1 closure)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list (apply (lambda _.1 'apply) _.0) (car _.0)
              (apply (lambda _.2 (list 'quote _.2)) _.0)))
      '((lambda _.0
          (list (apply (lambda _.1 'apply) _.0) (car _.0)
                (apply (lambda _.2 (list 'quote _.2)) _.0)))))
     (=/= ((_.0 apply)) ((_.0 car)) ((_.0 closure))
          ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
          ((_.1 closure)) ((_.1 quote)) ((_.2 closure))
          ((_.2 list)) ((_.2 quote)))
     (sym _.0 _.1 _.2))
    (((lambda (_.0) (list _.0 ((lambda _.1 _.1) 'quote _.0)))
      '(lambda (_.0) (list _.0 ((lambda _.1 _.1) 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 list))
          ((_.0 quote)) ((_.1 closure)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        ((lambda _.1 _.1) 'apply _.0 (list 'quote _.0)))
      '(lambda _.0
         ((lambda _.1 _.1) 'apply _.0 (list 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 list))
          ((_.0 quote)) ((_.1 closure)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list 'apply (car _.0)
              (list (apply (lambda _.1 'quote) '(_.2)) _.0)))
      '((lambda _.0
          (list 'apply (car _.0)
                (list (apply (lambda _.1 'quote) '(_.2)) _.0)))))
     (=/= ((_.0 apply)) ((_.0 car)) ((_.0 closure))
          ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
          ((_.1 closure)) ((_.1 quote)))
     (sym _.0 _.1) (absento (closure _.2)))
    ((apply
      (lambda _.0
        (apply
         (lambda _.1 (list 'apply _.0 (list 'quote _.1)))
         _.0))
      '(lambda _.0
         (apply
          (lambda _.1 (list 'apply _.0 (list 'quote _.1)))
          _.0)))
     (=/= ((_.0 _.1)) ((_.0 apply)) ((_.0 closure))
          ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
          ((_.1 closure)) ((_.1 list)) ((_.1 quote)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list (apply (lambda _.1 'apply) _.0) (car _.0)
              (apply (lambda _.2 (list 'quote _.0)) _.0)))
      '((lambda _.0
          (list (apply (lambda _.1 'apply) _.0) (car _.0)
                (apply (lambda _.2 (list 'quote _.0)) _.0)))))
     (=/= ((_.0 _.2)) ((_.0 apply)) ((_.0 car))
          ((_.0 closure)) ((_.0 lambda)) ((_.0 list))
          ((_.0 quote)) ((_.1 closure)) ((_.1 quote))
          ((_.2 closure)) ((_.2 list)) ((_.2 quote)))
     (sym _.0 _.1 _.2))
    ((apply
      (lambda _.0
        (list (apply (lambda _.1 'apply) _.0)
              (apply append _.0) (list 'quote _.0)))
      '(()
        (lambda _.0
          (list (apply (lambda _.1 'apply) _.0)
                (apply append _.0) (list 'quote _.0)))))
     (=/= ((_.0 append)) ((_.0 apply)) ((_.0 closure))
          ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
          ((_.1 closure)) ((_.1 quote)))
     (sym _.0 _.1))))

(test "quines-in-context-of-append"
#|  
  Here are the super cool answers, which actually use 'append':
  
  (apply
   (lambda _.0
     (list 'apply (apply append _.0) (list 'quote _.0)))
   '(()
     (lambda _.0
       (list 'apply (apply append _.0) (list 'quote _.0)))))
  
  (apply
   (lambda _.0
     (list 'apply (apply append _.0)
           (apply (lambda _.1 (list 'quote _.1)) _.0)))
   '(()
     (lambda _.0
       (list 'apply (apply append _.0)
             (apply (lambda _.1 (list 'quote _.1)) _.0)))))  

  (apply
   (lambda _.0
     (list 'apply (apply append _.0)
           (apply (lambda _.1 (list 'quote _.0)) _.0)))
   '(()
     (lambda _.0
       (list 'apply (apply append _.0)
             (apply (lambda _.1 (list 'quote _.0)) _.0)))))

  (apply
   (lambda _.0
     (list 'apply (apply append _.0)
           (list (apply (lambda _.1 'quote) _.0) _.0)))
   '(()
     (lambda _.0
       (list 'apply (apply append _.0)
             (list (apply (lambda _.1 'quote) _.0) _.0)))))

  (apply
   (lambda _.0
     (list (apply (lambda _.1 'apply) _.0)
           (apply append _.0) (list 'quote _.0)))
   '(()
     (lambda _.0
       (list (apply (lambda _.1 'apply) _.0)
             (apply append _.0) (list 'quote _.0)))))  
|#


(test "rember-1"
  (run* (q)
    (evalo
     `(letrec ((rember (lambda (x l)
                         (if (null? l)
                             '()
                             (if (equal? (car l) x)
                                 (cdr l)
                                 (cons (car l) (rember x (cdr l))))))))
        (rember 'love '(I love you love)))
     '(I you love)))
  '(_.0))

(test "rember-2"
  (run* (q)
    (evalo
     `(letrec ((rember (lambda (x l)
                         (if (null? l)
                             '()
                             (if (equal? (car l) x)
                                 (cdr l)
                                 (cons (car l) (rember x (cdr l))))))))
        (rember 'love '(I love you love)))
     q))
  '((I you love)))

;; The Relational Little Schemer!!!!
(test "rember-3"
  (run* (q)
    (evalo
     `(letrec ((rember (lambda (x l)
                         (if (null? l)
                             '()
                             (if (equal? (car l) x)
                                 (cdr l)
                                 (cons (car l) (rember x (cdr l))))))))
        (rember (quote ,q) '(I love you love)))
     '(I you love)))
  '(love))

(test "rember-4"
  (run* (q)
    (evalo
     `(letrec ((rember (lambda (x l)
                         (if (null? l)
                             '()
                             (if (equal? (car l) x)
                                 (cdr l)
                                 (cons (car l) (rember x (cdr l))))))))
        (rember (quote love) (quote ,q)))
     '(I you love)))
  '((love I you love)
    (I love you love)
    (I you love love)))

(test "rember-5"
  (run* (x y)
    (evalo
     `(letrec ((rember (lambda (x l)
                         (if (null? l)
                             '()
                             (if (equal? (car l) x)
                                 (cdr l)
                                 (cons (car l) (rember x (cdr l))))))))
        (rember (quote ,x) (quote ,y)))
     '(I you love)))
  '(((_.0 (_.0 I you love))
     (absento (closure _.0)))
    ((_.0 (I _.0 you love)) (=/= ((_.0 I)))
     (absento (closure _.0)))
    ((_.0 (I you _.0 love)) (=/= ((_.0 I)) ((_.0 you)))
     (absento (closure _.0)))
    ((_.0 (I you love))
     (=/= ((_.0 I)) ((_.0 love)) ((_.0 you)))
     (absento (closure _.0)))
    ((_.0 (I you love _.0))
     (=/= ((_.0 I)) ((_.0 love)) ((_.0 you)))
     (absento (closure _.0)))))



#|
cool answers from run 1000:

(apply rember '(_.0 (_.0 I you love)))

(list 'I 'you (apply rember (list '_.0 '(_.0 . love))))

(apply rember (list '_.0 '(I you _.0 love)))

(rember '_.0 '(I _.0 you love))

|#
(test "rember-6"
  (run 100 (q)
    (evalo
     `(letrec ((rember (lambda (x l)
                         (if (null? l)
                             '()
                             (if (equal? (car l) x)
                                 (cdr l)
                                 (cons (car l) (rember x (cdr l))))))))
        ,q)
     '(I you love)))
  '('(I you love) (list 'I 'you 'love)
    ((apply (lambda _.0 '(I you love)) '())
     (=/= ((_.0 quote))) (sym _.0))
    ((apply (lambda _.0 '(I you love)) '(_.1))
     (=/= ((_.0 quote))) (sym _.0) (absento (closure _.1)))
    ((apply (lambda _.0 '(I you love)) '(_.1 _.2))
     (=/= ((_.0 quote))) (sym _.0)
     (absento (closure _.1) (closure _.2)))
    ((apply (lambda _.0 _.0) '(I you love)) (sym _.0))
    ((list (apply (lambda _.0 'I) '()) 'you 'love)
     (=/= ((_.0 quote))) (sym _.0))
    (((lambda _.0 '(I you love))) (=/= ((_.0 quote)))
     (sym _.0))
    ((apply (lambda _.0 '(I you love)) '(_.1 _.2 _.3))
     (=/= ((_.0 quote))) (sym _.0)
     (absento (closure _.1) (closure _.2) (closure _.3)))
    ((letrec ((_.0 (lambda _.1 _.2))) '(I you love))
     (=/= ((_.0 quote))) (sym _.1))
    ((list 'I 'you (apply (lambda _.0 'love) '()))
     (=/= ((_.0 quote))) (sym _.0))
    ((list 'I (apply (lambda _.0 'you) '()) 'love)
     (=/= ((_.0 quote))) (sym _.0))
    (((lambda _.0 '(I you love)) '_.1) (=/= ((_.0 quote)))
     (sym _.0) (absento (closure _.1)))
    ((list (apply (lambda _.0 'I) '(_.1)) 'you 'love)
     (=/= ((_.0 quote))) (sym _.0) (absento (closure _.1)))
    ((list 'I 'you (apply (lambda _.0 'love) '(_.1)))
     (=/= ((_.0 quote))) (sym _.0) (absento (closure _.1)))
    ((apply (lambda _.0 (list 'I 'you 'love)) '())
     (=/= ((_.0 list)) ((_.0 quote))) (sym _.0))
    ((apply (lambda (_.0) _.0) '((I you love))) (sym _.0))
    (((lambda _.0 '(I you love)) rember) (=/= ((_.0 quote)))
     (sym _.0))
    ((apply (lambda _.0 '(I you love)) '(_.1 _.2 _.3 _.4))
     (=/= ((_.0 quote))) (sym _.0)
     (absento (closure _.1) (closure _.2) (closure _.3)
              (closure _.4)))
    ((apply (lambda (_.0) '(I you love)) '(_.1))
     (=/= ((_.0 quote))) (sym _.0) (absento (closure _.1)))
    ((list 'I (apply (lambda _.0 'you) '(_.1)) 'love)
     (=/= ((_.0 quote))) (sym _.0) (absento (closure _.1)))
    ((list 'I 'you (apply (lambda _.0 'love) '(_.1 _.2)))
     (=/= ((_.0 quote))) (sym _.0)
     (absento (closure _.1) (closure _.2)))
    (((lambda _.0 '(I you love)) '_.1 '_.2)
     (=/= ((_.0 quote))) (sym _.0)
     (absento (closure _.1) (closure _.2)))
    (((lambda _.0 _.0) 'I 'you 'love) (sym _.0))
    ((apply
      (lambda _.0 (apply (lambda _.1 '(I you love)) _.0)) '())
     (=/= ((_.0 apply)) ((_.0 lambda)) ((_.0 quote))
          ((_.1 quote)))
     (sym _.0 _.1))
    ((list (apply (lambda _.0 'I) '()) 'you
           (apply (lambda _.1 'love) '()))
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1))
    ((list 'I 'you ((lambda _.0 'love))) (=/= ((_.0 quote)))
     (sym _.0))
    ((list (apply (lambda _.0 'I) '())
           (apply (lambda _.1 'you) '()) 'love)
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1))
    ((apply (lambda _.0 (list 'I 'you 'love)) '(_.1))
     (=/= ((_.0 list)) ((_.0 quote))) (sym _.0)
     (absento (closure _.1)))
    (((lambda _.0 '(I you love)) '_.1 rember)
     (=/= ((_.0 quote))) (sym _.0) (absento (closure _.1)))
    ((apply (lambda _.0 '(I you love)) '(_.1 _.2 _.3 _.4 _.5))
     (=/= ((_.0 quote))) (sym _.0)
     (absento (closure _.1) (closure _.2) (closure _.3)
              (closure _.4) (closure _.5)))
    ((list 'I (apply (lambda _.0 'you) '())
           (apply (lambda _.1 'love) '()))
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1))
    ((list (apply (lambda _.0 'I) '(_.1 _.2)) 'you 'love)
     (=/= ((_.0 quote))) (sym _.0)
     (absento (closure _.1) (closure _.2)))
    ((car '((I you love) . _.0)) (absento (closure _.0)))
    ((list (apply (lambda _.0 'I) '()) 'you
           (apply (lambda _.1 'love) '(_.2)))
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1)
     (absento (closure _.2)))
    ((apply
      (lambda _.0 (apply (lambda _.1 '(I you love)) '())) '())
     (=/= ((_.0 apply)) ((_.0 lambda)) ((_.0 quote))
          ((_.1 quote)))
     (sym _.0 _.1))
    ((list 'I 'you (apply (lambda _.0 'love) '(_.1 _.2 _.3)))
     (=/= ((_.0 quote))) (sym _.0)
     (absento (closure _.1) (closure _.2) (closure _.3)))
    (((lambda _.0 '(I you love)) (list)) (=/= ((_.0 quote)))
     (sym _.0))
    (((lambda _.0 '(I you love)) rember '_.1)
     (=/= ((_.0 quote))) (sym _.0) (absento (closure _.1)))
    ((list 'I 'you ((lambda _.0 'love) '_.1))
     (=/= ((_.0 quote))) (sym _.0) (absento (closure _.1)))
    ((list 'I (apply (lambda _.0 'you) '())
           (apply (lambda _.1 'love) '(_.2)))
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1)
     (absento (closure _.2)))
    ((letrec ((_.0 (lambda (_.1) _.2))) '(I you love))
     (=/= ((_.0 quote))) (sym _.1))
    ((list 'I 'you ((lambda _.0 'love) rember))
     (=/= ((_.0 quote))) (sym _.0))
    ((list 'I 'you (letrec ((_.0 (lambda _.1 _.2))) 'love))
     (=/= ((_.0 quote))) (sym _.1))
    ((list (apply (lambda _.0 'I) '())
           (apply (lambda _.1 'you) '(_.2)) 'love)
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1)
     (absento (closure _.2)))
    (((lambda _.0 '(I you love)) '_.1 '_.2 '_.3)
     (=/= ((_.0 quote))) (sym _.0)
     (absento (closure _.1) (closure _.2) (closure _.3)))
    ((apply
      (lambda _.0 (apply (lambda _.1 '(I you love)) _.0))
      '(_.2))
     (=/= ((_.0 apply)) ((_.0 lambda)) ((_.0 quote))
          ((_.1 quote)))
     (sym _.0 _.1) (absento (closure _.2)))
    ((list (apply (lambda _.0 'I) '()) 'you
           (apply (lambda _.1 'love) '(_.2 _.3)))
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1)
     (absento (closure _.2) (closure _.3)))
    (((lambda _.0 '(I you love)) rember rember)
     (=/= ((_.0 quote))) (sym _.0))
    ((list (apply (lambda _.0 'I) '(_.1)) 'you
           (apply (lambda _.2 'love) '()))
     (=/= ((_.0 quote)) ((_.2 quote))) (sym _.0 _.2)
     (absento (closure _.1)))
    ((list 'I (apply (lambda _.0 'you) '(_.1 _.2)) 'love)
     (=/= ((_.0 quote))) (sym _.0)
     (absento (closure _.1) (closure _.2)))
    ((apply
      (lambda _.0 (apply (lambda _.1 '(I you love)) '(_.2)))
      '())
     (=/= ((_.0 apply)) ((_.0 lambda)) ((_.0 quote))
          ((_.1 quote)))
     (sym _.0 _.1) (absento (closure _.2)))
    ((list 'I (apply (lambda _.0 'you) '())
           (apply (lambda _.1 'love) '(_.2 _.3)))
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1)
     (absento (closure _.2) (closure _.3)))
    ((list (apply (lambda _.0 'I) '(_.1))
           (apply (lambda _.2 'you) '()) 'love)
     (=/= ((_.0 quote)) ((_.2 quote))) (sym _.0 _.2)
     (absento (closure _.1)))
    ((apply (lambda _.0 (list 'I 'you 'love)) '(_.1 _.2))
     (=/= ((_.0 list)) ((_.0 quote))) (sym _.0)
     (absento (closure _.1) (closure _.2)))
    ((list 'I 'you
           (apply (lambda _.0 (apply (lambda _.1 'love) _.0)) '()))
     (=/= ((_.0 apply)) ((_.0 lambda)) ((_.0 quote))
          ((_.1 quote)))
     (sym _.0 _.1))
    (((lambda _.0 '(I you love)) '_.1 '_.2 rember)
     (=/= ((_.0 quote))) (sym _.0)
     (absento (closure _.1) (closure _.2)))
    ((list (apply (lambda _.0 'I) '()) 'you
           ((lambda _.1 'love)))
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1))
    ((list 'I (apply (lambda _.0 'you) '(_.1))
           (apply (lambda _.2 'love) '()))
     (=/= ((_.0 quote)) ((_.2 quote))) (sym _.0 _.2)
     (absento (closure _.1)))
    ((list ((lambda _.0 'I)) 'you 'love) (=/= ((_.0 quote)))
     (sym _.0))
    ((apply (lambda _.0 '(I you love))
            '(_.1 _.2 _.3 _.4 _.5 _.6))
     (=/= ((_.0 quote))) (sym _.0)
     (absento (closure _.1) (closure _.2) (closure _.3)
              (closure _.4) (closure _.5) (closure _.6)))
    ((list 'I 'you ((lambda _.0 'love) '_.1 '_.2))
     (=/= ((_.0 quote))) (sym _.0)
     (absento (closure _.1) (closure _.2)))
    ((cdr '(_.0 I you love)) (absento (closure _.0)))
    ((list 'I (apply (lambda _.0 'you) '())
           ((lambda _.1 'love)))
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1))
    ((list 'I 'you (apply (lambda (_.0) _.0) '(love)))
     (sym _.0))
    ((apply (lambda (_.0) (list _.0 'you 'love)) '(I))
     (=/= ((_.0 list)) ((_.0 quote))) (sym _.0))
    ((list (apply (lambda _.0 'I) '(_.1)) 'you
           (apply (lambda _.2 'love) '(_.3)))
     (=/= ((_.0 quote)) ((_.2 quote))) (sym _.0 _.2)
     (absento (closure _.1) (closure _.3)))
    ((list 'I 'you
           (apply (lambda _.0 'love) '(_.1 _.2 _.3 _.4)))
     (=/= ((_.0 quote))) (sym _.0)
     (absento (closure _.1) (closure _.2) (closure _.3)
              (closure _.4)))
    ((list (apply (lambda _.0 'I) '())
           (apply (lambda _.1 'you) '())
           (apply (lambda _.2 'love) '()))
     (=/= ((_.0 quote)) ((_.1 quote)) ((_.2 quote)))
     (sym _.0 _.1 _.2))
    ((list 'I 'you (apply (lambda (_.0) 'love) '(_.1)))
     (=/= ((_.0 quote))) (sym _.0) (absento (closure _.1)))
    (((lambda _.0 '(I you love)) (lambda _.1 _.2))
     (=/= ((_.0 quote))) (sym _.0 _.1))
    ((apply
      (lambda _.0 (apply (lambda _.1 '(I you love)) '()))
      '(_.2))
     (=/= ((_.0 apply)) ((_.0 lambda)) ((_.0 quote))
          ((_.1 quote)))
     (sym _.0 _.1) (absento (closure _.2)))
    ((apply
      (lambda _.0
        (list (apply (lambda _.1 'I) _.0) 'you 'love))
      '())
     (=/= ((_.0 apply)) ((_.0 lambda)) ((_.0 list))
          ((_.0 quote)) ((_.1 quote)))
     (sym _.0 _.1))
    (((lambda _.0 '(I you love)) '_.1 (list))
     (=/= ((_.0 quote))) (sym _.0) (absento (closure _.1)))
    ((apply rember '(_.0 (_.0 I you love)))
     (absento (closure _.0)))
    ((list 'I 'you ((lambda _.0 'love) '_.1 rember))
     (=/= ((_.0 quote))) (sym _.0) (absento (closure _.1)))
    ((list 'I (apply (lambda _.0 'you) '(_.1))
           (apply (lambda _.2 'love) '(_.3)))
     (=/= ((_.0 quote)) ((_.2 quote))) (sym _.0 _.2)
     (absento (closure _.1) (closure _.3)))
    (((lambda _.0 '(I you love)) '_.1 rember '_.2)
     (=/= ((_.0 quote))) (sym _.0)
     (absento (closure _.1) (closure _.2)))
    (((lambda (_.0) _.0) '(I you love)) (sym _.0))
    ((list (apply (lambda _.0 'I) '()) 'you
           (apply (lambda _.1 'love) '(_.2 _.3 _.4)))
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1)
     (absento (closure _.2) (closure _.3) (closure _.4)))
    ((apply (lambda _.0 ((lambda _.1 '(I you love)))) '())
     (=/= ((_.0 lambda)) ((_.0 quote)) ((_.1 quote)))
     (sym _.0 _.1))
    ((list 'I 'you
           (apply (lambda _.0 (apply (lambda _.1 'love) '())) '()))
     (=/= ((_.0 apply)) ((_.0 lambda)) ((_.0 quote))
          ((_.1 quote)))
     (sym _.0 _.1))
    ((list (apply (lambda _.0 'I) '()) 'you
           ((lambda _.1 'love) '_.2))
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1)
     (absento (closure _.2)))
    ((list (apply (lambda _.0 'I) '())
           (apply (lambda _.1 'you) '())
           (apply (lambda _.2 'love) '(_.3)))
     (=/= ((_.0 quote)) ((_.1 quote)) ((_.2 quote)))
     (sym _.0 _.1 _.2) (absento (closure _.3)))
    (((lambda _.0 '(I you love)) (list) '_.1)
     (=/= ((_.0 quote))) (sym _.0) (absento (closure _.1)))
    (((lambda _.0 '(I you love)) rember '_.1 '_.2)
     (=/= ((_.0 quote))) (sym _.0)
     (absento (closure _.1) (closure _.2)))
    ((list 'I (apply (lambda _.0 'you) '())
           (apply (lambda _.1 'love) '(_.2 _.3 _.4)))
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1)
     (absento (closure _.2) (closure _.3) (closure _.4)))
    ((letrec ((_.0 (lambda (_.1 _.2) _.3))) '(I you love))
     (=/= ((_.0 quote))) (sym _.1 _.2))
    (((lambda (_.0) '(I you love)) '_.1) (=/= ((_.0 quote)))
     (sym _.0) (absento (closure _.1)))
    ((list 'I (apply (lambda _.0 'you) '())
           ((lambda _.1 'love) '_.2))
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1)
     (absento (closure _.2)))
    ((apply
      (lambda _.0
        (apply (lambda _.1 '(I you love)) '(_.2 _.3)))
      '())
     (=/= ((_.0 apply)) ((_.0 lambda)) ((_.0 quote))
          ((_.1 quote)))
     (sym _.0 _.1) (absento (closure _.2) (closure _.3)))
    ((list 'I 'you ((lambda _.0 'love) (list)))
     (=/= ((_.0 quote))) (sym _.0))
    ((list (apply (lambda _.0 'I) '(_.1))
           (apply (lambda _.2 'you) '(_.3)) 'love)
     (=/= ((_.0 quote)) ((_.2 quote))) (sym _.0 _.2)
     (absento (closure _.1) (closure _.3)))
    ((list 'I 'you ((lambda _.0 'love) rember '_.1))
     (=/= ((_.0 quote))) (sym _.0) (absento (closure _.1)))
    (((lambda _.0 '(I you love)) '_.1 '_.2 '_.3 '_.4)
     (=/= ((_.0 quote))) (sym _.0)
     (absento (closure _.1) (closure _.2) (closure _.3)
              (closure _.4)))
    ((list (apply (lambda _.0 'I) '()) 'you
           ((lambda _.1 'love) rember))
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1))
    ((list (apply (lambda _.0 'I) '()) 'you
           (letrec ((_.1 (lambda _.2 _.3))) 'love))
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.2))
    ((list (apply (lambda _.0 'I) '(_.1)) 'you
           (apply (lambda _.2 'love) '(_.3 _.4)))
     (=/= ((_.0 quote)) ((_.2 quote))) (sym _.0 _.2)
     (absento (closure _.1) (closure _.3) (closure _.4)))
    ((apply
      (lambda _.0 (apply (lambda _.1 '(I you love)) _.0))
      '(_.2 _.3))
     (=/= ((_.0 apply)) ((_.0 lambda)) ((_.0 quote))
          ((_.1 quote)))
     (sym _.0 _.1) (absento (closure _.2) (closure _.3)))
    ((list 'I ((lambda _.0 'you)) 'love) (=/= ((_.0 quote)))
     (sym _.0))))


(test "infer list 1"
  (run 10 (q)
    (absento 'I q)
    (absento 'love q)
    (absento 'you q)
    (evalo `(apply ,q '(I love you)) '(I love you)))
  '(((lambda _.0 _.0) (=/= ((_.0 I)) ((_.0 love)) ((_.0 you)))
     (sym _.0))
    ((lambda _.0 (apply (lambda _.1 _.0) '()))
     (=/= ((_.0 I)) ((_.0 _.1)) ((_.0 apply)) ((_.0 lambda))
          ((_.0 love)) ((_.0 quote)) ((_.0 you)) ((_.1 I))
          ((_.1 love)) ((_.1 you)))
     (sym _.0 _.1))
    ((apply (lambda _.0 (lambda _.1 _.1)) '())
     (=/= ((_.0 I)) ((_.0 lambda)) ((_.0 love)) ((_.0 you))
          ((_.1 I)) ((_.1 love)) ((_.1 you)))
     (sym _.0 _.1))
    ((lambda (_.0 _.1 _.2) (list _.0 _.1 _.2))
     (=/= ((_.0 I)) ((_.0 _.1)) ((_.0 _.2)) ((_.0 list))
          ((_.0 love)) ((_.0 you)) ((_.1 I)) ((_.1 _.2))
          ((_.1 list)) ((_.1 love)) ((_.1 you)) ((_.2 I))
          ((_.2 list)) ((_.2 love)) ((_.2 you)))
     (sym _.0 _.1 _.2))
    ((lambda _.0 (apply (lambda _.1 _.0) '(_.2)))
     (=/= ((_.0 I)) ((_.0 _.1)) ((_.0 apply)) ((_.0 lambda))
          ((_.0 love)) ((_.0 quote)) ((_.0 you)) ((_.1 I))
          ((_.1 love)) ((_.1 you)))
     (sym _.0 _.1)
     (absento (I _.2) (closure _.2) (love _.2) (you _.2)))
    ((lambda _.0 (apply (lambda _.1 _.0) '(_.2 _.3)))
     (=/= ((_.0 I)) ((_.0 _.1)) ((_.0 apply)) ((_.0 lambda))
          ((_.0 love)) ((_.0 quote)) ((_.0 you)) ((_.1 I))
          ((_.1 love)) ((_.1 you)))
     (sym _.0 _.1)
     (absento (I _.2) (I _.3) (closure _.2) (closure _.3)
              (love _.2) (love _.3) (you _.2) (you _.3)))
    ((apply (lambda _.0 (lambda _.1 _.1)) '(_.2))
     (=/= ((_.0 I)) ((_.0 lambda)) ((_.0 love)) ((_.0 you))
          ((_.1 I)) ((_.1 love)) ((_.1 you)))
     (sym _.0 _.1)
     (absento (I _.2) (closure _.2) (love _.2) (you _.2)))
    ((lambda _.0 (apply (lambda (_.1) _.0) '(_.2)))
     (=/= ((_.0 I)) ((_.0 _.1)) ((_.0 apply)) ((_.0 lambda))
          ((_.0 love)) ((_.0 quote)) ((_.0 you)) ((_.1 I))
          ((_.1 love)) ((_.1 you)))
     (sym _.0 _.1)
     (absento (I _.2) (closure _.2) (love _.2) (you _.2)))
    ((apply
      (lambda _.0 (lambda _.1 (apply (lambda _.2 _.1) '())))
      '())
     (=/= ((_.0 I)) ((_.0 apply)) ((_.0 lambda)) ((_.0 love))
          ((_.0 quote)) ((_.0 you)) ((_.1 I)) ((_.1 _.2))
          ((_.1 apply)) ((_.1 lambda)) ((_.1 love))
          ((_.1 quote)) ((_.1 you)) ((_.2 I)) ((_.2 love))
          ((_.2 you)))
     (sym _.0 _.1 _.2))
    ((lambda _.0 ((lambda _.1 _.0)))
     (=/= ((_.0 I)) ((_.0 _.1)) ((_.0 lambda)) ((_.0 love))
          ((_.0 you)) ((_.1 I)) ((_.1 love)) ((_.1 you)))
     (sym _.0 _.1))))


(test "plus 1"
  (run* (q)
    (evalo
     `(letrec ((plus (lambda (n m)
                       (if (equal? 'z n)
                           m
                           (cons 's (plus (cdr n) m))))))
        (plus (quote ,q) '(s . (s . z))))
     '(s s s s . z)))
  '(((_.0 _.1 . z) (absento (closure _.0) (closure _.1)))))

(test "plus 2"
  (run* (q)
    (evalo
     `(letrec ((plus (lambda (n m)
                       (if (equal? 'z n)
                           m
                           (cons 's (plus (cdr n) m))))))
        (plus '(s . (s . z)) (quote ,q)))
     '(s s s s . z)))
  '((s s . z)))

(test "plus 3"
  (run* (x y)
    (evalo
     `(letrec ((plus (lambda (n m)
                       (if (equal? 'z n)
                           m
                           (cons 's (plus (cdr n) m))))))
        (plus (quote ,x) (quote ,y)))
     '(s s s s . z)))
  '((z (s s s s . z))
    (((_.0 . z) (s s s . z))
     (absento (closure _.0)))
    (((_.0 _.1 . z) (s s . z))
     (absento (closure _.0) (closure _.1)))
    (((_.0 _.1 _.2 . z) (s . z))
     (absento (closure _.0) (closure _.1) (closure _.2)))
    (((_.0 _.1 _.2 _.3 . z) z)
     (absento (closure _.0) (closure _.1) (closure _.2) (closure _.3)))))

(test "plus 4"
  (run* (x y)
    (evalo
     `(letrec ((plus (lambda (n m)
                       (if (equal? 'z n)
                           m
                           (cons 's (plus (cdr n) m))))))
        (plus (quote ,x) (quote ,y)))
     'z))
  '((z z)))

(test "plus 5"
  (run* (x y)
    (evalo
     `(letrec ((plus (lambda (n m)
                       (if (equal? 'z n)
                           m
                           (cons 's (plus (cdr n) m))))))
        (plus (quote (s . ,x)) (quote ,y)))
     'z))
  '())

(test "plus 6"
  (run 1 (x y z)
    (evalo
     `(letrec ((plus (lambda (n m)
                       (if (equal? 'z n)
                           m
                           (cons 's (plus (cdr n) m))))))
        (plus (quote (s . ,x)) (quote ,y)))
     `(s . ,z)))
  '())

(test "plus 7"
  (run 2 (x y z)
    (evalo
     `(letrec ((plus (lambda (n m)
                       (if (equal? 'z n)
                           m
                           (cons 's (plus (cdr n) m))))))
        (plus (quote (s . ,x)) (quote ,y)))
     `(s . ,z)))
  '(((z _.0 _.0) (absento (closure _.0)))
   (((_.0 . z) _.1 (s . _.1))
    (absento (closure _.0) (closure _.1)))))

(test "plus 8"
  (run 1 (x y)
    (evalo
     `(letrec ((plus (lambda (n m)
                       (if (equal? 'z n)
                           m
                           (cons 's (plus (cdr n) m))))))
        (plus (quote (s . ,x)) (quote (s . ,y))))
     '(s . z)))
  '())


(letrec ((lookup (lambda (x env)
                   (if (null? env)
                       'unbound-variable-error
                       (if (equal? (car (car env)) x)
                           (cdr (car env))
                           (lookup x (cdr env)))))))
  (lookup 'y '((z . 5) (y . 6) (w . 7) (y . 2))))

(test "lookup 1"
  (run* (q)
    (evalo
     `(letrec ((lookup (lambda (x env)
                         (if (null? env)
                             'unbound-variable-error
                             (if (equal? (car (car env)) x)
                                 (cdr (car env))
                                 (lookup x (cdr env)))))))
        (lookup 'y '((z . 5) (y . 6) (w . 7) (y . 2))))
     q))
  '(6))

(test "lookup 2"
  (run* (q)
    (evalo
     `(letrec ((lookup (lambda (x env)
                         (if (null? env)
                             'unbound-variable-error
                             (if (equal? (car (car env)) x)
                                 (cdr (car env))
                                 (lookup x (cdr env)))))))
        (lookup 'v '((z . 5) (y . 6) (w . 7) (y . 2))))
     q))
  '(unbound-variable-error))

(test "lookup 3"
  (run* (q)
    (evalo
     `(letrec ((lookup (lambda (x env)
                         (if (null? env)
                             'unbound-variable-error
                             (if (equal? (car (car env)) x)
                                 (cdr (car env))
                                 (lookup x (cdr env)))))))
        (lookup (quote ,q) '((z . 5) (y . 6) (w . 7) (y . 2))))
     '6))
  '(y))

(test "lookup 4"
  (run* (q)
    (evalo
     `(letrec ((lookup (lambda (x env)
                         (if (null? env)
                             'unbound-variable-error
                             (if (equal? (car (car env)) x)
                                 (cdr (car env))
                                 (lookup x (cdr env)))))))
        (lookup (quote ,q) '((z . 5) (y . 6) (w . 7) (y . 2))))
     'unbound-variable-error))
  '((_.0 (=/= ((_.0 w)) ((_.0 y)) ((_.0 z)))
         (absento (closure _.0)))))








(letrec ((eval-expr
          (lambda (expr env)
            (if (symbol? expr)
                (env expr)
                (if (equal? (car expr) 'lambda)
                    (lambda (a)
                      (eval-expr (car (cdr (cdr expr)))
                                 (lambda (y)
                                   (if (equal? (car (car (cdr expr))) y)
                                       a
                                       (env (car (car (cdr expr))))))))
                    ((eval-expr (car expr) env) (eval-expr (car (cdr expr)) env)))))))
  (eval-expr '((lambda (z) z) (lambda (z) z)) (lambda (a) 'error-unbound-variable)))

(run 1 (q)
  (evalo
   `(letrec ((eval-expr
              (lambda (expr env)
                (if (symbol? expr)
                    (env expr)
                    (if (equal? (car expr) 'lambda)
                        (lambda (a)
                          (eval-expr (car (cdr (cdr expr)))
                                     (lambda (y)
                                       (if (equal? (car (car (cdr expr))) y)
                                           a
                                           (env (car (car (cdr expr))))))))
                        ((eval-expr (car expr) env) (eval-expr (car (cdr expr)) env)))))))
      (eval-expr '(lambda (z) w) (lambda (a) 'error-unbound-variable)))
   q))


(run 1 (q)
  (evalo
   `(letrec ((eval-expr
              (lambda (expr env)
                (if (symbol? expr)
                    (env expr)
                    (if (equal? (car expr) 'lambda)
                        (lambda (a)
                          (eval-expr (car (cdr (cdr expr)))
                                     (lambda (y)
                                       (if (equal? (car (car (cdr expr))) y)
                                           a
                                           (env (car (car (cdr expr))))))))
                        ((eval-expr (car expr) env) (eval-expr (car (cdr expr)) env)))))))
      (eval-expr '((lambda (z) z) (lambda (z) z)) (lambda (a) 'error-unbound-variable)))
   q))



(run 1 (q)
  (evalo
   `(letrec ((eval-expr
              (lambda (expr env)
                (if (symbol? expr)
                    (env expr)
                    (if (equal? (car expr) 'lambda)
                        (lambda (a)
                          (eval-expr (car (cdr (cdr expr)))
                                     (lambda (y)
                                       (if (equal? (car (car (cdr expr))) y)
                                           a
                                           (env (car (car (cdr expr))))))))
                        ((eval-expr (car expr) env) (eval-expr (car (cdr expr)) env)))))))
      (eval-expr '((lambda (z) w) (lambda (z) z)) (lambda (a) 'error-unbound-variable)))
   q))
(error-unbound-variable)


(run 1 (q)
  (evalo
   `(letrec ((eval-expr
              (lambda (expr env)
                (if (symbol? expr)
                    (env expr)
                    (if (equal? (car expr) 'lambda)
                        (lambda (a)
                          (eval-expr (car (cdr (cdr expr)))
                                     (lambda (y)
                                       (if (equal? (car (car (cdr expr))) y)
                                           a
                                           (env (car (car (cdr expr))))))))
                        ((eval-expr (car expr) env) (eval-expr (car (cdr expr)) env)))))))
      (eval-expr (quote ,q) (lambda (a) 'error-unbound-variable)))
   'error-unbound-variable))


(letrec ((eval-expr
          (lambda (expr env)
            (if (symbol? expr)
                (env expr)
                (if (equal? (car expr) 'quote)
                    (car (cdr expr))
                    (if (equal? (car expr) 'lambda)
                        (lambda (a)
                          (eval-expr (car (cdr (cdr expr)))
                                     (lambda (y)
                                       (if (equal? (car (car (cdr expr))) y)
                                           a
                                           (env (car (car (cdr expr))))))))
                        ((eval-expr (car expr) env) (eval-expr (car (cdr expr)) env))))))))
  (eval-expr '((lambda (x) (quote x)) (quote 5)) (lambda (a) 'error-unbound-variable)))

(letrec ((eval-expr
          (lambda (expr env)
            (if (symbol? expr)
                (env expr)
                (if (equal? (car expr) 'quote)
                    (car (cdr expr))
                    (if (equal? (car expr) 'cons)
                        (cons (eval-expr (car (cdr expr)) env)
                              (eval-expr (car (cdr (cdr expr))) env))
                        (if (equal? (car expr) 'lambda)
                            (lambda (a)
                              (eval-expr (car (cdr (cdr expr)))
                                         (lambda (y)
                                           (if (equal? (car (car (cdr expr))) y)
                                               a
                                               (env (car (car (cdr expr))))))))
                            ((eval-expr (car expr) env) (eval-expr (car (cdr expr)) env)))))))))
  (eval-expr '(cons (quote 3) (quote 4)) (lambda (a) 'error-unbound-variable)))


(run 1 (q)
  (evalo
   `(letrec ((eval-expr
              (lambda (expr env)
                (if (symbol? expr)
                    (env expr)
                    (if (equal? (car expr) 'quote)
                        (car (cdr expr))
                        (if (equal? (car expr) 'cons)
                            (cons (eval-expr (car (cdr expr)) env)
                                  (eval-expr (car (cdr (cdr expr))) env))
                            (if (equal? (car expr) 'lambda)
                                (lambda (a)
                                  (eval-expr (car (cdr (cdr expr)))
                                             (lambda (y)
                                               (if (equal? (car (car (cdr expr))) y)
                                                   a
                                                   (env (car (car (cdr expr))))))))
                                ((eval-expr (car expr) env) (eval-expr (car (cdr expr)) env)))))))))
      (eval-expr '(cons (quote 3) (quote 4)) (lambda (a) 'error-unbound-variable)))
   q))

(run 1 (q)
  (evalo
   `(letrec ((eval-expr
              (lambda (expr env)
                (if (symbol? expr)
                    (env expr)
                    (if (equal? (car expr) 'quote)
                        (car (cdr expr))
                        (if (equal? (car expr) 'cons)
                            (cons (eval-expr (car (cdr expr)) env)
                                  (eval-expr (car (cdr (cdr expr))) env))
                            (if (equal? (car expr) 'lambda)
                                (lambda (a)
                                  (eval-expr (car (cdr (cdr expr)))
                                             (lambda (y)
                                               (if (equal? (car (car (cdr expr))) y)
                                                   a
                                                   (env (car (car (cdr expr))))))))
                                ((eval-expr (car expr) env) (eval-expr (car (cdr expr)) env)))))))))
      (eval-expr (quote ,q) (lambda (a) 'error-unbound-variable)))
   q))


;; Hmm.  Trying to generate quines.  First answer is
;; 'error-unbound-variable, which seems bogus.  Maybe the relational
;; Scheme interpreter needs to have an explicit 'error' primitive,
;; with irritants and a message.
;;
;; Let 'run 2' run for a few minutes, with no answer.  Should verify
;; that a cons-based quine actually works here.
(run 1 (q)
  (evalo
   `(letrec ((eval-expr
              (lambda (expr env)
                (if (symbol? expr)
                    (env expr)
                    (if (equal? (car expr) 'quote)
                        (car (cdr expr))
                        (if (equal? (car expr) 'cons)
                            (cons (eval-expr (car (cdr expr)) env)
                                  (eval-expr (car (cdr (cdr expr))) env))
                            (if (equal? (car expr) 'lambda)
                                (lambda (a)
                                  (eval-expr (car (cdr (cdr expr)))
                                             (lambda (y)
                                               (if (equal? (car (car (cdr expr))) y)
                                                   a
                                                   (env (car (car (cdr expr))))))))
                                ((eval-expr (car expr) env) (eval-expr (car (cdr expr)) env)))))))))
      (eval-expr (quote ,q) (lambda (a) 'error-unbound-variable)))
   q))


(run 1 (q)
  (evalo
   `(letrec ((eval-expr
              (lambda (expr env)
                (if (symbol? expr)
                    (env expr)
                    (if (equal? (car expr) 'quote)
                        (car (cdr expr))
                        (if (equal? (car expr) 'cons)
                            (cons (eval-expr (car (cdr expr)) env)
                                  (eval-expr (car (cdr (cdr expr))) env))
                            (if (equal? (car expr) 'lambda)
                                (lambda (a)
                                  (eval-expr (car (cdr (cdr expr)))
                                             (lambda (y)
                                               (if (equal? (car (car (cdr expr))) y)
                                                   a
                                                   (env (car (car (cdr expr))))))))
                                ((eval-expr (car expr) env) (eval-expr (car (cdr expr)) env)))))))))
      (eval-expr (quote ,q) (lambda (a) 'error-unbound-variable)))
   '(I love you)))

;; A little sloppy joe: Lack of enforcing the syntax of LC is problematic
;;
;; probably need 'and' and 'pair?'
;;
;; or add 'match' to the relational interpreter
;;
;; or, maybe grammar constraints would help?
(run 1 (q)
  (evalo
   `(letrec ((eval-expr
              (lambda (expr env)
                (if (symbol? expr)
                    (env expr)
                    (if (equal? (car expr) 'quote)
                        (car (cdr expr))
                        (if (equal? (car expr) 'cons)
                            (cons (eval-expr (car (cdr expr)) env)
                                  (eval-expr (car (cdr (cdr expr))) env))
                            (if (equal? (car expr) 'lambda)
                                (lambda (a)
                                  (eval-expr (car (cdr (cdr expr)))
                                             (lambda (y)
                                               (if (equal? (car (car (cdr expr))) y)
                                                   a
                                                   (env (car (car (cdr expr))))))))
                                ((eval-expr (car expr) env) (eval-expr (car (cdr expr)) env)))))))))
      (eval-expr (quote ,q) (lambda (a) 'error-unbound-variable)))
   '(I love you)))

;; generate programs whose evaluation results in an unbound variable error
(run 3 (q)
  (absento 'error-unbound-variable q) ;; this part is necessary to avoid treating the error value as a variable name
  (evalo
   `(letrec ((eval-expr
              (lambda (expr env)
                (if (symbol? expr)
                    (env expr)
                    (if (equal? (car expr) 'quote)
                        (car (cdr expr))
                        (if (equal? (car expr) 'cons)
                            (cons (eval-expr (car (cdr expr)) env)
                                  (eval-expr (car (cdr (cdr expr))) env))
                            (if (equal? (car expr) 'lambda)
                                (lambda (a)
                                  (eval-expr (car (cdr (cdr expr)))
                                             (lambda (y)
                                               (if (equal? (car (car (cdr expr))) y)
                                                   a
                                                   (env (car (car (cdr expr))))))))
                                ((eval-expr (car expr) env) (eval-expr (car (cdr expr)) env)))))))))
      (eval-expr (quote ,q) (lambda (a) 'error-unbound-variable)))
   'error-unbound-variable))
=>
((_.0 (=/= ((_.0 closure)) ((_.0 error-unbound-variable))) (sym _.0))
 (((lambda (_.0 . _.1) _.0 . _.2) _.3 . _.4)
  (=/= ((_.0 closure)) ((_.0 error-unbound-variable))
       ((_.3 closure)) ((_.3 error-unbound-variable)))
  (sym _.0 _.3)
  (absento (closure _.1) (closure _.2) (closure _.4)
           (error-unbound-variable _.1)
           (error-unbound-variable _.2)
           (error-unbound-variable _.4)))
 (((lambda (_.0 . _.1) _.2 . _.3) _.4 . _.5)
  (=/= ((_.0 _.2)) ((_.2 closure))
       ((_.2 error-unbound-variable)) ((_.4 closure))
       ((_.4 error-unbound-variable)))
  (sym _.2 _.4)
  (absento (closure _.0) (closure _.1) (closure _.3)
           (closure _.5) (error-unbound-variable _.0)
           (error-unbound-variable _.1)
           (error-unbound-variable _.3)
           (error-unbound-variable _.5))))

(run 2 (q)
    (absento 'a q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (,q '(a b c) '(d e)))
     '(a b c d e)))
=>
(append
 ((apply (lambda _.0 append) '())
  (=/= ((_.0 a)) ((_.0 append))) (sym _.0)))
