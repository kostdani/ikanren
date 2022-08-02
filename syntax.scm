
(define-module (ikanren syntax)
  #:use-module (ikanren variables)
  #:use-module (ikanren goals)
  #:use-module (ikanren core)
  #:use-module (ikanren state)
  #:use-module (ikanren reification)
  #:export ( exist define-relation conde query run run* stream-take conj*
               disj*))
(define-syntax define-relation
  (syntax-rules ()
    ((_ (name param ...) g ...)
     (define (name param ...)
       (relate (lambda () (fresh () g ...)) `(,name  ,param ...))))))
;; Low-level goals
(define succeed (== #t #t))
(define fail    (== #f #t))
(define-syntax conj*
  (syntax-rules ()
    ((_)                succeed)
    ((_ g)              g)
    ((_ gs ... g-final) (conj (conj* gs ...) g-final))))
(define-syntax disj*
  (syntax-rules ()
    ((_)           fail)
    ((_ g)         g)
    ((_ g0 gs ...) (disj g0 (disj* gs ...)))))
;; High level goals
(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 gs ...)
     (let ((x (var! 'x)) ...) (conj* g0 gs ...)))))
(define-syntax conde
  (syntax-rules ()
    ((_ (g gs ...) (h hs ...) ...)
     (disj* (conj* g gs ...) (conj* h hs ...) ...))))
;; Queries
(define-syntax query
  (syntax-rules ()
    ((_ (x ...) g0 gs ...)
     (let ((goal (exist (x ...) (== (list x ...) initial-var) g0 gs ...)))
       (mk-pause empty-state goal)))))
(define (stream-take n s)
  (if (eqv? 0 n) '()
    (let ((s (mature s)))
      (if (pair? s)
        (cons (car s) (stream-take (and n (- n 1)) (cdr s)))
        '()))))
(define-syntax run
  (syntax-rules ()
    ((_ n body ...) (map reify/initial-var (stream-take n (query body ...))))))
(define-syntax run*
  (syntax-rules () ((_ body ...) (run #f body ...))))

(define-syntax exists
  (syntax-rules ()
    ((_ x g)
     (ex (lambda (n) (let ((x (mk-var 'x n))) g))))))


(define-syntax exist
  (syntax-rules ()
    ((_ () g0 gs ...)
      (conj* g0 gs ...))
    ((_ (x y ...) body ...)
      (exists x (exist (y ...) body ...)))
    ))
