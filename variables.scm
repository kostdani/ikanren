
(define-module (ikanren variables)
  #:use-module (srfi srfi-9)
  #:export (initial-var <var> mk-var var? var=? <cvar> mk-cvar))

;; Logic variables
(define-record-type <var>
    (mk-var name index)
    var?
    (name var-name)
    (index var-index))

(define (var=? x1 x2)
  (and (var? x1) (var? x2) (= (var-index x1) (var-index x2))))

(define initial-var (mk-var #f 0))


;; Constrained variables


(define-record-type <cvar>
    (mk-cvar var constrains)
    cvar?
    (var cvar-var)
    (constrains cvar-constrains))
