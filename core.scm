(define-module (ikanren core)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (ikanren goals)
  #:use-module (ikanren state)
  #:use-module (ikanren unification)
  #:use-module (ikanren substitution)
  #:export (mature start step pretty/goal))


(define (state->stream state)
    (if state (cons state #f) #f))

(define (mature? s) (or (not s) (pair? s)))
(define (mature s)
  (if (mature? s) s (mature (step s))))


(define (pretty/goal st g)
  (define (pretty/term t) (walk* t (state-sub st)))
  (match g
    (($ <disj> g1 g2)     `(disj ,(pretty/goal st g1) ,(pretty/goal st g2)))
    (($ <conj> g1 g2)     `(conj ,(pretty/goal st g1) ,(pretty/goal st g2)))
    (($ <==> t1 t2)       `(== ,(pretty/term t1) ,(pretty/term t2)))
    (($ <=/=> t1 t2)      `(=/= ,(pretty/term t1) ,(pretty/term t2)))
    (($ <cnot> t1 t2)     `(cnot ,(pretty/term t1) ,(pretty/term t2)))
    (($ <impl> t1 t2)     `(==> ,(pretty/goal st t1) ,(pretty/goal st t2)))
    (($ <relate> thunk d) `(relate ,(pretty/term (cdr d))))
    (_ g)))

(define (start st g)
  (match g
    (($ <tautology>)
     (state->stream st))
    (($ <contradiction>)
     #f)
    (($ <disj> g1 g2)
     (step (mplus (mk-pause st g1)
                  (mk-pause st g2))))
    (($ <conj> g1 g2)
     (step (bind (mk-pause st g1) g2)))
    (($ <relate> thunk _)
     (mk-pause st (thunk)))
    (($ <==> t1 t2) (state->stream (unify t1 t2 st)))
    (($ <impl> t1 t2) (match t2
                          (($ <relate> thunk _) (mk-pause st (implies t1 (thunk) #t)))
                          (_ #f)))
    (($ <cnot> g) (match g
                          (($ <relate> thunk _) (mk-pause st (neg (thunk))))
                          (_ #f)))
    (($ <=/=> t1 t2) (state->stream (disunify t1 t2 st)))
    (($ <exists> l) (mk-pause (state-inc st) (l (state-n st))))))

(define (step s)
  (match s
    (($ <mplus> s1 s2)
     (let ((s1 (mature s1)))
       (cond ((not s1) s2)
             ((pair? s1)
              (cons (car s1)
                    (mplus s2 (cdr s1))))
             (else (mplus s2 s1)))))
    (($ <bind> s g)
     (let ((s (mature s)))
       (cond ((not s) #f)
             ((pair? s)
              (step (mplus (mk-pause (car s) g)
                           (bind (cdr s) g))))
             (else (bind s g)))))
    (($ <pause> st g) (start st g))
    (_            s)))