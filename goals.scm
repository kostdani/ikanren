
(define-module (ikanren goals)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (ikanren variables)
  #:export (<tautology> tautology
            <contradiction> contradiction
            <exists> ex
            <forall> forall
            <disj> disj
            <conj> conj
            <relate> relate relate-description relate?
            <==> ==
            <impl> impl implies ==>
            <=/=> =/=
            <cnot> cnot neg))

;; Goals

(define-record-type <tautology>
    (tautology)
    tautology?)


(define-record-type <contradiction>
    (contradiction)
    contradiction?)

(define-record-type <disj>
  (disj g1 g2)
  disj?
  (g1 disj-g1)
  (g2 disj-g2))

(define-record-type <conj>
  (conj g1 g2)
  conj?
  (g1 conj-g1)
  (g2 conj-g2))

(define-record-type <relate>
  (relate thunk description)
  relate?
  (thunk relate-thunk)
  (description relate-description))

(define-record-type <exists>
    (ex l);; lambda which takes number for variable and returns substituted goal
    ex?
    (l ex-l))

(define-record-type <forall>
    (forall v d t)
    forall?
    (v forall-v);;variable
    (d forall-d);;domain
    (t forall-g));;goal


(define-record-type <==>
  (eqt t1 t2)
  ==?
  (t1 ==-t1)
  (t2 ==-t2))

(define (unifiable? t)
    (or (not (record? t)) (relate? t) (var? t)))

(define (== t1 t2)
    (if (and (unifiable? t1) (unifiable? t2))
        (eqt t1 t2)
        (conj (==> t1 t2)
              (==> t2 t1))))

(define-record-type <=/=>
  (=/= t1 t2)
  =/=?
  (t1 =/=-t1)
  (t2 =/=-t2))

(define-record-type <impl>
  (impl t1 t2)
  impl?
  (t1 impl-t1)
  (t2 impl-t2))

(define-record-type <cnot>
  (cnot g)
  cnot?
  (g cnot-g))

(define (neg g)
    (match g
        (($ <conj> g1 g2) (disj (neg g1) (neg g2)))
        (($ <disj> g1 g2) (conj (neg g1) (neg g2)))
        (($ <==> t1 t2)                (=/= t1 t2))
        (($ <=/=> t1 t2)                (== t1 t2))
        (($ <tautology> t1 t2)     (contradiction))
        (($ <contradiction> t1 t2)     (tautology))
        (_                              (cnot g))))

(define (implies t1 t2 s);;s is bool inforrming if implicaiton is syntactical i.e no need to falsify t1 or prove t2
    (match t1
    (($ <conj> g1 g2)
     (disj (implies g1 t2 s) (implies g2 t2 s)))
    (($ <disj> g1 g2)
     (conj (implies g1 t2 s) (implies g2 t2 s)))
    (($ <impl> g1 g2)
     (conj (disj g1 t2) (implies g2 t2 s)))
    (_     (match t2
           (($ <conj> g1 g2)
            (conj (implies t1 g1 s) (implies t1 g2 s)))
           (($ <disj> g1 g2)
            (disj (implies t1 g1 s) (implies t1 g2 s)))
           (($ <impl> g1 g2)
            (implies (conj t1 g1) g2 s))
           (_ (disj
               (disj
                (if (relate? t2) (impl t1 t2) (contradiction))
                (if (and (unifiable? t1) (unifiable? t2)) (eqt t1 t2) (contradiction)))
               (if (not s)
                   (disj
                    (if (record? t1) (neg t1) (contradiction))
                    (if (record? t2) t2 (contradiction)))
                   (contradiction))
               ))))))

(define (==> t1 t2)
    (implies t1 t2 #f))


