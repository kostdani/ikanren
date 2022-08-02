
(define-module (ikanren state)
  #:use-module (srfi srfi-9)
  #:use-module (ikanren substitution)
  #:export ( <state> empty-state mk-state state? state-sub state-n state-cstore state-inc
                     
            <mplus> mplus
            <bind> bind
            <pause> mk-pause))

;; States


(define-record-type <state>
    (mk-state n sub cstore)
    state?
    (n state-n)
    (sub state-sub)
    (cstore state-cstore))
(define empty-state (mk-state 1 empty-sub empty-cstore))

(define (state-inc st)
    (mk-state (+ 1 (state-n st)) (state-sub st) (state-cstore st)))




;; streams

(define-record-type <bind>
  (bind s g)
  bind?
  (s bind-s)
  (g bind-g))

(define-record-type <mplus>
  (mplus s1 s2)
  mplus?
  (s1 mplus-s1)
  (s2 mplus-s2))

(define-record-type <pause>
  (mk-pause state goal)
  pause?
  (state pause-state)
  (goal pause-goal))


