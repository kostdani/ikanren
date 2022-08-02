
(define-module (ikanren unification)
  #:use-module (ikanren substitution)
  #:use-module (ikanren state)
  #:export (unify disunify))

;; Unification

(define (unify u v st)
    (let* ((sub (state-sub st))
           (n (state-n st))
           (cstore (state-cstore st))
           (unifier (mgu u v sub))
           (newsub (sub-merge sub unifier))
           (newstore (update-cstore cstore newsub)))
        (and newsub newstore (mk-state n newsub newstore))))

;; Disunification

(define (disunify u v st)
    (let* ((sub (state-sub st))
           (n (state-n st))
           (cstore (state-cstore st))
           (unifier (mgu u v sub))
           (newcstore (cond
                       ((null? unifier) #f)
                       ((not unifier) cstore)
                       (else (cons unifier cstore)))))
        (and newcstore (mk-state n sub newcstore))))
