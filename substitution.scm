
(define-module (ikanren substitution)
  #:use-module (rnrs lists);;for assp
  #:use-module (ikanren variables)
  #:use-module (ikanren goals)
  #:export (empty-sub mk-sub sub-merge walk walk* occurs? extend-sub mgu update-cstore empty-cstore ))


(define empty-sub '())
(define empty-cstore '())

(define (mk-sub a b) (list (cons a b)))

(define (sub-merge . subs)
    (if (member #f subs)
        #f
        (apply append subs)))


(define (walk t sub)
  (let ((xt (and (var? t) (assp (lambda (x) (var=? t x)) sub))))
    (if xt (walk (cdr xt) sub) t)))

(define (mgu u v sub)
  (let* ((u (walk u sub)) (v (walk v sub))
         (u (if (relate? u) (relate-description u) u))
         (v (if (relate? v) (relate-description v) v)))
    (cond
      ((var=? u v)                         empty-sub)
      ((var? u)                            (mk-sub u v))
      ((var? v)                            (mk-sub v u))
      ((and (pair? u) (pair? v))           (sub-merge (mgu (car u) (car v) sub)
                                                      (mgu (cdr u) (cdr v) sub)))
      (else                                (and (equal? u v) empty-sub)))))


(define (occurs? x t sub)
  (cond ((pair? t) (or (occurs? x (walk (car t) sub) sub)
                       (occurs? x (walk (cdr t) sub) sub)))
        ((var? t)  (var=? x t))
        (else      #f)))

(define (extend-sub x t sub)
  (and (not (occurs? x t sub)) `((,x . ,t) . ,sub)))

(define (walk* tm sub)
  (let ((tm (walk tm sub)))
    (if (pair? tm)
        `(,(walk* (car tm) sub) .  ,(walk* (cdr tm) sub))
        tm)))


(define (update-constr constr subst)
    (mgu (map (lambda (c) (walk (car c) subst)) constr)
         (map (lambda (c) (walk (cdr c) subst)) constr)
         subst))

(define (update-cstore cstore subst)
    (cond
     ((not subst) #f)
     ((pair? cstore) (let ((first (update-constr (car cstore) subst))
                           (rest (update-cstore (cdr cstore) subst)))
                         (cond
                          ((or (null? first) (not rest)) #f)
                          ((not first) rest)
                          (else  (cons first rest)))))
     (else '())))
