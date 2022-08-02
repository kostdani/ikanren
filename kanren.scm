
(define-module (ikanren kanren)
  #:use-module (ikanren variables)
  #:use-module (ikanren goals)
  #:use-module (ikanren core)
  #:use-module (ikanren state)
  #:use-module (ikanren syntax)
  #:use-module (ikanren db)
  #:re-export (== =/= neg ==> mk-pause exist define-relation define-dbrel fact! conde query run run* stream-take conj* disj* ))
