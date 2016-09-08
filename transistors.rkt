#lang racket

(provide ordem points transistor make-transistor transistor-id transistor-points link)

(define ordem '(S . D))

(define-struct transistor
  (id points) #:transparent)

(define-struct link
  (point position) #:transparent)

(define (points pud pdn)
  (let ((ids
         (map (lambda(i) (transistor-id i)) pud))
        (pud-names
         (remove-duplicates (flatten (map (lambda(i) (transistor-points i)) pud))))
        (pdn-names
         (remove-duplicates (flatten (map (lambda(i) (transistor-points i)) pdn)))) )
    (let ((points-pud
           (for/list ((point (in-list pud-names)))
             (make-link point
                        (let loop [(pud-list pud) (acc '())]
                          (if (null? pud-list) acc
                              (match (car pud-list)
                                [(transistor id (cons point1 point2))
                                 (cond [(equal? point1 point)
                                        (loop (cdr pud-list) (cons (cons point (car ordem)) acc) )]
                                       [(equal? point2 point)
                                        (loop (cdr pud-list) (cons (cons point (cdr ordem)) acc) )]
                                       [else (loop (cdr pud-list) acc)])]
                                [_ (loop (cdr pud-list) acc)]) )) )) )
          (points-pdn
           (for/list ((point (in-list pdn-names)))
             (make-link point
                        (let loop [(pdn-list pdn) (acc '())]
                          (if (null? pdn-list) acc
                              (match (car pdn-list)
                                [(transistor id (cons point1 point2))
                                 (cond [(equal? point1 point)
                                        (loop (cdr pdn-list) (cons (cons point (car ordem)) acc) )]
                                       [(equal? point2 point)
                                        (loop (cdr pdn-list) (cons (cons point (cdr ordem)) acc) )]
                                       [else (loop (cdr pdn-list) acc)])]
                                [_ (loop (cdr pdn-list) acc)]) )) )) ) )
          (values points-pud points-pdn)
    ))
  )

;;pud e pdn tests
(provide pud pdn)
(define pud (list (make-transistor 'A '(P2 . Vdd)) (make-transistor 'D '(P1 . Vdd))
                  (make-transistor 'E '(P2 . P1)) (make-transistor 'B '(Out . P2)) (make-transistor 'C '(Out . P2))))
(define pdn (list (make-transistor 'A '(Out . P3)) (make-transistor 'D '(P3 . Vss))
                  (make-transistor 'E '(P3 . Vss)) (make-transistor 'B '(Out . P4)) (make-transistor 'C '(P4 . Vss))))
