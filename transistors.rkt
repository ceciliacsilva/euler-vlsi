#lang racket

(provide (all-defined-out))
(define ordem '(S . D))

(define Vdd  'Vdd)
(define Vss  'Vss)
(define Vout 'Out)

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
                                        (loop (cdr pud-list) (cons (cons id (car ordem)) acc) )]
                                       [(equal? point2 point)
                                        (loop (cdr pud-list) (cons (cons id (cdr ordem)) acc) )]
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
                                        (loop (cdr pdn-list) (cons (cons id (car ordem)) acc) )]
                                       [(equal? point2 point)
                                        (loop (cdr pdn-list) (cons (cons id (cdr ordem)) acc) )]
                                       [else (loop (cdr pdn-list) acc)])]
                                [_ (loop (cdr pdn-list) acc)]) )) )) ) )
      (let* ((vdd-ele (car (filter-map
                            (lambda(a) (and (equal? (link-point a) Vdd) a)) points-pud)))
             (vdd-index  (index-of points-pud vdd-ele))
             (vss-ele (car (filter-map
                            (lambda(a) (and (equal? (link-point a) Vss) a)) points-pdn)))
             (vss-index  (index-of points-pdn vss-ele))
             (pud-2 (list-swap points-pud vdd-index 0))
             (pdn-2 (list-swap points-pdn vss-index 0))
             (vout1-ele   (car (filter-map
                                (lambda(a) (and (equal? (link-point a) Vout) a)) pud-2)))
             (vout1-index (index-of pud-2 vout1-ele))
             (vout2-ele   (car (filter-map
                                (lambda(a) (and (equal? (link-point a) Vout) a)) pdn-2)))
             (vout2-index (index-of pdn-2 vout2-ele))
             (pud-3 (list-swap pud-2 vout1-index (round (/ (length points-pud) 2))))
             (pdn-3 (list-swap pdn-2 vout2-index (round (/ (length points-pdn) 2)))))
        (values pud-3 pdn-3) )
    ))
  )

(define (filter-transistor id g)
  (car (filter-map
        (lambda(a) (and (equal? (transistor-id a) id) a)) g)))
  
(define (list-swap list1 n1 n2)
  (let* ((ele1 (list-ref list1 n1))
         (ele2 (list-ref list1 n2))
         (n-list (list-set list1 n1 ele2))
         (f-list (list-set n-list n2 ele1)))
    f-list))

(define (nodes pud pdn)
  (let [(nodes-pud (remove-duplicates
                    (flatten (map (lambda(a) (transistor-points a)) pud))))
        (nodes-pdn (remove-duplicates
                    (flatten (map (lambda(a) (transistor-points a)) pdn))))]
    (values nodes-pud nodes-pdn) ))

(define (index-of l x)
  (cond
    [(list? l)
         (for/or ([y l] [i (in-naturals)] #:when (equal? x y)) i)]
    [(pair? l)
         (if (equal? (car l) x) 0
             1)]))

;;pud e pdn tests - A(D + E) + BC
(provide pud pdn)
(define pud (list (make-transistor 'A '(P2 . Vdd)) (make-transistor 'D '(P1 . Vdd))
                  (make-transistor 'E '(P2 . P1)) (make-transistor 'B '(Out . P2)) (make-transistor 'C '(Out . P2))))
(define pdn (list (make-transistor 'A '(Out . P3)) (make-transistor 'D '(P3 . Vss))
                  (make-transistor 'E '(P3 . Vss)) (make-transistor 'B '(Out . P4)) (make-transistor 'C '(P4 . Vss))))

;pud2 pdn2 - AB + E + CD
(provide pud2 pdn2)
(define pud2 (list (make-transistor 'A '(P1 . Vdd)) (make-transistor 'B '(P1 . Vdd))
                   (make-transistor 'E '(P2 . P1)) (make-transistor 'D '(Out . P2)) (make-transistor 'C '(Out . P2))))
(define pdn2 (list (make-transistor 'A '(Out . P3)) (make-transistor 'B '(P3 . Vss))
                   (make-transistor 'E '(Out . Vss)) (make-transistor 'D '(Out . P4)) (make-transistor 'C '(P4 . Vss))))

;;pud3 pdn3 - ABC + DE
(provide pud3 pdn3)
(define pud3 (list (make-transistor 'A '(P1 . Vdd)) (make-transistor 'B '(P1 . Vdd))
                   (make-transistor 'C '(P1 . Vdd)) (make-transistor 'D '(Out . P1)) (make-transistor 'E '(Out . P1))))
(define pdn3 (list (make-transistor 'A '(Out . P2)) (make-transistor 'B '(P2 . P3))
                   (make-transistor 'C '(P3 . Vss)) (make-transistor 'D '(P4 . Vss)) (make-transistor 'E '(Out . P4))))


