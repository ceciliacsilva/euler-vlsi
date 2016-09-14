#lang racket

;;Cecília Carneiro e Silva
;;Euler path to vlsi, stick diagrams.

(require "transistors.rkt")

(provide euler-path path-start)

(define (reach graph x)
  (let ((visited (list x)))
    (let loop ((g graph)
               (next-point x)
               (next (next? graph x)))
      (cond [(not (null? next))
             (for ((next-step (in-list next)))
               (let ((next-edge   (car next-step))
                     (next-vertex (cdr next-step)))
                 (cond ((not (member next-vertex visited))
                        (set! visited (cons next-vertex visited))
                        (loop (remove-edge next-edge g) next-vertex
                              (next? g next-vertex))
                        ))
                 ))])
      (length visited)
      )) )

(define (degree g x)
  (count (lambda(i)
           (match i
             [(transistor _ (cons e1 e2))
              (or (equal? x e1) (equal? x e2))]
             [_ #f])) g))

(define (next? g x)
  ;;return next vertice
  (let loop ((graph g) (acc '()))
    (if (null? graph) acc
        (let ((i (car graph)))
          (match i
            [(transistor id (cons e1 e2))
             (cond [(equal? e1 x) (loop (cdr graph) (cons (cons id e2) acc))]
                   [(equal? e2 x) (loop (cdr graph) (cons (cons id e1) acc))]
                   [else (loop (cdr graph) acc)]) ]
            [_ acc]) )) ))

(define (path-start pud pdn)
  ;;vertice to begin
  (let* ((pud-point (map (lambda(a) (transistor-points a)) pud))
         (vertices-pud (remove-duplicates (flatten pud-point))))
    (let* ((pdn-point (map (lambda(a) (transistor-points a)) pdn))
           (vertices-pdn (remove-duplicates (flatten pdn-point))))
      (let ((start-ele-pud
             (findf (lambda(i) (odd? (degree pud i))) vertices-pud))
            (start-ele-pdn
             (findf (lambda(i) (odd? (degree pdn i))) vertices-pdn)))
        (cond [start-ele-pud (values start-ele-pud #t)]
              [start-ele-pdn (values start-ele-pdn #f)]
              [else (values Vdd #t)]) )) ))

(define (euler-path pud pdn)
  (let-values [((start-ele pud?) (path-start pud pdn))]
    (cond [pud? (any-path pud start-ele)]
          [else (any-path pdn start-ele)]) ))

(define (any-path g vertex)
  ;;find path
  (let ((next  (next? g vertex)))
    (if (null? next)
        (values '() '())
        (let ((next-ele
               (if (<= (length (remove-duplicates (flatten (map cdr next)))) 1) 0
                   (for/or [(a (in-list next)) (i (in-naturals))]
                     (and (<= (reach g vertex) (reach (remove-edge (car a) g) vertex)) i))
                   )))
          (let* ((stack (list-ref next next-ele))
                 (id    (car stack)))
            (let-values [((euler1 euler2) (any-path (remove-edge id g) (cdr stack)))]
              (values (cons (make-transistor id (cons vertex (cdr stack)))  euler1)
                      (cons id euler2)))) )) ))
        
(define (remove-edge id g)
  (filter-map (lambda(a)
                (and (not (or (equal? (transistor-id a) id)
                              (equal? (transistor-id a) id) )) a ))
              g) )
  