#lang racket

;;Cecília Carneiro e Silva
;;Euler path to vlsi, stick diagrams.

(require "transistors.rkt")

(provide euler-path path-start)

(define Vdd 'Vdd)
(define Vss 'Vss)
  
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

(define (path-start g [pud? #t])
  ;;vertice to begin
  (if pud? Vdd Vss))

(define (euler-path g [pud? #t])
  (any-path g (path-start g pud?)))

(define (any-path g vertex)
  ;;find path
  (let ((next  (next? g vertex)))
    (if (null? next)
        (values '() '())
        (let* ((stack (car next))
               (id    (car stack)))
          (let-values [((euler1 euler2) (any-path (remove-edge vertex stack g) (cdr stack)))]
            (values (cons (make-transistor id (cons vertex (cdr stack)))  euler1)
                    (cons id euler2)))) )) )
        
(define (remove-edge point1 edge g)
  (let ((id     (car edge))
        (point2 (cdr edge)))
  (filter-map (lambda(a)
                (and (not (or (equal? a (make-transistor id (cons point1 point2)))
                              (equal? a (make-transistor id (cons point2 point1))) )) a ))
              g) ))
  