#lang racket

;;Cecília Carneiro e Silva
;;Euler path and circuit for undirected graph.
;;TODO: contracts

;;USE:
;;> (any-path '((a b) (b c) (c d) (d a)))
;;'(a d c b a)
;;> (any-path '((a . b) (b . c) (c . d) (d . a)))
;;'(a d c b a)
;;> (any-path '((a d) (d f) (f g) (g d) (d e) (e b) (b c) (c f) (f a)))
;;'(a f c b e d g f d a)

;;> (define pup '((P2 . Vdd) (P1 . Vdd) (P2 . P1) (Out . P2) (Out . P2)))
;;> (any-path pup)
;;'(P2 Out P2 P1 Vdd P2)
;;> (define pdn '((Out . P3) (P3 . Vss) (P3 . Vss) (Out . P4) (P4 . Vss)))
;;> (any-path pdn)
;;'(P3 Vss P4 Out P3 Vss)

(define (degree g x)
  ;;return degree of vertice x
  ;;'((a . b) (b . c)) or '((a b) (b c))
  (count (lambda(i)
           (match i
             [(or (list e1 e2) (cons e1 e2))
              (or (equal? x e1) (equal? x e2))]
             [_ #f])) g))

(define (euler? g)
  ;;return if the graph has at least one euler-path(0) or euler-circuit(1), else #f
  (let ((oddq
         (for/sum ((i (in-list (remove-duplicates (flatten g))))
                   #:when (odd? (degree g i)))
           1)))
    (cond [(= oddq 2) 0];;euler path
          [(= oddq 0) 1];;euler circuit
          [else       #f]) ))

(define (next? g x)
  ;;return next vertice
  (let loop ((graph g) (acc '()))
    (if (null? graph) acc
        (let ((i (car graph)))
          (match i
            [(or (list e1 e2) (cons e1 e2))
             (cond [(equal? e1 x) (loop (cdr graph) (cons e2 acc))]
                   [(equal? e2 x) (loop (cdr graph) (cons e1 acc))]
                   [else (loop (cdr graph) acc)]) ]
            [_ acc]) )) ))

(define (path-start g)
  ;;vertice to begin
  (let ((op (euler? g))
        (vertices (remove-duplicates (flatten g))))
    (cond [(= op 0)
           (findf (lambda(i) (odd? (degree g i))) vertices)];;euler-path choose odd vertex
          [(= op 1)
           (first vertices)];;euler circuit choose any-one
          [else #f]) ))

(define (any-path g [vertex (path-start g)])
  ;;find path
  (let ((next (next? g vertex)))
    (if (null? next)
        (list vertex) ;;end
        (let ((stack (car next)))
          (cons vertex (any-path (remove-edge vertex stack g) stack)) )) ))

(define (remove-edge st stack g)
  ;;remove one edge of a graph (directed and undirected)
  (let ((vlist (list st stack))
        (vpair (cons st stack)))
    (let ((new-g
           (if (list? (car g))
               (remove vlist g)
               (remove vpair g))))
      (if (equal? new-g g)
          (remove-edge stack st g)
          new-g)) ))
          
