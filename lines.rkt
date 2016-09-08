#lang racket

(require "transistors.rkt" "euler.rkt")

(define-struct size-window
  (lin col) #:transparent)

(define-struct line
  (x0 y0 x1 y1 color) #:transparent)

(define size-janela (make-size-window 100 200))

(define (polarity pud pdn)
  (let-values [((points-pud points-pdn) (points pud pdn))
               ((euler1 euler2) (euler-path pud))]
    (let ((polarity-pud
           (for/list ((point (in-list euler1)))
             (match point
               [(transistor id _)
                (let ((point-euler
                       (car (filter-map (lambda(a) (and (equal? (transistor-id a) id) a)) pud))))
                  (if (equal? point point-euler)
                      (cons id (list ordem))
                      (cons id (list (cons (cdr ordem) (car ordem)))))
                  )] )) )
          (polarity-pdn
           (let loop ((ids euler2) (point (path-start pdn #f)) (acc '()))
             (if (null? ids) (reverse acc)
                 (let ((id (car ids)))
                   (let ((pdn-element
                          (car (filter-map
                                (lambda(a) (and (equal? (transistor-id a) id) a)) pdn))))
               (match pdn-element
                 [(transistor _ (cons p1 p2))
                  (cond [(equal? p1 point)
                         (loop (cdr ids) p2 (cons (cons id (list ordem)) acc))]
                        [(equal? p2 point)
                         (loop (cdr ids) p1
                               (cons (cons id (list (cons (cdr ordem) (car ordem)))) acc))]
                        [else (loop (cdr ids) point acc)])]
                 [_ (loop (cdr ids) point acc)]) )) ))) )
      (values polarity-pud polarity-pdn)
      ))
  )

(define line-vdd
  (let ((pos-y (* 0.1  (size-window-lin size-janela)))
        (x0    (* 0.05 (size-window-col size-janela)))
        (x1    (* 0.95 (size-window-col size-janela))))
    (make-line x0 pos-y x1 pos-y 'blue)))

(define line-vss
  (let ((pos-y (* 0.9  (size-window-lin size-janela)))
        (x0    (* 0.05 (size-window-col size-janela)))
        (x1    (* 0.95 (size-window-col size-janela))))
    (make-line x0 pos-y x1 pos-y 'blue)))

(define line-p-type
  (let ((pos-y (* 0.3  (size-window-lin size-janela)))
        (x0    (* 0.05 (size-window-col size-janela)))
        (x1    (* 0.95 (size-window-col size-janela))))
    (make-line x0 pos-y x1 pos-y 'brown)))

(define line-n-type
  (let ((pos-y (* 0.7  (size-window-lin size-janela)))
        (x0    (* 0.05 (size-window-col size-janela)))
        (x1    (* 0.95 (size-window-col size-janela))))
    (make-line x0 pos-y x1 pos-y 'green)))

(define (line-poly pud pdn)
  (let-values [((euler1 euler2) (euler-path pud))]
    (let* ((y0  (* 0.25 (size-window-lin size-janela)))
           (y1  (* 0.75 (size-window-lin size-janela)))
           (dis (* 0.1 (size-window-col size-janela)))
           (div (/ (* 0.8 (size-window-col size-janela)) (length euler2))))
      (for/list ((id (in-list euler2)) (n (in-naturals 1)))
        (make-line (+ dis (* n div)) y0 (+ dis (* n div)) y1 'red))) ))
                  