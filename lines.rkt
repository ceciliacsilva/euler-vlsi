#lang racket

(require "transistors.rkt" "euler.rkt")

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


                      
  