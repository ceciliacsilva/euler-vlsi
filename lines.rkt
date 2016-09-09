#lang racket

(require "transistors.rkt" "euler.rkt")

(provide size-window size-window-lin size-window-col line polarity
         line-x0 line-y0 line-x1 line-y1 line-color make-line
         line-ver line-hor line-vout)

(define-struct size-window
  (col lin) #:transparent)

(define-struct line
  (x0 y0 x1 y1 color) #:transparent)

(provide size-janela)
(define size-janela (make-size-window 600 800))

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

(define (line-hor pud pdn)
  (let-values [((points-pud points-pdn) (points pud pdn))
               ((euler1 euler2) (euler-path pud))]
    (let ((line-pud (length points-pud))
          (line-pdn (length points-pdn)))
      (let ((y2 (/ (* 0.3 (size-window-col size-janela)) line-pud))
            (y3 (/ (* 0.3 (size-window-col size-janela)) line-pdn))
            (x0 (* 0 (size-window-lin size-janela)))
            (x1 (* 1 (size-window-lin size-janela))))
        (values
         (for/list ((a (in-list points-pud))(i (in-naturals 1)))
           (let ((id (link-point a)) (color "black"))
             (cond [(or (equal? id Vdd) (equal? id Vss)) (set! color "blue")]
                   [(equal? id Vout) (set! color "brown")]
                   [else (set! color "blue")])
             (list id (make-line x0 (* i y2) x1 (* i y2) color))))
         (for/list ((a (in-list points-pdn))(i (in-naturals 1)))
           (let ((y (- (size-window-col size-janela) (* i y3))))
             (let ((id (link-point a)) (color "black"))
               (cond [(or (equal? id Vdd) (equal? id Vss)) (set! color "blue")]
                     [(equal? id Vout) (set! color "green")]
                     [else (set! color "blue")])
               (list id (make-line x0 y x1 y color)))) )) )) ))

(define (line-ver pud pdn)
  (let-values [((points-pud points-pdn) (points pud pdn))
               ((euler1 euler2) (euler-path pud))
               ((polarity-pud polarity-pdn) (polarity pud pdn))]
    (let* ((qnt-con (length euler2))
           (x  (/ (* 0.8 (size-window-lin size-janela)) qnt-con))
           (y0 (* 0.2 (size-window-col size-janela)))
           (y1 (* 0.8 (size-window-col size-janela))))
      (for/list ((id (in-list euler2)) (i (in-naturals 1)))
        (list id (cadr (assoc id polarity-pud))
              (cadr (assoc id polarity-pdn))
              (make-line (* i x) y0 (* i x) y1 "red"))) )))

(define (line-vout)
  (list
   (make-line
    (* 0.1 (size-window-lin size-janela))
    (* 0.5 (size-window-col size-janela))
    (* 0.9 (size-window-lin size-janela))
    (* 0.5 (size-window-col size-janela))
    "blue")))
         
