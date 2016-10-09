#lang racket

(require "transistors.rkt" "euler.rkt")

(provide (all-defined-out))

(define-struct size-window
  (col lin) #:transparent)

(define-struct line
  (x0 y0 x1 y1 color) #:transparent)

(provide size-janela)
(define size-janela (make-size-window 600 800))

(define (polarity pud pdn)
  (let-values [((points-pud points-pdn) (points pud pdn))
               ((euler1 euler2 euler3 euler4) (euler-path pud pdn))
               ((no-start pud?) (path-start pud pdn))]
    (values (polarity-euler euler1 pud)
            (polarity-euler euler3 pdn))
    )
  )

(define (polarity-euler euler1 g)
  (for/list ((euler-transistor (in-list euler1)))
    (match euler-transistor
      [(transistor id pol)
       (let ((pol-g (transistor-points (filter-transistor id g))))
         (if (equal? pol pol-g)
             (list id ordem)
             (list id (cons (cdr ordem) (car ordem)))) )]) ))

             
(define (line-hor pud pdn)
  (let-values [((points-pud points-pdn) (points pud pdn))
               ((euler1 euler2 euler3 euler4) (euler-path pud pdn))]
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
               ((euler1 euler2 euler3 euler4) (euler-path pud pdn))
               ((polarity-pud polarity-pdn) (polarity pud pdn))]
    (let* ((qnt-con (length euler2))
           (eq (equal-pos euler2 euler4))
           (x  (/ (* 0.8 (size-window-lin size-janela)) qnt-con))
           (y0 (* 0.15 (size-window-col size-janela)))
           (y1 (* 0.4  (size-window-col size-janela)))
           (y2 (* 0.6  (size-window-col size-janela)))
           (y3 (* 0.85 (size-window-col size-janela))) )
      (values (lines-ver polarity-pud euler2 eq x y0 y1 y0)
              (lines-ver polarity-pdn euler4 eq x y2 y3 y1))
      )))

(define (lines-ver polarity euler eq x y0 y1 yc)
  (for/list ((id (in-list euler)) (i (in-naturals 1)))
    (cond [(assc id eq) 
           (list id (cadr (assoc id polarity))
                 (make-line (* i x) yc (* i x) y1 "red"))]
          [(list id (cadr (assoc id polarity))
                 (make-line (* i x) y0 (* i x) y1 "red"))] )))

(define (equal-pos e1 e2)
  (let loop [(l1 e1) (l2 e2) (cont 0) (acc '())]
    (if (or (null? l1) (null? l2)) acc
        (cond [(equal? (car l1) (car l2))
               (loop (cdr l1) (cdr l2) (+ cont 1) (cons (list (car l1) cont) acc))]
              [else (loop (cdr l1) (cdr l2) (+ cont 1) acc)]) )) )

(define (assc A lista)
  (if (assoc A lista) #t
      #f))

(define (line-vout)
  (list
   (make-line
    (* 0 (size-window-lin size-janela))
    (* 0.5 (size-window-col size-janela))
    (* 1 (size-window-lin size-janela))
    (* 0.5 (size-window-col size-janela))
    "blue")))

