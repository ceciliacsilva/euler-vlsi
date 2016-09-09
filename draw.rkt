#lang racket
(require racket/draw)
(require "transistors.rkt" "euler.rkt" "lines.rkt")

(define stick-diagram (make-bitmap (size-window-lin size-janela) (size-window-col size-janela)))

(define dc
  (new bitmap-dc% [bitmap stick-diagram]))
(send dc set-smoothing 'aligned)

(define (draw-stick-basic pud pdn)
  (match line-vdd
    [(line x0 y0 x1 y1 color)
     (send dc set-pen color 3 'solid)
     (send dc set-brush color 'solid)
     (send dc set-smoothing 'smoothed)
     (send dc draw-line x0 y0 x1 y1)])
  (match line-vss
    [(line x0 y0 x1 y1 color)
     (send dc set-pen color 3 'solid)
     (send dc set-brush color 'solid)
     (send dc set-smoothing 'smoothed)
     (send dc draw-line x0 y0 x1 y1)])
  (match line-p-type
    [(line x0 y0 x1 y1 color)
     (send dc set-pen color 3 'solid)
     (send dc set-brush color 'solid)
     (send dc set-smoothing 'smoothed)
     (send dc draw-line x0 y0 x1 y1)])
  (match line-n-type
    [(line x0 y0 x1 y1 color)
     (send dc set-pen color 3 'solid)
     (send dc set-brush color 'solid)
     (send dc set-smoothing 'smoothed)
     (send dc draw-line x0 y0 x1 y1)])
  (let ((poly (line-poly pud pdn)))
    (for ((col-poly (in-list poly)))
      (match col-poly
        [(line x0 y0 x1 y1 color)
         (send dc set-pen color 3 'solid)
         (send dc set-brush color 'solid)
         (send dc set-smoothing 'smoothed)
         (send dc draw-line x0 y0 x1 y1)]) ))
  (let-values [((points-pud points-pdn) (points pud pdn))
               ((euler1 euler2) (euler-path pud))
               ((polarity-pud polarity-pdn) (polarity pud pdn))]
    (let ((vdd (car (filter-map
                     (lambda(i) (and (equal? (link-point i) Vdd) (link-position i))) points-pud))))
      (for ((i (in-list vdd)))
        (match i
          [(cons id SD)
           (let ((no-num (+ 1 (index-of euler2 id)))
                 (div    (/ (* 0.8 (size-window-lin size-janela)) (length euler2)))
                 (y0     (* 0.1  (size-window-col size-janela)))
                 (y1     (* 0.3  (size-window-col size-janela)))
                 (pol    (cadr (assoc id polarity-pud))))
             (send dc set-pen "blue" 2 'solid)
             (send dc set-brush "blue" 'solid)
             (send dc set-smoothing 'smoothed)
             (if (equal? (index-of pol SD) 0)
                 (send dc draw-line (- (* no-num div) (/ div 2)) y0 (- (* no-num div) (/ div 2)) y1)
                 (send dc draw-line (+ (* no-num div) (/ div 2)) y0 (+ (* no-num div) (/ div 2)) y1)) )]
          [_ #f]))
           
      )
    (let ((vss (car (filter-map
                     (lambda(i) (and (equal? (link-point i) Vss) (link-position i))) points-pdn))))
      (for ((i (in-list vss)))
        (match i
          [(cons id SD)
           (let ((no-num (+ 1 (index-of euler2 id)))
                 (div    (/ (* 0.8 (size-window-lin size-janela)) (length euler2)))
                 (y0     (* 0.9  (size-window-col size-janela)))
                 (y1     (* 0.7  (size-window-col size-janela)))
                 (pol    (cadr (assoc id polarity-pdn))))
             (send dc set-pen "blue" 2 'solid)
             (send dc set-brush "blue" 'solid)
             (send dc set-smoothing 'smoothed)
             (if (equal? (index-of pol SD) 0)
                 (send dc draw-line (- (* no-num div) (/ div 2)) y0 (- (* no-num div) (/ div 2)) y1)
                 (send dc draw-line (+ (* no-num div) (/ div 2)) y0 (+ (* no-num div) (/ div 2)) y1)) )]
          [_ #f]))
      )
    )
  )

(define (index-of l x)
  (cond
    [(list? l)
         (for/or ([y l] [i (in-naturals)] #:when (equal? x y)) i)]
    [(pair? l)
         (if (equal? (car l) x) 0
             1)]))