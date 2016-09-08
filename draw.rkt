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
  )
