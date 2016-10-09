#lang racket
(require racket/draw)
(require "transistors.rkt" "euler.rkt" "lines.rkt")

(provide stick-diagram draw-stick-basic)
;;USE:
;;> (draw-stick-basic pud pdn)
;;> stick-diagram

(define stick-diagram (make-bitmap (size-window-lin size-janela) (size-window-col size-janela)))

(define dc
  (new bitmap-dc% [bitmap stick-diagram]))
(send dc set-smoothing 'aligned)

(define (draw-stick-basic pud pdn)
  (let-values [((line-pud line-pdn) (line-hor pud pdn))
               ((points-pud points-pdn) (points pud pdn))
               ((euler1 euler2 euler3 euler4) (euler-path pud pdn))
               ((line-id-pud line-id-pdn) (line-ver pud pdn))]
    (let* ((line-out (line-vout))
           (qnt-con (length euler2))
           (eq (equal-pos euler2 euler4))
           (x (/ (* 0.8 (size-window-lin size-janela)) qnt-con 2))
           (y-p-type (line-y0 (cadr (assoc Vout line-pud))))
           (y-n-type (line-y0 (cadr (assoc Vout line-pdn)))))
      (draw-id line-id-pud eq #t)
      (draw-id line-id-pdn eq #f)
      
      (draw-other-id line-pud euler2 points-pud line-id-pud x y-p-type)
      (draw-other-id line-pdn euler4 points-pdn line-id-pdn x y-n-type #f)
            
      (for ((i (in-list line-out)))
        (match i
          [(line x0 y0 x1 y1 color)
           (send dc set-pen color 2 'solid)
           (send dc set-brush color 'solid)
           (send dc set-smoothing 'smoothed)
           (send dc draw-line x0 y0 x1 y1)
           (send dc draw-text "Vout" (* 0.95 x1) y1)] ))
      ) ))

(define (draw-id line-id eq top?)
  (for ((i (in-list line-id)))
    (match i
      [(list id _ (line x0 y0 x1 y1 color))
       (send dc set-pen color 3 'solid)
       (send dc set-brush color 'solid)
       (send dc set-smoothing 'smoothed)
       (send dc draw-line x1 y0 x1 y1)
       (cond [(or top? (not (assc id eq)))
              (send dc draw-text (symbol->string id) x0 (if top? y0 y1))])] )))

(define (draw-other-id line-pud euler2 points-pud line-id x y-p-type [pud? #t])
  (for ((a (in-list line-pud)) (b (in-naturals)))
    (match a
      [(list id (line x0 y0 x1 y1 color))
       (cond [(or (equal? id Vdd) (equal? id Vss) (equal? id Vout))
              (send dc set-pen color 3 'solid)
              (send dc set-brush color 'solid)
              (send dc set-smoothing 'smoothed)
              (send dc draw-line x0 y0 x1 y1)])
       (let ((nos (car (filter-map
                        (lambda(i) (and (equal? (link-point i) id) (link-position i)))
                        points-pud))))
         (let loop ((node nos) (aux nos))
           (cond [(= (length node) 2)
                  ;;(displayln (~a id "  " node))
                  (let ((node1   (car (first node)))
                        (node2   (car (second node)))
                        (node1-p (cdr (first node)))
                        (node2-p (cdr (second node))))
                    (let ((index-node1 (index-of euler2 node1))
                          (index-node2 (index-of euler2 node2)))
                      (match (assoc node1 line-id)
                        [(list _ pol (line x1 y0 x1 y1 _))
                         (match (assoc node2 line-id)
                           [(list _ pol2 (line x2 y02 x2 y12 _))
                            (cond [(or (not (seguido index-node1 index-node2))
                                       (and (seguido index-node1 index-node2)
                                            (not (ligado pol node1-p pol2 node2-p
                                                         index-node1 index-node2)))
                                       (equal? id Vdd) (equal? id Vss))
                                   (send dc set-pen color 2 'solid)
                                   (send dc set-brush color 'solid)
                                   (send dc set-smoothing 'smoothed)
                                   (send dc draw-lines
                                         (list
                                          (make-object point%
                                            (if (zero? (index-of pol node1-p))
                                                (- x1 x)
                                                (+ x1 x))
                                            y-p-type)
                                          (make-object point%
                                            (if (zero? (index-of pol node1-p))
                                                (- x1 x)
                                                (+ x1 x))
                                            (line-y0 (cadr (assoc id line-pud))))
                                          (make-object point%
                                            (if (zero? (index-of pol2 node2-p))
                                                (- x2 x)
                                                (+ x2 x))
                                            (line-y0 (cadr (assoc id line-pud))))
                                          (make-object point%
                                            (if (zero? (index-of pol2 node2-p))
                                                (- x2 x)
                                                (+ x2 x))
                                            y-p-type)) )
                                   (send dc set-pen "black" 4 'solid)
                                   (send dc set-brush "black" 'solid)
                                   (send dc draw-ellipse 
                                         (if (zero? (index-of pol node1-p))
                                             (- x1 x)
                                             (+ x1 x))
                                         y-p-type 6 4)
                                   (send dc draw-ellipse 
                                         (if (zero? (index-of pol2 node2-p))
                                             (- x2 x)
                                             (+ x2 x))
                                         y-p-type 6 4)
                                   ])])
                         ])
                         ) )
                  (loop '() (cdr aux))]
                 [(> (length aux) 1) (loop (list (first aux) (second aux)) aux)]))
         )
       (cond [(equal? id Vout)
              (let ((nos (car (filter-map
                               (lambda(i) (and (equal? (link-point i) id) (link-position i)))
                               points-pud))))
                (for ((j (in-list nos)))
                  (match (assoc (car j) line-id)
                    [(list _ pol (line x1 y0 x1 y1 _))
                     (send dc set-pen "blue" 2 'solid)
                     (send dc set-brush "blue" 'solid)
                     (send dc set-smoothing 'smoothed)
                     (send dc draw-lines
                           (list
                            (make-object point%
                              (if (zero? (index-of pol (cdr j)))
                                  (- x1 x)
                                  (+ x1 x))
                              y-p-type)
                            (make-object point%
                              (if (zero? (index-of pol (cdr j)))
                                  (- x1 x)
                                  (+ x1 x))
                              (* 0.5 (size-window-col size-janela)))) )
                     (send dc set-pen "black" 4 'solid)
                     (send dc set-brush "black" 'solid)
                     (send dc draw-ellipse
                           (if (zero? (index-of pol (cdr j)))
                               (- x1 x)
                               (+ x1 x))
                           y-p-type
                           6 4)
                     ]))                      
                )])
       ]
      [_ #f]) ))

(define (seguido index-node1 index-node2)
  (or (= (add1 index-node1) index-node2)
      (= (sub1 index-node1) index-node2)))

(define (ligado pol node1-p pol2 node2-p index-node1 index-node2)
  (if (< index-node1 index-node2)
      (and (= (index-of pol node1-p) 1)
           (= (index-of pol2 node2-p) 0))
      (and (= (index-of pol node1-p) 0)
           (= (index-of pol2 node2-p) 1))) )

(define (index-of l x)
  (cond
    [(list? l)
         (for/or ([y l] [i (in-naturals)] #:when (equal? x y)) i)]
    [(pair? l)
         (if (equal? (car l) x) 0
             1)]))