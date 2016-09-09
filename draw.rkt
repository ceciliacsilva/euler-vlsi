#lang racket
(require racket/draw)
(require "transistors.rkt" "euler.rkt" "lines.rkt")

(define stick-diagram (make-bitmap (size-window-lin size-janela) (size-window-col size-janela)))

(define dc
  (new bitmap-dc% [bitmap stick-diagram]))
(send dc set-smoothing 'aligned)

(define (draw-stick-basic pud pdn)
  (let-values [((line-pud line-pdn) (line-hor pud pdn))
               ((points-pud points-pdn) (points pud pdn))
               ((euler1 euler2) (euler-path pud pdn))]
    (let* ((line-id (line-ver pud pdn))
           (qnt-con (length euler2))
           (x (/ (* 0.8 (size-window-lin size-janela)) qnt-con 2))
           (y-p-type (line-y0 (cadr (assoc Vout line-pud))))
           (y-n-type (line-y0 (cadr (assoc Vout line-pdn)))))
      ;;out
      (send dc set-pen "blue" 2 'solid)
      (send dc set-brush "blue" 'solid)
      (send dc set-smoothing 'smoothed)
      (send dc draw-line
            (* 0.1 (size-window-lin size-janela))
            (* 0.5 (size-window-col size-janela))
            (* 0.9 (size-window-lin size-janela))
            (* 0.5 (size-window-col size-janela)))

      (draw-other-id line-pud euler2 points-pud line-id x y-p-type)
      (draw-other-id line-pdn euler2 points-pdn line-id x y-n-type)

      (for ((i (in-list line-id)))
        (match i
          [(list id _ _ (line x0 y0 x1 y1 color))
           (send dc set-pen color 3 'solid)
           (send dc set-brush color 'solid)
           (send dc set-smoothing 'smoothed)
           (send dc draw-line x1 y0 x1 y1)] ))
      
      ) ))

(define (draw-other-id line-pud euler2 points-pud line-id x y-p-type [pud? #t])
  (for ((a (in-list line-pud)))
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
                  (let ((node1   (car (first node)))
                        (node2   (car (second node)))
                               (node1-p (cdr (first node)))
                               (node2-p (cdr (second node))))
                    (let ((index-node1 (index-of euler2 node1))
                          (index-node2 (index-of euler2 node2)))
                      (cond [(not (or (= (add1 index-node1) index-node2)
                                      (= (sub1 index-node1) index-node2)))
                             (match (assoc node1 line-id)
                               [(list _ p-pol n-pol (line x1 y0 x1 y1 _))
                                (match (assoc node2 line-id)
                                  [(list _ p-pol2 n-pol2 (line x2 y02 x2 y12 _))
                                   (cond [(not pud?) (set! p-pol  n-pol)
                                                     (set! p-pol2 n-pol2)])
                                   (send dc set-pen color 2 'solid)
                                   (send dc set-brush color 'solid)
                                   (send dc set-smoothing 'smoothed)
                                   (send dc draw-lines
                                         (list
                                          (make-object point%
                                            (if (zero? (index-of p-pol node1-p))
                                                (- x1 x)
                                                (+ x1 x))
                                            y-p-type)
                                          (make-object point%
                                            (if (zero? (index-of p-pol node1-p))
                                                (- x1 x)
                                                (+ x1 x))
                                            (line-y0 (cadr (assoc id line-pud))))
                                          (make-object point%
                                            (if (zero? (index-of p-pol2 node2-p))
                                                (- x2 x)
                                                (+ x2 x))
                                            (line-y0 (cadr (assoc id line-pud))))
                                          (make-object point%
                                            (if (zero? (index-of p-pol2 node2-p))
                                                (- x2 x)
                                                (+ x2 x))
                                            y-p-type)) )
                                   ])])
                             ] )) )
                  (loop '() aux)]
                 [(> (length aux) 1) (loop (list (first aux) (second aux)) (cdr aux))]))
         )
       (cond [(equal? id Vout)
              (let ((nos (car (filter-map
                               (lambda(i) (and (equal? (link-point i) id) (link-position i)))
                               points-pud))))
                ;(displayln nos)
                (for ((j (in-list nos)))
                  (match (assoc (car j) line-id)
                    [(list _ p-pol n-pol (line x1 y0 x1 y1 _))
                     (cond [(not pud?) (set! p-pol  n-pol)])
                     (send dc set-pen "blue" 2 'solid)
                     (send dc set-brush "blue" 'solid)
                     (send dc set-smoothing 'smoothed)
                     (send dc draw-lines
                           (list
                            (make-object point%
                              (if (zero? (index-of p-pol (cdr j)))
                                  (- x1 x)
                                  (+ x1 x))
                              y-p-type)
                            (make-object point%
                              (if (zero? (index-of p-pol (cdr j)))
                                  (- x1 x)
                                  (+ x1 x))
                              (* 0.5 (size-window-col size-janela)))) ) ]))                      
                )])
       ]
      [_ #f]) ))

(define (index-of l x)
  (cond
    [(list? l)
         (for/or ([y l] [i (in-naturals)] #:when (equal? x y)) i)]
    [(pair? l)
         (if (equal? (car l) x) 0
             1)]))