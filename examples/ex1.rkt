#lang racket

(require "../draw.rkt" "../lines.rkt" "../euler.rkt" "../transistors.rkt")

;;F=(not (or (and A B) (and C (or A B))))

(define F-pud (list (make-transistor 'A '(P1 . Vdd)) (make-transistor 'B '(P1 . Vdd))
                    (make-transistor 'C '(Out . P1)) (make-transistor 'Al '(P2 . P1)) (make-transistor 'Bl '(Out . P2))))
(define F-pdn (list (make-transistor 'A '(Out . P3)) (make-transistor 'B '(P3 . Vss))
                    (make-transistor 'C '(Out . P4)) (make-transistor 'Al '(P4 . Vss)) (make-transistor 'Bl '(P4 . Vss))))

(draw-stick-basic F-pud F-pdn)

stick-diagram