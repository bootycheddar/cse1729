;1a;
(define (usd-to-euro x)
  (* x 0.84))
(usd-to-euro 250)
;1b;
(define (euro-to-yen x)
  (* x 125.92))
(euro-to-yen 250)
;1c;
(define (usd-to-yen x)
  (* x 105.7728))
(usd-to-yen 250)
;2a;
(define e 2.71828)
;2b;
(define (tanh x)
  (/ (- (expt e (* 2 x)) 1) (+ (expt e (* 2 x)) 1) ))
;3a;
(define (det2x2 a b c d)
  (- (* a d) (* b c)))
(det2x2 -3 1 2 7)
;3b;
(define (invertible? a b c d)
  (not(= 0 (det2x2 a b c d))))
(invertible? -3 1 2 7)
(invertible? 2 -4 -6 12)
;3ci;
(define (prod-inv-direct? a1 b1 c1 d1 a2 b2 c2 d2)
  (invertible? (+ (* a1 a2) (* b1 c2)) (+ (* a1 b2) (* b1 d2)) (+ (* c1 a2) (* d1 c2)) (+ (* c1 b2) (* d1 d2))))
;3cii;
(define (prod-inv-indirect? a1 b1 c1 d1 a2 b2 c2 d2)
  (not(= 0 (* (det2x2 a1 b1 c1 d1) (det2x2 a2 b2 c2 d2)))))
