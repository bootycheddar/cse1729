;1;

(define (windchill W F)
  (let ((Q (sqrt W)))
    (+ (- (+ 1.05 (* 0.93 F)) (* 3.65 W)) (* 3.62 Q) (* 0.103 F Q) (* 0.0439 (* W W)))))

;2;

(define (iradius A B C)
  (let ((S (/ (+ A B C) 2)))
    (sqrt
     (/
       (* (- S A) (- S B) (- S C)) S))))

;3;

(define (pressure h)
  (let ((p 101325)
       (L 0.0065)
       (T 288.15)
       (g 9.80665)
       (M 0.0289644)
       (R 8.31447))
  (let ((e (/ (* g M) (* R L))))
    (let ((b (- 1 (/ (* L h) T))))
      (* p (expt b e))))))

;4;
(define (inc x) (+ 1 x))
(define (square x) (* x x))

(define (compose f g)
 (define (helper x)
  (f (g x)))
    helper)

;5;

(define (repeated f n)
  (cond
    ((= 1 n) f)
    (else (compose f (repeated f (- n 1))))))


((repeated square 2) 5)

