;1;
(define (geom-series-np2 n)
  (if (= n 0)
      1
      (+ (/ 1 (expt 2 n))
         (geom-series-np2 (- n 1)))))
;2;
(define (num-digits n)
  (if (< n 10)
      1
      (+ 1 (num-digits (/ n 10)))))
;3a;
(define (a n)
  (if (= 1 n)
      2
      (* 2 (a (- n 1)))))
;3b;
(define (num-ancestors n)
  (if (= n 1)
      2
      (+ (a n) (num-ancestors (- n 1)))))
;4;
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
(define (n-choose-k n k)
  (/ (factorial n)
     (* ( factorial (- n k)) (factorial k))))
;5;
(define (pascals-triangle n k)
  (cond ((and (= 0 k) (= 0 n)) 1)
        ((< n k) 0)
        ((< k 0) 0)
        (#t (+ (pascals-triangle (- n 1) k)
           (pascals-triangle (- n 1) (- k 1))))))
  