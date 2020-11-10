;1a;
(define (fizzbuzz x)
  (cond
    ((and (= 0 (modulo x 5)) (= 0 (modulo x 3))) "fizzbuzz")
    ((= 0 (modulo x 3)) "fizz")
    ((= 0 (modulo x 5)) "buzz")
    (else x)))


;1b;
(define (fizzbuzz2 x)
   (define (fizz x)
     (cond
       ((= 0 (modulo x 3)) "fizz")
       (else "")))
   (define (buzz x)
     (cond
       ((= 0 (modulo x 5)) "buzz")
       (else "")))
   (string-append (fizz x) (buzz x)))

;2;
(define (piecewise x)
  (cond
    ((< x -3.142) (- -x 3.142))
    ((> x (* 2 3.142)) (- x (* 2 3.142)))
    (else (sin x))))

;3;
(define (inc a) (+ a 1))
(define (dec a) (- a 1))
(define (add n m)
   (if (= 0 n) m (add (dec n) (inc m))))

;4;
(define (mult n m)
  (cond
     ((= 1 m) n)
     (else (add n (mult n (dec m))))))

;5;
(define (power b n)
  (cond
    ((= 0 n) 1)
    ((= 0 b) 0)
    (else (mult b (power b (dec n))))))

;6;
(define (square x)
  (mult x x))
(define (raise x n)
  (cond
    ((= 0 n) 1)
    ((= 0 x) 0)
    ((even? n) (square (raise x (/ n 2))))
    ((odd? n) (mult x (square (raise x (floor (/ n 2))))))))

;7a;
(define (sumEven n)
  (cond
    ((= n 0) 0)
    ((even? n) (+ n (sumEven (- n 2))))
    (else (sumEven (- n 1)))))

;7b;
(define (sumOdd n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    ((odd? n) (+ n (sumOdd (- n 2))))
    (else (sumOdd (- n 1)))))

;8;
(define (h-product k)
  (if (= k 1) 1
      (* (- 1 (/ 1 k)) (h-product (- k 1)))))

;9;

(define (divides a b) (= 0 (modulo b a)))

(define (divisors-upto n k)
  (cond
    ((= k 0) 0)
    ((= n 0) 0)
    ((= k 1) 1)
    ((divides k n) (+ 1 (divisors-upto n (- k 1))))
    (else (divisors-upto n (- k 1)))))

(define (divisors n) (divisors-upto n n))

;10;

(define (subfact n)
  (cond
    ((= n 0) 1)
    ((= n 1) 0)
    ((> n 1) (* (- n 1) (+ (subfact (- n 1)) (subfact (- n 2)))))))

;11;

(define (factorial n)
  (if (= n 0) 1
      (* n (factorial (- n 1)))))

(define (new-cos x n)
  (if (= n 0) 1
      (+ (new-cos x (- n 1)) (/ (* (expt -1.0 n) (expt x (* 2 n))) (factorial (* 2 n))))))
   