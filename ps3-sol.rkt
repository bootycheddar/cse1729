;1a;

(define (harmonic n)
  (cond
    ((= n 1) 1)
    (else (+ (/ 1.0 n) (harmonic (- n 1))))))
(harmonic 3)
;1b;

(define (Eulerest n)
  (abs (- (harmonic n) (log n))))

;2;

(define (prime? n)
  (define (divisor? k) (= 0 (modulo n k)))
  (define (divisors-upto k)
    (and (> k 1)
         (or (divisor? k) (divisors-upto (- k 1)))))
  (not (divisors-upto (- n 1))))

(define (count-primes m)
  (cond
    ((= m 1) 0)
    ((prime? m) (+ 1 (count-primes (- m 1))))
    (else (count-primes (- m 1)))))

;3;

(define (rel-prime a b)
  (define (divides-both d)
    (and (= 0 (modulo a d))
         (= 0 (modulo b d))))
  (define (divisor-upto k)
    (and (> k 1)
         (or (divides-both k)
             (divisor-upto (- k 1)))))
  (not (divisor-upto (min a b))))

(define (count-rel-prime n)
  (define (help b) (if b 1 0))
  (define (main count a b)
    (rel-prime a b)
  (cond
    ((and (= n a) (= n b))
     (+ count (help (rel-prime a b))))
    ((and (< a n) (= n b))
     (main (+ count (help (rel-prime a b))) (+ a 1) 1))
    (else
     (main (+ count (help (rel-prime a b))) a (+ b 1)))))
(main 0 1 1))

;4a;

(define (lucas n)
  (cond
    ((= n 0) 2)
    ((= n 1) 1)
    ((> n 1) (+ (lucas (- n 1)) (lucas (- n 2))))))

;4b;

(define (Lucas-ratio n)
  (/ (lucas n) (lucas (- n 1))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((> n 1) (+ (fib (- n 1)) (fib (- n 2))))))

(define (Fibonacci-ratio p)
  (/ (fib p) (fib (- p 1))))

(Fibonacci-ratio 20)

;4c;

(define (fast-Lucas-help n k lucas-a lucas-b)
  (if (= n k)
      lucas-a
      (fast-Lucas-help n (+ k 1) (+ lucas-a lucas-b) lucas-a)))

(define (fast-Lucas n) (fast-Lucas-help n 1 1 2))

(define (rec-call-lucas k)
    (cond ((= k 1)  0)
          ((= k 2)  2)
          ((= k 3)  4)
          ((= k 4)  8)
          ((= k 5)  14)
          ((= k 6)  24)
    ))

(define (rec-call-fast-lucas-helper k)
    (cond ((= k 1)  0)
          ((= k 2)  1)
          ((= k 3)  2)
          ((= k 4)  3)
          ((= k 5)  4)
          ((= k 6)  5)
    ))

;5a;

(define (P n)
  (if (= n 0) 0
      (+ (H (- n 1)) (P (- n 1)))))

(define (H n)
  (if (= n 0) 1
      (+ (* 2 (P (- n 1))) (H (- n 1)))))

;5b;



;5c;

(define (t n)
  (cond
    ((= n 0) 0)
    ((= 0 (modulo n 2)) (* 2 (expt (P n) 2)))
    (else (expt (H n) 2))))

;5d;

(define (s n)
  (* (H n) (P n)))

;5e;

(define (tri-square n)
  (/ (* (t n) (+ 1 (t n))) 2))

;5f;

(define (square-tri n)
  (expt (s n) 2))

;6a;

(define (golden-help m n)
  (if (= m n) 1
      (+ 1 (/ 1 (golden-help (+ m 1) n)))))
(define (golden n)
  (golden-help 0 n))

;6b;

(define (golden-sqrt n)
  (cond
    ((= n 0) 1)
    ((> n 0) (sqrt (+ 1 (golden (- n 1)))))))

;7;

(define (interval-sum m n)
  (if ((= m n) m)
      (+ m (interval-sum (+ m 1) (- n 1)) n)))

(define (explain-interval-sum)
  (define a "One can never do  an induction on both inputs at once.")
  (define b "The base case isn't quite right. It needs to be updated to account for the two inductive calls.")
  (define c "The inductive case should be adding three things together.")
  (define d "The predicate to recognize the base case is wrong. One can go from m > n to m < n without ever seeing n = m.")
  (define e "I have no idea.")
  d)

;8;

(define (ack m n)
  (cond
    ((= 0 m) (+ n 1))
    ((and (= n 0) (> m 0)) (ack (- m 1) 1))
    ((and (> m 0) (> n 0)) (ack (- m 1) (ack m (- n 1))))))

;9;

(define (catalan n)
  (define (helper k)
    (/ (+ n k) k))
  (define (main step x)
    (cond
      ((> step n) x)
      ((or (= step 0) (= step 1)) (main (+ 1 step) x))
      (else
       (main (+ 1 step) (* x (helper step))))))
  (main 0 1))



