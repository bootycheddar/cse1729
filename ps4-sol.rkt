;1;

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (harmonic n)
  (define (term a)
    (/ 1.0 a))
  (define (next b)
    (+ b 1))
  (sum term 1 next n))

;2a;

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;2b;

(define (wallis-pi n)
  (define (term n)
    (* (/ (* 2 n) (- (* 2 n) 1)) (/ (* 2 n) (+ (* 2 n) 1))))
  (define (next n)
    (+ n 1))
  (product term 1 next n))

;3;

(define (frac-sum f g n)
  (define (helper f g n m)
    (cond
      ((> n m) 0)
      ((= 0 (g n)) (helper f g (+ n 1) m))
      (else (+ (/ (f n) (g n))
               (helper f g (+ n 1) m)))))
  (helper f g (* -1 n) n))

;4a;

(define (der f h)
  (define (g x)
    (/ (- (f (+ x h)) (f x))
       h))
  g)

;4b;

(define (der-n f n h)
  (if (= n 0) f
      (der-n (der f h) (- n 1) h)))

;5;

(define (newton f x n)
  (define der-f (der f 0.01))
  (define (next x k)
    (let ((x1 (- x (/ (f x) (der-f x)))))
      (if (= k 0) x
          (next x1 (- k 1)))))
  (next x n))

;a;

(define (f x)
  (- (* 2 (* x x)) 1))
  
(newton f 4 0)

;b;

(define (g x)
  (- (- (* x x) x) 1))

(newton g 2 0)

;6;

(define (sum-term term a b)
  (if (> a b)
      0
      (+ (term a) (sum-term term (+ a 1) b))))

(define (simpson-integrate f a b n)
  (let ((dx (/ (- b a) n)))
    (define (p q)
      (f (+ a (* dx q))))
    (define (helps q)
      (cond
        ((= q 0) (p q))
        ((= q n) (p q))
        ((= 0 (modulo q 2)) (* 2 (p q)))
        (else (* 4 (p q)))))
    (* (/ dx 3) (sum-term helps 0 n))))
    

