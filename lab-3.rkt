;1a;
(define (pell n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((> n 1) (+ (* 2 (pell (- n 1))) (pell (- n 2))))))

;1b;

(define (find-pell n)
  (define x 2)
  (define (helper n x)
  (cond ((= n 0) 0)
        ((= n 1) 0)
        ((<= n (pell x)) (pell (- x 1)))
        (else (helper n (+ x 1)))))
  (helper n x))



;1c;
(define (comp-pell n)
  (cond ((= n 0) 2)
        ((= n 1) 2)
        ((> n 1) (+ (* 2 (comp-pell (- n 1))) (comp-pell (- n 2))))))

;1d;
(define (sqrt-2-approx n)
  (/ (/ (comp-pell n) 2) (pell n)))

(sqrt-2-approx 6)

;2;
(define (viete n)
  (define (viete-help x i)
    (let ((y (sqrt (+ 2 x))))
      (if (> i n) 1
          (* (/ y 2)
             (viete-help y (+ i 1))))))
  (* (/ (sqrt 2) 2) (viete-help (sqrt 2) 1)))

;3;
(define (new-sqrt x n)
  (define (continued y)
    (if (= y 0) 0
        (/ (- x 1) (+ 2 (continued (- y 1))))))
  (+ 1 (continued n)))

;4;
(define (m91 n)
  (cond
    ((< n 1) "invalid input")
    ((<= n 100) (m91 (m91 (+ n 11))))
    (else (- n 10))))