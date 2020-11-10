;1a;

(define (encode p)
  (let ((x (car p)) (y (cdr p)))
    (if (= x (max x y))
        (+ (* x x) x y)
    (+ (* y y) x))))

;1b;

(define (square x) (* x x))
(define (decode z)
  (if (< (- z (square (floor (sqrt z)))) (floor (sqrt z)))
      (cons (- z (square (floor (sqrt z)))) (floor (sqrt z)))
  (cons (floor (sqrt z)) (- (- z (square (floor (sqrt z)))) (floor (sqrt z))))))

;2a;

(define (sub-complex c d)
  (cons (- (car c) (car d)) (- (cdr c) (cdr d))))

;2b;

(define (div-complex c d)
  (let ((a (car c)) (b (cdr c)) (c1 (car d)) (d1 (cdr d)))
    (cons (/ (+ (* a c1) (* b d1)) (+ (* c1 c1) (* d1 d1)))
          (/ (- (* b c1) (* a d1)) (+ (* c1 c1) (* d1 d1))))))

;3ai;

(define (sum-quadratic-roots a b c)
  (cons (* -1 (car (div-complex b a))) (* -1 (cdr (div-complex b a)))))

;3aii;

(define (prod-quadratic-roots a b c)
  (div-complex c a))

;3bi;

(define (sum-cubic-roots a b c d)
  (cons (* -1 (car (div-complex b a))) (* -1 (cdr (div-complex b a)))))

;3bii;

(define (sum-pairs-cubic-roots a b c d)
  (div-complex c a))

;3biii;

(define (prod-cubic-roots a b c d)
  (cons (* -1 (car (div-complex d a))) (* -1 (cdr (div-complex d a)))))

;4;

(define (zip listA listB)
  (if (or (null? listA) (null? listB))
      '()
      (cons (cons (car listA) (car listB))
            (zip (cdr listA) (cdr listB)))))

;5;

(define (unzip pairList)
  (if (null? pairList) '(() . ())
      (let* ((pile (unzip (cdr pairList)))
             (list1 (car pile))
             (list2 (cdr pile)))
        (cons (cons (car (car pairList)) list1) (cons (cdr (car pairList)) list2)))))

(unzip (list (cons 1 4) (cons 2 5) (cons 3 6)))
      