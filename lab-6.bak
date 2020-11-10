;1a;

(define (list-at l i)
  (if (= i 0) (car l)
      (list-at (cdr l) (- i 1))))

;1b;

(define (list-median list1)
  (define (selSort l)
    (define (smallest l)
      (define (smaller a b) (if (< a b) a b))
      (if (null? (cdr l))
          (car l)
          (smaller (car l) (smallest (cdr l)))))
    (define (remove v l)
      (if (null? l)
          l
          (if (equal? v (car l))
              (cdr l)
              (cons (car l) (remove v (cdr l))))))
    (if (null? l)
        '()
        (let* ((first (smallest l))
               (rest (remove first l)))
          (cons first (selSort rest)))))
  (let ((sorted (selSort list1)))
    (if (= 0 (modulo (length list1) 2))
        (/ (+ (list-at sorted (floor (/ (length list1) 2)))
              (list-at sorted (- (floor (/ (length list1) 2)) 1)))
           2)
        (list-at sorted (floor (/ (length list1) 2))))))

;2a;

(define (explode x)
  (if (< x 10) (list x)
      (append (explode (floor (/ x 10)))
              (list (- x (* 10 (floor (/ x 10))))))))

;2b;

(define (implode l)
  (define (add l place)
    (if (null? l) 0
        (+ (* (car l) (expt 10 place))
           (add (cdr l) (+ place 1)))))
  (add (reverse l) 0))

;2c;

(define (sum-list list)
  (if (null? list) 0
      (+ (car list)
         (sum-list (cdr list)))))

(define (has-property x)
  (let* ((int (explode x))
         (sum-nums (sum-list int))
         (sum (explode sum-nums))
         (sum-reverse (implode (reverse sum))))
    (= (* sum-nums sum-reverse) x)))

;2d;

(define (find sequence test n)
  (define (helper x found)
    (let ((g (sequence x)))
      (if (test g)
          (if (= (+ found 1) n)
              g
              (helper (+ x 1) (+ found 1)))
          (helper (+ x 1) found))))
  (helper 1 0))


  

