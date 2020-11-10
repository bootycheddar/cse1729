;1;

(define (ins x l)
  (cond
    ((null? l) (list x))
    ((<= x (car l)) (cons x l))
    (else (cons (car l) (ins x (cdr l))))))

;2;

(define (insSort l)
  (if (null? l) l
    (ins (car l) (insSort (cdr l)))))

;3a;

(define (fold-right op initial sequence)
  (if (null? sequence) initial
      (op (car sequence) (fold-right op initial (cdr sequence)))))

;3b;

(define (fold-left op initial sequence)
  (if (null? sequence) initial
      (fold-left op
                 (op (car sequence) initial)
                 (cdr sequence))))

(fold-left + 0 (list 1 2 3 4 5))
  
(fold-left * 1 (list 1 2 3 4 5))
  
(fold-left cons '() (list 1 2 3 4 5))

;3c;

(define (my-map p sequence)
  (fold-right (lambda (x y) (cons (p x) y)) '() sequence))

;3d;

(define (my-append seq1 seq2)
  (fold-right cons seq2 seq1))

;3e;

(define (my-length sequence)
  (fold-right (lambda (x y) (+ y 1)) 0 sequence))

;3f;

(define (reverse-r sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

;3g;

(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons x y)) '() sequence))

;3h;

(define (horner-eval x coefficient-list)
  (fold-right (lambda (coefficient terms) (+ coefficient (* x (if (pair? terms) (horner-eval x terms) terms))))
              0
              coefficient-list))

;4ai;

(define (prime? n)
  (define (divisor? k) (= 0 (modulo n k)))
  (define (divisors-upto k)
    (and (> k 1)
         (or (divisor? k) (divisors-upto (- k 1)))))
  (not (divisors-upto (- n 1))))

(define (countDigits n)
  (if (< n 10) 1
      (+ 1 (countDigits (/ n 10)))))

(define (left-truncatable-prime? p)
  (cond
     ((= p 0) #t)
     ((= p 1) #f)
     ((not (prime? p)) #f)
     (else (left-truncatable-prime? (modulo p (expt 10 (- (countDigits p) 1)))))))

;4aii;

(define (find sequence test n)
  (define (helper x found)
    (let ((g (sequence x)))
      (if (test g)
          (if (= (+ found 1) n)
              g
              (helper (+ x 1) (+ found 1)))
          (helper (+ x 1) found))))
  (helper 1 0))

(define (nth-left-trunc-prime n)
  (find (lambda (x) x) left-truncatable-prime? n))
  

;4bi;

(define (right-truncatable-prime? p)
   (cond
     ((= p 0) #t)
     ((= p 1) #f)
     ((not (prime? p)) #f)
     (else (right-truncatable-prime? (floor (/ p 10))))))

;4bii;

(define (nth-right-trunc-prime n)
  (find (lambda (x) x) right-truncatable-prime? n))

;4ci;

(define (two-sided-prime? p)
  (and (left-truncatable-prime? p) (right-truncatable-prime? p)))

;4cii;

(define (nth-two-sided-prime n)
  (find (lambda (x) x) two-sided-prime? n))

;5;

(define (make-tree value left right) (list value left right))
(define (value tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))

(define (tree-map T f)
  (if (null? T) '()
      (make-tree (f (value T))
                 (tree-map (left T) f)
                 (tree-map (right T) f))))

;6;

(define (tree-equal? t1 t2)
  (or (eq? t1 t2) (and (pair? t1) (pair? t2)
                       (tree-equal? (car t1) (car t2))
                       (tree-equal? (cdr t1) (cdr t2)))))

;7;

(define (insert x T)
  (cond ((null? T) (make-tree x '() '()))
        ((eq? x (value T)) T)
        ((< x (value T)) (make-tree (value T)
                                    (insert x (left T))
                                    (right T)))
        ((> x (value T)) (make-tree (value T)
                                    (left T)
                                    (insert x (right T))))))

(define (insert-list L T)
  (fold-left insert T L))

(define (sort-extract T)
  (if (null? T) '()
      (append (sort-extract (left T))
              (list (value T))
              (sort-extract (right T)))))

(define (tree-sort l)
  (sort-extract (insert-list l '())))