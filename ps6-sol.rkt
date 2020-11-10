;1;

(define (change k l)
  (cond
    ((< k 0) 0)
    ((= k 0) 1)
    ((null? l) 0)
    (else (+ (change k (cdr l))
             (change (- k (car l)) l)))))

;2;

(define (make-change n den)
  (define (helper n den cur)
    (cond ((< n 0) '())
          ((= n 0) (list cur))
          ((null? den) '())
          (else (append (helper n (cdr den) cur) (helper (- n (car den)) den (cons (car den) cur))))))
     (helper n den '()))

;3;

(define (rle coins)
  (define (helper coins count prev)
    (cond
      ((null? coins) (list (cons count prev)))
      ((equal? prev (car coins)) (helper (cdr coins) (+ 1 count) prev))
      (else (cons (cons count prev) (helper (cdr coins) 1 (car coins))))))
  (helper coins 0 (car coins)))

;4;

(define (rle-all lcoins)
  (cond
    ((null? lcoins) '())
    (else (cons (rle (car lcoins)) (rle-all (cdr lcoins))))))

;5a;

(define (list-sum x)
  (if (null? x) 0
      (+ (car x) (list-sum (cdr x)))))

;5b;

(define (average X)
  (/ (list-sum X) (length X)))

;5c;

(define (var-map X)
  (let ((avg (average X)))
    (map (lambda (x) (* (- x avg) (- x avg))) X)))

;5d;

(define (stdev X)
  (sqrt (average (var-map X))))

;5e;

(define (map2 f X Y)
  (if (null? X) '()
      (cons (f (car X) (car Y))
            (map2 f (cdr X) (cdr Y)))))

;5f;

(define (covar-elements X Y)
  (let ((avgX (average X))
        (avgY (average Y)))
    (map2 (lambda (x y) (* (- x avgX) (- y avgY)))
          X Y)))

;5g;

(define (pearson X Y)
  (/ (average (covar-elements X Y))
     (* (stdev X) (stdev Y))))

;6a;

(define (best-fit X Y)
  (let* ((a (* (pearson X Y) (stdev Y) (/ 1 (stdev X))))
         (b (- (average Y) (* a (average X)))))
    (cons a b)))

;6b;

(define (best-fit-fn X Y)
  (let* ((a (* (pearson x y) (stdev y) (/ 1 (stdev x))))
         (b (- (average y) (* a (average x)))))
    (lambda (x) (+ (* a x) b))))

;7;

(define (merge L1 L2)
  (cond ((null? L1) L2)
        ((null? L2) L1)
        ((< (car L1) (car L2)) (cons (car L1) (merge (cdr L1) L2)))
        (else (cons (car L2) (merge L1 (cdr L2))))))

;8;

(define (mergeSort l)
  (define (helper l)
    (if (null? l) (list '() '())
    (if (null? (cdr l)) (list l '())
        (let ((rest (helper (cdr (cdr l)))))
              (list (cons (car l) (car rest))
                    (cons (car (cdr l)) (car (cdr rest))))))))
  (if (null? l) '()
  (if (null? (cdr l)) l
      (let ((s (helper l)))
        (let ((a (car s)) (b (car (cdr s))))
          (merge (mergeSort a) (mergeSort b)))))))
(mergeSort (list 0 9 1 4 2 10 13 3 7 14 -1))
(mergeSort (list))
(mergeSort (list 1))
    