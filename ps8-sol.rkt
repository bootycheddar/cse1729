;1;

(define (make-tree value left right) (list value left right))
(define (value tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))

(define (arithvalue T)
  (cond
    ((eq? (value T) #\+) (+ (arithvalue (left T)) (arithvalue (right T))))
    ((eq? (value T) #\*) (* (arithvalue (left T)) (arithvalue (right T))))
    ((eq? (value T) #\-) (- (arithvalue (left T))))
    ((eq? (value T) #\/) (/ 1 (arithvalue (left T))))
    (else (value T))))

(define example (list #\+ (list #\*
                                (list 4 '() '())
                                (list 5 '() '()))
                      (list #\+
                            (list #\/ (list 6 '() '()) '())
                            (list 7 '() '()))))

(arithvalue example)

;2;

(define (tree-max T)
  (if (null? (right T)) (value T)
      (max (value T) (tree-max (right T)))))

(define (delete-value v T)
  (cond
    ((null? T) '())
    ((< v (value T))
         (make-tree (value T)
                    (delete-value v (left T))
                    (right T)))
    ((> v (value T))
         (make-tree (value T)
                    (left T)
                    (delete-value v (right T))))
    ((and (null? (right T)) (and (null? (left T)))) '())
    ((and (null? (right T)) (not (null? (left T)))) (left T))
    ((and (null? (left T)) (not (null? (right T)))) (right T))
    (else (make-tree (tree-max (left T))
                     (delete-value (tree-max (left T))
                                   (left T))
                     (right T)))))

;3;

(define (hsort elements)
  (define (create-heap value left right)
  (list value left right))
  (define (heap-root heap) (car heap))
  (define (left T) (cadr T))
  (define (right T) (caddr T))

  (define (combine f Ha Hb)
  (cond
    ((null? Ha) Hb)
    ((null? Hb) Ha)
    ((f (car Ha) (car Hb))
     (create-heap (car Ha) Hb (combine f (left Ha) (right Ha))))
    (else (create-heap (car Hb) Ha (combine f (left Hb) (right Hb))))))

  (define (heap-insert f x H)
  (if (null? H) (create-heap x '() '())
      (let ((y (car H)))
        (if (f x y)
            (create-heap x (right H) (heap-insert f y (left H)))
            (create-heap y (right H) (heap-insert f x (left H)))))))

  (define (heap-removeRoot f H)
    (combine f (left H) (right H)))

  (define (empty? H)
    (if (null? H) #t
        #f))

  (define (heap-insertList elements heap)
    (if (null? elements) heap
        (heap-insertList (cdr elements)
                         (heap-insert < (car elements) heap))))

  (define (heap-extractSorted heap)
    (if (empty? heap) '()
        (cons (value heap)
              (heap-extractSorted (heap-removeRoot < heap)))))

  (heap-extractSorted (heap-insertList elements '())))


;4a;

(define (create-heap value left right)
  (list value left right))
  (define (heap-root heap) (car heap))
  (define (left T) (cadr T))
  (define (right T) (caddr T))

(define (combine f Ha Hb)
  (cond
    ((null? Ha) Hb)
    ((null? Hb) Ha)
    ((f (car Ha) (car Hb))
     (create-heap (car Ha) Hb (combine f (left Ha) (right Ha))))
    (else (create-heap (car Hb) Ha (combine f (left Hb) (right Hb))))))

(define (heap-remove f H)
    (combine f (left H) (right H)))

(define (heap-insert f x H)
  (if (null? H) (create-heap x '() '())
      (let ((y (car H)))
        (if (f x y)
            (create-heap x (right H) (heap-insert f y (left H)))
            (create-heap y (right H) (heap-insert f x (left H)))))))


(define (equalize-heaps heap-pair)
  (let ((size-of-heap1 (caar heap-pair))
        (size-of-heap2 (cadr heap-pair))
        (h1 (cdar heap-pair))
        (h2 (cddr heap-pair)))
    (if (>= 1 (abs (- size-of-heap1 size-of-heap2))) heap-pair
        (if (< size-of-heap1 size-of-heap2)
            (equalize-heaps (cons (cons (+ 1 size-of-heap1) (heap-insert > (car h2) h1)) (cons (- size-of-heap2 1) (heap-remove < h2))))
        (equalize-heaps (cons (cons (- size-of-heap1 1) (heap-remove > h1)) (cons (+ size-of-heap2 1) (heap-insert < (car h1) h2))))))))

;4b;

(define (add-number x heap-pair)
  (let ((size-of-heap1 (caar heap-pair))
        (size-of-heap2 (cadr heap-pair))
        (h1 (cdar heap-pair))
        (h2 (cddr heap-pair)))
    (if (< x (car h1)) (equalize-heaps (cons (cons (+ 1 size-of-heap1) (heap-insert > x h1)) (cons size-of-heap2 h2))) 
        (equalize-heaps (cons (cons size-of-heap1 h1) (cons (+ 1 size-of-heap2) (heap-insert < x h2)))))))
        
;4c;

(define (get-median heap-pair)
  (let ((size-of-heap1 (caar heap-pair))
        (size-of-heap2 (cadr heap-pair))
        (h1 (cdar heap-pair))
        (h2 (cddr heap-pair)))
    (cond
      ((= size-of-heap1 size-of-heap2) (/ (+ (car h1) (car h2)) 2))
      ((> size-of-heap1 size-of-heap2) (car h1))
      (else (car h2)))))
        
  




































  
  