(define (create-heap value left right)
  (list value left right))
(define (heap-root H) (car H))
(define (left T) (cadr T))
(define (right T) (caddr T))

;1a;

(define (heap-insert f x H)
  (if (null? H) (create-heap x '() '())
      (let ((y (car H)))
        (if (f x y)
            (create-heap x (right H) (heap-insert f y (left H)))
            (create-heap y (right H) (heap-insert f x (left H)))))))

;1b;

(define (combine f Ha Hb)
  (cond
    ((null? Ha) Hb)
    ((null? Hb) Ha)
    ((f (car Ha) (car Hb))
     (create-heap (car Ha) Hb (combine f (left Ha) (right Ha))))
    (else (create-heap (car Hb) Ha (combine f (left Hb) (right Hb))))))

;1c;

(define (empty? H)
  (if (null? H) #t
      #f))

;1d;

(define (heap-remove f H)
  (combine f (left H) (right H)))