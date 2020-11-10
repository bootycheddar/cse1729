(define (max-fg f g)
  (define (max-fg-helper x)
    (max (f x) (g x)))
  max-fg-helper)

(define (f x) (* x x))

(define (g x) (* x x))

(max-fg (f 2) (g 2))
