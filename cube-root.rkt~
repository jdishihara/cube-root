(define (NewtonMethod x y)
  (/(+(/ x (* y y))(* 2 y))3)
  )

(NewtonMethod 3 5)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
(define (close-enough? v1 v2)
(< (abs (- v1 v2))
tolerance))
(define (try guess)
(let ((next (f guess)))
(if (close-enough? guess next)
next
(try next))))
(try first-guess))

(define (square x) (* x x))
(define (average x y)
  (/(+ x y)2))
(define (average-damp f)

 (lambda (x) (average x (f x))))

 (define (cube-root x)
    (fixed-point (average-damp (lambda (y) (/ x (square y))))
                 1.0))

(define (dec x)
  (- x 1))
(define (inc x)
  (+ x 1))

(define (add a b)
   (if (= a 0) b (inc (add (dec a) b))))
(define (add a b)
  (if (= a 0) b (add (dec a) (inc b))))