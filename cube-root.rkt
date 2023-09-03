(#%require racket/trace)

(define (NewtonMethod x y)
  (/(+(/ x (* y y))(* 2 y))3))

(NewtonMethod 27 (/ 43 12))
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

; (define (cube-root x)
    ;(fixed-point (average-damp (lambda (y) (/ x (square y))))
                ; 1.0))

(define (dec x)
  (- x 1))
(define (inc x)
  (+ x 1))

(define (add1 a b)
   (if (= a 0) b (inc (add1 (dec a) b))))
(define (add2 a b)
  (if (= a 0) b (add2 (dec a) (inc b))))

(define (improve-guess x y)
  ((/(+(/ x (* y y))(* 2 y))3)))






(define (cube-root-iter guess x)
  (if (good-enough-cube-root? guess x)
          guess
          (cube-root-iter (improve-guess x guess) x)))

(define (good-enough-cube-root? guess x)
  (< (abs (- (* guess guess guess) x)) .00000001))


(define (cube-root x)
  (cube-root-iter 1.0 x))

(define (improve-guess x y)
  (/ (+ (/ x (* y y))(* 2 y))3))

(define (expt-iter b n result)
  (if (= n 0) result
      (expt-iter b (- n 1) (* result b))))
(define (my-expt b n)
  (expt-iter b n 1))



(define (square x) (* x x))

(define (fast-expt b n)
  (define (iter a b n)
  (cond ((= n 0) a)
        ((even? n) (iter a (square b) (/ n 2)))
        (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

