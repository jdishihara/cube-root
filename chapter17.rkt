;17.2
(define (f1 s1 s2)
  (list (append (cdr s1) (list (car s2)))))

(define (f2 l1 l2)
  (cons (cdr l1) (list (cadr l2))))

(define (f3 l1 l2)
  (append l1 l1))

(define (f4 l1 l2)
  (list (append (list (car l1)) (list (car l2))) (append (cdr l1) (cdr l2))))

;17.3
 (map (lambda (x) (lambda (y) (+ x y))) '(1 2 3 4))
 ((car (map (lambda (x) (lambda (y) (+ x y))) '(1 2 3 4))) 100 )
((cadr (map (lambda (x) (lambda (y) (+ x y))) '(1 2 3 4))) 100 )
((cadddr (map (lambda (x) (lambda (y) (+ x y))) '(1 2 3 4))) 100 )

;17.4
(define (mystery lst)
  (mystery-helper lst '()))

(define (mystery-helper lst other)
  (if (null? lst)
      other
      (mystery-helper (cdr lst) (cons (car lst) other)))) ;reverses list

;17.8
(define (new-member x ls)
  (cond ((null? ls)
        #f)
        ((equal? x (car ls))
                 ls)
        (else (new-member x (cdr ls)))))

;17.9
(define (my-list-ref x ls)
  (cond ((null? ls)
        '())
        ((equal? x 0)
         (car ls))
        (else (my-list-ref (- x 1) (cdr ls)))))

(my-list-ref 3 '(a b c d e f))

;17.10
(define (my-length ls)
  (if (null? ls) 0
        (+ 1 (my-length (cdr ls)))))

;17.11
(define (before-in-list? ls x y)
  (cond ((null? ls) #f)
        ((not (member x ls)) #f)
        ((not (member y ls)) #f)
        ((equal? (car ls) x) #t)
        ((equal? (car ls) y) #f)
        (else (before-in-list? (cdr ls) x y))))

;17.12

      
;17.14
(define (branch x ls)
  (cond ((null? ls) '())
        ((null? x) '())
        ((null? (cdr x)) (item (item 1 x) ls))
        (else (list? x) (item (item 2 x) (item (item 1 x) ls)))))
        


;different verison
(define (my-branch x y ls)
  (cond ((null? y) (item x ls))
        (else (item y (item x ls)))))
               
(my-branch 3 '() '((a b) (c d) (e f) (g h)))
         
(my-branch 3 2 '((a b) (c d) (e f) (g h)))

   
  

