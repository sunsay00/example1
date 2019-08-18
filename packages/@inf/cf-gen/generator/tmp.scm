(define (map-double-up fn l)
  (let ((ret (foldr (lambda (i s)
                      (if (null? (car s)) (cons i (cdr s))
                          (cons '() (cons (fn (list i (car s))) (cdr s)))))
                    (cons '() '()) l)))
    (if (null? (car ret)) (cdr ret)
        (cons (fn (list (car ret))) (cdr ret)))))

(define (string-append-ex strs)
  (cond
    ((null? strs) "")
    ((null? (cdr strs)) (car strs))
    (else (string-append-ex (map-double-up (lambda (d) (apply string-append d)) strs)))))

(print (string-append-ex (map ->string (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17))))
;(define x (map-double-up (lambda (d) (apply string-append d)) (map ->string (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17))))
;(define y (map-double-up (lambda (d) (apply string-append d)) x))
;(define z (map-double-up (lambda (d) (apply string-append d)) y))
;(define w (map-double-up (lambda (d) (apply string-append d)) z))
;(define u (map-double-up (lambda (d) (apply string-append d)) w))
;(print u)


