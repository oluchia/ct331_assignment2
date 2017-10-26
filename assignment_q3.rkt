#lang racket

(provide create)
(provide order)
(provide right_tree)
(provide value)
(provide left_tree)
(provide check)
(provide is_empty)
(provide insert)
(provide insert_list)
(provide sort)

;unfortunately i was unable to complete this assignment
;given time contraints but i feel
;i have a good understanding of the concepts taught

;two test trees
(define bst0 '(() 1 ()))
(define bst1 '((() 1 ()) 2 (() 3 ())))

;helper function
(define (create left-sub val right-sub)
  (list left-sub val right-sub))

;helper function
(define (right_tree bst) (caddr bst))

;helper function
(define (value bst) (cadr bst))

;helper function
(define (left_tree bst) (car bst))

;helper function
(define (is_empty bst)
  (null? bst))

;A
(define (order bst)
  (cond ((is_empty bst))
        (else
         (order (left_tree bst))
         (display (value bst)) (newline)
         (order (right_tree bst)))))
;B
(define (check item bst)
  (cond ((is_empty bst) #f)
        ((equal? item (value bst)) #t)
        ((< item (value bst))
         (check item (left_tree bst)))
        ((> item (value bst))
         (check item (right_tree bst)))
        (else bst)))

;C
(define (insert item bst)
  (cond ((is_empty bst)
         (create '() item '()))
        ((< item (value bst)) ;if item less than root, add to left sub_tree
         (create (value bst)
                 (insert item (left_tree bst))
                 (right_tree bst)))
        ((> item (value bst)) ;if item greater than root, add to right sub_tree
         (create (value bst)
                 (left_tree bst)
                 (insert item (right_tree bst))))
        (else bst)))

;D not working properly
(define (insert_list lst bst)
  (cond ((is_empty bst)
         (list '() lst '()))
        ((< lst (value bst))
         (create (value bst)
                 (insert lst (left_tree bst))
                 (right_tree bst)))
        ((> lst (value bst))
         (create (value bst)
                 (left_tree bst)
                 (insert lst (right_tree bst))))
        (else bst)))
