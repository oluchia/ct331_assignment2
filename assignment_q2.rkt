#lang racket

;Please note the provide function is necessary
;for the unit tests to work. Please include a
;(provide) for each function you write in your
;submitted assignment.

(provide ins_beg)
(provide ins_end)
(provide count_top)
(provide count_instances)
(provide count_instances_tr)
(provide count_instances_deep)

;A
(define (ins_beg el lst)
  (append (list el) lst))

;B
(define (ins_end el lst)
  (append lst (list el)))

;C
(define (count_top lst)
  (if (null? lst)
      0
      (+ 1 (count_top (cdr lst)))))

;D
(define (count_instances el lst)
  (cond ((null? lst) 0)
        ((equal? el (car lst))
         (+ 1 (count_instances el (cdr lst))))
        (else (count_instances el (cdr lst)))
  ))

;E
;helper function for tail recursion
(define (tail el lst)
  (count_instances_tr el lst 0)) ;assign acc to 0

(define (count_instances_tr el lst acc)
  (cond ((null? lst) acc)
      ((equal? el (car lst))
      (count_instances_tr el (cdr lst) (+ 1 acc)))
      (else (count_instances_tr el (cdr lst) acc))
 ))

;F
(define (count_instances_deep el lst)
  (cond ((null? lst) 0)
        ((equal? el (car lst))
         (+ 1 (count_instances_deep el (cdr lst))))
        ((list? (car lst))
         (+ 0 (count_instances_deep el (cdr lst))
             (count_instances_deep el (car lst))))
        (else (count_instances_deep el (cdr lst)))
 ))
  
  