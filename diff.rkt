#lang scheme

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get (operator exp) 'deriv) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))



(define (install-sum)
 
  (define (sum-deriv x y) (make-sum (deriv (addend x) y) (deriv (augend x) y)))
 (define (addend s) (car s))
 (define (augend s) (cadr s))
  (put '+ 'deriv sum-deriv)

  (define (product-deriv x y) (make-sum (make-product
                                         (multiplier x)
                                         (deriv (multiplicand x) y))
                                        (make-product
                                         (deriv (multiplier x ) y)
                                         (multiplicand x))))
(define (multiplier p) (car p))
(define (multiplicand p) (cadr p))
  (put '* 'deriv product-deriv)
  'done)
(install-sum)

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-expt base tavan)
    (cond ((=number? tavan 0) 1)
          ((=number? tavan 1) base)
          (else (list 'expt base tavan))))

(define (install-exp)
  
  
  (define (base a) (car a))
  (define (tavan a) (cadr a))
  (define (expt-deriv number var)
    (make-product (tavan number) (make-product (make-expt (base number) (- (tavan number) 1)) (deriv (base number) var))))
  (put 'expt 'deriv expt-deriv)
  'done)
(install-exp)

(define (install-chain)
  (define (make-chain m1 m2)
    (list 'o m1 m2))
  (define (f a) (car a))
  (define (g a) (cadr a))
  (define (change-var f var g)
  
  (define (helper f var g)
    (if (null? f) '()
        (if (eq? (car f) var) (cons g (change-var (cdr f) var g))
            (cons (car f) (change-var (cdr f) var g)))))
  (if (eq? (car g) 'o) (helper f var (helper (cadr g) var (caddr g))) (helper f var g)))
  (define (chain-deriv number var)
    (make-product (change-var (deriv (f number) var) var (g number)) (deriv (g number) var)))
  (put 'o 'deriv chain-deriv)
  'chained-up)
(install-chain)

(define (install-div)
  (define (make-div m1 m2) (if (=number? m2 0) (error "denumerator zero! abort") (list '/ m1 m2)))
  (define (num x) (car x))
  (define (denum x) (cadr x))
  (define (div-deriv number var)
    (make-div (make-sum (make-product (denum number) (deriv (num number) var)) (make-product -1 (make-product (num number) (deriv (denum number) var))))
              (make-expt (denum number) 2)))
  (put '/ 'deriv div-deriv)
  'all-set-up-and-ready-to-go)
(install-div)

(define (install-trig)
  (define (make-sin x) (list 'sin (car x)))
  (define (make-cos x) (list 'cos (car x)))
  (define (sin-deriv number var) (make-cos number))
  (define (cos-deriv number var) (make-product -1 (make-sin number)))
  (put 'sin 'deriv sin-deriv)
  (put 'cos 'deriv cos-deriv)
  'trigafied)
(install-trig)
  


(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (variable? x) (symbol? x))

 
