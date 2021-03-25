;Exercise 1.3.  Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

(define (square x) (* x x))
(define (sum-squares x y) (+ (square x) (square y)))
(define (excer13 x y z)
  (sum-squares (max x y) (max x z)))

;Exercise 1.6
(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
          (else else-clause)))

;I assume this forces the else-clause to be evaluated on the call-site causing infinite recursion.

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(define (sqrt-badif x)
  (sqrt-iter 1.0 x))

;Excercise 1.7

(define (good-enough2? guess delta)
  (< (/ delta guess) 0.00000001))

;Let syntax version
(define (sqrt-iter2 guess x)
  (let ((improved   (improve guess x)))
  (if (good-enough2? guess (abs (- guess improved)))
          guess
          (sqrt-iter2 improved x))))

(define (sqrt2 x)
  (sqrt-iter2 1.0 x))

;The truth about let syntax
(define (sqrt-iter2_unsugared guess x)
  ((lambda (improved)
    (if (good-enough2? guess (abs (- guess improved)))
      guess
      (sqrt-iter2_unsugared improved x)))
   (improve guess x)))

(define (sqrt2_unsugared x)
  (sqrt-iter2_unsugared 1.0 x))

;Exercise 1.8

(define (improve_cubed guess x)
    (/ (+ (/ x (square guess)) (* 2 guess))
       3))
(define (absdelta guess improved)
  (abs (- guess improved)))

(define (cubert-iter guess x)
  (let ((improved (improve_cubed guess x)))
  (if (good-enough2? guess (absdelta guess improved))
    guess
    (cubert-iter improved x))))

(define (cubert x)
  (cubert-iter 1.0 x))

;Generalization of rooting
;Section 1.3.4 apparently will deal with this.

(define (genroot-iter impf guess x)
  (let ((improved (impf guess x)))
  (if (good-enough2? guess (absdelta guess improved))
    guess
    (genroot-iter impf improved x))))

(define improve2 improve)
(define improve3 improve_cubed)

(define (rootn x n)
  (let ((improvefn (cond ((= n 2) improve2)
                         ((= n 3) improve3)
                         (else (lambda (x y) (raise 'unimpl)))))) ;not sure the idiomatic way to do this
  (genroot-iter improvefn 1.0 x)))


;Generate the respective root function given an improve fn and delta predicate
(define (genroot-iter2 impf deltapred)
  (letrec ((iter-inner
  (lambda (guess x)
    (let ((improved (impf guess x)))
    (if (good-enough2? guess (deltapred guess improved))
      guess
      (iter-inner improved x))))))
  iter-inner))

;Usage ((genroot-iter2 improve3 absdelta) 1.0 27)

(define (rootn2 x n)
  (let ((improvefn (cond ((= n 2) improve2)
                         ((= n 3) improve3)
                         (else (lambda (x y) (raise 'unimpl))))))
  ((genroot-iter2 improvefn absdelta) 1.0 x)))
