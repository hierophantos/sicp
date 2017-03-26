;; From Video, Lecture 2A - Higher Order Procedures

;; helpers (not in video)
(define (inc n) (+ n 1))
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
;; --------------------------------------------------

;; 03:30
(define (sum-int a b)
  (if (> a b)
      0
      (+ a
         (sum-int (inc a) b))))

;; 04:50
(define (sum-sq a b)
  (if (> a b)
      0
      (+ (square a)
         (sum-sq (inc a) b))))

;; 07:07
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

(real->double-flonum  (pi-sum 1 1000))

;; pi-sum = pi/8 as limit of b approaches ∞
(/ pi 8)

;; 10:35
;; sum generalizes the patterns seen above
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; 14:00
(define (sum-int a b)
  (define (identity x) x)
  (sum identity a inc b))

;; 15:45
(define (sum-sq a b)
  (sum square a inc b))

;; 17:00
(define (pi-sum a b)
  (sum (lambda (i) (/ 1 (* i (+ i 2))))
       a
       (lambda (i) (+ i 4))
       b))

(< (abs (- (real->double-flonum (pi-sum 1 1000)) ;; actual
           (/ pi 8)))                            ;; expected
   0.001)                                        ;; tolerance

(= (sum-int 1 10)
   (apply + (range 1 11)))

(= (sum-sq 1 10)
   (apply + (map square (range 1 11))))

;; iterative sum 19:00
(define (sum-iter term a next b)
  (define (iter j ans)
    (if (> j b)
        ans
        (iter (next j)
              (+ (term j) ans))))
  (iter a 0))

(define (pi-sum-iter a b)
  (sum-iter (lambda (i) (/ 1 (* i (+ i 2))))
            a
            (lambda (i) (+ i 4))
            b))

(time (pi-sum 1 1000))
(time (pi-sum-iter 1 1000))

(time (sum square 1 inc 100000))
(time (sum-iter square 1 inc 100000))
(time (apply + (map square (range 100001))))

;; iterative sum checks out to be faster

;; heron's method for square roots 23:47
(define (herons-method x)
  (define tolerance 0.00001)
  (define (good-enuf? y)
    (< (abs (- (* y y) x)) tolerance))
  (define (improve y)
    (average (/ x y) y))
  (define (try y)
    (if (good-enuf? y)
        y
        (try (improve y))))
  (try 1))

(real->double-flonum (herons-method 2))

;; 27:25
(define (herons-method2 x)
  (fixed-point
   (lambda (y) (average (/ x y) y))
   1))

;; 28:50
;; black-board
;; (define (fixed-point f start)
;;   (define (iter old new)
;;     (if (close-enuf? old new)
;;         new
;;         (iter new (f new))))
;;   (iter start (f start)))

;; 31:51 full version from overhead
(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (close-enuf? u v)
    (< (abs (- u v)) tolerance))
  (define (iter old new)
    (if (close-enuf? old new)
        new
        (iter new (f new))))
  (iter start (f start)))

(real->double-flonum (herons-method2 2))

;; 34:45
(define (heron-sqrt x)
  (fixed-point
   (average-damp (lambda (y) (/ x y)))
   1))

;; 36:00
;; average-damp takes a procedure and produces a procedure
(define average-damp
  (lambda (f)
    (lambda (x) (average (f x) x))))

(real->double-flonum (heron-sqrt 2))

;; 46:10
;; Newton's Method
;; to find a y such that f(y)=0
;; start with a guess, y_0
;; y_n+1 =  y_n - f(y_n) / (df/dy |@ y=y_n)

;; 46:30
(define (newton-sqrt x)
  (newton (lambda (y) (- x (square y)))
          1))

;; 48:10
(define (newton f guess)
  (define df (deriv f))
  (fixed-point
   (lambda (x) (- x (/ (f x) (df x))))
   guess))

;; 51:30
(define deriv
  (lambda (f)
    (lambda (x)
      (/ (- (f (+ x dx))
            (f x))
         dx))))

(define dx .000001)

(newton-sqrt 2)

;; 56:40
;; The rights and privileges of first-class citizens
;; • To be named by variables
;; • To be passed as arguments to procedures.
;; • To be returned as values of procedures.
;; • To be incorporated into data structures.



;; http://icampustutor.csail.mit.edu/6.001-public/tutor.cgi
(define second-order
  (lambda (x)
    (- (+ (* 3 (expt x 2))
          (* 14 x)) 5)))

(second-order 3)

(define quadratic-root
  (lambda (a b c)
    (let ((d (- (* b b) (* 4 a c))))
      (/ (+ (- b) (sqrt d)) (* 2 a)))))

(quadratic-root 4 6 1)

(define weird
  (lambda (a b)
    (if (< a b)
        (* a a)
        (sqrt b))))

(weird 6 5)

(define my-abs
  (lambda (a)
    (if (>= a 0)
        a
        (- a))))

(define bigger2
  (lambda (a b)
    (if (> a b)
        a
        b)))

(bigger2 4 3)

(define bigger3
  (lambda (a b c)
    (bigger2 (bigger2 a b) (bigger2 b c))))

(bigger3 6 15 9)
