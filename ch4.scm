;; ========================= ;;
;; Chapter 4 - Simply Scheme ;;
;; ========================= ;;

;; Exercise 4.5
;; ------------

(define (fah2cel f)
  (* 5/9 (- f 32)))

(define (cel2fah c)
  (+ (* 9/5 c) 32))


(= 100 (cel2fah (fah2cel 100)))


;; Exercise 4.6
;; ------------

(define (square x)
  (* x x))

(define (forth1 x)
  (* (* x x) (* x x)))

(define (forth2 x)
  (square (square x)))

(forth1 2)
(forth2 2)
(= (forth1 2) (forth2 2))


;; Exercise 4.7
;; ------------

(define (abs x)
  (sqrt (square x)))

(= 1 (abs -1))


;; Exercise 4.8
;; ------------

(define (scientific a b)
  (* a (expt 10 b)))

(= 7000  (scientific 7 3))
(= 21/50000  (scientific 42 -5))


;; Exercise 4.9
;; ------------

(define (discount x y)
  (* x (- 1 (/ y 100))))

(= 9.5 (discount 10 5))
(= 14.95 (discount 29.90 50))

;; Exercise 4.10
;; -------------

(define (tip x)
  (+ (* x 0.15)
     (- (ceiling (* x 1.15))
        (* x 1.15))))
  
(< (abs (- 3.02 (tip 19.98))) 0.01)
(< (abs (- 4.77 (tip 29.23))) 0.01)
(< (abs (- 1.46 (tip 7.54))) 0.01)



















