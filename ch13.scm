;; ========================== ;;
;; Chapter 13 - Simply Scheme ;;
;; ========================== ;;


"----- Exercise 13.1 -----"

(define (explode wd)
  (if (empty? wd)
      '()
      (se (first wd) (explode (bf wd)))))


"----- Exercise 13.2 -----"

(define (pigl wd)
  (if (member? (first wd) 'aeiou)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))


"----- Exercise 13.3 -----"

(define (downup wd)
  (se wd (downup (bl wd)) wd))


"----- Exercise 13.4 -----"

(define (forever n)
  (if (= n 0)
      1
      (+ 1 (forever n))))


"----- Exercise 13.6 -----"

(define (factorial n)
  (if (<= n 2)
      1
      (+ (factorial (- n 1))
         (factorial (- n 2)))))




