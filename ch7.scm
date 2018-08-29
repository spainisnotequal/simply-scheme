;; ========================= ;;
;; Chapter 7 - Simply Scheme ;;
;; ========================= ;;


;; Exercise 7.1
;; ------------

;; vowel?: word -> boolean
(define (vowel? wd)
  (member? wd '(a e i o u)))

;; gratitude: word -> sentence
(define (gratitude wd)
  (let ((subject (se (if (vowel? (first wd)) 'an 'a)
                     wd)))
    (se subject 'is subject 'is subject)))

(equal? '(a rose is a rose is a rose) (gratitude 'rose))
(equal? '(an iguana is an iguana is an iguana) (gratitude 'iguana))

;; Exercise 7.2
;; ------------

(let ((pi 3.14159)
      (pie '(lemon meringue)))
  (se '(pi is) pi '(but pie is) pie))