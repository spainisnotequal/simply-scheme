;; ========================== ;;
;; Chapter 11 - Simply Scheme ;;
;; ========================== ;;


"----- Exercise 11.1 -----"

(define (downup4 wd)
  (se wd
      (bl wd)
      (bl (bl wd))
      (first wd)
      (bl (bl wd))
      (bl wd)
      wd))

(equal? '(love lov lo l lo lov love) (downup4 'love))


"----- Exercise 11.2 -----"

(define (count-ums sent)
  (if (empty? sent)
      0
      (if (equal? 'um (first sent))
          (+ 1 (count-ums (bf sent)))
          (count-ums (bf sent)))))

(= 3 (count-ums '(today um we are going to um talk about functional um programming)))


"----- Exercise 11.3 -----"

(define (letter2number letter)
  (cond ((member? letter 'abc) 2)
        ((member? letter 'def) 3)
        ((member? letter 'ghi) 4)
        ((member? letter 'jkl) 5)
        ((member? letter 'mno) 6)
        ((member? letter 'prs) 7)
        ((member? letter 'tuv) 8)
        ((member? letter 'wxy) 9)
        (else 0)))

(define (phone-unspell wd)
  (if (= (count wd) 1)
      (letter2number wd)
      (word (letter2number (first wd))
            (phone-unspell (bf wd)))))

(= 7672676 (phone-unspell 'popcorn))
(= 280007800 (phone-unspell "buzz puzz"))

"----- Exercise 11.4 -----"


"----- Exercise 11.5 -----"

(define (initials sent)
  (if (empty? sent)
      '()
      (se (first (first sent))
          (initials (bf sent)))))

(equal? '(i i n s) (initials '(if i needed someone)))


"----- Exercise 11.6 -----"

(define (countdown n)
  (if (= n 0)
      'Blastoff!
      (se n (countdown (- n 1)))))

(equal? '(10 9 8 7 6 5 4 3 2 1 Blastoff!) (countdown 10))
(equal? '(3 2 1 Blastoff!) (countdown 3))

"----- Exercise 11.7 -----"

(define (copies n wd)
  (if (= n 1)
      wd
      (se wd (copies (- n 1) wd))))

(equal? '(spam spam spam spam spam spam spam spam) (copies 8 'spam))