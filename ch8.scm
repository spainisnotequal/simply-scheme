;; ========================= ;;
;; Chapter 8 - Simply Scheme ;;
;; ========================= ;;

(define (first-letters sent)
  (every first sent))

(every first '(2 más 2 son 4))
(every first '(más))

(keep number? '(2 más 2 son 4))
(keep number? '2más2son4)

(accumulate + '(2 2 4 -6))
(accumulate min '(2 2 4 -6))

(accumulate word '(a l t a v o z))

(accumulate + 2241)
(accumulate se 'altavoz)



(define (vowel? letter) (member? letter '(a e i o u)))
(define (square x) (* x x))  


;(define (any-numbers? sent)
;  (accumulate or (every number? sent)))

(keep number? '(a b 2 c 6))
(empty? (keep number? '(a b 2 c 6)))
(not (empty? (keep number? '(a b 2 c 6))))

;; any-numbers?: sentence -> boolean
(define (any-numbers? sent)
  (not (empty? (keep number? sent))))


"----- Exercise 8.1 -----"


(every last '(algebra purple spaghetti tomato gnu))

(keep number? '(one two three four))

(accumulate * '(6 7 13 0 9 42 17))

(member? 'h (keep vowel? '(t h r o a t)))

(every square (keep even? '(87 4 7 12 0 5)))

(accumulate word (keep vowel? (every first '(and i love her))))

((repeated square 0) 25)

(every (repeated bl 2) '(good day sunshine))


"----- Exercise 8.2 -----"


(keep vowel? 'birthday)
(every first '(golden slumbers))
(first '(golden slumbers))
(every last '(little child))
(accumulate word (every last '(little child)))
(every + '(2 3 4 5))
(accumulate + '(2 3 4 5))


"----- Exercise 8.4 -----"


(define BEATTLES '(John Paul George Ringo))

(define (choose-beatles predicate)
  (keep predicate BEATTLES))

(define (ends-vowel? wd) (vowel? (last wd)))

(define (even-count? wd) (even? (count wd)))

(equal? '(George Ringo) (choose-beatles ends-vowel?))

(equal? '(John Paul George) (choose-beatles even-count?))


"----- Exercise 8.5 -----"


(define (transform-beatles procedure)
  (every procedure BEATTLES))

(define (amazify name)
  (word 'the-amazing- name))

(equal? '(the-amazing-John the-amazing-Paul the-amazing-George the-amazing-Ringo)
        (transform-beatles amazify))

(equal? '(ohn aul eorge ingo) (transform-beatles butfirst))


"----- Exercise 8.6 -----"


(define (code-name letter)
  (cond ((equal? letter 'a) 'Alpha)
        ((equal? letter 'b) 'Bravo)
        ((equal? letter 'c) 'Charlie)
        ((equal? letter 'd) 'Delta)
        ((equal? letter 'e) 'Echo)
        ((equal? letter 'f) 'Foxtrot)))

(define (words wd)
  (every code-name wd))

(equal? '(Alpha Charlie Echo) (words 'ace))


"----- Exercise 8.7 -----"


(define (always-one arg)
  1)

;; count: word/sentence -> number
(define (count sent)
  (accumulate + (every always-one sent)))

;; count: word/sentence -> number
(define (letter-count sent)
  (accumulate + (every count sent)))

(= 11 (letter-count '(fixing a hole)))
(letter-count 'hola)
(count 'hola)


"----- Exercise 8.8 -----"

(define (substitute wd)
  (cond ((number? wd) (* 2 wd))
        ((equal? wd 'good) 'great)
        ((equal? wd 'bad) 'terrible)
        (else wd)))

(define (exaggerate sent)
  (every substitute sent))

(equal? '(I ate 6 terrible apples) (exaggerate '(I ate 3 bad apples)))
(equal? '(I had a great time) (exaggerate '(I had a good time)))


"----- Exercise 8.9 -----"

(equal? '(hi there) (every word '(hi there)))
(equal? '(hi there) (keep word '(hi there)))
(equal? '(hi there) (accumulate se '(hi there)))


"----- Exercise 8.10 -----"

(define (true-for-all? predicate sent)
  (equal? (keep predicate sent) sent))

(true-for-all? even? '(2 4 6 8))
(not (true-for-all? even? '(1 4 6 8)))


"----- Exercise 8.11 -----"

(define (base-grade wd)
  (cond ((equal? wd 'A) 4)
        ((equal? wd 'B) 3)
        ((equal? wd 'C) 2)
        ((equal? wd 'D) 1)
        ((equal? wd 'E) 0)))

;(= 4 (base-grade 'A))
;(= 1 (base-grade 'D))

(define (grade-modifier wd)
  (cond ((equal? wd '-) (- 0.33))
        ((equal? wd '+) 0.33)
        (else 0)))

;(= -0.33 (grade-modifier '-))
;(= 0.33 (grade-modifier '+))
;(= 0 (grade-modifier ""))

(define (grade wd)
  (+ (base-grade (first wd))
     (grade-modifier (bf wd))))

;(= (- 3 0.33) (grade 'B-))
;(= 3 (grade 'B))
;(= (+ 3 0.33) (grade 'B+))


(define (GPA sent)
  (/ (accumulate + (every grade sent))
     (count sent)))

(< (abs (- 3.67 (GPA '(A A+ B+ B)))) 0.01)


"----- Exercise 8.12 -----"

(define (um? wd)
  (equal? wd 'um))

(define (count-ums sent)
  (count (keep um? sent)))

(= 3 (count-ums '(today um we are going to um talk about functional um programming)))


"----- Exercise 8.13 -----"

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
  (accumulate word (every letter2number wd)))

(= 7672676 (phone-unspell 'popcorn))
(= 280007800 (phone-unspell "buzz puzz"))


"----- Exercise 8.14 -----"

(define (subword wd start end)
  ((repeated butlast (- (count wd) end))
   ((repeated butfirst (- start 1)) wd)))

(equal? 'then (subword 'polythene 5 8))
(equal? 'ythe (subword 'polythene 4 7))
