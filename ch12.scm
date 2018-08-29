;; ========================== ;;
;; Chapter 12 - Simply Scheme ;;
;; ========================== ;;


"----- Exercise 12.1 -----"

(define (addup nums)
  (if (empty? nums)
      0
      (+ (first nums) (addup (bf nums)))))

(= 10 (addup '(1 3 5 1)))


"----- Exercise 12.2 -----"

(define (acronym sent)
  (if (empty? sent)
      ""
      (word (first (first sent))
            (acronym (bf sent)))))

(equal? 'NGO (acronym '(Non Goverment Organization)))


"----- Exercise 12.4 -----"

(define (f sent)
  (if (empty? sent)
      sent
      (se (f (bf sent))
          (first sent))))

(equal? '(doing you are how) (f '(how are you doing)))


"----- Exercise 12.5 -----"

(define (replace wd)
  (cond ((number? wd) (* 2 wd))
        ((equal? wd 'good) 'great)
        ((equal? wd 'bad) 'terrible)
        (else wd)))

(define (exaggerate sent)
  (if (empty? sent)
      '()
      (se (replace (first sent))
          (exaggerate (bf sent)))))

(equal? '(I ate 6 terrible apples) (exaggerate '(I ate 3 bad apples)))
(equal? '(I had a great time) (exaggerate '(I had a good time)))


"----- Exercise 12.6 -----"


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


(define (points sent)
  (if (empty? sent)
      0
      (+ (grade (first sent)) (points (bf sent)))))

;(= 14.66 (points '(A A+ B+ B)))


(define (GPA sent)
  (/ (points sent) (count sent)))

(< (abs (- 3.67 (GPA '(A A+ B+ B)))) 0.01)


"----- Exercise 12.7 -----"

(define (spell-digit digit)
  (item (+ 1 digit)
        '(zero one two three four five six seven eight nine)))

(define (spell-number number)
  (if (empty? number)
      '()
      (se (spell-digit (first number))
          (spell-number (bf number)))))

(equal? '(one nine seven one) (spell-number 1971))


"----- Exercise 12.8 -----"

(define (numbers sent)
  (if (empty? sent)
      '()
      (if (number? (first sent))
          (se (first sent) (numbers (bf sent)))
          (se (numbers (bf sent))))))

(equal? '(76 110) (numbers '(76 trombones and 110 cornets)))


"----- Exercise 12.9 -----"

(define (real-word? wd)
  (not (member? wd '(a the an in of and for to with))))

(define (real-words sent)
  (if (empty? sent)
      '()
      (if (real-word? (first sent))
          (se (first sent) (real-words (bf sent)))
          (se (real-words (bf sent))))))

(equal? '(there is camel yard) (real-words '(there is a camel in the yard)))


"----- Exercise 12.10 -----"

(define (remove wd sent)
  (if (empty? sent)
      '()
      (if (equal? wd (first sent))
          (remove wd (bf sent))
          (se (first sent) (remove wd (bf sent))))))

(equal? '(song love of loved by beatles)
        (remove 'the '(the song love of the loved by the beatles)))



"----- Exercise 12.11 -----"

(define (count sent)
  (if (empty? sent)
      0
      (+ 1 (count (bf sent)))))

(= 5 (count 'hello))
(= 5 (count '(hello my dear friend Brian)))


"----- Exercise 12.12 -----"

(define (roman-value letter)
  (cond ((equal? letter 'I) 1)
        ((equal? letter 'V) 5)
        ((equal? letter 'X) 10)
        ((equal? letter 'L) 50)
        ((equal? letter 'C) 100)
        ((equal? letter 'D) 500)
        ((equal? letter 'M) 1000)
        (else '(INVALID LETTER))))

(define (final-value first-letter second-letter)
  (if (< (roman-value first-letter) (roman-value second-letter))
      (- (roman-value first-letter))
      (roman-value first-letter)))

(define (arabic num)
  (if (= (count num) 1)
      (roman-value num)
      (+ (final-value (first num) (first (bf num)))
         (arabic (bf num)))))

(= 1971 (arabic 'MCMLXXI))
(= 1066 (arabic 'MLXVI))


"----- Exercise 12.13 -----"

(define (convert seconds)
  (cond
    ((< seconds 60)
     (se seconds 'seconds))
    ((< (/ seconds 60) 60)
     (se (/ seconds 60) 'minutes))
    ((< (/ seconds 60 60) 24)
     (se (/ seconds 60 60) 'hours))
    ((< (/ seconds 60 60 24) 7)
     (se (/ seconds 60 60 24) 'days))
    ((< (/ seconds 60 60 24 7) 4)
     (se (/ seconds 60 60 24 7) 'weeks))
    ((< (/ seconds 60 60 24 7 4) 12)
     (se (/ seconds 60 60 24 7 4) 'months))
    ((< (/ seconds 60 60 24 7 4 12) 100)
     (se (/ seconds 60 60 24 7 4 12) 'years))
    (else (se (/ seconds 60 60 24 7 4 12 100) 'centuries))))

(define (factor seconds)
  (cond
    ((< seconds 60) 1)
    ((< (/ seconds 60) 60) 60)
    ((< (/ seconds 60 60) 24) (* 60 60))
    ((< (/ seconds 60 60 24) 7) (* 60 60 24))
    ((< (/ seconds 60 60 24 7) 4) (* 60 60 24 7))
    ((< (/ seconds 60 60 24 7 4) 12) (* 60 60 24 7 4))
    ((< (/ seconds 60 60 24 7 4 12) 100) (* 60 60 24 7 4 12))
    (else (* 60 60 24 7 4 12 100))))


(define (describe-time seconds)
  (let ((floor-part (floor (first (convert seconds)))))
  (if (< seconds 60)
      (convert seconds)
      (se floor-part (last (convert seconds))
          (describe-time (- seconds (* floor-part (factor seconds))))))))

(describe-time 22222)
(describe-time 4967189641)