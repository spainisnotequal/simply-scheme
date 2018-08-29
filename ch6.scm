;; ========================= ;;
;; Chapter 6 - Simply Scheme ;;
;; ========================= ;;


;; Exercise 6.5
;; ------------

;; european-time: sentence -> number
(define (european-time time)
  (if (equal? (last time) 'am)
      (if (< (first time) 12)
          (first time)
          (+ (first time) 12))
      (if (= (first time) 12)
          (first time)
          (+ (first time) 12))))

;; american-time: number -> sentence
(define (american-time time)
  (cond
    ((< time 12) (se time 'am))
    ((= time 12) (se time 'pm))
    (else (if (= time 24)
              (se (- time 12) 'am)
              (se (- time 12) 'pm)))))

(= 8 (european-time '(8 am)))
(= 24 (european-time '(12 am)))
(= 16 (european-time '(4 pm)))
(= 12 (european-time '(12 pm)))

(equal? '(8 am) (american-time 8))
(equal? '(4 pm) (american-time 16))
(equal? '(12 am) (american-time 24))


;; Exercise 6.6
;; ------------

;; teen?: number -> boolean
(define (teen? age)
  (and (<= 13 age) (< age 19)))

(not (teen? 12))
(teen? 13)
(teen? 18)
(not (teen? 19))


;; Exercise 6.7
;; ------------

;; type-of: anything -> word
(define (type-of x)
  (cond
    ((number? x) 'number) ; this clause has to be before the word check clause, because a "number" is consider a "word"
    ((boolean? x) 'boolean)
    ((word? x) 'word)
    ((sentence? x) 'sentence)
    (else '(I do not know))))

(equal? 'sentence (type-of '(getting better)))
(equal? 'word (type-of 'revolution))
(equal? 'boolean (type-of (= 3 3)))
(equal? 'number (type-of 23))

;; Exercise 6.8
;; ------------

;; inder-article: word -> sentence
(define (inder-article wd)
  (if (member? (first wd) '(a e i o u))
      (se 'an wd)
      (se 'a wd)))

(equal? '(a beatle) (inder-article 'beatle))
(equal? '(an album) (inder-article 'album))


;; Exercise 6.9
;; ------------

;; thismany: number word -> sentence
(define (thismany number wd)
  (if (= number 1)
      (se '1 wd)
      (se number (word wd 's))))

(equal? '(1 partridge) (thismany 1 'partridge))
(equal? '(2 french-hens) (thismany 2 'french-hen))


;; Exercise 6.10
;; -------------

;; sort2: sentence -> sentence
(define (sort2 sent)
  (if (< (first sent) (last sent))
      sent
      (se (last sent) (first sent))))

(equal? '(5 7) (sort2 '(5 7)))
(equal? '(5 7) (sort2 '(7 5)))


;; Exercise 6.11
;; -------------

;; leap_year: number -> boolean  ;; leap year = año bisiesto
(define (leap_year year)
  (if (= (remainder year 4) 0)
      (if (= (remainder year 100) 0)
          (if (= (remainder year 400) 0)
              #t
              #f)
          #t)
      #f))


;; valid_date?: number number number -> boolean
(define (valid_date? month day year)
  (if (not (and (and (<= 1 month) (<= month 12))
                (and (<= 1 day) (<= day 31))
                (and (<= 1 year) (<= year 2018))))
      #f
      (cond
        ((= month 2) (if (and (leap_year year) (<= day 29))
                         #t
                         (if (<= day 28)
                             #t
                             #f)))
        ((and (member? month '(4 6 9 11)) (= day 31)) #f)
        (else #t))))



(not (leap_year 1900))
(leap_year 2000)

(valid_date? 10 4 1949)
(not (valid_date? 20 4 1776))
(not (valid_date? 5 0 1992))
(not (valid_date? 2 29 1900))
(valid_date? 2 29 2000)


;; Exercise 6.12
;; -------------

;; plural: word -> word
(define (plural wd)
  (cond
    ((equal? (last wd) 'y) (if (equal? (last (bl wd)) 'o)
                               (word wd 's)
                               (word (bl wd) 'ies)))
    ((equal? (last wd) 'x) (word wd 'es))
    ((equal? (last wd) 'o) (word wd 'es))
    (else (word wd 's))))

(equal? 'beatles (plural 'beatle))
(equal? 'computers (plural 'computer))
(equal? 'boxes (plural 'box))
(equal? 'boys (plural 'boy))
(equal? 'flies (plural 'fly))
(equal? 'tomatoes (plural 'tomato))


;; Exercise 6.13
;; -------------


;; Exercise 6.14
;; -------------

;; describe-time: number -> sentence
(define (describe-time seconds)
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
    (else (se (/ seconds 60 60 24 7 4 12) 'years))))

(equal? '(1 seconds) (describe-time 1))
(equal? '(1 minutes) (describe-time 60))
(equal? '(1 hours) (describe-time (* 60 60)))
(equal? '(1 days) (describe-time (* 60 60 24)))
(equal? '(1 weeks) (describe-time (* 60 60 24 7)))
(equal? '(1 months) (describe-time (* 60 60 24 7 4)))
(equal? '(1 years) (describe-time (* 60 60 24 7 4 12)))
(equal? '(25 years) (describe-time (* 60 60 24 7 4 12 25)))




