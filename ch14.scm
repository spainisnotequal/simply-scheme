;; ========================== ;;
;; Chapter 14 - Simply Scheme ;;
;; ========================== ;;

(define (pairs-helper wd1 wd2 original)
  (cond ((empty? wd1) '())
        ((empty? wd2) (pairs-helper (bf wd1) original original))
        (else (se (word (first wd1) (first wd2))
                  (pairs-helper wd1 (bf wd2) original)))))

(define (pairs wd)
  (pairs-helper wd wd wd))

(pairs 'toy)


"----- Exercise 14.1 -----"

(define (remove-once wd sent)
  (cond ((empty? sent) '())
        ((equal? wd (first sent)) (bf sent))
        (else (se (first sent)
                  (remove-once wd (bf sent))))))

(equal? '(good good morning)
        (remove-once 'morning '(good morning good morning)))


"----- Exercise 14.2 -----"

(define (up wd)
  (if (empty? wd)
      '()
      (se (up (bl wd)) wd)))

(equal? '(t to tow town) (up 'town))


"----- Exercise 14.3 -----"

(define (remdup sent)
  (cond ((empty? sent) '())
        ((member? (first sent) (bf sent)) (remdup (bf sent)))
        (else (se (first sent) (remdup (bf sent))))))

(equal? '(di ob la da)
        (remdup '(ob la di ob la da)))


"----- Exercise 14.4 -----"

(define (odds sent)
  (cond ((empty? sent) '())
        ((= (count sent) 1) sent)
        (else (se (first sent) (odds (bf (bf sent)))))))

(equal? '(i my girl)
        (odds '(i lost my little girl)))



"----- Exercise 14.5 -----"

(define (letter-count-helper wd)
  (if (empty? wd)
      0
      (+ 1 (letter-count-helper (bf wd)))))

(define (letter-count sent)
  (if (empty? sent)
      0
      (+ (letter-count-helper (first sent))
         (letter-count (bf sent)))))

(= 11 (letter-count '(fixing a hole)))


"----- Exercise 14.6 -----"

(define (member? wd sent)
  (cond ((empty? sent) #f)
        ((equal? wd (first sent)) #t)
        (else (member? wd (bf sent)))))

(member? 'love '(I love you))
(not (member? 'love '(I like hotdogs)))


"----- Exercise 14.7 -----"

(define (differences sent)
  (if (<= (count sent) 1)
      '()
      (se (- (first (bf sent)) (first sent))
          (differences (bf sent)))))

(equal? '(19 -14 78 -81 6) (differences '(4 23 9 87 6 12)))


"----- Exercise 14.8 -----"

(define (repeat times wd)
  (if (<= times 0)
      '()
      (se wd (repeat (- times 1) wd))))

(define (expand sent)
  (cond ((empty? sent) '())
        ((number? (first sent))
         (se (repeat (- (first sent) 1) (first (bf sent)))
             (expand (bf sent))))
        (else (se (first sent) (expand (bf sent))))))

(equal? '(calling calling calling calling birds french french french hens)
        (expand '(4 calling birds 3 french hens)))

(equal? '(the samurai samurai samurai samurai samurai samurai samurai)
        (expand '(the 7 samurai)))


"----- Exercise 14.9 -----"

(define (location-helper wd sent pos)
  (cond ((empty? sent) #f)
        ((equal? wd (first sent)) pos)
        (else (location-helper wd (bf sent) (+ pos 1)))))

(define (location wd sent)
  (location-helper wd sent 1))

(= 4 (location 'me '(you never give me your money)))
(= 1 (location 'you '(you never give me your money)))
(= 6 (location 'money '(you never give me your money)))
(not (location 'umbrella '(you never give me your money)))


"----- Exercise 14.10 -----"

(define (duplicates? wd1 wd2)
  (if (equal? wd1 wd2)
      #t
      #f))

(define (count-adjacent-duplicates sent)
  (cond ((<= (count sent) 1) 0)
        ((duplicates? (first sent) (first (bf sent)))
         (+ 1 (count-adjacent-duplicates (bf sent))))
        (else (count-adjacent-duplicates (bf sent)))))

(= 3 (count-adjacent-duplicates '(y a b b a d a b b a d o o)))
(= 2 (count-adjacent-duplicates '(yeah yeah yeah)))


"----- Exercise 14.11 -----"

(define (duplicates? wd1 wd2)
  (if (equal? wd1 wd2)
      #t
      #f))

(define (remove-adjacent-duplicates sent)
  (cond ((<= (count sent) 1) sent)
        ((duplicates? (first sent) (first (bf sent)))
         (remove-adjacent-duplicates (bf sent)))
        (else (se (first sent) (remove-adjacent-duplicates (bf sent))))))

(equal? '(y a b a d a b a d o)
        (remove-adjacent-duplicates '(y a b b a d a b b a d o o)))

(equal? '(yeah) (remove-adjacent-duplicates '(yeah yeah yeah)))


"----- Exercise 14.12 -----"

(define (square x) (* x x))

(define (progressive-squares? sent)
  (cond ((<= (count sent) 1) #t)
        ((= (square (first sent)) (first (bf sent)))
         (progressive-squares? (bf sent)))
        (else #f)))

(progressive-squares? '(3 9 81 6561))

(not (progressive-squares? '(25 36 49 64)))


"----- Exercise 14.13 -----"

(define (any-vowel? wd)
  (cond ((empty? wd) #f)
        ((member? (first wd) 'aeiou) #t)
        (else (any-vowel? (bf wd)))))

(define (pigl wd)
  (cond ((not (any-vowel? wd)) (word wd 'ay))
        ((member? (first wd) 'aeiou) (word wd 'ay))
        (else (pigl (word (bf wd) (first wd))))))

(equal? 'frzzmlptay (pigl 'frzzmlpt))
(equal? 'astramipay (pigl 'pastrami))


"----- Exercise 14.14 -----"

(define (same-shape? sent1 sent2)
  (cond ((not (= (count sent1) (count sent2))) #f)
        ((empty? sent1) #t)
        ((not (= (count (first sent1)) (count (first sent2)))) #f)
        (else (same-shape? (bf sent1) (bf sent2)))))

(same-shape? '(the fool on the hill) '(you like me too much))
(not (same-shape? '(the fool on the hill) '(and your bird can sing)))


"----- Exercise 14.15 -----"

(define (merge sent1 sent2)
  (cond ((empty? sent1) sent2)
        ((empty? sent2) sent1)
        ((<= (first sent1) (first sent2))
         (se (first sent1) (merge (bf sent1) sent2)))
        (else
         (se (first sent2) (merge sent1 (bf sent2))))))

(equal? '(3 4 6 7 9 12 18 24 36 40 50 99)
        (merge '(4 7 18 40 99) '(3 6 9 12 24 36 50)))


"----- Exercise 14.16 -----"

(define (vowel? letter)
  (if (member? letter 'aeiou)
      #t
      #f))

(define (syllables wd)
  (cond ((<= (count wd) 1) (if (vowel? wd) 1 0))
        ((= (count wd) 1) 1)
        ((vowel? (first wd))
         (if (vowel? (first (bf wd)))
             (syllables (bf wd))
             (+ 1 (syllables (bf wd)))))
        (else (syllables (bf wd)))))

(syllables 'soaring)
(syllables 'boa)
(syllables 'canoa)
